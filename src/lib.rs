pub mod cmove;
pub mod fen;
pub mod moves;
pub mod piece;

use moves::{KING_ATTACK_MASKS, KNIGHT_ATTACK_MASKS};

use crate::cmove::*;
use crate::piece::*;

const RANK_SIZE: usize = 8;
const BOARD_SIZE: usize = RANK_SIZE * RANK_SIZE;

const WHITE_TO_MOVE_MASK: u8 = 0b00000001;
const BLACK_QUEEN_CASTLE_MASK: u8 = 0b00000010;
const BLACK_KING_CASTLE_MASK: u8 = 0b00000100;
const WHITE_QUEEN_CASTLE_MASK: u8 = 0b00001000;
const WHITE_KING_CASTLE_MASK: u8 = 0b00010000;

// these were generated with the king_attack function
// since the kings attack never changes depending on other pieces, we can have it constant instead

#[derive(Debug, Clone)]
pub struct Board {
    active_color_and_castle_avaliability: u8, // hot path

    // u8 would suffice for humans, but bot vs bot can go on forever
    // starts at 1, increases when black moves (as in the fen spec)
    full_moves: u16,
    half_moves: u8,
    en_passant_square: u8, // u8::MAX when none, does not indicate that en passant is possible (per first fen spec)
    state: BoardState,

    pieces: [Option<Piece>; BOARD_SIZE], // idx 0 => A1, idx 1 => A2, ... , idx 63 => H8
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoardState {
    InProgress,
    Checkmate,
    Stalemate,
}

pub(crate) fn square_str_to_idx(square: &[char]) -> u8 {
    if !('a'..='h').contains(&square[0]) {
        panic!("square file not a-h");
    }
    let file: u8 = (square[0] as u8) - b'a';

    let rank: u8 = (square[1] as u8) - b'1';
    assert!(rank < 8, "square rank not 1-8");

    rank * (RANK_SIZE as u8) + file
}

pub(crate) fn idx_to_square_str(idx: u8) -> [char; 2] {
    assert!(
        idx < BOARD_SIZE as u8,
        "index cannot be more than the board size"
    );
    let file = (b'a' + idx % 8) as char;
    let rank = (b'1' + idx / 8) as char;
    [file, rank]
}

impl Default for Board {
    fn default() -> Self {
        Self::new()
    }
}

impl Board {
    pub fn new() -> Self {
        Self::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
    }

    pub fn get_active_color(&self) -> PieceColor {
        if self.active_color_and_castle_avaliability & WHITE_TO_MOVE_MASK != 0 {
            PieceColor::White
        } else {
            PieceColor::Black
        }
    }

    fn get_pieces_by_type(&self, t: PieceType) -> impl Iterator<Item = (usize, Piece)> + '_ {
        self.pieces
            .iter()
            .enumerate()
            .filter(move |(_, p)| p.is_some() && p.unwrap().get_type() == t)
            .map(|(i, p)| (i, p.unwrap()))
    }

    pub fn get_pices(&self) -> [Option<Piece>; 64] {
        self.pieces
    }

    pub fn valid_kings(&self) -> bool {
        let kings: Vec<(usize, Piece)> = self.get_pieces_by_type(PieceType::King).collect();

        if kings.len() != 2 {
            return false;
        }

        if kings[0].1.get_color() == kings[1].1.get_color() {
            return false;
        }

        if KING_ATTACK_MASKS[kings[0].0] & 1 << kings[1].0 != 0
            || KING_ATTACK_MASKS[kings[1].0] & 1 << kings[0].0 != 0
        {
            // make sure that the kings aren't attackig each other
            return false;
        }

        true
    }

    pub fn get_legal_moves_for_idx(&self, from: u8, moves: &mut Vec<CMove>) {
        let mut move_mask = self.get_attacks_for_piece(from as u8);
        let piece = match self.pieces[from as usize] {
            Some(p) => p,
            None => return,
        };

        move_mask |= match piece.get_type() {
            PieceType::Pawn => self.pawn_moves(from as u8),
            PieceType::King => self.castling(from as u8),
            _ => 0,
        };

        for to in 0..64 {
            if move_mask & 1 << to == 0 {
                // we are not attacking that square
                continue;
            }

            if !(self.pieces[to].is_none()
                || self.pieces[to].as_ref().unwrap().get_color() != self.get_active_color())
            {
                // square is own piece
                continue;
            }
            if self.pieces[to].is_some()
                && self.pieces[to].as_ref().unwrap().get_type() == PieceType::King
            {
                // should not happen with a valid board, but just don't capture the king pls
                // this is here for fuzzing to work, but remove this when proper checks are in place
                continue;
            }

            let mv = CMove {
                from: from as u8,
                to: to as u8,
                promote_to: PieceType::Queen,
            };

            if self.clone().apply_move(&mv).is_ok() {
                moves.push(mv);
            }
        }
    }

    pub fn get_legal_moves(&self) -> Vec<CMove> {
        let mut moves = Vec::with_capacity(100);

        let ac = self.get_active_color();

        let active_pieces = self
            .pieces
            .iter()
            .enumerate()
            .filter(|(_, p)| p.is_some() && p.unwrap().get_color() == ac);

        for (from, _) in active_pieces {
            self.get_legal_moves_for_idx(from as u8, &mut moves);
        }

        moves
    }

    pub fn make_move(&mut self, mv: &CMove) -> Result<(), &'static str> {
        if self.state != BoardState::InProgress {
            return Err("game ended");
        }

        let mut moves: Vec<CMove> = Vec::with_capacity(10);
        self.get_legal_moves_for_idx(mv.from, &mut moves);
        if !moves.iter().any(|x| x.to == mv.to) {
            return Err("not legal move");
        }

        self.apply_move(mv)?;

        if self.get_legal_moves().is_empty() {
            if self.king_is_checked(self.get_active_color()) {
                self.state = BoardState::Checkmate;
            } else {
                self.state = BoardState::Stalemate;
            }
        }

        Ok(())
    }

    fn apply_move(&mut self, mv: &CMove) -> Result<(), &'static str> {
        let from_piece = self.pieces[mv.from as usize];
        let to_piece = self.pieces[mv.to as usize];

        let ac = self.get_active_color();

        if from_piece.is_none() {
            return Err("from square is empty");
        }
        let from_piece = from_piece.unwrap();

        if to_piece.is_some() && to_piece.unwrap().get_color() == from_piece.get_color() {
            return Err("can not capture own piece");
        }

        if from_piece.get_color() != self.get_active_color() {
            return Err("from color is not active");
        }

        let old_pieces = self.pieces;

        let mut new_piece = from_piece;

        if from_piece.get_type() == PieceType::Pawn {
            // promotion
            let fc = from_piece.get_color();

            if (fc == PieceColor::White && mv.to / 8 == 7)
                || (fc == PieceColor::Black && mv.to / 8 == 0)
            {
                if mv.promote_to == PieceType::Pawn {
                    return Err("can not promote to pawn");
                }
                new_piece = Piece::new(mv.promote_to, fc);
            }

            if to_piece.is_none() && mv.from % 8 != mv.to % 8 && mv.to != self.en_passant_square {
                return Err("can not do diagonal move unless attack");
            }
        }

        if from_piece.get_type() == PieceType::King && (mv.from % 8).abs_diff(mv.to % 8) > 1 {
            // castling
            let new_king_sqr = mv.from as i8 + (if mv.to < mv.from { -2_i8 } else { 2 });
            let new_rook_sqr = mv.from as i8 + (if mv.to < mv.from { -1_i8 } else { 1 });
            let old_rook_sqr = mv.to as i8 + (if mv.to < mv.from { -1_i8 } else { 1 });

            self.pieces[new_king_sqr as usize] = Some(from_piece);
            self.pieces[new_rook_sqr as usize] = self.pieces[old_rook_sqr as usize];
            self.pieces[old_rook_sqr as usize] = None;
            self.pieces[mv.from as usize] = None;
        } else if mv.to == self.en_passant_square && from_piece.get_type() == PieceType::Pawn {
            // en passant

            let x = mv.to as i8
                + if from_piece.get_color() == PieceColor::Black {
                    8
                } else {
                    -8_i8
                };
            self.pieces[mv.to as usize] = Some(from_piece);
            self.pieces[x as usize] = None;
            self.pieces[mv.from as usize] = None;
            self.en_passant_square = u8::MAX;
        } else {
            // default
            self.pieces[mv.to as usize] = Some(new_piece);
            self.pieces[mv.from as usize] = None;
        }

        if self.king_is_checked(self.get_active_color()) {
            self.pieces = old_pieces; // rollback
            return Err("your king is in check");
        }

        if ac == PieceColor::Black {
            self.full_moves += 1;
        }

        if from_piece.get_type() == PieceType::Pawn && (mv.to / 8).abs_diff(mv.from / 8) == 2 {
            // en passant could be avaliable

            for x in [-1_i8, 1] {
                let i = (mv.to as i8).checked_add(x);
                if i.is_none() || i.unwrap() >= 64 {
                    continue;
                }
                if self.pieces[(mv.to as i8 + x) as usize].is_none() {
                    continue;
                }
                let p = self.pieces[(mv.to as i8 + x) as usize].unwrap();
                if p.get_color() == ac || p.get_type() != PieceType::Pawn {
                    continue;
                }

                self.en_passant_square = match from_piece.get_color() {
                    PieceColor::White => mv.to - 8,
                    PieceColor::Black => mv.to + 8,
                };
                break;
            }
        }

        self.taint_castling_avaliability(mv, from_piece);
        self.active_color_and_castle_avaliability ^= WHITE_TO_MOVE_MASK;

        Ok(())
    }

    fn taint_castling_avaliability(&mut self, mv: &CMove, from_piece: Piece) {
        if from_piece.get_type() == PieceType::King {
            let mask = if self.get_active_color() == PieceColor::White {
                WHITE_KING_CASTLE_MASK | WHITE_QUEEN_CASTLE_MASK
            } else {
                BLACK_KING_CASTLE_MASK | BLACK_QUEEN_CASTLE_MASK
            };
            self.active_color_and_castle_avaliability &= !mask;
        }

        if from_piece.get_type() == PieceType::Rook {
            match mv.from {
                0 => {
                    self.active_color_and_castle_avaliability &= !WHITE_QUEEN_CASTLE_MASK;
                }
                7 => {
                    self.active_color_and_castle_avaliability &= !WHITE_KING_CASTLE_MASK;
                }
                53 => {
                    self.active_color_and_castle_avaliability &= !BLACK_QUEEN_CASTLE_MASK;
                }
                63 => {
                    self.active_color_and_castle_avaliability &= !BLACK_KING_CASTLE_MASK;
                }
                _ => {}
            }
        }
    }

    fn king_is_checked(&self, ac: PieceColor) -> bool {
        let king_square = self
            .get_pieces_by_type(PieceType::King)
            .find(|k| k.1.get_color() == ac)
            .expect("where knug")
            .0;

        self.pieces
            .iter()
            .enumerate()
            .filter(|(_, p)| p.is_some() && p.unwrap().get_color() != ac)
            .any(|(i, _)| self.get_attacks_for_piece(i as u8) & 1 << king_square != 0)
    }

    fn get_attacks_for_piece(&self, idx: u8) -> u64 {
        // maybe cache attack masks? they are used multiple times so it makes sense to only evaluate them once
        assert!(
            idx < BOARD_SIZE as u8,
            "idx cannot be larger than the board"
        );

        let p = match &self.pieces[idx as usize] {
            Some(p) => p,
            None => return 0,
        };

        match p.get_type() {
            PieceType::Pawn => self.pawn_attack(idx),
            PieceType::Rook => self.hori_vert_attack(idx),
            PieceType::Knight => KNIGHT_ATTACK_MASKS[idx as usize],
            PieceType::Bishop => self.diag_attack(idx),
            PieceType::Queen => self.hori_vert_attack(idx) | self.diag_attack(idx),
            PieceType::King => KING_ATTACK_MASKS[idx as usize],
        }
    }
}

#[cfg(test)]
mod lib_tests {
    use crate::{
        cmove::CMove,
        piece::{PieceColor, PieceType},
        square_str_to_idx,
    };

    use super::Board;

    #[test]
    fn fen_test() {
        // standard initial setup
        // todo: implement generic chessboard comparison
        let b =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();

        assert_eq!(b.en_passant_square, u8::MAX);
        assert_eq!(b.half_moves, 0);
        assert_eq!(b.full_moves, 1);

        for file in 'a'..='h' {
            for rank in '1'..='8' {
                let p = b.pieces[square_str_to_idx(&[file, rank]) as usize];

                if rank > '2' && rank < '7' {
                    assert!(p.is_none());
                    continue;
                }

                let p = p.unwrap();
                if rank <= '2' {
                    assert_eq!(
                        p.get_color(),
                        PieceColor::White,
                        "piece color must be white"
                    );
                } else if rank >= '7' {
                    assert_eq!(
                        p.get_color(),
                        PieceColor::Black,
                        "piece color must be black"
                    );
                }

                if rank == '7' || rank == '2' {
                    assert_eq!(
                        p.get_type(),
                        PieceType::Pawn,
                        "2/7th rank must be all pawns"
                    );
                }

                if rank == '8' || rank == '1' {
                    match p.get_type() {
                        PieceType::Rook => {
                            assert!(file == 'a' || file == 'h')
                        }
                        PieceType::Knight => {
                            assert!(file == 'b' || file == 'g')
                        }
                        PieceType::Bishop => {
                            assert!(file == 'c' || file == 'f')
                        }
                        PieceType::Queen => {
                            assert!(file == 'd')
                        }
                        PieceType::King => {
                            assert!(file == 'e')
                        }
                        _ => {
                            panic!("unknown piece")
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn make_moves() {
        let mut b = Board::default();

        b.make_move(&CMove {
            from: 12,
            to: 20,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(
            b.to_fen(),
            "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
        );

        b.make_move(&CMove {
            from: 50,
            to: 42,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(
            b.to_fen(),
            "rnbqkbnr/pp1ppppp/2p5/8/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
        );

        b.make_move(&CMove {
            from: 4,
            to: 12,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(
            b.to_fen(),
            "rnbqkbnr/pp1ppppp/2p5/8/8/4P3/PPPPKPPP/RNBQ1BNR b kq - 0 2"
        );
    }
}

#[cfg(test)]
#[allow(clippy::all)]
mod internal_tests {

    use crate::{cmove::CMove, piece::PieceType, BoardState};

    use super::{idx_to_square_str, square_str_to_idx, Board, BOARD_SIZE};

    const STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

    #[test]
    fn to_idx_test() {
        assert_eq!(square_str_to_idx(&['a', '1']), 0);
        assert_eq!(square_str_to_idx(&['b', '1']), 1);
        assert_eq!(square_str_to_idx(&['c', '1']), 2);
        assert_eq!(square_str_to_idx(&['d', '1']), 3);
        assert_eq!(square_str_to_idx(&['e', '1']), 4);
        assert_eq!(square_str_to_idx(&['f', '1']), 5);
        assert_eq!(square_str_to_idx(&['g', '1']), 6);
        assert_eq!(square_str_to_idx(&['h', '1']), 7);

        assert_eq!(square_str_to_idx(&['c', '1']), 2 + 8 * 0);
        assert_eq!(square_str_to_idx(&['c', '2']), 2 + 8 * 1);
        assert_eq!(square_str_to_idx(&['c', '3']), 2 + 8 * 2);
        assert_eq!(square_str_to_idx(&['c', '4']), 2 + 8 * 3);
        assert_eq!(square_str_to_idx(&['c', '5']), 2 + 8 * 4);
        assert_eq!(square_str_to_idx(&['c', '6']), 2 + 8 * 5);
        assert_eq!(square_str_to_idx(&['c', '7']), 2 + 8 * 6);
        assert_eq!(square_str_to_idx(&['c', '8']), 2 + 8 * 7);

        assert_eq!(square_str_to_idx(&['h', '8']), 63);
    }

    #[test]
    fn idx_from_to_test() {
        for idx in 0..BOARD_SIZE {
            assert_eq!(square_str_to_idx(&idx_to_square_str(idx as u8)), idx as u8);
        }
        for rank in '1'..='8' {
            for file in 'a'..='h' {
                assert_eq!(
                    idx_to_square_str(square_str_to_idx(&[file, rank])),
                    [file, rank]
                );
            }
        }
    }

    #[test]
    fn get_legal_moves_test() {
        let b = Board::from_fen(STARTING_FEN).unwrap();

        b.get_legal_moves();
    }

    #[test]
    fn move_while_checked() {
        let fen = "rnb1kbnr/pppp1ppp/8/4p3/4PP1q/8/PPPP2PP/RNBQKBNR w KQkq - 1 3";
        let mut b = Board::from_fen(fen).unwrap();
        b.make_move(&CMove {
            from: 6,
            to: 5 + 16,
            promote_to: PieceType::Pawn,
        })
        .expect_err("is in check, should not be able to move knight");

        assert_eq!(b.to_fen(), fen, "invalid move must not modify board");
    }

    #[test]
    fn promotion() {
        let mut b = Board::from_fen("8/7P/1k6/8/8/3K4/8/8 w - - 0 1").unwrap();

        b.make_move(&CMove {
            from: 63 - 8,
            to: 63,
            promote_to: PieceType::Queen,
        })
        .unwrap();

        assert_eq!("7Q/8/1k6/8/8/3K4/8/8 b - - 0 1", b.to_fen());
    }

    #[test]
    fn stalemate() {
        let mut b = Board::from_fen("6k1/8/6Q1/8/8/3K4/8/8 b - - 0 1").unwrap();

        b.make_move(&CMove {
            from: 62,
            to: 63,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(b.state, BoardState::InProgress);

        b.make_move(&CMove {
            from: 19,
            to: 20,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(b.state, BoardState::Stalemate);
    }

    #[test]
    fn checkmate() {
        let mut b = Board::from_fen("7k/5K2/6Q1/8/8/8/8/8 w - - 0 1").unwrap();

        b.make_move(&CMove {
            from: 46,
            to: 46 + 8,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(b.state, BoardState::Checkmate);
    }

    #[test]
    fn castling() {
        // white short
        let mut b =
            Board::from_fen("rnbqkbnr/ppp2ppp/3p4/4p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4")
                .unwrap();

        b.make_move(&CMove {
            from: 4,
            to: 6,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(
            b.to_fen(),
            "rnbqkbnr/ppp2ppp/3p4/4p3/2B1P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 0 4"
        );

        // white long
        let mut b =
            Board::from_fen("rnb1kbnr/ppp2pp1/3p3p/4p1q1/3P4/2N5/PPPQPPPP/R3KBNR w KQkq - 0 5")
                .unwrap();

        b.make_move(&CMove {
            from: 4,
            to: 1,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(
            b.to_fen(),
            "rnb1kbnr/ppp2pp1/3p3p/4p1q1/3P4/2N5/PPPQPPPP/2KR1BNR b kq - 0 5"
        );

        // black short
        let mut b =
            Board::from_fen("rnbqk2r/pppp1ppp/5n2/4p3/1b2PP2/2P2N2/PP1P2PP/RNBQKB1R b KQkq - 0 4")
                .unwrap();
        b.make_move(&CMove {
            from: 60,
            to: 62,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(
            b.to_fen(),
            "rnbq1rk1/pppp1ppp/5n2/4p3/1b2PP2/2P2N2/PP1P2PP/RNBQKB1R w KQ - 0 5"
        );

        // black long
        let mut b =
            Board::from_fen("r3kbnr/ppp1pppp/2nq4/3p4/3P1Bb1/2N2N1P/PPP1PPP1/R2QKB1R b KQkq - 0 5")
                .unwrap();

        b.make_move(&CMove {
            from: 60,
            to: 57,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(
            b.to_fen(),
            "2kr1bnr/ppp1pppp/2nq4/3p4/3P1Bb1/2N2N1P/PPP1PPP1/R2QKB1R w KQ - 0 6"
        );
    }

    #[test]
    fn en_passant_square() {
        let mut b = Board::from_fen("r1bqkbnr/pppppppp/2n5/4P3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2")
            .unwrap();

        b.make_move(&CMove {
            from: 51,
            to: 51 - 16,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(
            b.to_fen(),
            "r1bqkbnr/ppp1pppp/2n5/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3" // note d6
        );
    }

    #[test]
    fn en_passant_capture() {
        let mut b =
            Board::from_fen("r1bqkbnr/pppppp1p/2n5/6pP/8/8/PPPPPPP1/RNBQKBNR w KQkq g6 0 3")
                .unwrap();

        b.make_move(&CMove {
            from: 39,
            to: 46,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(
            b.to_fen(),
            "r1bqkbnr/pppppp1p/2n3P1/8/8/8/PPPPPPP1/RNBQKBNR b KQkq - 0 3"
        );
    }
}
