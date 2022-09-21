#![feature(let_chains)]
mod cmove;
mod fen;
mod moves;
mod piece;

use moves::KING_ATTACK_MASKS;

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

    pieces: [Option<Piece>; BOARD_SIZE], // idx 0 => A1, idx 1 => A2, ... , idx 63 => H8
}

pub(crate) fn square_str_to_idx(square: &[char]) -> u8 {
    match square[0] {
        'a'..='h' => true,
        _ => panic!("square file not a-h"),
    };
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

    pub fn valid_kings(&self) -> bool {
        let kings: Vec<(usize, Piece)> = self
            .pieces
            .iter()
            .enumerate()
            .filter(|(_, p)| p.is_some() && p.unwrap().get_type() == PieceType::King)
            .map(|(i, p)| (i, p.unwrap()))
            .collect();

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

    pub fn get_legal_moves(&self) -> Vec<CMove> {
        let mut moves = Vec::with_capacity(100);

        let ac = self.get_active_color();

        for (from, piece) in self.pieces.iter().enumerate() {
            if piece.is_none() {
                continue;
            }
            let piece = piece.as_ref().unwrap();

            if ac != piece.get_color() {
                continue;
            }

            let mut move_mask = self.get_attacks_for_piece(from as u8);

            if piece.get_type() == PieceType::Pawn {
                move_mask |= self.pawn_moves(from as u8);
            }

            for to in 0..64 {
                if move_mask & 1 << to == 0 {
                    // we are not attacking that square
                    continue;
                }

                if !(self.pieces[to].is_none()
                    || self.pieces[to].as_ref().unwrap().get_color() != ac)
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
                    promote_to: PieceType::Pawn,
                };

                let mut nb = self.clone();
                if nb.apply_move(&mv).is_ok() {
                    moves.push(mv);
                }
            }
        }

        moves
    }

    fn apply_move(&mut self, mv: &CMove) -> Result<(), &'static str> {
        // todo: implement en passant, castling, promotion

        let from_piece = self.pieces[mv.from as usize];
        let to_piece = self.pieces[mv.to as usize];

        if from_piece.is_none() {
            return Err("from square is empty");
        }

        if to_piece.is_some() && to_piece.unwrap().get_color() == from_piece.unwrap().get_color() {
            return Err("can not capture own piece");
        }

        let ac = self.get_active_color();
        if from_piece.unwrap().get_color() != ac {
            return Err("from color is not active");
        }

        if ac == PieceColor::Black {
            self.full_moves += 1;
        }

        self.active_color_and_castle_avaliability ^= WHITE_TO_MOVE_MASK;

        self.pieces[mv.to as usize] = from_piece;
        self.pieces[mv.from as usize] = None;

        if from_piece.unwrap().get_type() == PieceType::King {
            let mask = if ac == PieceColor::White {
                WHITE_KING_CASTLE_MASK | WHITE_QUEEN_CASTLE_MASK
            } else {
                BLACK_KING_CASTLE_MASK | BLACK_QUEEN_CASTLE_MASK
            };
            self.active_color_and_castle_avaliability &= !mask;
        }

        if from_piece.unwrap().get_type() == PieceType::Rook {
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

        if self.active_king_is_checked() {
            return Err("your king is in check bruv");
        }

        Ok(())
    }

    fn active_king_is_checked(&self) -> bool {
        // check that the one who just moved isn't in check

        let ac = self.get_active_color();

        // @castor, okok, bitmaps would be nicer
        let king_square = self
            .pieces
            .iter()
            .enumerate()
            .filter(|(_, p)| p.is_some())
            .map(|(i, p)| (i, p.unwrap()))
            .find(|(_, p)| p.get_type() == PieceType::King && p.get_color() == ac)
            .expect("could not find king")
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
            PieceType::Knight => self.knight_attack(idx),
            PieceType::Bishop => self.diag_attack(idx),
            PieceType::Queen => self.hori_vert_attack(idx) ^ self.diag_attack(idx),
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

        b.apply_move(&CMove {
            from: 12,
            to: 20,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(
            b.to_fen(),
            "rnbqkbnr/pppppppp/8/8/8/4P3/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
        );

        b.apply_move(&CMove {
            from: 50,
            to: 42,
            promote_to: PieceType::Pawn,
        })
        .unwrap();

        assert_eq!(
            b.to_fen(),
            "rnbqkbnr/pp1ppppp/2p5/8/8/4P3/PPPP1PPP/RNBQKBNR w KQkq - 0 2"
        );

        b.apply_move(&CMove {
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
}
