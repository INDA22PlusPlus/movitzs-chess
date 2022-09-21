mod cmove;
mod piece;

use crate::cmove::*;
use crate::piece::*;

use lazy_static::lazy_static;
use regex::Regex;

const RANK_SIZE: usize = 8;
const BOARD_SIZE: usize = RANK_SIZE * RANK_SIZE;

const WHITE_TO_MOVE_MASK: u8 = 0b00000001;
const BLACK_QUEEN_CASTLE_MASK: u8 = 0b00000010;
const BLACK_KING_CASTLE_MASK: u8 = 0b00000100;
const WHITE_QUEEN_CASTLE_MASK: u8 = 0b00001000;
const WHITE_KING_CASTLE_MASK: u8 = 0b00010000;

// these were generated with the king_attack function
// since the kings attack never changes depending on other pieces, we can have it constant instead
const KING_ATTACK_MASKS: [u64; 64] = [
    770,
    1797,
    3594,
    7188,
    14376,
    28752,
    57504,
    49216,
    197123,
    460039,
    920078,
    1840156,
    3680312,
    7360624,
    14721248,
    12599488,
    50463488,
    117769984,
    235539968,
    471079936,
    942159872,
    1884319744,
    3768639488,
    3225468928,
    12918652928,
    30149115904,
    60298231808,
    120596463616,
    241192927232,
    482385854464,
    964771708928,
    825720045568,
    3307175149568,
    7718173671424,
    15436347342848,
    30872694685696,
    61745389371392,
    123490778742784,
    246981557485568,
    211384331665408,
    846636838289408,
    1975852459884544,
    3951704919769088,
    7903409839538176,
    15806819679076352,
    31613639358152704,
    63227278716305408,
    54114388906344448,
    216739030602088448,
    505818229730443264,
    1011636459460886528,
    2023272918921773056,
    4046545837843546112,
    8093091675687092224,
    16186183351374184448,
    13853283560024178688,
    144959613005987840,
    362258295026614272,
    724516590053228544,
    1449033180106457088,
    2898066360212914176,
    5796132720425828352,
    11592265440851656704,
    4665729213955833856,
];

#[derive(Debug, Clone)]
#[allow(dead_code)]
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
    let file: u8 = (square[0] as u8) - 'a' as u8;

    let rank: u8 = (square[1] as u8) - '1' as u8;
    assert!(rank < 8, "square rank not 1-8");

    rank * (RANK_SIZE as u8) + file
}

pub(crate) fn idx_to_square_str(idx: u8) -> [char; 2] {
    assert!(
        idx < BOARD_SIZE as u8,
        "index cannot be more than the board size"
    );
    let file = ('a' as u8 + idx % 8) as char;
    let rank = ('1' as u8 + idx / 8) as char;
    [file, rank]
}

impl Board {
    pub fn from_fen(fen: &str) -> Result<Self, &'static str> {
        // Reference: https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation
        lazy_static! {
            static ref ENPS_STR_RE: Regex = Regex::new(r"(?i)^-$|^[a-h][1-8]$").unwrap();
        }
        if !fen.is_ascii() {
            return Err("fen input contains non-ascii");
        }

        let fen: Vec<&str> = fen.split(|f| f == ' ').collect();
        if fen.len() != 6 {
            return Err("fen not six parts");
        }
        if fen[1].len() != 1 {
            return Err("len(side to move) != 1");
        }
        if !ENPS_STR_RE.is_match(fen[3]) {
            return Err("len(side to move) != 1");
        }

        const INIT: Option<Piece> = None; // https://github.com/rust-lang/rust/issues/44796
        let mut pieces = [INIT; BOARD_SIZE];

        let mut rank = 7;
        let mut file = 0;

        let board_part = fen[0];
        for c in board_part.chars() {
            if c == '/' {
                if rank == 0 {
                    return Err("too many ranks");
                }
                rank -= 1;
                file = 0;
                continue;
            }
            if c.is_numeric() {
                file += c.to_digit(10).unwrap() as usize;
                if file > 8 {
                    return Err("too many files");
                }
                continue;
            }
            if c.is_alphabetic() {
                let color = match c.is_lowercase() {
                    true => PieceColor::Black,
                    false => PieceColor::White,
                };
                let piece_type = match c.to_ascii_lowercase() {
                    'p' => PieceType::Pawn,
                    'r' => PieceType::Rook,
                    'n' => PieceType::Knight,
                    'b' => PieceType::Bishop,
                    'k' => PieceType::King,
                    'q' => PieceType::Queen,
                    _ => return Err("unknown piece"),
                };
                let idx = rank * 8 + file;
                if idx >= 64 {
                    return Err("piece placement invalid");
                }
                pieces[rank * 8 + file] = Some(Piece::new(piece_type, color));
                file += 1;
                continue;
            }

            return Err("unknown character in piece placement string");
        }

        let mut ac_a_ca = match fen[1].chars().nth(0).unwrap() {
            'w' => WHITE_TO_MOVE_MASK,
            'b' => 0,
            _ => return Err("next to move invalid"),
        };

        for c in fen[2].chars() {
            match c {
                'q' => ac_a_ca ^= BLACK_QUEEN_CASTLE_MASK,
                'k' => ac_a_ca ^= BLACK_KING_CASTLE_MASK,
                'Q' => ac_a_ca ^= WHITE_QUEEN_CASTLE_MASK,
                'K' => ac_a_ca ^= WHITE_KING_CASTLE_MASK,
                '-' => {}
                _ => {
                    return Err("castle avaliability invalid");
                }
            }
        }

        let half_moves = match fen[4].parse() {
            Ok(hm) => hm,
            Err(_) => {
                return Err("half moves not an int");
            }
        };
        let full_moves = match fen[5].parse() {
            Ok(fm) => fm,
            Err(_) => {
                return Err("full moves not an int");
            }
        };

        let ensq: Vec<char> = fen[3].chars().collect();
        let en_passant_square = match ensq[0] {
            '-' => u8::MAX,
            _ => square_str_to_idx(&[
                ensq[0].to_ascii_lowercase(), // todo wtf happened here
                ensq[1],
            ]),
        };

        Ok(Board {
            pieces,
            active_color_and_castle_avaliability: ac_a_ca,
            full_moves,
            half_moves,
            en_passant_square,
        })
    }

    pub fn to_fen(&self) -> String {
        let mut fen = String::new();

        for rank in (0..=7).rev() {
            let mut blank_squares: u8 = 0;

            for file in 0..=7 {
                let idx = rank * 8 + file;

                if self.pieces[idx].is_none() {
                    blank_squares += 1;
                    continue;
                }

                if blank_squares != 0 {
                    fen.push(char::from_digit(blank_squares as u32, 10).unwrap());
                    blank_squares = 0;
                }

                let p = self.pieces[idx].as_ref().unwrap();

                let mut c: char = match p.get_type() {
                    PieceType::Pawn => 'p',
                    PieceType::Rook => 'r',
                    PieceType::Bishop => 'b',
                    PieceType::Knight => 'n',
                    PieceType::Queen => 'q',
                    PieceType::King => 'k',
                };

                if p.get_color() == PieceColor::White {
                    c = c.to_ascii_uppercase();
                }

                fen.push(c);
            }

            if blank_squares != 0 {
                fen.push(char::from_digit(blank_squares as u32, 10).unwrap());
            }
            if rank != 0 {
                fen.push('/');
            }
        }

        fen.push(' ');

        if self.active_color_and_castle_avaliability & WHITE_TO_MOVE_MASK == 0 {
            fen.push('b');
        } else {
            fen.push('w');
        }

        fen.push(' ');

        if self.active_color_and_castle_avaliability
            & (BLACK_KING_CASTLE_MASK
                | BLACK_QUEEN_CASTLE_MASK
                | WHITE_KING_CASTLE_MASK
                | WHITE_QUEEN_CASTLE_MASK)
            == 0
        {
            fen.push('-');
        } else {
            if self.active_color_and_castle_avaliability & WHITE_KING_CASTLE_MASK != 0 {
                fen.push('K');
            }
            if self.active_color_and_castle_avaliability & WHITE_QUEEN_CASTLE_MASK != 0 {
                fen.push('Q');
            }
            if self.active_color_and_castle_avaliability & BLACK_KING_CASTLE_MASK != 0 {
                fen.push('k');
            }
            if self.active_color_and_castle_avaliability & BLACK_QUEEN_CASTLE_MASK != 0 {
                fen.push('q');
            }
        }

        fen.push(' ');

        if self.en_passant_square == u8::MAX {
            fen.push('-');
        } else {
            fen += &idx_to_square_str(self.en_passant_square)
                .iter()
                .collect::<String>();
        }

        fen.push(' ');

        fen.push_str(self.half_moves.to_string().as_str());
        fen.push(' ');
        fen.push_str(self.full_moves.to_string().as_str());

        return fen;
    }

    fn get_piece_at(&self, sqr: &[char]) -> Option<&Piece> {
        self.pieces[square_str_to_idx(sqr) as usize].as_ref()
    }

    pub fn get_active_color(&self) -> PieceColor {
        if self.active_color_and_castle_avaliability & WHITE_TO_MOVE_MASK != 0 {
            PieceColor::White
        } else {
            PieceColor::Black
        }
    }

    pub fn get_legal_moves(&self) -> Vec<CMove> {
        let mut moves = Vec::with_capacity(100);

        let ac = self.get_active_color();

        for (from, piece) in (&self.pieces).into_iter().enumerate() {
            if piece.is_none() {
                continue;
            }
            let piece = piece.as_ref().unwrap();

            if ac != piece.get_color() {
                continue;
            }

            let attack_mask = self.get_attacks_for_piece(from as u8);

            for to in 0..64 {
                if attack_mask & 1 << to == 0 {
                    // we are not attacking that square
                    continue;
                }

                if !(self.pieces[to].is_none()
                    || self.pieces[to].as_ref().unwrap().get_color() != ac)
                {
                    // square is own piece
                    continue;
                }
                let mv = CMove {
                    from: from as u8,
                    to: to as u8,
                    promote_to: PieceType::Pawn,
                };

                let mut nb = self.clone();
                nb.apply_move(&mv);
                if nb.board_is_legal_after_move() {
                    moves.push(mv);
                }
            }
        }

        moves
    }

    fn apply_move(&mut self, mv: &CMove) {
        // todo: implement enpassant, castling, promotion

        self.pieces[mv.to as usize] = self.pieces[mv.from as usize];
        self.pieces[mv.from as usize] = Option::None;
    }

    fn board_is_legal_after_move(&self) -> bool {
        // check that the one who just moved isn't in check
        // could probably do some bitmask magic here, where the attacks in a u64 is and:ed with the king position

        let ac = self.get_active_color();

        let mut king_square: u8 = u8::MAX;

        for (i, p) in self.pieces.into_iter().enumerate() {
            if p.is_some()
                && p.unwrap().get_type() == PieceType::King
                && p.unwrap().get_color() == ac
            {
                king_square = i as u8;
                break;
            }
        }

        if king_square == u8::MAX {
            panic!("no king detected, invalid board state");
        }

        for (i, p) in self.pieces.into_iter().enumerate() {
            if p.is_none() {
                continue;
            };
            let p = p.as_ref().unwrap();

            if p.get_color() == ac {
                // piece of the player who just moved
                continue;
            }

            if (self.get_attacks_for_piece(i as u8) & 1 << king_square) != 0 {
                return false;
            }
        }

        true
    }

    fn get_attacks_for_piece(&self, idx: u8) -> u64 {
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

    fn get_legal_moves_for_piece(&self, idx: u8) -> Vec<CMove> {
        let piece = match &self.pieces[idx as usize] {
            Some(p) => p,
            None => return Vec::new(),
        };

        let [file, rank] = idx_to_square_str(idx);

        Vec::new()
    }

    fn pawn_moves(&self, idx: u8) -> u64 {
        let [_, rank] = idx_to_square_str(idx);

        let color = self.pieces[idx as usize].unwrap().get_color();

        let idx = idx as i8;

        if (color == PieceColor::Black && rank == '1')
            || (color == PieceColor::White && rank == '8')
        {
            return 0;
        }

        let dir: i8 = if color == PieceColor::White { 8 } else { -8 };

        if self.pieces[(idx + dir) as usize].is_some() {
            return 0;
        }

        let mut result = 1 << idx + dir;

        if ((color == PieceColor::Black && rank == '7')
            || (color == PieceColor::White && rank == '2'))
            && self.pieces[(idx + dir * 2) as usize].is_none()
        {
            result ^= 1 << idx + dir * 2;
        }

        result
    }

    fn pawn_attack(&self, idx: u8) -> u64 {
        let mut result = 0;
        let [file, _] = idx_to_square_str(idx);

        let color = self.pieces[idx as usize].unwrap().get_color();

        let idx = idx as i8;

        for att_mv in [1, -1_i8] {
            if (color == PieceColor::Black && file == 'a')
                || (color == PieceColor::White && file == 'h')
            {
                continue;
            }

            let dir: i8 = if color == PieceColor::White { 8 } else { -8 };

            let att_sqr = idx + dir + att_mv;
            if att_sqr < 0 || att_sqr >= BOARD_SIZE as i8 {
                continue;
            }

            if (att_sqr / 8) == (idx + dir) / 8 {
                result ^= 1 << att_sqr;
            }
        }
        result
    }

    fn diag_attack(&self, idx: u8) -> u64 {
        let mut result = 0;

        for f_dir in [-1_i8, 1] {
            for r_dir in [-1_i8, 1] {
                let [mut file, mut rank] = idx_to_square_str(idx);

                for _ in 1..8 {
                    file = ((file as i8) + f_dir) as u8 as char;
                    rank = ((rank as i8) + r_dir) as u8 as char;

                    if file < 'a' || file > 'h' || rank < '1' || rank > '8' {
                        break;
                    }

                    let n_idx = square_str_to_idx(&[file, rank]);

                    result ^= 1 << n_idx;

                    if self.pieces[n_idx as usize].is_some() {
                        break;
                    }
                }
            }
        }

        result
    }

    // kept for informational purposes, now we use constants instead
    #[allow(dead_code)]
    fn king_attack(&self, idx: u8) -> u64 {
        let mut result = 0;
        let [file, rank] = idx_to_square_str(idx);

        if rank > '1' {
            // attack down
            result ^= 1 << idx - 8;
        }
        if rank < '8' {
            // attack up
            result ^= 1 << idx + 8;
        }
        if file != 'a' {
            // attack left
            result ^= 1 << idx - 1;
        }
        if file != 'h' {
            // attack right
            result ^= 1 << idx + 1;
        }

        if rank != '1' && file != 'a' {
            // attack down left
            result ^= 1 << idx - 8 - 1;
        }
        if rank != '8' && file != 'a' {
            // attack up left
            result ^= 1 << idx + 8 - 1;
        }
        if rank != '1' && file != 'h' {
            // attack down right
            result ^= 1 << idx - 8 + 1;
        }
        if rank != '8' && file != 'h' {
            // attack up right
            result ^= 1 << idx + 8 + 1;
        }

        result
    }

    fn knight_attack(&self, idx: u8) -> u64 {
        let [file, rank] = idx_to_square_str(idx);

        // these could also be pre-computed

        let mut result = 0;
        if file != 'a' && rank < '7' {
            // attack up left
            result ^= 1 << idx + 16 - 1;
        }
        if file > 'b' && rank != '8' {
            // attack left up
            result ^= 1 << idx + 8 - 2;
        }

        if file != 'h' && rank < '7' {
            // attack up right
            result ^= 1 << idx + 16 + 1;
        }
        if file < 'h' && rank != '8' {
            // attack right up
            result ^= 1 << idx + 8 + 2;
        }

        if file != 'a' && rank > '2' {
            // attack down left
            result ^= 1 << idx - 16 - 1;
        }
        if file > 'b' && rank != '1' {
            // attack left down
            result ^= 1 << idx - 8 - 2;
        }

        if file != 'h' && rank > '2' {
            // attack down right
            result ^= 1 << idx - 16 + 1;
        }
        if file < 'g' && rank != '1' {
            // attack right down
            result ^= 1 << idx - 8 + 2;
        }

        result
    }

    fn hori_vert_attack(&self, idx: u8) -> u64 {
        let mut result = 0;
        for dir in [true, false] {
            for r in 1..8 {
                let n_idx = match dir {
                    true => idx.checked_add(r),
                    false => idx.checked_sub(r),
                };
                if n_idx.is_none() {
                    continue;
                }
                let n_idx = n_idx.unwrap();

                if idx / 8 != n_idx / 8 {
                    break;
                }

                result ^= 1 << n_idx;

                if self.pieces[n_idx as usize].is_some() {
                    break;
                }
            }
        }

        for dir in [true, false] {
            for r in 1..8 {
                let n_idx = match dir {
                    true => idx.checked_add(r * 8), // should always be fine
                    false => idx.checked_sub(r * 8),
                };
                if n_idx.is_none() {
                    continue;
                }

                let n_idx = n_idx.unwrap();

                if n_idx >= BOARD_SIZE as u8 {
                    break;
                }

                result ^= 1 << n_idx;

                if self.pieces[n_idx as usize].is_some() {
                    break;
                }
            }
        }
        result
    }
}

#[cfg(test)]
mod lib_tests {
    use crate::piece::{PieceColor, PieceType};

    use super::Board;

    #[test]
    fn fen_in_out() {
        let cases = [
            "rn1qkbnr/pppbp1pp/8/1B1pPp2/8/8/PPPP1PPP/RNBQK1NR w KQkq f6 0 4",
            "rn1qkbnr/pppbp1pp/8/1B1pPp2/8/8/PPPPKPPP/RNBQ2NR b kq - 1 4",
            "rn1qkbnr/ppp1p1pp/8/1b1pPp2/8/3P4/PPP1KPPP/RNBQ2NR b kq - 0 5",
            "rn1qkb1r/ppp1p1pp/5n2/1b1pPp2/8/3P4/PPP1KPPP/RNBQ2NR w kq - 1 6",
            "rn1qkbr1/ppp1p1pp/5n2/1b1pPp2/8/3P1N2/PPP1KPPP/RNBQ3R w q - 3 7",
            "rn1qkbr1/ppp1p1p1/5n2/1b1pP1Pp/5p2/3P1N2/PPP1KP1P/RNBQ3R w q h6 0 9",
            "8/8/8/8/8/8/8/8 w - - 0 1",
        ];

        for case in cases {
            let out_fen = Board::from_fen(case).unwrap().to_fen();
            assert!(
                out_fen == case,
                "in didn't match out, expected: {case}, got: {out_fen}"
            );
        }
    }

    #[test]
    fn fen_test() {
        // standard initial setup
        // todo: implement generic chessboard comparison
        let b =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();

        assert!(b.en_passant_square == u8::MAX);
        assert!(b.half_moves == 0);
        assert!(b.full_moves == 1); // todo this is internal

        for file in 'a'..='h' {
            for rank in '1'..='8' {
                let p = b.get_piece_at(&[file, rank]);

                if rank > '2' && rank < '7' {
                    assert!(p.is_none());
                    continue;
                }

                let p = p.unwrap();
                if rank <= '2' {
                    assert!(
                        p.get_color() == PieceColor::White,
                        "piece color must be white"
                    );
                } else if rank >= '7' {
                    assert!(
                        p.get_color() == PieceColor::Black,
                        "piece color must be black"
                    );
                }

                if rank == '7' || rank == '2' {
                    assert!(
                        p.get_type() == PieceType::Pawn,
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
}

#[cfg(test)]
mod internal_tests {
    use crate::KING_ATTACK_MASKS;

    use super::{idx_to_square_str, square_str_to_idx, Board, BOARD_SIZE};

    const STARTING_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

    #[test]
    fn to_idx_test() {
        assert!(square_str_to_idx(&['a', '1']) == 0);
        assert!(square_str_to_idx(&['b', '1']) == 1);
        assert!(square_str_to_idx(&['c', '1']) == 2);
        assert!(square_str_to_idx(&['d', '1']) == 3);
        assert!(square_str_to_idx(&['e', '1']) == 4);
        assert!(square_str_to_idx(&['f', '1']) == 5);
        assert!(square_str_to_idx(&['g', '1']) == 6);
        assert!(square_str_to_idx(&['h', '1']) == 7);

        assert!(square_str_to_idx(&['c', '1']) == 2 + 8 * 0);
        assert!(square_str_to_idx(&['c', '2']) == 2 + 8 * 1);
        assert!(square_str_to_idx(&['c', '3']) == 2 + 8 * 2);
        assert!(square_str_to_idx(&['c', '4']) == 2 + 8 * 3);
        assert!(square_str_to_idx(&['c', '5']) == 2 + 8 * 4);
        assert!(square_str_to_idx(&['c', '6']) == 2 + 8 * 5);
        assert!(square_str_to_idx(&['c', '7']) == 2 + 8 * 6);
        assert!(square_str_to_idx(&['c', '8']) == 2 + 8 * 7);

        assert!(square_str_to_idx(&['h', '8']) == 63);
    }

    #[test]
    fn idx_from_to_test() {
        for idx in 0..BOARD_SIZE {
            assert!(square_str_to_idx(&idx_to_square_str(idx as u8)) == idx as u8);
        }
        for rank in '1'..='8' {
            for file in 'a'..='h' {
                assert!(idx_to_square_str(square_str_to_idx(&[file, rank])) == [file, rank]);
            }
        }
    }

    #[test]
    fn king_attack_test() {
        let b = Board::from_fen("8/8/8/8/8/8/8/8 w KQkq - 0 1").unwrap();

        assert!(
            b.king_attack(0)
                == 0b_00000000_00000000_00000000_00000000_00000000_00000000_00000011_00000010
        );
        assert!(
            b.king_attack(8)
                == 0b_00000000_00000000_00000000_00000000_00000000_00000011_00000010_00000011
        );
        assert!(
            b.king_attack(8 + 3)
                == 0b_00000000_00000000_00000000_00000000_00000000_00011100_00010100_00011100
        );

        for i in 0..64 {
            assert!(b.king_attack(i) == KING_ATTACK_MASKS[i as usize]);
        }
    }

    #[test]
    fn hori_vert_attack_test() {
        let b = Board::from_fen("8/8/8/8/8/8/8/8 w KQkq - 0 1").unwrap();
        assert!(
            b.hori_vert_attack(0)
                == 0b_00000001_00000001_00000001_00000001_00000001_00000001_00000001_11111110
        );

        let b = Board::from_fen("8/pppppppp/8/8/8/8/8/8 w KQkq - 0 1").unwrap();
        assert!(
            b.hori_vert_attack(0)
                == 0b_00000000_00000001_00000001_00000001_00000001_00000001_00000001_11111110
        );
    }

    #[test]
    fn diag_attack_test() {
        let b = Board::from_fen("8/8/8/8/8/8/8/8 w KQkq - 0 1").unwrap();
        assert!(
            b.diag_attack(0)
                == 0b_10000000_01000000_00100000_00010000_00001000_00000100_00000010_00000000
        );

        let b = Board::from_fen("8/pppppppp/8/8/8/8/PPPPPPPP/8 w KQkq - 0 1").unwrap();
        assert!(
            b.diag_attack(8 * 2 + 4)
                == 0b_00000000_00000001_10000010_01000100_00101000_00000000_00101000_00000000
        );
    }

    #[test]
    fn pawn_attack_test() {
        let b = Board::from_fen("8/pppppppp/8/8/8/8/PPPPPPPP/8 w KQkq - 0 1").unwrap();
        assert!(
            b.pawn_attack(8)
                == 0b_00000000_00000000_00000000_00000000_00000000_00000010_00000000_00000000
        );

        assert!(
            b.pawn_attack(9)
                == 0b_00000000_00000000_00000000_00000000_00000000_00000101_00000000_00000000
        );
    }

    #[test]
    fn get_legal_moves_test() {
        let b = Board::from_fen(STARTING_FEN).unwrap();

        for i in b.get_legal_moves() {
            // println!("{:?}", i);
        }
    }
}
