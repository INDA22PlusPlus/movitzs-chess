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

#[derive(Debug)]
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

fn square_str_to_idx(square: &[char]) -> u8 {
    match square[0] {
        'a'..='h' => true,
        _ => panic!("square file not a-h"),
    };
    let file: u8 = (square[0] as u8) - 'a' as u8;

    let rank: u8 = (square[1] as u8) - '1' as u8;
    assert!(rank < 8, "square rank not 1-8");

    rank * (RANK_SIZE as u8) + file
}

fn idx_to_square_str(idx: u8) -> [char; 2] {
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
        let fen: Vec<char> = fen.chars().collect();

        const INIT: Option<Piece> = None; // https://github.com/rust-lang/rust/issues/44796
        let mut pieces = [INIT; BOARD_SIZE];

        let mut i = 0;
        let mut rank = 7;
        let mut file = 0;

        while fen[i] != ' ' {
            let c = fen[i];
            i += 1;

            if c == '/' {
                rank -= 1;
                file = 0;
                continue;
            }
            if c.is_numeric() {
                file += c.to_digit(10).unwrap() as usize;
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
                pieces[rank * 8 + file] = Some(Piece::new(piece_type, color));
                file += 1;
                continue;
            }

            return Err("unknown character in piece placement string");
        }

        i += 1; // skip whitespace

        let mut ac_a_ca = match fen[i] {
            'w' => WHITE_TO_MOVE_MASK,
            'b' => 0,
            _ => return Err("next to move invalid"),
        };

        i += 2; // skip whitespace

        while fen[i] != ' ' {
            match fen[i] {
                'q' => ac_a_ca ^= BLACK_QUEEN_CASTLE_MASK,
                'k' => ac_a_ca ^= BLACK_KING_CASTLE_MASK,
                'Q' => ac_a_ca ^= WHITE_QUEEN_CASTLE_MASK,
                'K' => ac_a_ca ^= WHITE_KING_CASTLE_MASK,
                '-' => {}
                _ => {
                    return Err("castle avaliability invalid");
                }
            }
            i += 1;
        }

        i += 1; // skip whitespace

        let parts: Vec<&[char]> = fen[i..].split(|y| *y == ' ').collect();

        let half_moves = match parts[1].iter().collect::<String>().parse() {
            Ok(hm) => hm,
            Err(_) => {
                return Err("half moves not an int");
            }
        };
        let full_moves = match parts[2].iter().collect::<String>().parse() {
            Ok(fm) => fm,
            Err(_) => {
                return Err("full moves not an int");
            }
        };

        let en_passant_square = match parts[0] {
            ['-'] => u8::MAX,
            _ => square_str_to_idx(parts[0]),
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

        fen.push(char::from_digit(self.half_moves as u32, 10).unwrap());
        fen.push(' ');
        fen.push(char::from_digit(self.full_moves as u32, 10).unwrap());

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

                if self.pieces[to].is_none() || self.pieces[to].as_ref().unwrap().get_color() != ac
                {
                    // the attacked square is empty or an opponents piece
                    moves.push(CMove {
                        from: from as u8,
                        to: to as u8,
                    });
                }
            }
        }

        moves
    }

    fn board_is_legal_after_move(&self) -> bool {
        // check that the one who just moved isn't in check
        // could probably do some bitmask magic here, where the attacks in a u64 is and:ed with the king position

        let white_to_move = self.active_color_and_castle_avaliability & WHITE_TO_MOVE_MASK != 0;

        for p in &self.pieces {
            if p.is_none() {
                continue;
            };
            let p = p.as_ref().unwrap();

            if (p.get_color() == PieceColor::White) == white_to_move {
                // piece of the player who just moved
                continue;
            }
        }

        false
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
            PieceType::Pawn => self.pawn_attack(idx, p), // this should also a constant, like the king values
            PieceType::Rook => self.hori_vert_attack(idx),
            PieceType::Knight => self.knight_attack(idx), // this too
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

        match piece.get_type() {
            PieceType::Pawn => {}
            PieceType::Rook => {}
            PieceType::Knight => {}
            PieceType::Bishop => {}
            PieceType::Queen => {}
            PieceType::King => {}
        }

        Vec::new()
    }

    fn pawn_attack(&self, idx: u8, p: &Piece) -> u64 {
        let mut result = 0;
        let [file, _] = idx_to_square_str(idx);

        // todo, this could be done beautifully branchless
        for x in [1, -1_i8] {
            match p.get_color() {
                PieceColor::Black => {
                    if file == 'a' {
                        continue;
                    }
                    let i = (idx + 8) as i8 + x;
                    if (i / 8) as u8 == (idx + 8) / 8 {
                        result ^= 1 << i;
                    }
                }
                PieceColor::White => {
                    if file == 'h' {
                        continue;
                    }
                    let i = (idx - 8) as i8 + x;
                    if (i / 8) as u8 == (idx - 8) / 8 {
                        result ^= 1 << i;
                    }
                }
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

                if n_idx > BOARD_SIZE as u8 {
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
    use crate::board::KING_ATTACK_MASKS;

    use super::{idx_to_square_str, square_str_to_idx, Board, BOARD_SIZE};

    #[test]
    fn to_idx_test() {
        // standard initial setup
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
        // _11111111
        // _00000000

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
    fn get_legal_moves_test() {}
}
