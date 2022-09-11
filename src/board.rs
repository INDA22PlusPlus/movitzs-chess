use crate::piece::*;

const RANK_SIZE: usize = 8;
const BOARD_SIZE: usize = RANK_SIZE * RANK_SIZE;

const WHITE_TO_MOVE_MASK: u8 = 0b00000001;
const BLACK_QUEEN_CASTLE_MASK: u8 = 0b00000010;
const BLACK_KING_CASTLE_MASK: u8 = 0b00000100;
const WHITE_QUEEN_CASTLE_MASK: u8 = 0b00001000;
const WHITE_KING_CASTLE_MASK: u8 = 0b00010000;

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

    let rank = square[1].to_digit(10).expect("square rank not digit") as u8;
    assert!(rank - 1 < 8, "square rank not 1-8");

    (rank - 1) * (RANK_SIZE as u8) + file
}

fn idx_to_square_str(idx: u8) -> [char; 2] {
    let file = ('a' as u8 + idx % 8) as char;
    let rank = char::from_digit((idx / 8) as u32 + 1, 10).unwrap();

    [file, rank]
}

#[allow(dead_code)]
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

    fn to_fen(&self) -> String {
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
            "8/8/8/8/8/8/8/8 w - - 0 1"
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
    use super::{idx_to_square_str, square_str_to_idx, BOARD_SIZE};

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
}
