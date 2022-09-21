use crate::{
    idx_to_square_str,
    piece::{Piece, PieceColor, PieceType},
    square_str_to_idx, Board, BLACK_KING_CASTLE_MASK, BLACK_QUEEN_CASTLE_MASK, BOARD_SIZE,
    WHITE_KING_CASTLE_MASK, WHITE_QUEEN_CASTLE_MASK, WHITE_TO_MOVE_MASK,
};

use lazy_static::lazy_static;
use regex::Regex;

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
            return Err("enpassant square is invalid"); // ok this could be a simple if statement, todo remove regex
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

        let mut ac_a_ca = match fen[1].chars().next().unwrap() {
            'w' => WHITE_TO_MOVE_MASK,
            'b' => 0,
            _ => return Err("next to move invalid"),
        };

        if fen[2].is_empty() {
            return Err("castle avaliability len can not be 0");
        }

        for c in fen[2].chars() {
            match c {
                'q' => ac_a_ca |= BLACK_QUEEN_CASTLE_MASK,
                'k' => ac_a_ca |= BLACK_KING_CASTLE_MASK,
                'Q' => ac_a_ca |= WHITE_QUEEN_CASTLE_MASK,
                'K' => ac_a_ca |= WHITE_KING_CASTLE_MASK,
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

        let b = Board {
            pieces,
            active_color_and_castle_avaliability: ac_a_ca,
            full_moves,
            half_moves,
            en_passant_square,
        };

        if !b.valid_kings() {
            return Err("kings not valid");
        }

        Ok(b)
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

        fen
    }
}

#[cfg(test)]
mod fen_tests {
    use crate::Board;

    #[test]
    fn fen_in_out() {
        let cases = [
            "rn1qkbnr/pppbp1pp/8/1B1pPp2/8/8/PPPP1PPP/RNBQK1NR w KQkq f6 0 4",
            "rn1qkbnr/pppbp1pp/8/1B1pPp2/8/8/PPPPKPPP/RNBQ2NR b kq - 1 4",
            "rn1qkbnr/ppp1p1pp/8/1b1pPp2/8/3P4/PPP1KPPP/RNBQ2NR b kq - 0 5",
            "rn1qkb1r/ppp1p1pp/5n2/1b1pPp2/8/3P4/PPP1KPPP/RNBQ2NR w kq - 1 6",
            "rn1qkbr1/ppp1p1pp/5n2/1b1pPp2/8/3P1N2/PPP1KPPP/RNBQ3R w q - 3 7",
            "rn1qkbr1/ppp1p1p1/5n2/1b1pP1Pp/5p2/3P1N2/PPP1KP1P/RNBQ3R w q h6 0 9",
        ];

        for case in cases {
            let out_fen = Board::from_fen(case).unwrap().to_fen();
            assert_eq!(out_fen, case);
        }
    }
}
