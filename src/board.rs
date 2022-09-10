use crate::piece::*;

const RANK_SIZE: usize = 8;
const BOARD_SIZE: usize = RANK_SIZE * RANK_SIZE;

const WHITE_TO_MOVE_MASK: u8 = 0b00000001;
const BLACK_QUEEN_CASTLE_MASK: u8 = 0b00000010;
const BLACK_KING_CASTLE_MASK: u8 = 0b00000100;
const WHITE_QUEEN_CASTLE_MASK: u8 = 0b00001000;
const WHITE_KING_CASTLE_MASK: u8 = 0b00010000;

#[derive(Debug)]
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

    rank * (RANK_SIZE as u8) + file
}

impl Board {
    pub fn from_fen(fen: &str) -> Result<Self, &'static str> {
        // Reference: https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation

        const INIT: Option<Piece> = None; // https://github.com/rust-lang/rust/issues/44796
        let mut pieces = [INIT; BOARD_SIZE];

        let mut i = 0;
        let fen: Vec<char> = fen.chars().collect();

        let mut counter = BOARD_SIZE; // fen starts at rank 8
        while fen[i] != ' ' {
            let c = fen[i];
            i += 1;

            if c == '/' {
                // we don't need to keep track of the ranks IF the fen 100% is correct
                // a human would understand ".../4p/..." but this parser would not
                continue;
            }
            if c.is_numeric() {
                counter -= c.to_digit(10).unwrap() as usize;
                continue;
            }
            if c.is_alphabetic() {
                let color = match c.is_lowercase() {
                    true => PieceColor::Black,
                    false => PieceColor::White,
                };
                let piece_type = match c.to_lowercase().nth(0).unwrap() {
                    'p' => PieceType::Pawn,
                    'r' => PieceType::Rook,
                    'n' => PieceType::Knight,
                    'b' => PieceType::Bishop,
                    'k' => PieceType::King,
                    'q' => PieceType::Queen,
                    _ => return Err("unknown piece"),
                };
                pieces[counter - 1] = Some(Piece::new(piece_type, color));
                counter -= 1;
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

    fn get_piece_at() -> Piece {
        Piece::new(PieceType::Rook, PieceColor::White) //todo impl
    }
}

#[cfg(test)]
mod tests {
    use super::Board;

    #[test]
    fn fen_test() {
        // standard initial setup
        let b =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
    }
}
