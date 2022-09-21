use std::fmt;

#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
pub enum PieceType {
    Pawn = 0,
    Rook,
    Bishop,
    Knight,
    Queen,
    King,
}

#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
pub enum PieceColor {
    White = 0,
    Black,
}

#[derive(Clone, Copy)]
pub struct Piece(u8);

const PIECE_MASK: u8 = 0b00000111;
const COLOR_MASK: u8 = 0b00001000;
// soo much real-estate here...

impl Piece {
    pub fn new(piece_type: PieceType, color: PieceColor) -> Self {
        let mut tmp: u8 = 0;
        if color == PieceColor::White {
            tmp ^= COLOR_MASK;
        }
        tmp ^= piece_type as u8;
        Piece(tmp)
    }

    pub fn get_type(&self) -> PieceType {
        match (self.0 as u8) & PIECE_MASK {
            0 => PieceType::Pawn,
            1 => PieceType::Rook,
            2 => PieceType::Bishop,
            3 => PieceType::Knight,
            4 => PieceType::Queen,
            5 => PieceType::King,
            _ => panic!("unknown piece type"),
        }
    }

    pub fn get_color(&self) -> PieceColor {
        if (self.0 as u8) & COLOR_MASK == 0 {
            return PieceColor::Black;
        }
        PieceColor::White
    }
}

impl fmt::Debug for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Piece")
            .field("type", &self.get_type())
            .field("color", &self.get_color())
            .finish()
    }
}
