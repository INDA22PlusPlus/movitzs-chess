use std::fmt::Debug;

use crate::{idx_to_square_str, piece::PieceType};

pub struct CMove {
    // ... could fit some metadata here, maybe w/b to move? promotion?
    pub from: u8,
    pub to: u8,
    pub promote_to: PieceType,
}

impl CMove {}

impl Debug for CMove {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CMove")
            .field("from", &idx_to_square_str(self.from))
            .field("to", &idx_to_square_str(self.to))
            .finish()
    }
}
