mod piece;
mod board;

use crate::{board::Board};


fn main() {
    let  p = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    println!("{p:?}");
    // p.taint_move();
    println!("{p:?}");
}
