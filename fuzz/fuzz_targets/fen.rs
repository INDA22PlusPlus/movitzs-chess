#![no_main]
extern crate hw1_chess as chess;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
        let b = chess::Board::from_fen(s);
        if b.is_ok() {
            let b = b.unwrap();
            let _ = b.to_fen();

            b.get_legal_moves();
        }
    }
});
