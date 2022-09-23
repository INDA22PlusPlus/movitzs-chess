use crate::*;

pub(crate) const KING_ATTACK_MASKS: [u64; 64] = [
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

impl Board {
    pub(crate) fn castling(&self, idx: u8) -> u64 {
        let k = self.pieces[idx as usize].unwrap();

        let ops_attacks = self
            .pieces
            .iter()
            .enumerate()
            .filter(|(_, p)| p.is_some() && p.unwrap().get_color() != self.get_active_color())
            .fold(0_u64, |acc, item| acc | 1 << item.0);

        let ranges = match k.get_color() {
            PieceColor::White => (
                (1..=3, 1, WHITE_KING_CASTLE_MASK),
                (5..=6, 6, WHITE_QUEEN_CASTLE_MASK),
            ),
            PieceColor::Black => (
                (57..=59, 57, BLACK_KING_CASTLE_MASK),
                (61..=62, 62, BLACK_QUEEN_CASTLE_MASK),
            ),
        };

        let mut result = 0;

        for mut range in [ranges.0, ranges.1] {
            if self.active_color_and_castle_avaliability & range.2 != 0
                && range
                    .0
                    .all(|i| self.pieces[i].is_none() && ops_attacks & 1 << i == 0)
            {
                result |= 1 << range.1;
            }
        }

        result
    }

    pub(crate) fn pawn_moves(&self, idx: u8) -> u64 {
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

        let mut result = 1 << (idx + dir);

        if ((color == PieceColor::Black && rank == '7')
            || (color == PieceColor::White && rank == '2'))
            && self.pieces[(idx + dir * 2) as usize].is_none()
        {
            result |= 1 << (idx + dir * 2);
        }

        result
    }

    pub(crate) fn pawn_attack(&self, idx: u8) -> u64 {
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

    pub(crate) fn diag_attack(&self, idx: u8) -> u64 {
        let mut result = 0;

        for f_dir in [-1_i8, 1] {
            for r_dir in [-1_i8, 1] {
                let [mut file, mut rank] = idx_to_square_str(idx);

                for _ in 1..8 {
                    file = ((file as i8) + f_dir) as u8 as char;
                    rank = ((rank as i8) + r_dir) as u8 as char;

                    if !('a'..='h').contains(&file) || !('1'..='8').contains(&rank) {
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

    pub(crate) fn knight_attack(&self, idx: u8) -> u64 {
        let [file, rank] = idx_to_square_str(idx);

        // these could also be pre-computed

        let mut result = 0;
        if file != 'a' && rank < '7' {
            // attack up left
            result |= 1 << (idx + 16 - 1);
        }
        if file > 'b' && rank != '8' {
            // attack left up
            result |= 1 << (idx + 8 - 2);
        }

        if file != 'h' && rank < '7' {
            // attack up right
            result |= 1 << (idx + 16 + 1);
        }
        if file < 'g' && rank != '8' {
            // attack right up
            result |= 1 << (idx + 8 + 2);
        }

        if file != 'a' && rank > '2' {
            // attack down left
            result |= 1 << (idx - 16 - 1);
        }
        if file > 'b' && rank != '1' {
            // attack left down
            result |= 1 << (idx - 8 - 2);
        }

        if file != 'h' && rank > '2' {
            // attack down right
            result |= 1 << (idx - 16 + 1);
        }
        if file < 'g' && rank != '1' {
            // attack right down
            result |= 1 << (idx - 8 + 2);
        }

        result
    }

    pub(crate) fn hori_vert_attack(&self, idx: u8) -> u64 {
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

                result |= 1 << n_idx;

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

                result |= 1 << n_idx;

                if self.pieces[n_idx as usize].is_some() {
                    break;
                }
            }
        }
        result
    }
}

#[allow(clippy::all)]
#[cfg(test)]
mod move_tests {
    use crate::{idx_to_square_str, moves::KING_ATTACK_MASKS, Board};

    #[test]
    fn king_attack_test() {
        assert_eq!(
            king_attack(0),
            0b_00000000_00000000_00000000_00000000_00000000_00000000_00000011_00000010
        );
        assert_eq!(
            king_attack(8),
            0b_00000000_00000000_00000000_00000000_00000000_00000011_00000010_00000011
        );
        assert_eq!(
            king_attack(8 + 3),
            0b_00000000_00000000_00000000_00000000_00000000_00011100_00010100_00011100
        );

        for i in 0..64 {
            assert_eq!(king_attack(i), KING_ATTACK_MASKS[i as usize]);
        }
    }
    // used when evaluating KING_ATTACK_MASKS
    fn king_attack(idx: u8) -> u64 {
        let mut result = 0;
        let [file, rank] = idx_to_square_str(idx);

        if rank > '1' {
            // attack down
            result |= 1 << (idx - 8);
        }
        if rank < '8' {
            // attack up
            result |= 1 << (idx + 8);
        }
        if file != 'a' {
            // attack left
            result |= 1 << (idx - 1);
        }
        if file != 'h' {
            // attack right
            result |= 1 << (idx + 1);
        }

        if rank != '1' && file != 'a' {
            // attack down left
            result |= 1 << (idx - 8 - 1);
        }
        if rank != '8' && file != 'a' {
            // attack up left
            result |= 1 << (idx + 8 - 1);
        }
        if rank != '1' && file != 'h' {
            // attack down right
            result |= 1 << (idx - 8 + 1);
        }
        if rank != '8' && file != 'h' {
            // attack up right
            result |= 1 << (idx + 8 + 1);
        }

        result
    }

    #[test]
    fn hori_vert_attack_test() {
        let b = Board::from_fen("k1K5/8/8/8/8/8/8/8 w KQkq - 0 1").unwrap(); // uhm
        assert_eq!(
            b.hori_vert_attack(0),
            0b_00000001_00000001_00000001_00000001_00000001_00000001_00000001_11111110
        );

        let b = Board::from_fen("k1K5/pppppppp/8/8/8/8/8/8 w KQkq - 0 1").unwrap();
        assert_eq!(
            b.hori_vert_attack(0),
            0b_00000000_00000001_00000001_00000001_00000001_00000001_00000001_11111110
        );
    }

    #[test]
    fn knight_attack_test() {
        let b = Board::default();

        for i in 0..64 {
            b.knight_attack(i);
        }
    }

    #[test]
    fn diag_attack_test() {
        let b = Board::from_fen("8/8/8/8/8/8/8/2k1K3 w KQkq - 0 1").unwrap();
        assert_eq!(
            b.diag_attack(0),
            0b_10000000_01000000_00100000_00010000_00001000_00000100_00000010_00000000
        );

        let b = Board::default();
        assert_eq!(
            b.diag_attack(8 * 2 + 4),
            0b_00000000_00000001_10000010_01000100_00101000_00000000_00101000_00000000
        );
    }

    #[test]
    fn pawn_attack_test() {
        let b = Board::from_fen("k7/pppppppp/8/8/8/8/PPPPPPPP/K7 w KQkq - 0 1").unwrap();
        assert_eq!(
            b.pawn_attack(8),
            0b_00000000_00000000_00000000_00000000_00000000_00000010_00000000_00000000
        );

        assert_eq!(
            b.pawn_attack(9),
            0b_00000000_00000000_00000000_00000000_00000000_00000101_00000000_00000000
        );
    }
}
