const BLOCK_SIZE: usize = 64;

const ROUND_SHIFT: [usize; BLOCK_SIZE] = [
    7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21,
];

// (1..=64).into_iter().map(|i| {
//     let x = (2u64 << 31) as f64;
//     let y = (i as f64).sin().abs();
//     (x * y).floor() as u32
// }).collect::<Vec<_>>()
const SINES: [u32; BLOCK_SIZE] = [
    0xD76AA478, 0xE8C7B756, 0x242070DB, 0xC1BDCEEE, 0xF57C0FAF, 0x4787C62A, 0xA8304613, 0xFD469501,
    0x698098D8, 0x8B44F7AF, 0xFFFF5BB1, 0x895CD7BE, 0x6B901122, 0xFD987193, 0xA679438E, 0x49B40821,
    0xF61E2562, 0xC040B340, 0x265E5A51, 0xE9B6C7AA, 0xD62F105D, 0x02441453, 0xD8A1E681, 0xE7D3FBC8,
    0x21E1CDE6, 0xC33707D6, 0xF4D50D87, 0x455A14ED, 0xA9E3E905, 0xFCEFA3F8, 0x676F02D9, 0x8D2A4C8A,
    0xFFFA3942, 0x8771F681, 0x6D9D6122, 0xFDE5380C, 0xA4BEEA44, 0x4BDECFA9, 0xF6BB4B60, 0xBEBFBC70,
    0x289B7EC6, 0xEAA127FA, 0xD4EF3085, 0x04881D05, 0xD9D4D039, 0xE6DB99E5, 0x1FA27CF8, 0xC4AC5665,
    0xF4292244, 0x432AFF97, 0xAB9423A7, 0xFC93A039, 0x655B59C3, 0x8F0CCC92, 0xFFEFF47D, 0x85845DD1,
    0x6FA87E4F, 0xFE2CE6E0, 0xA3014314, 0x4E0811A1, 0xF7537E82, 0xBD3AF235, 0x2AD7D2BB, 0xEB86D391,
];

const PADDING: [u8; BLOCK_SIZE] = [
    0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
];

#[inline(always)]
fn leftrotate(x: u32, amount: usize) -> u32 {
    (x << amount) | (x >> (32 - amount))
}

pub fn hexdigest<B>(message: B) -> String
    where B: AsRef<[u8]>
{
    use std::{convert::TryInto, fmt::Write};

    let message = message.as_ref();
    let message_len = message.len();

    let pad_len = 56 - (message_len % 56);

    let input = [&message[..], &PADDING[..pad_len], &(message_len * 8).to_le_bytes()].concat();

    // Start state
    let mut a0 = 0x67452301_u32;
    let mut b0 = 0xEFCDAB89_u32;
    let mut c0 = 0x98BADCFE_u32;
    let mut d0 = 0x10325476_u32;

    for block in input.chunks(BLOCK_SIZE) {
        let m = block.chunks(4).map(|c| {
            u32::from_le_bytes(c.try_into().unwrap())
        }).collect::<Vec<_>>();

        let (mut a, mut b, mut c, mut d) = (a0, b0, c0, d0);

        for i in 0..BLOCK_SIZE {
            let (f, g) = match i {
                 0..=15 => ((b & c) | (!b & d), i),
                16..=31 => ((b & d) | (c & !d), 5 * i + 1),
                32..=47 => (b ^ c ^ d, 3 * i + 5),
                48..=63 => (c ^ (b | !d), 7 * i),
                _ => unreachable!()
            };

            let f = f.wrapping_add(a).wrapping_add(SINES[i]).wrapping_add(m[g % 16]);
            a = d;
            d = c;
            c = b;
            b = b.wrapping_add(leftrotate(f, ROUND_SHIFT[i]));
        }

        a0 = a0.wrapping_add(a);
        b0 = b0.wrapping_add(b);
        c0 = c0.wrapping_add(c);
        d0 = d0.wrapping_add(d);
    }

    let digest = [a0.to_le_bytes(), b0.to_le_bytes(), c0.to_le_bytes(), d0.to_le_bytes()].concat();
    let hex_len = digest.len();

    digest.into_iter().fold(String::with_capacity(hex_len), |mut acc, val| {
        write!(acc, "{:02x}", val).unwrap();
        acc
    })
}
