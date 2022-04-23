module strconv

import strings

pub fn format_str_sb(s string, p BF_param, mut sb strings.Builder) {
	if p.len0 <= 0 {
		sb.write_string(s)
		return
	}

	dif := p.len0 - utf8_str_visible_length(s)

	if dif <= 0 {
		sb.write_string(s)
		return
	}

	if p.allign == .right {
		for i1 := 0; i1 < dif; i1++ {
			sb.write_u8(p.pad_ch)
		}
	}

	sb.write_string(s)

	if p.allign == .left {
		for i1 := 0; i1 < dif; i1++ {
			sb.write_u8(p.pad_ch)
		}
	}
}

const (
	max_size_f64_char = 32 // the f64 max representation is -36,028,797,018,963,968e1023, 21 chars, 32 is faster for the memory manger
	// digit pairs in reverse order
	digit_pairs       = '00102030405060708090011121314151617181910212223242526272829203132333435363738393041424344454647484940515253545556575859506162636465666768696071727374757677787970818283848586878889809192939495969798999'
)

// format_dec_sb format a u64
[direct_array_access]
pub fn format_dec_sb(d u64, p BF_param, mut res strings.Builder) {
	mut n_char := dec_digits(d)
	sign_len := if !p.positive || p.sign_flag { 1 } else { 0 }
	number_len := sign_len + n_char
	dif := p.len0 - number_len
	mut sign_written := false

	if p.allign == .right {
		if p.pad_ch == `0` {
			if p.positive {
				if p.sign_flag {
					res.write_u8(`+`)
					sign_written = true
				}
			} else {
				res.write_u8(`-`)
				sign_written = true
			}
		}
		// write the pad chars
		for i1 := 0; i1 < dif; i1++ {
			res.write_u8(p.pad_ch)
		}
	}

	if !sign_written {
		// no pad char, write the sign before the number
		if p.positive {
			if p.sign_flag {
				res.write_u8(`+`)
			}
		} else {
			res.write_u8(`-`)
		}
	}

	// Legacy version
	// max u64 18446744073709551615 => 20 byte
	mut buf := [32]u8{}
	mut i := 20
	mut d1 := d
	for i >= (21 - n_char) {
		buf[i] = u8(d1 % 10) + `0`

		d1 = d1 / 10
		i--
	}

	for _ in 0 .. n_char {
		i++
		res.write_u8(buf[i])
	}
	i++

	//===========================================

	if p.allign == .left {
		for i1 := 0; i1 < dif; i1++ {
			res.write_u8(p.pad_ch)
		}
	}
	return
}
