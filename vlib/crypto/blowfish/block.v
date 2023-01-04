module blowfish

// expand_key performs a key expansion on the given Blowfish cipher.
pub fn expand_key(key []u8, mut bf Blowfish) {
	mut j := 0
	for i := 0; i < 18; i++ {
		mut d := u32(0)
		for k := 0; k < 4; k++ {
			d = d << 8 | u32(key[j])
			j++
			if j >= key.len {
				j = 0
			}
		}
		bf.p[i] ^= d
	}

	mut l := u32(0)
	mut r := u32(0)
	for i := 0; i < 18; i += 2 {
		arr := setup_tables(l, r, mut bf)
		bf.p[i], bf.p[i + 1] = arr[0], arr[1]
	}

	for i := 0; i < 256; i += 2 {
		arr := setup_tables(l, r, mut bf)
		bf.s[0][i], bf.s[0][i + 1] = arr[0], arr[1]
	}
	for i := 0; i < 256; i += 2 {
		arr := setup_tables(l, r, mut bf)
		bf.s[1][i], bf.s[1][i + 1] = arr[0], arr[1]
	}
	for i := 0; i < 256; i += 2 {
		arr := setup_tables(l, r, mut bf)
		bf.s[2][i], bf.s[2][i + 1] = arr[0], arr[1]
	}
	for i := 0; i < 256; i += 2 {
		arr := setup_tables(l, r, mut bf)
		bf.s[3][i], bf.s[3][i + 1] = arr[0], arr[1]
	}
}

// expand_key_with_salt using salt to expand the key.
pub fn expand_key_with_salt(key []u8, salt []u8, mut bf Blowfish) {
	mut j := 0
	for i := 0; i < 18; i++ {
		bf.p[i] ^= get_next_word(key, &j)
	}

	j = 0

	mut l := u32(0)
	mut r := u32(0)
	for i := 0; i < 18; i += 2 {
		l ^= get_next_word(key, &j)
		r ^= get_next_word(key, &j)
		arr := setup_tables(l, r, mut bf)
		bf.p[i], bf.p[i + 1] = arr[0], arr[1]
	}

	for i := 0; i < 256; i += 2 {
		l ^= get_next_word(key, &j)
		r ^= get_next_word(key, &j)
		arr := setup_tables(l, r, mut bf)
		bf.s[0][i], bf.s[0][i + 1] = arr[0], arr[1]
	}
	for i := 0; i < 256; i += 2 {
		l ^= get_next_word(key, &j)
		r ^= get_next_word(key, &j)
		arr := setup_tables(l, r, mut bf)
		bf.s[1][i], bf.s[1][i + 1] = arr[0], arr[1]
	}
	for i := 0; i < 256; i += 2 {
		l ^= get_next_word(key, &j)
		r ^= get_next_word(key, &j)
		arr := setup_tables(l, r, mut bf)
		bf.s[2][i], bf.s[2][i + 1] = arr[0], arr[1]
	}
	for i := 0; i < 256; i += 2 {
		l ^= get_next_word(key, &j)
		r ^= get_next_word(key, &j)
		arr := setup_tables(l, r, mut bf)
		bf.s[3][i], bf.s[3][i + 1] = arr[0], arr[1]
	}
}

// setup_tables sets up the Blowfish cipher's pi and substitution tables.
fn setup_tables(l u32, r u32, mut bf Blowfish) []u32 {
	mut xl := l
	mut xr := r
	xl ^= bf.p[0]
	xr ^= ((bf.s[0][u8(xl >> 24)] + bf.s[1][u8(xl >> 16)]) ^ bf.s[2][u8(xl >> 8)]) +
		(bf.s[3][u8(xl)] ^ bf.p[1])
	xl ^= ((bf.s[0][u8(xr >> 24)] + bf.s[1][u8(xr >> 16)]) ^ bf.s[2][u8(xr >> 8)]) +
		(bf.s[3][u8(xr)] ^ bf.p[2])
	xr ^= ((bf.s[0][u8(xl >> 24)] + bf.s[1][u8(xl >> 16)]) ^ bf.s[2][u8(xl >> 8)]) +
		(bf.s[3][u8(xl)] ^ bf.p[3])
	xl ^= ((bf.s[0][u8(xr >> 24)] + bf.s[1][u8(xr >> 16)]) ^ bf.s[2][u8(xr >> 8)]) +
		(bf.s[3][u8(xr)] ^ bf.p[4])
	xr ^= ((bf.s[0][u8(xl >> 24)] + bf.s[1][u8(xl >> 16)]) ^ bf.s[2][u8(xl >> 8)]) +
		(bf.s[3][u8(xl)] ^ bf.p[5])
	xl ^= ((bf.s[0][u8(xr >> 24)] + bf.s[1][u8(xr >> 16)]) ^ bf.s[2][u8(xr >> 8)]) +
		(bf.s[3][u8(xr)] ^ bf.p[6])
	xr ^= ((bf.s[0][u8(xl >> 24)] + bf.s[1][u8(xl >> 16)]) ^ bf.s[2][u8(xl >> 8)]) +
		(bf.s[3][u8(xl)] ^ bf.p[7])
	xl ^= ((bf.s[0][u8(xr >> 24)] + bf.s[1][u8(xr >> 16)]) ^ bf.s[2][u8(xr >> 8)]) +
		(bf.s[3][u8(xr)] ^ bf.p[8])
	xr ^= ((bf.s[0][u8(xl >> 24)] + bf.s[1][u8(xl >> 16)]) ^ bf.s[2][u8(xl >> 8)]) +
		(bf.s[3][u8(xl)] ^ bf.p[9])
	xl ^= ((bf.s[0][u8(xr >> 24)] + bf.s[1][u8(xr >> 16)]) ^ bf.s[2][u8(xr >> 8)]) +
		(bf.s[3][u8(xr)] ^ bf.p[10])
	xr ^= ((bf.s[0][u8(xl >> 24)] + bf.s[1][u8(xl >> 16)]) ^ bf.s[2][u8(xl >> 8)]) +
		(bf.s[3][u8(xl)] ^ bf.p[11])
	xl ^= ((bf.s[0][u8(xr >> 24)] + bf.s[1][u8(xr >> 16)]) ^ bf.s[2][u8(xr >> 8)]) +
		(bf.s[3][u8(xr)] ^ bf.p[12])
	xr ^= ((bf.s[0][u8(xl >> 24)] + bf.s[1][u8(xl >> 16)]) ^ bf.s[2][u8(xl >> 8)]) +
		(bf.s[3][u8(xl)] ^ bf.p[13])
	xl ^= ((bf.s[0][u8(xr >> 24)] + bf.s[1][u8(xr >> 16)]) ^ bf.s[2][u8(xr >> 8)]) +
		(bf.s[3][u8(xr)] ^ bf.p[14])
	xr ^= ((bf.s[0][u8(xl >> 24)] + bf.s[1][u8(xl >> 16)]) ^ bf.s[2][u8(xl >> 8)]) +
		(bf.s[3][u8(xl)] ^ bf.p[15])
	xl ^= ((bf.s[0][u8(xr >> 24)] + bf.s[1][u8(xr >> 16)]) ^ bf.s[2][u8(xr >> 8)]) +
		(bf.s[3][u8(xr)] ^ bf.p[16])
	xr ^= bf.p[17]
	res := [xl, xr]
	return res
}

// get_next_word returns the next big-endian u32 value from the byte
// slice at the given position in a circular manner, updating the position.
fn get_next_word(b []u8, pos &int) u32 {
	mut w := u32(0)
	mut j := 0
	unsafe {
		j = *pos
	}
	for i := 0; i < 4; i++ {
		w = w << 8 | u32(b[j])
		j++
		if j >= b.len {
			j = 0
		}
	}
	unsafe {
		*pos = j
	}
	return w
}
