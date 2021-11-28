module blowfish

pub fn expand_key(key []byte, mut bf Blowfish) {
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
		arr := encrypt_block(l, r, mut bf)
		bf.p[i], bf.p[i + 1] = arr[0], arr[1]
	}

	for i := 0; i < 256; i += 2 {
		arr := encrypt_block(l, r, mut bf)
		bf.s[0][i], bf.s[0][i + 1] = arr[0], arr[1]
	}
	for i := 0; i < 256; i += 2 {
		arr := encrypt_block(l, r, mut bf)
		bf.s[1][i], bf.s[1][i + 1] = arr[0], arr[1]
	}
	for i := 0; i < 256; i += 2 {
		arr := encrypt_block(l, r, mut bf)
		bf.s[2][i], bf.s[2][i + 1] = arr[0], arr[1]
	}
	for i := 0; i < 256; i += 2 {
		arr := encrypt_block(l, r, mut bf)
		bf.s[3][i], bf.s[3][i + 1] = arr[0], arr[1]
	}
}

pub fn expand_key_with_salt(key []byte, salt []byte, mut bf Blowfish) {
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
		arr := encrypt_block(l, r, mut bf)
		bf.p[i], bf.p[i + 1] = arr[0], arr[1]
	}

	for i := 0; i < 256; i += 2 {
		l ^= get_next_word(key, &j)
		r ^= get_next_word(key, &j)
		arr := encrypt_block(l, r, mut bf)
		bf.s[0][i], bf.s[0][i + 1] = arr[0], arr[1]
	}
	for i := 0; i < 256; i += 2 {
		l ^= get_next_word(key, &j)
		r ^= get_next_word(key, &j)
		arr := encrypt_block(l, r, mut bf)
		bf.s[1][i], bf.s[1][i + 1] = arr[0], arr[1]
	}
	for i := 0; i < 256; i += 2 {
		l ^= get_next_word(key, &j)
		r ^= get_next_word(key, &j)
		arr := encrypt_block(l, r, mut bf)
		bf.s[2][i], bf.s[2][i + 1] = arr[0], arr[1]
	}
	for i := 0; i < 256; i += 2 {
		l ^= get_next_word(key, &j)
		r ^= get_next_word(key, &j)
		arr := encrypt_block(l, r, mut bf)
		bf.s[3][i], bf.s[3][i + 1] = arr[0], arr[1]
	}
}

fn encrypt_block(l u32, r u32, mut bf Blowfish) []u32 {
	mut xl := l
	mut xr := r
	xl ^= bf.p[0]
	xr ^= ((bf.s[0][byte(xl >> 24)] + bf.s[1][byte(xl >> 16)]) ^ bf.s[2][byte(xl >> 8)]) +
		bf.s[3][byte(xl)] ^ bf.p[1]
	xl ^= ((bf.s[0][byte(xr >> 24)] + bf.s[1][byte(xr >> 16)]) ^ bf.s[2][byte(xr >> 8)]) +
		bf.s[3][byte(xr)] ^ bf.p[2]
	xr ^= ((bf.s[0][byte(xl >> 24)] + bf.s[1][byte(xl >> 16)]) ^ bf.s[2][byte(xl >> 8)]) +
		bf.s[3][byte(xl)] ^ bf.p[3]
	xl ^= ((bf.s[0][byte(xr >> 24)] + bf.s[1][byte(xr >> 16)]) ^ bf.s[2][byte(xr >> 8)]) +
		bf.s[3][byte(xr)] ^ bf.p[4]
	xr ^= ((bf.s[0][byte(xl >> 24)] + bf.s[1][byte(xl >> 16)]) ^ bf.s[2][byte(xl >> 8)]) +
		bf.s[3][byte(xl)] ^ bf.p[5]
	xl ^= ((bf.s[0][byte(xr >> 24)] + bf.s[1][byte(xr >> 16)]) ^ bf.s[2][byte(xr >> 8)]) +
		bf.s[3][byte(xr)] ^ bf.p[6]
	xr ^= ((bf.s[0][byte(xl >> 24)] + bf.s[1][byte(xl >> 16)]) ^ bf.s[2][byte(xl >> 8)]) +
		bf.s[3][byte(xl)] ^ bf.p[7]
	xl ^= ((bf.s[0][byte(xr >> 24)] + bf.s[1][byte(xr >> 16)]) ^ bf.s[2][byte(xr >> 8)]) +
		bf.s[3][byte(xr)] ^ bf.p[8]
	xr ^= ((bf.s[0][byte(xl >> 24)] + bf.s[1][byte(xl >> 16)]) ^ bf.s[2][byte(xl >> 8)]) +
		bf.s[3][byte(xl)] ^ bf.p[9]
	xl ^= ((bf.s[0][byte(xr >> 24)] + bf.s[1][byte(xr >> 16)]) ^ bf.s[2][byte(xr >> 8)]) +
		bf.s[3][byte(xr)] ^ bf.p[10]
	xr ^= ((bf.s[0][byte(xl >> 24)] + bf.s[1][byte(xl >> 16)]) ^ bf.s[2][byte(xl >> 8)]) +
		bf.s[3][byte(xl)] ^ bf.p[11]
	xl ^= ((bf.s[0][byte(xr >> 24)] + bf.s[1][byte(xr >> 16)]) ^ bf.s[2][byte(xr >> 8)]) +
		bf.s[3][byte(xr)] ^ bf.p[12]
	xr ^= ((bf.s[0][byte(xl >> 24)] + bf.s[1][byte(xl >> 16)]) ^ bf.s[2][byte(xl >> 8)]) +
		bf.s[3][byte(xl)] ^ bf.p[13]
	xl ^= ((bf.s[0][byte(xr >> 24)] + bf.s[1][byte(xr >> 16)]) ^ bf.s[2][byte(xr >> 8)]) +
		bf.s[3][byte(xr)] ^ bf.p[14]
	xr ^= ((bf.s[0][byte(xl >> 24)] + bf.s[1][byte(xl >> 16)]) ^ bf.s[2][byte(xl >> 8)]) +
		bf.s[3][byte(xl)] ^ bf.p[15]
	xl ^= ((bf.s[0][byte(xr >> 24)] + bf.s[1][byte(xr >> 16)]) ^ bf.s[2][byte(xr >> 8)]) +
		bf.s[3][byte(xr)] ^ bf.p[16]
	xr ^= bf.p[17]
	return [xl, xr]
}

fn get_next_word(b []byte, pos &int) u32 {
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
