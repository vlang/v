module mlkem

import crypto.rand
import crypto.sha3

// ML-KEM-768 parameters (FIPS 203).
pub const n = 256
pub const q = 3329
pub const k = 3
pub const eta1 = 2
pub const eta2 = 2
pub const du = 10
pub const dv = 4

pub const ek_size = 1184
pub const dk_size = 2400
pub const ct_size = 1088
pub const ss_size = 32

pub type Poly = [n]int
pub type PolyVec = [k]Poly
pub type PolyMat = [k][k]Poly

// ---------- polynomial arithmetic ----------

pub fn poly_add(a Poly, b Poly) Poly {
	mut r := Poly{}
	for i in 0 .. n {
		r[i] = (a[i] + b[i]) % q
	}
	return r
}

pub fn poly_sub(a Poly, b Poly) Poly {
	mut r := Poly{}
	for i in 0 .. n {
		r[i] = (a[i] - b[i] + q) % q
	}
	return r
}

pub fn poly_mul(a Poly, b Poly) Poly {
	mut r := [2 * n - 1]int{}
	for i in 0 .. n {
		if a[i] == 0 {
			continue
		}
		for j in 0 .. n {
			r[i + j] = (r[i + j] + a[i] * b[j]) % q
		}
	}
	mut out := Poly{}
	for i in 0 .. n {
		out[i] = r[i]
	}
	for i in n .. 2 * n - 1 {
		out[i - n] = (out[i - n] - r[i] + q) % q
	}
	return out
}

pub fn poly_scalar_mul(a Poly, s int) Poly {
	mut r := Poly{}
	for i in 0 .. n {
		r[i] = (a[i] * s) % q
	}
	return r
}

// ---------- NTT (fast, 7-layer CT / GS) ----------
// Zeta table: 128 entries in bit-reversed 7-bit order, zetas[i] = 17^bitrev7(i) mod q.

const zetas = [int(1), 1729, 2580, 3289, 2642, 630, 1897, 848, 1062, 1919, 193, 797, 2786, 3260,
	569, 1746, 296, 2447, 1339, 1476, 3046, 56, 2240, 1333, 1426, 2094, 535, 2882, 2393, 2879,
	1974, 821, 289, 331, 3253, 1756, 1197, 2304, 2277, 2055, 650, 1977, 2513, 632, 2865, 33, 1320,
	1915, 2319, 1435, 807, 452, 1438, 2868, 1534, 2402, 2647, 2617, 1481, 648, 2474, 3110, 1227,
	910, 17, 2761, 583, 2649, 1637, 723, 2288, 1100, 1409, 2662, 3281, 233, 756, 2156, 3015, 3050,
	1703, 1651, 2789, 1789, 1847, 952, 1461, 2687, 939, 2308, 2437, 2388, 733, 2337, 268, 641,
	1584, 2298, 2037, 3220, 375, 2549, 2090, 1645, 1063, 319, 2773, 757, 2099, 561, 2466, 2594,
	2804, 1092, 403, 1026, 1143, 2150, 2775, 886, 1722, 1212, 1874, 1029, 2110, 2935, 885, 2154]

fn modpow(a int, e int, m int) int {
	mut r := 1
	mut base := a % m
	mut exp := e
	for exp > 0 {
		if exp & 1 == 1 {
			r = (r * base) % m
		}
		base = (base * base) % m
		exp >>= 1
	}
	return r
}

pub fn ntt(mut f Poly) {
	mut k := 1
	mut length := 128
	for length >= 2 {
		for start := 0; start < 256; start += 2 * length {
			z := zetas[k]
			k++
			for j := start; j < start + length; j++ {
				u := f[j]
				t := (f[j + length] * z) % q
				f[j] = (u + t) % q
				f[j + length] = (u - t + q) % q
			}
		}
		length >>= 1
	}
}

pub fn inv_ntt(mut f Poly) {
	mut k := 127
	mut length := 2
	for length <= 128 {
		for start := 0; start < 256; start += 2 * length {
			z := zetas[k]
			k--
			for j := start; j < start + length; j++ {
				u := f[j]
				v := f[j + length]
				f[j] = (u + v) % q
				f[j + length] = ((v - u + q) * z) % q
			}
		}
		length <<= 1
	}
	ci := modpow(128, q - 2, q)
	for i in 0 .. n {
		f[i] = (f[i] * ci) % q
	}
}

// ---------- compression ----------

pub fn compress(x int, d int) int {
	mut v := (x * (1 << d) + q / 2) / q
	v = v % (1 << d)
	return v
}

pub fn decompress(y int, d int) int {
	return (y * q + (1 << (d - 1))) >> d
}

// ---------- CBD sampling (eta=2) ----------

pub fn sample_cbd2(b []u8) Poly {
	mut f := Poly{}
	for i in 0 .. n {
		byte_idx := (4 * i) / 8
		bit_off := (4 * i) % 8
		mut val := 0
		for j in 0 .. 4 {
			pos := byte_idx * 8 + bit_off + j
			bi := pos / 8
			bo := pos % 8
			if bi < b.len {
				val |= ((int(b[bi]) >> bo) & 1) << j
			}
		}
		a := (val & 1) + ((val >> 1) & 1)
		bb := ((val >> 2) & 1) + ((val >> 3) & 1)
		f[i] = (a - bb + q) % q
	}
	return f
}

// ---------- byte encoding ----------

pub fn poly_compress(f Poly, d int) []u8 {
	mut out := []u8{}
	mut acc := 0
	mut bits := 0
	for i in 0 .. n {
		c := compress(f[i], d)
		for j in 0 .. d {
			if (c >> j) & 1 == 1 {
				acc |= 1 << bits
			}
			bits++
			if bits == 8 {
				out << u8(acc)
				acc = 0
				bits = 0
			}
		}
	}
	if bits > 0 {
		out << u8(acc)
	}
	return out
}

pub fn poly_decompress(b []u8, d int) Poly {
	mut f := Poly{}
	mut acc := 0
	mut bits := 0
	mut byte_idx := 0
	for i in 0 .. n {
		mut c := 0
		for j in 0 .. d {
			if bits == 0 {
				if byte_idx < b.len {
					acc = int(b[byte_idx])
					byte_idx++
				}
				bits = 8
			}
			if (acc & 1) == 1 {
				c |= 1 << j
			}
			acc >>= 1
			bits--
		}
		f[i] = decompress(c, d)
	}
	return f
}

pub fn poly_to_bytes(f Poly) []u8 {
	mut out := []u8{len: 3 * n / 2}
	for i in 0 .. n / 2 {
		a := f[2 * i]
		bb := f[2 * i + 1]
		out[3 * i] = u8(a)
		out[3 * i + 1] = u8(((a >> 8) & 0x0f) | ((bb & 0x0f) << 4))
		out[3 * i + 2] = u8(bb >> 4)
	}
	return out
}

pub fn bytes_to_poly(b []u8) Poly {
	mut f := Poly{}
	for i in 0 .. n / 2 {
		f[2 * i] = int(b[3 * i]) | ((int(b[3 * i + 1]) & 0x0f) << 8)
		f[2 * i + 1] = (int(b[3 * i + 1]) >> 4) | (int(b[3 * i + 2]) << 4)
	}
	return f
}

// ---------- SHAKE helpers ----------

fn shake128_xof(input []u8, out_len int) []u8 {
	mut s := sha3.new_shake128()
	s.write(input)
	return s.read(out_len)
}

fn shake256_xof(input []u8, out_len int) []u8 {
	mut s := sha3.new_shake256()
	s.write(input)
	return s.read(out_len)
}

fn sha3_256(data []u8) []u8 {
	return sha3.sum256(data)
}

fn sha3_512(data []u8) []u8 {
	return sha3.sum512(data)
}

// ---------- SampleNTT (SHAKE-128 rejection sampling) ----------

fn sample_ntt(rho []u8, i int, j int) Poly {
	mut input := []u8{}
	input << rho
	input << u8(j)
	input << u8(i)
	mut out := Poly{}
	mut ctr := 0
	mut buf := shake128_xof(input, 3 * 256)
	mut pos := 0
	for ctr < n {
		if pos + 2 > buf.len {
			buf = shake128_xof(input, buf.len + 3 * 256)
		}
		d := int(buf[pos]) | (int(buf[pos + 1]) << 8)
		pos += 2
		if d < q {
			out[ctr] = d
			ctr++
		}
	}
	return out
}

// ---------- K-PKE ----------

pub fn kpke_keygen(d []u8) !([]u8, []u8) {
	g := sha3_512(d)
	rho := g[..32].clone()
	sigma := g[32..].clone()
	mut a := PolyMat{}
	for i in 0 .. k {
		for j in 0 .. k {
			a[i][j] = sample_ntt(rho, i, j)
		}
	}
	mut s := PolyVec{}
	mut nctr := 0
	for i in 0 .. k {
		mut inbuf := []u8{}
		inbuf << sigma
		inbuf << u8(nctr)
		nctr++
		b := shake256_xof(inbuf, 64 * eta1)
		s[i] = sample_cbd2(b)
	}
	mut e := PolyVec{}
	for i in 0 .. k {
		mut inbuf := []u8{}
		inbuf << sigma
		inbuf << u8(nctr)
		nctr++
		b := shake256_xof(inbuf, 64 * eta1)
		e[i] = sample_cbd2(b)
	}
	mut s_hat := PolyVec{}
	mut e_hat := PolyVec{}
	for i in 0 .. k {
		s_hat[i] = s[i]
		ntt(mut s_hat[i])
		e_hat[i] = e[i]
		ntt(mut e_hat[i])
	}
	mut t_hat := PolyVec{}
	for i in 0 .. k {
		mut acc := Poly{}
		for j in 0 .. k {
			for l in 0 .. n {
				acc[l] = (acc[l] + a[i][j][l] * s_hat[j][l]) % q
			}
		}
		for l in 0 .. n {
			t_hat[i][l] = (acc[l] + e_hat[i][l]) % q
		}
	}
	mut ek := []u8{}
	for i in 0 .. k {
		ek << poly_to_bytes(t_hat[i])
	}
	ek << rho
	mut dk := []u8{}
	for i in 0 .. k {
		dk << poly_to_bytes(s_hat[i])
	}
	return ek, dk
}

pub fn kpke_encrypt(ek []u8, m []u8, r []u8) ![]u8 {
	mut t_hat := PolyVec{}
	mut off := 0
	for i in 0 .. k {
		t_hat[i] = bytes_to_poly(ek[off..off + 3 * n / 2])
		off += 3 * n / 2
	}
	rho := ek[off..off + 32]
	mut a := PolyMat{}
	for i in 0 .. k {
		for j in 0 .. k {
			a[i][j] = sample_ntt(rho, i, j)
		}
	}
	mut rr := PolyVec{}
	mut nctr := 0
	for i in 0 .. k {
		mut inbuf := []u8{}
		inbuf << r
		inbuf << u8(nctr)
		nctr++
		b := shake256_xof(inbuf, 64 * eta1)
		rr[i] = sample_cbd2(b)
	}
	mut e1 := PolyVec{}
	for i in 0 .. k {
		mut inbuf := []u8{}
		inbuf << r
		inbuf << u8(nctr)
		nctr++
		b := shake256_xof(inbuf, 64 * eta1)
		e1[i] = sample_cbd2(b)
	}
	mut inbuf2 := []u8{}
	inbuf2 << r
	inbuf2 << u8(nctr)
	b2 := shake256_xof(inbuf2, 64 * eta2)
	e2 := sample_cbd2(b2)

	mut r_hat := PolyVec{}
	for i in 0 .. k {
		r_hat[i] = rr[i]
		ntt(mut r_hat[i])
	}
	mut u := PolyVec{}
	for i in 0 .. k {
		mut acc := Poly{}
		for j in 0 .. k {
			for l in 0 .. n {
				acc[l] = (acc[l] + a[j][i][l] * r_hat[j][l]) % q
			}
		}
		inv_ntt(mut acc)
		u[i] = poly_add(acc, e1[i])
	}
	mut acc := Poly{}
	for i in 0 .. k {
		for l in 0 .. n {
			acc[l] = (acc[l] + t_hat[i][l] * r_hat[i][l]) % q
		}
	}
	inv_ntt(mut acc)
	mut m_poly := Poly{}
	for i in 0 .. n {
		bit := (int(m[i / 8]) >> (i % 8)) & 1
		m_poly[i] = decompress(bit, 1)
	}
	mut v := poly_add(acc, e2)
	v = poly_add(v, m_poly)

	mut ct := []u8{}
	for i in 0 .. k {
		ct << poly_compress(u[i], du)
	}
	ct << poly_compress(v, dv)
	return ct
}

pub fn kpke_decrypt(dk []u8, ct []u8) ![]u8 {
	mut s_hat := PolyVec{}
	mut off := 0
	for i in 0 .. k {
		s_hat[i] = bytes_to_poly(dk[off..off + 3 * n / 2])
		off += 3 * n / 2
	}
	mut u := PolyVec{}
	mut uoff := 0
	for i in 0 .. k {
		ul := n * du / 8
		u[i] = poly_decompress(ct[uoff..uoff + ul], du)
		uoff += ul
	}
	vl := n * dv / 8
	v := poly_decompress(ct[uoff..uoff + vl], dv)

	mut u_hat := PolyVec{}
	for i in 0 .. k {
		u_hat[i] = u[i]
		ntt(mut u_hat[i])
	}
	mut acc := Poly{}
	for i in 0 .. k {
		for l in 0 .. n {
			acc[l] = (acc[l] + s_hat[i][l] * u_hat[i][l]) % q
		}
	}
	inv_ntt(mut acc)
	mut w := poly_sub(v, acc)
	mut m := []u8{len: 32}
	for i in 0 .. n {
		bit := compress(w[i], 1)
		if bit == 1 {
			m[i / 8] |= u8(1 << (i % 8))
		}
	}
	return m
}

// ---------- ML-KEM ----------

pub struct EncapsulationKey {
pub:
	data []u8
}

pub struct DecapsulationKey {
pub:
	data []u8
}

pub struct Ciphertext {
pub:
	data []u8
}

pub fn keygen() !(EncapsulationKey, DecapsulationKey) {
	d := rand.bytes(32)!
	z := rand.bytes(32)!
	ek, dk_pke := kpke_keygen(d)!
	h := sha3_256(ek)
	mut dk := []u8{}
	dk << dk_pke
	dk << ek
	dk << h
	dk << z
	return EncapsulationKey{ data: ek }, DecapsulationKey{ data: dk }
}

pub fn encaps(ek EncapsulationKey) !(Ciphertext, []u8) {
	m := rand.bytes(32)!
	h := sha3_256(ek.data)
	mut input := []u8{}
	input << m
	input << h
	g := sha3_512(input)
	k_bar := g[..32].clone()
	r := g[32..].clone()
	ct := kpke_encrypt(ek.data, m, r)!
	mut ss_input := []u8{}
	ss_input << k_bar
	ss_input << sha3_256(ct)
	ss := sha3_256(ss_input)
	return Ciphertext{ data: ct }, ss
}

pub fn decaps(dk DecapsulationKey, ct Ciphertext) ![]u8 {
	dk_pke := dk.data[..k * 3 * n / 2].clone()
	ek := dk.data[k * 3 * n / 2..k * 3 * n / 2 + ek_size].clone()
	h := dk.data[k * 3 * n / 2 + ek_size..k * 3 * n / 2 + ek_size + 32].clone()
	z := dk.data[k * 3 * n / 2 + ek_size + 32..].clone()
	m_prime := kpke_decrypt(dk_pke, ct.data)!
	mut input := []u8{}
	input << m_prime
	input << h
	g := sha3_512(input)
	k_bar_prime := g[..32].clone()
	r_prime := g[32..].clone()
	ct_prime := kpke_encrypt(ek, m_prime, r_prime)!
	mut fail := false
	if ct.data.len != ct_prime.len {
		fail = true
	} else {
		for i in 0 .. ct.data.len {
			if ct.data[i] != ct_prime[i] {
				fail = true
			}
		}
	}
	if fail {
		mut ss_input := []u8{}
		ss_input << z
		ss_input << sha3_256(ct.data)
		return sha3_256(ss_input)
	}
	mut ss_input := []u8{}
	ss_input << k_bar_prime
	ss_input << sha3_256(ct.data)
	return sha3_256(ss_input)
}
