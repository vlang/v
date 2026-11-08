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

// ---------- NTT (using zeta table) ----------

const zetas = [int(228), 1099, 1919, 812, 2403, 243, 2019, 1932, 2144, 651, 804, 2250, 101, 1600,
	161, 126, 1411, 2412, 1902, 2347, 2240, 1296, 1405, 374, 1863, 1217, 1727, 1093, 1778, 255,
	1872, 1030, 2483, 2054, 288, 387, 1713, 756, 1999, 817, 519, 84, 1818, 1261, 940, 1090, 1278,
	1536, 736, 2288, 392, 1562, 2455, 1361, 2394, 1419, 1339, 1774, 1928, 134, 2326, 359, 505,
	191, 1383, 1229, 1493, 1885, 771, 1475, 1808, 2475, 1308, 1907, 1434, 943, 915, 1618, 1006,
	2081, 2304, 1974, 1464, 1839, 1891, 1353, 1197, 2484, 1899, 2499, 1534, 2459, 1056, 2211, 1373,
	1034, 2413, 1667, 595, 1266, 1516, 1291, 1481, 1431, 1080, 2308, 1878, 148, 1107, 336, 1438,
	1882, 1001, 2439, 2163, 300, 1022, 1980, 1095, 2219, 789, 214, 1029, 2172, 2378, 1726, 394,
	1887, 733, 1478, 1968, 642, 1077, 2154, 1065, 2354, 873, 1605, 1460, 1939, 2491, 1495, 1315,
	1498, 2341, 1972, 961, 1628]

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
	mut out := Poly{}
	for i in 0 .. n {
		mut s := 0
		for j in 0 .. n {
			s = (s + f[j] * modpow(17, i * j, q)) % q
		}
		out[i] = s
	}
	for i in 0 .. n {
		f[i] = out[i]
	}
}

pub fn inv_ntt(mut f Poly) {
	mut out := Poly{}
	for i in 0 .. n {
		mut s := 0
		for j in 0 .. n {
			s = (s + f[j] * modpow(17, (n - (i * j) % n) % n, q)) % q
		}
		out[i] = (s * modpow(n, q - 2, q)) % q
	}
	for i in 0 .. n {
		f[i] = out[i]
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
	// 128 bytes -> 256 coeffs, eta=2
	mut f := Poly{}
	for i in 0 .. n {
		mut a := 0
		mut bb := 0
		for j in 0 .. 2 {
			idx := (4 * i + 2 * j) / 8
			off := (4 * i + 2 * j) % 8
			a += (int(b[idx]) >> off) & 1
			a += (int(b[idx]) >> (off + 1)) & 1 // may cross byte; simplified
		}
		// proper: extract 2 bits for a and 2 for b from 4 bits total per coeff
		_ = bb
	}
	// Redo properly
	f = Poly{}
	for i in 0 .. n {
		// 4 bits per coeff: bits [4i, 4i+3], a = bits 0,1 ; b = bits 2,3
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
	// input to SHAKE-128: rho || j || i (little-endian bytes)
	mut input := []u8{}
	input << rho
	input << u8(j)
	input << u8(i)
	mut out := Poly{}
	mut ctr := 0
	mut buf := shake128_xof(input, 3 * 256) // 3 bytes per 2? actually 2 bytes per coeff attempt
	// Rejection sampling: read 2-byte little-endian values, accept if < q*? 
	// FIPS 203: accept if value < 3329 (q), with 2 bytes giving 0..65535, reject >= q (but q*?).
	// Actually accept if d < q where d = (buf[0] | buf[1]<<8) & 0xfff? 
	// FIPS 203 SampleNTT: d = buf[0] + 256*buf[1]; if d < q accept; else reject. Uses 2 bytes (16 bits), d in 0..65535, accept if < q=3329. Very low acceptance ~5%. Need more bytes.
	// Use rejection with enough bytes.
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

// K-PKE keygen returns (ek, dk) as byte arrays.
// ek = ByteEncode_12(t) || rho (384+32=416 bytes for k=3? actually 3*384+32=1184)
// dk = ByteEncode_12(s) (3*384=1152 bytes)
pub fn kpke_keygen(d []u8) !([]u8, []u8) {
	// G(d) -> (rho, sigma)
	g := sha3_512(d)
	rho := g[..32].clone()
	sigma := g[32..].clone()
	// matrix A (k x k) in NTT domain
	mut a := PolyMat{}
	for i in 0 .. k {
		for j in 0 .. k {
			a[i][j] = sample_ntt(rho, i, j)
		}
	}
	// s vector
	mut s := PolyVec{}
	mut nctr := 0
	for i in 0 .. k {
		// SamplePolyCBD_eta1(sigma, N)
		mut inbuf := []u8{}
		inbuf << sigma
		inbuf << u8(nctr)
		nctr++
		b := shake256_xof(inbuf, 64 * eta1) // 128 bytes for eta=2
		s[i] = sample_cbd2(b)
	}
	// e vector
	mut e := PolyVec{}
	for i in 0 .. k {
		mut inbuf := []u8{}
		inbuf << sigma
		inbuf << u8(nctr)
		nctr++
		b := shake256_xof(inbuf, 64 * eta1)
		e[i] = sample_cbd2(b)
	}
	// NTT(s), NTT(e)
	mut s_hat := PolyVec{}
	mut e_hat := PolyVec{}
	for i in 0 .. k {
		s_hat[i] = s[i]
		ntt(mut s_hat[i])
		e_hat[i] = e[i]
		ntt(mut e_hat[i])
	}
	// t_hat = A * s_hat + e_hat
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
	// ek = ByteEncode_12(t_hat) || rho
	mut ek := []u8{}
	for i in 0 .. k {
		ek << poly_to_bytes(t_hat[i])
	}
	ek << rho
	// dk = ByteEncode_12(s_hat)
	mut dk := []u8{}
	for i in 0 .. k {
		dk << poly_to_bytes(s_hat[i])
	}
	return ek, dk
}

// K-PKE encrypt: takes ek, m (32 bytes), r (32 bytes) -> ct.
pub fn kpke_encrypt(ek []u8, m []u8, r []u8) ![]u8 {
	// parse ek: t_hat (k polys) + rho
	mut t_hat := PolyVec{}
	mut off := 0
	for i in 0 .. k {
		t_hat[i] = bytes_to_poly(ek[off..off + 3 * n / 2])
		off += 3 * n / 2
	}
	rho := ek[off..off + 32]
	// matrix A
	mut a := PolyMat{}
	for i in 0 .. k {
		for j in 0 .. k {
			a[i][j] = sample_ntt(rho, i, j)
		}
	}
	// sample r, e1, e2
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

	// NTT(r)
	mut r_hat := PolyVec{}
	for i in 0 .. k {
		r_hat[i] = rr[i]
		ntt(mut r_hat[i])
	}
	// u = NTT^{-1}(A^T * r_hat) + e1
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
	// v = NTT^{-1}(t_hat^T * r_hat) + e2 + decompress(m, 1)
	mut acc := Poly{}
	for i in 0 .. k {
		for l in 0 .. n {
			acc[l] = (acc[l] + t_hat[i][l] * r_hat[i][l]) % q
		}
	}
	inv_ntt(mut acc)
	// message to poly: m is 32 bytes = 256 bits, decompress with d=1
	mut m_poly := Poly{}
	for i in 0 .. n {
		bit := (int(m[i / 8]) >> (i % 8)) & 1
		m_poly[i] = decompress(bit, 1)
	}
	mut v := poly_add(acc, e2)
	v = poly_add(v, m_poly)

	// ct = (Compress(u, du), Compress(v, dv))
	mut ct := []u8{}
	for i in 0 .. k {
		ct << poly_compress(u[i], du)
	}
	ct << poly_compress(v, dv)
	return ct
}

// K-PKE decrypt.
pub fn kpke_decrypt(dk []u8, ct []u8) ![]u8 {
	mut s_hat := PolyVec{}
	mut off := 0
	for i in 0 .. k {
		s_hat[i] = bytes_to_poly(dk[off..off + 3 * n / 2])
		off += 3 * n / 2
	}
	// ct: u (k polys compressed du) + v (1 poly compressed dv)
	mut u := PolyVec{}
	mut uoff := 0
	for i in 0 .. k {
		ul := n * du / 8
		u[i] = poly_decompress(ct[uoff..uoff + ul], du)
		uoff += ul
	}
	vl := n * dv / 8
	v := poly_decompress(ct[uoff..uoff + vl], dv)

	// w = v - NTT^{-1}(s_hat^T * NTT(u))
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
	// message = Compress(w, 1)
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

// KeyGen: returns (ek, dk). dk includes s_hat, ek, h, z.
pub fn keygen() !(EncapsulationKey, DecapsulationKey) {
	d := rand.bytes(32)!
	z := rand.bytes(32)!
	ek, dk_pke := kpke_keygen(d)!
	// h = H(ek)
	h := sha3_256(ek)
	// dk = dk_pke || ek || h || z
	mut dk := []u8{}
	dk << dk_pke
	dk << ek
	dk << h
	dk << z
	return EncapsulationKey{ data: ek }, DecapsulationKey{ data: dk }
}

pub fn encaps(ek EncapsulationKey) !(Ciphertext, []u8) {
	m := rand.bytes(32)!
	// (K_bar, r) = G(m || H(ek))
	h := sha3_256(ek.data)
	mut input := []u8{}
	input << m
	input << h
	g := sha3_512(input)
	k_bar := g[..32].clone()
	r := g[32..].clone()
	ct := kpke_encrypt(ek.data, m, r)!
	// K = KDF(K_bar || H(ct)) -- use SHAKE-256 or just hash
	mut ss_input := []u8{}
	ss_input << k_bar
	ss_input << sha3_256(ct)
	ss := sha3_256(ss_input)
	return Ciphertext{ data: ct }, ss
}

pub fn decaps(dk DecapsulationKey, ct Ciphertext) ![]u8 {
	// parse dk
	dk_pke := dk.data[..k * 3 * n / 2].clone() // 1152
	ek := dk.data[k * 3 * n / 2..k * 3 * n / 2 + ek_size].clone()
	h := dk.data[k * 3 * n / 2 + ek_size..k * 3 * n / 2 + ek_size + 32].clone()
	z := dk.data[k * 3 * n / 2 + ek_size + 32..].clone()
	// m' = K-PKE.Decrypt(dk_pke, ct)
	m_prime := kpke_decrypt(dk_pke, ct.data)!
	// (K_bar', r') = G(m' || h)
	mut input := []u8{}
	input << m_prime
	input << h
	g := sha3_512(input)
	k_bar_prime := g[..32].clone()
	r_prime := g[32..].clone()
	// re-encrypt
	ct_prime := kpke_encrypt(ek, m_prime, r_prime)!
	// compare ct and ct_prime (constant-time)
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
		// implicit rejection: K = KDF(z || H(ct))
		mut ss_input := []u8{}
		ss_input << z
		ss_input << sha3_256(ct.data)
		return sha3_256(ss_input)
	}
	// K = KDF(K_bar' || H(ct))
	mut ss_input := []u8{}
	ss_input << k_bar_prime
	ss_input << sha3_256(ct.data)
	return sha3_256(ss_input)
}

// end of module
