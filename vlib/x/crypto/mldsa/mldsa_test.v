module mldsa

fn test_field_to_montgomery_roundtrip() {
	for val in [u32(0), 1, 2, 100, 1000, q - 1] {
		m := field_to_montgomery(val) or { panic(err) }
		back := field_from_montgomery(m)
		assert back == val, 'roundtrip failed for ${val}: got ${back}'
	}
}

fn test_field_add_sub() {
	a := field_to_montgomery(100) or { panic(err) }
	b := field_to_montgomery(200) or { panic(err) }
	sum := field_add(a, b)
	assert field_from_montgomery(sum) == 300

	diff := field_sub(sum, b)
	assert field_from_montgomery(diff) == 100
}

fn test_field_mul() {
	a := field_to_montgomery(1000) or { panic(err) }
	b := field_to_montgomery(2000) or { panic(err) }
	prod := field_montgomery_mul(a, b)
	assert field_from_montgomery(prod) == (1000 * 2000) % q
}

fn test_ntt_inverse_ntt_roundtrip() {
	mut f := RingElement{}
	for i in 0 .. n {
		f[i] = field_to_montgomery(u32(i % 100)) or { panic(err) }
	}
	ntt_f := ntt(f)
	back := inverse_ntt(ntt_f)
	for i in 0 .. n {
		assert field_from_montgomery(back[i]) == field_from_montgomery(f[i]), 'NTT roundtrip failed at index ${i}'
	}
}

fn test_ntt_mul_is_polynomial_product() {
	// (1 + x)^2 ?= x^2 + 2x + 1
	mut a := RingElement{}
	a[0] = field_to_montgomery(1) or { panic(err) }
	a[1] = field_to_montgomery(1) or { panic(err) }

	a_ntt := ntt(a)
	prod_ntt := ntt_mul(a_ntt, a_ntt)
	prod := inverse_ntt(prod_ntt)

	assert field_from_montgomery(prod[0]) == 1, 'expected x^2'
	assert field_from_montgomery(prod[1]) == 2, 'expected 2x'
	assert field_from_montgomery(prod[2]) == 1, 'expected 1'

	// rest should be zero
	for i in 3 .. n {
		assert field_from_montgomery(prod[i]) == 0, 'expected 0 at index ${i}, got ${field_from_montgomery(prod[i])}'
	}
}

fn test_power2_round() {
	// r = hi * 2^d + lo (mod q)
	for val in [u32(0), 1, 100, 1000, q / 2, q - 1] {
		r := field_to_montgomery(val) or { panic(err) }
		hi, lo := power2_round(r)
		reconstructed := field_add(field_to_montgomery(u32(hi) << d) or { panic(err) }, lo)
		assert field_from_montgomery(reconstructed) == val, 'power2_round failed for ${val}'
	}
}

struct SignVerifyCase {
	label     string
	new_key   fn ([]u8) !PrivateKey
	seed_off  u8
}

const sign_verify_cases = [
	SignVerifyCase{
		label: 'ML-DSA-44'
		new_key: new_private_key_44
		seed_off: 42
	},
	SignVerifyCase{
		label: 'ML-DSA-65'
		new_key: new_private_key_65
		seed_off: 0
	},
	SignVerifyCase{
		label: 'ML-DSA-87'
		new_key: new_private_key_87
		seed_off: 99
	},
]

fn test_keygen_sign_verify() {
	for c in sign_verify_cases {
		seed := []u8{len: 32, init: u8(index + c.seed_off)}
		priv_key := c.new_key(seed) or { panic('${c.label}: ${err}') }
		pub_key := priv_key.public_key()

		msg := 'hello ${c.label}'.bytes()
		sig := sign_deterministic(&priv_key, msg, '') or { panic('${c.label}: ${err}') }
		valid := verify(pub_key, msg, sig, '') or { panic('${c.label}: ${err}') }
		assert valid, '${c.label} signature verification failed'

		wrong_msg := 'wrong message'.bytes()
		valid2 := verify(pub_key, wrong_msg, sig, '') or { panic('${c.label}: ${err}') }
		assert !valid2, '${c.label} verification should fail with wrong message'
	}
}

struct PkRoundtripCase {
	label    string
	new_key  fn ([]u8) !PrivateKey
	new_pk   fn ([]u8) !PublicKey
	pk_size  int
	sig_size int
}

const pk_roundtrip_cases = [
	PkRoundtripCase{
		label:    'ML-DSA-44'
		new_key:  new_private_key_44
		new_pk:   new_public_key_44
		pk_size:  public_key_size_44
		sig_size: signature_size_44
	},
	PkRoundtripCase{
		label:    'ML-DSA-65'
		new_key:  new_private_key_65
		new_pk:   new_public_key_65
		pk_size:  public_key_size_65
		sig_size: signature_size_65
	},
	PkRoundtripCase{
		label:    'ML-DSA-87'
		new_key:  new_private_key_87
		new_pk:   new_public_key_87
		pk_size:  public_key_size_87
		sig_size: signature_size_87
	},
]

fn test_public_key_encode_decode() {
	for c in pk_roundtrip_cases {
		seed := []u8{len: 32, init: u8(index + 7)}
		sk := c.new_key(seed) or { panic('${c.label}: ${err}') }
		pk := sk.public_key()

		pk_bytes := pk.bytes()
		assert pk_bytes.len == c.pk_size, '${c.label}: pk size mismatch'

		pk2 := c.new_pk(pk_bytes) or { panic('${c.label}: ${err}') }
		assert pk.equal(&pk2), '${c.label}: pk roundtrip mismatch'
	}
}

fn test_signature_sizes() {
	for c in pk_roundtrip_cases {
		seed := []u8{len: 32, init: u8(index + 13)}
		sk := c.new_key(seed) or { panic('${c.label}: ${err}') }
		msg := 'size test'.bytes()
		sig := sign_deterministic(&sk, msg, '') or { panic('${c.label}: ${err}') }
		assert sig.len == c.sig_size, '${c.label}: expected sig size ${c.sig_size}, got ${sig.len}'
	}
}

fn test_context_string() {
	seed := []u8{len: 32, init: u8(index)}
	sk := new_private_key_65(seed) or { panic(err) }
	pk := sk.public_key()

	msg := 'context test'.bytes()
	sig := sign_deterministic(&sk, msg, 'my-context') or { panic(err) }

	valid := verify(pk, msg, sig, 'my-context') or { panic(err) }
	assert valid

	valid2 := verify(pk, msg, sig, 'wrong-context') or { panic(err) }
	assert !valid2
}

fn test_randomized_sign() {
	seed := []u8{len: 32, init: u8(index + 1)}
	sk := new_private_key_44(seed) or { panic(err) }
	pk := sk.public_key()
	msg := 'randomized'.bytes()

	sig := sign(&sk, msg, '') or { panic(err) }
	valid := verify(pk, msg, sig, '') or { panic(err) }
	assert valid

	// two randomized signatures should differ
	sig2 := sign(&sk, msg, '') or { panic(err) }
	assert sig != sig2
}

fn test_verify_with_mu() {
	seed := []u8{len: 32, init: u8(index + 5)}
	sk := new_private_key_65(seed) or { panic(err) }
	pk := sk.public_key()
	msg := 'mu test'.bytes()

	mu := compute_mu(pk.tr[..], msg, '')
	sig := sign_deterministic(&sk, msg, '') or { panic(err) }

	valid := verify_with_mu(pk, mu[..], sig) or { panic(err) }
	assert valid

	// wrong mu should fail
	mut bad_mu := []u8{len: 64}
	for i in 0 .. 64 {
		bad_mu[i] = mu[i] ^ 0xff
	}
	valid2 := verify_with_mu(pk, bad_mu, sig) or { panic(err) }
	assert !valid2
}

fn test_generate_key() {
	sk44 := generate_key_44() or { panic(err) }
	pk44 := sk44.public_key()
	sig := sign_deterministic(&sk44, 'gen'.bytes(), '') or { panic(err) }
	assert verify(pk44, 'gen'.bytes(), sig, '') or { panic(err) }

	sk65 := generate_key_65() or { panic(err) }
	assert sk65.public_key().bytes().len == public_key_size_65

	sk87 := generate_key_87() or { panic(err) }
	assert sk87.public_key().bytes().len == public_key_size_87
}

fn test_private_key_equal() {
	seed := []u8{len: 32, init: u8(index)}
	sk1 := new_private_key_44(seed) or { panic(err) }
	sk2 := new_private_key_44(seed) or { panic(err) }
	assert sk1.equal(&sk2)

	seed2 := []u8{len: 32, init: u8(index + 1)}
	sk3 := new_private_key_44(seed2) or { panic(err) }
	assert !sk1.equal(&sk3)
}

fn test_private_key_bytes_roundtrip() {
	seed := []u8{len: 32, init: u8(index + 3)}
	sk := new_private_key_65(seed) or { panic(err) }
	seed_out := sk.bytes()
	sk2 := new_private_key_65(seed_out) or { panic(err) }
	assert sk.equal(&sk2)
}

fn test_error_invalid_seed_length() {
	short := []u8{len: 16}
	if _ := new_private_key_44(short) {
		assert false, 'should reject short seed'
	}
	long := []u8{len: 64}
	if _ := new_private_key_65(long) {
		assert false, 'should reject long seed'
	}
}

fn test_error_invalid_public_key() {
	bad := []u8{len: 10}
	if _ := new_public_key_44(bad) {
		assert false, 'should reject invalid pk'
	}
}

fn test_error_context_too_long() {
	seed := []u8{len: 32, init: u8(index)}
	sk := new_private_key_44(seed) or { panic(err) }
	pk := sk.public_key()
	long_ctx := 'x'.repeat(256)

	if _ := sign(&sk, 'msg'.bytes(), long_ctx) {
		assert false, 'sign should reject long context'
	}
	if _ := sign_deterministic(&sk, 'msg'.bytes(), long_ctx) {
		assert false, 'sign_deterministic should reject long context'
	}
	if _ := verify(pk, 'msg'.bytes(), []u8{}, long_ctx) {
		assert false, 'verify should reject long context'
	}
}

fn test_error_verify_with_mu_wrong_length() {
	seed := []u8{len: 32, init: u8(index)}
	sk := new_private_key_44(seed) or { panic(err) }
	pk := sk.public_key()

	if _ := verify_with_mu(pk, []u8{len: 32}, []u8{}) {
		assert false, 'should reject mu with wrong length'
	}
}

fn test_verify_corrupted_signature() {
	seed := []u8{len: 32, init: u8(index + 2)}
	sk := new_private_key_44(seed) or { panic(err) }
	pk := sk.public_key()
	msg := 'corrupt'.bytes()
	sig := sign_deterministic(&sk, msg, '') or { panic(err) }

	// flip a byte in the sig
	mut bad_sig := sig.clone()
	bad_sig[sig.len / 2] ^= 0xff
	valid := verify(pk, msg, bad_sig, '') or { false }
	assert !valid
}
