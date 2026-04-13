// vtest build: has_modern_openssl?
// regenerate go test vecs: v run testdata/gen.vsh [go-source-path]

module mldsa

import crypto.sha256
import encoding.hex
import json
import os

struct TestVec {
	kind       string
	seed       string
	msg        string
	pk_sha256  string
	sig_sha256 string
	context    string
}

const vecs_json = os.read_file(os.real_path(os.join_path(os.dir(@FILE), 'testdata', 'vectors.json'))) or {
	panic(err)
}
const test_vecs = json.decode([]TestVec, vecs_json) or { panic(err) }

fn parse_kind(s string) Kind {
	return match s {
		'ml_dsa_44' { Kind.ml_dsa_44 }
		'ml_dsa_65' { Kind.ml_dsa_65 }
		'ml_dsa_87' { Kind.ml_dsa_87 }
		else { panic('unknown kind: ${s}') }
	}
}

fn test_keygen_sign_verify() {
	assert test_vecs.len > 0, 'no test vectors loaded'

	for tv in test_vecs {
		kind := parse_kind(tv.kind)
		seed := hex.decode(tv.seed) or { panic(err) }
		msg := hex.decode(tv.msg) or { panic(err) }
		expected_pk_hash := hex.decode(tv.pk_sha256) or { panic(err) }
		expected_sig_hash := hex.decode(tv.sig_sha256) or { panic(err) }

		sk := PrivateKey.from_seed(seed, kind) or { panic(err) }
		pk := sk.public_key()

		pk_hash := sha256.sum(pk.bytes())
		assert pk_hash[..] == expected_pk_hash, 'pk hash mismatch for ${tv.kind} seed=${tv.seed[..16]}...'

		sig := sk.sign(msg, deterministic: true, context: tv.context) or { panic(err) }
		sig_hash := sha256.sum(sig)
		assert sig_hash[..] == expected_sig_hash, 'sig hash mismatch for ${tv.kind} seed=${tv.seed[..16]}...'

		verified := pk.verify(msg, sig, context: tv.context) or { panic(err) }
		assert verified, 'verify returned false for ${tv.kind} seed=${tv.seed[..16]}...'
	}
}

fn test_verify_rejects_bad_signature() {
	for kind in [Kind.ml_dsa_44, .ml_dsa_65, .ml_dsa_87] {
		seed := []u8{len: 32, init: 0x00}
		sk := PrivateKey.from_seed(seed, kind) or { panic(err) }
		pk := sk.public_key()
		msg := 'deadbeef'.bytes()

		sig := sk.sign(msg, deterministic: true) or { panic(err) }

		mut bad_sig := sig.clone()
		bad_sig[10] ^= 0xff

		result := pk.verify(msg, bad_sig) or { false }
		assert result == false, 'verify should reject tampered sig for ${kind}'
	}
}

fn test_verify_rejects_wrong_message() {
	for kind in [Kind.ml_dsa_44, .ml_dsa_65, .ml_dsa_87] {
		seed := []u8{len: 32, init: 0x01}
		sk := PrivateKey.from_seed(seed, kind) or { panic(err) }
		pk := sk.public_key()
		msg := 'the beef is alive'.bytes()

		sig := sk.sign(msg, deterministic: true) or { panic(err) }

		result := pk.verify('I love strawberries'.bytes(), sig) or { false }
		assert result == false, 'verify should reject wrong message for ${kind}'
	}
}

fn test_verify_rejects_wrong_context() {
	for kind in [Kind.ml_dsa_44, .ml_dsa_65, .ml_dsa_87] {
		seed := []u8{len: 32, init: 0x02}
		sk := PrivateKey.from_seed(seed, kind) or { panic(err) }
		pk := sk.public_key()
		msg := 'very cool message'.bytes()

		sig := sk.sign(msg, deterministic: true, context: 'some context a') or { panic(err) }

		result := pk.verify(msg, sig, context: 'another context b') or { false }
		assert result == false, 'verify should reject wrong context for ${kind}'
	}
}

fn test_public_key_roundtrip() {
	for kind in [Kind.ml_dsa_44, .ml_dsa_65, .ml_dsa_87] {
		seed := []u8{len: 32, init: 0x03}
		sk := PrivateKey.from_seed(seed, kind) or { panic(err) }
		pk := sk.public_key()
		msg := 'pk roundtrip'.bytes()

		sig := sk.sign(msg, deterministic: true) or { panic(err) }

		pk2 := PublicKey.from_bytes(pk.bytes(), kind) or { panic(err) }
		assert pk.equal(&pk2), 'roundtripped public key not equal'

		verified := pk2.verify(msg, sig) or { panic(err) }
		assert verified, 'verify failed after public key roundtrip for ${kind}'
	}
}

fn test_prehash_sign_verify() {
	prehashes := [
		PreHash.sha2_256,
		.sha2_384,
		.sha2_512,
		.sha3_256,
		.sha3_512,
		.shake_128,
		.shake_256,
	]
	for kind in [Kind.ml_dsa_44, .ml_dsa_65, .ml_dsa_87] {
		seed := []u8{len: 32, init: 0x05}
		sk := PrivateKey.from_seed(seed, kind) or { panic(err) }
		pk := sk.public_key()
		msg := 'prehash test message'.bytes()

		for ph in prehashes {
			sig := sk.sign(msg, deterministic: true, prehash: ph) or { panic(err) }
			verified := pk.verify(msg, sig, prehash: ph) or { panic(err) }
			assert verified, 'prehash verify failed for ${kind} ${ph}'

			// pure verify must reject a prehashed sig
			pure_result := pk.verify(msg, sig) or { false }
			assert pure_result == false, 'pure verify should reject prehash signature for ${kind} ${ph}'
		}
	}
}

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

	for i in 3 .. n {
		assert field_from_montgomery(prod[i]) == 0, 'expected 0 at index ${i}, got ${field_from_montgomery(prod[i])}'
	}
}

fn test_power2_round() {
	for val in [u32(0), 1, 100, 1000, q / 2, q - 1] {
		r := field_to_montgomery(val) or { panic(err) }
		hi, lo := power2_round(r)
		reconstructed := field_add(field_to_montgomery(u32(hi) << d) or { panic(err) }, lo)
		assert field_from_montgomery(reconstructed) == val, 'power2_round failed for ${val}'
	}
}

fn test_private_key_roundtrip() {
	for kind in [Kind.ml_dsa_44, .ml_dsa_65, .ml_dsa_87] {
		seed := []u8{len: 32, init: 0x04}
		sk := PrivateKey.from_seed(seed, kind) or { panic(err) }
		sk2 := PrivateKey.from_seed(sk.seed(), kind) or { panic(err) }
		assert sk.equal(&sk2), 'roundtripped private key not equal for ${kind}'
	}
}
