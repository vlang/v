module mlkem

import time

fn det_bytes(base u8) []u8 {
	mut b := []u8{len: 32}
	for i in 0 .. 32 {
		b[i] = base + u8(i)
	}
	return b
}

// Deterministic K-PKE KAT for every variant: fixed d/m/r must round-trip,
// and re-encryption must reproduce the identical ciphertext.
fn test_kpke_deterministic_all_variants() {
	for v in [MlKemVariant.ml_kem_512, .ml_kem_768, .ml_kem_1024] {
		p := params_of(v)
		d := det_bytes(0x10)
		m := det_bytes(0xa0)
		r := det_bytes(0x50)
		ek, dk := kpke_keygen_params(p, d) or { panic(err) }
		ct := kpke_encrypt_params(p, ek, m, r) or { panic(err) }
		assert ct.len == p.ct_size
		m2 := kpke_decrypt_params(p, dk, ct) or { panic(err) }
		assert m == m2
		ct2 := kpke_encrypt_params(p, ek, m, r) or { panic(err) }
		assert ct == ct2
	}
}

// Tampered ciphertexts must still decapsulate to 32 bytes (implicit
// rejection) and must not reproduce the valid shared secret.
fn test_encaps_tamper_implicit_rejection() {
	for v in [MlKemVariant.ml_kem_512, .ml_kem_768, .ml_kem_1024] {
		pk, sk := generate_keypair(v) or { panic(err) }
		ct, ss1 := encapsulate(pk) or { panic(err) }
		assert ss1.len == 32
		mut bad := ct.data.clone()
		bad[0] ^= 0x01
		bad[bad.len - 1] ^= 0x80
		ss2 := decapsulate(Ciphertext{ data: bad }, sk) or { panic(err) }
		assert ss2.len == 32
		assert ss2 != ss1
		p := params_of(v)
		assert ct.data.len == p.ct_size
	}
}

// Stress: repeated keygen/encaps/decaps round-trips per variant with
// throughput reporting. Generous ceiling only guards against hangs.
fn test_mlkem_stress_all_variants() {
	for v in [MlKemVariant.ml_kem_512, .ml_kem_768, .ml_kem_1024] {
		start := time.now()
		iters := 5
		for _ in 0 .. iters {
			pk, sk := generate_keypair(v) or { panic(err) }
			ct, ss1 := encapsulate(pk) or { panic(err) }
			ss2 := decapsulate(ct, sk) or { panic(err) }
			assert ss1 == ss2
		}
		elapsed := time.since(start)
		println('mlkem ${v} ${iters}x keygen/encaps/decaps: ${elapsed.milliseconds()} ms')
		assert elapsed.milliseconds() < 300_000
	}
}
