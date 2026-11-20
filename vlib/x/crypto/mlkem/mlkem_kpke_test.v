module mlkem

import crypto.rand
import crypto

fn test_kpke_roundtrip() {
	ek, dk := kpke_keygen(crypto.rand.bytes(32) or { panic(err) }) or { panic(err) }
	m := []u8{len: 32, init: u8(index + 1)}
	r := crypto.rand.bytes(32) or { panic(err) }
	ct := kpke_encrypt(ek, m, r) or { panic(err) }
	m2 := kpke_decrypt(dk, ct) or { panic(err) }
	assert m == m2
}
