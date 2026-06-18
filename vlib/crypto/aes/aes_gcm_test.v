module aes

import encoding.hex

// Test vectors from "The Galois/Counter Mode of Operation (GCM)",
// and NIST SP 800-38D worked examples.
fn test_aes128_gcm_vector() {
	key := hex.decode('feffe9928665731c6d6a8f9467308308')!
	iv := hex.decode('cafebabefacedbaddecaf888')!
	plaintext :=
		hex.decode('d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b39')!
	ad := hex.decode('feedfacedeadbeeffeedfacedeadbeefabaddad2')!
	want_ct :=
		hex.decode('42831ec2217774244b7221b784d0d49ce3aa212f2c02a4e035c17e2329aca12e21d514b25466931c7d8f6a5aac84aa051ba30b396a0aac973d58e091')!
	want_tag := hex.decode('5bc94fbc3221a5db94fae95ae7121a47')!

	mut g := new_aes_gcm(key)!
	out := g.encrypt(plaintext, iv, ad)!
	ct := out[..out.len - 16]
	tag := out[out.len - 16..]
	assert ct == want_ct
	assert tag == want_tag

	dec := g.decrypt(out, iv, ad)!
	assert dec == plaintext
}

fn test_aes256_gcm_vector() {
	key := hex.decode('feffe9928665731c6d6a8f9467308308feffe9928665731c6d6a8f9467308308')!
	iv := hex.decode('cafebabefacedbaddecaf888')!
	plaintext :=
		hex.decode('d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b39')!
	ad := hex.decode('feedfacedeadbeeffeedfacedeadbeefabaddad2')!
	want_ct :=
		hex.decode('522dc1f099567d07f47f37a32a84427d643a8cdcbfe5c0c97598a2bd2555d1aa8cb08e48590dbb3da7b08b1056828838c5f61e6393ba7a0abcc9f662')!
	want_tag := hex.decode('76fc6ece0f4e1768cddf8853bb2d551b')!

	mut g := new_aes_gcm(key)!
	out := g.encrypt(plaintext, iv, ad)!
	assert out[..out.len - 16] == want_ct
	assert out[out.len - 16..] == want_tag
	assert g.decrypt(out, iv, ad)! == plaintext
}

fn test_gcm_tamper_detected() {
	key := hex.decode('00000000000000000000000000000000')!
	iv := hex.decode('000000000000000000000000')!
	mut g := new_aes_gcm(key)!
	out := g.encrypt('hello'.bytes(), iv, []u8{})!
	mut bad := out.clone()
	bad[0] ^= 0xff
	if _ := g.decrypt(bad, iv, []u8{}) {
		assert false, 'tampered ciphertext should fail authentication'
	}
}
