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

// The test materials taken and adapted from CAVP Testing: Block Cipher Modes.
// Its only take the samples, not all tests was performed, some of them was not supported.
// See https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Algorithm-Validation-Program/documents/mac/gcmtestvectors.zip
fn test_avcp_1() ! {
	// [Keylen = 128]
	// [IVlen = 96]
	// [PTlen = 0]
	// [AADlen = 0]
	// [Taglen = 128]

	// Count = 0
	key := hex.decode('11754cd72aec309bf52f7687212e8957')!
	iv := hex.decode('3c819d9a9bed087615030b65')!
	pt := hex.decode('')!
	aad := hex.decode('')!
	ct := hex.decode('')!
	tag := hex.decode('250327c674aaf477aef2675748cf6971')!

	mut g := new_aes_gcm(key)!
	out := g.encrypt(pt, iv, aad)!

	// ciphertext only contains the tag
	assert out == tag
}

fn test_avcp_2() ! {
	// [Keylen = 128]
	// [IVlen = 96]
	// [PTlen = 128]
	// [AADlen = 128]
	// [Taglen = 120] // 15 bytes
	//
	// Count = 0
	key := hex.decode('89c54b0d3bc3c397d5039058c220685f')!
	iv := hex.decode('bc7f45c00868758d62d4bb4d')!
	pt := hex.decode('582670b0baf5540a3775b6615605bd05')!
	aad := hex.decode('48d16cda0337105a50e2ed76fd18e114')!
	ct := hex.decode('fc2d4c4eee2209ddbba6663c02765e69')!
	tag := hex.decode('55e783b00156f5da0446e2970b877f')!

	mut g := new_aes_gcm(key)!
	out := g.encrypt(pt, iv, aad)!
	assert out[0..pt.len] == ct
	// the tag len was truncated to 15-bytes
	assert out[pt.len..pt.len + 15].hex() == tag.hex()

	mut g2 := new_aes_gcm(key)!
	ptx := g2.decrypt(out, iv, aad)!
	assert ptx == pt
}

struct ItemTest {
	count int
	key   string
	iv    string
	pt    string
	aad   string
	ct    string
	tag   string
}

fn test_avcp_3() ! {
	// [Keylen = 128]
	// [IVlen = 96]
	// [PTlen = 128]
	// [AADlen = 160]
	// [Taglen = 128]
	tag_len := 16

	for t in tests_data {
		key := hex.decode(t.key)!
		iv := hex.decode(t.iv)!
		pt := hex.decode(t.pt)!
		aad := hex.decode(t.aad)!
		ct := hex.decode(t.ct)!
		tag := hex.decode(t.tag)!

		mut g := new_aes_gcm(key)!
		out := g.encrypt(pt, iv, aad)!
		assert out[0..pt.len] == ct
		assert out[pt.len..pt.len + tag_len] == tag
	}
}

const tests_data = [
	ItemTest{
		count: 0
		key:   'd4a22488f8dd1d5c6c19a7d6ca17964c'
		iv:    'f3d5837f22ac1a0425e0d1d5'
		pt:    '7b43016a16896497fb457be6d2a54122'
		aad:   'f1c5d424b83f96c6ad8cb28ca0d20e475e023b5a'
		ct:    'c2bd67eef5e95cac27e3b06e3031d0a8'
		tag:   'f23eacf9d1cdf8737726c58648826e9c'
	},
	ItemTest{
		count: 1
		key:   'e8899345e4d89b76f7695ddf2a24bb3c'
		iv:    '9dfaeb5d73372ceb06ca7bbe'
		pt:    'c2807e403e9babf645268c92bc9d1de6'
		aad:   'fed0b45a9a7b07c6da5474907f5890e317e74a42'
		ct:    '8e44bf07454255aa9e36eb34cdfd0036'
		tag:   '2f501e5249aa595a53e1985e90346a22'
	},
	ItemTest{
		count: 2
		key:   'c1629d6320b9da80a23c81be53f0ef57'
		iv:    'b8615f6ffa30668947556cd8'
		pt:    '65771ab52532c9cdfcb3a9eb7b8193df'
		aad:   '5f2955e4301852a70684f978f89e7a61531f0861'
		ct:    'c2a72d693181c819f69b42b52088d3a2'
		tag:   'cadaee305d8bb6d70259a6503280d99a'
	},
	ItemTest{
		count: 3
		key:   '196ed78281bb7543d60e68cca2aaa941'
		iv:    '6e7d2c8f135715532a075c50'
		pt:    '15b42e7ea21a8ad5dcd7a9bba0253d44'
		aad:   'd6fc98c632d2e2641041ff7384d92a8358ae9abe'
		ct:    '06e5cc81c2d022cb2b5de5a881c62d09'
		tag:   '28e8cad3346ce583d5eebaa796e50974'
	},
	ItemTest{
		count: 4
		key:   '55fe8a1bdc6806ed2f4a84891db943a0'
		iv:    'af4d0ba0a90f1e713d71ae94'
		pt:    '81315972f0b1aeaa005363e9eca09d7a'
		aad:   '677cd4e6c0a67913085dba4cc2a778b894e174ad'
		ct:    'c47bcb27c5a8d9beb19fee38b90861b7'
		tag:   'e061ee4868edf2d969e875b8685ca8a9'
	},
	ItemTest{
		count: 5
		key:   '6d86a855508657f804091be2290a17e0'
		iv:    '65dce18a4461afd83f1480f5'
		pt:    '0423bd1c8aea943637c7c3b0ca61d54b'
		aad:   'e0ef8f0e1f442a2c090568d2af336ec59f57c896'
		ct:    '53505d449369c9bcd8a138740ea6602e'
		tag:   '86f928b4532825af9cac3820234afe73'
	},
	ItemTest{
		count: 6
		key:   '66bd7b5dfd0aaaed8bb8890eee9b9c9a'
		iv:    '6e92bf7e8fd0fb932451fdf2'
		pt:    '8005865c8794b79612447f5ef33397d0'
		aad:   '60459c681bda631ece1aacca4a7b1b369c56d2bb'
		ct:    '83b99253de05625aa8e68490bb368bb9'
		tag:   '65d444b02a23e854a85423217562d07f'
	},
]
