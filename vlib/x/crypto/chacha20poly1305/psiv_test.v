// Copyright (c) 2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// ChaCha20-Poly1305-PSIV test.
// The test was adapted from Rust reference implementation of ChaCha20-Poly1305-PSIV
module chacha20poly1305

import rand
import encoding.hex

struct PsivKatTest {
	key   string
	nonce string
	ad    string
	tag   string
	pt    string
	ct    string
}

// This Konwn Answer Test (KAT) test material was adapted from this link:
// https://codeberg.org/Gusted/chacha20-poly1305-psiv/raw/branch/main/testdata/KAT/chacha20-poly1305-psiv.json
//
const psiv_kattest_data = [
	PsivKatTest{
		key:   'da5e8b4dc96a45cbf6868996cf9374968639a0e462993a5a4ce0f50c30387b4d'
		nonce: 'baf068c8aa4fecc4485f0673'
		ad:    'fb69bf2be138f95831e2f80cd6'
		tag:   'b414135310ce4430cddcdc883d0b8533'
		pt:    '53bc'
		ct:    'be6d'
	},
	PsivKatTest{
		key:   '8c5f631e8f9d29efa780d031232022a1d388aeb3f4d50592dd0d05d7cc5d4bc1'
		nonce: 'e5ddfb49845b9dc73dfa4b10'
		ad:    '126ce447818b55571d725495e6614a6f'
		tag:   '6d5c42a83ad867653625c10f6ef05385'
		pt:    '31d5c270085a9f80ef90ebf732d928e8'
		ct:    'ae44247314351bfca354474c71998195'
	},
	PsivKatTest{
		key:   '81432e9fa573a3e0aaa352f668a15b754c81502ed14c8ee6fc9ec0fb7100344d'
		nonce: '78904fc961c52e65e13d302e'
		ad:    '663f149d40338426e81e5257991630202dd06ced12a2bca83f89dc7296541782'
		tag:   '4e43af952bb555961f2910633ece24fc'
		pt:    'd0037b9ad2e00b2f698c26a791c0a67ee5a298929ce5ce2ba4999a5d89befa92'
		ct:    '6266ba8132a9193af5e9d8511c27166050e01571e55a3d27d3ab98e5b6a3c11b'
	},
	PsivKatTest{
		key:   'faad2687fe7fcaad896a50dd005089a8fee8d95e79e3705d8136b5c45ef03d1a'
		nonce: '2748826716da2a887ff63885'
		ad:    'e85d908a97f6673421e1c4ad674d792e914972147191be9e2b1158f489eb47a41a594112cc74854c521d84029a959cfd'
		tag:   '5bf71bbda8e147f51524395009d1bbd2'
		pt:    '93dd95401f779b9fcd64261c94e861ed0947c378654dfe8cda6776a0a5e5d35e83cbb595cbd1b449b2278f17e81ac83f'
		ct:    'e898487b800760a38f2727161264ac3e439606a94a0525f8eb0f4ade565cccbd25e3df2415b5cf71fbf9e871816de8d7'
	},
	PsivKatTest{
		key:   '6df081bcb85c439a1def71c8aaf69d3c885b992ba9c8271fd6c969f48e0b11c6'
		nonce: 'ebb85860f597fc51715eb5d6'
		ad:    '3e00e080c3bffc3ec4b2e45dc09faaf9caae0a92fb74eb4b245c0a6aa0109d4932208478022ed98a6815cd57cd34e0cc2ad0b85a37cd03d955012e2f69e6a030'
		tag:   '8beaf75797d5f4c316b946cfb0de31ee'
		pt:    '8525879de99305c0fdc4edeb66dc229faaaebf111804253afba2a2878313dda53009531fee0ff5613e39ea694efe7093ae7b349851700e5f6075cd5cbd784df0'
		ct:    'f194e1e2f8288afd6a8013d8a14eb32851fedb97ce9151c25ca7678ec7eb7eb62652e1176666799f379307e92c79263c948bc3b6cd9aeeded02d98fcedb1c94f'
	},
	PsivKatTest{
		key:   'fce8cb2c75f84f98f44ae5a48dbbebe46536ebae80a054aeeaefbdceb3a1a956'
		nonce: '86c43a87f6082ca50a2cf552'
		ad:    'c1d6753875f2f1a7223ef8769b2136f171124ce1bf502ded1429439407f91fabcb16a354d958dba41a475de6137b2cbd4e4c2dcf3334e351d09f504db6f78ed783c92b622637304e4e50faab674040f8'
		tag:   '056b0315e62c7925d20495f5ee1824cc'
		pt:    'caa171079f29612875d0bac2772e563add27281d91376bc72c1e9ad7fbff3af3910fe2c1d6432df3cda86ccf931fb439f22c492d3e1cc1dd5aecbd312045aacafffabf3d4ef8cadafa685a371f299c38'
		ct:    '19c86459a6b3aa8ce0a99849e995a633461642553c0b8b5935d67d796b3bfc46888575636dfa7c4f8343a302dcc0296089115d0713c8a84376dbdd3afdb15b41b494c35feab382a876dec957abf6990a'
	},
	PsivKatTest{
		key:   '8b119e13f8ad308e0d2a15284b45dad10de7c700f30148e3bf12c137eaa4125b'
		nonce: 'ce81fc611e7f1160413e6d52'
		ad:    '65536d29922a97570818bda61970f2130260bd404447552dc1b8be4f4fb3a7d403479926c82e18846bed7c326b43b605cdd9a589f5467bbac21735ae8d6000477936e294dde2ab0e58da59aa847be8b85207e437c53c61c879e2365e62357203'
		tag:   'c463dea4dc1b6ccd711aef9823da39e9'
		pt:    '4e38a5eda409058b6ccfc9759f73042b3a91fa7f85757346f8199567b92c4702524942753e3f1b06aa2bd0a65b60fc555bd6e737e121ef27084938c5149e42d9421feba9201d9a553128b84c7de17fe34754829307f11a184057a05fc408a2fb'
		ct:    'd70af5d8999d64ac57cf477e5b49b22ef39c0b9cde96b10312bfda93bbe4f2d0901dc3081c0439df34fe991383c1773f27e7c13a04d7c52ff2696fc60d076428ab03d3e7ef19618861fa99577da350ec7745779660f093c0bec764b7afd2a7df'
	},
	PsivKatTest{
		key:   '9d1efe472184bcedc72fc0ef1e1462d8f1e5a62a7430e08df26d04026e6135d8'
		nonce: 'b5141ce04e81f40113337293'
		ad:    'dbb0d72f901383b584eb7ab4cd070ad0dae9de2904fb03ae39d09bf96b9d1cd75cbb196154323321b2ed8ecb7dc14ece42008c889cab063250d15050f3281e4df55afcef1f411837a8ccc7546f4c571846e38c5afd846eadae7171b01f93f57c841ad02819bc2bfc7de607ef19a73ec3'
		tag:   '4d12a32e158b02657bd4a7c9fa676529'
		pt:    'd1e946809fb78c7e4e145870f70982cf950dcb18856fb96477f5fdfc3e542227456cb3c501494facd3c2f08a31bdb5198993db98346241fb54559a650067d97e45227fd89408d33c322b70eb081f5b5b3ddd084147bb0cf8826790deb4bb0b5cd34e92475f141353e6dbdc839ff44697'
		ct:    'fd1422017e046f8940669371ed905eda2cc8f0b33586794bdbd99625ae7694c9c6bb0f6d3cf3353d0ed732241800063b45c16ca0ddf616d1f82ac1fbb2a42a96e6fde25e7439c064d45766bbcd277b8908e6561cb52bc494635a6d1b7c4d86b05bac9f647f9197efee25ebaa1ab9a22c'
	},
	PsivKatTest{
		key:   'af8ce66d984e6507f1a810bf372324cd91e1d52a2d232148f4ad240ac1620059'
		nonce: '7d2a7c08cdc00e531bd8ef56'
		ad:    '88d029543383ecbb02783ddd4419d0e5af3b389a666dff57882b5c49a1e0fb50bc78f7c8b2f0d5cabd832d3a10d8ee2d06a414ce4f5e645bb8b6c032e3f9d17cbe31b1313d5345f109c8251759c6a415dcc0340779b6bb40aea06ca323c6ab5690b713e1dea52fc3fe99926bacaca9d7888b6dd39569d534494a63cc602dcc9b'
		tag:   '12c1ee0e495c42ef85fc602fa4393934'
		pt:    '505c5d76005285cd7f4dd9196023d466b4bdae3e03a9ad5ef6b63def0a74442fe27132496935e010a6a15f785eaaa7367bc0b2ca9381873dbb80eb22701113ca5a996faf3d97c85bb16d62c581ee90ff73ebaa6f7a34f7a1acf784a854d73aecbb5aede2c197a73b1ad057a8d1c7e798e7a686a94dcfb8b1047bc9442d42c535'
		ct:    'ce98619c023027e08eeb67c44b3801547191c01db1bb840164490da254f2196669347b9db669feb505a7126fe29c2c3eb1a5aeb7a46dbc8f3966316bfb39b8e189d5e5be09e42ac668a6dd218b72652d3851c2895982bb52f055a172b729aaa351dc213db04738395565b3e31b4a4bb84823e33066dda7aca1c473dff2eaf964'
	},
	PsivKatTest{
		key:   'd58e34f08258aadad1cdf264b893c34b6d8c9ea4048640310b96b937e3e344e3'
		nonce: '2b0a958f3e2dfc64431b3410'
		ad:    '88b216f01541dd888e4fd04443e638b4498758643583a7b04d13277d48e6096f3f4d33e3e387a1d90ac6d1a53c0574c1290099386834f803f48ff8c21b68d9650dcebd2232c9c2775bdf0fe28765bd1f5bf1392f9ba262264ef19f371fa1351055a74c8566098e9e861759ffe58e14cacc909fcaaeac4fbe7466613ddc09ded32cec9e4a014d64ab7eccafd58914ed1b'
		tag:   '91329d284bad6b733d3d48467d4cce45'
		pt:    '4be34f526d428683def93efc38d2c4f710ee7637aba54ff086d278128d3b68420c31982a772b15c5438158ed090d65c6f8f5cba4e8a320eea7d500d1c490fc6c59f8991ef58164f46c8b2685c08b2d85bff6b480d30d5ba14e3d1e96e28e880ed63a6c1738b6575334076a42522fb9b22c9164faa9fcc69881976e3773345dacf232a6b7e1a852ee6ddf8dd51f84e6b0'
		ct:    '06e5d414215a1e2aefdfae18c54fe721e21f163dcf6454babce5b3a5be2f219a110e9b893addc5177b6ad48ddd6a6f4981c6309f67e3101387646a8970ab13593c3ac62cd57c608fa7f5f4be8c214171eaa223e7d291c9ee2979339d74438ad7084526224e8df5909b37d6a0fb2a97be1b2f1d26ac8a8024edabe05ac8828f1eb8bb0a9236669bf75613264272e81f2a'
	},
]

// Test for Known Answert Test (KAT) of ChaCha20-Poly1305-PSIV
// based on https://codeberg.org/Gusted/chacha20-poly1305-psiv/raw/branch/main/testdata/KAT/chacha20-poly1305-psiv.json
// Thanks to @tankf33der to point this test vectors.
fn test_aead_psiv_for_kat_tests() ! {
	for i, item in psiv_kattest_data {
		key := hex.decode(item.key)!
		nonce := hex.decode(item.nonce)!
		ad := hex.decode(item.ad)!
		tag := hex.decode(item.tag)!
		pt := hex.decode(item.pt)!
		ct := hex.decode(item.ct)!

		// full aead ciphertext = ct + tag
		mut cxt := []u8{}
		cxt << ct
		cxt << tag

		// psiv encryption
		enc_msg := psiv_encrypt(pt, key, nonce, ad)!
		assert cxt == enc_msg

		// decrypts it back
		dec_msg := psiv_decrypt(enc_msg, key, nonce, ad)!
		assert dec_msg == pt
	}
}

// test for internal encryption routine
fn test_psiv_insternal_encryption_of_encrypted_text_is_plaintext() ! {
	for i := 0; i < 1024; i++ {
		input := rand.bytes(i)!
		key := rand.bytes(36)!
		mut dkey := [36]u8{}
		unsafe { vmemcpy(dkey, key.data, key.len) }
		tag := rand.bytes(16)!
		nonce := rand.bytes(12)!

		mut out := []u8{len: input.len}
		psiv_encrypt_internal(mut out, input, dkey, tag, nonce)!

		// encrypting this output with the same params was result in original input
		// make a clone of ciphertext output as an input into internal encrypt routine
		text := out.clone()
		psiv_encrypt_internal(mut out, text, dkey, tag, nonce)!
		assert out == input
	}
}

// test for AEAD of ChaCha20Poly1305-PSIV encrypt and decrypt
fn test_psiv_aead_encryption_of_encrypted_text_is_plaintext() ! {
	for i := 0; i < 1024; i++ {
		input := rand.bytes(i)!
		key := rand.bytes(32)!
		nonce := rand.bytes(12)!
		ad := rand.bytes(i)!

		// encrypt message input
		out := psiv_encrypt(input, key, nonce, ad)!
		// decrypting this output with the same params was resulting an original input
		awal := psiv_decrypt(out, key, nonce, ad)!
		assert awal == input

		// test with object-based construct
		mut c := new_psiv(key)!
		out1 := c.encrypt(input, nonce, ad)!
		back1 := c.decrypt(out1, nonce, ad)!
		assert back1 == input
		unsafe { c.free() }
	}
}

fn test_for_wrong_tag() ! {
	for i := 0; i < 1024; i++ {
		input := rand.bytes(i)!
		key := rand.bytes(32)!
		nonce := rand.bytes(12)!
		ad := rand.bytes(i)!

		mut out := psiv_encrypt(input, key, nonce, ad)!
		out[out.len - tag_size] ^= 1

		// decrypting would fail
		_ := psiv_decrypt(out, key, nonce, ad) or {
			assert err == error('unmatching tag')
			continue
		}
	}
}

fn test_chacha20_core() ! {
	// null input
	s := [16]u32{}
	x0 := chacha20_core(s)
	assert x0 == s

	// u32 input
	u32input := [u32(0x61707865), 0x3320646e, 0x79622d32, 0x6b206574, 0x03020100, 0x07060504,
		0x0b0a0908, 0x0f0e0d0c, 0x13121110, 0x17161514, 0x1b1a1918, 0x1f1e1d1c, 0x00000001,
		0x09000000, 0x4a000000, 0x00000000]!
	// expected output
	exp_x1 := [u32(0xe4e7f110), 0x15593bd1, 0x1fdd0f50, 0xc47120a3, 0xc7f4d1c7, 0x0368c033,
		0x9aaa2204, 0x4e6cd4c3, 0x466482d2, 0x09aa9f07, 0x05d7c214, 0xa2028bd9, 0xd19c12b5,
		0xb94e16de, 0xe883d0cb, 0x4e3c50a2]!
	x1 := chacha20_core(u32input)
	assert x1 == exp_x1
}
