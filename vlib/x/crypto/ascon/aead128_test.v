// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
module ascon

import encoding.hex
import rand

// This test materials was taken and adapted into v from references implementation of Ascon-aead128
// especially for the known answer test data, but, its not all fully-taken, just randomly choosen item.
// See at https://github.com/ascon/ascon-c/blob/main/crypto_aead/asconaead128/LWC_AEAD_KAT_128_128.txt
struct KatTest {
	cnt   int
	key   string
	nonce string
	pt    string
	ad    string
	ct    string
}

// testing for Ascon-AEAD128 encryption and decryption.
fn test_ascon_aead128_enc_dec() ! {
	for item in aead128_kat_tests_data {
		key := hex.decode(item.key)!
		nonce := hex.decode(item.nonce)!
		pt := hex.decode(item.pt)!
		ad := hex.decode(item.ad)!
		ct := hex.decode(item.ct)!

		out := encrypt(key, nonce, ad, pt)!
		assert out == ct

		msg := decrypt(key, nonce, ad, ct)!
		assert msg == pt

		// Work with object-based Cipher
		mut c := new_aead128(key)!
		// Lets encrypt the message
		exp_ct := c.encrypt(msg, nonce, ad)!
		assert exp_ct == ct
		// Lets decrypt it back
		exp_msg := c.decrypt(exp_ct, nonce, ad)!
		assert exp_msg == msg
	}
}

const aead128_kat_tests_data = [
	KatTest{
		cnt:   1
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    ''
		ct:    '4F9C278211BEC9316BF68F46EE8B2EC6'
	},
	KatTest{
		cnt:   2
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '30'
		ct:    'CCCB674FE18A09A285D6AB11B35675C0'
	},
	KatTest{
		cnt:   3
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '3031'
		ct:    'F65B191550C4DF9CFDD4460EBBCCA782'
	},
	KatTest{
		cnt:   4
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '303132'
		ct:    'D127CF7D2CD4DA8930616C70B3619F42'
	},
	KatTest{
		cnt:   5
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '30313233'
		ct:    '000BA92E52B5ED6B97C9D913CC4C82DF'
	},
	KatTest{
		cnt:   6
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '3031323334'
		ct:    'F7CC167F8FED3AEEA99B385B8622157E'
	},
	KatTest{
		cnt:   7
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '303132333435'
		ct:    '51CCBC46D56E93B89B1A3BFDAD0AA4D5'
	},
	KatTest{
		cnt:   8
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '30313233343536'
		ct:    'B38ABBD573E071C6265EEAC4A68F65AB'
	},
	KatTest{
		cnt:   9
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '3031323334353637'
		ct:    '865C594093A9EDEE2C1D6384CCB4939E'
	},
	KatTest{
		cnt:   10
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '303132333435363738'
		ct:    '24F13284A0F90F906B18C7E4061C0896'
	},
	KatTest{
		cnt:   27
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '303132333435363738393A3B3C3D3E3F40414243444546474849'
		ct:    '4ED362C4407B1D3BE17A51465659DECF'
	},
	KatTest{
		cnt:   28
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '303132333435363738393A3B3C3D3E3F404142434445464748494A'
		ct:    'A35C52EC6E7C78C051B23D03F691916F'
	},
	KatTest{
		cnt:   29
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '303132333435363738393A3B3C3D3E3F404142434445464748494A4B'
		ct:    'F1C946363A21CCFFE291A289202FC64C'
	},
	KatTest{
		cnt:   30
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '303132333435363738393A3B3C3D3E3F404142434445464748494A4B4C'
		ct:    'F1D453E933904578EEC3EA8E85550CE5'
	},
	KatTest{
		cnt:   31
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '303132333435363738393A3B3C3D3E3F404142434445464748494A4B4C4D'
		ct:    '82E22C860881C0485EC5F5E8CEA42CEA'
	},
	KatTest{
		cnt:   32
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '303132333435363738393A3B3C3D3E3F404142434445464748494A4B4C4D4E'
		ct:    'C6306F1F154C78833984173360AAE874'
	},
	KatTest{
		cnt:   33
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    ''
		ad:    '303132333435363738393A3B3C3D3E3F404142434445464748494A4B4C4D4E4F'
		ct:    'EFC3E78B02AD9A80A6F0548C5B0BB5BA'
	},
	KatTest{
		cnt:   34
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    '20'
		ad:    ''
		ct:    'E8DD576ABA1CD3E6FC704DE02AEDB79588'
	},
	KatTest{
		cnt:   35
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    '20'
		ad:    '30'
		ct:    '962B8016836C75A7D86866588CA245D886'
	},
	KatTest{
		cnt:   49
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    '20'
		ad:    '303132333435363738393A3B3C3D3E'
		ct:    '2089CB1DE2AE7D3E45BA7E9CC293548546'
	},
	KatTest{
		cnt:   599
		key:   '000102030405060708090a0b0c0d0e0f'
		nonce: '101112131415161718191a1b1c1d1e1f'
		pt:    '202122232425262728292a2b2c2d2e2f3031'
		ad:    '30313233'
		ct:    'cf5337fcb70ec45d179e0c3f51bb25ac967a2e7062ee9bd80da6c72e3a9b43aed9e0'
	},
	KatTest{
		cnt:   600
		key:   '000102030405060708090a0b0c0d0e0f'
		nonce: '101112131415161718191a1b1c1d1e1f'
		pt:    '202122232425262728292a2b2c2d2e2f3031'
		ad:    '3031323334'
		ct:    '3076658cba8bf3bb6dccaa2f1255ee2e7db6f6493c7698f65f6860a7433a0f561e6c'
	},
	KatTest{
		cnt:   601
		key:   '000102030405060708090a0b0c0d0e0f'
		nonce: '101112131415161718191a1b1c1d1e1f'
		pt:    '202122232425262728292a2b2c2d2e2f3031'
		ad:    '303132333435'
		ct:    '9310c6dd8e9cbc3e406c0ebfbea312435f2c6975faf3b6b2b17ef1ea2503c3d31ef5'
	},
	KatTest{
		cnt:   602
		key:   '000102030405060708090a0b0c0d0e0f'
		nonce: '101112131415161718191a1b1c1d1e1f'
		pt:    '202122232425262728292a2b2c2d2e2f3031'
		ad:    '30313233343536'
		ct:    '6e024bd403f386eb9d1c56f459cfdcde1b2fdf8fd8be2faf0576c81e8d21c0dd8f8a'
	},
	KatTest{
		cnt:   603
		key:   '000102030405060708090A0B0C0D0E0F'
		nonce: '101112131415161718191A1B1C1D1E1F'
		pt:    '202122232425262728292A2B2C2D2E2F3031'
		ad:    '3031323334353637'
		ct:    'fabe2cb1e7eba6329a30080f26e7dc72503dfc57f4de06a334b7ebadca03b44b73e9'
	},
]

fn test_ascon_aead128_loop() {
	for n in 0 .. 256 {
		key := []u8{len: 16, init: rand.u8()}
		nonce := []u8{len: 16, init: rand.u8()}
		ad := []u8{len: int(rand.u8()), init: rand.u8()}
		txt := []u8{len: n, init: rand.u8()}
		e := encrypt(key, nonce, ad, txt)!
		d := decrypt(key, nonce, ad, e)!
		assert txt == d
	}
}
