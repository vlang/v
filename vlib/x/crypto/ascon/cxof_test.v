// Copyright ©2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
module ascon

import arrays
import encoding.hex

struct CxofTest {
	count int
	msg   string
	z     string
	md    string
}

// Known Answer Tests (KAT) with 512-bits output for Ascon-CXOF128
fn test_cxof128() ! {
	for item in cxof128_test_data {
		msg := hex.decode(item.msg)!
		z := hex.decode(item.z)!
		md := hex.decode(item.md)!

		out := cxof128_64(msg, z)!
		assert md == out

		// With Xof128Digest opaque
		mut cx := new_cxof128(default_xof_size, z)!
		md0 := cx.sum(msg)
		assert md0 == md

		// test .read()
		cx.reset()
		mut dst := []u8{len: 64}
		_ := cx.write(msg)!
		nr := cx.read(mut dst)!
		assert nr == 64
		assert dst == md

		// with chunked messages
		cx.reset()
		chunks := arrays.chunk[u8](msg, 20)
		mut tot := 0
		for chunk in chunks {
			n := cx.write(chunk)!
			tot += n
		}
		assert msg.len == tot

		chunked_md := cx.sum([]u8{})
		assert chunked_md == md
	}
}

const cxof128_test_data = [
	CxofTest{
		count: 1
		msg:   ''
		z:     ''
		md:    '4f50159ef70bb3dad8807e034eaebd44c4fa2cbbc8cf1f05511ab66cdcc529905ca12083fc186ad899b270b1473dc5f7ec88d1052082dcdfe69fb75d269e7b74'
	},
	CxofTest{
		count: 2
		msg:   ''
		z:     '10'
		md:    '0c93a483e7d574d49fe52cce03ee646117977d57a8aa57704ab4daf44b501430ff6ac11a5d1fd6f2154b5c65728268270c8bb578508487b8965718ada6272fd6'
	},
	CxofTest{
		count: 3
		msg:   ''
		z:     '1011'
		md:    'd1106c7622e79fe955bd9d79e03b918e770fe0e0cddde28beb924b02c5fc936b33acca299c89eca5d71886cbbfa4d54a21c55fde2b679f5e2488063a1719dc32'
	},
	CxofTest{
		count: 4
		msg:   ''
		z:     '101112'
		md:    '6a53a6dbf1bec15a79ce1214ff76a4d6bb16f60cfa56bf2c218aec5e160372117d2a2e647b128624e9b1d2259faf083f2bedd0fc751a2e2ff268d0ee026b6449'
	},
	CxofTest{
		count: 5
		msg:   ''
		z:     '10111213'
		md:    'cc333dd5b4ea61abe4376d61058b16df5eda7056299865ed7d25f43ac5b8541574608bd95ab0a3c3b74abd4abf9e50e63be6efe1b836b58595d8c47705c4dffb'
	},
	CxofTest{
		count: 6
		msg:   ''
		z:     '1011121314'
		md:    '8ee69d28b1bf3eafacf1e169fd10b6b7b72a7e2aaf0625e8e7c00153833b7224ed8c8c127b9808352c5647f9e862958d6de9eb93c4a236d59ecd84665e7164d9'
	},
	CxofTest{
		count: 7
		msg:   ''
		z:     '101112131415'
		md:    '3681695c40d83f60b401ecfa14bc03780ad474438f74b823eec9f0d5a375c13488803d3b4b8c8d4acd03186039f905fa15c7860dd0e9d566f31cd9e5822a937c'
	},
	CxofTest{
		count: 8
		msg:   ''
		z:     '10111213141516'
		md:    '717a9ba3b3c00f4078572b2d3fb3f0a86d45f70bc4e1cd89cb7a952bfa64162383735534ecbe0a7e62e7592cf447404db0361d98c2237245688ead15c05ae59b'
	},
	CxofTest{
		count: 9
		msg:   ''
		z:     '1011121314151617'
		md:    '61324766441dd6c11e1736bad1d2185820885ed76fe2ce537775a6e855eeafd2a6651b5e862a44982765f8b4c7cbe9c8b354f569ead6abc62cc9b7cdd72e0cb3'
	},
	CxofTest{
		count: 10
		msg:   ''
		z:     '101112131415161718'
		md:    '32fde6b9d290f56fc74aac9368f32c69973e1bab35d96118db7181aae577687673c01a9e35327aded556987eed3441d4f42ec36b0c198498d9e7f357b948d560'
	},
	CxofTest{
		count: 11
		msg:   ''
		z:     '10111213141516171819'
		md:    '690fc893055910d7d1d38055cf5589bbbe6b82bf175847ab3e0fd9a578b044dcb42be2932067eaa563a09e634581f34c2b4cfb38e1b06841b45b7b34c746d6dd'
	},
	CxofTest{
		count: 12
		msg:   ''
		z:     '101112131415161718191a'
		md:    'ecdab5b15324f99a1709be26fc329d305bd475e5f39bc2b63788792166ad08fe720ccd14e0a4de7d83ede1c7744929dc509c73748d6661a3d3215995357d3f88'
	},
	CxofTest{
		count: 13
		msg:   ''
		z:     '101112131415161718191a1b'
		md:    'ec2ba3309cfa6d6d0b581374e7c020ad17c330ea2b76d48724a415dceca3859c11146c2f64e52e44d27b1c44fd27476990a2e959b9998827a527a7e69089895f'
	},
	CxofTest{
		count: 14
		msg:   ''
		z:     '101112131415161718191a1b1c'
		md:    '659a59bc7fdece2f1bafa9f1bfb4c262043f74da550f85c902c9c4302adfcf898fbcd74c92d67bded153137e0d32ccba88767354be99103dfeb59c686ca98dea'
	},
	CxofTest{
		count: 15
		msg:   ''
		z:     '101112131415161718191a1b1c1d'
		md:    '17756044b742028b508d797c2c75a0722dde763c59d3fe5f70435b82faca5a80fe9c5ec9f3c59072ae48f37a241281c25d2e903c9d9290128265f1fe92b80bed'
	},
]
