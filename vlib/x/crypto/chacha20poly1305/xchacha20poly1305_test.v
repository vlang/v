// Copyright (c) 2025 blackshirt.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
import encoding.hex
import x.crypto.chacha20poly1305

// Test from Developer-Friendly Test Vectors
// See https://datatracker.ietf.org/doc/html/draft-arciszewski-xchacha#appendix-A.3
fn test_xchacha20poly1305_aead_rfc() ! {
	plaintext := hex.decode('4c616469657320616e642047656e746c656d656e206f662074686520636c617373206f66202739393a204966204920636f756c64206f6666657220796f75206f6e6c79206f6e652074697020666f7220746865206675747572652c2073756e73637265656e20776f756c642062652069742e')!
	aad := hex.decode('50515253c0c1c2c3c4c5c6c7')!
	key := hex.decode('808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f')!
	nonce := hex.decode('404142434445464748494a4b4c4d4e4f5051525354555657')!
	ciphertext := hex.decode('bd6d179d3e83d43b9576579493c0e939572a1700252bfaccbed2902c21396cbb731c7f1b0b4aa6440bf3a82f4eda7e39ae64c6708c54c216cb96b72e1213b4522f8c9ba40db5d945b11b69b982c1bb9e3f3fac2bc369488f76b2383565d3fff921f9664c97637da9768812f615c68b13b52e')!
	tag := hex.decode('c0875924c1c7987947deafd8780acf49')!

	// encrypt produces ciphertext plus tag appends into it
	mut expected_output := []u8{}
	expected_output << ciphertext
	expected_output << tag

	out := chacha20poly1305.encrypt(plaintext, key, nonce, aad)!
	assert out == expected_output

	// decryption
	decrypted_text := chacha20poly1305.decrypt(expected_output, key, nonce, aad)!
	assert decrypted_text == plaintext

	// With object-based approach
	cipher := chacha20poly1305.new(key, nonce.len)!
	encrypted_msg := cipher.encrypt(plaintext, nonce, aad)!
	assert encrypted_msg == expected_output

	calc_plaintext := cipher.decrypt(encrypted_msg, nonce, aad)!
	assert calc_plaintext == plaintext
}

fn test_xchacha20poly1305_invalid_tag() ! {
	item := XChaCha20Test{
		tcid:    121
		comment: 'Flipped bit 1 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0718b3e73e52c3be2eaba76807b784e1'
		result:  'invalid'
		flags:   ''
	}
	key := hex.decode(item.key)!
	nonce := hex.decode(item.iv)!
	aad := hex.decode(item.aad)!
	msg := hex.decode(item.msg)!
	ct := hex.decode(item.ct)!
	tag := hex.decode(item.tag)!

	expected_tag := hex.decode('0518b3e73e52c3be2eaba76807b784e1')!

	// encrypt produces ciphertext plus tag appends into it
	mut encrypted := []u8{}
	encrypted << ct
	encrypted << tag // this is an invalid tag

	mut c := chacha20poly1305.new(key, nonce.len)!
	enc := c.encrypt(msg, nonce, aad)!
	assert enc != encrypted
	assert enc[0..enc.len - c.overhead()] == ct
	assert enc[enc.len - c.overhead()..] != tag
	assert enc[enc.len - c.overhead()..] == expected_tag
}

struct XChaCha20Test {
	tcid    int
	comment string
	key     string
	iv      string
	aad     string
	msg     string
	ct      string
	tag     string
	result  string
	flags   string
}

// This test materials was adapted from pycryptodome test vector for xchacha20poly1305
// See https://github.com/Legrandin/pycryptodome/blob/master/test_vectors/pycryptodome_test_vectors/Cipher/wycheproof/xchacha20_poly1305_test.json
fn test_xchacha20poly1305_aead_from_pycryptodome_with_valid_results() ! {
	for item in xchacha20poly1305_aead_testdata_with_valid_results {
		key := hex.decode(item.key)!
		nonce := hex.decode(item.iv)!
		aad := hex.decode(item.aad)!
		msg := hex.decode(item.msg)!
		ct := hex.decode(item.ct)!
		tag := hex.decode(item.tag)!

		// encrypt produces ciphertext plus tag appends into it
		mut encrypted := []u8{}
		encrypted << ct
		encrypted << tag

		// test raw encryption
		out := chacha20poly1305.encrypt(msg, key, nonce, aad)!
		assert out == encrypted

		// test raw decryption
		decrypted_text := chacha20poly1305.decrypt(encrypted, key, nonce, aad)!
		assert decrypted_text == msg

		// test with object-based construct
		mut c0 := chacha20poly1305.new(key, nonce.len)!
		out0 := c0.encrypt(msg, nonce, aad)!
		assert out0 == encrypted

		dec0 := c0.decrypt(encrypted, nonce, aad)!
		assert dec0 == msg

		// Note: this is an experimental test with 64-bit counter construct
		mut c1 := chacha20poly1305.new(key, nonce.len, use_64bit_counter: true)!
		out1 := c1.encrypt(msg, nonce, aad)!
		assert out1 == encrypted

		dec1 := c1.decrypt(encrypted, nonce, aad)!
		assert dec1 == msg
	}
}

// where the result == valid
const xchacha20poly1305_aead_testdata_with_valid_results = [
	XChaCha20Test{
		tcid:    1
		comment: 'draft-arciszewski-xchacha-02'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '404142434445464748494a4b4c4d4e4f5051525354555657'
		aad:     '50515253c0c1c2c3c4c5c6c7'
		msg:     '4c616469657320616e642047656e746c656d656e206f662074686520636c617373206f66202739393a204966204920636f756c64206f6666657220796f75206f6e6c79206f6e652074697020666f7220746865206675747572652c2073756e73637265656e20776f756c642062652069742e'
		ct:      'bd6d179d3e83d43b9576579493c0e939572a1700252bfaccbed2902c21396cbb731c7f1b0b4aa6440bf3a82f4eda7e39ae64c6708c54c216cb96b72e1213b4522f8c9ba40db5d945b11b69b982c1bb9e3f3fac2bc369488f76b2383565d3fff921f9664c97637da9768812f615c68b13b52e'
		tag:     'c0875924c1c7987947deafd8780acf49'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    2
		comment: ''
		key:     'ab1562faea9f47af3ae1c3d6d030e3af230255dff3df583ced6fbbcbf9d606a9'
		iv:      '6a5e0c4617e07091b605a4de2c02dde117de2ebd53b23497'
		aad:     ''
		msg:     ''
		ct:      ''
		tag:     'e2697ea6877aba39d9555a00e14db041'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    3
		comment: ''
		key:     'd821dce9b890ea37ae1c89e7cb6aeae9371b8179add0d08f5494718322ae0071'
		iv:      '3ec3f7c45e687d75a895bf5e71809e7cdac32158bb48ec0d'
		aad:     '8780fb400f94c55d'
		msg:     ''
		ct:      ''
		tag:     '966c22d655b9e56326024f028cf887ad'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    4
		comment: ''
		key:     '303ccb2e1567c3d9f629a5c632dbc62a9a82c525674f67988b31bd1dee990538'
		iv:      '05188738844ab90a8b11beef38eaec3e100d8f4f85ae7a41'
		aad:     ''
		msg:     '62'
		ct:      '45'
		tag:     'd15734f984d749fa3f0550a70c43dddf'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    5
		comment: ''
		key:     '697c197c9e0023c8eee42ddf08c12c46718a436561b0c66d998c81879f7cb74c'
		iv:      'cd78f4533c94648feacd5aef0291b00b454ee3dcdb76dcc8'
		aad:     '6384f4714ff18c18'
		msg:     'e1'
		ct:      'b0'
		tag:     'e5e35f5332f91bdd2d28e59d68a0b141'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    6
		comment: ''
		key:     'c11213bcff39a88b0e3ecc47b23acf6c3014e4708d80dcca162da7377b316ab3'
		iv:      'b60ca1ab736deebe4d9da78bc7cbbab91be14a2f884240b7'
		aad:     ''
		msg:     '57f9'
		ct:      '5e03'
		tag:     'eed21c2cd3f395538d677602964ed578'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    7
		comment: ''
		key:     'b0f51b8227013464943370e926b6ed1c9fb45b5994af829ff3a9f998b77d822c'
		iv:      '4fd76cbf27cb387502a706461564e5a5c14e027d40bc6eef'
		aad:     '322f82a87ee82997'
		msg:     'ab8c'
		ct:      'b56a'
		tag:     'edcafa2c9032aff695e427fc2a344767'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    8
		comment: ''
		key:     '17afb080753f2aa0af0a7f4821f6ab2709a6b2b5b9f2f262910e3b27b82c6c1c'
		iv:      '737e3e7699f788c4136938c0f65310684eacbb5f96ecd98d'
		aad:     ''
		msg:     '2af96a'
		ct:      '31a461'
		tag:     '2b745098b154bb90903b0240c3bc95e9'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    9
		comment: ''
		key:     'b720aea3df85fb3fb00583eddbebc5c545bcdcb7f6f2a94c1087950e16d68278'
		iv:      '1436f36466fce5db337a73ec18e269e6e985d91035128183'
		aad:     '9d53316bd2aa3e3d'
		msg:     '4799c4'
		ct:      'd41c02'
		tag:     '8faa889d7f189cd9473e19200ef03920'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    10
		comment: ''
		key:     'd7704e505826124ab02935e7349a4e13391e6dc020fee95cd30654cdc5d5f393'
		iv:      '7c39999d498286d974d266b2f027a26d7fbcd330869d9f93'
		aad:     ''
		msg:     'c44efab6'
		ct:      'a3b405bb'
		tag:     'c50e2ddb97df1ee58561c97a7b746c24'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    11
		comment: ''
		key:     'c70ef9ee59259019960c918bfc91237ed6786c73f2b62427e4cbd4d8096a1f03'
		iv:      'eb4e36c637d1908db2c2ae9c72cfbae50655cb5f6504c4b6'
		aad:     '8e0ac97934605052'
		msg:     '2738c9d0'
		ct:      '9406a621'
		tag:     '916b78ee04b20b8cd90f00b81bb8091c'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    12
		comment: ''
		key:     '7fac2a879ffddf5e36e04e3edcb8aa6be18a8326b28f76b15623307badc1ece2'
		iv:      '49875536d4946af49288f36684e25ff35998d50be6bcfcc2'
		aad:     ''
		msg:     '2c4c38f435'
		ct:      '2a01d08fe2'
		tag:     '9cbe5f3e782f57a33a45b1f4aeeeea6e'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    13
		comment: ''
		key:     '48f1389d9222a80898ca26b5cfef5dc82dfc0af7cf66ea1e01bc5279e7414247'
		iv:      '88ccb58d435ea760f19e1fa6172139a071c0c5143959a56c'
		aad:     '5cbdd482f3429a27'
		msg:     '945a1fd040'
		ct:      'fb5daf8c6e'
		tag:     'ec1682b61957493c2eb758d7a2b7a179'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    14
		comment: ''
		key:     '737cdaa2ce1e4740e75af4aaf68c0296c1607bde871d2452e628f1456239c753'
		iv:      '89c9806ad153b805f1bf5b50738319011d5fc070bb551ee1'
		aad:     ''
		msg:     'fae858dd3150'
		ct:      '856c300cfceb'
		tag:     'aaa9875ebd42a11d12cf0aca26021f4a'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    15
		comment: ''
		key:     '9f7cd632bd5eb5f017b898590d645571ef56e521024eda36eff893a6ad04b935'
		iv:      '5cbdc34772b54fb4fba9eca1e2745e0e3704d9d7b5c78fb4'
		aad:     '71b29930f84a572d'
		msg:     '53abb8943ada'
		ct:      '6438fc8f8788'
		tag:     'af05a4def2ad39a195a7b8c222050111'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    16
		comment: ''
		key:     'ecf60cd2af8c7155c0be848ecdaa5baddad6bd5f254a2d98f47bef83999f60ee'
		iv:      'a020b016d952a5948a3d226bb1b73efc39d46845f3bf0ca5'
		aad:     ''
		msg:     'ea30907da57d78'
		ct:      '843f1039531fe1'
		tag:     'efd99acdab540690ec91a7ad5697cb33'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    17
		comment: ''
		key:     'a9376583c47176728d7b2ed1039f0b12b2c7a97563937f7fe976ce4548f7cb00'
		iv:      'b1f05bb66d29bcddf7412f6a556ff7540aac452457dd69e6'
		aad:     '0c87cc97c49e166a'
		msg:     '4a3d9926dc9757'
		ct:      'f99f3fb49ec920'
		tag:     '91c3356ee6601ae7073673d2ef30293b'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    18
		comment: ''
		key:     'bf9ae8ceceb8d3001da7652c4cec02adda8696294a4ab542b41b5ba86c096a75'
		iv:      'f4f3484cacdce37cf5134a12f57903096acd3553607eb682'
		aad:     ''
		msg:     '6eb5e11b358c0ab1'
		ct:      '5b596bab0890286e'
		tag:     'd4474d9520f7178e9811f624209721ea'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    19
		comment: ''
		key:     'd447796ed4ceb2e43942700e7759e335f67afa8653748db95f924c94488195db'
		iv:      'cc4781134455e89c836f7433bd0426776f945d82f6358276'
		aad:     '06947c3afa797e99'
		msg:     '77c46ada19c81849'
		ct:      '80c8e9ac2cec97ce'
		tag:     '9b62dcc8076098affcb6e7995aaa99a6'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    20
		comment: ''
		key:     '08eb57d7bc113f7fbdda1b32237cdd06cccd52ef4a89a831c5e0564370c885ad'
		iv:      '200a30270bc911dd3b8a8ea2a6e6ce75be9cfb0f5431db3d'
		aad:     ''
		msg:     '704df23a31893799ee'
		ct:      '37d696264f781338c9'
		tag:     '5fddaf74438159acc3c5667b5e84af13'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    21
		comment: ''
		key:     '9f093b6bb75f1609ab1e00a4bf4667961d885f01deb6520c5bb16ec21e033766'
		iv:      'a613e0b17fafb47c79614d39959b986ba2c97b0215676d41'
		aad:     '00fc4f61d9777504'
		msg:     '472578ece9fe828dc6'
		ct:      'a55cbb308f81e449e9'
		tag:     '8174bd595da1be72cc226e74c46a4af5'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    22
		comment: ''
		key:     'e421bb3269130c731d1947e7b5d233c11d195ceed1d08634743db9c252bfefa5'
		iv:      '21b40036745f64b2aab3e89665cf4dab2b690d88721fe9a3'
		aad:     ''
		msg:     '1155c7f0ee3e1faa641e'
		ct:      '8bd51b64fcd244f0b3aa'
		tag:     'dbdd1558934b83ae4393ade73e9edadb'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    23
		comment: ''
		key:     '8a275c90eb8688c5d9e82b74331cf104a2c8757d6257079b1d8035bb40d6a8d9'
		iv:      '33dfa71a0cb2aca008e4c8e8a72dbda4c407bbadd5d7e1a5'
		aad:     'e7c9d1dda90b699e'
		msg:     '3c2da491f244acfbd1dc'
		ct:      'e5aad5c055dc6df73cf0'
		tag:     '96fc30292cc8381c345d5f2964ba5626'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    24
		comment: ''
		key:     '2d97a35e4b6617e5f4a0f50dcda7622f321cad936a246d9beada9d75e142ef3d'
		iv:      '5a44801d2baabfe8cbee6da52bb51b5297856065fbf33944'
		aad:     ''
		msg:     'b94df0d444dac848ffcad4'
		ct:      '2a41cc14a6a65bbb153758'
		tag:     '1044cd75f2e61cbecbf3a7a77c13ef01'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    25
		comment: ''
		key:     '70d11ca92903865c6a6d8ba497f5a2d65f23b72198d7fc7fdaeda6c2632f7e46'
		iv:      '07590877a1e1df3a78fe4d04dd64b6cb79f1df45de17685b'
		aad:     'd78dcb5431ef5669'
		msg:     'f61bb0dd66e5905f1a7ea1'
		ct:      '5b3193405830b6840a4474'
		tag:     '4b10bef8e8a3c2e6ae87fb8fb2a8bdd5'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    26
		comment: ''
		key:     '05c7317f07a0e89ce1b5ac41df8064faa9fd569ee1c357cd01a2872076477ac5'
		iv:      '94f86b0fd8a6ed90d3780eca23a82f4387da82b0894ae317'
		aad:     ''
		msg:     'b63e50c9bcd01406b6f78f86'
		ct:      '528dfb79ea182945f13bafb7'
		tag:     '4fc22f4491449bb4ffe6a1eb266e2a91'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    27
		comment: ''
		key:     '924aafdb5b8a206b3e49aefe8944918cdcc8ccb5bb4b8c4ee81b847aa6fa52a0'
		iv:      '829cb09e40c2cc5f7648adc177e56ef53a58bfa16a859338'
		aad:     'a67a57310055b193'
		msg:     '68576b935acaab8b33ab62e3'
		ct:      '2345bfc502f9c62d64ad87f9'
		tag:     '6736f095a28b887238f80dc562eaa25c'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    28
		comment: ''
		key:     '332b7ec9bf4a983eb02af7efee8ffaf5627b66f29e3e4728f50894fe176788d8'
		iv:      '016dac89c624a9d425ae377132421c37c4486895bef270f0'
		aad:     ''
		msg:     '8289397a58921bb3201b29c505'
		ct:      'd1f725ace69f7899ef51c11dd3'
		tag:     '0d2858cc30497107a035929fdf2eb6af'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    29
		comment: ''
		key:     'b75fd9dd7ecca4f3eab36c36a176530dd3ffc825c202613740311d11cd501804'
		iv:      'e8252b018f9e0c3fbd4a6ad0d06346302b8ed7dcb206c3ad'
		aad:     '4dc711c827a6f626'
		msg:     '9800f8b835c4ff490ebd764914'
		ct:      '6c0e9d31b8e45591726f4cfc63'
		tag:     '2ce700f1f3dc7d3f60607058ac3b817e'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    30
		comment: ''
		key:     '2bddfb332f74ac31fcf91d652c7b41fbcb26a10f2792ecf8075478e645042f87'
		iv:      'e698d39b3cec2634dbe035a55b8fce3b0041aabe4156f713'
		aad:     ''
		msg:     '813974b924c7618c63070d0247f0'
		ct:      '23a49dbe4b699d481621d9fc2db6'
		tag:     'ef2cfb8423ae6f9faaec81025e6e274b'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    31
		comment: ''
		key:     'ea029c829c13a580b66aca21133a16933235c11c42905a640104a2ae9bb5cf82'
		iv:      'd025b0188edc9c40a8d6fc807cead97749016c9016d62ea5'
		aad:     '0b9df4ffd1c9ccbe'
		msg:     'a67e672df18cfbe125b212d63ec8'
		ct:      '0596f5709407a62fffce84240346'
		tag:     '893772def69053b0aaf3bf1c21144ebf'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    32
		comment: ''
		key:     '1c838d9f68e687fbdddc6dff7f2e44b277bfeb316ae5d11b3e935889b48539d8'
		iv:      '9ce202557c11a57cb14e7e4bd7986f1cf6232196672d25ea'
		aad:     ''
		msg:     '37905d98be9839e02923d119a88d56'
		ct:      'c5aa0caf82b963f1e9b84a789a77d3'
		tag:     '59c3e2e43cc098ed413ece9d9a6fd47a'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    33
		comment: ''
		key:     '51a99f0646767fbc01d7736df0340191acfbb5ae0288ed6fff2d34f0ea31470f'
		iv:      'ffdca5c51a0852ab18dd484af6664b63ab4097d303450837'
		aad:     'a2e44e165e7ca5f7'
		msg:     '93553954f0be4e24185601ce5c3c34'
		ct:      'f91d01453f568774115f75b5dad642'
		tag:     '8fc36af6ae5ee3e05b38ed43598bbfcd'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    34
		comment: ''
		key:     '6a1f808358461e75072a054e2fc4e4c3e7f882c57920dda3278d0c860ca704e3'
		iv:      '25dc279923c1bcdaa7a36e7b884b51f62343abad71986037'
		aad:     ''
		msg:     'f242209c67698ea32c2152f8785b7d82'
		ct:      '732715c60018fb0ed55c14c1fa9a5273'
		tag:     'afe3c4f050bf001e1dfcb2313dd8edd2'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    35
		comment: ''
		key:     '7fb18b56f3f5122585754a3b6c6a4e523036e66793db569c3e8e28032e916eb6'
		iv:      'c02c8c595064ac303b1be5df6ab43048856e97ae9962fb8f'
		aad:     '8981c7260d514ab6'
		msg:     '6e8c0bb3361908f5b33e059408651ae3'
		ct:      'a7eb11bfaa0d1c2ce457598049399575'
		tag:     '485a94f61aa5f47a3036e85a57effd2f'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    36
		comment: ''
		key:     '3b11469dc670f5dfbe0aad7d15ee4862c92cb07842e5dcc48fa8e5fc817f1749'
		iv:      '9a61cf35aecbd40a65b35a64b516896f3de7f977b5c9901d'
		aad:     ''
		msg:     '540731e4ba3e4e2fd623a1a13233736ee7'
		ct:      '0fd7386b41396e0558495c45cdba029062'
		tag:     '29f601a11f6a1072342c60b631de6085'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    37
		comment: ''
		key:     'e6d9fc8a9e3fa6ecadd9faffbb6ff387aa96502e60adadab029a9146ee39de28'
		iv:      '6570889af7acab7f555337bdce05499e8eb0d8d3d1a77660'
		aad:     '23230be73ba2a6fb'
		msg:     'deec95974eeef6e2b99739bed2f4a74771'
		ct:      '86d0fd1a325d501fe9efe83d3a3f62e346'
		tag:     '1ed9a79616c787a8de2ff5cdac6af0c9'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    38
		comment: ''
		key:     'cbaa654cd4ad70ae96d3412680e60522807e9b887ec6dbfcd6e71e917e29ce62'
		iv:      'f3d84207ab5574e4bc74ae61b17ccaccc7c46eb3471e0e53'
		aad:     ''
		msg:     'f55aaf5a55432c20fb782c552e5ae096eb23'
		ct:      'daea40da316b8e78254a737c57063c4ad8b7'
		tag:     'e13ff7a7e2c85b1abb5350134dfa7f9b'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    39
		comment: ''
		key:     '5b51ea4943ce173baa53f84a6ef59cb1e25b794768508b8dd8dcbfbc1744c18a'
		iv:      'de1e034363b0daec9828159e7996faff33a5f63eb552eb5f'
		aad:     'b6bea5c60f288109'
		msg:     '953939dd7601f17071b2bf776e4b1ed629ce'
		ct:      'eef62d53545698255648a483708c9cc93937'
		tag:     '182529b1d07dbcb4bd89b3c5e4c8fac9'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    40
		comment: ''
		key:     'c5d3917ffb42b0508296cb245d468b04bbaa2c8c8c32e845415a911ea85f95f1'
		iv:      '74533cbe3ff9ec5a66604c88f5dae4d7efe4f604111f79fc'
		aad:     ''
		msg:     '0afab6dbab51f929332d743ccfbb9f34877bc9'
		ct:      '03dfbb3407a55ab0dbc451d0289de44acb5f33'
		tag:     'a050def2e06a9ed3d10be180bafa636d'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    41
		comment: ''
		key:     '77cbd62759966c03b4487ce7cb3fca652c30198cdc0de5d447256e979e041c87'
		iv:      '562f3b788783bbb72e465c9d04eb555f366c66de32356e7b'
		aad:     '880ac1004984fb3e'
		msg:     '0e677082f7dd9c56bd365310c15a18de78df6d'
		ct:      '95a9bd7bf7e9836e5f8a75393c70da0d9b1d97'
		tag:     'f028003066f8902c5d74ca6bc526e346'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    42
		comment: ''
		key:     '40e231268005ff28c36bd00167ea39131d262f3a591b0d1508c11b00ed04a0b6'
		iv:      '5fb9a00843c4b192bf6c3bc29451c237f30a607d3c637b85'
		aad:     ''
		msg:     'd34b950a1c4f2ae5c94a1fddd6574c5d9c0ab18f'
		ct:      '4ad85a75f1a975bbf3ee5302b71949036e3a2198'
		tag:     'b82c05b09328949aa70bb537e871cd70'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    43
		comment: ''
		key:     'd66e92c86712132b1e3f5ba3a4cd006b9de1fa444246d99ef02e5b190a73089d'
		iv:      '7ade1bc01148ac071bfbe9870fe2023a7769b92312f45e0a'
		aad:     '043cd9069dbd8cb5'
		msg:     '1cf9f2a93cb056fa4222c5850872d9989bc8c185'
		ct:      'dfca9d845c21093f43348a4f6e72e324e9673129'
		tag:     '9defc3de90d493be2a1945d11c569095'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    44
		comment: ''
		key:     '841404f7e07cdebeb48efd25a75444b6de170995cd460e38ff5930dc9cf5eba2'
		iv:      '45ccb4a19073c79a4ac1e052d4664d0dd1c730a6a2e87fe8'
		aad:     ''
		msg:     '5d583f68421d00cd8d95896a091b9bb10b744c61c4'
		ct:      '74634f111539fac80bb29d76ba656e5af90fd37f8e'
		tag:     'c04ce25d27416ae5f181238acf9508bc'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    45
		comment: ''
		key:     '77a812cdbce2b7327dbbaecf6f81340b0ac97589676939d1ff0e69c3373326a3'
		iv:      '89248df60acfa757945d12647a14cc5bc6508bb2b9e4999c'
		aad:     '91b46ee1f7a9361b'
		msg:     '2573f8f0276ce3b2b38fb727575f376a2eeb305758'
		ct:      '0c1afa5419abb32e479b181a6e51cd99eb041bc37d'
		tag:     '6c0b51ea2fc63841893216b03eb47be0'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    46
		comment: ''
		key:     'f2f9bdba59206e8c31a3338213d6a46a40aee237f631906aff076fe2d29d3b85'
		iv:      'ec272b052c33c84a611512a483c3fcec40501240eb7a42ee'
		aad:     ''
		msg:     '408c4cac91b4bd3ce25c8971b1ed8adb20ed667f8393'
		ct:      '59d9c3f18cbc59a3c04cdc6904cb860aae69a5485147'
		tag:     '63e55e220873e295a5b86543334b1715'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    47
		comment: ''
		key:     'd9aa0213bfac5ee89f9ef2c6f616d8f71c3725dafe7926504e18b141192c33b0'
		iv:      'a131b4b0582be36dcce56beb036ec4fc31147efed7ff4718'
		aad:     '1bc37fc6729b401d'
		msg:     '081280932efbce0a5500d76d41c7dd2ddbc3311dc0cd'
		ct:      'd5a1f87dae98ab385d5d34626c295cca0ed6931635f4'
		tag:     '25f2fa45c86c4cb0f02f99050e9d5ab7'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    48
		comment: ''
		key:     'd7b0b278c5ede48da2db2f6ec6f8b23282d3c940bd1eb59f7102bf69c683298d'
		iv:      'df72b7fe00eb070276ba1b0de6b17a6100fe0d660bf3c6c7'
		aad:     ''
		msg:     '0f44c184d297c0a66467d54ac982f922b119d5b4c8b238'
		ct:      '93034cdc9298d0086b8e8bbf3aea637484454015cf544d'
		tag:     'b1e1dcf03663a995c6c14991b5558159'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    49
		comment: ''
		key:     'bd5040047cd7bd0bd1ca22164058a2901feb383c1ccba5c71c853f186d4e2b9e'
		iv:      '0378f12d4891c68477d90f16f2ff59287c81922b73cec608'
		aad:     '04e0e991fb5a465e'
		msg:     '29b7080f92c860ca4dd501f18b041c5cbc5c131783a720'
		ct:      '83a8bbe26ad18129459f66f6dc771c653a3dbb88a00b11'
		tag:     '791971c0f5ba2c8b7635924267c68f32'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    50
		comment: ''
		key:     '3b96dbe28ee07208cdf703f1488f478134147363da1502249e025e0efe5cb663'
		iv:      'c9f5d4dfd5dd2276d68b25c6178d9ef2f38756df4be9d4b3'
		aad:     ''
		msg:     '8f37fd7e3e2f6563a9883d4adb92b5c37242a56b73a6fb7e'
		ct:      '1a0bc208b17fb629200e805da495db70c599ecb3c3b9cc94'
		tag:     '08b9477bc98543019ddaa7ae380f83dd'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    51
		comment: ''
		key:     '53fc679ebe23b70714ab4ce6c8b0de5df656dca27177512654da31f6848dbe6b'
		iv:      '90b932e3464c8b66d3d2fec2bc9097289f147e05f18a9867'
		aad:     'e1b2f309ce5fabe8'
		msg:     '8b0b4038c0eebea97fa1f93b7c2f3576898e7cdc9fd702d0'
		ct:      'e9dd13d48dd7258682311bfec967e1a1ebc562855f224f41'
		tag:     'd9038207dbfc82a9a9d507fe254d57c2'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    52
		comment: ''
		key:     '275ac60ffa734bf86601c951d0bd263b9651181c32f41fce90d59cb8d59da081'
		iv:      'd758776af8d089ef14a075ddf683e6669ed8109fe5681833'
		aad:     ''
		msg:     '1fa3b565515a429f78fb36e93e048425ffb64bc9e9e68336b3'
		ct:      '666f807a6e5d0253fe1967d45efea42cf1f421789b7f48e0dc'
		tag:     '5d423636988dd257e5cbd40ee28ae94e'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    53
		comment: ''
		key:     'ec4d4b14860a36fe8afb2861c1376db8004cc2d37eb1ebb609343daf24bc39fb'
		iv:      '9628e46f25d08b206371449e7321d6bf5d811629e01ef32b'
		aad:     'bbcbfa1779f4122c'
		msg:     '201ec6c1d0675e818cb7a4e583ea1aa1afde1bbda1f0f549e1'
		ct:      '369a80f75ad28fd05cb3c944e0a8c8b37ce65bbd1f6d4b355a'
		tag:     '3ca5005eda0b99d6566ac841340ad23a'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    54
		comment: ''
		key:     '53f9c2c335c1c5cde744e890f6bd291e4484925aaa036f1e74f0144603322648'
		iv:      'ec3dae28ec71ceba5b97a933d30b9fb98a40d4c92e6f54ef'
		aad:     ''
		msg:     '00f4f6a8c09ecbff3e6e825ca676a5cb8373d4915ecaf5d317a1'
		ct:      'b6faccf43dabd8965cb231fe96a2bdf2cb51e0b9afb6445c21eb'
		tag:     'ee91b39d01a114f80a7c5e7e1a0b2868'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    55
		comment: ''
		key:     '9bb8bc991f01fb26df610032e1bf6ed0e2652629a6726aec9c23df4fefbdb594'
		iv:      'a7f4c26140ba7d8a884de794fb23a50c6647627fa85ef9f7'
		aad:     'a6d7d9034512781c'
		msg:     'ebcb0777bd1c3385376270e543521e11f4bac00d0f9c0192581e'
		ct:      'c97a4ba644788bfdeeb0a5de228948902a57359879c82cf8ead9'
		tag:     'bf51aa205497db895f008d828040150f'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    56
		comment: ''
		key:     '69b8b0846c47226dbb278f83082b75476e89a77444bfa06de69395f16c6eed01'
		iv:      '7e4c8d0e24ab24f500053964774c92f808bafc42be0f6a34'
		aad:     ''
		msg:     '3b406d4c07f2ef751ac701fe944b2392bd59fb0ee4b32e6cbf8958'
		ct:      '28cf032caf586255ee3f3f70492d33458a7b42473b8e354d983dfd'
		tag:     '58896a5d7618837701ed8dda9b18d82c'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    57
		comment: ''
		key:     'aa6d2da8fe7ce3228f15e09ae8c7f3d1b0220679a3e0e13e7523060b5b8d09b6'
		iv:      '26b2165f4b22415df4c052564b87d62c4c2c01df47c82cd8'
		aad:     'f5fa84749ff438f4'
		msg:     '92763e759a5c0b8c4d40d6398fa9e257900ff4b1f31000dbd9a15e'
		ct:      'be95d62d6acb3e5344f6b4ddbddfb45fa479c2d1577a42967dc0ad'
		tag:     '61ac094fefb1237c9d44ab7f4bbbf5f9'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    58
		comment: ''
		key:     '31b9e848dfd3dd1ec05410975190109f550ee6e5235f040ce6faf6c380fba49d'
		iv:      'b595d9204461e311915cc17df51a3bbfa55c3a98aafbbaee'
		aad:     ''
		msg:     '95272cdea7a15889059b4e1de058c869e1776384159539470b542ed8'
		ct:      '2248e5332ed42c42fcb6a029e3d8f9f96cbc32d34fa5f302fabf1bf3'
		tag:     'b777e88479292944c5d6ace1ffd24ac2'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    59
		comment: ''
		key:     'da132c34b2291a15777d3ebda2ed0078028c215038c2410d822578dcc869ea8d'
		iv:      'bc101b6d01bda7e13d402aa0023f0507ab02aa58758cb6aa'
		aad:     '96fc6284d7eeb53c'
		msg:     '331f3d53965bfee2edb463c5b21751eb445289287fada2aedae99258'
		ct:      'b10f9fbd87f51ebeae1942b9afb59749987b1575babd8008b281a662'
		tag:     '54ad4e664b86333223fca6869c501dc2'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    60
		comment: ''
		key:     'd7e5e9c008af44266c876fa6b02a453854703c1a4fd221573c382c8d512a982d'
		iv:      '4adcd5ecf1506fe7a38adf5634b454bf90278c9ebffbac87'
		aad:     ''
		msg:     'f8b3ae84d6502d353d57c970da5f9bc53de7a5c6262ba7a7b2220d0ee1'
		ct:      'ffb587ec97c7d11ca75629f066881f6b2c392fa71b73fc4cb4559a645d'
		tag:     'ec9db510c3bb11831c20684d82e45053'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    61
		comment: ''
		key:     '1e72be02d7ebf3c78b400efd005f5b6b983ede08443541475808d43e6d30eab8'
		iv:      '055776b422138960f6631e3c58f3ba0688082747de4ae5f6'
		aad:     'cec8c976f2e25979'
		msg:     'f2654733ca29af4bb29347f7a6508ed87913e0faa885505928ac1ee86e'
		ct:      '5d3ce03a6f43eab32a91b6eb87666af14e5e28d98d23c49c56557497d5'
		tag:     'b324b10851d159bd3822705a9d638038'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    62
		comment: ''
		key:     '98362eff7af1e38d3d77d4a013bb6bf3fb3690568bf897651c578b21572fd37e'
		iv:      'cb52ad5674aff0762ef49fb3bed4722dcef2bcbc4f3c316a'
		aad:     ''
		msg:     'a40610eaf3a823c06936293473ca36a2952d0eb5e5bbc18be123a07f8bc8'
		ct:      'aa6edcb0f49535b2d2fa2e5f0b29343ba0c9c1667c401c78a3a8b8a61ad2'
		tag:     '98d5e90a5a64e411c98d7c9e91557f5c'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    63
		comment: ''
		key:     '8f0e3dc43b86943ed4b0361fa5aa49999f24bc1e102bf3afb439e44f9ce43504'
		iv:      'f2f09c3469e2cf73b07620e461d7b1ad999c5f7d54867d21'
		aad:     'f5203e702570c4b1'
		msg:     'dba4ed2a7938826c43548f6976d8f0ec1838fe71cc535b2a5d56e4d3d5ca'
		ct:      '3d1add00e51e60b16825272790ff47c0d533bfe65484d105ee7a69896c48'
		tag:     'a018e2629d5656920f1202e65624b056'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    64
		comment: ''
		key:     '16a376d68b3105262a07558e5e448ecdcbe075770cf60e7b7db1420f4fa4e36d'
		iv:      '17d6ff40ad135ac9df55fa5c0eaf03e5d91cdac63c684e8e'
		aad:     ''
		msg:     'bdb5500794edd38a398f18f83de03e16f135ea960d3b8c6578abc541aa1d03'
		ct:      '1ca6389e16c2f43e9e89447991d1472c8283a8dd94fdf61c4f5aee746cb537'
		tag:     '33107bbbc06e563abf48979dbc7c66b6'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    65
		comment: ''
		key:     '8e1fb8cc57ca60ae091d27e292923272439c37f2dede36b2c2aaee96439d5a31'
		iv:      'c306b69443bfdbedb5ce9f9bb6088132a88e8a175d3bd769'
		aad:     '3fe9ad465d0aa3fa'
		msg:     '1d884a83a5f9b00b8951ef81778bd7c991cdc911127eee9dfeff82c48ca937'
		ct:      'e8ae311bf2e80d696c543cd272d3e50dc968a0ab47259c461e0dec35f77530'
		tag:     '906de4c31eb2ce283eeb95388b0d83ce'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    66
		comment: ''
		key:     '2ed460a56867ee1a2877a8f3d2d98fb886cfcc8913e31c3d08f42374ba37ebb1'
		iv:      '0140f2791eb81fd4b69edf2d9ba4b2d62eab1d296741583f'
		aad:     ''
		msg:     '318cc4bf151c3baaee5a783ec091ab618f2ecacf38c962ba9c32c323696cc94c'
		ct:      'd34c1778d105d0e80d429c86b879d52835cf8aebc5a04a9084cff1f9646e040a'
		tag:     'ac8a68605a0567c559442342b764b964'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    67
		comment: ''
		key:     'b43328e39cc6f6e94ea601fbebadb4b41cfe6a52c3a4d5eeabaa9853db45ccb1'
		iv:      '97438f178419732feaade58a5d5c21bed14d04c4add50465'
		aad:     '1264b91e71865033'
		msg:     '63cb5c20c9edf36757b795921437d3fd228af1fcdbb329505cbdde12afaf9f84'
		ct:      'cc24cfa62063d11b2c31cf25ceb7308ca376feb1dd6bc102ed7db8ed46b06759'
		tag:     'dda7fc160e23f57e8392809f1e3b5ee8'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    68
		comment: ''
		key:     '92b9b40c00480a50ee16a86349a46e37b02d5ba74d2e5a67eaf333e467fa0152'
		iv:      'daca1f50a4c0d9b77151c75f2e58ce404847d0aab493086d'
		aad:     ''
		msg:     'c857f3c55da61d72563912a2534e01b6426ba41bf417c15b725086d31a1645c94d'
		ct:      '82be237be008228a8a9ff1a506d5b893cf9dcaa1dd33c0523b13582bcade4629ef'
		tag:     '723437af0b684b6e04024352206cbaf9'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    69
		comment: ''
		key:     '5c271bac09a0454c83d158bcc9ec331ca92e62726903b7bb5799adff47d671ee'
		iv:      'ffcaddf85da09293c4352c81cbb5dd82e30b0f9e7623e92a'
		aad:     'aaefd84240ade0ed'
		msg:     '7c716a5b6cf0b8b0e1ff825ff9324bb5715b0d40af5338d5337f66de681932d423'
		ct:      'ff98ead89d45d70f09b9e3f31f4ff56ae8b8cad1517294a8af3c962bad24a92efe'
		tag:     '4b8a06a1613737d0f8e3fb88184b23e4'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    70
		comment: ''
		key:     'c28403cce44ff256d055c2cbc84bb2d9773346e0d51bd38e80cebd861b03fa30'
		iv:      '64cc9f3cc334abce364cec9efe8ad54117ff0bbb03e3e8b9'
		aad:     ''
		msg:     'f9e8f60b70044b03a189c26f1c8fd246239bc23f8adf0f88516f88d73d11c9290882bb6ad49d956b10c9f848180065'
		ct:      'd0e84c6450f348d887c49c4b44ac38721d4a1742e72095c330249c7348bade49dc776d449272e0f3dd5422c2a6ab18'
		tag:     '28c72dea441cffac2f7811286f8ea5dd'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    71
		comment: ''
		key:     '7c72c748ea0010c90e1dfbde8e91edf6ead2474148cf234e0559dcd881cc3b2b'
		iv:      '7b97c8b1c06b69b99220042ab2ac65b88d8b4294b76b4bd1'
		aad:     '7185f9cbf59d2095'
		msg:     '9a1f6c42a8a0f3032e8dfa36e0f5750479276866c920672a0454c41bfae5dd74fbf0fbcc8e6fbf4843f20d06440837'
		ct:      '4093dcbca1555835b78140fe7a3798a77bd97a01b0a7c1f7157fedb27c40d9d16cc3e935f649faf0dcf431636cd539'
		tag:     'c7c9133ff17a296c987d72885182874d'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    72
		comment: ''
		key:     '7948151a374363d07dfb12869b7f90502f2de8117d3d72d5133b9b3e3dc78ef7'
		iv:      '8052acef0423bb07a6fbaf8f63039f1eaa2cdefc61b31b18'
		aad:     ''
		msg:     '76e03034be5514561e99c32ab58901eabac0f67b40c366202ac8a08ee3f68c3b283c1adeefee6f5544330d4771e5148c5231ec27b3f3f9d81a3dca52e115e1b5'
		ct:      '764ab84b844b57b0564f63ec70ad12d81dc3a0e65233a9bf06d6b2c653787eb991bc37a885a04509690ab49fd8dedcabe3c346df9036d735de3bf73ab03f5ba7'
		tag:     '075248c91d1f246aebaa96c86627d18e'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    73
		comment: ''
		key:     '50a1b2b155150936609d45596e9175f3271be548574405f827593fc5a0578c3a'
		iv:      'f357e3b3d3d5e4187e34da08afd4817635adde91b676da1d'
		aad:     'dc514d540551b9dc'
		msg:     'e854b8531ace95c975a5b1497f3dec6d80b29ca673690411abe277bbfd29fa00133ee17570805c1c605452d648581be8db878e782f217b481b1268591593efc0'
		ct:      'de03f775aee744e4148e008dfefa7156ce2a23a613d4d9cae99c3164f54a173f895a9466ef046c020179383d70c813e765f207860c79dcf627f17663ea76af20'
		tag:     'b473a9f1d5312d556bd0b62d84bb0803'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    74
		comment: ''
		key:     '92570a01d2b6123b67055400c8a9b0cb948e32c9b8520758cd1abd73f83c8507'
		iv:      '6d609141e3e4331f55344c1f5e6fad589b39ec1d12b9fbf0'
		aad:     ''
		msg:     'e86fc97c194d37a5e1345d139fe82dd669b6350c435cb446fcbdcc90fe5859bb2ef1f69d930e29dc343b57dfd7ff3c382652939bbd1c978a790ed1dbe5ad1fcbe157925ab4335c649c2f80c19d541e9e7eb4feb64e596bc6d7df8aa3476e0a9f7e'
		ct:      '98fc26e0cfd5a75b5bcd9e046e89c6e9dc5aaefdd5e8ea7e4d286dcdaca0fe6ae744d244678f91c9ccf6e294bd5586be671645ae87d3435836a5ab383b253602c25a6cc04353c076725b4fc4aff9b4dc9bd194fe92ef0a920f15d6b8fea9f19065'
		tag:     '03bb49593f116a30a8390f96380a9888'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    75
		comment: ''
		key:     '4a3bc8f5c4aab87c20772404a291c1d6d68eb12e5f3c82e582564d6300fc28c9'
		iv:      '1a80def5bd8be8eef5f6643a5c1aed9947c3ee5ca0cb56df'
		aad:     'e40cb55a18f2885f'
		msg:     '2b0815f7eb0a83b9617e4f0906e9179b600b0c822bfb56c5012103aecb4550a57099dcebae00b6c06f3537fb1550c78b249d00a4007d23b882cb5511fdd53482575554028e9db437b8224368ead730d157a64d5571c706cbd9c0d2b10b3b14c3e2'
		ct:      '7042beb6e4f08e583752f23048e2f3433e0821423d72a7e531b86684b57b32c5bdcc11164db0b8516d7b463cf7f8b0e3ed8a7d584345934ef184e4f8fee31e126601f08558c725aaa23d38c8017b07adbf1e742128795b03458b581b8cd9100bd9'
		tag:     'b5e3df83f18cbc0bd99427b9a172bf1b'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    76
		comment: ''
		key:     '2e89767b15f18b855d04c0b6b47c1f8facc9a058e2194ad2ad901ef940ab54cb'
		iv:      '3507ec4cd1a6c2eaf081ec32888e08839481f35b3b0f7872'
		aad:     ''
		msg:     'eff2e375228756f995b8ab52213177c4b7ca92bc81114f5c23aa64dd7eaff7b86ee2e674984c4b65bf4c5ff402e23902c005e05de25b3c6e8a64323aeafe04ec6cd1f6c851be39e55208d76476d3ed7100042eccb72cf1349ea101253b7a5a4a8677c1d6df5a54e9c24558e2d68c3f50acbd1ebbb4773884b0ff23d95a4ff60d'
		ct:      'cf2e17f9d8c6562de6d3e8c8bc30ba2904cf5c3616d15ea77667186ee45f444ea264327dcf210b6735a39005b62529d557480ed0462e49d982cf5962e5ee6d8ccc388d5de102e676a55426ce5a873d2e84a2d841e7b30c7ab19035274886b3c5c979d065bdde9b0b9e466b22559e30a5a5abc4817312e15d2c0dcdd99d867361'
		tag:     'c844d555bb43a83b4aa735b2aa1d566a'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    77
		comment: ''
		key:     '6357cd94e2d9503288eaf3abf9604b050d4a483350a828029baaa9cae184f075'
		iv:      '0a5914f29abb1cb48dc686159f09480370477f6069018e18'
		aad:     'e2f0d2f16704527e'
		msg:     'bb266ddea2f88c2f0fea7f0cf4a1a33363344fb49672b821f76863a9edaee638d75140d21d848efc475d3814911c8bb34202c4e7ae0de1a57cea6f3af7180be454d7bc6fa5c02a999dc71eba7d5553828c963c1b7c559afa2e30e788ef2d0b479d0da1f24fcda5548773e77abc716f498b08bfe69b1a7e4b6fef27ccc72686d4'
		ct:      '4f0e805a2b3f2e1bfe3c06c83f5c77b9c4e562514a78f9f2cbf3206f68f686923656885878087d17da261666e798649d74841753525875f425e82a4795fdf8dfb629a8b1d2faa5594557d62f421f4e6a5dbb9f8336875f2fe2e2a4a1d0084358d9583e6b6662895a07c924c0a7cdba07be8a020e1b8ef3a0b5d007ec47a8e8cf'
		tag:     'f130ad7a2b7dad5e8f8715eb5b93e45e'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    78
		comment: ''
		key:     '57f37ad4992d336d13c3967c701e60c7842a55195687bbc1f680a33e78e0658a'
		iv:      '74ef0301cc545539723c78ea9e2d75b851ea8641df1685d5'
		aad:     'f5'
		msg:     '58fea87518e42b504a9c53035081aabb'
		ct:      'fe59e8bf4250dc02843f3be602a7aeaa'
		tag:     '012c6e4f5017c78b96247763c8ff5f68'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    79
		comment: ''
		key:     'aec0407a0cfa59096a489edc29e40cc67843ce71a95afc8deaa409a655aeba3d'
		iv:      '4a75dc9936c891cf5385f84e2a6d484c612115b9ce053f86'
		aad:     'b93b'
		msg:     '8afe8b8b22ed249e21a44247345ede5f'
		ct:      '7b2f702cf01a0007f4bd949230197e78'
		tag:     '56bddc4fdccf099f128b177b3cc73520'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    80
		comment: ''
		key:     '9b0400ac1a917c7571430b78eca2c108e1824a078f21eb1995bfeae7fcc51f83'
		iv:      '91752e5f97bdadb6a9811c2144e27f73baa9458b6c58c9a0'
		aad:     'd0926a'
		msg:     '2b512160837e427a04c6bd9105cd2304'
		ct:      'f95dda4c5457dae8daffdf0ba5439ba7'
		tag:     '3e97e87d976efe3de7d84df933ac980d'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    81
		comment: ''
		key:     '85b2f31409600b36ba8013f79b6aa84c9509546e218edb75c77d743a781c9bf4'
		iv:      '95150d436a72c173e502ac22df904f26c0f4edffc29ca98b'
		aad:     '6cb8a59b'
		msg:     '44872f0602c76c1d4d36fd462cc886f0'
		ct:      '9d2527bc8df2e71d20864e7789ce2a84'
		tag:     'acd61c1b526988ec6105855ebdb7533a'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    82
		comment: ''
		key:     '76b087aa42ca8bb9a42133eb9279da0d0093b4e5028f4edd1c2183f81e6754b7'
		iv:      '383cd40e9aad35c35e3a46021b90acc87d51255be3443a7b'
		aad:     '20976ec087'
		msg:     '671e3615661511a8e668941126908c84'
		ct:      '9d403b239c7497781bfb4468bd930cfe'
		tag:     '83dc6dfb3ae111ab05ac30116b89d65e'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    83
		comment: ''
		key:     '335d796b0dff04636e39bbc408ad6aef0d423fc1772994e61409396c9c1ff1b2'
		iv:      'da3fe15576474fe36bc3d2c42fe505617454c23aa1475e80'
		aad:     '03caf0a03be7'
		msg:     'f3a55e4591e697a9f1aaac2eda219c59'
		ct:      '610417044213e2a64c1b9b2fd1839268'
		tag:     '444c11cdb783c3b432365ebe12378c9a'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    84
		comment: ''
		key:     '0a059d6ed699ffed57c6734b67eed5bd62d508772e0d1edfb5793f805908b035'
		iv:      'c991adee7c6e2bc5aeefd24c11be59a429b3198a3ea372cb'
		aad:     '16317d3050bf51'
		msg:     'b1ecd5c730695626454e8f89a598ad23'
		ct:      '25a22c28b26f1712ba56b46e0103c444'
		tag:     'ef27336fed160e6bbf257fb0e7770aa8'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    85
		comment: ''
		key:     'b992dd885d0499a17268656665fde641be102d2fb642992d97e3107ee9aba20b'
		iv:      'b755995c547fdf21a2398d1f4adc6476d1291b1723a331ee'
		aad:     'df2f4f832de7a1518e'
		msg:     '5fc0609d86c5bd4e5e9e335cb1954458'
		ct:      'ceb42438dc40f7a0cc38ca0b9a48091f'
		tag:     '7c1045faf49b58415ccdd2a1e2bc4429'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    86
		comment: ''
		key:     '58fcfc12acbb234bf13d28b856693a0952245bc0c1d751c52bca708c7a196137'
		iv:      'dd62b11c6826d2c53a8be69860f359a703594efaa42411a6'
		aad:     'faa3abe6bc4eeb5316bf'
		msg:     '66cc8a0fad0f6b05f0422b53ce8fecb3'
		ct:      'dbb22f3a39c46abe3cee3980c1df88bf'
		tag:     '81da8bfd8ebbc1eca4870f8196156e3b'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    87
		comment: ''
		key:     '2765b4d865629fc232d37ca5e240a8532dc9a3e381daa7ca547ff5da5c417e41'
		iv:      'ca05d9a76be1149bcc4ef529b305854f7990b20aaffec384'
		aad:     '9a227709205f1f740ed232'
		msg:     'fe956a36f31adcc13ccd325f7a17f59c'
		ct:      '4905c8641a0ee3ea6687ed7452527903'
		tag:     '7d574e549b5cd377992de204627de5fa'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    88
		comment: ''
		key:     '430878f3ab311fb40d2c9b0f534a4043350f0ff495c80122355ec2b7557cb831'
		iv:      'a37403e233f4fed7e00d9bf3a5407eae04fee3d667b65493'
		aad:     'af191751f447cc49efff74f8'
		msg:     '6e775f424e7d9d8e23cdbf14607d3a44'
		ct:      '06218a47245edaa15a9f1162ec011d3b'
		tag:     '478423cbe48897ce756e3adbd9a1ee9a'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    89
		comment: ''
		key:     'a2c4d1d5ab1dc812200e18ccee9ef797195633d355a873c90f6d051041177cfd'
		iv:      '1ed54a330d347fb9ffcc68cabe540f2ff300cc3ee2691255'
		aad:     '455fd94646f6edf9aef71275e0'
		msg:     'cfac1a30be69f203e6efda92a19682c1'
		ct:      '27f9bbdc89a079abba54ddc01781f11a'
		tag:     'edce3ba94ea658694368d78878ad9227'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    90
		comment: ''
		key:     '89f5bf87986d39fdfa8debf5a9810d3cf186f277f5fdc3f849ac7dcce6381205'
		iv:      '6cf744267d87aa512f949e66579c074c6ac371d5228adff3'
		aad:     'd5736a239a1e598560a84a81a60b'
		msg:     '9775a1e1ca33d579075e0a80f2bf1184'
		ct:      'a712f0a4c9932e6413bf501508693a5c'
		tag:     'db8c77d539a6b41b6fc2d32ddd612ba3'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    91
		comment: ''
		key:     '67af73796c9eba6ac7a847825cc56fac92595a8eb17ae2fece4a1f09c9d8c85f'
		iv:      '7c76e9bd896c75245444f96fcfc419da23cf09b3be3610f8'
		aad:     'c3036660fc872e55b0697104be59a2'
		msg:     '3454b49cf7d10ec416770f76aa73bff7'
		ct:      'e2322c9638222677e4b6f7fa474accaa'
		tag:     '82712972e906ea74f99dfb642c560db0'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    92
		comment: ''
		key:     '73005bc9d00e9688afcb340ea7cf81113d49e33d628e13b89949920102b1a9c1'
		iv:      '367a95373b3f2bd4f2bfb03619368639fcc19eccdeccd04f'
		aad:     'f15449e7c7810a11609f5da5e33b9085'
		msg:     'c47c17dcd3efabfe2de42702f27a840f'
		ct:      '7732ee206cd5734558c2f05f5bc1907b'
		tag:     '4e32369f9ba08950b27b7952c3804fe8'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    93
		comment: ''
		key:     '3a0c554dc2242950ec97b63a7f1de739ce18c247f4ce1f23b539b51feb82bec8'
		iv:      '36213fcb5bff9b54db3c6af8c24a758b29b1143970b44168'
		aad:     '17bc7a713365234f08e703a652816245d6'
		msg:     'c04a2ebfcc30967e691a9ef1c52bcf6b'
		ct:      '8ed2c330b349dac3709bbc8ca2fd6d52'
		tag:     'b6c38642002ac48847c715b317b26a86'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    94
		comment: ''
		key:     '6419d685e6804488ad4f09870db55f2448b82d4715e1d5fefa00ca9e08f21bc8'
		iv:      'bd605dba58a18d5a38fcab1f92f6cb406a276d8d0ca7fb30'
		aad:     '7f1e1f7fcb831cd7501e9608fda8ccb3c54f537ad601c033fdba7f7dc419'
		msg:     '676d9476348a31c6873016ab196852fe'
		ct:      'a4fc8309e455d263bc6b4c95e6c79cb4'
		tag:     '9a439843444888d056b3e45a718a000d'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    95
		comment: ''
		key:     'd92d949112061c11471efa77552daeda52b390efcac420c453c5b8499048983d'
		iv:      '505a1b8d68cc2f77a10ad67cdeea4393a2ea6db590f5be17'
		aad:     'bb044891ccae7f4f9493b8728293b772613c4ef2c088b3922f14466ea32a7a'
		msg:     '161dc03e36cee9f246fd3a45481eea46'
		ct:      '1351d5642bfa9eaf78efd34733bc0b5c'
		tag:     'd666424d2d66969944f2b1a9dba68ebc'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    96
		comment: ''
		key:     '2390931b9c99b9ac7e56bbbb86e6794b36ec3175432f731bea2e3a12c83e559b'
		iv:      '972f9e74b0d118734549fe0d237f0c6249c43674ceaa328d'
		aad:     'cf8c4a35d879e5051b1cff63ac64580ee80a8d80e9b6c90ff841fab3673aa573'
		msg:     '0a182ff667eeccab0f8054405879dc36'
		ct:      '7454d60539e1738ab6ff8609443a90f5'
		tag:     'dd67f6363f66d20541d0aa24008be6ba'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    97
		comment: ''
		key:     'f563e70eefbe6cfd7a0b0d167a8b381fd14105ff4426fa326e9c2e4ca059a53e'
		iv:      '3654bf38ffe7d4dabab310657322af2da359fcaf79a81044'
		aad:     'c69f4dca85af6c39b5991f9386622f98acdb24f66b785cb3636a212ec13bed601b'
		msg:     '5133dcb7ed3fa91add15224a4a9d21a9'
		ct:      'bb5efe5a45ea17d63eb75509452daf51'
		tag:     '31502fc74e063f0636bf9799f02c147a'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    98
		comment: ''
		key:     '7c6410343a2938b9cf2d82419ee8c645fc9ed819b3b2ef876af0b1221ac4590a'
		iv:      'f2f43b87fbb56938060cc9638d3d61ff2ebf26d037e4564e'
		aad:     '760cd62e1d1123fd7d49b670037adb6dae66e7c8a0ca95ffed67a5965a35ca21c0ad9aa069d4edd48b71d5c93077ad'
		msg:     '998c2e5f2900a0ab445b443b14e343e2'
		ct:      '5e8d9d12295525439d0a9fde1a585ca4'
		tag:     '5b7be3245ab2cd28d6b8a4b884e7547c'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    99
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '00000000000000000000000000000000'
		msg:     '60e28a8f89adf230daab792c94dfebe766eda542d7c092d97ccb7501486fc6a3'
		ct:      '0000000000000000000000000000000000000000000000000000000000000000'
		tag:     '52ca5edfb3c4fca83d5776154188a08d'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    100
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '00000000000000000000000000000000'
		msg:     '60e28a8f89adf230daab792c94dfebe766eda542d7c092d97ccb7501486fc6a31ec9568c72a762296f76685b29a5f903cb0198722ad071bde29b48a62d367f3b'
		ct:      '00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000'
		tag:     'a7c21e96322a7f8c453961640791f3e6'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    101
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '00000000000000000000000000000000'
		msg:     '60e28a8f89adf230daab792c94dfebe766eda542d7c092d97ccb7501486fc6a31ec9568c72a762296f76685b29a5f903cb0198722ad071bde29b48a62d367f3b1e90919140f50187df7df42caa37287538c16d481265de62bbf98c235d595c824575acd33c51e271f13844673cb5dafd249dbd394b866c34aecd42c57f2630e5'
		ct:      '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000'
		tag:     '70b88b3bf88b8f11f7513545b8dbfa63'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    102
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'ffffffffffffffffffffffffffffffff'
		msg:     '9f1d757076520dcf255486d36b20141899125abd283f6d2683348afeb790395c'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     'c29cd2ef4874d93267c935cd9ffd34f1'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    103
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'ffffffffffffffffffffffffffffffff'
		msg:     '9f1d757076520dcf255486d36b20141899125abd283f6d2683348afeb790395ce136a9738d589dd6908997a4d65a06fc34fe678dd52f8e421d64b759d2c980c4'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     '02add84dfa902f0d4a11d3bdc096417e'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    104
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'ffffffffffffffffffffffffffffffff'
		msg:     '9f1d757076520dcf255486d36b20141899125abd283f6d2683348afeb790395ce136a9738d589dd6908997a4d65a06fc34fe678dd52f8e421d64b759d2c980c4e16f6e6ebf0afe7820820bd355c8d78ac73e92b7ed9a219d440673dca2a6a37dba8a532cc3ae1d8e0ec7bb98c34a2502db6242c6b47993cb5132bd3a80d9cf1a'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     '82a067b3b3e51cd9d139a5222ea70258'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    105
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '00000080000000800000008000000080'
		msg:     '60e28a0f89adf2b0daab79ac94dfeb6766eda5c2d7c092597ccb7581486fc623'
		ct:      '0000008000000080000000800000008000000080000000800000008000000080'
		tag:     '2bd279a556e3dde6151e698e0496b3aa'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    106
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '00000080000000800000008000000080'
		msg:     '60e28a0f89adf2b0daab79ac94dfeb6766eda5c2d7c092597ccb7581486fc6231ec9560c72a762a96f7668db29a5f983cb0198f22ad0713de29b48262d367fbb'
		ct:      '00000080000000800000008000000080000000800000008000000080000000800000008000000080000000800000008000000080000000800000008000000080'
		tag:     'dc37087d3aaa8b97e985152fa9f1ee04'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    107
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '00000080000000800000008000000080'
		msg:     '60e28a0f89adf2b0daab79ac94dfeb6766eda5c2d7c092597ccb7581486fc6231ec9560c72a762a96f7668db29a5f983cb0198f22ad0713de29b48262d367fbb1e90911140f50107df7df4acaa3728f538c16dc81265dee2bbf98ca35d595c024575ac533c51e2f1f13844e73cb5da7d249dbdb94b866cb4aecd42457f263065'
		ct:      '0000008000000080000000800000008000000080000000800000008000000080000000800000008000000080000000800000008000000080000000800000008000000080000000800000008000000080000000800000008000000080000000800000008000000080000000800000008000000080000000800000008000000080'
		tag:     '3b1dbe65bafcd37fdb15b34fafabc07f'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    108
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'ffffff7fffffff7fffffff7fffffff7f'
		msg:     '9f1d75f076520d4f255486536b20149899125a3d283f6da683348a7eb79039dc'
		ct:      'ffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7f'
		tag:     'e994b729a655f8f48e024354dcef21d4'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    109
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'ffffff7fffffff7fffffff7fffffff7f'
		msg:     '9f1d75f076520d4f255486536b20149899125a3d283f6da683348a7eb79039dce136a9f38d589d5690899724d65a067c34fe670dd52f8ec21d64b7d9d2c98044'
		ct:      'ffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7f'
		tag:     'cd37ef66f2102302a6c41ef31e364660'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    110
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'ffffff7fffffff7fffffff7fffffff7f'
		msg:     '9f1d75f076520d4f255486536b20149899125a3d283f6da683348a7eb79039dce136a9f38d589d5690899724d65a067c34fe670dd52f8ec21d64b7d9d2c98044e16f6eeebf0afef820820b5355c8d70ac73e9237ed9a211d4406735ca2a6a3fdba8a53acc3ae1d0e0ec7bb18c34a2582db624246b479934b5132bdba80d9cf9a'
		ct:      'ffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7f'
		tag:     'b73b3589f174d86aed75271837d73c3c'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    111
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '7fffffff7fffffff7fffffff7fffffff'
		msg:     '1f1d7570f6520dcfa55486d3eb20141819125abda83f6d2603348afe3790395c'
		ct:      '7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff'
		tag:     'fbf9b30e0b9c1240ee0528ba82e961a8'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    112
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '7fffffff7fffffff7fffffff7fffffff'
		msg:     '1f1d7570f6520dcfa55486d3eb20141819125abda83f6d2603348afe3790395c6136a9730d589dd6108997a4565a06fcb4fe678d552f8e429d64b75952c980c4'
		ct:      '7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff'
		tag:     '1ea55841efece2587f6f72c2a2d1e329'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    113
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '7fffffff7fffffff7fffffff7fffffff'
		msg:     '1f1d7570f6520dcfa55486d3eb20141819125abda83f6d2603348afe3790395c6136a9730d589dd6108997a4565a06fcb4fe678d552f8e429d64b75952c980c4616f6e6e3f0afe78a0820bd3d5c8d78a473e92b76d9a219dc40673dc22a6a37d3a8a532c43ae1d8e8ec7bb98434a25025b6242c6347993cbd132bd3a00d9cf1a'
		ct:      '7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff7fffffff'
		tag:     '5adef66e4501595bc742d55c126b1896'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    114
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '00000000ffffffff00000000ffffffff'
		msg:     '60e28a8f76520dcfdaab792c6b20141866eda542283f6d267ccb7501b790395c'
		ct:      '00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff'
		tag:     '81682925f4f8a57392d2a9d4157f2c86'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    115
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '00000000ffffffff00000000ffffffff'
		msg:     '60e28a8f76520dcfdaab792c6b20141866eda542283f6d267ccb7501b790395c1ec9568c8d589dd66f76685bd65a06fccb019872d52f8e42e29b48a6d2c980c4'
		ct:      '00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff'
		tag:     '077001742d67566612633a0b3f3f8c99'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    116
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '00000000ffffffff00000000ffffffff'
		msg:     '60e28a8f76520dcfdaab792c6b20141866eda542283f6d267ccb7501b790395c1ec9568c8d589dd66f76685bd65a06fccb019872d52f8e42e29b48a6d2c980c41e909191bf0afe78df7df42c55c8d78a38c16d48ed9a219dbbf98c23a2a6a37d4575acd3c3ae1d8ef1384467c34a2502249dbd39b47993cbaecd42c580d9cf1a'
		ct:      '00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff'
		tag:     '1c0e1d3c611eda884919789540fc27f1'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    117
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'ffffffff00000000ffffffff00000000'
		msg:     '9f1d757089adf230255486d394dfebe799125abdd7c092d983348afe486fc6a3'
		ct:      'ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000'
		tag:     '93fe07aa08403068124e020ecb06a9f8'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    118
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'ffffffff00000000ffffffff00000000'
		msg:     '9f1d757089adf230255486d394dfebe799125abdd7c092d983348afe486fc6a3e136a97372a76229908997a429a5f90334fe678d2ad071bd1d64b7592d367f3b'
		ct:      'ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000'
		tag:     'a2fff56fff5358337de7f91689e8a8cb'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    119
		comment: ''
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'ffffffff00000000ffffffff00000000'
		msg:     '9f1d757089adf230255486d394dfebe799125abdd7c092d983348afe486fc6a3e136a97372a76229908997a429a5f90334fe678d2ad071bd1d64b7592d367f3be16f6e6e40f5018720820bd3aa372875c73e92b71265de62440673dc5d595c82ba8a532c3c51e2710ec7bb983cb5dafddb6242c64b866c345132bd3a7f2630e5'
		ct:      'ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000ffffffff00000000'
		tag:     'db4ad6b24a53d2617f7262d2a586d5ca'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    180
		comment: 'edge case for poly1305 key:ffffff3f24ac6f2f6436cec230be9ab31d8434bf94e1042d20952749a99cf641'
		key:     '606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f'
		iv:      '000102030405060708090a0b0c0d0e0f101112133e8775b2'
		aad:     'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		msg:     '7ee395bd21ada42ed12310d34918a28e596a49ee7a22f623d756b896663f68733e6c71a344f4726ac24e330679f25e492be08603aaa23f1e88c10299047c8e585983332a8b6eadcd9b6061b63fe3b58a2021b38c7cf379fe9a9f6d114f3cfe422f91af78c6fd87d4269af0e3e471abed457ae75c027e134c96cf4d9a4a646288'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     '4921f7c24a2d42f4da7ad9d45e8ec26c'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    181
		comment: 'edge case for poly1305 key:bf358f18ffffffbf4b62ed6e1f53790785c4dabdfc72e2a219d377a682c85f38'
		key:     '606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121303e9b9a4'
		aad:     'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		msg:     'af205bda819f7451be0f28667d4b01b59ff2daa8173cab52046c3c9e0d989889c5e021ef7afd06e9ce6cc30e3a6ebab509134ba10d10e570c55587c13eee53e73be54804c8539ffbf23b35922b1ca37b9e9bc24ee204837ca5a294ce05d12600c7eff6aee32270db2feff47dc5a04176169e15850628e6035f78994f9f56035c'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     'b86b0a8e9427af3516950efc81d935d5'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    182
		comment: 'edge case for poly1305 key:d0b7b3a352a4010ffeffffbfe8cc66dc6e5e7451dc61762c5753174fed88e746'
		key:     '606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130700b982'
		aad:     'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		msg:     '68c67272036fb652a0182eeb4781358e4704a4a702fd731bf3b3ea994717989e7d9104e0ae81732a8c7e9a82b3d31d541761a366b67c3396f1a6c67e293ddb65a59e42541dda144dc6c78388cfca982e23350958ac5b3d54a1722fd64733577862e1879c9e9445ebdec5315d1706db7ebbedd4c779935e72057e5b0ecde0814d'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     '3661dc6ddd1852221050ff5b8d58c13f'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    183
		comment: 'edge case for poly1305 key:7bee33931a4157a8cb701becfeffff4fbe7e69f19cd065313bb49a252628dd3d'
		key:     '606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		msg:     'c483b7334ebe2e879b0c3f9db4fcd9f5219062360d6ce44cdae0f94e04c8345ea7e3ae33855118741dcafe0de4ae98c4e43af7b12b04ee8ab175625823ac040e5abac4403f1d45238adcb8c0cf44bd56917f9f5d93974c82b56951986a9c0450bd9047b5a616e814526ad0580e3ecd8189c9fef2cdb979a22ad3a01930fbd15e'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     '02c70e4defe897a47a65063a468db630'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    184
		comment: 'edge case for poly1305 key:df39fb3f36d8e58f91abffdff9f5feaf109d0e960edcf2b728446ec175ad4c7b'
		key:     '606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f'
		iv:      '000102030405060708090a0b0c0d0e0f101112133f1a8eb1'
		aad:     'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		msg:     '85e40e2106db6aba0fb236d3c980a72e58ce538db7aa3b0326a23d52175c7465c454d8206b4d8aedd51d8cc47424f6124d2586370f4eb51153d215e48347abf8791a6d6d3da4871ab2c0fe5718878c3942365fc75887e6ea6e779911f883fe90b6c0e5870769a860cf619f91c7eeaad69212325404ec4de4d3ab5e7aa89537a4'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     'ecccb94178b76a769c91c27d921fcc6c'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    185
		comment: 'edge case for poly1305 key:00000090e6e328c242cde5c83e3d8262d467f2bcd53d3755c781f3c6a2cb0648'
		key:     '606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		msg:     'eaccaa778935ef249e0900149dd889462d2a061486ba102b8caebe465f3959fb3119ebb5689676ffdd6d851a26739e772b54a2f5f473ea9c7e58ccbc4cfc953e8c420b2175d9dd519265630bb79bd87a601b113231a8b16ce54c331347ec04c2b1c9160f38207aa46e96feb06dee883eb422fa14908df300bb1a1ef758c408f5'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     'f00ee0097d7dffbd3e4b216c45da89ef'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    186
		comment: 'edge case for poly1305 key:9e98d64e000000505a07183c5c68c63c14c9266dd37ff86aafc22ddbdb355617'
		key:     '606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130c807a72'
		aad:     'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		msg:     'a76c330e015060a17e64cb7b6d753f201f75be8759fd7539fb92b22aef54c9d3029dba0c15cbf7c95135888319c6b2e6276da21e0c351fd522b29aabb5883a3291d6f427de773b124390ef6fd96621ffbc42dfbf7a34da272cbc9ccb1a498d078033d1ac3bf7e92715948b06d69d5c5039e9164ba9c3a02219ec5908206b3bd2'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     '8691693787763ec6c7bf957658b51370'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    187
		comment: 'edge case for poly1305 key:1048a92e65f5e63102000080d9ae08de4319a7c45fdbe707b9ec1b7e0d635161'
		key:     '606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130397a143'
		aad:     'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		msg:     '228a7e15bcce13051de9145f77f7f4ff7921828b4f99efc4ff55ee0d9344955b69ec2d4798b0517f0273c4456ae5ffc5929cbe74ddb0da51d4f2b4df7578a31240c88ae922c3c5eca7b97d72d497062050a587447c562b343d5c71921944872f9fd06b8f34b3eb5d4341f5ff8a907dd7c2e1676b81252726ba54814da51eab8c'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     '7fc8d4bb91c543b9bf5dbf1e7277d823'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    188
		comment: 'edge case for poly1305 key:01517a2ceb89bbfb5741f7d9000000401a65b132ad661072a00ffe7defbb18a5'
		key:     '606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121308cb0f3f'
		aad:     'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		msg:     'c7d843188ab193dfef5c4daf583f952cd4b195f240fa2e704d021723023c123371a41e87dfc6e6c3874a42f331cf035988a38c72ba2da854b1208f98bf8cc29948169481ab3a402d5fcc7ff78f9e31925576dc3938074b8c5b27960e3afc750ad686563688b7441787288d5256c1301d563b7744843bd1ab4eff5be6f1653d44'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     '834c91a6580bf514dfcb5e2f456efe3c'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    189
		comment: 'edge case for poly1305 key:e73c0100fbd50c408e3c06701c3908209a66d9388dd8e29458376300cb04f56a'
		key:     '606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213d580ecf3'
		aad:     'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		msg:     '56d20c8500203274099502f38d547f3008588f396cb521a2bae1800514f1f797c00386d52c09fd64a28b393431848e13dda47f65536bfc681ca73b55a7fc019a4c8358186e009ad3e22a5f08a59b19ca4b3bf11269fecaa49a9e9aff53a02ce2f235fba061ee95eae6177f1153502a50428122a73c83695f17dff5cfde23fdf9'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     'ca3de68e124484e8bb825b069afaa53d'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    190
		comment: 'edge case for tag'
		key:     '404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'abffffffffffffffffffffffffffffff5a20e89e14ed5af85da66b5e4bdbe002'
		msg:     '660336ffb732a4dcda556c2539d3d2de6cdaed0d7d9104593f8ed69bf0db8aa33c0e746482b7dc53d40b8a5331ca33b874639cdc7a787badd436bcd56e798af3'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     '000102030405060708090a0b0c0d0e0f'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    191
		comment: 'edge case for tag'
		key:     '404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'ffffffffffffffffffffffffffffffff7c85b8e5991711f804915250b99cf7a7'
		msg:     '660336ffb732a4dcda556c2539d3d2de6cdaed0d7d9104593f8ed69bf0db8aa33c0e746482b7dc53d40b8a5331ca33b874639cdc7a787badd436bcd56e798af3'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     '00000000000000000000000000000000'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    192
		comment: 'edge case for tag'
		key:     '404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'a8ffffffffffffffffffffffffffffff57599fb21558a903b6a3193419537e06'
		msg:     '660336ffb732a4dcda556c2539d3d2de6cdaed0d7d9104593f8ed69bf0db8aa33c0e746482b7dc53d40b8a5331ca33b874639cdc7a787badd436bcd56e798af3'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     'ffffffffffffffffffffffffffffffff'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    193
		comment: 'edge case for tag'
		key:     '404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'c1fffffffffffffffffffffffffffffffd71560c5091b863662ffaebc0dd2501'
		msg:     '660336ffb732a4dcda556c2539d3d2de6cdaed0d7d9104593f8ed69bf0db8aa33c0e746482b7dc53d40b8a5331ca33b874639cdc7a787badd436bcd56e798af3'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     '00000080000000800000008000000080'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    194
		comment: 'edge case for tag'
		key:     '404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'f9ffffffffffffffffffffffffffffff169a825d7ecbf7e107396a2a3dfb4508'
		msg:     '660336ffb732a4dcda556c2539d3d2de6cdaed0d7d9104593f8ed69bf0db8aa33c0e746482b7dc53d40b8a5331ca33b874639cdc7a787badd436bcd56e798af3'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     'ffffff7fffffff7fffffff7fffffff7f'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    195
		comment: 'edge case for tag'
		key:     '404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'd9ffffffffffffffffffffffffffffffe344f9752a885ccd1a3fa5a9c4187d04'
		msg:     '660336ffb732a4dcda556c2539d3d2de6cdaed0d7d9104593f8ed69bf0db8aa33c0e746482b7dc53d40b8a5331ca33b874639cdc7a787badd436bcd56e798af3'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     '01000000010000000100000001000000'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    196
		comment: 'edge case for tag'
		key:     '404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     'd7ffffffffffffffffffffffffffffffa6627ce99c9c49deb89855b0f9e3f407'
		msg:     '660336ffb732a4dcda556c2539d3d2de6cdaed0d7d9104593f8ed69bf0db8aa33c0e746482b7dc53d40b8a5331ca33b874639cdc7a787badd436bcd56e798af3'
		ct:      'ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff'
		tag:     'ffffffff000000000000000000000000'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    197
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'b3df302dcb7dc4ea184e7fe455afac170395c7a18e950fff87e6de1fb6247d63c3df6823c0030e4c987ef266c4a26f1de7226805d43221db305501f8bd70d20c019264f8f28a963bf61e76272e736412a650fa30062629ead26c6ff651361dd1'
		ct:      'ffffffffffffffffffffffffffffffff9bf90b74324f392a4f5bacf25b31b293b4ffffffffffffffffffffffffffffff09b73f897139ab1417163ce2e8377d03b4ffffffffffffffffffffffffffffff09b73f897139ab1417163ce2e8377d03'
		tag:     'eabfdde61ad23c8f2a380280248e58c3'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    198
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     '0d4381de8e79d6e26f93ecb286f0c39beb93332a4325c92a37428d1212ea300f8225755c15bffd71892a7cdfb3bb10e19d6aa8735af47530d8bcc2e5aab850f04068798727366506e74af89e596a1bee'
		ct:      '41634e0cbafbedf788226ca92ca0907373fffffffffffffffffffffffffffffff505e2802a430cc2eeab714688e6800373fffffffffffffffffffffffffffffff505e2802a430cc2eeab714688e68003'
		tag:     '50c4a57ebedcdea7ca65660b209f59a5'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    199
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'a098e743ac00bfe73bf07e350a8681030a93332a4325c92a37428d1212ea300f5952ee9617b1e5f6fef674f5d57ba2e27c6aa8735af47530d8bcc2e5aab850f09b1fe24d25387d819096f0b43faaa9ed'
		ct:      'ecb82891988284f2dc41fe2ea0d6d2eb92ffffffffffffffffffffffffffffff2e72794a284d14459977796cee26320092ffffffffffffffffffffffffffffff2e72794a284d14459977796cee263200'
		tag:     'e87f0c943e93cad2aa76933330d178b3'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    200
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     '4f84f6651a29fbfee258748e1aca766af793332a4325c92a37428d1212ea300f4efc9bcd40b2c933349ccf9943bffee2816aa8735af47530d8bcc2e5aab850f08cb19716723b51445afc4bd8a96ef5ed'
		ct:      '03a439b72eabc0eb05e9f495b09a25826fffffffffffffffffffffffffffffff39dc0c117f4e3880531dc20078e26e006fffffffffffffffffffffffffffffff39dc0c117f4e3880531dc20078e26e00'
		tag:     '66eaccae5e377108c3cbcb65c4cf1fc5'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    201
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     '89c4ebb097d8a8ca651efd999ec5e978f293332a4325c92a37428d1212ea300f1441067a4cbbeefa9753105cd0f774e2846aa8735af47530d8bcc2e5aab850f0d60c0aa17e32768df933941d3a267fed'
		ct:      'c5e42462a35a93df82af7d823495ba906affffffffffffffffffffffffffffff636191a673471f49f0d21dc5ebaae4006affffffffffffffffffffffffffffff636191a673471f49f0d21dc5ebaae400'
		tag:     '7c10f4defe910369bb3131c06800e6e4'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    202
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'fddf302dcb7dc4ea184e7fe455afac17939c6e78f0822881ce7c8770358492f05edf6823c0030e4c987ef266c4a26f1da1256412895a0a39e02b6e33de1e470f9c9264f8f28a963bf61e76272e736412e057f6275b4e02080212003d325888d2'
		ct:      'b1ffffffffffffffffffffffffffffff0bf0a2ad4c581e5406c1f59dd8915d0029ffffffffffffffffffffffffffffff4fb0339e2c5180f6c76853298b59e80029ffffffffffffffffffffffffffffff4fb0339e2c5180f6c76853298b59e800'
		tag:     'e269df519b62d7658b8ce3487588f409'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    203
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'cadf302dcb7dc4ea184e7fe455afac1775d301ea3449120a660e322211c0faf2ef3bc9988d0f611185984c8403a7c321768e09c817f81a6dc55a7c076dbdfccc'
		ct:      '86ffffffffffffffffffffffffffffffedbfcd3f889324dfaeb340cffcd53502981b5e44b2f390a2e219411d38fa53c3981b5e44b2f390a2e219411d38fa53c3'
		tag:     'a084fcb71338faabb02bb26c1b7c1a55'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    204
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     '0adf302dcb7dc4ea184e7fe455afac174162bce2fd47b3594ef404de7191f6f286df6823c0030e4c987ef266c4a26f1d776e5ca8708b77eef3cb4a6b5a87d50c449264f8f28a963bf61e76272e736412361cce9da29f7fdf11f22465b6c11ad1'
		ct:      '46ffffffffffffffffffffffffffffffd90e7037419d858c864976339c843902f1ffffffffffffffffffffffffffffff99fb0b24d580fd21d48877710fc07a03f1ffffffffffffffffffffffffffffff99fb0b24d580fd21d48877710fc07a03'
		tag:     '8f7f47eb00819694ca25bd8a5cd263cd'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    205
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     '14df302dcb7dc4ea184e7fe455afac17b326bb0d3f07ed0375e02c41e3162ff1f5187d3ad276581fb80055b5ab3a07c26cadbd6a48812363f8c26536c520382f'
		ct:      '58ffffffffffffffffffffffffffffff2b4a77d883dddbd6bd5d5eac0e03e0018238eae6ed8aa9acdf81582c906797208238eae6ed8aa9acdf81582c90679720'
		tag:     '6e29bbb07bacd01dde68f7ceba8dcf6c'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    206
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     '26df302dcb7dc4ea184e7fe455afac1774a785d5558e3814492622e81e8334f018269cb3ac4b09e4f30a18a38601afbd81935ce336bc7298b3c82820e81b9050'
		ct:      '6affffffffffffffffffffffffffffffeccb4900e9540ec1819b5005f396fb006f060b6f93b7f857948b153abd5c3f5f6f060b6f93b7f857948b153abd5c3f5f'
		tag:     'dc8de3adc9cf0095ab93f73b92e38f8e'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    207
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'c2df302dcb7dc4ea184e7fe455afac171b2de59c9d989d18693805cc529ef7f3293410a473f31e55f38cdf2621490502b081d0f4e9046529b34eefa54f533aef'
		ct:      '8effffffffffffffffffffffffffffff834129492142abcda1857721bf8b38035e1487784c0fefe6940dd2bf1a1495e05e1487784c0fefe6940dd2bf1a1495e0'
		tag:     '41da20c0d2480aabf6ec50678325ca55'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    208
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     '02df302dcb7dc4ea184e7fe455afac17f7fc079566913aaeb1a6df68b32ff3f38ddf6823c0030e4c987ef266c4a26f1d46edbe93aa3a33c96e1639a5b0b8ea0f4f9264f8f28a963bf61e76272e736412079f2ca6782e3bf88c2f57ab5cfe25d2'
		ct:      '4effffffffffffffffffffffffffffff6f90cb40da4b0c7b791bad855e3a3c03faffffffffffffffffffffffffffffffa878e91f0f31b906495504bfe5ff4500faffffffffffffffffffffffffffffffa878e91f0f31b906495504bfe5ff4500'
		tag:     '30d56bf3bf91a69310e75b85c47b13ce'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    209
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'b4061d1155ecf464e71b76b58f7001fdc393332a4325c92a37428d1212ea300fc8d4b0692d780bf3bd8b6ad483a9a6e0b56aa8735af47530d8bcc2e5aab850f00a99bcb21ff19384d3ebee956978adef'
		ct:      'f826d2c3616ecf7100aaf6ae252052155bffffffffffffffffffffffffffffffbff427b51284fa40da0a674db8f436025bffffffffffffffffffffffffffffffbff427b51284fa40da0a674db8f43602'
		tag:     'cdfe4b827e5558a497f309fb493d209d'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    210
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     '79fff2a7076a9cdbe3e98b10b2de26310893332a4325c92a37428d1212ea300f2d2d2a92a3d127ed8ded071cbf2d71e37e6aa8735af47530d8bcc2e5aab850f0ef6026499158bf9ae38d835d55fc7aec'
		ct:      '35df3d7533e8a7ce04580b0b188e75d990ffffffffffffffffffffffffffffff5a0dbd4e9c2dd65eea6c0a858470e10190ffffffffffffffffffffffffffffff5a0dbd4e9c2dd65eea6c0a858470e101'
		tag:     'd168917ea5f3a1aaf11f260b1ad87ba6'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    211
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'b3df302dcb7dc4ea184e7fe455afac1788d961aaa04f7a3b682f69a70583605285df6823c0030e4c987ef266c4a26f1dd6acb067c1cfa95c474abc0784f6440d479264f8f28a963bf61e76272e73641297de225213dba16da573d20968b08bd0'
		ct:      'ffffffffffffffffffffffffffffffff10b5ad7f1c954ceea0921b4ae896afa2f2ffffffffffffffffffffffffffffff3839e7eb64c423936009811dd1b1eb02f2ffffffffffffffffffffffffffffff3839e7eb64c423936009811dd1b1eb02'
		tag:     '7c9d1f445000923ba0f6712dcef103f8'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    212
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     '77df302dcb7dc4ea184e7fe455afac17fe07b21e1574641d2f3bf1f112533ff0eddf6823c0030e4c987ef266c4a26f1d01519c6f96c6673d21e6e5cad204f00d2f9264f8f28a963bf61e76272e73641240230e5a44d26f0cc3df8bc43e423fd0'
		ct:      '3bffffffffffffffffffffffffffffff666b7ecba9ae52c8e786831cff46f0009affffffffffffffffffffffffffffffefc4cbe333cdedf206a5d8d087435f029affffffffffffffffffffffffffffffefc4cbe333cdedf206a5d8d087435f02'
		tag:     '0eff35568cd1f12e9c87c10f7ac886d9'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    213
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'b3df302dcb7dc4ea184e7fe455afac170495c7a18e950fff87e6de1fb6247d632cdf6823c0030e4c987ef266c4a26f1dbd83680da360ceb5adec202a9a7b9e0cee9264f8f28a963bf61e76272e736412fcf1fa387174c6844fd54e24763d51d1'
		ct:      'ffffffffffffffffffffffffffffffff9cf90b74324f392a4f5bacf25b31b2935bffffffffffffffffffffffffffffff53163f81066b447a8aaf1d30cf3c31035bffffffffffffffffffffffffffffff53163f81066b447a8aaf1d30cf3c3103'
		tag:     '972e0ccb273da0e432560bb025a3dafd'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    214
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'b3df302dcb7dc4ea184e7fe455afac170295c7a18e950fff87e6de1fb6247d6388df6823c0030e4c987ef266c4a26f1dbd7d7050390dc18a9374c53a6e25993a4a9264f8f28a963bf61e76272e736412fc0fe265eb19c9bb714dab34826356e7'
		ct:      'ffffffffffffffffffffffffffffffff9af90b74324f392a4f5bacf25b31b293ffffffffffffffffffffffffffffffff53e827dc9c064b45b437f8203b623635ffffffffffffffffffffffffffffffff53e827dc9c064b45b437f8203b623635'
		tag:     '3851af020e67d939221af94f2379d688'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    215
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'dcdf302dcb7dc4ea184e7fe455afac17cb0c79e00a6a36ff33be642b05f84df2d8df6823c0030e4c987ef266c4a26f1dc738f395f5468655095afa9f9134cb0d1a9264f8f28a963bf61e76272e736412864a61a027528e64eb6394917d7204d0'
		ct:      '90ffffffffffffffffffffffffffffff5360b535b6b0002afb0316c6e8ed8202afffffffffffffffffffffffffffffff29ada419504d0c9a2e19c785c4736402afffffffffffffffffffffffffffffff29ada419504d0c9a2e19c785c4736402'
		tag:     '41c5ffe07dbb3c988a9e3687296007bd'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    216
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     '70d7ba38c2570c77ca364185a1184c5f6793332a4325c92a37428d1212ea300fa4fb7ba93c8f1aeb596ee85db59503be116aa8735af47530d8bcc2e5aab850f066b677720e06829c370e6c1c5f4408b1'
		ct:      '3cf775eaf6d537622d87c19e0b481fb7ffffffffffffffffffffffffffffffffd3dbec750373eb583eefe5c48ec8935cffffffffffffffffffffffffffffffffd3dbec750373eb583eefe5c48ec8935c'
		tag:     'a0dcb67a9f930f9d79381e04c38c91a1'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    217
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'b3df302dcb7dc4ea184e7fe455afac176ef295891c6dd4bc8cc077c62e41f9ab88df6823c0030e4c987ef266c4a26f1da826a752304368e3b429f83cd53af9ce4a9264f8f28a963bf61e76272e736412e9543567e25760d256109632397c3613'
		ct:      'fffffffffffffffffffffffffffffffff69e595ca0b7e269447d052bc354365bffffffffffffffffffffffffffffffff46b3f0de9548e22c936ac526807d56c1ffffffffffffffffffffffffffffffff46b3f0de9548e22c936ac526807d56c1'
		tag:     'a12b34e8828deb913809858245813ac4'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    218
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'd028532613e6ac2299f545fa399347d35a56485c99d079eb0214cff4e9a45b15b51a13551af6be8dad28b0803fec0407'
		ct:      '9c089cf4276497377e44c5e193c3143bc23a8489250a4f3ecaa9bd1904b194e5c23a8489250a4f3ecaa9bd1904b194e5'
		tag:     '3e5a4dd56980cdc49b3fc7f1a4e5de80'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    219
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'd4df302dcb7dc4ea184e7fe455afac17c0441010cd4c556db2422415f1ac35f25af02df6e257058ff8b02f629b6335e1c345eda678a07ef3b8721fe1f5790a0c'
		ct:      '98ffffffffffffffffffffffffffffff5828dcc5719663b87aff56f81cb9fa022dd0ba2addabf43c9f3122fba03ea5032dd0ba2addabf43c9f3122fba03ea503'
		tag:     '693df6c4750d80c9c6db9b8290908856'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    220
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'b3df302dcb7dc4ea184e7fe455afac171c62379820fccbe5ee09c766a678900da0df6823c0030e4c987ef266c4a26f1d10d9fb4ebf330d167b33ce570a3bef0c629264f8f28a963bf61e76272e73641251ab697b6d270527990aa059e67d20d1'
		ct:      'ffffffffffffffffffffffffffffffff840efb4d9c26fd3026b4b58b4b6d5ffdd7fffffffffffffffffffffffffffffffe4cacc21a3887d95c70f34d5f7c4003d7fffffffffffffffffffffffffffffffe4cacc21a3887d95c70f34d5f7c4003'
		tag:     '6c551dccfa2d965912e3c94d908fb1b1'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    221
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     '0adf302dcb7dc4ea184e7fe455afac17f2a46865a2bc9f3931a21ebec77a0bf282df6823c0030e4c987ef266c4a26f1df3fdca1d1d24c0a818c33b641cced60c409264f8f28a963bf61e76272e736412b28f5828cf30c899fafa556af08819d1'
		ct:      '46ffffffffffffffffffffffffffffff6ac8a4b01e66a9ecf91f6c532a6fc402f5ffffffffffffffffffffffffffffff1d689d91b82f4a673f80067e49897903f5ffffffffffffffffffffffffffffff1d689d91b82f4a673f80067e49897903'
		tag:     '041184e17ae48184f2f3f675a0c1d0bf'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    222
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     '15df302dcb7dc4ea184e7fe455afac177ba4dff6c26c959df060bfbcace3f2f236df6823c0030e4c987ef266c4a26f1dc5a90df77ff0db40fc59c0318f88cb0cf49264f8f28a963bf61e76272e73641284db9fc2ade4d3711e60ae3f63ce04d1'
		ct:      '59ffffffffffffffffffffffffffffffe3c813237eb6a34838ddcd5141f63d0241ffffffffffffffffffffffffffffff2b3c5a7bdafb518fdb1afd2bdacf640341ffffffffffffffffffffffffffffff2b3c5a7bdafb518fdb1afd2bdacf6403'
		tag:     '3932ed921c20c30f251e2495cafcee7d'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    223
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     'b3df302dcb7dc4ea184e7fe455afac17be99ee99ce9825877e959642d2f3f02688df6823c0030e4c987ef266c4a26f1d16c6f0928b580a43c7ffc494a6818c704a9264f8f28a963bf61e76272e73641257b462a7594c027225c6aa9a4ac743ad'
		ct:      'ffffffffffffffffffffffffffffffff26f5224c72421352b628e4af3fe63fd6fffffffffffffffffffffffffffffffff853a71e2e53808ce0bcf98ef3c6237ffffffffffffffffffffffffffffffffff853a71e2e53808ce0bcf98ef3c6237f'
		tag:     '1fe14698bc0d7e1a5d622f9f4ec97681'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    224
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130bc672c3'
		aad:     'ffffffff'
		msg:     '1adf302dcb7dc4ea184e7fe455afac17375870a70cf967d44c072c976f960cf202df6823c0030e4c987ef266c4a26f1d90342adb10594c487a08ef575ddc120fc09264f8f28a963bf61e76272e736412d146b8eec24d447998318159b19addd2'
		ct:      '56ffffffffffffffffffffffffffffffaf34bc72b023510184ba5e7a8283c30275ffffffffffffffffffffffffffffff7ea17d57b552c6875d4bd24d089bbd0075ffffffffffffffffffffffffffffff7ea17d57b552c6875d4bd24d089bbd00'
		tag:     '48a52de01229d381e42274737fceeebe'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    225
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'e15491e17fb1ebf66bb0a3ecbc1bc251544d1a6c930b659ca3903632f84c51b676fb4365776d1717fb3f9f45f9888c9764482cfbc35450c0a2a1c8aab04f5ac644871d72e9f20f1fd6a77429527f8b81df97e388864bad33006e671b757b158b'
		ct:      'e2ffffffffffffffffffffffffffffff2e36b18f2fc925af8ca7b8922f3d0402ffffffffffffffffffffffffffffffff730856e81fee6ffa095f96faa3c87deeffffffffffffffffffffffffffffffff730856e81fee6ffa095f96faa3c87dee'
		tag:     '457c13b040b790624a47b6d232c96c2e'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    226
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'c55491e17fb1ebf66bb0a3ecbc1bc25151b278ab6424829315f4b083322f0db37ffb4365776d1717fb3f9f45f9888c97f3f689761bc8de9b4c93af122a8f78224d871d72e9f20f1fd6a77429527f8b81482946055ed72368ee5c00a3efbb376f'
		ct:      'c6ffffffffffffffffffffffffffffff2bc9d348d8e6c2a03ac33e23e55e5807f6ffffffffffffffffffffffffffffffe4b6f365c772e1a1e76df14239085f0af6ffffffffffffffffffffffffffffffe4b6f365c772e1a1e76df14239085f0a'
		tag:     '4c21dabf80184666ee0752d61aee183a'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    227
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'fc5491e17fb1ebf66bb0a3ecbc1bc2510c5890208ce83ac784320ba346f951dc879bb667caf70858b7f01163af0ef3d019df70ee9edfdf8a18ce2f89bafea790'
		ct:      'ffffffffffffffffffffffffffffffff76233bc3302a7af4ab058503918804680e9f0afd4265e0b0b33071d9a97980b80e9f0afd4265e0b0b33071d9a97980b8'
		tag:     'b41fc59d364e1050c2076dfe596ba799'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    228
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'd65491e17fb1ebf66bb0a3ecbc1bc251ceae77b54a415c7267e60dd49c6196be74fb4365776d1717fb3f9f45f9888c97715e4c777914ba04fa45c4c0a4133a2a46871d72e9f20f1fd6a77429527f8b81ca8183043c0b47f7588a6b7161277567'
		ct:      'd5ffffffffffffffffffffffffffffffb4d5dc56f6831c4148d183744b10c30afdffffffffffffffffffffffffffffff661e3664a5ae853e51bb9a90b7941d02fdffffffffffffffffffffffffffffff661e3664a5ae853e51bb9a90b7941d02'
		tag:     '8beed54dc183a687b2cdcbf6423a26a3'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    229
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'e65491e17fb1ebf66bb0a3ecbc1bc2514c46b4b6687d80ee9e29a3621eedd4b676fb4365776d1717fb3f9f45f9888c971cff23a21f6a8bd7453510a0ae5e1f1c44871d72e9f20f1fd6a77429527f8b81a720ecd15a757624e7fabf116b6a5051'
		ct:      'e5ffffffffffffffffffffffffffffff363d1f55d4bfc0ddb11e2dc2c99c8102ffffffffffffffffffffffffffffffff0bbf59b1c3d0b4edeecb4ef0bdd93834ffffffffffffffffffffffffffffffff0bbf59b1c3d0b4edeecb4ef0bdd93834'
		tag:     'cabbd1db01ef06a9769345176b86330c'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    230
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'ca5491e17fb1ebf66bb0a3ecbc1bc25149abeaedc11f1dfc700d3df2a8cf80b36ffb4365776d1717fb3f9f45f9888c97fbfd1b38b0d343eaa71b3b22c0e0fb225d871d72e9f20f1fd6a77429527f8b814022d44bf5ccbe1905d4949305d4b46f'
		ct:      'c9ffffffffffffffffffffffffffffff33d0410e7ddd5dcf5f3ab3527fbed507e6ffffffffffffffffffffffffffffffecbd612b6c697cd00ce56572d367dc0ae6ffffffffffffffffffffffffffffffecbd612b6c697cd00ce56572d367dc0a'
		tag:     'd16098eb4150bcac1a54e11a53abdf17'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    231
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'bc5491e17fb1ebf66bb0a3ecbc1bc25122253ac53ee2d495cd59c3cf874bdfb378fb4365776d1717fb3f9f45f9888c97f85d6026b6bdd3ade44614db13a4d22e4a871d72e9f20f1fd6a77429527f8b814382af55f3a22e5e4689bb6ad6909d63'
		ct:      'bfffffffffffffffffffffffffffffff585e9126822094a6e26e4d6f503a8a07f1ffffffffffffffffffffffffffffffef1d1a356a07ec974fb84a8b0023f506f1ffffffffffffffffffffffffffffffef1d1a356a07ec974fb84a8b0023f506'
		tag:     '6005e4ae07fba16ee6e7cfd2ee645c26'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    232
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     '8686c6910ecd2fef887e60e38f44794013658b8d8a4428cc894d48296a78289fe01a9cf4be148017a2baa633bb7e0e43'
		ct:      '852da88f8e833be61c313cf0cca044ee691e206e368668ffa67ac689bd097d2b691e206e368668ffa67ac689bd097d2b'
		tag:     '91d0ac28cd0f09a8261194b8df0abc0d'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    233
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'fc5491e17fb1ebf66bb0a3ecbc1bc25176cab059fb79ced851f140731235a58a23285769d164044f8231da13d8421fe7bd6c91e0854cd39d2d0fe4f9cdb24ba7'
		ct:      'ffffffffffffffffffffffffffffffff0cb11bba47bb8eeb7ec6ced3c544f03eaa2cebf359f6eca786f1baa9de356c8faa2cebf359f6eca786f1baa9de356c8f'
		tag:     '9d652db06f7b08621f02c7f294abbe58'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    234
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'fc5491e17fb1ebf66bb0a3ecbc1bc2519418c44ef4ee0a2eecf688092ee3893805db827dd2f54831ce2c92c414f32bb59b9f44f486dd9fe36112ac2e01037ff5'
		ct:      'ffffffffffffffffffffffffffffffffee636fad482c4a1dc3c106a9f992dc8c8cdf3ee75a67a0d9caecf27e128458dd8cdf3ee75a67a0d9caecf27e128458dd'
		tag:     'b5893d2129f0408a5480c7fc3fc9de5d'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    235
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'c45491e17fb1ebf66bb0a3ecbc1bc25198183353086ce0c47d429f682ef227bc70fb4365776d1717fb3f9f45f9888c97d9e04fda5db617d05384a3871e4be42f42871d72e9f20f1fd6a77429527f8b81623f80a918a9ea23f14b0c36db7fab62'
		ct:      'c7ffffffffffffffffffffffffffffffe26398b0b4aea0f7527511c8f9837208f9ffffffffffffffffffffffffffffffcea035c9810c28eaf87afdd70dccc307f9ffffffffffffffffffffffffffffffcea035c9810c28eaf87afdd70dccc307'
		tag:     '0b88cfa42284726dad2be1de8a057626'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    236
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'fc5491e17fb1ebf66bb0a3ecbc1bc251f4ab27dfe421f982006cf28959f81f8da248e0ece28cc88632460845a00db5e13c0c2665b6a41f549d7836afb5fde1a1'
		ct:      'ffffffffffffffffffffffffffffffff8ed08c3c58e3b9b12f5b7c298e894a392b4c5c766a1e206e368668ffa67ac6892b4c5c766a1e206e368668ffa67ac689'
		tag:     '5e97665318961c4c95e928fc11140063'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    237
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'fc5491e17fb1ebf66bb0a3ecbc1bc2513d04e3a75f48cd89944fe3aeb6fa3a376dfeabe47e571b9ec763395e7e0b98bbf3ba6d6d2a7fcc4c685d07b46bfbccfb'
		ct:      'ffffffffffffffffffffffffffffffff477f4844e38a8dbabb786d0e618b6f83e4fa177ef6c5f376c3a359e4787cebd3e4fa177ef6c5f376c3a359e4787cebd3'
		tag:     'ff71ef44bc734132701144f73497756b'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    238
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'c75491e17fb1ebf66bb0a3ecbc1bc251ea035faf17204b15b07af9577ad41ab57dfb4365776d1717fb3f9f45f9888c977c945978f412121f3e3061486ce872284f871d72e9f20f1fd6a77429527f8b81c74b960bb10defec9cffcef9a9dc3d65'
		ct:      'c4ffffffffffffffffffffffffffffff9078f44cabe20b269f4d77f7ada54f01f4ffffffffffffffffffffffffffffff6bd4236b28a82d2595ce3f187f6f5500f4ffffffffffffffffffffffffffffff6bd4236b28a82d2595ce3f187f6f5500'
		tag:     '2ab3cabef2de4268a5313cd59dcd193e'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    239
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'db5491e17fb1ebf66bb0a3ecbc1bc251b8328dab56edd30a61fa13c9309ca7b656fb4365776d1717fb3f9f45f9888c97a34a4557084bc0e73cf64dd52006892b64871d72e9f20f1fd6a77429527f8b8118958a244d543d149e39e264e532c666'
		ct:      'd8ffffffffffffffffffffffffffffffc2492648ea2f93394ecd9d69e7edf202dfffffffffffffffffffffffffffffffb40a3f44d4f1ffdd970813853381ae03dfffffffffffffffffffffffffffffffb40a3f44d4f1ffdd970813853381ae03'
		tag:     '32bcb167c875045b9651816e4bedf51a'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    240
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'c45491e17fb1ebf66bb0a3ecbc1bc2519df3bd6867975c3fe85b5ac979cd8cbd77fb4365776d1717fb3f9f45f9888c979a108f9b640c85feaa90eda0ca3b552245871d72e9f20f1fd6a77429527f8b8121cf40e82113780d085f42110f0f1a6f'
		ct:      'c7ffffffffffffffffffffffffffffffe788168bdb551c0cc76cd469aebcd909feffffffffffffffffffffffffffffff8d50f588b8b6bac4016eb3f0d9bc720afeffffffffffffffffffffffffffffff8d50f588b8b6bac4016eb3f0d9bc720a'
		tag:     '2ec4ac70d29ad094e46ce355eb94d1ea'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    241
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'e15491e17fb1ebf66bb0a3ecbc1bc251554d1a6c930b659ca3903632f84c51b676fb4365776d1717fb3f9f45f9888c976a196530d71b9fdcce0639e14bb7952d44871d72e9f20f1fd6a77429527f8b81d1c6aa439204622f6cc996508e83da60'
		ct:      'e2ffffffffffffffffffffffffffffff2f36b18f2fc925af8ca7b8922f3d0402ffffffffffffffffffffffffffffffff7d591f230ba1a0e665f867b15830b205ffffffffffffffffffffffffffffffff7d591f230ba1a0e665f867b15830b205'
		tag:     '3c2b4a7555046076eeade41b7e613817'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    242
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'e15491e17fb1ebf66bb0a3ecbc1bc251574d1a6c930b659ca3903632f84c51b646fb4365776d1717fb3f9f45f9888c97a9e8b1b1ebcd7efac51aa6a07ccbb72f74871d72e9f20f1fd6a77429527f8b8112377ec2aed2830967d50911b9fff862'
		ct:      'e2ffffffffffffffffffffffffffffff2d36b18f2fc925af8ca7b8922f3d0402cfffffffffffffffffffffffffffffffbea8cba2377741c06ee4f8f06f4c9007cfffffffffffffffffffffffffffffffbea8cba2377741c06ee4f8f06f4c9007'
		tag:     '4ecddcea2b6ac14ea6e08789e730a145'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    243
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'fc5491e17fb1ebf66bb0a3ecbc1bc25183a4bdec687df1f2c2ad89017dc3882773fb4365776d1717fb3f9f45f9888c97a3bf9d6304f938afc62286de1edf752a41871d72e9f20f1fd6a77429527f8b811860521041e6c55c64ed296fdbeb3a67'
		ct:      'fffffffffffffffffffffffffffffffff9df160fd4bfb1c1ed9a07a1aab2dd93faffffffffffffffffffffffffffffffb4ffe770d84307956ddcd88e0d585202faffffffffffffffffffffffffffffffb4ffe770d84307956ddcd88e0d585202'
		tag:     'c21949bfae429c58105372c714964e39'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    244
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     '5160a0e22d2c72c25ca00ff76844f5219984541c433dbfccd0c8715f288eaa4b84bee9e212f601afdf23e0616fc61b6bf4bf85ec2345c0c55401a1afec78d8d7b6c2b7f58c6919a7f2bb0b0dc4311c7d'
		ct:      '52cbcefcad6266cbc8ef53e42ba0c88fe3ffffffffffffffffffffffffffffff0dba55789a64e947dbe380db69b16803e3ffffffffffffffffffffffffffffff0dba55789a64e947dbe380db69b16803'
		tag:     '31827cfd0227dae6020564fe6677828e'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    245
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'aa9b0cff80d2504071b708a267795adc9a84541c433dbfccd0c8715f288eaa4b1278f017b369022628c50b0e914a786df7bf85ec2345c0c55401a1afec78d8d72004ae002df61a2e055de0623abd7f7b'
		ct:      'a93062e1009c4449e5f854b1249d6772e0ffffffffffffffffffffffffffffff9b7c4c8d3bfbeace2c056bb4973d0b05e0ffffffffffffffffffffffffffffff9b7c4c8d3bfbeace2c056bb4973d0b05'
		tag:     '0f146dfc74edd6e8b92e4efde9568392'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    246
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'd65491e17fb1ebf66bb0a3ecbc1bc2513544a1ff7d10d3f8228b7397b54ab7b34afb4365776d1717fb3f9f45f9888c978f4f155538a2b7fa71deaf145ed8782278871d72e9f20f1fd6a77429527f8b813490da267dbd4a09d31100a59bec376f'
		ct:      'd5ffffffffffffffffffffffffffffff4f3f0a1cc1d293cb0dbcfd37623be207c3ffffffffffffffffffffffffffffff980f6f46e41888c0da20f1444d5f5f0ac3ffffffffffffffffffffffffffffff980f6f46e41888c0da20f1444d5f5f0a'
		tag:     '8fcacbc3d070336717f838d754f7033d'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    247
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'fc5491e17fb1ebf66bb0a3ecbc1bc25127c97067599ad88acd1d636b71347168702917247f01169dee95b91bb941d344ee6dd1ad2b29c14f41ab87f1acb18704'
		ct:      'ffffffffffffffffffffffffffffffff5db2db84e55898b9e22aedcba64524dcf92dabbef793fe75ea55d9a1bf36a02cf92dabbef793fe75ea55d9a1bf36a02c'
		tag:     'ddd96272c682dd45ffb580f4db058e79'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    248
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'fc5491e17fb1ebf66bb0a3ecbc1bc251319e8e2a169df9a16b932cf0eae456437bfb4365776d1717fb3f9f45f9888c97c943391482e56fb6754746325960302049871d72e9f20f1fd6a77429527f8b81729cf667c7fa9245d788e9839c547f6d'
		ct:      'ffffffffffffffffffffffffffffffff4be525c9aa5fb99244a4a2503d9503f7f2ffffffffffffffffffffffffffffffde0343075e5f508cdeb918624ae71708f2ffffffffffffffffffffffffffffffde0343075e5f508cdeb918624ae71708'
		tag:     '2243326100d105501684abc0aa1010f4'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    249
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'fe5491e17fb1ebf66bb0a3ecbc1bc2514233fe057ace7ef2ad7b40a5b53109bc417d2b9ad117d19e4636e26eac900c29df39ed13853f064ce908dc84b9605869'
		ct:      'fdffffffffffffffffffffffffffffff384855e6c60c3ec1824cce0562405c08c87997005985397642f682d4aae77f41c87997005985397642f682d4aae77f41'
		tag:     '8a411d3fb606d039ea83c6e8e98d9e53'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    250
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'dc5491e17fb1ebf66bb0a3ecbc1bc2514402b2a59ff82793e8c9de3fc366c3b357fb4365776d1717fb3f9f45f9888c9756054f75d716ae500d8b97befddc012065871d72e9f20f1fd6a77429527f8b81edda8006920953a3af44380f38e84e6d'
		ct:      'dfffffffffffffffffffffffffffffff3e791946233a67a0c7fe509f14179607deffffffffffffffffffffffffffffff414535660bac916aa675c9eeee5b2608deffffffffffffffffffffffffffffff414535660bac916aa675c9eeee5b2608'
		tag:     '5ed9e3c1cacff152211b5c57fd628278'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    251
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     'c75491e17fb1ebf66bb0a3ecbc1bc2514e1cde0c37340ebdfe6eb58c35626bb54bfb4365776d1717fb3f9f45f9888c972073eb1ca8d246621b2eed2bafdfe92979871d72e9f20f1fd6a77429527f8b819bac246fedcdbb91b9e1429a6aeba664'
		ct:      'c4ffffffffffffffffffffffffffffff346775ef8bf64e8ed1593b2ce2133e01c2ffffffffffffffffffffffffffffff3733910f74687958b0d0b37bbc58ce01c2ffffffffffffffffffffffffffffff3733910f74687958b0d0b37bbc58ce01'
		tag:     '657eaad10a31a756c5dbf75ae5872e84'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    252
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f101112130552a411'
		aad:     'ffffffff'
		msg:     '55cc2b7c86aa8ef3dc2545e3e79c9afa402316a313248720fc842d2942908a25b35c01da27742ffbd773c3339396acf9'
		ct:      '5667456206e49afa486a19f0a478a7543a58bd40afe6c713d3b3a38995e1df913a58bd40afe6c713d3b3a38995e1df91'
		tag:     '25545d0636b19f9319ff5bb0191c89fb'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    253
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '690ed780cbde3c9617205cba5e51ca35b6361e9d88e9f262299d7730839f48db3a32d61828f3f54056c1f9113f9b8e180d52a0ddb24c9b3018d53415e705c35e1fa08298547a71b0467c59e48a75cea42a00d0f01b609d7723ec2d39056428f1'
		ct:      'deffffffffffffffffffffffffffffff0846af843d1c80165bbd914582a77702deffffffffffffffffffffffffffffffd7010c1eb016839cbd95cd5eaad61a03deffffffffffffffffffffffffffffffd7010c1eb016839cbd95cd5eaad61a03'
		tag:     '92a14f8e928d42edaab17206102c8123'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    254
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '750ed780cbde3c9617205cba5e51ca351ab3232ebce6fcd6e34569079ff4a9de2632d61828f3f54056c1f9113f9b8e187b1d570cd9c860b7c98731c357edd95d03a08298547a71b0467c59e48a75cea45c4f272170e466f0f2be28efb58c32f2'
		ct:      'c2ffffffffffffffffffffffffffffffa4c3923709138ea291658f729ecc9607c2ffffffffffffffffffffffffffffffa14efbcfdb92781b6cc7c8881a3e0000c2ffffffffffffffffffffffffffffffa14efbcfdb92781b6cc7c8881a3e0000'
		tag:     '64d2438f32085bfa32287509bce9ba2b'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    255
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '660ed780cbde3c9617205cba5e51ca355ff14d9062edd3279f18abddb87051de2832d61828f3f54056c1f9113f9b8e186a6ef5e04849bc75ea51708aaca4c8550da08298547a71b0467c59e48a75cea44d3c85cde165ba32d16869a64ec523fa'
		ct:      'd1ffffffffffffffffffffffffffffffe181fc89d718a153ed384da8b9486e07ccffffffffffffffffffffffffffffffb03d59234a13a4d94f1189c1e1771108ccffffffffffffffffffffffffffffffb03d59234a13a4d94f1189c1e1771108'
		tag:     'ac951493b2f2bc2e53027f156ce0a14c'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    256
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '720ed780cbde3c9617205cba5e51ca3556fce497646fdbf7851de07d441414dd5f32d61828f3f54056c1f9113f9b8e186d1b1ee446cfb4a5ff56bb6a208817597aa08298547a71b0467c59e48a75cea44a496ec9efe3b2e2c46fa246c2e9fcf6'
		ct:      'c5ffffffffffffffffffffffffffffffe88c558ed19aa983f73d0608452c2b04bbffffffffffffffffffffffffffffffb748b2274495ac095a1642216d5bce04bbffffffffffffffffffffffffffffffb748b2274495ac095a1642216d5bce04'
		tag:     'c689d997d2573770fb528b24c894c275'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    257
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '7f0ed780cbde3c9617205cba5e51ca359226a9fc2cd7b7102f369be8eab380d95132d61828f3f54056c1f9113f9b8e182142d9bd0e47d0461aae41fd5e68bb5c74a08298547a71b0467c59e48a75cea40610a990a76bd601219758d1bc0950f3'
		ct:      'c8ffffffffffffffffffffffffffffff2c5618e59922c5645d167d9deb8bbf00b5fffffffffffffffffffffffffffffffb11757e0c1dc8eabfeeb8b613bb6201b5fffffffffffffffffffffffffffffffb11757e0c1dc8eabfeeb8b613bb6201'
		tag:     '28416fa072a713e6c37da13fd43fcabf'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    258
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '1b0ed780cbde3c9617205cba5e51ca3576a34a8ed0eca184e19e9cbf0689e1dc1b32d61828f3f54056c1f9113f9b8e18ceb8b40c75e7e49d9ca641e6b75c9fea3ea08298547a71b0467c59e48a75cea4e9eac421dccbe2daa79f58ca553d7445'
		ct:      'acffffffffffffffffffffffffffffffc8d3fb976519d3f093be7aca07b1de05ffffffffffffffffffffffffffffffff14eb18cf77bdfc3139e6b8adfa8f46b7ffffffffffffffffffffffffffffffff14eb18cf77bdfc3139e6b8adfa8f46b7'
		tag:     'fa7163a112222cf34bf4a34280fd03c8'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    259
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '430ed780cbde3c9617205cba5e51ca35899db7a761de4105d249e53dc29d54de1132d61828f3f54056c1f9113f9b8e183528b78e77af595ac787df1f2e8dd55834a08298547a71b0467c59e48a75cea4127ac7a3de835f1dfcbec633ccec3ef7'
		ct:      'f4ffffffffffffffffffffffffffffff37ed06bed42b3371a0690348c3a56b07f5ffffffffffffffffffffffffffffffef7b1b4d75f541f662c72654635e0c05f5ffffffffffffffffffffffffffffffef7b1b4d75f541f662c72654635e0c05'
		tag:     'd79da397c4431ee1c2b58810ece4491e'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    260
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '6e0ed780cbde3c9617205cba5e51ca35bf4a4776b55d5a843cbb180432356edd3d32d61828f3f54056c1f9113f9b8e18639ba63ca22b2fd9b4b9d8359e16285c18a08298547a71b0467c59e48a75cea444c9d6110b07299e8f80c1197c77c3f3'
		ct:      'd9ffffffffffffffffffffffffffffff013af66f00a828f04e9bfe71330d5104d9ffffffffffffffffffffffffffffffb9c80affa071377511f9217ed3c5f101d9ffffffffffffffffffffffffffffffb9c80affa071377511f9217ed3c5f101'
		tag:     'a9ce979864be36ee4a2c8b1398a28326'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    261
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '1a0ed780cbde3c9617205cba5e51ca356e9e850317ce94dbf35d481f264260d85332d61828f3f54056c1f9113f9b8e18fe86ef8131ce6495df632dd83246635e76a08298547a71b0467c59e48a75cea4d9d49fac98e262d2e45a34f4d02788f1'
		ct:      'adffffffffffffffffffffffffffffffd0ee341aa23be6af817dae6a277a5f01b7ffffffffffffffffffffffffffffff24d5434233947c397a23d4937f95ba03b7ffffffffffffffffffffffffffffff24d5434233947c397a23d4937f95ba03'
		tag:     'd48e13f63365ba66c44270d827d787cf'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    262
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '6a0ed780cbde3c9617205cba5e51ca351b4ed3f20484140402257331eaec0bd81432d61828f3f54056c1f9113f9b8e18a41c8f61637288515b385d363e0f9e5831a08298547a71b0467c59e48a75cea4834eff4cca5e8e166001441adc6e75f7'
		ct:      'ddffffffffffffffffffffffffffffffa53e62ebb171667070059544ebd43401f0ffffffffffffffffffffffffffffff7e4f23a2612890fdfe78a47d73dc4705f0ffffffffffffffffffffffffffffff7e4f23a2612890fdfe78a47d73dc4705'
		tag:     'edaccb5e75b74aed654a70adeb3fc883'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    263
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     'ccf57358a179c8c883031861c610022db8c7092f268753bd3e5593cafe1ead27e27a91d1447e2b76e54b73513f42e319'
		ct:      '7b045b2795580ba16bdcbb2467be37e706b7b836937221c94c7575bfff2692fe06b7b836937221c94c7575bfff2692fe'
		tag:     '6c49f0fd16742ea3fc257e460099469a'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    264
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     'a8daa1c0d4c4a8d72852d7b8138a33cc17ad57d76b0af392d306c8ed4ab55fc44d10cf2909f38b59081828768be911fa'
		ct:      '1f2b89bfe0e56bbec08d74fdb2240606a9dde6cedeff81e6a1262e984b8d601da9dde6cedeff81e6a1262e984b8d601d'
		tag:     '3e7ae4feb6ee46b0849c8049ac5680a2'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    265
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '610ed780cbde3c9617205cba5e51ca35e60d3613f05d8ec15cc42053313284da3232d61828f3f54056c1f9113f9b8e182c9f615e5de52895bf4a2a0943acb55b17a08298547a71b0467c59e48a75cea40bcd1173f4c92ed284733325a1cd5ef4'
		ct:      'd6ffffffffffffffffffffffffffffff587d870a45a8fcb52ee4c626300abb03d6fffffffffffffffffffffffffffffff6cccd9d5fbf30391a0ad3420e7f6c06d6fffffffffffffffffffffffffffffff6cccd9d5fbf30391a0ad3420e7f6c06'
		tag:     'c30d6694ea7011f02164a2035ae67221'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    266
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '1d0ed780cbde3c9617205cba5e51ca353b732c2b99fc7e34f71720381e4187d84f32d61828f3f54056c1f9113f9b8e18978f8d771b2b9f12df626a06ac6e9a5e6aa08298547a71b0467c59e48a75cea4b0ddfd5ab2079955e45b732a4e0f71f1'
		ct:      'aaffffffffffffffffffffffffffffff85039d322c090c408537c64d1f79b801abffffffffffffffffffffffffffffff4ddc21b4197187be7a22934de1bd4303abffffffffffffffffffffffffffffff4ddc21b4197187be7a22934de1bd4303'
		tag:     'c75333a577a5b1e78f28df1cca585f1a'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    267
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '5a0ed780cbde3c9617205cba5e51ca359c6a27bcd3d6c3a2d621763b2270c9de3932d61828f3f54056c1f9113f9b8e18f0fceb5c94551f68cd4411910155b05e1ca08298547a71b0467c59e48a75cea4d7ae9b713d79192ff67d08bde3345bf1'
		ct:      'edffffffffffffffffffffffffffffff221a96a56623b1d6a401904e2348f607ddffffffffffffffffffffffffffffff2aaf479f960f07c46804e8da4c866903ddffffffffffffffffffffffffffffff2aaf479f960f07c46804e8da4c866903'
		tag:     '79d53d3dd5457757caef7dc3cef74cac'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    268
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '320c4dd472095a3de21c2f57e4ca4fcbb1c072a3f1b7cd089e7ba0d8dcf3ebc1eb7dea5d934eb5c3456540431dafa5ff'
		ct:      '85fd65ab462899540ac38c1245647a010fb0c3ba4442bf7cec5b46adddcbd4180fb0c3ba4442bf7cec5b46adddcbd418'
		tag:     'e2714a882d47f25b7373b9e68edc1721'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    269
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '690ed780cbde3c9617205cba5e51ca35b7361e9d88e9f262299d7730839f48db1b32d61828f3f54056c1f9113f9b8e186e9a1b20b92f6767e78d3af5dcc8834a3ea08298547a71b0467c59e48a75cea449c86b0d10036120dcb423d93ea968e5'
		ct:      'deffffffffffffffffffffffffffffff0946af843d1c80165bbd914582a77702ffffffffffffffffffffffffffffffffb4c9b7e3bb757fcb42cdc3be911b5a17ffffffffffffffffffffffffffffffffb4c9b7e3bb757fcb42cdc3be911b5a17'
		tag:     '9f8e29a9e65f1e0e9c322b43fc73d0ff'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    270
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '690ed780cbde3c9617205cba5e51ca35b9361e9d88e9f262299d7730839f48db5e32d61828f3f54056c1f9113f9b8e18aa9f9098867dc8387581274c8a19515a7ba08298547a71b0467c59e48a75cea48dcde0b52f51ce7f4eb83e606878baf5'
		ct:      'deffffffffffffffffffffffffffffff0746af843d1c80165bbd914582a77702baffffffffffffffffffffffffffffff70cc3c5b8427d094d0c1de07c7ca8807baffffffffffffffffffffffffffffff70cc3c5b8427d094d0c1de07c7ca8807'
		tag:     '80b475733ebb66ccb930bac923e43147'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    271
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     'a2d1dde0f5b7e33fcdc15cde5ba7f27b668f4ee64a0a8d8b8ddf198afec7c0266c1d803229363cdc340ec04a0e6c16e702ac533cfda5e7535abf06b4b22c26a2498fd4b255bfb82c24b360bfbb82565b'
		ct:      '1520f59fc1962056251eff9bfa09c7b1d8ffffffffffffffffffffffffffffff88d0a9d5fe3a36639d30c6a4ce086700d8ffffffffffffffffffffffffffffff88d0a9d5fe3a36639d30c6a4ce086700'
		tag:     '6818e1b9360bcb4c10bb8d7b2c5679f1'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    272
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '470ed780cbde3c9617205cba5e51ca3502c4d86c98dc9f7a9ffe71079d3e5bd84b32d61828f3f54056c1f9113f9b8e18d418d5a1df9e2f7e3072e429598d9f586ea08298547a71b0467c59e48a75cea4f34aa58c76b229390b4bfd05bbec74f7'
		ct:      'f0ffffffffffffffffffffffffffffffbcb469752d29ed0eedde97729c066401afffffffffffffffffffffffffffffff0e4b7962ddc437d295321d62145e4605afffffffffffffffffffffffffffffff0e4b7962ddc437d295321d62145e4605'
		tag:     '612c9d78f50e3203f04ec9f36c2ceb36'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    273
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '600ed780cbde3c9617205cba5e51ca35d155d968456cf1cd1a073725c84880da1b32d61828f3f54056c1f9113f9b8e18ed67cc27b816759c429c10bbe85e97e43ea08298547a71b0467c59e48a75cea4ca35bc0a113a73db79a509970a3f7c4b'
		ct:      'd7ffffffffffffffffffffffffffffff6f256871f09983b96827d150c970bf03ffffffffffffffffffffffffffffffff373460e4ba4c6d30e7dce9f0a58d4eb9ffffffffffffffffffffffffffffffff373460e4ba4c6d30e7dce9f0a58d4eb9'
		tag:     'c4ad8c8e3aac88f04c4f33077b9b8f25'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    274
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '0b0ed780cbde3c9617205cba5e51ca358702e63aa9e30b4c65792a0f38e09bd9ac7d109949bcb2db03e3e0201e9bf00192e395bd9ceaa0c80f9d1f85932c58bb'
		ct:      'bcffffffffffffffffffffffffffffff397257231c1679381759cc7a39d8a40048b0397e9eb0b864aadde6cedeff81e648b0397e9eb0b864aadde6cedeff81e6'
		tag:     'b744dd541b16fb820525d29042e77b16'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    275
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     'd5d52278d802c033f52946493353c02755a6d60f5fc86bf68dae79d22b5e6fdc0f1b4ef13d31133d56b09949ea0221e2'
		ct:      '62240a07ec23035a1df6e50c92fdf5edebd66716ea3d1982ff8e9fa72a665005ebd66716ea3d1982ff8e9fa72a665005'
		tag:     'ead346fd87d575c364f6514fcfc6e9a7'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    276
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '5d0ed780cbde3c9617205cba5e51ca35bf4be799f66f059222983e90f7fb85dc0b32d61828f3f54056c1f9113f9b8e180aa51edab4ce62c016d0edb45320845b2ea08298547a71b0467c59e48a75cea42df76ef71de264872de9f498b1416ff4'
		ct:      'eaffffffffffffffffffffffffffffff013b5680439a77e650b8d8e5f6c3ba05efffffffffffffffffffffffffffffffd0f6b219b6947a6cb39014ff1ef35d06efffffffffffffffffffffffffffffffd0f6b219b6947a6cb39014ff1ef35d06'
		tag:     '78ad8a897228c8ab026166f7b37760fa'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    277
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '4e0ed780cbde3c9617205cba5e51ca3580890ecba455f8e3deab706e1378addc1e32d61828f3f54056c1f9113f9b8e18d7e6b0af86c095b1aa242b7f77bcec5b3ba08298547a71b0467c59e48a75cea4f0b4c0822fec93f6911d325395dd07f4'
		ct:      'f9ffffffffffffffffffffffffffffff3ef9bfd211a08a97ac8b961b12409205faffffffffffffffffffffffffffffff0db51c6c849a8d1d0f64d2343a6f3506faffffffffffffffffffffffffffffff0db51c6c849a8d1d0f64d2343a6f3506'
		tag:     'c0705b8df2122ae0223b7003646e471b'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    278
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '710ed780cbde3c9617205cba5e51ca35204cf2b6d791471c5cdaa05e680cdcdf4932d61828f3f54056c1f9113f9b8e1877b5dd4ac6a7af831863d9053d03815f6ca08298547a71b0467c59e48a75cea450e7ad676f8ba9c4235ac029df626af0'
		ct:      'c6ffffffffffffffffffffffffffffff9e3c43af626435682efa462b6934e306adffffffffffffffffffffffffffffffade67189c4fdb72fbd23204e70d05802adffffffffffffffffffffffffffffffade67189c4fdb72fbd23204e70d05802'
		tag:     'd477566543e826f3c7f31248fd4452f6'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    279
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '1c0ed780cbde3c9617205cba5e51ca35d6f983783b155893ae0ba420d8a3f7da1b32d61828f3f54056c1f9113f9b8e18ec6ad8112d9330a25326f566ba64b72b3ea08298547a71b0467c59e48a75cea4cb38a83c84bf36e5681fec4a58055c84'
		ct:      'abffffffffffffffffffffffffffffff688932618ee02ae7dc2b4255d99bc803ffffffffffffffffffffffffffffffff363974d22fc9280ef6660c2df7b76e76ffffffffffffffffffffffffffffffff363974d22fc9280ef6660c2df7b76e76'
		tag:     'a6a84a66e3623f00506a154ba9028cfe'
		result:  'valid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    280
		comment: 'edge case intermediate sums in poly1305'
		key:     '808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f'
		iv:      '000102030405060708090a0b0c0d0e0f10111213019836bb'
		aad:     'ffffffff'
		msg:     '9e6c400d5714f21a9b56fcd3293916c87d8f4ee64a0a8d8b8ddf198afec7c0265d79ed5deee2b7163f72ea0ac66189e319ac533cfda5e7535abf06b4b22c26a278ebb9dd926b33e62fcf4aff738fc95f'
		ct:      '299d68726335317373895f9688972302c3ffffffffffffffffffffffffffffffb9b4c4ba39eebda9964cece40605f804c3ffffffffffffffffffffffffffffffb9b4c4ba39eebda9964cece40605f804'
		tag:     '85b241bbbd0556368ec3fb749e7601ea'
		result:  'valid'
		flags:   ''
	},
]

// Test with invalid results
//
fn test_xchacha20poly1305_aead_from_pycryptodome_with_invalid_result() ! {
	for item in xchacha20poly1305_aead_testdata_with_invalid_results {
		key := hex.decode(item.key)!
		nonce := hex.decode(item.iv)!
		aad := hex.decode(item.aad)!
		msg := hex.decode(item.msg)!
		ct := hex.decode(item.ct)!
		tag := hex.decode(item.tag)!

		// encrypt produces ciphertext plus tag appends into it
		mut encrypted := []u8{}
		encrypted << ct
		encrypted << tag // this is an ivalid tag

		out := chacha20poly1305.encrypt(msg, key, nonce, aad)!
		// produces different result
		assert out != encrypted
		assert out[0..out.len - 16] == ct
		assert out[out.len - 16..] != tag

		mut c := chacha20poly1305.new(key, nonce.len)!
		enc := c.encrypt(msg, nonce, aad)!
		assert enc != encrypted
		assert enc[0..enc.len - c.overhead()] == ct
		assert enc[enc.len - c.overhead()..] != tag

		// from calculated values, it should match
		dec0 := c.decrypt(enc, nonce, aad)!
		assert dec0 == msg

		// this should fail to verify
		_ := c.decrypt(encrypted, nonce, aad) or {
			assert err == error('chacha20poly1305: unmatching tag')
			continue
		}
	}
}

// result == invalid, with reason within the comment field
const xchacha20poly1305_aead_testdata_with_invalid_results = [
	XChaCha20Test{
		tcid:    120
		comment: 'Flipped bit 0 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0418b3e73e52c3be2eaba76807b784e1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    121
		comment: 'Flipped bit 1 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0718b3e73e52c3be2eaba76807b784e1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    122
		comment: 'Flipped bit 7 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '8518b3e73e52c3be2eaba76807b784e1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    123
		comment: 'Flipped bit 8 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0519b3e73e52c3be2eaba76807b784e1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    124
		comment: 'Flipped bit 31 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3673e52c3be2eaba76807b784e1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    125
		comment: 'Flipped bit 32 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3e73f52c3be2eaba76807b784e1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    126
		comment: 'Flipped bit 33 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3e73c52c3be2eaba76807b784e1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    127
		comment: 'Flipped bit 63 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3e73e52c33e2eaba76807b784e1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    128
		comment: 'Flipped bit 64 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3e73e52c3be2faba76807b784e1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    129
		comment: 'Flipped bit 77 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3e73e52c3be2e8ba76807b784e1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    130
		comment: 'Flipped bit 80 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3e73e52c3be2eaba66807b784e1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    131
		comment: 'Flipped bit 96 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3e73e52c3be2eaba76806b784e1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    132
		comment: 'Flipped bit 97 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3e73e52c3be2eaba76805b784e1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    133
		comment: 'Flipped bit 120 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3e73e52c3be2eaba76807b784e0'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    134
		comment: 'Flipped bit 121 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3e73e52c3be2eaba76807b784e3'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    135
		comment: 'Flipped bit 126 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3e73e52c3be2eaba76807b784a1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    136
		comment: 'Flipped bit 127 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3e73e52c3be2eaba76807b78461'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    137
		comment: 'Flipped bit 63 and 127 in tag expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '0518b3e73e52c33e2eaba76807b78461'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    138
		comment: 'Tag changed to all zero expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     '00000000000000000000000000000000'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    139
		comment: 'tag change to all 1 expected tag:0518b3e73e52c3be2eaba76807b784e1'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     ''
		ct:      ''
		tag:     'ffffffffffffffffffffffffffffffff'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    140
		comment: 'Flipped bit 0 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2464a8ce1a360e8352971c8110885031'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    141
		comment: 'Flipped bit 1 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2764a8ce1a360e8352971c8110885031'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    142
		comment: 'Flipped bit 7 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     'a564a8ce1a360e8352971c8110885031'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    143
		comment: 'Flipped bit 8 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2565a8ce1a360e8352971c8110885031'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    144
		comment: 'Flipped bit 31 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a84e1a360e8352971c8110885031'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    145
		comment: 'Flipped bit 32 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a8ce1b360e8352971c8110885031'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    146
		comment: 'Flipped bit 33 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a8ce18360e8352971c8110885031'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    147
		comment: 'Flipped bit 63 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a8ce1a360e0352971c8110885031'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    148
		comment: 'Flipped bit 64 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a8ce1a360e8353971c8110885031'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    149
		comment: 'Flipped bit 77 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a8ce1a360e8352b71c8110885031'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    150
		comment: 'Flipped bit 80 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a8ce1a360e8352971d8110885031'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    151
		comment: 'Flipped bit 96 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a8ce1a360e8352971c8111885031'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    152
		comment: 'Flipped bit 97 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a8ce1a360e8352971c8112885031'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    153
		comment: 'Flipped bit 120 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a8ce1a360e8352971c8110885030'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    154
		comment: 'Flipped bit 121 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a8ce1a360e8352971c8110885033'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    155
		comment: 'Flipped bit 126 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a8ce1a360e8352971c8110885071'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    156
		comment: 'Flipped bit 127 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a8ce1a360e8352971c81108850b1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    157
		comment: 'Flipped bit 63 and 127 in tag expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '2564a8ce1a360e0352971c81108850b1'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    158
		comment: 'Tag changed to all zero expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     '00000000000000000000000000000000'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    159
		comment: 'tag change to all 1 expected tag:2564a8ce1a360e8352971c8110885031'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f'
		ct:      'b70886f2313d015e1fe741365f5e35f1'
		tag:     'ffffffffffffffffffffffffffffffff'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    160
		comment: 'Flipped bit 0 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8161d3df064071df8082a0f813417f05'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    161
		comment: 'Flipped bit 1 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8261d3df064071df8082a0f813417f05'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    162
		comment: 'Flipped bit 7 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '0061d3df064071df8082a0f813417f05'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    163
		comment: 'Flipped bit 8 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8060d3df064071df8082a0f813417f05'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    164
		comment: 'Flipped bit 31 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d35f064071df8082a0f813417f05'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    165
		comment: 'Flipped bit 32 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d3df074071df8082a0f813417f05'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    166
		comment: 'Flipped bit 33 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d3df044071df8082a0f813417f05'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    167
		comment: 'Flipped bit 63 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d3df0640715f8082a0f813417f05'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    168
		comment: 'Flipped bit 64 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d3df064071df8182a0f813417f05'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    169
		comment: 'Flipped bit 77 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d3df064071df80a2a0f813417f05'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    170
		comment: 'Flipped bit 80 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d3df064071df8082a1f813417f05'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    171
		comment: 'Flipped bit 96 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d3df064071df8082a0f812417f05'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    172
		comment: 'Flipped bit 97 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d3df064071df8082a0f811417f05'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    173
		comment: 'Flipped bit 120 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d3df064071df8082a0f813417f04'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    174
		comment: 'Flipped bit 121 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d3df064071df8082a0f813417f07'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    175
		comment: 'Flipped bit 126 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d3df064071df8082a0f813417f45'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    176
		comment: 'Flipped bit 127 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d3df064071df8082a0f813417f85'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    177
		comment: 'Flipped bit 63 and 127 in tag expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '8061d3df0640715f8082a0f813417f85'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    178
		comment: 'Tag changed to all zero expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     '00000000000000000000000000000000'
		result:  'invalid'
		flags:   ''
	},
	XChaCha20Test{
		tcid:    179
		comment: 'tag change to all 1 expected tag:8061d3df064071df8082a0f813417f05'
		key:     '202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f'
		iv:      '000102030405060708090a0b0c0d0e0f1011121314151617'
		aad:     '000102'
		msg:     '000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20'
		ct:      'b70886f2313d015e1fe741365f5e35f1080e0f78ccfb51809417e879689418ef98'
		tag:     'ffffffffffffffffffffffffffffffff'
		result:  'invalid'
		flags:   ''
	},
]
