import crypto.sha512
import crypto.sha256
import crypto.pbkdf2
import encoding.hex

struct TestCaseData {
	name       string
	password   string
	salt       string
	count      int
	key_length int
	sha224     string
	sha256     string
	sha384     string
	sha512     string
}

const cases = [
	TestCaseData{
		name:       'test case 1'
		password:   'password'
		salt:       'salt'
		count:      1
		key_length: 20
		sha224:     '3c198cbdb9464b7857966bd05b7bc92bc1cc4e6e'
		sha256:     '120fb6cffcf8b32c43e7225256c4f837a86548c9'
		sha384:     'c0e14f06e49e32d73f9f52ddf1d0c5c719160923'
		sha512:     '867f70cf1ade02cff3752599a3a53dc4af34c7a6'
	},
	TestCaseData{
		name:       'test case 2'
		password:   'password'
		salt:       'salt'
		count:      2
		key_length: 20
		sha224:     '93200ffa96c5776d38fa10abdf8f5bfc0054b971'
		sha256:     'ae4d0c95af6b46d32d0adff928f06dd02a303f8e'
		sha384:     '54f775c6d790f21930459162fc535dbf04a93918'
		sha512:     'e1d9c16aa681708a45f5c7c4e215ceb66e011a2e'
	},
	TestCaseData{
		name:       'test case 3'
		password:   'password'
		salt:       'salt'
		count:      4096
		key_length: 20
		sha224:     '218c453bf90635bd0a21a75d172703ff6108ef60'
		sha256:     'c5e478d59288c841aa530db6845c4c8d962893a0'
		sha384:     '559726be38db125bc85ed7895f6e3cf574c7a01c'
		sha512:     'd197b1b33db0143e018b12f3d1d1479e6cdebdcc'
	},
	/* this test takes several minutes
	TestCaseData{
		name:       'test case 4'
		password:   'password'
		salt:       'salt'
		count:      16777216
		key_length: 20
		sha224:     'b49925184cb4b559f365e94fcafcd4cdb9f7aef4'
		sha256:     'cf81c66fe8cfc04d1f31ecb65dab4089f7f179e8'
		sha384:     'a7fdb349ba2bfa6bf647bb0161bae1320df27e64'
		sha512:     '6180a3ceabab45cc3964112c811e0131bca93a35'
	},
    */
	TestCaseData{
		name:       'test case 5'
		password:   'passwordPASSWORDpassword'
		salt:       'saltSALTsaltSALTsaltSALTsaltSALTsalt'
		count:      4096
		key_length: 25
		sha224:     '056c4ba438ded91fc14e0594e6f52b87e1f3690c0dc0fbc057'
		sha256:     '348c89dbcbd32b2f32d814b8116e84cf2b17347ebc1800181c'
		sha384:     '819143ad66df9a552559b9e131c52ae6c5c1b0eed18f4d283b'
		sha512:     '8c0511f4c6e597c6ac6315d8f0362e225f3c501495ba23b868'
	},
	TestCaseData{
		name:       'test case 6'
		password:   'pass\0word'
		salt:       'sa\0lt'
		count:      4096
		key_length: 16
		sha224:     '9b4011b641f40a2a500a31d4a392d15c'
		sha256:     '89b69d0516f829893c696226650a8687'
		sha384:     'a3f00ac8657e095f8e0823d232fc60b3'
		sha512:     '9d9e9c4cd21fe4be24d5b8244c759665'
	},
	TestCaseData{
		name:       'test case 7'
		password:   'passwd'
		salt:       'salt'
		count:      1
		key_length: 128
		sha224:     'e55bd77cfc18b012ac6362e22d7cdf77c4b03879a6af51fbf0045bc32a03e7f0d829d26b765bff0ca5873e07a8e85804ff4a17683ed706130d51657456bc0ebd07c35ca0675b3113ad9c33fe48a5eb9e9dc6c6a8cf5cf6de1318b414dbe667bfaeb863ef8399ff4a732520dab4ba82336513a25077ddfc11fc618c11efaf04ae'
		sha256:     '55ac046e56e3089fec1691c22544b605f94185216dde0465e68b9d57c20dacbc49ca9cccf179b645991664b39d77ef317c71b845b1e30bd509112041d3a19783c294e850150390e1160c34d62e9665d659ae49d314510fc98274cc79681968104b8f89237e69b2d549111868658be62f59bd715cac44a1147ed5317c9bae6b2a'
		sha384:     'cd3443723a41cf1460cca9efeede428a8898a82d2ad4d1fc5cca08ed3f4d3cb47a62a70b3cb9ce65dcbfb9fb9d425027a8be69b53e2a22674b0939e5e0a682f76d21f449ad184562a3bc4c519b4d048de6d8e0999fb88770f95e40185e19fc8b68767417ccc064f47a455d045b3bafda7e81b97ad0e4c5581af1aa27871cd5e4'
		sha512:     'c74319d99499fc3e9013acff597c23c5baf0a0bec5634c46b8352b793e324723d55caa76b2b25c43402dcfdc06cdcf66f95b7d0429420b39520006749c51a04ef3eb99e576617395a178ba33214793e48045132928a9e9bf2661769fdc668f31798597aaf6da70dd996a81019726084d70f152baed8aafe2227c07636c6ddece'
	},
	TestCaseData{
		name:       'test case 8'
		password:   'Password'
		salt:       'NaCl'
		count:      80000
		key_length: 128
		sha224:     'bebbdf809d53fc84531d0abe06679a8c8526fde47b47245634186908335857334a7578543f9241726d845ee8e575105e4a733b5dcaefa7560af3d028eccf95937535918dbaa84269fc0586711e7a5b9dc0d4c28fc7a89469db7ff5829b8fc1ef709d7ef95c6c7db24cece88f7c1408c8e7cee55c84db0eebb8d8e419bb50e17b'
		sha256:     '4ddcd8f60b98be21830cee5ef22701f9641a4418d04c0414aeff08876b34ab56a1d425a1225833549adb841b51c9b3176a272bdebba1d078478f62b397f33c8d62aae85a11cdde829d89cb6ffd1ab0e63a981f8747d2f2f9fe5874165c83c168d2eed1d2d5ca4052dec2be5715623da019b8c0ec87dc36aa751c38f9893d15c3'
		sha384:     '11c198987730fa113458053cd5cc9b51d7024a35f9134f1ee8740923c901aab23bbaea43686981b6e6a9f4130a1401daeeec74060246ebac958f3cfc3c65579b6e3d08b94ade5fc257a6902a0a1664b8dbd5a8ae2af70438931d3f3679abffc7a17770582f1ee413cc0d9914ce5f8143c8a7dc9c43fbc31e3d41b2030fb73c02'
		sha512:     'e6337d6fbeb645c794d4a9b5b75b7b30dac9ac50376a91df1f4460f6060d5addb2c1fd1f84409abacc67de7eb4056e6bb06c2d82c3ef4ccd1bded0f675ed97c65c33d39f81248454327aa6d03fd049fc5cbb2b5e6dac08e8ace996cdc960b1bd4530b7e754773d75f67a733fdb99baf6470e42ffcb753c15c352d4800fb6f9d6'
	},
	TestCaseData{
		name:       'test case 9'
		password:   'Password'
		salt:       'sa\0lt'
		count:      4096
		key_length: 256
		sha224:     'a329a360c825e12e454ad8633a842a06ba1456907770779d1fa4e0b61a5b1c6ce02e71de74ae433bbf14b907690d008d0cab5b01c976c1e627b027a9a809fd001082c809650344ecfcdebdf0d64b92cb1e869bf91b75517ea36918127b1eccc4cac145fb965071292a6dfa388d8ad893d2541f83a0dac1c55d2d90709963b066de985e92974e87b7d8c0e8026d96684bb0425203919b4792962b065e2b2b815ba888b8428ae51f57a74f637a658e27cf5fbc5593e85f775a1f81660850a723e2eb565f30dfc2cf2973ad57ec95b89c0979c7bab81c11d8987540a32badb2f7bbe4ff21a4f0d91dbd911b88ddd928603fd27b0ede994ee99edd2c04667b82067f'
		sha256:     '436c82c6af9010bb0fdb274791934ac7dee21745dd11fb57bb90112ab187c495ad82df776ad7cefb606f34fedca59baa5922a57f3e91bc0e11960da7ec87ed0471b456a0808b60dff757b7d313d4068bf8d337a99caede24f3248f87d1bf16892b70b076a07dd163a8a09db788ae34300ff2f2d0a92c9e678186183622a636f4cbce15680dfea46f6d224e51c299d4946aa2471133a649288eef3e4227b609cf203dba65e9fa69e63d35b6ff435ff51664cbd6773d72ebc341d239f0084b004388d6afa504eee6719a7ae1bb9daf6b7628d851fab335f1d13948e8ee6f7ab033a32df447f8d0950809a70066605d6960847ed436fa52cdfbcf261b44d2a87061'
		sha384:     'cf6f194aaf4e970afea1f41169045029e34759e124a670b5f73053da552a190ad2d7085533b8b22901f0e3caeeb431ba673468f981352dfcbe517699db791777cf52346a460b093c59ea300fb18daee270e2ea8473806da1663cebe7438b51fe56ba832c13d88ad5b2e46404457c34cc6ad8e5cd8707a1acfa737f3617628a5983d8d10fa16a92652cfa736d4610132710a517c216cc3252e6c2b8aae0275d04a49756fa5bf1bb067bc367d1b8c80c3df7dc22ee74b4be4150871624bfdde3f86f5fbd4e0828af7d5a4f01b5605e54471435d827eaecf199db315ae60d1a6350105c0e1a71b40518a4a66ebba4792a511f8f52aeac961ebea215f8fb89ba998b'
		sha512:     '10176fb32cb98cd7bb31e2bb5c8f6e425c103333a2e496058e3fd2bd88f657485c89ef92daa0668316bc23ebd1ef88f6dd14157b2320b5d54b5f26377c5dc279b1dcdec044bd6f91b166917c80e1e99ef861b1d2c7bce1b961178125fb86867f6db489a2eae0022e7bc9cf421f044319fac765d70cb89b45c214590e2ffb2c2b565ab3b9d07571fde0027b1dc57f8fd25afa842c1056dd459af4074d7510a0c020b914a5e202445d4d3f151070589dd6a2554fc506018c4f001df6239643dc86771286ae4910769d8385531bba57544d63c3640b90c98f1445ebdd129475e02086b600f0beb5b05cc6ca9b3633b452b7dad634e9336f56ec4c3ac0b4fe54ced8'
	},
]

fn test_sha224() {
	for c in cases {
		expected_result := hex.decode(c.sha224)!
		key := pbkdf2.key(c.password.bytes(), c.salt.bytes(), c.count, c.key_length, sha256.new224())!
		assert key == expected_result, 'failed ${c.name}'
	}
}

fn test_sha256() {
	for c in cases {
		expected_result := hex.decode(c.sha256)!
		key := pbkdf2.key(c.password.bytes(), c.salt.bytes(), c.count, c.key_length, sha256.new())!
		assert key == expected_result, 'failed ${c.name}'
	}
}

fn test_sha384() {
	for c in cases {
		expected_result := hex.decode(c.sha384)!
		key := pbkdf2.key(c.password.bytes(), c.salt.bytes(), c.count, c.key_length, sha512.new384())!
		assert key == expected_result, 'failed ${c.name}'
	}
}

fn test_sha512() {
	for c in cases {
		expected_result := hex.decode(c.sha512)!
		key := pbkdf2.key(c.password.bytes(), c.salt.bytes(), c.count, c.key_length, sha512.new())!
		assert key == expected_result, 'failed ${c.name}'
	}
}
