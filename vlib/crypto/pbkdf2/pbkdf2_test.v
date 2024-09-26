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
