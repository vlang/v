import crypto.sha1
import crypto.sha512
import crypto.sha256
import crypto.pbkdf2
import encoding.hex
import hash

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
		key := pbkdf2.key(c.password.bytes(), c.salt.bytes(), c.count, c.key_length,
			sha256.new224())!
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
		key := pbkdf2.key(c.password.bytes(), c.salt.bytes(), c.count, c.key_length,
			sha512.new384())!
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

struct Sha1Case {
	password   string
	salt       string
	count      int
	key_length int
	expected   string
}

// The PBKDF2-HMAC-SHA1 test vectors of RFC 6070, without the one that takes 16777216 iterations.
const sha1_cases = [
	Sha1Case{'password', 'salt', 1, 20, '0c60c80f961f0e71f3a9b524af6012062fe037a6'},
	Sha1Case{'password', 'salt', 2, 20, 'ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957'},
	Sha1Case{'password', 'salt', 4096, 20, '4b007901b765489abead49d926f721d065a429c1'},
	Sha1Case{'passwordPASSWORDpassword', 'saltSALTsaltSALTsaltSALTsaltSALTsalt', 4096, 25, '3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038'},
	Sha1Case{'pass\0word', 'sa\0lt', 4096, 16, '56fa6aa75548099dcc37d7f03425e0c3'},
]

fn test_sha1() {
	for c in sha1_cases {
		key := pbkdf2.key(c.password.bytes(), c.salt.bytes(), c.count, c.key_length, sha1.new())!
		assert key.hex() == c.expected, 'failed c=${c.count} dkLen=${c.key_length}'
	}
}

struct TruncatedSha512Case {
	password   string
	salt       string
	count      int
	key_length int
	sha512_224 string
	sha512_256 string
}

// The expected values were generated with Python's `hashlib.pbkdf2_hmac`
// (OpenSSL), with the digest names 'sha512_224' and 'sha512_256'.
const truncated_sha512_cases = [
	TruncatedSha512Case{
		password:   'password'
		salt:       'salt'
		count:      1
		key_length: 20
		sha512_224: 'b34ab626276a61ce19d2ecb4c7e15f8198a2989a'
		sha512_256: '4b6a63117d3ec0032624616082c1c1912f56fa5f'
	},
	TruncatedSha512Case{
		password:   'password'
		salt:       'salt'
		count:      2
		key_length: 20
		sha512_224: 'b8878ac5e4509c165c1b508961fa3c3afcef3f37'
		sha512_256: 'fcfd108c99cc888ec0af9f184885aff5f02d19a9'
	},
	TruncatedSha512Case{
		password:   'password'
		salt:       'salt'
		count:      4096
		key_length: 20
		sha512_224: 'ed54af699cc307e08965098bda5ff4e41ea1931f'
		sha512_256: 'f2fbe5f8ec3618bb145279a8c6a8dfa476c282a3'
	},
	TruncatedSha512Case{
		password:   'passwordPASSWORDpassword'
		salt:       'saltSALTsaltSALTsaltSALTsaltSALTsalt'
		count:      4096
		key_length: 64
		sha512_224: '573df96762ea7da4f71231859ca282ef482764ad9671c5275c3272fe6ae94d285a5709d1080fd6d8b88b696e3072f0e1a2a378a98592dd26df77e3557c168019'
		sha512_256: '31cf94e3d8e36aa18d40ad92654ab80f500ed7fb575a2215547db6f82dd227ed0f41215e8f9bb97641a2d8156b7b7c16a669a0475d609314d0fa8cc2ace4ec66'
	},
]

fn test_sha512_224() {
	for c in truncated_sha512_cases {
		key := pbkdf2.key(c.password.bytes(), c.salt.bytes(), c.count, c.key_length,
			sha512.new512_224())!
		assert key.hex() == c.sha512_224, 'failed c=${c.count} dkLen=${c.key_length}'
	}
}

fn test_sha512_256() {
	for c in truncated_sha512_cases {
		key := pbkdf2.key(c.password.bytes(), c.salt.bytes(), c.count, c.key_length,
			sha512.new512_256())!
		assert key.hex() == c.sha512_256, 'failed c=${c.count} dkLen=${c.key_length}'
	}
}

// Every digest that `pbkdf2.key` supports.
const variants = ['sha1', 'sha224', 'sha256', 'sha384', 'sha512', 'sha512_224', 'sha512_256']

fn new_hash(name string) hash.Hash {
	match name {
		'sha1' { return sha1.new() }
		'sha224' { return sha256.new224() }
		'sha256' { return sha256.new() }
		'sha384' { return sha512.new384() }
		'sha512' { return sha512.new() }
		'sha512_224' { return sha512.new512_224() }
		'sha512_256' { return sha512.new512_256() }
		else { panic('unknown hash ${name}') }
	}
}

fn hash_sum(name string, data []u8) []u8 {
	return match name {
		'sha1' { sha1.sum(data) }
		'sha224' { sha256.sum224(data) }
		'sha256' { sha256.sum256(data) }
		'sha384' { sha512.sum384(data) }
		'sha512' { sha512.sum512(data) }
		'sha512_224' { sha512.sum512_224(data) }
		'sha512_256' { sha512.sum512_256(data) }
		else { panic('unknown hash ${name}') }
	}
}

fn block_size_of(name string) int {
	return if name in ['sha1', 'sha224', 'sha256'] { sha256.block_size } else { sha512.block_size }
}

// naive_hmac is a direct transcription of RFC 2104, used as a reference.
fn naive_hmac(name string, key []u8, data []u8) []u8 {
	block_size := block_size_of(name)
	mut k := if key.len > block_size { hash_sum(name, key) } else { key.clone() }
	for k.len < block_size {
		k << 0
	}
	mut inner := []u8{}
	mut outer := []u8{}
	for b in k {
		inner << (b ^ 0x36)
		outer << (b ^ 0x5c)
	}
	inner << data
	outer << hash_sum(name, inner)
	return hash_sum(name, outer)
}

// naive_pbkdf2 is a direct transcription of RFC 8018, section 5.2, used as a reference.
fn naive_pbkdf2(name string, password []u8, salt []u8, count int, key_length int) []u8 {
	mut dk := []u8{}
	for i := 1; dk.len < key_length; i++ {
		mut msg := salt.clone()
		msg << [u8(i >> 24), u8(i >> 16), u8(i >> 8), u8(i)]
		mut u := naive_hmac(name, password, msg)
		mut t := u.clone()
		for _ in 1 .. count {
			u = naive_hmac(name, password, u)
			for j in 0 .. t.len {
				t[j] ^= u[j]
			}
		}
		dk << t
	}
	return dk[..key_length]
}

fn test_matches_naive_reference() {
	salt := 'NaCl, and some more salt'.bytes()
	for name in variants {
		block_size := block_size_of(name)
		for count in [1, 2, 3, 100, 4096] {
			// passwords shorter than, equal to and longer than the block size
			mut password_lengths := [0, 7, block_size, block_size + 1, 2 * block_size + 3]
			mut key_lengths := [1, 20, 32, 33, 64, 100]
			if count == 4096 {
				// keep the test fast without -prod; 33 bytes needs two blocks for every
				// digest except sha384 and sha512
				password_lengths = [7, block_size, block_size + 1]
				key_lengths = [33]
			}
			for password_length in password_lengths {
				password := []u8{len: password_length, init: u8(index * 13 + 5)}
				// a shorter key is a prefix of a longer one
				expected := naive_pbkdf2(name, password, salt, count, key_lengths.last())
				for key_length in key_lengths {
					got := pbkdf2.key(password, salt, count, key_length, new_hash(name))!
					assert got == expected[..key_length], '${name} password.len=${password_length} c=${count} dkLen=${key_length}'
				}
			}
		}
	}
}
