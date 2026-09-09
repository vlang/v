// Test vectors adapted from the Go x/crypto argon2 package.
module argon2

import encoding.hex

const gen_kat_password = [
	u8(0x01),
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
	0x01,
]!
const gen_kat_salt = [
	u8(0x02),
	0x02,
	0x02,
	0x02,
	0x02,
	0x02,
	0x02,
	0x02,
	0x02,
	0x02,
	0x02,
	0x02,
	0x02,
	0x02,
	0x02,
	0x02,
]!
const gen_kat_secret = [u8(0x03), 0x03, 0x03, 0x03, 0x03, 0x03, 0x03, 0x03]!
const gen_kat_aad = [u8(0x04), 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04]!

struct TestVector {
	mode    int
	time    u32
	memory  u32
	threads u8
	hash    string
}

const test_vectors = [
	TestVector{
		mode:    argon2_i
		time:    1
		memory:  64
		threads: 1
		hash:    'b9c401d1844a67d50eae3967dc28870b22e508092e861a37'
	},
	TestVector{
		mode:    argon2_d
		time:    1
		memory:  64
		threads: 1
		hash:    '8727405fd07c32c78d64f547f24150d3f2e703a89f981a19'
	},
	TestVector{
		mode:    argon2_id
		time:    1
		memory:  64
		threads: 1
		hash:    '655ad15eac652dc59f7170a7332bf49b8469be1fdb9c28bb'
	},
	TestVector{
		mode:    argon2_id
		time:    2
		memory:  64
		threads: 2
		hash:    '350ac37222f436ccb5c0972f1ebd3bf6b958bf2071841362'
	},
	TestVector{
		mode:    argon2_i
		time:    3
		memory:  256
		threads: 2
		hash:    'f5bbf5d4c3836af13193053155b73ec7476a6a2eb93fd5e6'
	},
	TestVector{
		mode:    argon2_id
		time:    4
		memory:  1024
		threads: 8
		hash:    '8dafa8e004f8ea96bf7c0f93eecf67a6047476143d15577f'
	},
]!

fn test_argon2_known_answer_vectors() {
	want_d := hex.decode('512b391b6f1162975371d30919734294f868e3be3984f3c1a13a4db9fabe4acb')!
	got_d := derive_key(argon2_d, gen_kat_password[..], gen_kat_salt[..], gen_kat_secret[..],
		gen_kat_aad[..], 3, 32, 4, u32(want_d.len))!
	assert got_d == want_d

	want_i := hex.decode('c814d9d1dc7f37aa13f0d77f2494bda1c8de6b016dd388d29952a4c4672b6ce8')!
	got_i := derive_key(argon2_i, gen_kat_password[..], gen_kat_salt[..], gen_kat_secret[..],
		gen_kat_aad[..], 3, 32, 4, u32(want_i.len))!
	assert got_i == want_i

	want_id := hex.decode('0d640df58d78766c08c037a34a8b53c9d01ef0452d75b65eb52520e96b01e659')!
	got_id := derive_key(argon2_id, gen_kat_password[..], gen_kat_salt[..], gen_kat_secret[..],
		gen_kat_aad[..], 3, 32, 4, u32(want_id.len))!
	assert got_id == want_id
}

fn test_argon2_reference_vectors() {
	password := 'password'.bytes()
	salt := 'somesalt'.bytes()
	for vector in test_vectors {
		want := hex.decode(vector.hash)!
		got := derive_key(vector.mode, password, salt, []u8{}, []u8{}, vector.time, vector.memory,
			vector.threads, u32(want.len))!
		assert got == want
	}
}

fn test_generate_from_password_and_compare() {
	params := Params{
		time:     2
		memory:   64
		threads:  1
		key_len:  24
		salt_len: 16
	}
	password := 'correct horse battery staple'.bytes()
	hash := generate_from_password_with_params(password, params)!
	assert hash.starts_with('\$argon2id\$v=19\$m=64,t=2,p=1\$')
	compare_hash_and_password(password, hash.bytes())!
	compare_hash_and_password('incorrect'.bytes(), hash.bytes()) or {
		assert err.msg() == 'mismatched hash and password'
		return
	}
	assert false
}

fn test_generate_from_password_uses_defaults() {
	hash := generate_from_password('hunter2'.bytes())!
	assert hash.starts_with('\$argon2id\$v=19\$m=65536,t=3,p=4\$')
}
