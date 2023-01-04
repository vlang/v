import crypto.bcrypt

fn test_crypto_bcrypt() {
	hash := bcrypt.generate_from_password('password'.bytes(), 10) or { panic(err) }

	bcrypt.compare_hash_and_password('password'.bytes(), hash.bytes()) or { panic(err) }

	bcrypt.compare_hash_and_password('password2'.bytes(), hash.bytes()) or {
		assert err.msg() == 'mismatched hash and password'
	}

	hash2 := bcrypt.generate_from_password('bb'.bytes(), 10) or { panic(err) }
	mut hash2_must_mismatch := false

	bcrypt.compare_hash_and_password('bbb'.bytes(), hash2.bytes()) or {
		hash2_must_mismatch = true
		assert err.msg() == 'mismatched hash and password'
	}

	assert hash2_must_mismatch
}
