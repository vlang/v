import crypto.bcrypt

fn test_crypto_bcrypt() {
	bcrypt.compare_hash_and_password('123456'.bytes(), r'$2a$07$MRniCPEgEQnrJmmgN.maM.kF2a/TI2PB37EQQNsUtEuINwultcHTm'.bytes())!

	hash := bcrypt.generate_from_password('password'.bytes(), 5)!
	bcrypt.compare_hash_and_password('password'.bytes(), hash.bytes())!

	bcrypt.compare_hash_and_password('password2'.bytes(), hash.bytes()) or {
		assert err.msg() == 'mismatched hash and password'
	}

	hash2 := bcrypt.generate_from_password('bb'.bytes(), 5)!
	mut hash2_must_mismatch := false
	bcrypt.compare_hash_and_password('bbb'.bytes(), hash2.bytes()) or {
		hash2_must_mismatch = true
		assert err.msg() == 'mismatched hash and password'
	}

	assert hash2_must_mismatch

	long_password := 'jvaqhblwxtoytiaglflbisdeyoieianidksglxyitwopxgrjurhjvrsuydlcguaiueliuoikabibownvfcrcaogheq'
	assert long_password.len > 72
	bcrypt.generate_from_password(long_password.bytes(), 5) or {
		assert err.msg() == 'Maximum password length is 72 bytes'
	}
	bcrypt.compare_hash_and_password(long_password.bytes(), hash2.bytes()) or {
		assert err.msg() == 'Maximum password length is 72 bytes'
	}
}
