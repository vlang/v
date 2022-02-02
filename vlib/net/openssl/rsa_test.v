import crypto.rsa

/*
This test has to be moved here, because in the rsa there is an strange error.
OpenSSL should be a correct place aswell.
*/

fn test_rsa() {
	instance := rsa.gen_key_pair(1024, 3) or {
		eprintln(err)
		assert false
		return
	}

	message := 'abc123456'

	encrypted, len := instance.encrypt(message.bytes()) or {
		eprintln(err)
		assert false
		return
	}

	decrypted := instance.decrypt(len, encrypted) or {
		eprintln(err)
		assert false
		return
	}

	assert message == decrypted.bytestr()
}
