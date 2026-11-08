// Unit tests for the pieces of the module that do not need a full exchange.
module scram

import encoding.hex

fn test_mechanism_names() {
	assert Mechanism.sha1.name() == 'SCRAM-SHA-1'
	assert Mechanism.sha256.name() == 'SCRAM-SHA-256'
	assert Mechanism.sha512.name() == 'SCRAM-SHA-512'
	assert Mechanism.sha256.name_plus() == 'SCRAM-SHA-256-PLUS'
	assert Mechanism.sha1.size() == 20
	assert Mechanism.sha256.size() == 32
	assert Mechanism.sha512.size() == 64
}

fn test_mechanism_from_name_accepts_both_spellings() {
	assert mechanism_from_name('SCRAM-SHA-1')! == .sha1
	assert mechanism_from_name('SCRAM-SHA-256')! == .sha256
	assert mechanism_from_name('SCRAM-SHA-512')! == .sha512
	assert mechanism_from_name('SCRAM-SHA-256-PLUS')! == .sha256
	assert mechanism_from_name('SCRAM-SHA-512-PLUS')! == .sha512
}

fn test_mechanism_from_name_rejects_the_rest() {
	for name in ['', 'PLAIN', 'CRAM-MD5', 'scram-sha-256', 'SCRAM-SHA-224'] {
		mechanism_from_name(name) or {
			assert err.msg() == 'scram: unsupported mechanism: ${name}'
			assert err is UnsupportedMechanism, name
			continue
		}
		assert false, 'accepted the unsupported mechanism `${name}`'
	}
}

fn test_new_credentials_generates_a_fresh_salt() {
	first := new_credentials(.sha256, 'pencil')!
	second := new_credentials(.sha256, 'pencil')!
	assert first.salt.len == default_salt_size
	assert first.iterations == default_iterations
	assert first.stored_key.len == 32
	assert first.server_key.len == 32
	// The same password must not produce the same record twice, otherwise
	// a stolen database would show which users share a password.
	assert first.salt != second.salt
	assert first.stored_key != second.stored_key
}

fn test_derive_credentials_rejects_unusable_parameters() {
	derive_credentials(.sha256, 'pencil', []u8{}, 4096) or {
		assert err.msg() == 'scram: the salt must not be empty'
		derive_credentials(.sha256, 'pencil', 'salt'.bytes(), 0) or {
			assert err.msg() == 'scram: the iteration count must be at least 1, got 0'
			return
		}
		assert false, 'accepted a zero iteration count'
	}
	assert false, 'accepted an empty salt'
}

fn test_derive_credentials_rejects_an_iteration_count_above_the_wire_limit() {
	derive_credentials(.sha256, 'pencil', 'salt'.bytes(), max_wire_iterations + 1) or {
		assert err.msg().contains('must not exceed ${max_wire_iterations}')
		return
	}
	assert false, 'derived credentials with an iteration count outside the wire grammar'
}

// dummy_secret is a fixed secret for the `dummy_credentials` tests.
const dummy_secret = []u8{len: 32, init: u8(index)}

fn test_dummy_credentials_match_independent_vectors() {
	// HMAC(secret, label || 0x00 || 'nobody'), computed with Python's hmac
	// module, so that a change of labels, which changes every stable salt,
	// cannot go unnoticed.
	vectors := {
		Mechanism.sha1: ['a32361470cb638e400f278a96aa786b9', 'afff10634d650455fb19e96f31d7a88dd4803b54',
			'bb31e856046224b4163aaafaded0632a62a2dc7f']
		.sha256:        ['398b8e4668149b86f7b295382a22a5da',
			'aa357cb20a6ac10327435f6714b3aa5b3aa891d2d1ee7e1ab336a4a0997ecc2a',
			'e19cbd917c81c69860bfc3fae0a0bf78389338afcd643832cb9a547e8b2c990f']
		.sha512:        ['5e21e3caafb81fade3eb7331028891f0',
			'7dbd0523a0d52d34b33adb2fb6525efae358f9d39616ade8c68a060d78776c4e8b437551a4dba0ea38357bb48a631a64452af866f668a5adcefbf40b354f3021',
			'2c230ba72441790481467a522f5775801cf033c87615c55a31da5a23642d06fa940d4db75d68cf08d92520ce66183f67a80d00d2313610e114046f04c8d14924']
	}
	for mechanism, expected in vectors {
		creds := dummy_credentials(mechanism, dummy_secret, 'nobody', 4096)!
		assert creds.mechanism == mechanism
		assert creds.iterations == 4096
		assert creds.salt.len == default_salt_size
		assert creds.stored_key.len == mechanism.size()
		assert creds.server_key.len == mechanism.size()
		assert hex.encode(creds.salt) == expected[0], mechanism.name()
		assert hex.encode(creds.stored_key) == expected[1], mechanism.name()
		assert hex.encode(creds.server_key) == expected[2], mechanism.name()
	}
}

fn test_dummy_credentials_are_stable_per_name_and_unrelated_across_names() {
	first := dummy_credentials(.sha256, dummy_secret, 'nobody', 4096)!
	again := dummy_credentials(.sha256, dummy_secret, 'nobody', 4096)!
	assert first.salt == again.salt
	assert first.stored_key == again.stored_key
	// The salt depends on the name, not on the iteration count.
	assert dummy_credentials(.sha256, dummy_secret, 'nobody', 8192)!.salt == first.salt
	assert dummy_credentials(.sha256, dummy_secret, 'somebody', 4096)!.salt != first.salt
	assert dummy_credentials(.sha256, dummy_secret, 'Nobody', 4096)!.salt != first.salt
	other_secret := []u8{len: 32, init: u8(index + 1)}
	assert dummy_credentials(.sha256, other_secret, 'nobody', 4096)!.salt != first.salt
}

fn test_dummy_credentials_reject_unusable_parameters() {
	dummy_credentials(.sha256, []u8{len: 31}, 'nobody', 4096) or {
		assert err.msg() == 'scram: the secret must be at least 32 bytes long, got 31'
		dummy_credentials(.sha256, dummy_secret, '', 4096) or {
			assert err.msg() == 'scram: the user name must not be empty'
			dummy_credentials(.sha256, dummy_secret, 'nobody', 0) or {
				assert err.msg() == 'scram: the iteration count must be at least 1, got 0'
				dummy_credentials(.sha256, dummy_secret, 'nobody', max_wire_iterations + 1) or {
					assert err.msg().contains('must not exceed ${max_wire_iterations}')
					return
				}
				assert false, 'accepted an iteration count outside the wire grammar'
			}
			assert false, 'accepted a zero iteration count'
		}
		assert false, 'accepted an empty user name'
	}
	assert false, 'accepted a short secret'
}

fn test_generated_nonces_are_unique_and_printable() {
	mut seen := map[string]bool{}
	for _ in 0 .. 256 {
		nonce := generate_nonce()!
		assert !seen[nonce], 'the CSPRNG repeated a nonce'
		seen[nonce] = true
		validate_nonce(nonce)!
	}
}

fn test_xor_is_its_own_inverse() {
	a := hex.decode('00112233445566778899aabbccddeeff')!
	b := hex.decode('ffeeddccbbaa99887766554433221100')!
	assert xor(xor(a, b), b) == a
	assert xor(a, a) == []u8{len: a.len}
}

fn test_credentials_survive_a_storage_round_trip() {
	for mechanism in [Mechanism.sha1, .sha256, .sha512] {
		original := derive_credentials(mechanism, 'pencil', 'saltsaltsaltsalt'.bytes(), 4096)!
		restored := parse_credentials(original.encode())!
		assert restored.mechanism == original.mechanism, mechanism.name()
		assert restored.salt == original.salt, mechanism.name()
		assert restored.iterations == original.iterations, mechanism.name()
		assert restored.stored_key == original.stored_key, mechanism.name()
		assert restored.server_key == original.server_key, mechanism.name()
	}
}

fn test_encode_matches_the_rfc_5803_layout() {
	credentials := derive_credentials(.sha256, 'pencil', 'saltsaltsaltsalt'.bytes(), 4096)!
	encoded := credentials.encode()
	// mechanism $ iterations : salt $ stored : server
	assert encoded.starts_with('SCRAM-SHA-256\$4096:c2FsdHNhbHRzYWx0c2FsdA==\$')
	assert encoded.split('\$').len == 3
	assert encoded.split('\$')[2].split(':').len == 2
}

fn test_parse_credentials_refuses_a_broken_record() {
	good := derive_credentials(.sha256, 'pencil', 'saltsaltsaltsalt'.bytes(), 4096)!.encode()
	parts := good.split('\$')
	cases := {
		'empty':                      ''
		'no separators':              'SCRAM-SHA-256'
		'one separator':              '${parts[0]}\$${parts[1]}'
		'an unknown mechanism':       'SCRAM-MD5\$${parts[1]}\$${parts[2]}'
		'a -PLUS mechanism':          'SCRAM-SHA-256-PLUS\$${parts[1]}\$${parts[2]}'
		'no colon in the info':       '${parts[0]}\$4096\$${parts[2]}'
		'no colon in the value':      '${parts[0]}\$${parts[1]}\$abcd'
		'a zero iteration count':     '${parts[0]}\$0:c2FsdA==\$${parts[2]}'
		'a salt that is not base64':  '${parts[0]}\$4096:not!base64\$${parts[2]}'
		'an empty salt':              '${parts[0]}\$4096:\$${parts[2]}'
		'keys of the wrong length':   '${parts[0]}\$${parts[1]}\$c2hvcnQ=:c2hvcnQ='
		'keys for another mechanism': 'SCRAM-SHA-512\$${parts[1]}\$${parts[2]}'
	}
	for what, record in cases {
		parse_credentials(record) or {
			assert err is MalformedMessage || err is UnsupportedMechanism, '${what}: ${err}'
			continue
		}
		assert false, 'accepted a record with ${what}'
	}
}

fn test_secrets_are_not_rendered_by_str() {
	// V formats structs automatically, so anything printed while debugging must
	// not carry the password or the key material.
	client := new_client(username: 'alice', password: 'sup3r-s3cret')!
	assert !client.str().contains('sup3r-s3cret')
	assert client.str().contains('alice')

	credentials := derive_credentials(.sha256, 'sup3r-s3cret', 'saltsalt'.bytes(), 4096)!
	rendered := '${credentials}'
	assert !rendered.contains(credentials.server_key.hex())
	assert !rendered.contains(credentials.stored_key.hex())
	assert !rendered.contains('${credentials.server_key}')
	assert rendered.contains('SCRAM-SHA-256')

	mut server := new_server(
		lookup: fn [credentials] (username string) !Credentials {
			return credentials
		}
	)!
	server.first(client_first_of('alice'))!
	assert !server.str().contains('${credentials.server_key}')
	assert server.str().contains('alice')
}

fn client_first_of(username string) string {
	mut client := new_client(username: username, password: 'sup3r-s3cret') or { panic(err) }
	return client.first() or { panic(err) }
}
