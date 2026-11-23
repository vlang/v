module auth

import crypto.sha256
import rand
import encoding.hex

fn test_password_verifier_matches_production_work_factor_vectors() {
	// Independent PBKDF2-HMAC-SHA256 vectors with 600,000 iterations and 32-byte keys.
	prefix := 'pbkdf2-sha256$v1$600000$73616c74$'
	ascii := prefix + '669cfe52482116fda1aa2cbe409b2f56c8e4563752b7a28f6eaab614ee005178'
	assert hash_password_with_salt('password', 'salt') == ascii
	assert compare_password_with_hash('password', 'unused-separate-salt', ascii)
	assert hash_password_with_salt('日本語', 'salt') == prefix +
		'd0f34255b7bad7437a0fc9651b04a77b058561642d7d888cb2224e41ffa76c37'
}

fn test_legacy_password_verifier_remains_compatible() {
	salt := 'old-salt'
	hashed := sha256.sum('password${salt}'.bytes()).hex()
	assert compare_password_with_hash('password', salt, hashed)
	assert !compare_password_with_hash('wrong', salt, hashed)
	assert !compare_password_with_hash('password', 'wrong-salt', hashed)
}

fn test_generated_salts_are_not_repeated_or_controlled_by_rand_seed() {
	mut salts := map[string]bool{}
	for _ in 0 .. 32 {
		rand.seed([u32(1), 2])
		salt := generate_salt()
		assert salt.len == 32
		assert hex.decode(salt)!.len == 16
		assert salt !in salts
		salts[salt] = true
	}
}

fn test_password_verifier_is_self_contained_and_empty_salts_are_random() {
	first := hash_password_with_salt('', '')
	second := hash_password('password')
	first_salt := first.split('$')[3]
	second_salt := second.split('$')[3]
	// The persisted salt stores the hex-encoded form of generate_salt's text.
	assert hex.decode(first_salt)!.len == 32
	assert hex.decode(second_salt)!.len == 32
	assert first_salt != second_salt
	assert compare_password_with_hash('', '', first)
	assert compare_password_with_hash('password', '', second)
}

fn test_pbkdf2_password_verifier_matches_known_vector() {
	// PBKDF2-HMAC-SHA256(password, salt, 1, 32).
	hashed := 'pbkdf2-sha256$v1$1$73616c74$120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b'
	assert compare_password_with_hash('password', '', hashed)
	assert !compare_password_with_hash('Password', '', hashed)
	assert !compare_password_with_hash('', '', hashed)
	assert !compare_password_with_hash('password', '', hashed.replace('$v1$', '$v2$'))
	assert !compare_password_with_hash('password', '', hashed.replace('$73616c74$', '$73616c75$'))
	assert !compare_password_with_hash('password', '', hashed[..63])
	assert !compare_password_with_hash('password', '', hashed[..hashed.len - 1] + 'a')
}

fn test_password_verifier_preserves_password_bytes() {
	// Independent two-iteration vectors exercise verification and exact byte matching
	// without repeating the production work factor for every password.
	for password, key in {
		'':                             '62384466264daadc4144018c6bd864648272b34da8980d31521ffcce92ae003b'
		'a':                            '7ff76b82ae4943db0fd9ec9a9f777dae7ffee62b3f79b044beadac19a137b8c4'
		'hunter2':                      '6854de87b3a514187b2104973b096232b1e5f1d0d3e61e5bea027657789f3867'
		'correct horse battery staple': '6515520332d81f79aeab232116cf2f42d18ce89e41609a43e698ee3c2841e119'
		'élève':                        'ddf5da4860c84c6c2bc88dd92d8632b4caeea52ff28072d447a6f5cb64bc6b16'
		'日本語':                       '66abecbe19c77e4aaf79fb9a45f74fa91265baf8a4252707ba7b51b4cd5f9add'
	} {
		hashed := 'pbkdf2-sha256$v1$2$73616c74$${key}'
		assert compare_password_with_hash(password, '', hashed), password
		assert !compare_password_with_hash('${password} ', '', hashed), password
	}
}

fn test_malformed_password_verifiers_are_rejected_before_derivation() {
	key := '00'.repeat(32)
	for hashed in [
		'',
		'00'.repeat(31),
		'zz'.repeat(32),
		'pbkdf2-sha256$v1$1$73616c74',
		'pbkdf2-sha256$v1$1$73616c74$${key}$extra',
		'unknown$v1$1$73616c74$${key}',
		'pbkdf2-sha256$v2$1$73616c74$${key}',
		'pbkdf2-sha256$v1$0$73616c74$${key}',
		'pbkdf2-sha256$v1$-1$73616c74$${key}',
		'pbkdf2-sha256$v1$10000001$73616c74$${key}',
		'pbkdf2-sha256$v1$9999999999999999999999999$73616c74$${key}',
		'pbkdf2-sha256$v1$no$73616c74$${key}',
		'pbkdf2-sha256$v1$01$73616c74$${key}',
		'pbkdf2-sha256$v1$1$$${key}',
		'pbkdf2-sha256$v1$1$0x$${key}',
		'pbkdf2-sha256$v1$1$z0$${key}',
		'pbkdf2-sha256$v1$1$123$${key}',
		'pbkdf2-sha256$v1$1$73616c74$00',
		'pbkdf2-sha256$v1$1$73616c74$${'zz'.repeat(32)}',
	] {
		assert !compare_password_with_hash('password', '', hashed), hashed
	}
}
