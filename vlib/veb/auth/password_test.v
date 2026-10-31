module auth

import crypto.sha256
import rand
import encoding.hex

fn test_password_verifier_stores_version_work_factor_and_salt() {
	salt := generate_salt()
	assert salt.len == 32
	assert hex.decode(salt)!.len == 16
	hashed := hash_password_with_salt('password', salt)
	parts := hashed.split('$')
	assert parts.len == 5
	assert parts[0] == 'pbkdf2-sha256'
	assert parts[1] == 'v1'
	assert parts[2] == '600000'
	assert hex.decode(parts[3])!.bytestr() == salt
	assert parts[4].len == 64
	assert compare_password_with_hash('password', salt, hashed)
	assert !compare_password_with_hash('wrong', salt, hashed)
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
	first := hash_password_with_salt('password', '')
	second := hash_password('password')
	assert first != second
	assert compare_password_with_hash('password', '', first)
	assert compare_password_with_hash('password', 'unused-separate-salt', second)
}

fn test_pbkdf2_password_verifier_matches_known_vector() {
	// PBKDF2-HMAC-SHA256(password, salt, 1, 32).
	hashed := 'pbkdf2-sha256$v1$1$73616c74$120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b'
	assert compare_password_with_hash('password', '', hashed)
	assert !compare_password_with_hash('password', '', hashed.replace('$v1$', '$v2$'))
	assert !compare_password_with_hash('password', '', hashed.replace('$73616c74$', '$73616c75$'))
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
