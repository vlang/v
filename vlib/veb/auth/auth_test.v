module auth

import crypto.sha256

// The db-backed entry points (`new`, `add_token`, `find_token`,
// `delete_tokens`) need an `orm.Connection`, so they are not covered here.
// These tests pin the parts that run without a database.

fn is_lowercase_hex(s string) bool {
	for c in s.bytes() {
		is_digit := c >= u8(`0`) && c <= u8(`9`)
		is_lower := c >= u8(`a`) && c <= u8(`f`)
		if !is_digit && !is_lower {
			return false
		}
	}
	return true
}

fn is_signed_decimal(s string) bool {
	if s.len == 0 {
		return false
	}
	dash := u8(`-`)
	for c in s.bytes() {
		if c == dash {
			continue
		}
		if c < u8(`0`) || c > u8(`9`) {
			return false
		}
	}
	return true
}

fn test_hash_password_with_salt_is_a_lowercase_hex_sha256() {
	hash := hash_password_with_salt('hunter2', '81345099680477805')
	assert hash.len == 64
	assert is_lowercase_hex(hash), 'unexpected characters in ${hash}'
}

fn test_hash_password_with_salt_concatenates_password_then_salt() {
	password := 'hunter2'
	salt := '81345099680477805'
	expected := sha256.sum('${password}${salt}'.bytes()).hex().str()
	assert hash_password_with_salt(password, salt) == expected
}

fn test_hash_password_with_salt_is_deterministic() {
	assert hash_password_with_salt('hunter2', 'salt') == hash_password_with_salt('hunter2', 'salt')
}

fn test_hash_password_with_salt_distinguishes_the_salt() {
	first := hash_password_with_salt('hunter2', 'salt-a')
	second := hash_password_with_salt('hunter2', 'salt-b')
	assert first != second
}

fn test_hash_password_with_salt_accepts_empty_password_and_salt() {
	empty := hash_password_with_salt('', '')
	assert empty.len == 64
	assert empty == sha256.sum(''.bytes()).hex().str()
	assert hash_password_with_salt('', 'salt') != hash_password_with_salt('pw', 'salt')
}

fn test_compare_password_with_hash_accepts_the_matching_hash() {
	salt := generate_salt()
	hash := hash_password_with_salt('correct horse battery staple', salt)
	assert compare_password_with_hash('correct horse battery staple', salt, hash)
}

fn test_compare_password_with_hash_rejects_a_wrong_password() {
	salt := generate_salt()
	hash := hash_password_with_salt('correct horse battery staple', salt)
	assert !compare_password_with_hash('Correct horse battery staple', salt, hash)
	assert !compare_password_with_hash('', salt, hash)
}

fn test_compare_password_with_hash_rejects_a_wrong_salt() {
	hash := hash_password_with_salt('hunter2', 'salt-a')
	assert !compare_password_with_hash('hunter2', 'salt-b', hash)
}

fn test_compare_password_with_hash_rejects_a_mangled_hash() {
	salt := generate_salt()
	hash := hash_password_with_salt('hunter2', salt)
	assert !compare_password_with_hash('hunter2', salt, '')
	assert !compare_password_with_hash('hunter2', salt, hash[..63])
	mut neighbour := hash.bytes()
	neighbour[62] = neighbour[62] ^ u8(0x01)
	assert !compare_password_with_hash('hunter2', salt, neighbour.bytestr())
}

fn test_hash_and_compare_round_trip_over_several_passwords() {
	passwords := ['', 'a', 'hunter2', 'correct horse battery staple', 'élève', '日本語']
	for password in passwords {
		salt := generate_salt()
		hash := hash_password_with_salt(password, salt)
		assert compare_password_with_hash(password, salt, hash), 'round trip failed for ${password}'
		assert !compare_password_with_hash('${password} ', salt, hash), 'round trip accepted a neighbour for ${password}'
	}
}

fn test_generate_salt_returns_a_signed_decimal_string() {
	for _ in 0 .. 200 {
		salt := generate_salt()
		assert is_signed_decimal(salt), 'unexpected characters in ${salt}'
	}
}

fn test_generate_salt_does_not_repeat_within_a_sample() {
	mut salts := map[string]bool{}
	for _ in 0 .. 500 {
		salt := generate_salt()
		assert salt !in salts, 'generate_salt repeated ${salt}'
		salts[salt] = true
	}
	assert salts.len == 500
}

fn test_set_rand_crypto_safe_seed_leaves_a_working_rand() {
	set_rand_crypto_safe_seed()
	mut seen := map[string]bool{}
	for _ in 0 .. 100 {
		salt := generate_salt()
		assert salt !in seen, 'generate_salt repeated ${salt} after reseeding'
		seen[salt] = true
	}
	assert seen.len == 100
}

fn test_token_struct_holds_the_values_it_is_given() {
	token := Token{
		id:      7
		user_id: 31
		value:   'ec2f1b3e'
	}
	assert token.id == 7
	assert token.user_id == 31
	assert token.value == 'ec2f1b3e'
	assert Token{}.value == ''
}

fn test_request_struct_holds_the_values_it_is_given() {
	req := Request{
		client_id:     'client-id'
		client_secret: 'client-secret'
		code:          'code'
		state:         'state'
	}
	assert req.client_id == 'client-id'
	assert req.client_secret == 'client-secret'
	assert req.code == 'code'
	assert req.state == 'state'
	assert Request{}.state == ''
}
