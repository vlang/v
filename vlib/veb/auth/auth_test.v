module auth

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

fn test_generate_salt_returns_16_random_bytes_as_hex() {
	for _ in 0 .. 200 {
		salt := generate_salt()
		assert salt.len == 32
		assert is_lowercase_hex(salt), 'unexpected characters in ${salt}'
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
