// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module auth

import rand
import crypto.rand as crypto_rand
import crypto.hmac
import crypto.sha256
import crypto.pbkdf2
import encoding.hex
import strconv

const max_safe_unsigned_integer = u32(4_294_967_295)
const password_iterations = 600_000
const max_password_iterations = 10_000_000
const password_key_length = 32

pub struct Auth[T] {
	db T
	// pub:
	// salt string
}

pub struct Token {
pub:
	id      int @[primary; sql: serial]
	user_id int
	value   string
	// ip      string
}

pub fn new[T](db T) Auth[T] {
	set_rand_crypto_safe_seed()
	sql db {
		create table Token
	} or { eprintln('veb.auth: failed to create table Token') }
	return Auth[T]{
		db: db
		// salt: generate_salt()
	}
}

// fn (mut app App) add_token(user_id int, ip string) !string {
pub fn (mut app Auth[T]) add_token(user_id int) !string {
	mut uuid := rand.uuid_v4()
	token := Token{
		user_id: user_id
		value:   uuid
		// ip: ip
	}
	sql app.db {
		insert token into Token
	}!
	return uuid
}

pub fn (app &Auth[T]) find_token(value string) ?Token {
	tokens := sql app.db {
		select from Token where value == value limit 1
	} or { []Token{} }
	if tokens.len == 0 {
		return none
	}
	return tokens.first()
}

pub fn (mut app Auth[T]) delete_tokens(user_id int) ! {
	sql app.db {
		delete from Token where user_id == user_id
	}!
}

pub fn set_rand_crypto_safe_seed() {
	first_seed := generate_crypto_safe_int_u32()
	second_seed := generate_crypto_safe_int_u32()
	rand.seed([first_seed, second_seed])
}

fn generate_crypto_safe_int_u32() u32 {
	return u32(crypto_rand.int_u64(max_safe_unsigned_integer) or { 0 })
}

// generate_salt returns 16 cryptographically random bytes encoded as hexadecimal.
// It panics if the operating system cannot provide random bytes.
pub fn generate_salt() string {
	return (crypto_rand.bytes(16) or { panic(err) }).hex()
}

// hash_password generates a fresh salt and returns a self-contained password verifier.
// Store the entire returned string; no separate salt column is needed.
pub fn hash_password(plain_text_password string) string {
	return hash_password_with_salt(plain_text_password, generate_salt())
}

// hash_password_with_salt derives a PBKDF2-HMAC-SHA256 password verifier with 600,000 iterations.
// The returned string contains the format version, iteration count, salt and derived key.
// An empty salt is replaced with a freshly generated salt.
pub fn hash_password_with_salt(plain_text_password string, salt string) string {
	actual_salt := if salt == '' { generate_salt() } else { salt }
	key := pbkdf2.key(plain_text_password.bytes(), actual_salt.bytes(), password_iterations,
		password_key_length, sha256.new()) or { panic(err) }
	return 'pbkdf2-sha256$v1$${password_iterations}$${actual_salt.bytes().hex()}$${key.hex()}'
}

// compare_password_with_hash verifies a password using a constant-time key comparison.
// Versioned verifiers contain their own salt; the salt argument is only used for legacy SHA256 hashes.
// Pass an empty salt when verifying a value returned by hash_password.
pub fn compare_password_with_hash(plain_text_password string, salt string, hashed string) bool {
	if hashed.len == password_key_length * 2 {
		// Preserve authentication for existing accounts until their verifier is upgraded.
		if _ := hex.decode(hashed) {
			digest := sha256.sum('${plain_text_password}${salt}'.bytes()).hex()
			return hmac.equal(digest.bytes(), hashed.bytes())
		}
		return false
	}
	parts := hashed.split('$')
	if parts.len != 5 || parts[0] != 'pbkdf2-sha256' || parts[1] != 'v1' {
		return false
	}
	iterations := strconv.atoi(parts[2]) or { return false }
	// Bound the work before deriving a key from a malformed persisted verifier.
	if iterations < 1 || iterations > max_password_iterations || parts[2] != iterations.str()
		|| parts[3].len == 0 || parts[3].len % 2 != 0 || parts[4].len != password_key_length * 2 {
		return false
	}
	stored_salt := hex.decode(parts[3]) or { return false }
	stored_key := hex.decode(parts[4]) or { return false }
	if stored_salt.hex() != parts[3] || stored_key.hex() != parts[4]
		|| stored_key.len != password_key_length {
		return false
	}
	key := pbkdf2.key(plain_text_password.bytes(), stored_salt, iterations, password_key_length,
		sha256.new()) or { return false }
	return hmac.equal(key, stored_key)
}
