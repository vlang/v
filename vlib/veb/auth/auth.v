// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module auth

import rand
import crypto.rand as crypto_rand
import crypto.hmac
import crypto.sha256

const max_safe_unsigned_integer = u32(4_294_967_295)

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

pub fn generate_salt() string {
	return rand.i64().str()
}

pub fn hash_password_with_salt(plain_text_password string, salt string) string {
	salted_password := '${plain_text_password}${salt}'
	return sha256.sum(salted_password.bytes()).hex().str()
}

pub fn compare_password_with_hash(plain_text_password string, salt string, hashed string) bool {
	digest := hash_password_with_salt(plain_text_password, salt)
	// constant time comparison
	// I know this is operating on the hex-encoded strings, but it's still constant time
	// and better than not doing it at all
	return hmac.equal(digest.bytes(), hashed.bytes())
}
