module auth

import db.pg // TODO make work with any db, need to make a DB interface like in Go or use generics
import rand
import crypto.rand as crypto_rand
import crypto.sha256

const max_safe_unsigned_integer = u32(4_294_967_295)

pub struct Auth {
	db pg.DB
	// pub:
	// salt string
}

pub struct Token {
pub:
	id      int    @[primary; sql: serial]
	user_id int
	value   string
	// ip      string
}

pub fn new(db pg.DB) Auth {
	set_rand_crypto_safe_seed()
	sql db {
		create table Token
	} or { eprintln('veb.auth: failed to create table Token') }
	return Auth{
		db: db
		// salt: generate_salt()
	}
}

// fn (mut app App) add_token(user_id int, ip string) !string {
pub fn (mut app Auth) add_token(user_id int) !string {
	mut uuid := rand.uuid_v4()
	token := Token{
		user_id: user_id
		value: uuid
		// ip: ip
	}
	sql app.db {
		insert token into Token
	}!
	return uuid
}

pub fn (mut app Auth) find_token(value string) ?Token {
	tokens := sql app.db {
		select from Token where value == value limit 1
	} or { []Token{} }
	if tokens.len == 0 {
		return none
	}
	return tokens.first()
}

pub fn (mut app Auth) delete_tokens(user_id int) ! {
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
	return u32(crypto_rand.int_u64(auth.max_safe_unsigned_integer) or { 0 })
}

pub fn generate_salt() string {
	return rand.i64().str()
}

pub fn hash_password_with_salt(plain_text_password string, salt string) string {
	salted_password := '${plain_text_password}${salt}'
	return sha256.sum(salted_password.bytes()).hex().str()
}

pub fn compare_password_with_hash(plain_text_password string, salt string, hashed string) bool {
	return hash_password_with_salt(plain_text_password, salt) == hashed
}
