module main

import crypto.hmac
import crypto.sha256
import crypto.bcrypt
import encoding.base64
import json
import databases
import time
import os

struct JwtHeader {
	alg string
	typ string
}

struct JwtPayload {
	sub         string    // (subject) = Entidade à quem o token pertence, normalmente o ID do usuário;
	iss         string    // (issuer) = Emissor do token;
	exp         string    // (expiration) = Timestamp de quando o token irá expirar;
	iat         time.Time // (issued at) = Timestamp de quando o token foi criado;
	aud         string    // (audience) = Destinatário do token, representa a aplicação que irá usá-lo.
	name        string
	roles       string
	permissions string
}

fn (mut app App) service_auth(username string, password string) ?string {
	mut db := databases.create_db_connection() or {
		eprintln(err)
		panic(err)
	}

	user := sql db {
		select from User where username == username limit 1
	}
	if user.username != username {
		return error('user not found')
	}

	if !user.active {
		return error('user is not active')
	}

	db.close()

	bcrypt.compare_hash_and_password(password.bytes(), user.password.bytes()) or {
		return error('Failed to auth user, $err')
	}

	token := make_token(user)

	return token
}

fn make_token(user User) string {
	secret := os.getenv('SECRET_KEY')

	jwt_header := JwtHeader{'HS256', 'JWT'}
	jwt_payload := JwtPayload{
		sub: '$user.id'
		name: '$user.username'
		iat: time.now()
	}

	header := base64.url_encode(json.encode(jwt_header).bytes())
	payload := base64.url_encode(json.encode(jwt_payload).bytes())
	signature := base64.url_encode(hmac.new(secret.bytes(), '${header}.$payload'.bytes(),
		sha256.sum, sha256.block_size).bytestr().bytes())

	jwt := '${header}.${payload}.$signature'

	return jwt
}

fn auth_verify(token string) bool {
	secret := os.getenv('SECRET_KEY')
	token_split := token.split('.')

	signature_mirror := hmac.new(secret.bytes(), '${token_split[0]}.${token_split[1]}'.bytes(),
		sha256.sum, sha256.block_size).bytestr().bytes()

	signature_from_token := base64.url_decode(token_split[2])

	return hmac.equal(signature_from_token, signature_mirror)
}
