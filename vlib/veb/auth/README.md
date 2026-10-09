## Description

`veb.auth` is a module that helps with common logic required for authentication.

It allows to easily generate hashed and salted passwords and to compare password hashes.

It also handles authentication tokens, including DB table creation and insertion.
All DBs are supported.

## Usage

```v
import veb
import db.pg
import veb.auth

pub struct App {
	veb.StaticHandler
pub mut:
	db   pg.DB
	auth auth.Auth[pg.DB] // or auth.Auth[sqlite.DB] etc
}

const port = 8081

pub struct Context {
	veb.Context
	current_user User
}

struct User {
	id            int @[primary; sql: serial]
	name          string
	password_hash string
}

fn main() {
	mut app := &App{
		// Use your actual local PostgreSQL password here.
		db: pg.connect(
			host:     'localhost'
			user:     'postgres'
			password: 'password'
			dbname:   'postgres'
		)!
	}
	app.auth = auth.new(app.db)
	veb.run[App, Context](mut app, port)
}

@[post]
pub fn (mut app App) register_user(mut ctx Context, name string, password string) veb.Result {
	new_user := User{
		name:          name
		password_hash: auth.hash_password(password)
	}
	user_id := sql app.db {
		insert new_user into User
	} or { 0 }

	// Generate and insert the token using user ID
	token := app.auth.add_token(user_id) or { '' }
	// Authenticate the user by adding the token to the cookies
	ctx.set_cookie(name: 'token', value: token)

	return ctx.redirect('/')
}

@[post]
pub fn (mut app App) login_post(mut ctx Context, name string, password string) veb.Result {
	user := app.find_user_by_name(name) or {
		ctx.error('Bad credentials')
		return ctx.redirect('/login')
	}
	// Verify user password using veb.auth
	if !auth.compare_password_with_hash(password, '', user.password_hash) {
		ctx.error('Bad credentials')
		return ctx.redirect('/login')
	}
	// Find the user token in the Token table
	token := app.auth.add_token(user.id) or { '' }
	// Authenticate the user by adding the token to the cookies
	ctx.set_cookie(name: 'token', value: token)
	return ctx.redirect('/')
}

pub fn (mut app App) find_user_by_name(name string) ?User {
	// ... db query
	return User{}
}
```

On macOS, Postgres.app rejects passwordless `trust` connections from unknown
processes. Use a real password in the example above instead of `password: ''`.

## Security considerations

`hash_password` generates a fresh 16-byte salt using the operating system's cryptographic
random source. It derives a 32-byte key using PBKDF2-HMAC-SHA256 with 600,000 iterations.
The returned verifier stores its algorithm, format version, work factor, salt and key:
`pbkdf2-sha256$v1$600000$<hex salt>$<hex key>`. Store the entire string in `password_hash`;
allow enough space for the full verifier rather than limiting the column to 64 characters.

`hash_password_with_salt` retains its existing signature for applications that supply their
own salt. It produces the same versioned format and generates a fresh salt if given an empty
string. `compare_password_with_hash` reads the salt and work factor from versioned verifiers,
so its salt argument can be empty for new accounts. It rejects unsupported versions and
invalid or excessive work factors, and compares the derived keys in constant time.

Existing 64-character SHA256 verifiers are still accepted with their original separate salt.
After a successful legacy login, replace the stored verifier with `hash_password(password)`
and persist it before discarding the old salt. Identify legacy verifiers by their 64-character
length. Legacy verification is provided only for migration; new hashes always use PBKDF2.
Applications remain responsible for persisting updated verifiers and limiting login attempts.

The default work factor follows the PBKDF2-HMAC-SHA256 guidance in the
[OWASP Password Storage Cheat Sheet][password-storage].

[password-storage]: https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html
