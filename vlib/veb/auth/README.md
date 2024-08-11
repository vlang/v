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
	salt          string
}

fn main() {
	mut app := &App{
		db: pg.connect(host: 'localhost', user: 'postgres', password: '', dbname: 'postgres')!
	}
	app.auth = auth.new(app.db)
	veb.run[App, Context](mut app, port)
}

@[post]
pub fn (mut app App) register_user(mut ctx Context, name string, password string) veb.Result {
	salt := auth.generate_salt()
	new_user := User{
		name:          name
		password_hash: auth.hash_password_with_salt(password, salt)
		salt:          salt
	}
	sql app.db {
		insert new_user into User
	} or {}

	// Get new user ID (until RETURNING is supported by ORM)
	if x := app.find_user_by_name(name) {
		// Generate and insert the token using user ID
		token := app.auth.add_token(x.id) or { '' }
		// Authenticate the user by adding the token to the cookies
		ctx.set_cookie(name: 'token', value: token)
	}

	return ctx.redirect('/')
}

@[post]
pub fn (mut app App) login_post(mut ctx Context, name string, password string) veb.Result {
	user := app.find_user_by_name(name) or {
		ctx.error('Bad credentials')
		return ctx.redirect('/login')
	}
	// Verify user password using veb.auth
	if !auth.compare_password_with_hash(password, user.salt, user.password_hash) {
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

## Security considerations

`hash_password_with_salt` and its related functions use `sha256` for hashing with a single
iteration. This is not secure for production use, and you should use a more secure hashing
algorithm and multiple iterations.

See also:
- [OWASP Password Storage Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html)