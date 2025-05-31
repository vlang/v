# Sessions

A sessions module for web projects.

## Usage

The sessions module provides an implementation for [session stores](#custom-stores).
The session store handles the saving, storing and retrieving of data. You can
either use a store directly yourself, or you can use the `session.Sessions` struct
which is easier to use since it also handles session verification and integrates nicely
with veb.

If you want to use `session.Sessions` in your web app the session id's will be
stored using cookies. The best way to get started is to follow the
[getting started](#getting-started) section.

Otherwise have a look at the [advanced usage](#advanced-usage) section.

## Getting Started

The examples in this section use `x.veb`. See the [advanced usage](#advanced-usage) section
for examples without `x.veb`.

To start using sessions in veb embed `sessions.CurrentSession` on the
Context struct and add `sessions.Sessions` to the app struct. We must also pass the type
of our session data.

For any further example code we will use the `User` struct.
**Example:**

```v ignore
import x.sessions
import veb

pub struct User {
pub mut:
	name	 string
	verified bool
}

pub struct Context {
	veb.Context
	// By embedding the CurrentSession struct we can directly access the current session id
	// and any associated session data. Set the session data type to `User`
	sessions.CurrentSession[User]
}

pub struct App {
pub mut:
	// this struct contains the store that holds all session data it also provides
	// an easy way to manage sessions in your veb app. Set the session data type to `User`
	sessions &sessions.Sessions[User]
}
```

Next we need to create the `&sessions.Sessions[User]` instance for our app. This
struct provides functionality to easier manage sessions in a veb app.

### Session Stores

To create `sessions.Sessions` We must specify a "store" which handles the session data.
Currently veb provides two options for storing session data:

1. The `MemoryStore[T]` stores session data in memory only using the `map` datatype.
2. The `DBStore[T]` stores session data in a database by encoding the session data to JSON.
   It will create the table `DBStoreSessions` in your database, to store the session data.

It is possible to create your own session store, see [custom stores](#custom-stores).

### Starting the App

For this example we will use the memory store.

**Example:**

```v ignore
fn main() {
	mut app := &App{
		store: sessions.MemoryStore[User]{}
		// use your own secret which will be used to verify session id's
		secret: 'my secret'.bytes()
	}

	veb.run[App, Context](mut app, 8080)
}
```

### Middleware

The `sessions.veb2_middleware` module provides a middleware handler. This handler will execute
before your own route handlers and will verify the current session and fetch any associated
session data and load it into `sessions.CurrentSession`, which is embedded on the Context struct.

> **Note:**
> It is recommended to use the middleware, so the sessions are always verfied
> and loaded correctly.

**Example:**

```v ignore
// add this import at the top of your file
import x.sessions.veb2_middleware

pub struct App {
    // embed the Middleware struct from veb
    veb.Middleware[Context]
pub mut:
	// this struct contains the store that holds all session data it also provides
	// an easy way to manage sessions in your veb app. Set the session data type to `User`
	sessions &sessions.Sessions[User]
}

fn main() {
	mut app := &App{
		store: sessions.MemoryStore[User]{}
		// use your own secret which will be used to verify session id's
		secret: 'my secret'.bytes()
	}

    // register the sessions middleware
    app.use(veb2_middleware.create[User, Context](mut app.sessions))

	veb.run[App, Context](mut app, 8080)
}
```

You can now start using sessions with veb!

### Usage in endpoint handlers

#### Using Session Data

Because `sessions.CurrentSession` is embedded on the Context struct we can directly
access any session data via `ctx.session_data`. This field is an option, it will be `none`
if no data is set.

**Example:**

```v ignore
pub fn (app &App) index(mut ctx Context) veb.Result {
	// check if a user is logged in
	if user := ctx.session_data {
		return ctx.text('Welcome ${user.name}! Verification status: ${user.verified}')
	} else {
		// user is not logged in
		return ctx.text('You are not logged in :(')
	}
}
```

#### Saving / updating session data

You can use the `save` method to update and save any session data.

When the user logs in, the `save` method is called and a new session id is generated
and set as cookie. Assuming there wasn't already a session going on. If you want to
be sure that a new session id is generated when you save data, you can use the `resave`
method. This method will save the data and *always* set a new session id.

**Example:**

```v ignore
pub fn (mut app App) login(mut ctx Context) veb.Result {
	// set a session id cookie and save data for the new user
	app.sessions.save(mut ctx, User{
		name: '[no name provided]'
	}) or { return ctx.server_error('could not save session data, please try again') }
	return ctx.text('You are now logged in!')
}
```

The following endpoint checks if a session exists, if it doesn't inform the
user that they need to login.

If a session does exists the users name is updated to the `name` query parameter,
you can use this route via `http://localhost:8080/save?name=myname`. And if the
query parameter is not passed an error 400 (bad request) is returned.

**Example:**

```v ignore
pub fn (mut app App) save(mut ctx Context) veb.Result {
	// check if there is a session
	app.sessions.get(ctx) or { return ctx.request_error('You are not logged in :(') }

	if name := ctx.query['name'] {
		// update the current user
		app.sessions.save(mut ctx, User{
			name: name
		}) or { return ctx.server_error('could not save session data, please try again') }
		return ctx.redirect('/', typ: .see_other)
	} else {
		// send HTTP 400 error
		return ctx.request_error('query parameter "name" must be present!')
	}
}
```

#### Destroying data / logging out

If a user logs out you can use the `logout` method to destroy the session data and
clear the session id cookie. If you only want to destroy the session data use the `destroy`
method.

**Example:**

```v ignore
pub fn (mut app App) logout(mut ctx Context) veb.Result {
	app.sessions.logout(mut ctx) or { return ctx.server_error('could not logout, please try again') }
	return ctx.text('You are now logged out!')
}
```

### Configuration

Change the `cookie_options` field to modify how the session cookie is stored.

**Example:**

```v ignore
mut app := &App{
	sessions: &sessions.Sessions[User]{
		// ...
		cookie_options: sessions.CookieOptions{
			// cookie can only be stored on an HTTPS site.
			secure: true
		}
	}
}
```

#### Mag-age

By default the expiration date of a session is 30 days. You can change this by
setting the `max_age` field. If `max_age = 0`, then session expiration times are not
checked and sessions will be stored forever until they are destroyed.

#### Pre-sessions

By default a session cookie is only generated when you call `save`, or `resave`.
By setting `save_uninitialized` to `true` a session cookie will always be set,
even if there is no data for the session yet. This is useful when you need session
data to be always available.

Or, for example, you could use pre-sessions to mitigate login-csrf,
since you can bind a csrf-token to the "pre-session" id. Then when the user logs
in, you can set a new session id with `resave`..

## Advanced Usage

If you want to store session id's in another manner than cookies, or if you want
to use this sessions module outside of veb, the easiest way is to create an
instance of a `Store` and directly interact with it.

First we create an instance of the `MemoryStore` and pass the user struct as data type.
**Example:**

```v
import x.sessions

const secret = 'my secret'.bytes()

pub struct User {
pub mut:
	name     string
	verified bool
}

fn main() {
	mut store := sessions.MemoryStore[User]{}

	user := User{
		name: 'vaesel'
	}
}
```

### Generating and validating session id's

The session module provides a function for generating a new signed session id
and for verifying a signed session id. You can ofcourse generate your own session id's.

**Example:**

```v ignore
// fn main
// generate a new session id and sign it
session_id, signed_session_id := sessions.new_session_id(secret)
// save session data to our store
store.set(session_id, user)!

// get a normal session id from the signed version and verify it
verified_session_id, valid := sessions.verify_session_id(signed_session_id, secret)
assert verified_session_id == session_id && valid == true
```

We can retrieve the saved user and verify that the data we saved can be retrieved
from the verified session id.

**Example:**

```v ignore
// fn main
// pass `max_age = 0` to ignore the expiration time.
if saved_user := store.get(verified_session_id, 0) {
	assert user == saved_user
	println('Retrieved a valid user! ${saved_user}')
} else {
	println(':(')
}
```

## Custom Stores

You can easily create your own custom store in order to control how session data is
stored and retrieved. Each session store needs to implement the `Store[T]` interface.

```v ignore
pub interface Store[T] {
mut:
	// get the current session data if the id exists and if it's not expired.
	// If the session is expired, any associated data should be destroyed.
	// If `max_age=0` the store will not check for expiration of the session.
	get(sid string, max_age time.Duration) !T
	// destroy session data for `sid`
	destroy(sid string) !
	// set session data for `sid`
	set(sid string, val T) !
}

// get data from all sessions, optional to implement
pub fn (mut s Store) all[T]() ![]T {
	return []T{}
}

// clear all session data, optional to implement
pub fn (mut s Store) clear[T]() ! {}
```

Only the `get`, `destroy` and `set` methods are required to implement.

### Session Expire time

The `max_age` argument in `get` can be used to check whether the session is still valid.
The database and memory store both check the expiration time from the time the session data
first inserted. But if `max_age = 0`, the stores will not check for expiration time.
