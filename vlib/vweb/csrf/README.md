# Cross-Site Request Forgery (CSRF) protection

This module implements the [double submit cookie][owasp] technique to protect routes
from CSRF attacks.

CSRF is a type of attack that occurs when a malicious program/website (and others) causes
a user's web browser to perform an action without them knowing. A web browser automatically sends
cookies to a website when it performs a request, including session cookies. So if a user is
authenticated on your website the website can not distinguish a forged request by a legitimate
request.

## When to not add CSRF-protection
If you are creating a service that is intended to be used by other servers e.g. an API,
you probably don't want CSRF-protection. An alternative would be to send an Authorization
token in, and only in, an HTTP-header (like JSON Web Tokens). If you do that your website
isn't vulnerable to CSRF-attacks.

## Usage

You can add `CsrfApp` to your own `App` struct to have the functions available
in your app's context, or you can use it with the middleware of vweb.

The advantage of the middleware approach is that you have to define
the configuration separate from your `App`. This makes it possible to share the
configuration between modules or controllers.

### Usage with the CsrfApp

Change `secret` and `allowed_hosts` when creating the `CsrfApp`.

**Example**:
```v ignore
module main

import net.http
import vweb
import vweb.csrf

struct App {
	vweb.Context
pub mut:
	csrf csrf.CsrfApp [vweb_global]
}

fn main() {
	app := &App{
		csrf: csrf.CsrfApp{
			// change the secret
			secret: 'my-64bytes-secret'
			// change to which domains you want to allow
			allowed_hosts: ['*']
		}
	}
	vweb.run(app, 8080)
}

pub fn (mut app App) index() vweb.Result {
	// this line sets `app.token` and the cookie
	app.csrf.set_token(mut app.Context)
	return $vweb.html()
}

[post]
pub fn (mut app App) auth() vweb.Result {
	// this line protects the route against CSRF
	app.csrf.protect(mut app.Context)
	return app.text('authenticated!')
}
```

index.html
```html
<form action="/auth" method="post">
    <input type="hidden" name="@app.csrf.token_name" value="@app.csrf.token"/>
    <label for="password">Your password:</label>
    <input type="text" id="password" name="password" placeholder="Your password" />
</form>
```

### Usage without CsrfApp
If you use `vweb.Middleware` you can protect multiple routes at once.

**Example**:
```v ignore
module main

import net.http
import vweb
import vweb.csrf

// the configuration moved here
const csrf_config = csrf.CsrfConfig{
	// change the secret
	secret: 'my-64bytes-secret'
	// change to which domains you want to allow
	allowed_hosts: ['*']
}


struct App {
	vweb.Context
pub mut:
	middlewares map[string][]vweb.Middleware
}

fn main() {
	app := &App{
		middlewares: {
			// protect all routes starting with the url '/auth'
			'/auth': [csrf.middleware(csrf_config)]
		}
	}
	vweb.run(app, 8080)
}

pub fn (mut app App) index() vweb.Result {
	// get the token and set the cookie
	csrftoken := csrf.set_token(mut app.Context, csrf_config)
	return $vweb.html()
}

[post]
pub fn (mut app App) auth() vweb.Result {
	return app.text('authenticated!')
}

[post]
pub fn (mut app App) register() vweb.Result {
	// protect an individual route with the following line
	csrf.protect(mut app.Context, csrf_config)
	// ...
}
```

index.html (the hidden input has changed)
```html
<form action="/auth" method="post">
    <input type="hidden" name="@csrf_config.token_name" value="@csrftoken"/>
    <label for="password">Your password:</label>
    <input type="text" id="password" name="password" placeholder="Your password" />
</form>
```

### Protect all routes
It is possible to protect all routes against CSRF-attacks. Every request that is not
defined as a [safe method](#safe-methods) (`GET`, `OPTIONS`, `HEAD` by default)
will have CSRF-protection.

**Example**:
```v ignore
pub fn (mut app App) before_request() {
	app.csrf.protect(mut app.Context)
	// or if you don't use `CsrfApp`:
	// csrf.protect(mut app.Context, csrf_config)
}
```

## How it works
This module implements the [double submit cookie][owasp] technique: a random token
is generated, the CSRF-token. The hmac of this token and the secret key is stored in a cookie.

When a request is made, the CSRF-token should be placed inside a HTML form element.
The CSRF-token the hmac of the CSRF-token in the formdata is compared to the cookie.
If the values match, the request is accepted.

This approach has the advantage of being stateless: there is no need to store tokens on the server
side and validate them. The token and cookie are bound cryptographically to each other so
an attacker would need to know both values in order to make a CSRF-attack succeed. That
is why is it important to **not leak the CSRF-token** via an url, or some other way.
See [client side CSRF][client-side-csrf] for more information.

This is a high level overview of the implementation.

## Security Considerations

### The secret key
The secret key should be a random string that is not easily guessable.
The recommended size is 64 bytes.

### Sessions
If your app supports some kind of user sessions, it is recommended to cryptographically
bind the CSRF-token to the users' session. You can do that by providing the name
of the session ID cookie. If an attacker changes the session ID in the cookie, in the
token or both the hmac will be different and the request will be rejected.

**Example**:
```v ignore
csrf_config = csrf.CsrfConfig{
	// ...
	session_cookie: 'my_session_id_cookie_name'
}
```

### Safe Methods
The HTTP methods `GET`, `OPTIONS`, `HEAD` are considered
[safe methods][mozilla-safe-methods] meaning they should not alter the state of
an application. If a request with a "safe method" is made, the csrf protection will be skipped.

You can change which methods are considered safe by changing `CsrfConfig.safe_methods`.

### Allowed Hosts

By default, both the http Origin and Referer headers are checked and matched strictly
to the values in `allowed_hosts`. That means that you need to include each subdomain.

If the value of `allowed_hosts` contains the wildcard: `'*'` the headers will not be checked.

#### Domain name matching
The following configuration will not allow requests made from `test.example.com`,
only from `example.com`.

**Example**
```v ignore
config := csrf.CsrfConfig{
    secret: '...'
    allowed_hosts: ['example.com']
}
```

#### Referer, Origin header check
In some cases (like if your server is behind a proxy), the Origin or Referer header will
not be present. If that is your case you can set `check_origin_and_referer` to `false`.
Request will now be accepted when the Origin *or* Referer header is valid.

### Share csrf cookie with subdomains
If you need to share the CSRF-token cookie with subdomains, you can set
`same_site` to `.same_site_lax_mode`.

## Configuration

All configuration options are defined in `CsrfConfig`.

[//]: # (Sources)
[owasp]: https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html#double-submit-cookie
[client-side-csrf]: https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html#client-side-csrf
[mozilla-safe-methods]: https://developer.mozilla.org/en-US/docs/Glossary/Safe/HTTP
