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

To enable CSRF-protection for your veb app you must embed the `CsrfContext` struct
on your `Context` struct. You must also provide configuration options
(see [configuration & security](#configuration--security-considerations)).

**Example:**

```v
import veb
import veb.csrf

pub struct Context {
	veb.Context
	csrf.CsrfContext
}
```

Change `secret` and `allowed_hosts` in a production environment!

**Example:**

```v ignore
const csrf_config := csrf.CsrfConfig{
	secret: 'my-secret'
	allowed_hosts: ['*']
}
```

### Middleware

Enable CSRF protection for all routes, or a certain route(s) by using veb's middleware.

**Example:**

```v ignore
pub struct App {
	veb.Middleware[Context]
}

fn main() {
	mut app := &App{}
	// register the CSRF middleware and pass our configuration
	// protect a specific route
	app.route_use('/login', csrf.middleware[Context](csrf_config))
	veb.run[App, Context](mut app, 8080)
}
```

### Setting the token

For the CSRF-protection to work we have to generate an anti-CSRF token and set it
as an hidden input field on any form that will be submitted to the route we
want to protect.

**Example:**
_main.v_

```v ignore
fn (app &App) index(mut ctx) veb.Result {
	// this function will set a cookie header and generate a CSRF token
	ctx.set_csrf_token(mut ctx)
	return $veb.html()
}

@[post]
fn (app &App) login(mut ctx, password string) veb.Result {
	// implement your own password validation here
	if password == 'password' {
		return ctx.text('You are logged in!')
	} else {
		return ctx.text('Invalid password!')
	}
}
```

_templates/index.html_

```html
<h1>Log in</h1>
<form method="POST" action="/login">
	@{ctx.csrf_token_input()}
	<label for="password">Password:</label>
	<input type="text" name="password" id="password" />
	<button type="submit">Log in</button>
</form>
```

If we run the app with `v run main.v` and navigate to `http://localhost:8080/`
we will see the login form and we can login using the password "password".

If we remove the hidden input, by removing the line `@{ctx.csrf_token_input()}`
from our html code we will see an error message indicating that the CSRF token
is not set or invalid! By default the CSRF module sends an HTTP-403 response when
a token is invalid, if you want to send a custom response see the
[advanced usage](#advanced-usage) section.

> **Note:**
> Please read the security and configuration section! If you configure
> the CSRF module in an unsafe way, the protection will be useless.

## Advanced Usage

If you want more control over what routes are protected or what action you want to
do when a CSRF-token is invalid, you can call `csrf.protect` yourself whenever you want
to protect a route against CSRF attacks. This function returns `false` if the current CSRF token
and cookie combination is not valid.

**Example:**

```v ignore
@[post]
fn (app &App) login(mut ctx, password string) veb.Result {
	if csrf.protect(mut ctx, csrf_config) == false {
		// CSRF verification failed!
	}
	// ...
}
```

### Obtaining the anti-CSRF token

When `set_csrf_token` is called the token is stored in the `csrf_token` field. You access
this field directly to use it in an input field, or call `csrf_token_input`.

**Example:**

```v ignore
fn (app &App) index(mut ctx) veb.Result {
	token := ctx.set_csrf_token(mut ctx)
}
```

### Clearing the anti-CSRF token

If you want to remove the anti-CSRF token and the cookie header you can call `clear_csrf_token`

**Example:**

```v ignore
ctx.clear_csrf_token()
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
is why is it important to **not leak the CSRF-token** via an url, or some other way. This is way
by default the `HTTPOnly` flag on the cookie is set to true.
See [client side CSRF][client-side-csrf] for more information.

This is a high level overview of the implementation.

## Configuration & Security Considerations

### The secret key

The secret key should be a random string that is not easily guessable.

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
Request will now be accepted when the Origin _or_ Referer header is valid.

### Share csrf cookie with subdomains

If you need to share the CSRF-token cookie with subdomains, you can set
`same_site` to `.same_site_lax_mode`.

## Configuration

All configuration options are defined in `CsrfConfig`.

[//]: # 'Sources'
[owasp]: https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html#double-submit-cookie
[client-side-csrf]: https://cheatsheetseries.owasp.org/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.html#client-side-csrf
[mozilla-safe-methods]: https://developer.mozilla.org/en-US/docs/Glossary/Safe/HTTP
