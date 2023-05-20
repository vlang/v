# Cross-Site Request Forgery (CSRF) protection



## Quick Start
Below is an example how you could use the csrf module.

**Example**:
```v oksyntax
module main

import net.http
import vweb
import vweb.csrf

struct App {
	vweb.Context
pub mut:
	csrf        csrf.CsrfApp [vweb_global]
}

// get_secret should generate a secret string based on information in the http request
// after a user logs in the secret should be bound to something that is unique from each user
fn get_secret(req http.Request) string {
	// verification of the session id should be done elsewhere
	session_id := req.cookies['session_id'] or { '' }
	user_id := get_user_id(session_id) or { '' }
	// change 'my-secret' and update it regularly in production
	secret := '${user_id}:my-secret'
	return secret
}

fn main() {
	app := &App{
		csrf: csrf.CsrfApp{
			get_secret: get_secret
			// change to which domains you want to allow
			allowed_hosts: ['*']
		}
	}
	vweb.run(app, 8080)
}

pub fn (mut app App) index() vweb.Result {
	app.csrf.set_token(mut app.Context)
	return $vweb.html()
}

[post]
pub fn (mut app App) auth() vweb.Result {
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