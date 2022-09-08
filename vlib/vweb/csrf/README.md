# vweb.csrf - Provides protection against Cross-site request forgery (CSRF) 
# for web apps written with vweb

## Usage

When building a csrf-protected service, first of all create a `struct`that implements `csrf.App`

```v ignore
module main

import vweb
import vweb.csrf

// embeds the csrf.App struct in order to empower the struct to protect against CSRF
struct App {
	csrf.App
}
```

Start a server e.g. in the main function.

```v ignore
fn main() {
	vweb.run_at(&App{}, vweb.RunParams{
        port: 8080
    }) or { panic(err) }
}
```

### Enable CSRF-protection

Then add a handler-function to define on which route or on which site the CSRF-Token shall be set.

```v ignore
fn (mut app App) index() vweb.Result {

    // Set a Csrf-Cookie (Token will be generated automatically)
	app.set_csrf_cookie()

	// Get the token-value from the csrf-cookie that was just setted
	token := app.get_csrf_token() or { panic(err) }

	return app.text("Csrf-Token set! It's value is: $token")
}
```

If you want to set the cookies's HttpOnly-status to false in order to make it  
 accessible to scripts on your site, you can do it like this:
`app.set_csrf_cookie(csrf.HttpOnly{false})`
If no argument is passed the value will be set to true by default.


### Protect against CSRF

If you want to protect a route or a site against CSRF just add  
`app.csrf_protect()` at the beginning of the handler-function.

```v ignore
fn (mut app App) foo() vweb.Result {
    // Protect this handler-function against CSRF
	app.csrf_protect()
	return app.text("Checked and passed csrf-guard")
}
```


