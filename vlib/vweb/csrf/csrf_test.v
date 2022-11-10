import time
import net.http
import vweb
import vweb.csrf

const sport = 10801

struct App {
	csrf.App
}

// index - will handle requests to path '/'
fn (mut app App) index() vweb.Result {
	// Set a Csrf-Cookie(Token will be generated automatically) and set http_only-status. If no argument ist passed, it will be true by default.
	app.set_csrf_cookie(csrf.HttpOnly{false})
	// Get the token-value from the csrf-cookie that was just setted
	token := app.get_csrf_token() or { panic(err) }
	return app.text("Csrf-Token set! It's value is: $token")
}

fn test_send_a_request_to_homepage_expecting_a_csrf_cookie() {
	spawn vweb.run_at(&App{}, vweb.RunParams{ port: sport })
	time.sleep(500 * time.millisecond)
	res := http.get('http://localhost:$sport/')?
	if res.header.str().contains('__Host-Csrf-Token') {
		assert true
	} else {
		assert false
	}
}
