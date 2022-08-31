import vweb.csrf
import vweb
import net.http

struct App {
	csrf.App
}

// test_server - Run the server.
fn test_server() {
	go server()
}

// test_request - Sending a request an check if Csrf-Cookie is set properly.
fn test_request() {
	res := http.get('http://localhost:8080/') or { panic(err) }
	assert look_for_csrf_cookie(res) == true
}

// look_for_csrf_cookie - checks if a Csrf-Token-Cookie is in the headers of the response.
fn look_for_csrf_cookie(res http.Response) bool {
	if res.header.str().contains('__Host-Csrf-Token') {
		return true
	} else {
		return false
	}
}

// test_server - function that contains the starting process that will be called in an separate task.
fn server() {
	vweb.run_at(&App{}, vweb.RunParams{
		port: 8080
	}) or { panic(err) }
}

// index - will handle requests to path '/'
fn (mut app App) index() vweb.Result {
	// Set a Csrf-Cookie(Token will be generated automatically) and set http_only-status. If no argument ist passed, it will be true by default.
	app.set_csrf_cookie(csrf.HttpOnly{false})
	// Get the token-value from the csrf-cookie that was just setted
	token := app.get_csrf_token() or { panic(err) }
	return app.text("Csrf-Token set! It's value is: $token")
}
