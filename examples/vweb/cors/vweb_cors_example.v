import time
import vweb

// See https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
// and https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS#preflighted_requests
// >  Cross-Origin Resource Sharing (CORS) is an HTTP-header based mechanism that allows
// >  a server to indicate any origins (domain, scheme, or port) other than its own from
// >  which a browser should permit loading resources...

// Usage: do `./v run examples/vweb/cors/` to start the app,
// then check the headers in another shell:
//
// 1) `curl -vvv -X OPTIONS http://localhost:45678/time`
// 2) `curl -vvv -X POST http://localhost:45678/

struct App {
	vweb.Context
}

// Browsers like Chrome use OPTIONS requests for their preflight requests.
// This route method ensures, that they will get the headers they need, to allow CORS.
@['/:path...'; options]
pub fn (mut app App) preflight(path string) vweb.Result {
	app.add_header('Access-Control-Allow-Methods', 'GET,HEAD,PUT,PATCH,POST,DELETE')
	app.add_header('Vary', 'Access-Control-Request-Headers')
	return app.text('ok')
}

// before_request will be called for each route. *Either* use the add_header here, to allow
// *all* endpoints, or add it for each specific route, including for the preflight one above.
// Note that browsers disallow using the Access-Control-Allow-Origin header more than once.
pub fn (mut app App) before_request() {
	app.add_header('Access-Control-Allow-Origin', '*')
}

// time is a simple POST request handler, that returns the current time. It should be available
// to JS scripts, running on arbitrary other origins/domains.
@[post]
pub fn (mut app App) time() vweb.Result {
	// *Either* use the .add_header line here, *or* the one in before_request, but *not both*.
	// app.add_header('Access-Control-Allow-Origin', '*')
	return app.json({
		'time': time.now().format_ss_milli()
	})
}

fn main() {
	println("
To test, if CORS works, copy this JS snippet, then go to for example https://stackoverflow.com/ , 
press F12, then paste the snippet in the opened JS console. You should see the vweb server's time:
	
var xhr = new XMLHttpRequest();
xhr.onload = function(data) {
	console.log('xhr loaded');
	console.log(xhr.response);
};
xhr.open('POST', 'http://localhost:45678/time');
xhr.send();

	")
	vweb.run(&App{}, 45678)
}
