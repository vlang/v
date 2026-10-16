module veb

// url_for builds the URL for one of the app's handlers from its method name
// and the route parameters in the order the route declares them, so renaming
// a handler or reordering its parameters breaks the call site at compile
// time instead of shipping a 404. An unknown handler, or a parameter count
// that does not fit the route, is `none` rather than a half-built path.
//
// ```v
// @['/user/:id']
// pub fn (app &App) user(mut ctx Context, id int) veb.Result { ... }
//
// url := veb.url_for[App]('user', '42') or { '' } // '/user/42'
// ```
pub fn url_for[A](handler string, params ...string) ?string {
	$for method in A.methods {
		if method.name == handler {
			_, path, _ := parse_attrs(method.name, method.attrs) or { return none }
			return fill_route_params(path, params)
		}
	}
	return none
}

// fill_route_params substitutes the `:name` segments of a route path with the
// given values in order. A trailing `:name...` segment takes the rest joined
// with `/`. Too few or too many values is `none`: a URL built from the wrong
// arity is a bug, and it should fail where it is built.
fn fill_route_params(path string, params []string) ?string {
	words := path.split('/')
	mut out := []string{cap: words.len}
	mut taken := 0
	for word in words {
		if word.ends_with('...') && word.starts_with(':') {
			out << params[taken..].join('/')
			taken = params.len
		} else if word.starts_with(':') {
			if taken >= params.len {
				return none
			}
			out << params[taken]
			taken++
		} else {
			out << word
		}
	}
	if taken != params.len {
		return none
	}
	return out.join('/')
}
