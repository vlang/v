# Middleware, shared state and static files

> The basics are in the parent skill. This reference covers: the middleware chain,
> `shared` and locking, and serving files.

## The middleware chain

`App` embeds `veb.Middleware[Context]`, which gives it the chain. Handlers are
registered in `main` before `veb.run`.

```v ignore
import veb

const http_port = 8080

pub struct App {
	veb.Middleware[Context]
}

fn main() {
	mut app := &App{}
	app.Middleware.use(handler: other_func1)
	app.Middleware.use(handler: other_func2)
	app.Middleware.route_use('/admin/:path...', handler: check_auth)
	veb.run[App, Context](mut app, http_port)
}
```

Chaining is allowed and the middleware run in the order registered.

## The handler

```v ignore
fn other_func1(mut _ Context) bool {
	println('1')
	return true
}
```

- **`mut ctx Context`** — the request. Use `_` when the middleware does not need it.
- **returns `bool`** — `true` continues the chain, `false` stops it.

Returning `false` is how a middleware short-circuits: a middleware that rejects an
unauthenticated request writes the response itself and returns `false`.

```v ignore
fn check_auth(mut ctx Context) bool {
	if ctx.is_authenticated == false {
		ctx.redirect('/')
		return false      // stop: the redirect is the answer
	}
	return true
}

fn middleware_early(mut ctx Context) bool {
	ctx.text(':(')
	// false stops the chain, so the user sees ":("
	return false
}
```

Note the `return false` after `ctx.redirect` in the shape above: without it the
chain continues and the handler also runs, writing a second response.

## Global versus scoped

```v ignore
// Every request.
app.Middleware.use(handler: log_request)

// Only requests matching the path.
app.Middleware.route_use('/admin/:path...', handler: check_auth)
app.Middleware.route_use('/early', handler: middleware_early)
```

The `:path...` form is what makes a scoped rule work under a whole subtree. A
rule for `/admin` alone would not cover `/admin/secrets`.

## Middleware options

`use` and `route_use` take a `MiddlewareOptions` struct as well as a handler, which
is where a built-in behaviour goes:

```v ignore
app.Middleware.use(handler: veb.encode_gzip())
app.Middleware.use(handler: veb.encode_auto())
app.Middleware.use(handler: veb.decode_gzip())
```

`encode_auto` picks the encoding from the request, which is what a public API
wants; `encode_gzip` is the one to name when you mean it.

Keep middleware cheap and stateless. Anything slow there is a slow server, and
anything that blocks in there is a stalled one.

## Shared state

`App` is shared by every request thread. A field that is written needs `shared`,
and every write needs a lock.

```v ignore
struct State {
mut:
	cnt int
}

pub struct App {
mut:
	state shared State
}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	mut c := 0
	lock app.state {
		app.state.cnt++
		c = app.state.cnt
	}
	return ctx.text('request number: ${c}')
}
```

Read it out while still holding the lock, as above. Reading after the lock is
released is another thread's value.

For a single counter that does not need to be consistent with anything else, an
atomic avoids the lock entirely:

```v ignore
import sync.stdatomic

pub struct App {
mut:
	state shared State
}
```

See [v-concurrency](../SKILL.md) for atomics and lock discipline. The rule that
matters most here: **never hold a lock across `ctx.text`, a template render, or any
other work that can block.**

## Static files

```v ignore
// Serve a directory at a URL prefix.
app.mount_static_folder_at('/var/share/myassets', '/assets')

// Serve a directory at the site root.
app.handle_static('public', true)
```

Mount at a prefix rather than at the root when you also have routes: a root mount
claims everything that is not a route, which turns a missing asset into a
surprising 200.

veb infers the content type from the file extension. Set `ctx.content_type` when
you need to override it.

## before_request

For one piece of setup on every request, `Context` can have a `before_request`
method:

```v ignore
struct Context {
	veb.Context
}

pub fn (ctx &Context) before_request() {
	$if trace_before_request ? {
		eprintln('[veb] ${ctx.req.method} ${ctx.req.url}')
	}
}
```

The `$if` keeps the trace out of a release build. For anything that is not a
tracing aid, prefer middleware — it is easier to remove and to test.

## Testing a veb app

A veb handler is reached through the server, not by constructing a `Context`
yourself. The pattern the standard library's own tests use is: start the server on
a port, wait for it to be up, make a real request, assert on the response.

```v ignore
import net.http
import veb

const test_port = 13062

struct App {
mut:
	server  &veb.Server = unsafe { nil }
	started chan bool
}

pub fn (mut app App) init_server(server &veb.Server) {
	app.server = server
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

fn test_it_serves_the_index() {
	mut app := &App{}
	spawn veb.run_at[App, Context](mut app, veb.RunParams{
		port:                 test_port
		show_startup_message: false
		timeout_in_seconds:   1
		family:               .ip
	})
	_ := <-app.started

	resp := http.get('http://127.0.0.1:${test_port}/') or {
		assert false, 'request failed: ${err.msg()}'
		return
	}
	assert resp.status_code == 200
	assert resp.body == 'hello'
}
```

`veb.run[A, Context](mut app, port)` is the short form and takes a bare port.
`veb.run_at` takes a `RunParams` struct and is what you need for the timeout, the
address family or a non-default host.

`before_accept_loop` is the hook that signals readiness, so the test does not race
the listen loop. `vlib/veb/tests/` has working examples of each shape — routing
order, multipart uploads, middleware.

Then check the whole thing:

```bash
v -check -shared path/to/veb_app.v
v fmt -verify path/to/veb_app.v
```

A data race in a handler will not be caught by `-check`. Run the app under `-race`
if it holds shared state.