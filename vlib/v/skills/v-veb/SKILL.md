---
name: v-veb
description: Writing V web applications with veb - the App and Context structs, route attributes with path parameters, ctx.html and ctx.json, templates with $tmpl and $veb.html, middleware with Middleware.use and route_use, shared state with shared and lock, and static file mounting. Use when creating or changing a veb server, adding a route or handler, returning JSON or HTML, adding middleware, or serving files. Does not cover concurrency in general (see v-concurrency), the language rules (see v-lang), the build loop (see v-workflow), scripting a task in V (see v-scripts), or the wider command surface (see v-tools).
license: MIT
---

# veb, V's web framework

A veb server is two structs and a `main`. Handlers are methods on `App`, and every
request runs with the whole `App` shared across threads.

## Resource Routing

- `references/ROUTES.md` - Read when adding a route, a path parameter, or a
  response body.
- `references/MIDDLEWARE.md` - Read when adding cross-cutting behaviour, shared
  state, or static files.

## Quick Reference

| Need | Do |
| --- | --- |
| A route | `@['/path']` above a `pub fn (mut app App) handler(mut ctx Context) veb.Result` |
| A path parameter | `@['/users/:id']` and an extra `id string` argument |
| A specific method | `@[post]` or `@['/upload'; post]` |
| HTML | `return $veb.html()` or `return $veb.html('page.html')` |
| JSON | `return ctx.json(payload)` |
| Plain text | `return ctx.text('...')` |
| Middleware | `app.Middleware.use(handler: f)` |
| Middleware for a path | `app.Middleware.route_use('/admin/:path...', handler: f)` |
| Shared state | `field shared T`, mutated inside `lock app.state { }` |
| Serve files | `app.mount_static_folder_at(...)` |
| Serve a folder at root | `app.handle_static(directory_path, root)` |

## The shape of a server

```v ignore
import veb

const port = 8080

pub struct App {}

struct Context {
	veb.Context
}

fn main() {
	mut app := &App{}
	veb.run[App, Context](mut app, port)
}

@['/']
pub fn (mut app App) index(mut ctx Context) veb.Result {
	return ctx.text('hello')
}

@[post]
pub fn (mut app App) submit(mut ctx Context) veb.Result {
	return ctx.json({ok: true})
}
```

Three things to notice:

- `App` is `pub` and `Context` embeds `veb.Context`. Both are type arguments to
  `veb.run`, which is how veb knows your types.
- A handler takes `mut ctx Context` and returns `veb.Result`. Return the result of
  a `ctx` method rather than constructing one.
- `mut app` in the signature means the method may mutate `App`. A read-only handler
  takes `app &App`, and veb accepts that too.

## Routes

The attribute is a list of paths. Without a leading `/`, it is an HTTP method with
the default path.

```v ignore
@['/users/:user']
pub fn (mut app App) user_endpoint(mut ctx Context, user string) veb.Result {
	return ctx.json({user: user})
}

@[post]
pub fn (mut app App) create(mut ctx Context) veb.Result { ... }

@['/upload'; post]
pub fn (mut app App) upload(mut ctx Context) veb.Result { ... }
```

A `:name` in the path becomes an argument **after** `ctx`. A `:name...` at the end
captures the rest of the path.

See `references/ROUTES.md` for matching rules and ordering.

## Shared state

`App` is shared across request threads, so a mutable field needs to say so, and
every write needs a lock:

```v ignore
pub struct App {
mut:
	state shared State
}

struct State {
mut:
	hits int
}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	lock app.state {
		app.state.hits++
	}
	return ctx.text('${app.state.hits}')
}
```

`shared` is what makes the field shared rather than per-request. Omitting it, or
writing without the `lock`, is a data race — and it is the mistake that matters
most in a veb app.

## Middleware

Middleware wraps requests. Global for the app, or scoped to a path:

```v ignore
struct App {
	veb.Middleware[Context]
}

fn main() {
	mut app := &App{}
	// Chained, evaluated in order.
	app.Middleware.use(handler: other_func1)
	app.Middleware.use(handler: add_headers)
	// Only for matching paths.
	app.Middleware.route_use('/admin/:path...', handler: check_auth)
	veb.run[App, Context](mut app, http_port)
}
```

A middleware handler takes `mut ctx Context` and returns a `bool`. Return `true` to
continue the chain; return `false` to stop it, which is how a middleware short
circuits a request.

See `references/MIDDLEWARE.md`.

## Templates

`$veb.html()` renders the templates directory; `$veb.html('name.html')` renders one
file; `$tmpl` renders a string in memory, which is how a layout is composed with a
page:

```v ignore
@['/']
pub fn (app &App) index(mut ctx Context) veb.Result {
	content := $tmpl('templates/index.html')
	base := $tmpl('templates/base.html')
	return ctx.html(base)
}
```

## Validation

A veb app is checked the same way as any V code, and `-check` catches most of it —
including a handler whose signature does not match what veb expects:

```bash
v -check -shared path/to/veb_app.v     # library-shaped: needs -shared
v fmt -verify path/to/veb_app.v
v run path/to/main.v
```

See [v-workflow](../v-workflow/SKILL.md) for the full loop.

## Related Skills

- **Concurrency**: see [v-concurrency](../v-concurrency/SKILL.md) for what
  `shared` means and the lock discipline that goes with it.
- **The build loop**: see [v-workflow](../v-workflow/SKILL.md) for `-check`,
  `-shared`, `fmt -verify` and the other flags.
- **The language rules**: see [v-lang](../v-lang/SKILL.md) for `mut` and for
  `?T` versus `!T` when a handler has to return an error.