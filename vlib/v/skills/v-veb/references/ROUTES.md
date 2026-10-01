# Routes and responses

> The basics are in the parent skill. This reference covers: how a path is
> matched, what the `:name` forms do, and choosing a response.

## The attribute

```v ignore
@[get]                                  // GET /
@['/users']                             // any method, /users
@['/users/:id']                         // one segment
@['/files/:path...']                    // the rest of the path
@['/upload'; post]                      // a method and a path
@[post; '/submit']                      // order does not matter
```

A bare `@[get]` is a method attribute: the default path for `GET` is `/`. A bare
`@[get]` on a method named `index` becomes `/`; a bare `@[get]` on `about` becomes
`/about`.

## Path parameters become arguments

A `:name` in the path is an argument **after** `ctx`, and it arrives already
decoded:

```v ignore
@['/users/:user']
pub fn (mut app App) user_endpoint(mut ctx Context, user string) veb.Result {
	return ctx.json({user: user})
}
```

Requesting `/users/alice` calls it with `user == 'alice'`.

`:name...` captures everything remaining, which is what a route-scoped middleware
uses:

```v ignore
app.Middleware.route_use('/admin/:path...', handler: check_auth)
```

## Method attributes

The full set: `get`, `post`, `put`, `patch`, `delete`, `head`, `options`.

A method plus a path, for the cases where the default path is wrong:

```v ignore
@['/upload'; post]
pub fn (mut app App) upload(mut ctx Context) veb.Result { ... }
```

## The handler signature

```v ignore
pub fn (mut app App) name(mut ctx Context) veb.Result
pub fn (app &App) name(mut ctx Context) veb.Result      // read-only app
```

- `mut app App` when the handler mutates `App`.
- `app &App` when it only reads. Prefer this: it states the intent and lets the
  compiler reject an accidental mutation.
- `mut ctx Context` is the request. `ctx` is per-request, so anything on it needs
  no lock.
- Return `veb.Result`, which is what the `ctx` helpers return.

Path parameters go after `ctx`. Getting this wrong is a compile error, not a
runtime one.

## Responses

| Want | Write |
| --- | --- |
| Plain text | `return ctx.text('hello')` |
| JSON | `return ctx.json(payload)` |
| Pretty JSON | `return ctx.json_pretty(payload)` |
| A 404 | `return ctx.not_found()` |
| Another status | `ctx.res.set_status(.bad_request)` then return a body |
| A redirect | `return ctx.redirect('/login')` |
| The rendered templates | `return $veb.html()` |
| One template file | `return $veb.html('page.html')` |
| A template in memory | `return ctx.html($tmpl('templates/index.html'))` |

Watch the name collision: **`ctx.error(s)` is not an HTTP error.** It sets a form
validation message on `ctx.form_error` and prints to stderr. For a response with a
status code use `ctx.not_found()`, or set `ctx.res.set_status(...)` and return a
body.

`ctx.json` is generic, so it serialises structs and maps alike:

```v ignore
struct User {
	name string
	age  int
}

@['/users/:id']
pub fn (mut app App) show(mut ctx Context, id string) veb.Result {
	return ctx.json(User{
		name: id
		age:  30
	})
}
```

Note that the struct needs `pub` fields for `json` to see them. A field that must
not be serialised is left out of the struct rather than filtered afterwards.

## Templates

`$veb.html()` renders the `templates/` directory; the argument names one file.
`$tmpl` renders a string, which is how a page is composed into a layout:

```v ignore
@['/']
pub fn (app &App) index(mut ctx Context) veb.Result {
	content := $tmpl('templates/index.html')
	base := $tmpl('templates/base.html')
	return ctx.html(base)
}
```

`$veb.html()` also enables live reload in debug builds, so during development
prefer it over naming a file.

## Reading the request

```v ignore
// Query string: /search?q=hello&n=3
q := ctx.query['q'] or { '' }

// Form body, for a POST
name := ctx.form['name'] or { '' }

// Uploaded files, for a multipart POST
file := ctx.files['upload'] or { [] }[0]

// Raw request, for anything else
println(ctx.req.method)
println(ctx.req.url)
```

`ctx.query`, `ctx.form` and `ctx.files` are `map[string]string`,
`map[string]string` and `map[string][]http.FileData`. A key that was not sent is
absent from the map, so unwrap at the boundary with `or { ... }` and treat the
result as a plain value. Reading a map key without `or { ... }` gives you an empty
string and no signal that the parameter was missing.

## Errors

`ctx.error` is for form validation, not for a response. For an error response,
return a status and a body:

```v ignore
@['/users/:id']
pub fn (mut app App) show(mut ctx Context, id string) veb.Result {
	user := load_user(id) or {
		ctx.res.set_status(.not_found)
		return ctx.text('no such user: ${id}')
	}
	return ctx.json(user)
}
```

`ctx.not_found()` is the shorthand when the status is genuinely 404.

## Before_request

For something that must run on every request, a `before_request` method on
`Context` is the hook:

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

Keep it cheap. It runs on every request, and anything slow there is a slow server.
Middleware is usually the better home for anything non-trivial.

## Validation

```bash
v -check -shared path/to/veb_app.v
v fmt -verify path/to/veb_app.v
v run path/to/main.v
```

`-check` catches a malformed handler signature and a path parameter that does not
match the attribute, which are the two mistakes that are otherwise only visible as
404s.