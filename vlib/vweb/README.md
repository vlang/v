# DEPRECATED see instead [veb](https://github.com/vlang/v/tree/master/vlib/veb)

# vweb - the V Web Server

A simple yet powerful web server with built-in routing, parameter handling, templating, and other
features.
The [gitly](https://gitly.org/) site is based on vweb.

**_Some features may not be complete, and have some bugs._**

## Quick Start
Just run **`v new --web <name>`** in your terminal.

Run your vweb app with a live reload via `v -d vweb_livereload watch run .`

Now modifying any file in your web app (whether it's a .v file with the backend logic
or a compiled .html template file) will result in an instant refresh of your app
in the browser. No need to quit the app, rebuild it, and refresh the page in the browser!


## Features

- **Very fast** performance of C on the web.
- **Small binary** hello world website is <100 KB.
- **Templates are precompiled** all errors are visible at compilation time, not at runtime.
- **Multithreaded** by default
- **Easy to deploy** just one binary file that also includes all templates. No need to install any
  dependencies.

### Examples

There are some examples
that can be explored [here](https://github.com/vlang/v/tree/master/examples/vweb).

And others like:

- [vweb_orm_jwt](https://github.com/vlang/v/tree/master/examples/vweb_orm_jwt) (back-end)
- [vorum](https://github.com/vlang/vorum) (front-end)
- [gitly](https://github.com/vlang/gitly) (full-stack)

**Front-end getting start example**
`src/main.v`

```v ignore
module main

import vweb
import os

struct App {
	vweb.Context
}

struct Object {
	title       string
	description string
}

fn main() {
	vweb.run_at(new_app(), vweb.RunParams{
		port: 8081
	}) or { panic(err) }
}

fn new_app() &App {
	mut app := &App{}
	// makes all static files available.
	app.mount_static_folder_at(os.resource_abs_path('.'), '/')
	return app
}

@['/']
pub fn (mut app App) page_home() vweb.Result {
	// all these constants can be accessed by src/templates/page/home.html file.
	page_title := 'V is the new V'
	v_url := 'https://github.com/vlang/v'

	list_of_object := [
		Object{
			title: 'One good title'
			description: 'this is the first'
		},
		Object{
			title: 'Other good title'
			description: 'more one'
		},
	]
	// $vweb.html() in `<folder>_<name> vweb.Result ()` like this
	// render the `<name>.html` in folder `./templates/<folder>`
	return $vweb.html()
}

```

`$vweb.html()` compiles an HTML template into V during compilation, and embeds the resulting code
into the current action.

That means that the template automatically has access to that action's entire environment.

`src/templates/page/home.html`

```html
<html>
  <head>
    <title>${page_title}</title>
    @css 'src/templates/page/home.css'
  </head>
  <body>
    <h1 class="title">Hello, Vs.</h1>
    @for var in list_of_object
    <div>
      <a href="${v_url}">${var.title}</a>
      <span>${var.description}</span>
    </div>
    @end
    <div>@include 'component.html'</div>
  </body>
</html>
```

`src/templates/page/component.html`

```html
<div>This is a component</div>
```

`src/templates/page/home.css`

```css
h1.title {
  font-family: Arial, Helvetica, sans-serif;
  color: #3b7bbf;
}
```

V supports [Template directives](/vlib/v/TEMPLATES.md) like
`@css`, `@js` for static files in \<path\>
`@if`, `@for` for conditional and loop
and
`@include` to include html components.

## Deploying vweb apps

Everything, including HTML templates, is in one binary file. That's all you need to deploy.

## Getting Started

To start with vweb, you have to import the module `vweb` and define a struct to hold vweb.Context
(and any other variables your program will need).
The web server can be started by calling `vweb.run(&App{}, port)` or `vweb.run(&App{}, RunParams)`

**Example:**

```v ignore
import vweb

struct App {
    vweb.Context
}

fn main() {
	vweb.run(&App{}, 8080)
	// // or
	// vweb.run_at(new_app(), vweb.RunParams{
	// 	host: 'localhost'
	// 	port: 8099
	// 	family: .ip
	// }) or { panic(err) }
}
```

### Defining endpoints

To add endpoints to your web server, you have to extend the `App` struct.
For routing you can either use auto-mapping of function names or specify the path as an attribute.
The function expects a response of the type `vweb.Result`.

**Example:**

```v ignore
// This endpoint can be accessed via http://localhost:port/hello
fn (mut app App) hello() vweb.Result {
	return app.text('Hello')
}

// This endpoint can be accessed via http://localhost:port/foo
@["/foo"]
fn (mut app App) world() vweb.Result {
	return app.text('World')
}
```

#### - HTTP verbs

To use any HTTP verbs (or methods, as they are properly called),
such as `[post]`, `[get]`, `[put]`, `[patch]` or `[delete]`
you can simply add the attribute before the function definition.

**Example:**

```v ignore
@[post]
fn (mut app App) world() vweb.Result {
	return app.text('World')
}

@['/product/create'; post]
fn (mut app App) create_product() vweb.Result {
	return app.text('product')
}
```

#### - Parameters

Parameters are passed directly in the endpoint route using a colon sign `:` and received
using the same name in the function.
To pass a parameter to an endpoint, you simply define it inside an attribute, e. g.
`['/hello/:user]`.
After it is defined in the attribute, you have to add it as a function parameter.

**Example:**

```v ignore
          vvvv
@['/hello/:user']            vvvv
fn (mut app App) hello_user(user string) vweb.Result {
	return app.text('Hello $user')
}
```

You have access to the raw request data such as headers
or the request body by accessing `app` (which is `vweb.Context`).
If you want to read the request body, you can do that by calling `app.req.data`.
To read the request headers, you just call `app.req.header` and access the
header you want, for example `app.req.header.get(.content_type)`. See `struct Header`
for all available methods (`v doc net.http Header`).
It also has fields for the `query`, `form`, and `files`.

#### - Parameter Arrays

If you want multiple parameters in your route and if you want to parse the parameters 
yourself, or you want a wildcard route, you can add `...`  after the `:` and name,
e.g. `['/:path...']`.

This will match all routes after `'/'`. For example the url `/path/to/test` would give
`path = '/path/to/test'`.

```v ignore
        vvv
@['/:path...']             vvvv
fn (mut app App) wildcard(path string) vweb.Result {
	return app.text('URL path = "${path}"')
}
```

#### - Query
To handle the query context, you just need use the  `query` field

**Example:**

```v
module main

import vweb

struct App {
	vweb.Context
}

fn main() {
	vweb.run(&App{}, 8081)
}

@['/user'; get]
pub fn (mut app App) controller_get_user_by_id() vweb.Result {
	// http://localhost:3000/user?q=vpm&order_by=desc => { 'q': 'vpm', 'order_by': 'desc' }
	return app.text(app.query.str())
}
```
#### - Host
To restrict an endpoint to a specific host, you can use the `host` attribute
followed by a colon `:` and the host name. You can test the Host feature locally
by adding a host to the "hosts" file of your device.

**Example:**

```v ignore
@['/'; host: 'example.com']
pub fn (mut app App) hello_web() vweb.Result {
	return app.text('Hello World')
}

@['/'; host: 'api.example.org']
pub fn (mut app App) hello_api() vweb.Result {
	return app.text('Hello API')
}

// define the handler without a host attribute last if you have conflicting paths.
@['/']
pub fn (mut app App) hello_others() vweb.Result {
	return app.text('Hello Others')
}
```

You can also [create a controller](#hosts) to handle all requests from a specific
host in one app.

### Middleware

Vweb has different kinds of middleware.
The `before_request()` method is always called before every request before any
other middleware is processed. You could use it to check user session cookies or to add a header.

**Example:**

```v ignore
pub fn (mut app App) before_request() {
    app.user_id = app.get_cookie('id') or { '0' }
}
```

Middleware functions can be passed directly when creating an App instance and is
executed when the url starts with the defined key.

In the following example, if a user navigates to `/path/to/test` the middleware
is executed in the following order: `middleware_func`, `other_func`, `global_middleware`.
The middleware is executed in the same order as they are defined and if any function in
the chain returns `false` the propagation is stopped.

**Example:**
```v
module main

import vweb

struct App {
	vweb.Context
	middlewares map[string][]vweb.Middleware
}

fn new_app() &App {
	mut app := &App{
		middlewares: {
			// chaining is allowed, middleware will be evaluated in order
			'/path/to/': [middleware_func, other_func]
			'/':         [global_middleware]
		}
	}

	// do stuff with app
	// ...
	return app
}

fn middleware_func(mut ctx vweb.Context) bool {
	// ...
	return true
}

fn other_func(mut ctx vweb.Context) bool {
	// ...
	return true
}

fn global_middleware(mut ctx vweb.Context) bool {
	// ...
	return true
}
```

Middleware functions will be of type `vweb.Middleware` and are not methods of App,
so they could also be imported from other modules.
```v ignore
pub type Middleware = fn (mut Context) bool
```

Middleware can also be added to route specific functions via attributes.

**Example:**
```v ignore
@[middleware: check_auth]
@['/admin/data']
pub fn (mut app App) admin() vweb.Result {
	// ...
}

// check_auth is a method of App, so we don't need to pass the context as parameter.
pub fn (mut app App) check_auth () bool {
	// ...
	return true
}
```
You can only add 1 middleware to a route specific function via attributes.

#### Middleware evaluation order
The middleware is executed in the following order:

1. `before_request`
2. The middleware in `app.middlewares`
3. The middleware in the `[middleware]` attribute

If any function of step 2 or 3 returns `false` the middleware functions that would
come after it are not executed and the app handler will also not be executed. You
can think of it as a chain.

### Context values

You can store a value pair in vweb's context. It is especially useful for passing variables
from a middleware function to the route handler.

**Example**:
```v oksyntax
module main

import vweb

struct App {
	vweb.Context
	middlewares map[string][]vweb.Middleware
}

pub fn (mut app App) index() vweb.Result {
	// get the user or return HTTP 401
	user := app.get_value[User]('user') or {
		app.set_status(401, '')
		return app.text('HTTP 401: Unauthorized')
	}

	return app.text('welcome ${user.name}')
}

fn main() {
	vweb.run(&App{
		middlewares: {
			'/': [get_session]
		}
	}, 8080)
}

struct User {
	session_id string
	name       string
}

fn get_session(mut ctx vweb.Context) bool {
	// implement your own logic to get the user
	user := User{
		session_id: '123456'
		name:       'Vweb'
	}

	// set the user
	ctx.set_value('user', user)
	return true
}
```

When you visit the index page the middleware function `get_session` will run first
This function sets a `User` value to a key `'user'`.
We get this key in `index` and display it to the user if the `'user'` key exists.

#### Changing Context values

By default context values are immutable when retrieved with `get_value`. If you want to
change the value later you have to set it again with `set_value`.

**Example:**
```v ignore
fn change_user(mut ctx vweb.Context) bool {
	user := User{
		session_id: '654321'
		name: 'tester'
	}

	// set the user
	ctx.set_value('user', user)
	return true
}
```

### Redirect

Used when you want to be redirected to an url

**Examples:**

```v ignore
pub fn (mut app App) before_request() {
	app.user_id = app.get_cookie('id') or { app.redirect('/') }
}
```

```v ignore
@['/articles'; get]
pub fn (mut app App) articles() vweb.Result {
	if !app.token {
		app.redirect('/login')
	}
	return app.text('patatoes')
}
```

You can also combine middleware and redirect.

**Example:**

```v ignore
@[middleware: with_auth]
@['/admin/secret']
pub fn (mut app App) admin_secret() vweb.Result {
	// this code should never be reached
	return app.text('secret')
}

@['/redirect']
pub fn (mut app App) with_auth() bool {
	app.redirect('/auth/login')
	return false
}
```

### Custom not found page
You can implement a `not_found` route that is called when a request is made and no
matching route is found to replace the default HTTP 404 not found page.

**Example:**

``` v ignore
pub fn (mut app App) not_found() vweb.Result {
	app.set_status(404, 'Not Found')
	return app.html('<h1>Page not found</h1>')
}
```

### Databases
The `db` field in a vweb app is reserved for database connections. The connection is
copied to each new request.

**Example:**

```v
module main

import vweb
import db.sqlite

struct App {
	vweb.Context
mut:
	db sqlite.DB
}

fn main() {
	// create the database connection
	mut db := sqlite.connect('db')!

	vweb.run(&App{
		db: db
	}, 8080)
}
```

### Multithreading
By default, a vweb app is multithreaded, that means that multiple requests can
be handled in parallel by using multiple CPU's: a worker pool. You can
change the number of workers (maximum allowed threads) by altering the `nr_workers`
option. The default behaviour is to use the maximum number of jobs (cores in most cases).

**Example:**
```v ignore
fn main() {
	// assign a maximum of 4 workers
	vweb.run_at(&App{}, nr_workers: 4)
}
```

#### Database Pool
A single connection database works fine if you run your app with 1 worker, of if
you access a file-based database like a sqlite file.

This approach will fail when using a non-file based database connection like a mysql
connection to another server somewhere on the internet. Multiple threads would need to access
the same connection at the same time.

To resolve this issue, you can use the vweb's built-in database pool. The database pool
will keep a number of connections open when the app is started and each worker is
assigned its own connection.

Let's look how we can improve our previous example with database pooling and using a
postgresql server instead.

**Example:**
```v
module main

import vweb
import db.pg

struct App {
	vweb.Context
	db_handle vweb.DatabasePool[pg.DB]
mut:
	db pg.DB
}

fn get_database_connection() pg.DB {
	// insert your own credentials
	return pg.connect(user: 'user', password: 'password', dbname: 'database') or { panic(err) }
}

fn main() {
	// create the database pool and pass our `get_database_connection` function as handler
	pool := vweb.database_pool(handler: get_database_connection)

	// no need to set the `db` field
	vweb.run(&App{
		db_handle: pool
	}, 8080)
}
```

If you don't use the default number of workers (`nr_workers`) you have to change
it to the same number in `vweb.run_at` as in `vweb.database_pool`

### Extending the App struct with `[vweb_global]`
You can change your `App` struct however you like, but there are some things you
have to keep in mind. Under the hood at each request a new instance of `App` is
constructed, and all fields are re-initialized with their default type values,
except for the `db` field.

This behaviour ensures that each request is treated equally and in the same context, but
problems arise when we want to provide more context than just the default `vweb.Context`.

Let's view the following example where we want to provide a secret token to our app:

```v
module main

import vweb

struct App {
	vweb.Context
	secret string
}

fn main() {
	vweb.run(&App{
		secret: 'my secret'
	}, 8080)
}

fn (mut app App) index() vweb.Result {
	return app.text('My secret is: ${app.secret}')
}
```

When you visit `localhost:8080/` you would expect to see the text
`"My secret is: my secret"`, but instead there is only the text
`"My secret is: "`. This is because of the way vweb works. We can override the default
behaviour by adding the attribute `[vweb_global]` to the `secret` field.

**Example:**
```v ignore
struct App {
	vweb.Context
	secret string [vweb_global]
}
```

Now if you visit `localhost:8080/` you see the text `"My secret is: my secret"`.
> **Note**: the value of `secret` gets initialized with the provided value when creating
> `App`. If you would modify `secret` in one request the value won't be changed in the
> next request. You can use shared fields for this.

### Shared Objects across requests
We saw in the previous section that we can persist data across multiple requests,
but what if we want to be able to mutate the data? Since vweb works with threads,
we have to use `shared` fields.

Let's see how we can add a visitor counter to our `App`.

**Example:**
```v
module main

import vweb

struct Counter {
pub mut:
	count int
}

struct App {
	vweb.Context
mut:
	counter shared Counter // shared fields can only be structs, arrays or maps.
}

fn main() {
	// initialize the shared object
	shared counter := Counter{
		count: 0
	}

	vweb.run(&App{
		counter: counter
	}, 8080)
}

fn (mut app App) index() vweb.Result {
	mut count := 0
	// lock the counter so we can modify it
	lock app.counter {
		app.counter.count += 1
		count = app.counter.count
	}
	return app.text('Total visitors: ${count}')
}
```

#### Drawback of Shared Objects
The drawback of using shared objects is that it affects performance. In the previous example
`App.counter` needs to be locked each time the page is loaded if there are simultaneous
requests the next requests will have to wait for the lock to be released.

It is best practice to limit the use of shared objects as much as possible.

### Controllers
Controllers can be used to split up app logic so you are able to have one struct
per `"/"`.  E.g. a struct `Admin` for urls starting with `"/admin"` and a struct `Foo`
for urls starting with `"/foo"`

**Example:**
```v
module main

import vweb

struct App {
	vweb.Context
	vweb.Controller
}

struct Admin {
	vweb.Context
}

struct Foo {
	vweb.Context
}

fn main() {
	mut app := &App{
		controllers: [
			vweb.controller('/admin', &Admin{}),
			vweb.controller('/foo', &Foo{}),
		]
	}
	vweb.run(app, 8080)
}
```

You can do everything with a controller struct as with a regular `App` struct.
The only difference being is that only the main app that is being passed to `vweb.run`
is able to have controllers. If you add `vweb.Controller` on a controller struct it
will simply be ignored.

#### Routing
Any route inside a controller struct is treated as a relative route to its controller namespace.

```v ignore
@['/path']
pub fn (mut app Admin) path vweb.Result {
    return app.text('Admin')
}
```
When we created the controller with `vweb.controller('/admin', &Admin{})` we told
vweb that the namespace of that controller is `"/admin"` so in this example we would
see the text `"Admin"` if we navigate to the url `"/admin/path"`.

Vweb doesn't support fallback routes or duplicate routes, so if we add the following
route to the example the code will produce an error.

```v ignore
@['/admin/path']
pub fn (mut app App) admin_path vweb.Result {
    return app.text('Admin overwrite')
}
```
There will be an error, because the controller `Admin` handles all routes starting with
`"/admin"`; the method `admin_path` is unreachable.

#### Hosts
You can also set a host for a controller. All requests coming from that host will be handled
by the controller.

**Example:**
```v
module main

import vweb

struct App {
	vweb.Context
	vweb.Controller
}

pub fn (mut app App) index() vweb.Result {
	return app.text('App')
}

struct Example {
	vweb.Context
}

// You can only access this route at example.com: http://example.com/
pub fn (mut app Example) index() vweb.Result {
	return app.text('Example')
}

fn main() {
	vweb.run(&App{
		controllers: [
			vweb.controller_host('example.com', '/', &Example{}),
		]
	}, 8080)
}
```

#### Databases and `[vweb_global]` in controllers

Fields with `[vweb_global]` have to passed to each controller individually.
The `db` field is unique and will be treated as a `vweb_global` field at all times.

**Example:**
```v
module main

import vweb
import db.sqlite

struct App {
	vweb.Context
	vweb.Controller
mut:
	db sqlite.DB
}

struct Admin {
	vweb.Context
mut:
	db sqlite.DB
}

fn main() {
	mut db := sqlite.connect('db')!

	mut app := &App{
		db:          db
		controllers: [
			vweb.controller('/admin', &Admin{
				db: db
			}),
		]
	}
}
```

#### Using a database pool

**Example:**
```v
module main

import vweb
import db.pg

struct App {
	vweb.Context
	vweb.Controller
	db_handle vweb.DatabasePool[pg.DB]
mut:
	db pg.DB
}

struct Admin {
	vweb.Context
	db_handle vweb.DatabasePool[pg.DB]
mut:
	db pg.DB
}

fn get_database_connection() pg.DB {
	// insert your own credentials
	return pg.connect(user: 'user', password: 'password', dbname: 'database') or { panic(err) }
}

fn main() {
	// create the database pool and pass our `get_database_connection` function as handler
	pool := vweb.database_pool(handler: get_database_connection)

	mut app := &App{
		db_handle:   pool
		controllers: [
			vweb.controller('/admin', &Admin{
				db_handle: pool
			}),
		]
	}
}
```

### Responses

#### - set_status

Sets the response status
**Example:**

```v ignore
@['/user/get_all'; get]
pub fn (mut app App) controller_get_all_user() vweb.Result {
    token := app.get_header('token')

    if !token {
        app.set_status(401, '')
        return app.text('Not valid token')
    }

    response := app.service_get_all_user() or {
        app.set_status(400, '')
        return app.text('$err')
    }
    return app.json(response)
}
```

#### - html

Response HTTP_OK with payload with content-type `text/html`
**Example:**

```v ignore
pub fn (mut app App) html_page() vweb.Result {
    return app.html('<h1>ok</h1>')
}
```

#### - text

Response HTTP_OK with payload with content-type `text/plain`
**Example:**

```v ignore
pub fn (mut app App) simple() vweb.Result {
    return app.text('A simple result')
}
```

#### - json

Response HTTP_OK with payload with content-type `application/json`
**Examples:**

```v ignore
@['/articles'; get]
pub fn (mut app App) articles() vweb.Result {
    articles := app.find_all_articles()
    json_result := json.encode(articles)
    return app.json(json_result)
}
```

```v ignore
@['/user/create'; post]
pub fn (mut app App) controller_create_user() vweb.Result {
    body := json.decode(User, app.req.data) or {
        app.set_status(400, '')
        return app.text('Failed to decode json, error: $err')
	}

    response := app.service_add_user(body.username, body.password) or {
        app.set_status(400, '')
        return app.text('error: $err')
    }

    return app.json(response)
}
```

#### - json_pretty

Response HTTP_OK with a pretty-printed JSON result
**Example:**

```v ignore
fn (mut app App) time_json_pretty() {
    app.json_pretty({
        'time': time.now().format()
    })
}
```

#### - file

Response HTTP_OK with file as payload

#### - ok

Response HTTP_OK with payload
**Example:**

```v ignore
@['/form_echo'; post]
pub fn (mut app App) form_echo() vweb.Result {
    app.set_content_type(app.req.header.get(.content_type) or { '' })
    return app.ok(app.form['foo'])
}
```

#### - server_error

Response a server error
**Example:**

```v ignore
fn (mut app App) sse() vweb.Result {
    return app.server_error(501)
}
```

#### - not_found

Response HTTP_NOT_FOUND with payload
**Example:**

```v ignore
@['/:user/:repo/settings']
pub fn (mut app App) user_repo_settings(username string, repository string) vweb.Result {
    if username !in known_users {
        return app.not_found()
    }
    return app.html('username: $username | repository: $repository')
}
```

### Requests

#### - get_header

Returns the header data from the key
**Example:**

```v ignore
@['/user/get_all'; get]
pub fn (mut app App) controller_get_all_user() vweb.Result {
    token := app.get_header('token')
    return app.text(token)
}
```

#### - get_cookie

Sets a cookie
**Example:**

```v ignore
pub fn (mut app App) before_request() {
    app.user_id = app.get_cookie('id') or { '0' }
}
```

#### - add_header

Adds an header to the response with key and val
**Example:**

```v ignore
@['/upload'; post]
pub fn (mut app App) upload() vweb.Result {
    fdata := app.files['upfile']

    data_rows := fdata[0].data.split('\n')

    mut output_data := ''

    for elem in data_rows {
        delim_row := elem.split('\t')
        output_data += '${delim_row[0]}\t${delim_row[1]}\t'
        output_data += '${delim_row[0].int() + delim_row[1].int()}\n'
	}

    output_data = output_data.all_before_last('\n')

    app.add_header('Content-Disposition', 'attachment; filename=results.txt')
    app.send_response_to_client('application/octet-stream', output_data)

    return $vweb.html()
}
```

#### - set_cookie

Sets a cookie
**Example:**

```v ignore
pub fn (mut app App) cookie() vweb.Result {
    app.set_cookie(name: 'cookie', value: 'test')
    return app.text('Response Headers\n$app.header')
}
```

#### - set_cookie_with_expire_date

Sets a cookie with a `expire_data`
**Example:**

```v ignore
pub fn (mut app App) cookie() vweb.Result {
    key := 'cookie'
    value := 'test'
    duration := time.Duration(2 * time.minute ) // add 2 minutes
    expire_date := time.now().add(duration)

    app.set_cookie_with_expire_date(key, value, expire_date)
    return app.text('Response Headers\n$app.header')
}
```

#### - set_content_type

Sets the response content type
**Example:**

```v ignore
@['/form_echo'; post]
pub fn (mut app App) form_echo() vweb.Result {
    app.set_content_type(app.req.header.get(.content_type) or { '' })
    return app.ok(app.form['foo'])
}
```

### Template

#### -handle_static

handle_static is used to mark a folder (relative to the current working folder) as one that
contains only static resources (css files, images etc).\
host_handle_static can be used to limit the static resources to a specific host.

If `root` is set the mount path for the dir will be in '/'

**Example:**

```v ignore
fn main() {
    mut app := &App{}
    app.serve_static('/favicon.ico', 'favicon.ico')
    // app.host_serve_static('localhost', '/favicon.ico', 'favicon.ico')
    // Automatically make available known static mime types found in given directory.
    os.chdir(os.dir(os.executable()))?
    app.handle_static('assets', true)
    vweb.run(app, port)
}
```

#### -mount_static_folder_at

makes all static files in `directory_path` and inside it, available at http://server/mount_path.

For example: suppose you have called .mount_static_folder_at('/var/share/myassets', '/assets'),
and you have a file /var/share/myassets/main.css .
=> That file will be available at URL: http://server/assets/main.css .

mount_static_folder_at can be used to limit the static resources to a specific host.

#### -serve_static

Serves a file static.
`url` is the access path on the site, `file_path` is the real path to the file, `mime_type` is the
file type

host_serve_static can be used to limit the static resources to a specific host.

**Example:**

```v ignore
fn main() {
    mut app := &App{}
    app.serve_static('/favicon.ico', 'favicon.ico')
    // app.host_serve_static('localhost', /favicon.ico', 'favicon.ico')
    app.mount_static_folder_at(os.resource_abs_path('.'), '/')
    vweb.run(app, 8081)
}
```

### Others

#### -user_agent

Returns the user-agent from the current user

**Example:**

```v ignore
pub fn (mut app App) user_agent() vweb.Result {
    ua := app.user_agent()
    return app.text('User-Agent: $ua')
}
```

#### -ip

Returns the ip address from the current user

**Example:**

```v ignore
pub fn (mut app App) ip() vweb.Result {
    ip := app.ip()
    return app.text('ip: $ip')
}
```

#### -error

Set a string to the form error

**Example:**

```v ignore
pub fn (mut app App) error() vweb.Result {
    app.error('here as an error')
    println(app.form_error) //'vweb error: here as an error'
}
```
# Cross-Site Request Forgery (CSRF) protection

Vweb has built-in csrf protection. Go to the [csrf module](csrf/) to learn how
you can protect your app against CSRF.