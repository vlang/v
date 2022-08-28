# vweb - the V Web Server

A simple yet powerful web server with built-in routing, parameter handling, templating, and other
features.
The [gitly](https://gitly.org/) site is based on vweb.

**_Some features may not be complete, and have some bugs._**

## Features

- **Very fast** performance of C on the web.
- **Small binary** hello world website is <100 KB.
- **Easy to deploy** just one binary file that also includes all templates. No need to install any
  dependencies.
- **Templates are precompiled** all errors are visible at compilation time, not at runtime.

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

['/']
pub fn (mut app App) page_home() vweb.Result {
	// all this constants can be accessed by src/templates/page/home.html file.
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
  <header>
    <title>${page_title}</title>
    @css 'src/templates/page/home.css'
  </header>
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

V suport some [Template directives](/vlib/v/TEMPLATES.md) like
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
["/foo"]
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
[post]
fn (mut app App) world() vweb.Result {
	return app.text('World')
}

['/product/create'; post]
fn (mut app App) create_product() vweb.Result {
	return app.text('product')
}
```

#### - Parameters

Parameters are passed direcly in endpoint route using colon sign `:` and received using the same
name at function
To pass a parameter to an endpoint, you simply define it inside an attribute, e. g.
`['/hello/:user]`.
After it is defined in the attribute, you have to add it as a function parameter.

**Example:**

```v ignore
          vvvv
['/hello/:user']            vvvv
fn (mut app App) hello_user(user string) vweb.Result {
	return app.text('Hello $user')
}
```

You have access to the raw request data such as headers
or the request body by accessing `app` (which is `vweb.Context`).
If you want to read the request body, you can do that by calling `app.req.data`.
To read the request headers, you just call `app.req.header` and access the
header you want example. `app.req.header.get(.content_type)`. See `struct Header`
for all available methods (`v doc net.http Header`).
It has, too, fields for the `query`, `form`, `files`.

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

['/user'; get]
pub fn (mut app App) controller_get_user_by_id() vweb.Result {
	// http://localhost:3000/user?q=vpm&order_by=desc => { 'q': 'vpm', 'order_by': 'desc' }
	return app.text(app.query.str())
}
```
### Middleware

V haven't a well defined middleware.
For now, you can use `before_request()`. This method called before every request.
Probably you can use it for check user session cookie or add header
**Example:**

```v ignore
pub fn (mut app App) before_request() {
    app.user_id = app.get_cookie('id') or { '0' }
}
```

### Redirect

Used when you want be redirected to an url
**Examples:**

```v ignore
pub fn (mut app App) before_request() {
    app.user_id = app.get_cookie('id') or { app.redirect('/') }
}
```

```v ignore
['/articles'; get]
pub fn (mut app App) articles() vweb.Result {
    if !app.token {
        app.redirect('/login')
    }
    return app.text("patatoes")
}
```

### Responses

#### - set_status

Sets the response status
**Example:**

```v ignore
['/user/get_all'; get]
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
['/articles'; get]
pub fn (mut app App) articles() vweb.Result {
    articles := app.find_all_articles()
    json_result := json.encode(articles)
    return app.json(json_result)
}
```

```v ignore
['/user/create'; post]
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
['/form_echo'; post]
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
['/:user/:repo/settings']
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
['/user/get_all'; get]
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
['/upload'; post]
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
['/form_echo'; post]
pub fn (mut app App) form_echo() vweb.Result {
    app.set_content_type(app.req.header.get(.content_type) or { '' })
    return app.ok(app.form['foo'])
}
```

### Template

#### -handle_static

handle_static is used to mark a folder (relative to the current working folder) as one that
contains only static resources (css files, images etc).

If `root` is set the mount path for the dir will be in '/'

**Example:**

```v ignore
fn main() {
    mut app := &App{}
    app.serve_static('/favicon.ico', 'favicon.ico')
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

#### -serve_static

Serves a file static.
`url` is the access path on the site, `file_path` is the real path to the file, `mime_type` is the
file type

**Example:**

```v ignore
fn main() {
    mut app := &App{}
    app.serve_static('/favicon.ico', 'favicon.ico')
    app.mount_static_folder_at(os.resource_abs_path('.'), '/')
    vweb.run(app, 8081)
}
```

### Others

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
