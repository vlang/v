# vweb - the V Web Server #

A simple yet powerful web server with built-in routing, parameter handling, templating, and other features.
The [gitly](https://gitly.org/) site is based on vweb.


***Some features may not be complete, and have some bugs.***

## Features ##

- **Very fast** performance of C on the web.
- **Small binary** hello world website is <100 KB.
- **Easy to deploy** just one binary file that also includes all templates. No need to install any dependencies.
- **Templates are precompiled** all errors are visible at compilation time, not at runtime.

### Examples ###
There are some examples that can be explored [here](https://github.com/vlang/v/tree/master/examples/vweb).

And others like:
- [vweb_orm_jwt](https://github.com/vlang/v/tree/master/examples/vweb_orm_jwt) (back-end)
- [vorum](https://github.com/vlang/vorum) (front-end)
- [gitly](https://github.com/vlang/gitly) (full-stack)


`vorum.v` contains all GET and POST actions.

```v ignore
pub fn (app mut App) index() {
	posts := app.find_all_posts()
	$vweb.html()
}

// TODO ['/post/:id/:title']
// TODO `fn (app App) post(id int)`
pub fn (app App) post() {
	id := app.get_post_id()
	post := app.retrieve_post(id) or {
		app.redirect('/')
		return
	}
	comments := app.find_comments(id)
	show_form := true
	$vweb.html()
}
```

`index.html` is an example of the V template language:

```html
@for post in posts
	<div class=post>
		<a class=topic href="@post.url">@post.title</a>
		<img class=comment-img>
		<span class=nr-comments>@post.nr_comments</span>
		<span class=time>@post.time</span>
	</div>
@end
```

`$vweb.html()` compiles an HTML template into V during compilation, and embeds the resulting code into the current action.

That means that the template automatically has access to that action's entire environment.

## Deploying vweb apps ##

Everything, including HTML templates, is in one binary file. That's all you need to deploy.

## Getting Started ##

To start with vweb, you have to import the module `vweb` and define a struct to hold vweb.Context.
The web server can be started by calling `vweb.run(&App{}, port)`.

**Example:**

```v ignore
import vweb

struct App {
    vweb.Context
}

fn main() {
	vweb.run(&App{}, 8080)
}
```

### Defining endpoints ###

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
####- HTTP verbs
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
####- Parameters
Parameters are passed direcly in endpoint route using colon sign `:` and received using the same name at function
To pass a parameter to an endpoint, you simply define it inside an attribute, e. g. `['/hello/:user]`.
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
header you want, e.g. `app.req.header.get(.content_type)`. See `struct Header`
for all available methods (`v doc net.http Header`).

### Middleware ###
V haven't a well defined middleware. 
For now, you can use `before_request()`. This method called before every request.
Probably you can use it for check user session cookie or add header
**Example:**
```v ignore
pub fn (mut app App) before_request() {
    app.user_id = app.get_cookie('id') or { '0' }
}
```
### Redirect ###
Used when you want be redirected for one page
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

### Responses ###
####- get_header
**Example:**
```v ignore
['/user/get_all'; get]
pub fn (mut app App) controller_get_all_user() vweb.Result {
    token := app.get_header('token')
    return app.text(token)
}
```
####- get_cookie
**Example:**
```v ignore

```

####- set_status
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

####- html
**Example:**
```v ignore
pub fn (mut app App) html_page() vweb.Result {
    return app.html('<h1>ok</h1>')
}
```

####- text
**Example:**
```v ignore
pub fn (mut app App) simple() vweb.Result {
    return app.text('A simple result')
}
```

####- json
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

####- json_pretty
**Example:**
```v ignore
fn (mut app App) time_json_pretty() {
    app.json_pretty({
        'time': time.now().format()
    })
}

```

####- file
**Example:**
```v ignore

```

####- ok
**Example:**
```v ignore
['/form_echo'; post]
pub fn (mut app App) form_echo() vweb.Result {
    app.set_content_type(app.req.header.get(.content_type) or { '' })
    return app.ok(app.form['foo'])
}
```

####- server_error
**Example:**
```v ignore
fn (mut app App) sse() vweb.Result {
    return app.server_error(501)
}

```

####- not_found
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

### Requests ###

####- add_header
**Example:**
```v ignore

```


####- set_cookie
**Example:**
```v ignore

```


####- set_content_type
**Example:**
```v ignore

```


####- set_cookie_with_expire_date
**Example:**
```v ignore

```

####- add_header
**Example:**
```v ignore

```
