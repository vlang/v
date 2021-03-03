This is pre-alpha software.

## Features
- Very fast: performance of C on the web.
- Small binary: hello world website is <100 KB.
- Easy to deploy: just one binary file that also includes all templates.
  No need to install any dependencies.
- Templates are precompiled, all errors are visible at compilation time,
  not at runtime.

Lots of things are broken and not implemented yet in V and vweb.

There's no documentation yet, have a look at a simple example:

https://github.com/vlang/v/tree/master/examples/vweb/vweb_example.v

There's also the V forum: https://github.com/vlang/vorum

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

`$vweb.html()` compiles an HTML template into V during compilation,
and embeds the resulting code in current action.

That means that the template automatically has access to that action's entire environment.


### Deploying vweb apps

Everything, including HTML templates, is in one binary file. That's all you need to deploy.

## Getting Started

To start with vweb, you have to import the module `vweb`.
After the import, define a struct to hold vweb.Context 
(and any other variables your program will need).
The web server can be started by calling `vweb.run<App>(port)`.

**Example:**
```v ignore
import vweb

struct App {
    vweb.Context
}

fn main() {
	vweb.run<App>(8080)
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

To create an HTTP POST endpoint, you simply add a `[post]` attribute before the function definition.

**Example:**
```v ignore
[post]
fn (mut app App) world() vweb.Result {
	return app.text('World')
}
```

To pass a parameter to an endpoint, you simply define it inside 
an attribute, e. g. `['/hello/:user]`.
After it is defined in the attribute, you have to add it as a function parameter.

**Example:**
```v ignore
['/hello/:user']
fn (mut app App) hello_user(user string) vweb.Result {
	return app.text('Hello $user')
}
```

You have access to the raw request data such as headers 
or the request body by accessing `app` (which is `vweb.Context`).
If you want to read the request body, you can do that by calling `app.req.data`.
To read the request headers, you just call `app.req.headers` and access the header you want, 
e.g. `app.req.headers['Content-Type']`
