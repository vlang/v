# Building a 400 KB Web Blog in V & SQLite with Veb

Hello,

In this guide, we'll build a simple web blog using V and the Veb framework.

The benefits of using V and Veb for web development:

- A safe, fast language with the development agility of Python or Ruby and the performance of C
- Zero dependencies: everything needed comes in a 1 MB package
- Very small binaries: this blog will be about 150 KB
- Easy deployments: single binary including precompiled templates
- Runs on minimal hardware: $3 instance sufficient for most apps
- Fast development with minimal boilerplate

> [!NOTE]
> V and Veb are at an early stage and evolving rapidly.

The code is available [here](./code/blog).

## Installing V
```
wget --quiet https://github.com/vlang/v/releases/latest/download/v_linux.zip
unzip v_linux.zip
cd v
sudo ./v symlink
```


V should now be globally available.

> On macOS use `v_macos.zip`, on Windows - `v_windows.zip`.
> For BSD, Solaris, Android, or source installation, see:
> https://github.com/vlang/v#installing-v-from-source

## Install SQLite Development Dependency

See [`sqlite` README](../../vlib/db/sqlite/README.md) for instructions if not already installed.

## Creating a New Veb Project

V projects can be created anywhere:

```bash
mkdir blog
cd blog
touch blog.v
```

First, let's create a simple hello world website:

```v oksyntax
// blog.v
module main

import veb

pub struct Context {
	veb.Context
}

pub struct App {}

fn main() {
	mut app := &App{}
	veb.run[App, Context](mut app, 8081)
}

pub fn (app &App) index(mut ctx Context) veb.Result {
	return ctx.text('Hello world from Veb!')
}
```

Run it with

```bash
v run blog.v
```

```
Running a Veb app on http://localhost:8081 ...
```

Veb helpfully provided a link, open http://localhost:8081/ in your browser:

<img width=662 src="https://github.com/vlang/v/blob/master/tutorials/building_a_simple_web_blog_with_veb/img/hello.png?raw=true">

The App struct holds shared application data, while Context handles per-request data and embeds
`veb.Context` for response methods like `.text()`.

If you have experiencewith an MVC web framework, you can think of it as a controller. (Veb is
not an MVC framework however.)



As you can see, there are no routing rules. The `index()` action handles the `/` request by default.
Vweb often uses convention over configuration and adding a new action requires
no routing rules either:

```v oksyntax
// blog.v
import veb
import time

fn (app &App) time(mut ctx Context) veb.Result {
	return ctx.text(time.now().format())
}
```

Custom routes can be defined using attributes like @['/index'].

<img width=662 src="https://github.com/vlang/v/blob/master/tutorials/building_a_simple_web_blog_with_veb/img/time.png?raw=true">

> TIP: run the following command to live-reload the server: `v watch run blog.v`

The `.text(string)` method returns a plain text document with the provided
text, which isn't frequently used in websites.

### HTML View

Let's return an HTML view instead. Create `index.html` in the same directory:

```html
<html>
  <head>
	<title>V Blog</title>
  </head>
  <body>
	<b>@message</b>
	<br />
	<img src="https://vlang.io/img/v-logo.png" width="100" />
  </body>
</html>
```

and update our `index()` action so that it returns the HTML view we just created:

```v ignore
// blog.v
pub fn (app &App) index(mut ctx Context) veb.Result {
	message := 'Hello, world from Vweb!'
	return $veb.html()
}
```

<img width=662 src="https://github.com/vlang/v/blob/master/tutorials/building_a_simple_web_blog_with_veb/img/hello_html.png?raw=true">

Good, now we have an actual HTML page.

The V template language is similar to C#'s Razor: `@message` prints the value
of `message`.

You may notice something unusual: the `message` variable created in the `index()`
action is automatically available in the view.

It's another feature of Vweb to reduce the boilerplate in your web apps.
No need to create view models just to pass data, or use an unsafe and untyped
alternative, like C#'s `ViewBag["message"]`.

Making all action variables available in the view may seem crazy,
but V is a language with pure functions by default, and you won't be able
to modify any data from a view. `<b>@foo.bar()</b>` will only work if the `bar()` method
doesn't modify `foo`.

The HTML template is compiled to V during the compilation of the website,
that's done by the `$veb.html()` line.
(`$` always means compile time actions in V.) offering the following benefits:

- Great performance, since the templates don't need to be compiled
  on every request, like in almost every major web framework.

- Easier deployment, since all your HTML templates are compiled
  into a single binary file together with the web application itself.

- All errors in the templates are guaranteed to be caught during compilation.

### Fetching data with V ORM

Now let's display some articles!

We'll be using V's built-in ORM and a SQLite database.
(V ORM will also support MySQL, Postgre, and SQL Server soon.)

Add a SQLite handle to `App`:

```v oksyntax
// blog.v
import db.sqlite
import veb

struct App {
	// ...
	db sqlite.DB
	// ...
}
```

In `fn main()` we'll connect to a database.
Code in the `main()` function is run only once during app's startup, so we are going
to have one DB connection for all requests. This improves the performance of the web application,
since a DB connection doesn't have to be set up for each request.

```v oksyntax
// blog.v
fn main() {
	mut app := &App{
		db: sqlite.connect(':memory:')!
	}
	sql app.db {
		create table Article
	}!

	first_article := Article{
		title: 'Hello, world!'
		text:  'V is great.'
	}

	second_article := Article{
		title: 'Second post.'
		text:  'Hm... what should I write about?'
	}

	sql app.db {
		insert first_article into Article
		insert second_article into Article
	}!
	veb.run[App, Context](mut app, 8081)
}
```

Create a new file `article.v`:

```v oksyntax
// article.v
module main

struct Article {
	id    int @[primary; sql: serial]
	title string
	text  string
}

pub fn (app &App) find_all_articles() []Article {
	return sql app.db {
		select from Article
	} or { panic(err) }
}
```

Notice that the `Article` structure conforms to the same structure and naming as
the database table in the creation SQL statement. Also we need to add ORM decorators
to our primary key to let it know that it is the primary key and it should auto-increment

Let's fetch the articles in the `index()` action:

```v ignore
// blog.v
pub fn (app &App) index() veb.Result {
	articles := app.find_all_articles()
	return $veb.html()
}
```

Finally, let's update our view:

```html
<body>
  @for article in articles
  <div>
	<b>@article.title</b> <br />
	@article.text
  </div>
  @end
</body>
```

```bash
v run .
```

<img width=662 src="https://github.com/vlang/v/blob/master/tutorials/building_a_simple_web_blog_with_veb/img/articles1.png?raw=true">

That was very simple, wasn't it?

The built-in V ORM uses a syntax very similar to SQL. The queries are built with V.
For example, if we only wanted to find articles with ids between 100 and 200, we'd do:

```v oksyntax
// article.v

return sql app.db {
	select from Article where id >= 100 && id <= 200
} or { panic(err) }
```

Retrieving a single article is very simple:

```v oksyntax
// article.v
pub fn (app &App) retrieve_article() ?Article {
	return sql app.db {
		select from Article limit 1
	} or { panic(err) }[0]
}
```

V ORM uses V's optionals for single values, which is very useful, since
bad queries will always be handled by the developer:

```v ignore
// article.v
article := app.retrieve_article() or {
	return ctx.text('Article not found')
}
```

### Adding new articles

Create `new.html`:

```html
<html>
  <head>
	<title>V Blog</title>
  </head>
  <body>
	<form action="/new_article" method="post">
	  <input type="text" placeholder="Title" name="title" /> <br />
	  <textarea placeholder="Text" name="text"></textarea>
	  <input type="submit" />
	</form>
  </body>
</html>
```

```v ignore
@['/new']
pub fn (app &App) new(mut ctx Context) veb.Result {
	return $veb.html()
}

@['/new_article'; post]
pub fn (app &App) new_article(mut ctx Context) veb.Result {
	title := ctx.form['title'] or { '' }
	text := ctx.form['text'] or { '' }

	if title == '' || text == '' {
		return ctx.text('Empty text/title')
	}

	article := Article{
		title: title
		text: text
	}
	sql app.db {
		insert article into Article
	} or { panic(err) }
	return ctx.redirect('/')
}
```

The decorator on our function tells Veb that it is an HTTP POST type operation.

This time Vweb parses the HTTP form and assigns correct values with correct types to
function arguments, which saves a lot of typing (e.g. `title := app.form['title']` is
not necessary).

We need to update `index.html` to add a link to the "new article" page:

```html
<a href="/new">New article</a>
```

Next we need to add the HTML endpoint to our code like we did with `index.html`:

```v ignore
@['/new']
pub fn (app &App) new() veb.Result {
	return $veb.html()
}
```

Re-running this code will now allow us to add new posts to our blog endpoint

### JSON endpoints

This tutorial used the traditional server-side rendering. If you prefer
to render everything on the client or need an API, creating JSON endpoints
in V is very simple:

```v oksyntax
import veb

@['/articles'; get]
pub fn (app &App) articles(mut ctx Context) veb.Result {
	articles := app.find_all_articles()
	return ctx.json(articles)
}
```

<img width=662 src="https://github.com/vlang/v/blob/master/tutorials/building_a_simple_web_blog_with_veb/img/articles_json.png?raw=true">

### Persistent data

If one wants to persist data they need to use a file instead of memory SQLite Database.
Replace the db setup code with this instead:

```
db: sqlite.connect('blog.db')!
```

As we can see it attempts to open a file in the current directory named `blog.db`.
If the database file doesn't exist it will create it. The second command will
create the table `Article` if none exists already. Now every time the
app is run you will see the articles created from the previous executions

### Building the blog

Run

```bash
v -d use_openssl -o blog -prod . && strip ./blog
```

This will result in a ~400KB binary. `-d use_openssl` tells Veb to link to OpenSSL.
Without this flag mbedtls will be embedded, and the binary size will increase to ~700KB.


### To be continued...

For an example of a more sophisticated web app written in V, check out Gitly: https://github.com/vlang/gitly
