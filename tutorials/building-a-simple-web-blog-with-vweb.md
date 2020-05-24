## Building a 150 KB web blog in V with 0 dependencies

Hello,

In this guide, we'll build a simple web blog in V.

The benefits of using V for web:
- A safe, fast, language with the development speed of Python and
the performance of C.
- Zero dependencies: everything you need for web development comes with the language
in a 1 MB package.
- Very small resulting binaries: the blog we'll create in this tutorial is about 150 KB.
- Easy deployments: a single binary file that even  includes the precompiled templates.
- Runs on the cheapest hardware with minimum footprint: for most apps a $3 instance
is enough.
- Fast development without any boilerplate.

*Please note that V and Vweb are at a very early stage and are changing rapidly.*

The code is available <a href='https://github.com/vlang/v/tree/master/tutorials/code/blog'>here</a>.


### Installing V

```
wget https://github.com/vlang/v/releases/latest/download/v_linux.zip
unzip v_linux.zip
cd v
sudo ./v symlink
```

Now V should be globally available on your system.

> On macOS use `v_macos.zip`, on Windows - `v_windows.zip`.
If you use a BSD system, Solaris, Android, or simply want to install V
from source, follow the simple instructions here:
https://github.com/vlang/v#installing-v-from-source


### Creating a new Vweb project

V projects can be created anywhere and don't need to have a certain structure:

```bash
mkdir blog
cd blog
touch blog.v
```

First, let's create a simple hello world website:

```v
// blog.v
module main

import vweb

pub struct App {
mut:
	vweb vweb.Context
}

fn main() {
	vweb.run<App>(8080)
}

fn (mut app App) index() {
	app.vweb.text('Hello, world from vweb!')
}

pub fn (app &App) init() {}
pub fn (app &App) reset() {}

```

Run it with

```bash
v run blog.v
```

```
Running a Vweb app on http://localhost:8080 ...
```

Vweb helpfully provided a link, open http://localhost:8080/ in your browser:

<img width=662 src="https://github.com/vlang/v/blob/master/tutorials/img/hello.png?raw=true">

The `App` struct is an entry point of our web application. If you have experience
with an MVC web framework, you can think of it as a controller. (Vweb is
not an MVC framework however.)

As you can see, there are no routing rules. The `index()` action handles the `/` request by default.
Vweb often uses convention over configuration and adding a new action requires
no routing rules either:

```v
fn (mut app App) time() {
	app.vweb.text(time.now().format())
}
```


<img width=662 src="https://github.com/vlang/v/blob/master/tutorials/img/time.png?raw=true">

>You have to rebuild and restart the website every time you change the code.
In the future, Vweb will detect changes and recompile the website in the background
while it's running.

The `.text(string)` method returns a plain text document with the provided
text, which isn't frequently used in websites.


### HTML View


Let's return an HTML view instead. Create `index.html` in the same directory:

```html
<html>
<header>
	<title>V Blog</title>
</header>
<body>
	<b>@message</b>
	<br>
	<img src='https://vlang.io/img/v-logo.png' width=100>
</body>
</html>
```

and update our `index()` action so that it returns the HTML view we just created:

```v
fn (mut app App) index() {
	message := 'Hello, world from Vweb!'
	$vweb.html()
}
```

<img width=662 src="https://github.com/vlang/v/blob/master/tutorials/img/hello_html.png?raw=true">

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

The HTML template is compiled to V during the compilation of the website, that's done by the `$vweb.html()` line.
(`$` always means compile time actions in V.) offering the following benefits:

- Great performance, since the templates don't need to be compiled
on every request, like in almost every major web framework.

- Easier deployment, since all your HTML templates are  compiled
into a single binary file together with the web application itself.

- All errors in the templates are guaranteed to be caught during compilation.

### Fetching data with V ORM

Now let's display some articles!

We'll be using V's builtin ORM and a Postgres database. (V ORM will also
support MySQL, SQLite, and SQL Server soon.)

Create a SQL file with the schema:
```sql
create database blog;

\c blog

drop table articles;

create table articles (
	id serial primary key,
	title text default '',
	text text default ''
);

insert into articles (title, text) values (
	'Hello, world!',
	'V is great.'
);

insert into articles (title, text) values (
	'Second post.',
	'Hm... what should I write about?'
);
```

Run the file with `psql -f blog.sql`.


Add a Postgres DB handle to `App`:

```v
struct App {
mut:
	vweb vweb.Context
	db   pg.DB
}
```



Modify the `init()` method we created earlier to connect to a database:

```v
pub fn (mut app App) init() {
	db := pg.connect(pg.Config{
		host:   '127.0.0.1'
		dbname: 'blog'
		user:   'blog'
	}) or { panic(err) }
	app.db = db
}
```

Code in the `init()` function is run only once during app's startup, so we are going
to have one DB connection for all requests.

Create a new file `article.v`:


```v

module main

struct Article {
	id    int
	title string
	text  string
}

pub fn (app &App) find_all_articles() []Article {
	db := app.db
	articles := db.select from Article
	return articles
}
```

Let's fetch the articles in the `index()` action:

```v
fn (app &App) index() {
	articles := app.find_all_articles()
	$vweb.html()
}
```


Finally, let's update our view:

```html
<body>
	@for article in articles
		<div>
			<b>@article.title</b> <br>
			@article.text
		</div>
	@end
</body>
```

```bash
v run .
```

<img width=662 src="https://github.com/vlang/v/blob/master/tutorials/img/articles1.png?raw=true">

That was very simple, wasn't it?

The built-in V ORM uses a syntax very similar to SQL. The queries are built with V.
For example, if we only wanted to find articles with ids between 100 and 200, we'd do:

```
articles := db.select from Article where id >= 100 && id <= 200
```

Retrieving a single article is very simple:

```v

pub fn (app &App) retrieve_article() ?Article {
	db := app.db
	article := db.select from Article limit 1
	return article
}
```

V ORM uses V's optionals for single values, which is very useful, since
bad queries will always be handled by the developer:

```v
article := app.retrieve_article(10) or {
	app.vweb.text('Article not found')
	return
}
```


> `db := app.db` is a temporary limitation in the
V ORM, soon this will not be needed.


### Adding new articles

Create `new.html`:

```html
<html>
<header>
	<title>V Blog</title>
</header>
<body>
	<form action='/new_article' method='post'>
		<input type='text' placeholder='Title' name='title'> <br>
		<textarea placeholder='Text' name='text'></textarea>
		<input type='submit'>
	</form>
</body>
</html>
```

```v
pub fn (mut app App) new_article() {
	title := app.vweb.form['title']
	text := app.vweb.form['text']
	if title == '' || text == ''  {
		app.vweb.text('Empty text/title')
		return
	}
	article := Article{
		title: title
		text: text
	}
	db := app.db
	db.insert(article)
	app.vweb.redirect('/article/')
}
```

> Untyped `form['key']` is temporary. Very soon Vweb will accept query and form
parameters via function arguments: `new_article(title, text string) {`.

We need to update `index.html` to add a link to the "new article" page:

```html
<a href='/new'>New article</a>
```



### JSON endpoints

This tutorial used the traditional server-side rendering. If you prefer
to render everything on the client or need an API, creating JSON endpoints
in V is very simple:

```v
pub fn (mut app App) articles() {
	articles := app.find_all_articles()
	app.vweb.json(json.encode(articles))
}
```

<img width=662 src="https://github.com/vlang/v/blob/master/tutorials/img/articles_json.png?raw=true">



To be continued...

For an example of a more sophisticated web app written in V, check out Vorum: https://github.com/vlang/vorum
