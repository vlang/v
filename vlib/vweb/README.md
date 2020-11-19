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
		app.vweb.redirect('/')
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
