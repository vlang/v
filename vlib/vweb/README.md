There's no documentation yet, have a look at https://github.com/vlang/vtalk for an example of a simple vweb app.

`vtalk.v` contains all GET and POST actions.

```
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

```
@for post in posts 
	<div class=post>
		<a class=topic href="@post.url">@post.title</a> 
		<img class=comment-img> 
		<span class=nr-comments>@post.nr_comments</span> 
		<span class=time>@post.time</span>
	</div>
@end
```

`$vweb.html()` compiles an HTML template into V during compilation, and embeds the resulting code in current action.

That means that the template automatically has access to that action's entire environemnt.
