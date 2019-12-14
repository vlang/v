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




