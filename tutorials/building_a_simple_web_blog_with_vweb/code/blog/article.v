module main

struct Article {
	id    int @[primary; sql: serial]
	title string
	text  string
}

pub fn (app &App) find_all_articles() []Article {
	return sql app.db {
		select from Article
	} or { []Article{} }
}
