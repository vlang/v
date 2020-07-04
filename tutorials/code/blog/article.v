module main

import sqlite

struct Article {
	id    int
	title string
	text  string
}

pub fn (app &App) find_all_articles() []Article {
	rows, _ := app.db.exec('select * from Article')
	mut articles := []Article{}
	for r in rows {
		articles << Article {
			r.vals[0].int()
			r.vals[1]
			r.vals[2]
		}
	}
	return articles
}
