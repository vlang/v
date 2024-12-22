module db

import db.sqlite

const path = './db/scraping.db'

pub fn new() !sqlite.DB {
	return sqlite.connect(path)
}
