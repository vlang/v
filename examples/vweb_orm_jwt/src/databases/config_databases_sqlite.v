module databases

import sqlite

pub fn create_db_connection() ?sqlite.DB {
	// REVIEW - Why don't connect in ':memory:'
	mut db := sqlite.connect('database.db')?
	return db
}
