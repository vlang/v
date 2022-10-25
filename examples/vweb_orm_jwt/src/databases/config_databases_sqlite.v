module databases

import sqlite

pub fn create_db_connection() !sqlite.DB {
	mut db := sqlite.connect('database.db')!
	return db
}
