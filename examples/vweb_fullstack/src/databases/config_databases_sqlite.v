module databases

import sqlite // can change to 'mysql', 'pg'

pub fn create_db_connection() !sqlite.DB {
	mut db := sqlite.connect('vweb.sql')!
	return db
}
