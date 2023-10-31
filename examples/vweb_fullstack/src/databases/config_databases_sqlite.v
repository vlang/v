module databases

import db.sqlite // can change to 'db.mysql', 'db.pg'

pub fn create_db_connection() !sqlite.DB {
	mut db := sqlite.connect('app.db')!
	return db
}
