module databases

import mysql
import os

pub fn create_db_connection() ?mysql.Connection {
	mut db := mysql.Connection{
		host: os.getenv('DB_HOST')
		port: os.getenv('DB_PORT').u32()
		username: os.getenv('DB_USERNAME')
		password: os.getenv('DB_PASSWORD')
		dbname: os.getenv('DB_NAME')
	}

	db.connect() or { println(err) }

	return db
}
