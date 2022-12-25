module main

import crypto.bcrypt
import databases
import json

fn (mut app App) service_add_user(username string, password string) !User {
	mut db := databases.create_db_connection()!

	defer {
		db.close() or { panic(err) }
	}

	hashed_password := bcrypt.generate_from_password(password.bytes(), bcrypt.min_cost) or {
		eprintln(err)
		return err
	}

	user_model := User{
		username: username
		password: hashed_password
		active: true
	}

	sql db {
		insert user_model into User
	}

	result := sql db {
		select from User where username == username limit 1
	}

	return result
}

fn (mut app App) service_get_all_user() ?[]User {
	mut db := databases.create_db_connection() or {
		println(err)
		return err
	}

	defer {
		db.close() or { panic(err) }
	}

	results := sql db {
		select from User
	}

	return results
}
