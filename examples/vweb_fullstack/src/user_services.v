module main

import crypto.bcrypt
import databases

fn (mut app App) service_add_user(username string, password string) ! {
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
		active:   true
	}

	mut insert_error := ''
	sql db {
		insert user_model into User
	} or { insert_error = err.msg() }
	if insert_error != '' {
		return error(insert_error)
	}
}

fn (mut app App) service_get_all_user() ![]User {
	mut db := databases.create_db_connection() or {
		println(err)
		return err
	}

	defer {
		db.close() or { panic(err) }
	}

	results := sql db {
		select from User
	}!

	return results
}

fn (mut app App) service_get_user(id int) !User {
	mut db := databases.create_db_connection() or {
		println(err)
		return err
	}

	defer {
		db.close() or { panic(err) }
	}

	results := sql db {
		select from User where id == id
	}!

	return results.first()
}
