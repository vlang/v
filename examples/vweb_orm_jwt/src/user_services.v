module main

import crypto.bcrypt
import databases

fn (mut app App) service_add_user(username string, password string) ?User {
	mut db := databases.create_db_connection() or {
		eprintln(err)
		return err
	}

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

fn (mut app App) service_get_user_by_id(user_id int) ?User {
	mut db := databases.create_db_connection() or {
		println(err)
		return err
	}

	defer {
		db.close() or { panic(err) }
	}

	results := sql db {
		select from User where id == user_id
	}

	return results
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

fn (mut app App) service_get_by_username(username string) ?User {
	mut db := databases.create_db_connection() or {
		println(err)
		return err
	}

	defer {
		db.close() or { panic(err) }
	}

	results := sql db {
		select from User where username == username
	}

	if results.len == 0 {
		return error('User not found')
	}

	return results[0]
}
