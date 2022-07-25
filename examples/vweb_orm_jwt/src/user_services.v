module main

import crypto.bcrypt
import databases
import time

fn (mut app App) service_add_user(username string, password string) ?User {
	mut db := databases.create_db_connection() or {
		eprintln(err)
		return err
	}

	defer {
		db.close()
	}

	hashed_password := bcrypt.generate_from_password(password.bytes(), bcrypt.min_cost) or {
		eprintln(err)
		return err
	}

	user_model := User{
		username: username
		name: password
		password: hashed_password
		created_at: time.now()
		updated_at: time.now()
		deleted_at: time.now()
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
		db.close()
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
		db.close()
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
		db.close()
	}

	results := sql db {
		select from User where username == username
	}

	if results.len == 0 {
		return error('Usuário não encontrado')
	}

	return results[0]
}
