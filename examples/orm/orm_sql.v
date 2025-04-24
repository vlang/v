module main

import db.sqlite
import time

@[table: 'sys_users']
struct User {
pub:
	id         string     @[immutable; primary; sql: 'id'; sql_type: 'VARCHAR(255)'; unique]
	name       ?string    @[immutable; sql: 'name'; sql_type: 'VARCHAR(255)'; unique]
	created_at ?time.Time @[omitempty; sql_type: 'TIMESTAMP']
	updated_at time.Time  @[default: new; omitempty; sql_type: 'TIMESTAMP']
}

fn main() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }

	user1 := User{
		id:         '001'
		name:       'Jengro'
		created_at: time.now()
		updated_at: time.now()
	}

	user2 := User{
		id:         '002'
		name:       'Dev'
		created_at: time.now()
		updated_at: time.now()
	}

	// create table
	sql db {
		create table User
	}!

	// insert into table
	sql db {
		insert user1 into User
		insert user2 into User
	}!

	// query all fields
	all_users := sql db {
		select from User
	}!
	dump(all_users)

	// where
	selected_users := sql db {
		select from User where id == '001'
	}!
	dump(selected_users)

	// update
	sql db {
		update User set name = 'Tom' where id == '001'
	}!
	updated_user := sql db {
		select from User
	}!
	dump(updated_user)

	// delete
	sql db {
		delete from User where id == '001'
	}!
	remain_users := sql db {
		select from User
	}!
	dump(remain_users)

	// drop table
	sql db {
		drop table User
	}!
}
