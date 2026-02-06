module main

import db.sqlite
import time
import orm

@[table: 'sys_users']
struct User {
pub:
	id         string     @[immutable; primary; sql: 'id'; sql_type: 'VARCHAR(255)'; unique]
	name       ?string    @[immutable; sql: 'nick_name'; sql_type: 'VARCHAR(255)'; unique]
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

	mut qb := orm.new_query[User](db)

	// create table
	qb.create()!

	// insert into table
	qb.insert(user1)!
	qb.insert(user2)!

	// query all fields
	all_users := qb.query()!
	dump(all_users)

	// query all users' nick_name
	all_user_names := qb.select('nick_name')!.query()!
	dump(all_user_names)

	// where
	selected_users := qb.where('id = ?', '001')!.query()!
	dump(selected_users)

	// update
	qb.set('nick_name = ?', 'Tom')!.where('id = ?', '001')!.update()!
	updated_user := qb.query()!
	dump(updated_user)

	// delete
	qb.where('id = ?', '001')!.delete()!
	remain_users := qb.query()!
	dump(remain_users)

	// drop table
	qb.drop()!
}
