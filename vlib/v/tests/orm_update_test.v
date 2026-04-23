import db.sqlite
import time

struct Person {
	name   string
	height Height
}

enum Height as u8 {
	tall
	small
}

fn test_main() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Person
	}!

	a := Person{'A', Height.small}
	b := Person{'A', Height.tall}

	sql db {
		insert a into Person
	}!

	sql db {
		insert b into Person
	}!

	new_height := Height.small
	sql db {
		update Person set height = new_height where height == Height.tall
	}!

	rows := sql db {
		select from Person where height == Height.small
	}!

	assert rows.len == 2
}

@[table: 'orm_update_users']
struct OrmUpdateUser {
pub:
	id         string @[primary]
	name       string
	updated_at time.Time @[sql_type: 'TIMESTAMP']
}

struct OrmUpdateReq {
	updated_at ?time.Time
}

fn test_orm_update_with_option_selector_or_block() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table OrmUpdateUser
	}!

	user := OrmUpdateUser{
		id:         '100'
		name:       'Alice'
		updated_at: time.unix(1_700_000_000)
	}
	sql db {
		insert user into OrmUpdateUser
	}!

	req := OrmUpdateReq{}
	expected_updated_at := time.unix(1_700_000_123)

	sql db {
		update OrmUpdateUser set updated_at = req.updated_at or { expected_updated_at } where id == '100'
	}!

	rows := sql db {
		select from OrmUpdateUser where id == '100'
	}!

	assert rows.len == 1
	assert rows[0].updated_at.unix() == expected_updated_at.unix()
}

struct OrmUpdateThing {
	id    int @[primary]
	value string
}

struct OrmUpdateCommit {
	id    int @[primary]
	tag   string
	thing OrmUpdateThing
}

fn test_orm_update_with_struct_field() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table OrmUpdateThing
		create table OrmUpdateCommit
	}!

	second := OrmUpdateThing{
		id:    2
		value: 'goodbye'
	}
	sql db {
		insert second into OrmUpdateThing
	}!

	commit := OrmUpdateCommit{
		id:    1
		tag:   'head'
		thing: OrmUpdateThing{
			id:    1
			value: 'hello'
		}
	}
	sql db {
		insert commit into OrmUpdateCommit
	}!

	sql db {
		update OrmUpdateCommit set thing = second where tag == 'head'
	}!

	rows := sql db {
		select from OrmUpdateCommit where tag == 'head'
	}!

	assert rows.len == 1
	assert rows[0].thing.id == second.id
	assert rows[0].thing.value == 'goodbye'
}

struct DynamicOrmUser {
	id              int @[primary; sql: serial]
	simplified_name string
	currency_code   string
}

struct DynamicOrmRequest {
	simplified_name ?string
	currency_code   ?string
}

fn test_dynamic_orm_select_and_update_with_if_guards() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table DynamicOrmUser
	}!

	first := DynamicOrmUser{
		id:              1
		simplified_name: 'usd'
		currency_code:   'USD'
	}
	second := DynamicOrmUser{
		id:              2
		simplified_name: 'eur'
		currency_code:   'EUR'
	}

	sql db {
		insert first into DynamicOrmUser
		insert second into DynamicOrmUser
	}!

	select_req := DynamicOrmRequest{
		simplified_name: ?string('usd')
	}
	where_expr := {
				if name := select_req.simplified_name {
						simplified_name == name
				},
				if code := select_req.currency_code {
						currency_code == code
				}
		}

	rows := sql db {
		dynamic select from DynamicOrmUser where where_expr
	}!

	assert rows.len == 1
	assert rows[0].id == first.id

	update_req := DynamicOrmRequest{
		simplified_name: ?string('dollar')
	}
	set_expr := {
				if name := update_req.simplified_name {
						simplified_name == name
				},
				if code := update_req.currency_code {
						currency_code == code
				}
		}

	sql db {
		dynamic update DynamicOrmUser set set_expr where id == first.id
	}!

	updated_rows := sql db {
		select from DynamicOrmUser where id == first.id
	}!

	assert updated_rows.len == 1
	assert updated_rows[0].simplified_name == 'dollar'
	assert updated_rows[0].currency_code == 'USD'
}
