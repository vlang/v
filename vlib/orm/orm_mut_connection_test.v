// vtest retry: 3
import orm

struct Database {
mut:
	last_query string
}

struct User {
	id   int @[primary; sql: serial]
	name string
}

fn (mut db Database) query(sql_stmt string) ! {
	db.last_query = sql_stmt
}

// select is used internally by V's ORM for processing `SELECT` queries
fn (mut db Database) select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	// 1. Create query and bind necessary data
	query := orm.orm_select_gen(config, '', true, ':', 1, where)
	mut ret := [][]orm.Primitive{}
	return ret
}

// insert is used internally by V's ORM for processing `INSERT` queries
fn (mut db Database) insert(table orm.Table, data orm.QueryData) ! {
	query, _ := orm.orm_stmt_gen(.sqlite, table, '', .insert, false, '?', 1, data, orm.QueryData{})

	db.query(query)!
}

// update is used internally by V's ORM for processing `UPDATE` queries
fn (mut db Database) update(table orm.Table, data orm.QueryData, where orm.QueryData) ! {
	mut query, _ := orm.orm_stmt_gen(.sqlite, table, '', .update, true, ':', 1, data,
		where)

	db.query(query)!
}

// delete is used internally by V's ORM for processing `DELETE ` queries
fn (mut db Database) delete(table orm.Table, where orm.QueryData) ! {
	query, converted := orm.orm_stmt_gen(.sqlite, table, '', .delete, true, ':', 1, orm.QueryData{},
		where)

	db.query(query)!
}

// last_id is used internally by V's ORM for post-processing `INSERT` queries
fn (mut db Database) last_id() int {
	return 0
}

// DDL (table creation/destroying etc)
fn sqlite_type_from_v(typ int) !string {
	return if typ in orm.nums || typ in orm.num64 || typ in [orm.serial, orm.time_, orm.enum_] {
		'INTEGER'
	} else if typ in orm.float {
		'REAL'
	} else if typ == orm.type_string {
		'TEXT'
	} else {
		error('Unknown type ${typ}')
	}
}

// create is used internally by V's ORM for processing table creation queries (DDL)
fn (mut db Database) create(table orm.Table, fields []orm.TableField) ! {
	mut query := orm.orm_table_gen(.sqlite, table, '', true, 0, fields, sqlite_type_from_v,
		false) or { return err }
	db.query(query)!
}

// drop is used internally by V's ORM for processing table destroying queries (DDL)
fn (mut db Database) drop(table orm.Table) ! {
	query := 'DROP TABLE ${table.name};'
	$if trace_orm ? {
		eprintln('> vsql drop: ${query}')
	}
	db.query(query)!
}

fn test_orm_mut_connection() {
	mut db := Database{}

	sql db {
		create table User
	}!

	first_user := User{
		name: 'first'
	}
	second_user := User{
		name: 'second'
	}

	sql db {
		insert first_user into User
		insert second_user into User
	}!

	sql db {
		drop table User
	}!

	assert db.last_query == 'DROP TABLE User;'
}
