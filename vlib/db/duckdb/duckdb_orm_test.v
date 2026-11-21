// vtest build: present_duckdb?
import db.duckdb
import orm

struct DuckdbPerson {
	id   int @[primary; sql: serial]
	name string
	age  int
}

struct DuckdbTag {
	id    int    @[primary; sql: serial]
	label string @[unique]
}

struct DuckdbBig {
	id  int @[primary; sql: serial]
	val u64
}

fn new_duckdb_orm_db() duckdb.DB {
	return duckdb.connect(':memory:') or { panic(err) }
}

fn test_duckdb_orm_crud() {
	mut db := new_duckdb_orm_db()
	defer {
		db.close() or { panic(err) }
	}

	sql db {
		create table DuckdbPerson
	}!

	// The serial primary key is backed by a sequence created by `create`.
	ddl := db.exec("SELECT sql FROM duckdb_tables() WHERE table_name = 'duckdbperson'") or {
		panic(err)
	}
	assert ddl.len == 1
	assert ddl[0].vals[0].contains("nextval('seq_duckdbperson')")

	alice := DuckdbPerson{
		name: 'Alice'
		age:  30
	}
	bob := DuckdbPerson{
		name: 'Bob'
		age:  25
	}
	id1 := sql db {
		insert alice into DuckdbPerson
	}!
	id2 := sql db {
		insert bob into DuckdbPerson
	}!
	assert id1 == 1
	assert id2 == 2
	assert db.last_id() == 2

	all := sql db {
		select from DuckdbPerson
	}!
	assert all.len == 2

	found := sql db {
		select from DuckdbPerson where name == 'Alice'
	}!
	assert found.len == 1
	assert found[0].age == 30

	limited := sql db {
		select from DuckdbPerson limit 1 offset 1
	}!
	assert limited.len == 1
	assert limited[0].name == 'Bob'

	sql db {
		update DuckdbPerson set age = 31 where name == 'Alice'
	}!
	updated := sql db {
		select from DuckdbPerson where name == 'Alice'
	}!
	assert updated[0].age == 31

	sql db {
		delete from DuckdbPerson where name == 'Bob'
	}!
	assert (sql db {
		select from DuckdbPerson
	}!).len == 1

	raw := db.execute('SELECT COUNT(*) AS n FROM DuckdbPerson') or { panic(err) }
	assert raw[0].vals[0] == '1'
	assert raw[0].names == ['n']

	sql db {
		drop table DuckdbPerson
	}!
}

fn test_duckdb_orm_u64_max_roundtrip() {
	mut db := new_duckdb_orm_db()
	defer {
		db.close() or { panic(err) }
	}
	sql db {
		create table DuckdbBig
	}!

	// u64 columns must be UBIGINT, not BIGINT, or the maximum value wraps.
	ddl := db.exec("SELECT sql FROM duckdb_tables() WHERE table_name = 'duckdbbig'") or {
		panic(err)
	}
	assert ddl.len == 1
	assert ddl[0].vals[0].contains('UBIGINT')

	max := u64(18446744073709551615)
	row := DuckdbBig{
		val: max
	}
	sql db {
		insert row into DuckdbBig
	}!
	rows := sql db {
		select from DuckdbBig
	}!
	assert rows.len == 1
	assert rows[0].val == max
}

fn test_duckdb_orm_transactions() {
	mut db := new_duckdb_orm_db()
	defer {
		db.close() or { panic(err) }
	}
	sql db {
		create table DuckdbTag
	}!

	{
		mut tx := orm.begin(mut db)!
		tag := DuckdbTag{
			label: 'committed'
		}
		sql tx {
			insert tag into DuckdbTag
		}!
		tx.commit()!
	}
	assert (sql db {
		select from DuckdbTag
	}!).len == 1

	{
		mut tx := orm.begin(mut db)!
		tag := DuckdbTag{
			label: 'rolled back'
		}
		sql tx {
			insert tag into DuckdbTag
		}!
		tx.rollback()!
	}
	assert (sql db {
		select from DuckdbTag
	}!).len == 1
}

fn test_duckdb_orm_savepoints_unsupported() {
	mut db := new_duckdb_orm_db()
	defer {
		db.close() or { panic(err) }
	}
	if _ := db.orm_savepoint('sp') {
		panic('expected savepoint error')
	} else {
		assert err.msg().contains('SAVEPOINT is not supported')
	}
	if _ := db.orm_rollback_to('sp') {
		panic('expected savepoint error')
	} else {
		assert err.msg().contains('ROLLBACK TO SAVEPOINT is not supported')
	}
	if _ := db.orm_release_savepoint('sp') {
		panic('expected savepoint error')
	} else {
		assert err.msg().contains('RELEASE SAVEPOINT is not supported')
	}
}

fn test_duckdb_orm_low_level_connection() {
	mut db := new_duckdb_orm_db()
	defer {
		db.close() or { panic(err) }
	}
	table := orm.Table{
		name: 'LowLevel'
	}
	db.create(table, [
		orm.TableField{
			name:  'id'
			typ:   typeof[int]().idx
			attrs: [
				VAttribute{
					name: 'primary'
				},
				VAttribute{
					name:    'sql'
					has_arg: true
					kind:    .plain
					arg:     'serial'
				},
			]
		},
		orm.TableField{
			name: 'name'
			typ:  typeof[string]().idx
		},
	]) or {
		panic(err)
	}
	db.insert(table, orm.QueryData{
		fields: ['name']
		data:   [orm.Primitive('Carol')]
	}) or {
		panic(err)
	}
	assert db.last_id() == 1
	res := db.select(orm.SelectConfig{
		table:     table
		has_where: true
		fields:    ['id', 'name']
		types:     [typeof[int]().idx, typeof[string]().idx]
	}, orm.QueryData{}, orm.QueryData{
		fields: ['name']
		data:   [orm.Primitive('Carol')]
		types:  [typeof[string]().idx]
		is_and: [true]
		kinds:  [.eq]
	}) or {
		panic(err)
	}
	assert res.len == 1
	assert res[0][0] is int
	name := res[0][1]
	assert name is string
	if name is string {
		assert name == 'Carol'
	}
}
