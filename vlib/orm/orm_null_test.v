// vtest retry: 3
import orm
import db.sqlite

struct MockDBState {
mut:
	last  string
	data  []orm.Primitive
	where []orm.Primitive
}

struct MockDB {
	use_num bool
	st      &MockDBState = unsafe { nil }
	db      sqlite.DB
}

fn MockDB.new() &MockDB {
	return &MockDB{
		st: &MockDBState{}
		db: sqlite.connect(':memory:') or { panic(err) }
	}
}

fn (db MockDB) select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	mut st := db.st
	st.last = orm.orm_select_gen(config, '`', false, '?', 5, where)
	st.data = data.data
	st.where = where.data
	return db.db.select(config, data, where)
}

fn (db MockDB) insert(table orm.Table, data orm.QueryData) ! {
	mut st := db.st
	last, qdata := orm.orm_stmt_gen(.sqlite, table, '`', .insert, false, '?', 1, data,
		orm.QueryData{})
	st.last = last
	st.data = qdata.data
	st.where = []orm.Primitive{}
	return db.db.insert(table, data)
}

fn (db MockDB) update(table orm.Table, data orm.QueryData, where orm.QueryData) ! {
	mut st := db.st
	st.last, _ = orm.orm_stmt_gen(.sqlite, table, '`', .update, false, '?', 1, data, where)
	st.data = data.data
	st.where = where.data
	return db.db.update(table, data, where)
}

fn (db MockDB) delete(table orm.Table, where orm.QueryData) ! {
	mut st := db.st
	st.last, _ = orm.orm_stmt_gen(.sqlite, table, '`', .delete, false, '?', 1, orm.QueryData{},
		where)
	return db.db.delete(table, where)
}

const typ_to_typename = {
	typeof[i8]().idx:     'i8'
	typeof[i16]().idx:    'i16'
	typeof[int]().idx:    'int'
	typeof[i64]().idx:    'i64'
	typeof[u8]().idx:     'u8'
	typeof[u16]().idx:    'u16'
	typeof[u32]().idx:    'u32'
	typeof[u64]().idx:    'u64'
	typeof[f32]().idx:    'f32'
	typeof[f64]().idx:    'f64'
	typeof[string]().idx: 'string'
	typeof[bool]().idx:   'bool'
	orm.serial:           'serial'
	orm.time_:            'time'
	orm.enum_:            'enum'
}

fn mock_type_from_v(typ int) !string {
	return if typ in typ_to_typename {
		'${typ_to_typename[typ]}-type'
	} else {
		error('unknown type ${typ}')
	}
}

fn (db MockDB) create(table orm.Table, fields []orm.TableField) ! {
	mut st := db.st
	st.last = orm.orm_table_gen(.sqlite, table, '`', true, 0, fields, mock_type_from_v,
		false)!
	return db.db.create(table, fields)
}

fn (db MockDB) drop(table orm.Table) ! {
	return db.db.drop(table)
}

fn (db MockDB) last_id() int {
	return db.db.last_id()
}

// --

@[table: 'foo']
struct Foo {
mut:
	id u64 @[primary; sql: serial]
	a  string
	b  string @[default: '"yes"']
	c  ?string
	d  ?string = 'hi'
	e  int
	f  int @[default: 33]
	g  ?int
	h  ?int = 55
}

fn test_option_struct_fields_and_none() {
	db := MockDB.new()

	sql db {
		create table Foo
	}!
	assert db.st.last == 'CREATE TABLE IF NOT EXISTS `foo` (`id` serial-type NOT NULL, `a` string-type NOT NULL, `b` string-type DEFAULT "yes" NOT NULL, `c` string-type, `d` string-type, `e` int-type NOT NULL, `f` int-type DEFAULT 33 NOT NULL, `g` int-type, `h` int-type, PRIMARY KEY(`id`));'

	_ := sql db {
		select from Foo where e > 5 && c is none && c !is none && h == 2
	}!
	assert db.st.last == 'SELECT `id`, `a`, `b`, `c`, `d`, `e`, `f`, `g`, `h` FROM `foo` WHERE `e` > ? AND `c` IS NULL AND `c` IS NOT NULL AND `h` = ?;'
	assert db.st.data.len == 0
	assert db.st.where.len == 2
	assert db.st.where == [orm.Primitive(int(5)), orm.Primitive(int(2))]

	foo := Foo{}
	sql db {
		insert foo into Foo
	}!
	assert db.st.last == 'INSERT INTO `foo` (`a`, `c`, `d`, `e`, `g`, `h`) VALUES (?, ?, ?, ?, ?, ?);'
	assert db.st.data.len == 6
	assert db.st.data == [orm.Primitive(string('')), orm.Null{}, orm.Primitive(string('hi')), int(0),
		orm.Null{}, int(55)]
	id := db.last_id()

	res1 := sql db {
		select from Foo where id == id
	}!
	assert db.st.last == 'SELECT `id`, `a`, `b`, `c`, `d`, `e`, `f`, `g`, `h` FROM `foo` WHERE `id` = ?;'
	assert db.st.data.len == 0
	assert db.st.where.len == 1
	assert db.st.where == [orm.Primitive(int(id))]
	assert res1.len == 1
	assert res1[0] == Foo{
		id: 1
		a:  ''
		b:  'yes'
		c:  none
		d:  'hi'
		e:  0
		f:  33
		g:  none
		h:  55
	}

	sql db {
		update Foo set c = 'yo', d = none, g = 44, h = none where id == id
	}!
	assert db.st.last == 'UPDATE `foo` SET `c` = ?, `d` = ?, `g` = ?, `h` = ? WHERE `id` = ?;'
	assert db.st.data.len == 4
	assert db.st.data == [orm.Primitive(string('yo')), orm.Null{}, int(44), orm.Null{}]
	assert db.st.where.len == 1
	assert db.st.where == [orm.Primitive(int(id))]

	res2 := sql db {
		select from Foo where id == id
	}!
	assert db.st.last == 'SELECT `id`, `a`, `b`, `c`, `d`, `e`, `f`, `g`, `h` FROM `foo` WHERE `id` = ?;'
	assert db.st.data.len == 0
	assert db.st.where.len == 1
	assert db.st.where == [orm.Primitive(int(id))]
	assert res2.len == 1
	assert res2[0] == Foo{
		id: 1
		a:  ''
		b:  'yes'
		c:  'yo'
		d:  none
		e:  0
		f:  33
		g:  44
		h:  none
	}

	assert sql db {
		select count from Foo where a == 'yo'
	}! == 0
	assert sql db {
		select count from Foo where b == 'yes'
	}! == 1
	assert sql db {
		select count from Foo where d == 'yo'
	}! == 0
	assert sql db {
		select count from Foo where f == 33
	}! == 1
	assert sql db {
		select count from Foo where c == 'yo'
	}! == 1
	assert sql db {
		select count from Foo where a == ''
	}! == 1
	assert sql db {
		select count from Foo where d == ''
	}! == 0
	assert sql db {
		select count from Foo where c == ''
	}! == 0
	assert sql db {
		select count from Foo where a is none
	}! == 0
	assert sql db {
		select count from Foo where d is none
	}! == 1
	assert sql db {
		select count from Foo where c is none
	}! == 0
	assert sql db {
		select count from Foo where a !is none
	}! == 1
	assert sql db {
		select count from Foo where d !is none
	}! == 0
	assert sql db {
		select count from Foo where c !is none
	}! == 1
}

struct Bar {
	id   u64 @[primary; sql: serial]
	name ?string
	age  int
}

fn update_bar1(db MockDB, id u64, name ?string) ! {
	foo := 66
	sql db {
		update Bar set name = name, age = age + 3 + foo where id == id
	}!
}

fn update_bar2(db MockDB, name ?string, new_name ?string) ! {
	sql db {
		update Bar set name = new_name where name == name
	}!
}

type NameFn = fn () ?string

fn update_bar3(db MockDB, name_fn NameFn, new_name string) ! {
	sql db {
		update Bar set name = new_name where name == name_fn()
	}!
}

fn test_inserting_passed_optionals() {
	db := MockDB.new()

	entry1 := Bar{}
	entry2 := Bar{
		name: 'Alice'
		age:  55
	}
	entry3 := Bar{
		name: 'Bob'
		age:  66
	}
	sql db {
		create table Bar
		insert entry1 into Bar
		insert entry2 into Bar
		insert entry3 into Bar
	}!

	update_bar1(db, 2, none)!
	update_bar1(db, 1, 'hi')!

	res1 := sql db {
		select from Bar
	}!
	assert res1.len == 3
	assert res1[0].name or { '' } == 'hi'
	assert res1[1].name == none
	assert res1[2].name or { '' } == 'Bob'

	update_bar2(db, none, 'xxx')! // no effect (select using "is none", not "== none")
	update_bar2(db, 'hi', none)!

	res2 := sql db {
		select from Bar
	}!
	assert res2.len == 3
	assert res2[0].name == none
	assert res2[1].name == none
	assert res2[2].name or { '' } == 'Bob'

	update_bar3(db, fn () ?string {
		return none // no effect (select using "is none", not "== none")
	}, 'yyy')!
	update_bar3(db, fn () ?string {
		return 'Bob'
	}, 'www')!

	res3 := sql db {
		select from Bar
	}!
	assert res3.len == 3
	assert res3[0].name == none
	assert res3[1].name == none
	assert res3[2].name or { '' } == 'www'
}
