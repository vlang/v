import orm
import db.sqlite

struct State {
mut:
	last  string
	data  []orm.Primitive
	where []orm.Primitive
}

struct MockDB {
	use_num bool
	st      &State = unsafe { nil }
	db      sqlite.DB
}

fn MockDB.new() &MockDB {
	return &MockDB{
		st: &State{}
		db: sqlite.connect(':memory:') or { panic(err) }
	}
}

fn (db MockDB) @select(config orm.SelectConfig, data orm.QueryData, where orm.QueryData) ![][]orm.Primitive {
	mut st := db.st
	st.last = orm.orm_select_gen(config, '`', false, '?', 5, where)
	st.data = data.data
	st.where = where.data
	return db.db.@select(config, data, where)
}

fn (db MockDB) insert(table string, data orm.QueryData) ! {
	mut st := db.st
	last, qdata := orm.orm_stmt_gen(.sqlite, table, '`', .insert, false, '?', 1, data,
		orm.QueryData{})
	st.last = last
	st.data = qdata.data
	st.where = []orm.Primitive{}
	return db.db.insert(table, data)
}

fn (db MockDB) update(table string, data orm.QueryData, where orm.QueryData) ! {
	mut st := db.st
	st.last, _ = orm.orm_stmt_gen(.sqlite, table, '`', .update, false, '?', 1, data, where)
	st.data = data.data
	st.where = where.data
	return db.db.update(table, data, where)
}

fn (db MockDB) delete(table string, where orm.QueryData) ! {
	mut st := db.st
	st.last, _ = orm.orm_stmt_gen(.sqlite, table, '`', .delete, false, '?', 1, orm.QueryData{},
		where)
	return db.db.delete(table, where)
}

const (
	typ_to_typename = {
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
	}
)

fn mock_type_from_v(typ int) !string {
	return if typ in typ_to_typename {
		'${typ_to_typename[typ]}-type'
	} else {
		error('unknown type ${typ}')
	}
}

fn (db MockDB) create(table string, fields []orm.TableField) ! {
	mut st := db.st
	st.last = orm.orm_table_gen(table, '`', true, 0, fields, mock_type_from_v, false)!
	return db.db.create(table, fields)
}

fn (db MockDB) drop(table string) ! {
	return db.db.drop(table)
}

fn (db MockDB) last_id() int {
	return db.db.last_id()
}

[table: 'foo']
struct Foo {
mut:
	id u64    [primary]
	a  string
	//	b  string  [default: '"yes"']
	c ?string
	d ?string = 'hi'
	e int
	//	f  int     [default: 33]
	g ?int
	h ?int = 55
}

fn test_option_struct_fields_and_none() {
	db := MockDB.new()

	sql db {
		create table Foo
	}!
	assert db.st.last == 'CREATE TABLE IF NOT EXISTS `foo` (`id` u64-type NOT NULL, `a` string-type NOT NULL, `c` string-type, `d` string-type, `e` int-type NOT NULL, `g` int-type, `h` int-type, PRIMARY KEY(`id`));'

	_ := sql db {
		select from Foo where e > 5 && c !is none && c is none && h == 2
	}!
	// assert db.st.last == 'SELECT `id`, `a`, `c`, `d`, `e`, `g` , `h` FROM `foo` WHERE `e` > ? AND `c` IS NULL AND `c` IS NOT NULL AND `h` = ?;'

	foo := Foo{}
	sql db {
		insert foo into Foo
	}!
	assert db.st.last == 'INSERT INTO `foo` (`a`, `c`, `d`, `e`, `g`, `h`) VALUES (?, ?, ?, ?, ?, ?);'
	assert db.st.data.len == 6
	assert db.st.data == [orm.Primitive(string('')), orm.NullType{}, orm.Primitive(string('hi')),
		int(0), orm.NullType{}, int(55)]
	id := db.last_id()

	sql db {
		update Foo set c = 'yo', d = none, g = 44, h = none where id == id
	}!
	assert db.st.last == 'UPDATE `foo` SET `c` = ?, `d` = ?, `g` = ?, `h` = ? WHERE `id` = ?;'
	assert db.st.data.len == 4
	assert db.st.data == [orm.Primitive(string('yo')), orm.NullType{}, int(44), orm.NullType{}]
	assert db.st.where.len == 1
	assert db.st.where == [orm.Primitive(int(id))]

	res := sql db {
		select from Foo where id == id
	}!
	assert db.st.last == 'SELECT `id`, `a`, `c`, `d`, `e`, `g`, `h` FROM `foo` WHERE `id` = ?;'
	println(db.st.data)
	assert db.st.data.len == 0
	assert db.st.where.len == 1
	assert db.st.where == [orm.Primitive(int(id))]
	assert res.len == 1
	row := res[0]
	assert row == Foo{
		id: 1
		a: ''
		c: 'yo'
		d: none
		e: 0
		g: 44
		h: none
	}
}
