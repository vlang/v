import orm

// The `sql db { create table T }` statement is lowered into orm.v_sql_create_table[T],
// which rebuilds every field's attributes from the comptime `field.attrs` strings
// (orm_field_attrs_from_strings). That is the path the Function Call API and the V3
// backend take, and it is distinct from the orm_table_gen() unit test in
// orm_fn_test.v. This driver is pure V, so both backends compile it as is.
// See https://github.com/vlang/v/issues/27987
struct FakeDB {
mut:
	last string
}

fn (mut db FakeDB) select(_config orm.SelectConfig, _data orm.QueryData, _where orm.QueryData) ![][]orm.Primitive {
	return [][]orm.Primitive{}
}

fn (mut db FakeDB) insert(_table orm.Table, _data orm.QueryData) ! {}

fn (mut db FakeDB) update(_table orm.Table, _data orm.QueryData, _where orm.QueryData) ! {}

fn (mut db FakeDB) delete(_table orm.Table, _where orm.QueryData) ! {}

fn (mut db FakeDB) create(table orm.Table, fields []orm.TableField) ! {
	db.last = orm.orm_table_gen(.default, table, '`', true, 0, fields, fake_sql_type_from_v, false)!
}

fn (mut db FakeDB) drop(_table orm.Table) ! {}

fn (mut db FakeDB) last_id() int {
	return 0
}

fn (mut db FakeDB) execute(_query string) ![]orm.Row {
	return []orm.Row{}
}

fn fake_sql_type_from_v(typ int) !string {
	return if typ == orm.type_idx['int'] { 'INT' } else { 'TEXT' }
}

@[table: 'demo_default']
struct DemoDefault {
	home_path string @[default: '`/dashboard`']
	tags      string @[default: '`["all"]`']
	method    string @[default: '`POST`']
	quoted    string @[default: "`o'brien`"]
	empty     string @[default: '``']
	amount    int @[default: 42]
	created   string @[default: 'CURRENT_TIMESTAMP']
}

fn test_backtick_defaults_survive_the_comptime_attribute_path() {
	mut db := FakeDB{}
	sql db {
		create table DemoDefault
	}!
	assert db.last == 'CREATE TABLE IF NOT EXISTS `demo_default` (`home_path` TEXT DEFAULT ' + "'/dashboard'" + ' NOT NULL, `tags` TEXT DEFAULT ' + '\'["all"]\'' + ' NOT NULL, `method` TEXT DEFAULT ' + "'POST'" + ' NOT NULL, `quoted` TEXT DEFAULT ' + "'o''brien'" + ' NOT NULL, `empty` TEXT DEFAULT ' + "''" + ' NOT NULL, `amount` INT DEFAULT 42 NOT NULL, `created` TEXT DEFAULT CURRENT_TIMESTAMP NOT NULL);'
}
