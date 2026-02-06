// vtest build: present_sqlite3?
import orm
import db.sqlite
import time

struct TestCustomSqlType {
	id      int    @[primary; sql: serial]
	custom  string @[sql_type: 'INTEGER']
	custom1 string @[sql_type: 'TEXT']
	custom2 string @[sql_type: 'REAL']
	custom3 string @[sql_type: 'NUMERIC']
	custom4 string
	custom5 int
	custom6 time.Time
}

struct TestDefaultAttribute {
	id          string @[primary; sql: serial]
	name        string
	created_at  ?string @[default: 'CURRENT_TIME']
	created_at1 ?string @[default: 'CURRENT_DATE']
	created_at2 ?string @[default: 'CURRENT_TIMESTAMP']
}

struct EntityToTest {
	id   int    @[notnull; sql_type: 'INTEGER']
	smth string @[notnull; sql_type: 'TEXT']
}

fn test_sqlite_orm() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}
	table := orm.Table{
		name: 'Test'
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
			name:  'name'
			typ:   typeof[string]().idx
			attrs: []
		},
		orm.TableField{
			name: 'age'
			typ:  typeof[i64]().idx
		},
	]) or { panic(err) }

	db.insert(table, orm.QueryData{
		fields: ['name', 'age']
		data:   [orm.string_to_primitive('Louis'), orm.i64_to_primitive(100)]
	}) or { panic(err) }

	res := db.select(orm.SelectConfig{
		table:     table
		has_where: true
		fields:    ['id', 'name', 'age']
		types:     [typeof[int]().idx, typeof[string]().idx, typeof[i64]().idx]
	}, orm.QueryData{}, orm.QueryData{
		fields: ['name', 'age']
		data:   [orm.Primitive('Louis'), i64(100)]
		types:  [typeof[string]().idx, typeof[i64]().idx]
		is_and: [true, true]
		kinds:  [.eq, .eq]
	}) or { panic(err) }

	id := res[0][0]
	name := res[0][1]
	age := res[0][2]

	assert id is int
	if id is int {
		assert id == 1
	}

	assert name is string
	if name is string {
		assert name == 'Louis'
	}

	assert age is i64
	if age is i64 {
		assert age == 100
	}

	/** test orm sql type
	* - verify if all type create by attribute sql_type has created
	*/

	sql db {
		create table TestCustomSqlType
	}!

	mut result_custom_sql := db.exec('
		pragma table_info(TestCustomSqlType);
	')!

	mut table_info_types_results := []string{}
	information_schema_custom_sql := ['INTEGER', 'INTEGER', 'TEXT', 'REAL', 'NUMERIC', 'TEXT',
		'INTEGER', 'INTEGER']

	for data_type in result_custom_sql {
		table_info_types_results << data_type.vals[2]
	}
	assert table_info_types_results == information_schema_custom_sql

	sql db {
		drop table TestCustomSqlType
	}!

	/** test default attribute
	*/

	sql db {
		create table TestDefaultAttribute
	}!

	mut result_default_sql := db.exec('
			pragma table_info(TestDefaultAttribute);
		')!

	mut information_schema_data_types_results := []string{}
	information_schema_default_sql := ['', '', 'CURRENT_TIME', 'CURRENT_DATE', 'CURRENT_TIMESTAMP']

	for data_type in result_default_sql {
		information_schema_data_types_results << data_type.vals[4]
	}
	assert information_schema_data_types_results == information_schema_default_sql

	test_default_attribute := TestDefaultAttribute{
		name: 'Hitalo'
	}

	sql db {
		insert test_default_attribute into TestDefaultAttribute
	}!

	test_default_attributes := sql db {
		select from TestDefaultAttribute limit 1
	}!

	result_test_default_attribute := test_default_attributes.first()
	assert result_test_default_attribute.name == 'Hitalo'
	assert test_default_attribute.created_at or { '' } == ''
	assert test_default_attribute.created_at1 or { '' } == ''
	assert test_default_attribute.created_at2 or { '' } == ''
	assert result_test_default_attribute.created_at or { '' }.len == 8 // HH:MM:SS
	assert result_test_default_attribute.created_at1 or { '' }.len == 10 // YYYY-MM-DD
	assert result_test_default_attribute.created_at2 or { '' }.len == 19 // YYYY-MM-DD HH:MM:SS

	sql db {
		drop table TestDefaultAttribute
	}!
}

fn test_get_affected_rows_count() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}

	db.exec('create table EntityToTest(
		id integer not null constraint tbl_pk primary key,
		smth  integer
	);')!

	fst := EntityToTest{
		id:   1
		smth: '1'
	}

	sql db {
		insert fst into EntityToTest
	} or { panic('first insert failed') }

	assert db.get_affected_rows_count() == 1

	snd := EntityToTest{
		id:   1
		smth: '2'
	}

	mut sndfailed := false
	sql db {
		insert snd into EntityToTest
	} or { sndfailed = true }

	assert db.get_affected_rows_count() == 0
	assert sndfailed

	all := sql db {
		select from EntityToTest
	}!
	assert 1 == all.len

	sql db {
		update EntityToTest set smth = '2' where id == 1
	}!
	assert db.get_affected_rows_count() == 1

	sql db {
		update EntityToTest set smth = '2' where id == 2
	}!
	assert db.get_affected_rows_count() == 0

	sql db {
		delete from EntityToTest where id == 2
	}!
	assert db.get_affected_rows_count() == 0

	sql db {
		delete from EntityToTest where id == 1
	}!
	assert db.get_affected_rows_count() == 1
}
