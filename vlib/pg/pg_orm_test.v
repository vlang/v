module main

import orm
import pg

fn test_pg_orm() {
	mut db := pg.connect(
		host: 'localhost'
		user: 'postgres'
		password: ''
		dbname: 'postgres'
	) or { panic(err) }

	db.create('Test', [
		orm.TableField{
			name: 'id'
			typ: 7
			attrs: [
				StructAttribute{
					name: 'primary'
				},
				StructAttribute{
					name: 'sql'
					has_arg: true
					kind: .plain
					arg: 'serial'
				},
			]
		},
		orm.TableField{
			name: 'name'
			typ: 18
			attrs: []
		},
		orm.TableField{
			name: 'age'
			typ: 7
		},
	]) or { panic(err) }

	db.insert('Test', orm.QueryData{
		fields: ['name', 'age']
		data: [orm.string_to_primitive('Louis'), orm.int_to_primitive(101)]
	}) or { panic(err) }

	res := db.@select(orm.SelectConfig{
		table: 'Test'
		has_where: true
		fields: ['id', 'name', 'age']
		types: [7, 18, 8]
	}, orm.QueryData{}, orm.QueryData{
		fields: ['name']
		data: [orm.Primitive('Louis'), i64(101)]
		types: [18]
		is_and: [true]
		kinds: [.eq]
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
		assert age == 101
	}
}
