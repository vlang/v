import orm
import sqlite

fn test_sqlite_orm() {
	sdb := sqlite.connect(':memory:') or { panic(err) }
	db := orm.OrmConnection(sdb)
	db.create('Test', [
		orm.OrmTableField{
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
		orm.OrmTableField{
			name: 'name'
			typ: 18
			attrs: []
		},
		orm.OrmTableField{
			name: 'age'
			typ: 8
		},
	]) or { panic(err) }

	db.insert('Test', orm.OrmQueryData{
		fields: ['name', 'age']
		data: [orm.string_to_primitive('Louis'), orm.i64_to_primitive(100)]
	}) or { panic(err) }

	res := db.@select(orm.OrmSelectConfig{
		table: 'Test'
		has_where: true
		fields: ['id', 'name', 'age']
		types: [7, 18, 8]
	}, orm.OrmQueryData{}, orm.OrmQueryData{
		fields: ['name', 'age']
		data: [orm.Primitive('Louis'), i64(100)]
		types: [18, 8]
		kinds: [.eq, .eq]
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
}

fn test_orm() {
	sdb := sqlite.connect(':memory:') or { panic(err) }
	db := orm.OrmConnection(sdb)

	db.create('userlist', [
		orm.OrmTableField{
			name: 'id'
			typ: 7
			attrs: [
				StructAttribute{
					name: 'primary'
				},
				StructAttribute{
					name: 'sql'
					has_arg: true
					arg: 'serial'
					kind: .plain
				},
			]
		},
		orm.OrmTableField{
			name: 'age'
			typ: 7
		},
		orm.OrmTableField{
			name: 'name'
			attrs: [
				StructAttribute{
					name: 'sql'
					has_arg: true
					arg: 'username'
					kind: .string
				},
			]
			typ: 18
		},
		orm.OrmTableField{
			name: 'is_customer'
			typ: 16
		},
	]) or { panic(err) }

	db.insert('userlist', orm.OrmQueryData{
		fields: ['age', 'username', 'is_customer']
		data: [orm.int_to_primitive(10), orm.string_to_primitive('Louis'),
			orm.bool_to_primitive(false),
		]
	}) or { panic(err) }

	db.drop('userlist') or { panic(err) }
}
