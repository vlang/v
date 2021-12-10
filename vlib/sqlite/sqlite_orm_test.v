import orm
import sqlite
import v.ast

fn test_sqlite_orm() {
	sdb := sqlite.connect(':memory:') or { panic(err) }
	db := orm.Connection(sdb)
	db.create('Test', [
		orm.TableField{
			name: 'id'
			typ: ast.int_type_idx
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
			typ: ast.string_type_idx
			attrs: []
		},
		orm.TableField{
			name: 'age'
			typ: ast.i64_type_idx
		},
	]) or { panic(err) }

	db.insert('Test', orm.QueryData{
		fields: ['name', 'age']
		data: [orm.string_to_primitive('Louis'), orm.i64_to_primitive(100)]
	}) or { panic(err) }

	res := db.@select(orm.SelectConfig{
		table: 'Test'
		has_where: true
		fields: ['id', 'name', 'age']
		types: [ast.int_type_idx, ast.string_type_idx, ast.i64_type_idx]
	}, orm.QueryData{}, orm.QueryData{
		fields: ['name', 'age']
		data: [orm.Primitive('Louis'), i64(100)]
		types: [ast.string_type_idx, ast.i64_type_idx]
		is_and: [true, true]
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
