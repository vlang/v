import orm
import sqlite

fn test_sqlite_orm() {
	sdb := sqlite.connect(':memory:') or { panic(err) }
	db := orm.OrmConnection(sdb)
	db.create('Test', [
		orm.OrmTableField{
			name: 'id'
			typ: 7
			kind: .primitive
			attrs: [
				StructAttribute{
					name: 'primary'
				}
				StructAttribute{
					name: 'sql'
					has_arg: true
					kind: .plain
					arg: 'serial'
				}
			]
		}
		orm.OrmTableField{
			name: 'name'
			typ: 18
			kind: .primitive
			attrs: []
		}
		orm.OrmTableField{
			name: 'age'
			typ: 8
			kind: .primitive
		}
	]) or { panic(err) }

	db.insert('Test', orm.OrmQueryData{
			fields: ['name', 'age']
			types: [18, 8]
			data: [voidptr('Louis'.str), voidptr(&i64(100))]
		}
	) or { panic(err) }

	name_col := sdb.q_string('select name from Test where id = 1;')
	println('# name: $name_col')

	age_col := sdb.q_string('select age from Test where id = 1;')
	println('# age: $age_col')

	res := db.@select(orm.OrmSelectConfig{
		table: 'Test'
		has_where: true
		fields: ['id', 'name', 'age']
		types: [7, 18, 8]
	}, orm.OrmQueryData{}, orm.OrmQueryData{
		fields: ['name', 'age']
		data: [voidptr('Louis'.str), voidptr(i64(100))]
		types: [18, 8]
		kinds: [.eq, .eq]
	}) or { panic(err) }

	eprintln(res[0])

	assert res[0][0] == '1'
	assert res[0][1] == 'Louis'
	assert res[0][2] == '100'
}