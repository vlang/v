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
	
	name := voidptr('Louis'.str)

	db.insert('Test', orm.OrmQueryData{
			fields: ['name', 'age']
			types: [18, 8]
			data: [name, voidptr(&i64(100))]
		}
	) or { panic(err) }

	res := db.@select(orm.OrmSelectConfig{
		table: 'Test'
		has_where: true
		fields: ['id', 'name', 'age']
		types: [7, 18, 8]
	}, orm.OrmQueryData{}, orm.OrmQueryData{
		fields: ['name', 'age']
		data: [name, voidptr(i64(100))]
		types: [18, 8]
		kinds: [.eq, .eq]
	}) or { panic(err) }

	assert res[0] == ['1', 'Louis', '100']
}