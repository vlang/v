import orm
import mysql

fn test_mysql_orm() {
	mut mdb := mysql.Connection{
		host: 'localhost'
		port: 3306
		username: 'root'
		password: 'L[ypGVQ95x[;U/Tk'
		dbname: 'mysql'
	}
	mdb.connect() or { panic(err) }
	db := orm.OrmConnection(mdb)
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
			typ: 7
		},
	]) or { panic(err) }

	db.insert('Test', orm.OrmQueryData{
		fields: ['name', 'age']
		data: [orm.string_to_primitive('Louis'), orm.int_to_primitive(101)]
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
