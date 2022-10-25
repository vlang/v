import orm
import v.ast

fn test_orm_stmt_gen_update() {
	query, _ := orm.orm_stmt_gen('Test', "'", .update, true, '?', 0, orm.QueryData{
		fields: ['test', 'a']
		data: []
		types: []
		kinds: []
	}, orm.QueryData{
		fields: ['id', 'name']
		data: []
		types: []
		kinds: [.ge, .eq]
	})
	assert query == "UPDATE 'Test' SET 'test' = ?0, 'a' = ?1 WHERE 'id' >= ?2 AND 'name' = ?3;"
}

fn test_orm_stmt_gen_insert() {
	query, _ := orm.orm_stmt_gen('Test', "'", .insert, true, '?', 0, orm.QueryData{
		fields: ['test', 'a']
		data: []
		types: []
		kinds: []
	}, orm.QueryData{})
	assert query == "INSERT INTO 'Test' ('test', 'a') VALUES (?0, ?1);"
}

fn test_orm_stmt_gen_delete() {
	query, _ := orm.orm_stmt_gen('Test', "'", .delete, true, '?', 0, orm.QueryData{
		fields: ['test', 'a']
		data: []
		types: []
		kinds: []
	}, orm.QueryData{
		fields: ['id', 'name']
		data: []
		types: []
		kinds: [.ge, .eq]
	})
	assert query == "DELETE FROM 'Test' WHERE 'id' >= ?0 AND 'name' = ?1;"
}

fn get_select_fields() []string {
	return ['id', 'test', 'abc']
}

fn test_orm_select_gen() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table: 'test_table'
		fields: get_select_fields()
	}, "'", true, '?', 0, orm.QueryData{})

	assert query == "SELECT 'id', 'test', 'abc' FROM 'test_table';"
}

fn test_orm_select_gen_with_limit() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table: 'test_table'
		fields: get_select_fields()
		has_limit: true
	}, "'", true, '?', 0, orm.QueryData{})

	assert query == "SELECT 'id', 'test', 'abc' FROM 'test_table' LIMIT ?0;"
}

fn test_orm_select_gen_with_where() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table: 'test_table'
		fields: get_select_fields()
		has_where: true
	}, "'", true, '?', 0, orm.QueryData{
		fields: ['abc', 'test']
		kinds: [.eq, .gt]
		is_and: [true]
	})

	assert query == "SELECT 'id', 'test', 'abc' FROM 'test_table' WHERE 'abc' = ?0 AND 'test' > ?1;"
}

fn test_orm_select_gen_with_order() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table: 'test_table'
		fields: get_select_fields()
		has_order: true
		order_type: .desc
	}, "'", true, '?', 0, orm.QueryData{})

	assert query == "SELECT 'id', 'test', 'abc' FROM 'test_table' ORDER BY '' DESC;"
}

fn test_orm_select_gen_with_offset() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table: 'test_table'
		fields: get_select_fields()
		has_offset: true
	}, "'", true, '?', 0, orm.QueryData{})

	assert query == "SELECT 'id', 'test', 'abc' FROM 'test_table' OFFSET ?0;"
}

fn test_orm_select_gen_with_all() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table: 'test_table'
		fields: get_select_fields()
		has_limit: true
		has_order: true
		order_type: .desc
		has_offset: true
		has_where: true
	}, "'", true, '?', 0, orm.QueryData{
		fields: ['abc', 'test']
		kinds: [.eq, .gt]
		is_and: [true]
	})

	assert query == "SELECT 'id', 'test', 'abc' FROM 'test_table' WHERE 'abc' = ?0 AND 'test' > ?1 ORDER BY '' DESC LIMIT ?2 OFFSET ?3;"
}

fn test_orm_table_gen() {
	query := orm.orm_table_gen('test_table', "'", true, 0, [
		orm.TableField{
			name: 'id'
			typ: ast.int_type_idx
			default_val: '10'
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
		orm.TableField{
			name: 'test'
			typ: ast.string_type_idx
		},
		orm.TableField{
			name: 'abc'
			typ: ast.i64_type_idx
			default_val: '6754'
		},
	], sql_type_from_v, false) or { panic(err) }
	assert query == "CREATE TABLE IF NOT EXISTS 'test_table' ('id' SERIAL DEFAULT 10, 'test' TEXT, 'abc' INT64 DEFAULT 6754, PRIMARY KEY('id'));"

	alt_query := orm.orm_table_gen('test_table', "'", true, 0, [
		orm.TableField{
			name: 'id'
			typ: ast.int_type_idx
			default_val: '10'
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
		orm.TableField{
			name: 'test'
			typ: ast.string_type_idx
		},
		orm.TableField{
			name: 'abc'
			typ: ast.i64_type_idx
			default_val: '6754'
		},
	], sql_type_from_v, true) or { panic(err) }
	assert alt_query == "IF NOT EXISTS (SELECT * FROM sysobjects WHERE name='test_table' and xtype='U') CREATE TABLE 'test_table' ('id' SERIAL DEFAULT 10, 'test' TEXT, 'abc' INT64 DEFAULT 6754, PRIMARY KEY('id'));"

	unique_query := orm.orm_table_gen('test_table', "'", true, 0, [
		orm.TableField{
			name: 'id'
			typ: ast.int_type_idx
			default_val: '10'
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
		orm.TableField{
			name: 'test'
			typ: ast.string_type_idx
			attrs: [
				StructAttribute{
					name: 'unique'
				},
			]
		},
		orm.TableField{
			name: 'abc'
			typ: ast.i64_type_idx
			default_val: '6754'
		},
	], sql_type_from_v, false) or { panic(err) }
	assert unique_query == "CREATE TABLE IF NOT EXISTS 'test_table' ('id' SERIAL DEFAULT 10, 'test' TEXT, 'abc' INT64 DEFAULT 6754, PRIMARY KEY('id'), UNIQUE('test'));"

	mult_unique_query := orm.orm_table_gen('test_table', "'", true, 0, [
		orm.TableField{
			name: 'id'
			typ: ast.int_type_idx
			default_val: '10'
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
		orm.TableField{
			name: 'test'
			typ: ast.string_type_idx
			attrs: [
				StructAttribute{
					name: 'unique'
					has_arg: true
					arg: 'test'
					kind: .string
				},
			]
		},
		orm.TableField{
			name: 'abc'
			typ: ast.i64_type_idx
			default_val: '6754'
			attrs: [
				StructAttribute{
					name: 'unique'
					has_arg: true
					arg: 'test'
					kind: .string
				},
			]
		},
	], sql_type_from_v, false) or { panic(err) }
	assert mult_unique_query == "CREATE TABLE IF NOT EXISTS 'test_table' ('id' SERIAL DEFAULT 10, 'test' TEXT, 'abc' INT64 DEFAULT 6754, /* test */UNIQUE('test', 'abc'), PRIMARY KEY('id'));"
}

fn sql_type_from_v(typ int) !string {
	return if typ in orm.nums {
		'INT'
	} else if typ in orm.num64 {
		'INT64'
	} else if typ in orm.float {
		'DOUBLE'
	} else if typ == orm.string {
		'TEXT'
	} else if typ == -1 {
		'SERIAL'
	} else {
		error('Unknown type $typ')
	}
}
