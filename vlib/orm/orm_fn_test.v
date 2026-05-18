// vtest retry: 3
import orm

fn test_orm_stmt_gen_update() {
	table := orm.Table{
		name: 'Test'
	}
	query_and, _ := orm.orm_stmt_gen(.default, table, "'", .update, true, '?', 0, orm.QueryData{
		fields: ['test', 'a']
		data:   []
		types:  []
		kinds:  []
	}, orm.QueryData{
		fields: ['id', 'name']
		data:   []
		types:  []
		kinds:  [.ge, .eq]
		is_and: [true]
	})
	assert query_and == "UPDATE 'Test' SET 'test' = ?0, 'a' = ?1 WHERE 'id' >= ?2 AND 'name' = ?3;"

	query_or, _ := orm.orm_stmt_gen(.default, table, "'", .update, true, '?', 0, orm.QueryData{
		fields: ['test', 'a']
		data:   []
		types:  []
		kinds:  []
	}, orm.QueryData{
		fields: ['id', 'name']
		data:   []
		types:  []
		kinds:  [.ge, .eq]
		is_and: [false]
	})
	assert query_or == "UPDATE 'Test' SET 'test' = ?0, 'a' = ?1 WHERE 'id' >= ?2 OR 'name' = ?3;"
}

fn test_orm_stmt_gen_insert() {
	table := orm.Table{
		name: 'Test'
	}
	query, _ := orm.orm_stmt_gen(.default, table, "'", .insert, true, '?', 0, orm.QueryData{
		fields: ['test', 'a']
		data:   []
		types:  []
		kinds:  []
	}, orm.QueryData{})
	assert query == "INSERT INTO 'Test' ('test', 'a') VALUES (?0, ?1);"
}

fn test_orm_stmt_gen_bulk_insert() {
	table := orm.Table{
		name: 'Test'
	}
	query, converted := orm.orm_stmt_gen(.default, table, "'", .insert, true, '?', 0, orm.QueryData{
		fields:     ['name', 'age']
		data:       [orm.Primitive('Alice'), orm.Primitive(25), orm.Primitive('Bob'), orm.Primitive(30)]
		batch_rows: 2
	}, orm.QueryData{})
	assert query == "INSERT INTO 'Test' ('name', 'age') VALUES (?0, ?1), (?2, ?3);"
	assert converted.data.len == 4

	pg_query, _ := orm.orm_stmt_gen(.pg, table, '"', .insert, true, '$', 1, orm.QueryData{
		fields:     ['name', 'age']
		data:       [orm.Primitive('Alice'), orm.Primitive(25), orm.Primitive('Bob'), orm.Primitive(30)]
		batch_rows: 2
	}, orm.QueryData{})
	assert pg_query == 'INSERT INTO "Test" ("name", "age") VALUES ($1, $2), ($3, $4);'

	mysql_query, _ := orm.orm_stmt_gen(.mysql, table, '`', .insert, false, '?', 1, orm.QueryData{
		fields:     ['name', 'age']
		data:       [orm.Primitive('Alice'), orm.Primitive(25), orm.Primitive('Bob'), orm.Primitive(30)]
		batch_rows: 2
	}, orm.QueryData{})
	assert mysql_query == 'INSERT INTO `Test` (`name`, `age`) VALUES (?, ?), (?, ?);'
}

fn test_orm_stmt_gen_bulk_update() {
	table := orm.Table{
		name: 'Test'
	}
	query, _ := orm.orm_stmt_gen(.default, table, "'", .update, true, '?', 0, orm.QueryData{
		fields:     ['name', 'age']
		data:       [orm.Primitive(1), orm.Primitive('Alice'), orm.Primitive(2), orm.Primitive('Bob'),
			orm.Primitive(1), orm.Primitive(25), orm.Primitive(2), orm.Primitive(30)]
		batch_rows: 2
		batch_key:  'id'
	}, orm.QueryData{
		fields: ['id', 'id']
		data:   [orm.Primitive(1), orm.Primitive(2)]
		kinds:  [.eq, .eq]
		is_and: [false]
	})
	assert query == "UPDATE 'Test' SET 'name' = CASE 'id' WHEN ?0 THEN ?1 WHEN ?2 THEN ?3 ELSE 'name' END, 'age' = CASE 'id' WHEN ?4 THEN ?5 WHEN ?6 THEN ?7 ELSE 'age' END WHERE 'id' = ?8 OR 'id' = ?9;"

	pg_query, _ := orm.orm_stmt_gen(.pg, table, '"', .update, true, '$', 1, orm.QueryData{
		fields:     ['name']
		data:       [orm.Primitive(1), orm.Primitive('Alice'), orm.Primitive(2), orm.Primitive('Bob')]
		batch_rows: 2
		batch_key:  'id'
	}, orm.QueryData{
		fields: ['id', 'id']
		data:   [orm.Primitive(1), orm.Primitive(2)]
		kinds:  [.eq, .eq]
		is_and: [false]
	})
	assert pg_query == 'UPDATE "Test" SET "name" = CASE "id" WHEN $1 THEN $2 WHEN $3 THEN $4 ELSE "name" END WHERE "id" = $5 OR "id" = $6;'

	mysql_query, _ := orm.orm_stmt_gen(.mysql, table, '`', .update, false, '?', 1, orm.QueryData{
		fields:     ['name']
		data:       [orm.Primitive(1), orm.Primitive('Alice'), orm.Primitive(2), orm.Primitive('Bob')]
		batch_rows: 2
		batch_key:  'id'
	}, orm.QueryData{
		fields: ['id', 'id']
		data:   [orm.Primitive(1), orm.Primitive(2)]
		kinds:  [.eq, .eq]
		is_and: [false]
	})
	assert mysql_query == 'UPDATE `Test` SET `name` = CASE `id` WHEN ? THEN ? WHEN ? THEN ? ELSE `name` END WHERE `id` = ? OR `id` = ?;'
}

fn test_orm_stmt_gen_insert_default_values_pg() {
	table := orm.Table{
		name: 'Test'
	}
	query, converted := orm.orm_stmt_gen(.pg, table, "'", .insert, true, '$', 1, orm.QueryData{
		fields:      ['id', 'example']
		data:        [orm.Primitive(0), orm.Primitive('')]
		types:       []
		kinds:       []
		auto_fields: [0, 1]
	}, orm.QueryData{})
	assert query == "INSERT INTO 'Test' DEFAULT VALUES;"
	assert converted.fields.len == 0
	assert converted.data.len == 0
}

fn test_orm_stmt_gen_insert_default_values_mysql() {
	table := orm.Table{
		name: 'Test'
	}
	query, converted := orm.orm_stmt_gen(.mysql, table, '`', .insert, false, '?', 1, orm.QueryData{
		fields:      ['id']
		data:        [orm.Primitive(0)]
		auto_fields: [0]
	}, orm.QueryData{})
	assert query == 'INSERT INTO `Test` () VALUES ();'
	assert converted.fields.len == 0
	assert converted.data.len == 0

	bulk_query, bulk_converted := orm.orm_stmt_gen(.mysql, table, '`', .insert, false, '?', 1, orm.QueryData{
		fields:      ['id']
		data:        [orm.Primitive(0), orm.Primitive(0), orm.Primitive(0)]
		auto_fields: [0]
		batch_rows:  3
	}, orm.QueryData{})
	assert bulk_query == 'INSERT INTO `Test` () VALUES (), (), ();'
	assert bulk_converted.fields.len == 0
	assert bulk_converted.data.len == 0
}

fn test_orm_stmt_gen_h2_insert_default_values() {
	table := orm.Table{
		name: 'Test'
	}
	query, _ :=
		orm.orm_stmt_gen(.h2, table, '"', .insert, false, '?', 1, orm.QueryData{}, orm.QueryData{})
	assert query == 'INSERT INTO "Test" DEFAULT VALUES;'
}

fn test_orm_stmt_gen_delete() {
	table := orm.Table{
		name: 'Test'
	}
	query_and, _ := orm.orm_stmt_gen(.default, table, "'", .delete, true, '?', 0, orm.QueryData{
		fields: ['test', 'a']
		data:   []
		types:  []
		kinds:  []
	}, orm.QueryData{
		fields: ['id', 'name']
		data:   []
		types:  []
		kinds:  [.ge, .eq]
		is_and: [true]
	})
	assert query_and == "DELETE FROM 'Test' WHERE 'id' >= ?0 AND 'name' = ?1;"

	query_or, _ := orm.orm_stmt_gen(.default, table, "'", .delete, true, '?', 0, orm.QueryData{
		fields: ['test', 'a']
		data:   []
		types:  []
		kinds:  []
	}, orm.QueryData{
		fields: ['id', 'name']
		data:   []
		types:  []
		kinds:  [.ge, .eq]
		is_and: [false]
	})
	assert query_or == "DELETE FROM 'Test' WHERE 'id' >= ?0 OR 'name' = ?1;"
}

fn get_select_fields() []string {
	return ['id', 'test', 'abc']
}

fn test_orm_select_gen() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table:  orm.Table{
			name: 'test_table'
		}
		fields: get_select_fields()
	}, "'", true, '?', 0, orm.QueryData{})

	assert query == "SELECT 'id', 'test', 'abc' FROM 'test_table';"
}

fn test_orm_select_gen_with_limit() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table:     orm.Table{
			name: 'test_table'
		}
		fields:    get_select_fields()
		has_limit: true
	}, "'", true, '?', 0, orm.QueryData{})

	assert query == "SELECT 'id', 'test', 'abc' FROM 'test_table' LIMIT ?0;"
}

fn test_orm_select_gen_with_where() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table:     orm.Table{
			name: 'test_table'
		}
		fields:    get_select_fields()
		has_where: true
	}, "'", true, '?', 0, orm.QueryData{
		fields: ['abc', 'test']
		kinds:  [.eq, .gt]
		is_and: [true]
	})

	assert query == "SELECT 'id', 'test', 'abc' FROM 'test_table' WHERE 'abc' = ?0 AND 'test' > ?1;"
}

fn test_orm_select_gen_with_order() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table:      orm.Table{
			name: 'test_table'
		}
		fields:     get_select_fields()
		has_order:  true
		order_type: .desc
	}, "'", true, '?', 0, orm.QueryData{})

	assert query == "SELECT 'id', 'test', 'abc' FROM 'test_table' ORDER BY '' DESC;"
}

fn test_orm_select_gen_with_offset() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table:      orm.Table{
			name: 'test_table'
		}
		fields:     get_select_fields()
		has_offset: true
	}, "'", true, '?', 0, orm.QueryData{})

	assert query == "SELECT 'id', 'test', 'abc' FROM 'test_table' OFFSET ?0;"
}

fn test_orm_select_gen_with_all() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table:      orm.Table{
			name: 'test_table'
		}
		fields:     get_select_fields()
		has_limit:  true
		has_order:  true
		order_type: .desc
		has_offset: true
		has_where:  true
	}, "'", true, '?', 0, orm.QueryData{
		fields: ['abc', 'test']
		kinds:  [.eq, .gt]
		is_and: [true]
	})

	assert query == "SELECT 'id', 'test', 'abc' FROM 'test_table' WHERE 'abc' = ?0 AND 'test' > ?1 ORDER BY '' DESC LIMIT ?2 OFFSET ?3;"
}

fn test_orm_select_gen_with_distinct() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table:        orm.Table{
			name: 'test_table'
		}
		fields:       get_select_fields()
		has_distinct: true
	}, "'", true, '?', 0, orm.QueryData{})

	assert query == "SELECT DISTINCT 'id', 'test', 'abc' FROM 'test_table';"
}

fn test_orm_select_gen_with_distinct_and_where() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table:        orm.Table{
			name: 'test_table'
		}
		fields:       get_select_fields()
		has_distinct: true
		has_where:    true
	}, "'", true, '?', 0, orm.QueryData{
		fields: ['abc']
		kinds:  [.eq]
		is_and: []
	})

	assert query == "SELECT DISTINCT 'id', 'test', 'abc' FROM 'test_table' WHERE 'abc' = ?0;"
}

fn test_orm_select_gen_with_sum() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table:           orm.Table{
			name: 'test_table'
		}
		aggregate_kind:  .sum
		aggregate_field: 'age'
		fields:          ['age']
		types:           [orm.type_idx['int']]
	}, "'", true, '?', 0, orm.QueryData{})

	assert query == "SELECT SUM('age') FROM 'test_table';"
}

fn test_orm_select_gen_with_avg_and_where() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table:           orm.Table{
			name: 'test_table'
		}
		aggregate_kind:  .avg
		aggregate_field: 'score'
		fields:          ['score']
		types:           [orm.type_idx['f64']]
		has_where:       true
	}, "'", true, '?', 0, orm.QueryData{
		fields: ['abc']
		kinds:  [.eq]
		is_and: []
	})

	assert query == "SELECT AVG('score') FROM 'test_table' WHERE 'abc' = ?0;"
}

fn test_orm_select_gen_with_count() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table:          orm.Table{
			name: 'test_table'
		}
		aggregate_kind: .count
		types:          [orm.type_idx['int']]
	}, "'", true, '?', 0, orm.QueryData{})

	assert query == "SELECT COUNT(*) FROM 'test_table';"
}

fn test_orm_table_gen() {
	table := orm.Table{
		name: 'test_table'
	}
	query := orm.orm_table_gen(.default, table, "'", true, 0, [
		orm.TableField{
			name:        'id'
			typ:         typeof[int]().idx
			default_val: '10'
			nullable:    true
			attrs:       [
				VAttribute{
					name: 'primary'
				},
				VAttribute{
					name:    'sql'
					has_arg: true
					arg:     'serial'
					kind:    .plain
				},
			]
		},
		orm.TableField{
			name:     'test'
			typ:      typeof[string]().idx
			nullable: true
		},
		orm.TableField{
			name:        'abc'
			typ:         typeof[i64]().idx
			nullable:    true
			default_val: '6754'
		},
	], sql_type_from_v, false) or { panic(err) }
	assert query == "CREATE TABLE IF NOT EXISTS 'test_table' ('id' SERIAL DEFAULT 10, 'test' TEXT, 'abc' INT64 DEFAULT 6754, PRIMARY KEY('id'));"

	alt_query := orm.orm_table_gen(.default, table, "'", true, 0, [
		orm.TableField{
			name:        'id'
			typ:         typeof[int]().idx
			nullable:    true
			default_val: '10'
			attrs:       [
				VAttribute{
					name: 'primary'
				},
				VAttribute{
					name:    'sql'
					has_arg: true
					arg:     'serial'
					kind:    .plain
				},
			]
		},
		orm.TableField{
			name:     'test'
			typ:      typeof[string]().idx
			nullable: true
		},
		orm.TableField{
			name:        'abc'
			typ:         typeof[i64]().idx
			nullable:    true
			default_val: '6754'
		},
	], sql_type_from_v, true) or { panic(err) }
	assert alt_query == "IF NOT EXISTS (SELECT * FROM sysobjects WHERE name='test_table' and xtype='U') CREATE TABLE 'test_table' ('id' SERIAL DEFAULT 10, 'test' TEXT, 'abc' INT64 DEFAULT 6754, PRIMARY KEY('id'));"

	unique_query := orm.orm_table_gen(.default, table, "'", true, 0, [
		orm.TableField{
			name:        'id'
			typ:         typeof[int]().idx
			nullable:    true
			default_val: '10'
			attrs:       [
				VAttribute{
					name: 'primary'
				},
				VAttribute{
					name:    'sql'
					has_arg: true
					arg:     'serial'
					kind:    .plain
				},
			]
		},
		orm.TableField{
			name:  'test'
			typ:   typeof[string]().idx
			attrs: [
				VAttribute{
					name: 'unique'
				},
			]
		},
		orm.TableField{
			name:        'abc'
			typ:         typeof[i64]().idx
			default_val: '6754'
		},
	], sql_type_from_v, false) or { panic(err) }
	assert unique_query == "CREATE TABLE IF NOT EXISTS 'test_table' ('id' SERIAL DEFAULT 10, 'test' TEXT NOT NULL, 'abc' INT64 DEFAULT 6754 NOT NULL, PRIMARY KEY('id'), UNIQUE('test'));"

	mult_unique_query := orm.orm_table_gen(.default, table, "'", true, 0, [
		orm.TableField{
			name:        'id'
			typ:         typeof[int]().idx
			nullable:    true
			default_val: '10'
			attrs:       [
				VAttribute{
					name: 'primary'
				},
				VAttribute{
					name:    'sql'
					has_arg: true
					arg:     'serial'
					kind:    .plain
				},
			]
		},
		orm.TableField{
			name:     'test'
			typ:      typeof[string]().idx
			nullable: true
			attrs:    [
				VAttribute{
					name:    'unique'
					has_arg: true
					arg:     'test'
					kind:    .string
				},
			]
		},
		orm.TableField{
			name:        'abc'
			typ:         typeof[i64]().idx
			nullable:    true
			default_val: '6754'
			attrs:       [
				VAttribute{
					name:    'unique'
					has_arg: true
					arg:     'test'
					kind:    .string
				},
			]
		},
	], sql_type_from_v, false) or { panic(err) }
	assert mult_unique_query == "CREATE TABLE IF NOT EXISTS 'test_table' ('id' SERIAL DEFAULT 10, 'test' TEXT, 'abc' INT64 DEFAULT 6754, /* test */UNIQUE('test', 'abc'), PRIMARY KEY('id'));"

	table_with_unique := orm.Table{
		name:  'test_table'
		attrs: [
			VAttribute{
				name:    'unique_key'
				has_arg: true
				arg:     'test, abc'
				kind:    .string
			},
		]
	}
	table_unique_query := orm.orm_table_gen(.default, table_with_unique, "'", true, 0, [
		orm.TableField{
			name:        'id'
			typ:         typeof[int]().idx
			nullable:    true
			default_val: '10'
			attrs:       [
				VAttribute{
					name: 'primary'
				},
				VAttribute{
					name:    'sql'
					has_arg: true
					arg:     'serial'
					kind:    .plain
				},
			]
		},
		orm.TableField{
			name:     'test'
			typ:      typeof[string]().idx
			nullable: true
		},
		orm.TableField{
			name:        'abc'
			typ:         typeof[i64]().idx
			nullable:    true
			default_val: '6754'
		},
	], sql_type_from_v, false) or { panic(err) }
	assert table_unique_query == "CREATE TABLE IF NOT EXISTS 'test_table' ('id' SERIAL DEFAULT 10, 'test' TEXT, 'abc' INT64 DEFAULT 6754, UNIQUE('test', 'abc'), PRIMARY KEY('id'));"
}

fn test_orm_table_gen_h2() {
	table := orm.Table{
		name:  'test_table'
		attrs: [
			VAttribute{
				name:    'comment'
				has_arg: true
				arg:     'test table'
				kind:    .string
			},
		]
	}
	query := orm.orm_table_gen(.h2, table, '"', true, 0, [
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
					arg:     'serial'
					kind:    .plain
				},
			]
		},
		orm.TableField{
			name:  'name'
			typ:   typeof[string]().idx
			attrs: [
				VAttribute{
					name:    'comment'
					has_arg: true
					arg:     'display name'
					kind:    .string
				},
				VAttribute{
					name: 'index'
				},
			]
		},
	], sql_type_from_v, false) or { panic(err) }
	assert query == 'CREATE TABLE IF NOT EXISTS "test_table" ("id" SERIAL NOT NULL, "name" TEXT NOT NULL, PRIMARY KEY("id"));\nCOMMENT ON TABLE "test_table" IS \'test table\';\nCOMMENT ON COLUMN "test_table"."name" IS \'display name\';\nCREATE INDEX "idx_test_table" ON "test_table" ("name");'
}

fn sql_type_from_v(typ int) !string {
	return if typ in orm.nums {
		'INT'
	} else if typ in orm.num64 {
		'INT64'
	} else if typ in orm.float {
		'DOUBLE'
	} else if typ == orm.type_string {
		'TEXT'
	} else if typ == orm.serial {
		'SERIAL'
	} else {
		error('Unknown type ${typ}')
	}
}
