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

fn test_orm_stmt_gen_h2_insert_default_values() {
	table := orm.Table{
		name: 'Test'
	}
	query, _ := orm.orm_stmt_gen(.h2, table, '"', .insert, false, '?', 1, orm.QueryData{},
		orm.QueryData{})
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

fn reset_tenant_filter() {
	orm.configure_tenant_filter(enabled: false, field_name: 'tenant_id')
	orm.clear_current_tenant_id()
}

fn test_apply_tenant_filter_appends_default_field() {
	defer {
		reset_tenant_filter()
	}
	reset_tenant_filter()
	orm.configure_tenant_filter(field_name: 'tenant_id')
	orm.set_current_tenant_id(77)
	where := orm.QueryData{
		fields: ['id']
		data:   [orm.Primitive(int(11))]
		kinds:  [.eq]
	}
	filtered := orm.apply_tenant_filter(orm.Table{
		name: 'test_table'
	}, where)
	assert filtered.fields == ['id', 'tenant_id']
	assert filtered.data == [orm.Primitive(int(11)), orm.Primitive(int(77))]
	assert filtered.kinds == [.eq, .eq]
	assert filtered.is_and == [true]
}

fn test_apply_tenant_filter_does_not_duplicate_existing_field() {
	defer {
		reset_tenant_filter()
	}
	reset_tenant_filter()
	orm.configure_tenant_filter(field_name: 'tenant_id')
	orm.set_current_tenant_id(77)
	where := orm.QueryData{
		fields: ['tenant_id']
		data:   [orm.Primitive(int(11))]
		kinds:  [.eq]
	}
	filtered := orm.apply_tenant_filter(orm.Table{
		name: 'test_table'
	}, where)
	assert filtered.fields == ['tenant_id']
	assert filtered.data == [orm.Primitive(int(11))]
	assert filtered.kinds == [.eq]
	assert filtered.is_and == []
}

fn test_apply_tenant_filter_with_table_override_and_ignore() {
	defer {
		reset_tenant_filter()
	}
	reset_tenant_filter()
	orm.configure_tenant_filter(field_name: 'tenant_id')
	orm.set_current_tenant_id(5)
	with_override := orm.apply_tenant_filter(orm.Table{
		name:  'test_table'
		attrs: [
			VAttribute{
				name:    'tenant_field'
				has_arg: true
				arg:     'company_id'
				kind:    .string
			},
		]
	}, orm.QueryData{})
	assert with_override.fields == ['company_id']
	assert with_override.data == [orm.Primitive(int(5))]
	ignored := orm.apply_tenant_filter(orm.Table{
		name:  'test_table'
		attrs: [
			VAttribute{
				name: 'ignore_tenant_filter'
			},
		]
	}, orm.QueryData{})
	assert ignored.fields == []
	assert ignored.data == []
}

fn test_apply_tenant_filter_wraps_existing_where_clause() {
	defer {
		reset_tenant_filter()
	}
	reset_tenant_filter()
	orm.configure_tenant_filter(field_name: 'tenant_id')
	orm.set_current_tenant_id(42)
	filtered := orm.apply_tenant_filter(orm.Table{
		name: 'test_table'
	}, orm.QueryData{
		fields: ['a', 'b']
		kinds:  [.eq, .eq]
		is_and: [false]
	})
	assert filtered.fields == ['a', 'b', 'tenant_id']
	assert filtered.is_and == [false, true]
	assert filtered.parentheses.len == 1
	assert filtered.parentheses[0] == [0, 1]
}

fn test_without_tenant_filter_and_with_tenant() {
	defer {
		reset_tenant_filter()
	}
	reset_tenant_filter()
	orm.configure_tenant_filter(field_name: 'tenant_id')
	orm.set_current_tenant_id(10)
	without_filter := orm.without_tenant_filter[orm.QueryData](fn () !orm.QueryData {
		return orm.apply_tenant_filter(orm.Table{
			name: 'test_table'
		}, orm.QueryData{})
	}) or { panic(err) }
	assert without_filter.fields == []

	with_override := orm.with_tenant[orm.QueryData](22, fn () !orm.QueryData {
		return orm.apply_tenant_filter(orm.Table{
			name: 'test_table'
		}, orm.QueryData{})
	}) or { panic(err) }
	assert with_override.fields == ['tenant_id']
	assert with_override.data == [orm.Primitive(int(22))]

	after_scope := orm.apply_tenant_filter(orm.Table{
		name: 'test_table'
	}, orm.QueryData{})
	assert after_scope.data == [orm.Primitive(int(10))]
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
