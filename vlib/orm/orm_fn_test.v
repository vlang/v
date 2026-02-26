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

fn reset_tenant_filter_state() {
	orm.configure_tenant_filter(orm.TenantFilterConfig{})
	orm.clear_current_tenant_id()
	orm.enable_tenant_filter()
}

fn test_orm_tenant_filter_select_without_where() {
	reset_tenant_filter_state()
	defer {
		reset_tenant_filter_state()
	}
	orm.configure_tenant_filter(orm.TenantFilterConfig{
		field_name: 'tenant_id'
	})
	orm.set_current_tenant_id(42)

	cfg, where := orm.apply_tenant_filter_to_select_config(orm.SelectConfig{
		table:  orm.Table{
			name: 'tenant_table'
		}
		fields: get_select_fields()
	}, orm.QueryData{})
	query := orm.orm_select_gen(cfg, "'", true, '?', 0, where)
	assert query == "SELECT 'id', 'test', 'abc' FROM 'tenant_table' WHERE 'tenant_id' = ?0;"
	assert where.fields == ['tenant_id']
	assert where.kinds == [.eq]
	assert where.data == [orm.Primitive(42)]
}

fn test_orm_tenant_filter_select_with_existing_where() {
	reset_tenant_filter_state()
	defer {
		reset_tenant_filter_state()
	}
	orm.set_current_tenant_id(10)
	cfg, where := orm.apply_tenant_filter_to_select_config(orm.SelectConfig{
		table:     orm.Table{
			name: 'tenant_table'
		}
		fields:    get_select_fields()
		has_where: true
	}, orm.QueryData{
		fields: ['abc']
		data:   [orm.Primitive(1)]
		kinds:  [.eq]
	})
	query := orm.orm_select_gen(cfg, "'", true, '?', 0, where)
	assert query == "SELECT 'id', 'test', 'abc' FROM 'tenant_table' WHERE 'abc' = ?0 AND 'tenant_id' = ?1;"
	assert where.fields == ['abc', 'tenant_id']
	assert where.kinds == [.eq, .eq]
	assert where.is_and == [true]
	assert where.data == [orm.Primitive(1), orm.Primitive(10)]
}

fn test_orm_tenant_filter_table_override_and_ignore() {
	reset_tenant_filter_state()
	defer {
		reset_tenant_filter_state()
	}
	orm.set_current_tenant_id(8)
	overridden := orm.apply_tenant_filter(orm.Table{
		name:  'tenant_table'
		attrs: [
			VAttribute{
				name:    'tenant'
				has_arg: true
				arg:     'company_id'
				kind:    .string
			},
		]
	}, orm.QueryData{})
	assert overridden.fields == ['company_id']
	assert overridden.data == [orm.Primitive(8)]

	ignored := orm.apply_tenant_filter(orm.Table{
		name:  'tenant_table'
		attrs: [
			VAttribute{
				name:    'tenant_filter'
				has_arg: true
				arg:     'ignore'
				kind:    .string
			},
		]
	}, orm.QueryData{})
	assert ignored.fields == []
	assert ignored.data == []
}

fn test_orm_tenant_filter_scoped_disable_and_override() {
	reset_tenant_filter_state()
	defer {
		reset_tenant_filter_state()
	}
	orm.set_current_tenant_id(3)
	table := orm.Table{
		name: 'tenant_table'
	}
	hidden_where := orm.without_tenant_filter(fn [table] () !orm.QueryData {
		return orm.apply_tenant_filter(table, orm.QueryData{})
	}) or { panic(err) }
	assert hidden_where.fields == []
	assert hidden_where.data == []

	with_tenant_where := orm.with_tenant(7, fn [table] () !orm.QueryData {
		return orm.apply_tenant_filter(table, orm.QueryData{})
	}) or { panic(err) }
	assert with_tenant_where.fields == ['tenant_id']
	assert with_tenant_where.data == [orm.Primitive(7)]

	current_where := orm.apply_tenant_filter(table, orm.QueryData{})
	assert current_where.fields == ['tenant_id']
	assert current_where.data == [orm.Primitive(3)]
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
