// Coverage for the parts of `orm_select_gen` the existing `orm_fn_test.v`
// suites do not reach: `SelectConfig.joins` (every `JoinType`), the
// `AggregateKind.min`/`.max` spellings, `OrderType.asc` with a real column, and
// `select_exprs` for a raw expression and for a table-qualified field.
//
// Every expectation is a generated SQL string, so there is nothing to stub:
// the function never touches a connection.
import orm

// select_users is the smallest config that produces a column list.
fn select_users() orm.SelectConfig {
	return orm.SelectConfig{
		table:  orm.Table{
			name: 'user'
		}
		fields: ['id', 'name']
	}
}

// joined adds a single JOIN to `cfg` and returns the generated statement.
fn joined(cfg orm.SelectConfig, kind orm.JoinType, right string, left_col string, right_col string, left_table string) string {
	return orm.orm_select_gen(orm.SelectConfig{
		...cfg
		joins: [
			orm.JoinConfig{
				kind:          kind
				table:         orm.Table{
					name: right
				}
				on_left_table: left_table
				on_left_col:   left_col
				on_right_col:  right_col
			},
		]
	}, "'", true, '?', 0, orm.QueryData{})
}

fn test_join_every_kind_renders_its_own_keyword() {
	// Measured: `JoinType.to_str()` decides the keyword, and the ON clause
	// always quotes the table, the left column, the right table and the right
	// column, whatever the join kind.
	assert joined(select_users(), .inner, 'post', 'id', 'user_id', '') == "SELECT 'id', 'name' FROM 'user' INNER JOIN 'post' ON 'user'.'id' = 'post'.'user_id';"
	assert joined(select_users(), .left, 'post', 'id', 'user_id', '') == "SELECT 'id', 'name' FROM 'user' LEFT JOIN 'post' ON 'user'.'id' = 'post'.'user_id';"
	assert joined(select_users(), .right, 'post', 'id', 'user_id', '') == "SELECT 'id', 'name' FROM 'user' RIGHT JOIN 'post' ON 'user'.'id' = 'post'.'user_id';"
	assert joined(select_users(), .full_outer, 'tag', 'id', 'user_id', '') == "SELECT 'id', 'name' FROM 'user' FULL OUTER JOIN 'tag' ON 'user'.'id' = 'tag'.'user_id';"
}

fn test_join_without_an_explicit_left_table_uses_the_config_table() {
	// Measured: an empty `on_left_table` falls back to `cfg.table.name`, so the
	// joined table is resolved against the table being selected from.
	assert joined(select_users(), .inner, 'post', 'id', 'user_id', '') == "SELECT 'id', 'name' FROM 'user' INNER JOIN 'post' ON 'user'.'id' = 'post'.'user_id';"
}

fn test_join_with_an_explicit_left_table_uses_the_name_it_was_given() {
	assert joined(select_users(), .left, 'post', 'id', 'user_id', 'account') == "SELECT 'id', 'name' FROM 'user' LEFT JOIN 'post' ON 'account'.'id' = 'post'.'user_id';"
}

fn test_join_is_emitted_between_the_from_and_the_where() {
	cfg := orm.SelectConfig{
		...select_users()
		has_where: true
		has_order: true
		order:     'name'
		joins:     [
			orm.JoinConfig{
				kind:         .inner
				table:        orm.Table{
					name: 'post'
				}
				on_left_col:  'id'
				on_right_col: 'user_id'
			},
		]
	}
	query := orm.orm_select_gen(cfg, "'", true, '?', 0, orm.QueryData{
		fields: ['name']
		kinds:  [.eq]
	})
	assert query == "SELECT 'id', 'name' FROM 'user' INNER JOIN 'post' ON 'user'.'id' = 'post'.'user_id' WHERE 'name' = ?0 ORDER BY 'name' ASC;"
}

fn test_two_joins_are_emitted_in_order() {
	cfg := orm.SelectConfig{
		...select_users()
		joins: [
			orm.JoinConfig{
				kind:         .inner
				table:        orm.Table{
					name: 'post'
				}
				on_left_col:  'id'
				on_right_col: 'user_id'
			},
			orm.JoinConfig{
				kind:          .full_outer
				table:         orm.Table{
					name: 'tag'
				}
				on_left_table: 'post'
				on_left_col:   'id'
				on_right_col:  'post_id'
			},
		]
	}
	query := orm.orm_select_gen(cfg, "'", true, '?', 0, orm.QueryData{})
	// Measured: both joins resolve their left table against the config table
	// when `on_left_table` is empty, and against the given name when it is not.
	assert query == "SELECT 'id', 'name' FROM 'user' INNER JOIN 'post' ON 'user'.'id' = 'post'.'user_id' FULL OUTER JOIN 'tag' ON 'post'.'id' = 'tag'.'post_id';"
}

fn test_join_quote_character_is_shared_with_the_rest_of_the_query() {
	cfg := orm.SelectConfig{
		...select_users()
		joins: [
			orm.JoinConfig{
				kind:         .left
				table:        orm.Table{
					name: 'post'
				}
				on_left_col:  'id'
				on_right_col: 'user_id'
			},
		]
	}
	assert orm.orm_select_gen(cfg, '`', true, '?', 0, orm.QueryData{}) == 'SELECT `id`, `name` FROM `user` LEFT JOIN `post` ON `user`.`id` = `post`.`user_id`;'
}

fn test_aggregate_kind_decides_the_projection() {
	for kind in [orm.AggregateKind.none, .count, .sum, .avg, .min, .max] {
		mut want := ''
		match kind {
			.none {
				want = "SELECT 'age' FROM 'user';"
			}
			.count {
				want = 'SELECT COUNT(*) FROM '
			}
			else {
				// The enum member name is lowercase; the emitted SQL keyword is upper.
				want = 'SELECT ${kind.str().to_upper()}('
			}
		}
		query := orm.orm_select_gen(orm.SelectConfig{
			table:           orm.Table{
				name: 'user'
			}
			aggregate_kind:  kind
			aggregate_field: 'age'
			fields:          ['age']
		}, "'", true, '?', 0, orm.QueryData{})
		if kind == .none || kind == .count {
			if kind == .none {
				assert query == want
			} else {
				assert query == want + "'user';", '${kind}: ${query}'
			}
		} else {
			assert query == want + "'age') FROM 'user';", '${kind}: ${query}'
		}
	}
}

fn test_aggregate_min_and_max_use_the_uppercase_spelling() {
	assert orm.orm_select_gen(orm.SelectConfig{
		table:           orm.Table{
			name: 'user'
		}
		aggregate_kind:  .min
		aggregate_field: 'age'
		fields:          ['age']
	}, "'", true, '?', 0, orm.QueryData{}) == "SELECT MIN('age') FROM 'user';"

	assert orm.orm_select_gen(orm.SelectConfig{
		table:           orm.Table{
			name: 'user'
		}
		aggregate_kind:  .max
		aggregate_field: 'age'
		fields:          ['age']
	}, "'", true, '?', 0, orm.QueryData{}) == "SELECT MAX('age') FROM 'user';"
}

fn test_aggregate_field_may_be_table_qualified() {
	// Measured: a `table::v_orm_table::column` aggregate field is split and both
	// halves are quoted, which keeps a column name containing a dot intact.
	query := orm.orm_select_gen(orm.SelectConfig{
		table:           orm.Table{
			name: 'user'
		}
		aggregate_kind:  .sum
		aggregate_field: 'user::v_orm_table::age'
		fields:          ['age']
	}, "'", true, '?', 0, orm.QueryData{})
	assert query == "SELECT SUM('user'.'age') FROM 'user';"
}

fn test_count_ignores_the_field_list() {
	query := orm.orm_select_gen(orm.SelectConfig{
		table:          orm.Table{
			name: 'user'
		}
		aggregate_kind: .count
		has_distinct:   true
		fields:         ['age']
	}, "'", true, '?', 0, orm.QueryData{})
	// Measured: `has_distinct` still reaches the projection, so a distinct
	// count is expressed rather than silently dropped.
	assert query == "SELECT DISTINCT COUNT(*) FROM 'user';"
}

fn test_asc_order_renders_the_asc_keyword() {
	// Measured: `orm_fn_test.v` only exercises `.desc`, with an empty `order`
	// column; the pair of a real column and the default direction is what the
	// chained callers actually generate.
	query := orm.orm_select_gen(orm.SelectConfig{
		...select_users()
		has_order:  true
		order:      'name'
		order_type: .asc
	}, "'", true, '?', 0, orm.QueryData{})
	assert query == "SELECT 'id', 'name' FROM 'user' ORDER BY 'name' ASC;"
}

fn test_desc_order_renders_the_desc_keyword_for_a_named_column() {
	query := orm.orm_select_gen(orm.SelectConfig{
		...select_users()
		has_order:  true
		order:      'name'
		order_type: .desc
	}, "'", true, '?', 0, orm.QueryData{})
	assert query == "SELECT 'id', 'name' FROM 'user' ORDER BY 'name' DESC;"
}

fn test_order_column_may_be_table_qualified() {
	query := orm.orm_select_gen(orm.SelectConfig{
		...select_users()
		has_order:  true
		order:      'post::v_orm_table::created_at'
		order_type: .asc
	}, "'", true, '?', 0, orm.QueryData{})
	assert query == "SELECT 'id', 'name' FROM 'user' ORDER BY 'post'.'created_at' ASC;"
}

fn test_placeholders_without_numbering() {
	// Measured: `num: false` leaves every placeholder bare and the limit and
	// offset share one shape, which is what the drivers that bind by position
	// need.
	query := orm.orm_select_gen(orm.SelectConfig{
		...select_users()
		has_where:  true
		has_limit:  true
		has_offset: true
	}, "'", false, '?', 0, orm.QueryData{
		fields: ['name']
		kinds:  [.eq]
	})
	assert query == "SELECT 'id', 'name' FROM 'user' WHERE 'name' = ? LIMIT ? OFFSET ?;"
}

fn test_select_exprs_override_the_column_list() {
	cfg := orm.SelectConfig{
		table:        orm.Table{
			name: 'user'
		}
		fields:       ['id', 'age', 'note']
		select_exprs: ['id', 'user::v_orm_table::age', 'COUNT(*)']
	}
	query := orm.orm_select_gen(cfg, "'", true, '?', 0, orm.QueryData{})
	// Measured: a select expr equal to the field name is quoted as a column, a
	// table-qualified one is split and quoted, and anything else is emitted
	// verbatim so raw SQL can be projected.
	assert query == "SELECT 'id', 'user'.'age', COUNT(*) FROM 'user';"
}

fn test_select_expr_shorter_than_the_field_list_keeps_the_remaining_columns() {
	cfg := orm.SelectConfig{
		table:        orm.Table{
			name: 'user'
		}
		fields:       ['id', 'name']
		select_exprs: ['UPPER(id)']
	}
	query := orm.orm_select_gen(cfg, "'", true, '?', 0, orm.QueryData{})
	assert query == "SELECT UPPER(id), 'name' FROM 'user';"
}
