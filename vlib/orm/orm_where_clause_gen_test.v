// Coverage for the WHERE half of `orm_stmt_gen`: which SQL keyword each
// `OperationKind` renders, how identifiers are quoted, how the parentheses
// groups in `QueryData` are emitted, and how placeholder numbering moves.
//
// Everything here is pure string building. `orm_stmt_gen` returns the SQL and
// the (possibly filtered) insert data, so the second return value is asserted
// only where it is what the caller consumes.
//
// The `keyword` column below was read off the running library, not from the
// enum member names: `OperationKind.to_str()` (orm.v:163) maps `orm_ilike` to
// `ILIKE` and `neq` to `<>`, so neither spelling can be guessed from `kind`.
import orm

// WhereOpRow joins an operation to the keyword it emits and to whether it
// consumes one entry of `QueryData.data`. The unary kinds (`is_null`,
// `is_not_null`) are `OperationKind.is_unary`, so they render a bare column.
struct WhereOpRow {
	kind     orm.OperationKind
	keyword  string
	consumes bool
}

const wcc_op_rows = [
	WhereOpRow{orm.OperationKind.neq, '<>', true},
	WhereOpRow{orm.OperationKind.eq, '=', true},
	WhereOpRow{orm.OperationKind.gt, '>', true},
	WhereOpRow{orm.OperationKind.lt, '<', true},
	WhereOpRow{orm.OperationKind.ge, '>=', true},
	WhereOpRow{orm.OperationKind.le, '<=', true},
	WhereOpRow{orm.OperationKind.orm_like, 'LIKE', true},
	WhereOpRow{orm.OperationKind.orm_ilike, 'ILIKE', true},
	WhereOpRow{orm.OperationKind.is_null, 'IS NULL', false},
	WhereOpRow{orm.OperationKind.is_not_null, 'IS NOT NULL', false},
	WhereOpRow{orm.OperationKind.in, 'IN', true},
	WhereOpRow{orm.OperationKind.not_in, 'NOT IN', true},
]

// wcc_delete builds a single-table DELETE and returns the emitted SQL.
fn wcc_delete(q string, num bool, qm string, start_pos int, where orm.QueryData) string {
	query, _ := orm.orm_stmt_gen(.default, orm.Table{
		name: 'Test'
	}, q, .delete, num, qm, start_pos, orm.QueryData{}, where)
	return query
}

fn test_every_operation_kind_renders_its_own_keyword() {
	for row in wcc_op_rows {
		mut where := orm.QueryData{
			fields: ['id']
			kinds:  [row.kind]
		}
		if row.consumes {
			where.data = [orm.Primitive(7)]
		}
		query := wcc_delete("'", false, '?', 0, where)
		expected := if row.consumes {
			"DELETE FROM 'Test' WHERE 'id' ${row.keyword} ?;"
		} else {
			"DELETE FROM 'Test' WHERE 'id' ${row.keyword};"
		}
		assert query == expected, '${row.kind}: got "${query}", want "${expected}"'
	}
}

fn test_every_operation_kind_renders_its_own_keyword_when_numbered() {
	for row in wcc_op_rows {
		mut where := orm.QueryData{
			fields: ['id']
			kinds:  [row.kind]
		}
		if row.consumes {
			where.data = [orm.Primitive(7)]
		}
		query := wcc_delete("'", true, '?', 0, where)
		expected := if row.consumes {
			"DELETE FROM 'Test' WHERE 'id' ${row.keyword} ?0;"
		} else {
			"DELETE FROM 'Test' WHERE 'id' ${row.keyword};"
		}
		assert query == expected, '${row.kind}: got "${query}", want "${expected}"'
	}
}

fn test_a_unary_kind_takes_no_placeholder_so_numbering_skips_it() {
	// A unary condition between two value conditions owns no slot, so the
	// counters run 0,1 rather than 0,1,2.
	query := wcc_delete("'", true, '?', 0, orm.QueryData{
		fields: ['a', 'gone', 'b']
		data:   [orm.Primitive(1), orm.Primitive(2)]
		kinds:  [.eq, .is_null, .eq]
		is_and: [true, true]
	})
	assert query == "DELETE FROM 'Test' WHERE 'a' = ?0 AND 'gone' IS NULL AND 'b' = ?1;"
}

fn test_the_quote_character_is_shared_by_the_table_and_every_column() {
	rows := [
		WhereQuoteRow{"'", "DELETE FROM 'Test' WHERE 'a' = ? AND 'b' = ?;"},
		WhereQuoteRow{'"', 'DELETE FROM "Test" WHERE "a" = ? AND "b" = ?;'},
		WhereQuoteRow{'`', 'DELETE FROM `Test` WHERE `a` = ? AND `b` = ?;'},
		WhereQuoteRow{'', 'DELETE FROM Test WHERE a = ? AND b = ?;'},
	]
	for row in rows {
		query := wcc_delete(row.quote, false, '?', 0, orm.QueryData{
			fields: ['a', 'b']
			kinds:  [.eq, .eq]
			is_and: [true]
		})
		assert query == row.expected, 'quote "${row.quote}": got "${query}"'
	}
}

struct WhereQuoteRow {
	quote    string
	expected string
}

fn test_one_parentheses_group_wraps_only_the_fields_it_names() {
	query := wcc_delete("'", true, '?', 0, orm.QueryData{
		fields:      ['a', 'b', 'c']
		data:        [orm.Primitive(1), orm.Primitive(2), orm.Primitive(3)]
		kinds:       [.eq, .eq, .eq]
		is_and:      [false, true]
		parentheses: [[0, 1]]
	})
	// Measured: the group is surrounded by a padding space on both sides, so
	// the opening parenthesis is separated from `WHERE` by two spaces.
	assert query == "DELETE FROM 'Test' WHERE  ( 'a' = ?0 OR 'b' = ?1 )  AND 'c' = ?2;"
}

fn test_two_parentheses_groups_keep_their_order() {
	query := wcc_delete("'", true, '?', 0, orm.QueryData{
		fields:      ['a', 'b', 'c', 'd']
		data:        [orm.Primitive(1), orm.Primitive(2), orm.Primitive(3), orm.Primitive(4)]
		kinds:       [.eq, .eq, .eq, .eq]
		is_and:      [false, false, true]
		parentheses: [[0, 1], [2, 3]]
	})
	// Measured: the closing parenthesis of the last group is emitted before the
	// statement's semicolon, so the SQL ends with " ;".
	assert query == "DELETE FROM 'Test' WHERE  ( 'a' = ?0 OR 'b' = ?1 )  OR  ( 'c' = ?2 AND 'd' = ?3 ) ;"
}

fn test_a_group_may_wrap_a_single_field() {
	query := wcc_delete("'", true, '?', 0, orm.QueryData{
		fields:      ['a', 'b']
		data:        [orm.Primitive(1), orm.Primitive(2)]
		kinds:       [.eq, .eq]
		is_and:      [false]
		parentheses: [[0, 0], [1, 1]]
	})
	assert query == "DELETE FROM 'Test' WHERE  ( 'a' = ?0 )  OR  ( 'b' = ?1 ) ;"
}

fn test_placeholder_numbering_continues_from_start_pos() {
	query := wcc_delete('"', true, '$', 5, orm.QueryData{
		fields: ['a', 'b']
		data:   [orm.Primitive(1), orm.Primitive([1, 2])]
		kinds:  [.eq, .in]
		is_and: [true]
	})
	// Measured: an IN array advances the counter once per element, so the first
	// field after it starts at position 7 rather than 6.
	assert query == 'DELETE FROM "Test" WHERE "a" = $5 AND "b" IN ($6, $7);'
}

fn test_not_in_expands_a_typed_int_array() {
	query := wcc_delete("'", true, '?', 0, orm.QueryData{
		fields: ['ids']
		data:   [orm.Primitive([1, 2, 3])]
		kinds:  [.not_in]
	})
	assert query == "DELETE FROM 'Test' WHERE 'ids' NOT IN (?0, ?1, ?2);"
}

fn test_in_expands_a_typed_string_array() {
	query := wcc_delete("'", false, '?', 0, orm.QueryData{
		fields: ['names']
		data:   [orm.Primitive(['a', 'b'])]
		kinds:  [.in]
	})
	assert query == "DELETE FROM 'Test' WHERE 'names' IN (?, ?);"
}

fn test_in_expands_an_array_of_primitives() {
	query := wcc_delete("'", true, '?', 0, orm.QueryData{
		fields: ['ids']
		data:   [orm.Primitive([orm.Primitive(4), orm.Primitive(5)])]
		kinds:  [.in]
	})
	assert query == "DELETE FROM 'Test' WHERE 'ids' IN (?0, ?1);"
}

fn test_an_empty_array_expands_to_an_empty_placeholder_list() {
	// NOTE: `IN ()` is not valid on most engines. Measured current behaviour:
	// the guard in `gen_where_clause` only checks the *kind*, not that the array
	// has members, so an empty `[]int` still expands.
	query := wcc_delete("'", true, '?', 0, orm.QueryData{
		fields: ['ids']
		data:   [orm.Primitive([]int{})]
		kinds:  [.in]
	})
	assert query == "DELETE FROM 'Test' WHERE 'ids' IN ();"
}

fn test_in_with_a_scalar_value_emits_a_bare_placeholder() {
	// NOTE: only an array-shaped value gets a parenthesised list. A scalar bound
	// to `.in` renders `IN ?` with no parentheses, which is invalid SQL on every
	// named engine. The DataScope path guards this with
	// `filter_value_matches_operator` (orm_scope.v:187); `gen_where_clause` does
	// not, so the guard is the caller's job.
	query := wcc_delete("'", true, '?', 0, orm.QueryData{
		fields: ['id']
		data:   [orm.Primitive(7)]
		kinds:  [.in]
	})
	assert query == "DELETE FROM 'Test' WHERE 'id' IN ?0;"
}

fn test_a_delete_without_where_fields_still_emits_the_keyword() {
	// NOTE: `DELETE FROM 'Test' WHERE ;` is emitted rather than omitting the
	// keyword. No caller in this module reaches it, because the `sql` block
	// omits a WHERE-less delete, but the string builder does not check.
	query := wcc_delete("'", true, '?', 0, orm.QueryData{})
	assert query == "DELETE FROM 'Test' WHERE ;"
}

fn test_adjacent_conditions_join_with_and_or_or() {
	and_query := wcc_delete("'", false, '?', 0, orm.QueryData{
		fields: ['a', 'b']
		kinds:  [.eq, .eq]
		is_and: [true]
	})
	assert and_query == "DELETE FROM 'Test' WHERE 'a' = ? AND 'b' = ?;"

	or_query := wcc_delete("'", false, '?', 0, orm.QueryData{
		fields: ['a', 'b']
		kinds:  [.eq, .eq]
		is_and: [false]
	})
	assert or_query == "DELETE FROM 'Test' WHERE 'a' = ? OR 'b' = ?;"
}

fn test_update_shares_the_placeholder_counter_with_its_set_clause() {
	// The SET clause consumes one placeholder per field before the WHERE starts,
	// so numbering must not restart at `start_pos`.
	query, _ := orm.orm_stmt_gen(.default, orm.Table{
		name: 'Test'
	}, "'", .update, true, '?', 0, orm.QueryData{
		fields: ['a', 'b']
	}, orm.QueryData{
		fields: ['c', 'd']
		kinds:  [.eq, .eq]
		is_and: [true]
	})
	assert query == "UPDATE 'Test' SET 'a' = ?0, 'b' = ?1 WHERE 'c' = ?2 AND 'd' = ?3;"
}
