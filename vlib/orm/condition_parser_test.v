module orm

import db.sqlite

struct ConditionEmbedded {
	tenant int @[sql: 'tenant_id']
}

struct ConditionFirst {
	ConditionEmbedded
	id   int    @[primary]
	name string @[sql: 'first_name'; sql_select: 'upper(first_name)']
}

struct ConditionSecond {
	id      int    @[primary]
	name    string @[sql: 'second_name']
	enabled bool
}

fn test_shared_condition_parser_keeps_each_models_metadata_and_query_state() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	mut first := new_query[ConditionFirst](db)
	mut second := new_query[ConditionSecond](db)

	first.where('(name LIKE ? OR tenant = ?) AND id >= ?', 'a%', 7, 2)!
	second.where('name = ?', 'two')!.or_where('enabled IS NULL OR id IN ?', [2, 3])!
	first.where('id != ?', 9)!

	assert first.where.fields == ['first_name', 'tenant_id', 'id', 'id']
	assert first.where.data == [Primitive('a%'), 7, 2, 9]
	assert first.where.kinds == [.orm_like, .eq, .ge, .neq]
	assert first.where.is_and == [false, true, true]
	assert first.where.parentheses == [[0, 1]]

	assert second.where.fields == ['second_name', 'enabled', 'id']
	assert second.where.kinds == [.eq, .is_null, .in]
	assert second.where.is_and == [false, false]
	assert second.where.data[0] == Primitive('two')
	assert second.where.data[1] == Primitive([Primitive(2), 3])

	first.config.fields = ['first_name', 'id']
	first.prepare()!
	second.prepare()!
	assert first.config.fields == ['first_name', 'id']
	assert first.config.select_exprs == ['upper(first_name)', 'id']
	assert first.config.types == [type_string, type_idx['int']]
	assert second.config.fields == ['id', 'second_name', 'enabled']
	assert second.config.select_exprs == ['id', 'second_name', 'enabled']
	assert second.config.types == [type_idx['int'], type_string, type_idx['bool']]
}

fn test_shared_condition_parser_preserves_error_text_and_positions() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	mut query := new_query[ConditionSecond](db)
	conditions := ['', 'id = ?', 'id + ?', ')', 'id = ? XOR name = ?']
	params := [
		[]Primitive{},
		[]Primitive{},
		[Primitive(1)],
		[]Primitive{},
		[Primitive(1), 'x'],
	]
	expected := [
		'parse_conditions(): empty condition',
		'parse_conditions(): condition requires `1` params but got `0`\n\tid = ?\n\t^\n',
		'parse_conditions(): unsupported operator: `+`\n\tid + ?\n\t   ^\n',
		'parse_conditions: unexpected `)`\n\t)\n\t^\n',
		'parse_conditions(): unexpected `XOR`, maybe `AND`,`OR`\n\tid = ? XOR name = ?\n\t       ^\n',
	]
	for i, condition in conditions {
		query.reset()
		query.parse_conditions(condition, params[i]) or {
			assert err.msg() == expected[i]
			continue
		}
		assert false, 'expected an error for ${condition}'
	}

	query.reset()
	query.where.parentheses = [[0, -1]]
	query.prepare() or {
		assert err.msg() == 'prepare(): missing `)`'
		assert query.config.fields.len == 0
		return
	}
	assert false, 'expected an unmatched parenthesis error'
}
