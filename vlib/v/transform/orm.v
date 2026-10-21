module transform

import v.flat
import v.types

const sql_transform_table_qualified_field_separator = '::v_orm_table::'

struct SqlTransformTableInfo {
	name   u32
	fields []types.StructField
}

enum SqlTransformStmtKind {
	create
	drop
	insert
	upsert
	select
	count
	aggregate
	update
	delete
}

enum SqlDynamicDataKind {
	where_
	set_
}

struct SqlTransformSet {
	field string
	value string
}

// SqlValueMemberChain is a value ending in member accesses on a receiver that is not a
// plain name, e.g. `time.now().format_ss()`, `f(x).field` or `items[0].name`.
struct SqlValueMemberChain {
	receiver string
	members  []string
	is_call  bool
	args     []string
}

struct SqlTransformJoin {
	kind        string
	left_table  SqlTransformTableInfo
	table       SqlTransformTableInfo
	left_field  string
	right_field string
}

struct SqlTransformStmt {
	kind            SqlTransformStmtKind
	table           SqlTransformTableInfo
	value_name      string
	insert_many     bool
	distinct        bool
	is_dynamic      bool
	sets            []SqlTransformSet
	dynamic_set     SqlTransformDynamicData
	fields          []string
	joins           []SqlTransformJoin
	where           SqlTransformWhere
	order_field     string
	order_desc      bool
	limit           string
	offset          string
	aggregate       string
	aggregate_field string
}

struct SqlTransformDynamicData {
	name   string
	tokens []string
}

struct SqlDynamicIfItem {
	cond []string
	body []string
}

struct SqlDynamicTokenOp {
	idx int
	op  string
}

struct SqlDynamicGuard {
	name string
	expr []string
}

struct SqlTransformWhere {
	condition    string
	params       []SqlTransformWhereParam
	items        []SqlTransformWhereParam
	is_and       []bool
	parentheses  [][]int
	dynamic_data SqlTransformDynamicData
	error        string
}

struct SqlTransformWhereParam {
	field string
	op    string
	value string
}

struct SqlTransformWhereScope {
	table   SqlTransformTableInfo
	joined  []SqlTransformTableInfo
	qualify bool
}

struct SqlTransformResolvedWhereField {
	table  SqlTransformTableInfo
	field  string
	column string
	name   string
}

struct SqlAggregateResultInfo {
	optional_type u32
	method        string
}

// transform_sql_expr lowers SQL/ORM expressions to normal ORM helper calls.
fn (mut t Transformer) transform_sql_expr(_id flat.NodeId, node flat.Node) flat.NodeId {
	tokens := sql_clean_tokens(node.value.split(' '))
	if tokens.len > 0 && tokens[0] == 'querydata' {
		if message := sql_dynamic_invalid_predicate(tokens[1..]) {
			t.record_monomorph_error(message)
		}
		return t.make_struct_init(flat.source_type_id('orm.QueryData'))
	}
	stmts := t.parse_sql_transform_stmts(tokens) or { return t.sql_fallback_value(_id, node) }
	if stmts.len == 0 || node.children_count == 0 {
		return t.sql_fallback_value(_id, node)
	}
	db_id := t.a.child(&node, 0)
	t.mark_sql_db_methods(db_id)
	db_expr := if stmts.len > 1 {
		t.make_stable_sql_db_expr(db_id)
	} else {
		db_id
	}
	if stmts.len > 1 {
		if multi := t.transform_multi_sql_expr(stmts, db_expr) {
			return multi
		}
	}
	mut last := flat.empty_node
	for i, stmt in stmts {
		call := t.sql_stmt_call(stmt, db_expr) or { return t.sql_fallback_value(_id, node) }
		transformed_call := t.transform_expr(call)
		if i + 1 < stmts.len {
			t.pending_stmts << t.make_expr_stmt(transformed_call)
		} else {
			last = transformed_call
		}
	}
	if int(last) >= 0 {
		return last
	}
	return t.sql_fallback_value(_id, node)
}

fn (mut t Transformer) transform_multi_sql_expr(stmts []SqlTransformStmt, db_expr flat.NodeId) ?flat.NodeId {
	if stmts.len == 0 {
		return none
	}
	result_type_source_id := t.sql_stmt_result_type(stmts[stmts.len - 1])

	result_name := t.new_temp('sql_result')
	ok_name := t.new_temp('sql_ok')
	err_name := t.new_temp('sql_err')
	t.pending_stmts << t.make_decl_assign_typed(result_name, t.make_optional_none(result_type_source_id),
		result_type_source_id)
	t.pending_stmts << t.make_decl_assign_typed(ok_name, t.make_bool_literal(true), flat.source_type_bool)
	t.pending_stmts << t.make_decl_assign_typed(err_name, t.make_ierror_none(), flat.source_type_id('IError'))
	for i, stmt in stmts {
		pending_start := t.pending_stmts.len
		call := t.sql_stmt_call(stmt, db_expr) or {
			t.pending_stmts = t.pending_stmts[..pending_start].clone()
			return none
		}
		mut body := t.pending_stmts[pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..pending_start].clone()
		transform_pending_start := t.pending_stmts.len
		transformed_call := t.transform_expr(call)
		body << t.pending_stmts[transform_pending_start..].clone()
		t.pending_stmts = t.pending_stmts[..transform_pending_start].clone()
		if i + 1 == stmts.len {
			body << t.make_assign(t.make_ident(result_name), transformed_call)
		} else {
			stmt_type_source_id := t.sql_stmt_result_type(stmt)

			stmt_name := t.new_temp('sql_stmt')
			body << t.make_decl_assign_typed(stmt_name, transformed_call, stmt_type_source_id)
			fail_body := [
				t.make_assign(t.make_ident(ok_name), t.make_bool_literal(false)),
				t.make_assign(t.make_ident(err_name), t.make_selector(t.make_ident(stmt_name),
					'err', flat.source_type_id('IError'))),
			]
			body << t.make_if(t.make_prefix(.not, t.make_selector(t.make_ident(stmt_name), 'ok',
				flat.source_type_bool)), t.make_block(fail_body), t.make_empty())
		}
		t.pending_stmts << t.make_if(t.make_ident(ok_name), t.make_block(body), t.make_empty())
	}
	t.pending_stmts << t.make_if(t.make_prefix(.not, t.make_ident(ok_name)), t.make_block([
		t.make_assign(t.make_ident(result_name), t.make_optional_none_with_err(result_type_source_id,
			t.make_ident(err_name))),
	]), t.make_empty())
	return t.make_ident(result_name)
}

fn (mut t Transformer) transform_sql_aggregate_or_expr(_id flat.NodeId, node flat.Node) ?flat.NodeId {
	if node.value != '!' || node.children_count < 1 {
		return none
	}
	expr_id := t.a.child(&node, 0)
	if int(expr_id) < 0 || int(expr_id) >= t.a.nodes.len {
		return none
	}
	expr_node := t.a.nodes[int(expr_id)]
	info := t.sql_node_aggregate_result_info(expr_node) or { return none }
	aggregate_value := t.lower_or_expr_to_temp(_id, node)
	t.mark_fn_used_name('orm.AggregateValue.${info.method}')
	selector := t.make_selector(aggregate_value, info.method, u32(0))
	return t.make_call_expr_typed(selector, []flat.NodeId{}, info.optional_type)
}

fn (mut t Transformer) sql_fallback_value(_id flat.NodeId, node flat.Node) flat.NodeId {
	checker_typ_source_id := t.raw_checker_node_type(_id)
	checker_typ_source_id_2 := checker_typ_source_id

	typ := (if checker_typ_source_id != 0 {
		checker_typ_source_id_2
	} else if node.typ != 0 {
		node.typ
	} else {
		flat.source_type_id('!void')
	})
	if t.is_optional_type_name(typ) {
		opt_type_source_id := t.qualify_optional_type(typ)

		value_type_source_id_2 := t.optional_base_type(opt_type_source_id)

		value := if value_type_source_id_2 != 0 && value_type_source_id_2 != flat.source_type_void {
			t.zero_value_for_type(value_type_source_id_2)
		} else {
			flat.empty_node
		}
		return t.make_optional_some(value, opt_type_source_id)
	}
	return t.zero_value_for_type(typ)
}

fn sql_node_is_aggregate_expr(node flat.Node) bool {
	if node.kind != .sql_expr {
		return false
	}
	tokens := sql_clean_tokens(node.value.split(' '))
	mut select_start := 1
	if tokens.len > 1 && tokens[1] == 'distinct' {
		select_start = 2
	}
	return tokens.len > select_start && tokens[0] == 'select'
		&& tokens[select_start] in ['sum', 'avg', 'min', 'max']
}

fn (mut t Transformer) sql_node_aggregate_result_info(node flat.Node) ?SqlAggregateResultInfo {
	if node.kind != .sql_expr {
		return none
	}
	tokens := sql_clean_tokens(node.value.split(' '))
	stmts := t.parse_sql_transform_stmts(tokens) or { return none }
	if stmts.len == 0 {
		return none
	}
	stmt := stmts[stmts.len - 1]
	if stmt.kind != .aggregate {
		return none
	}
	field_type := sql_transform_field_type(stmt.table, stmt.aggregate_field)
	if field_type == 0 {
		return none
	}
	return sql_aggregate_result_info(stmt.aggregate, field_type)
}

fn (t &Transformer) sql_expr_result_type(node flat.Node) ?u32 {
	if node.kind != .sql_expr {
		return none
	}
	tokens := sql_clean_tokens(node.value.split(' '))
	stmts := t.parse_sql_transform_stmts(tokens) or { return none }
	if stmts.len == 0 {
		return none
	}
	return t.sql_stmt_result_type(stmts[stmts.len - 1])
}

fn (t &Transformer) sql_stmt_result_type(stmt SqlTransformStmt) u32 {
	return flat.source_type_id(match stmt.kind {
		.create, .drop {
			'!int'
		}
		.insert, .upsert, .update, .delete, .count {
			'!int'
		}
		.select {
			'![]${flat.source_type_name(stmt.table.name)}'
		}
		.aggregate {
			'!orm.AggregateValue'
		}
	})
}

fn (mut t Transformer) sql_or_expr_aggregate_optional_type(node flat.Node) ?u32 {
	if node.kind != .or_expr || node.value != '!' || node.children_count < 1 {
		return none
	}
	expr_id := t.a.child(&node, 0)
	if int(expr_id) < 0 || int(expr_id) >= t.a.nodes.len {
		return none
	}
	info := t.sql_node_aggregate_result_info(t.a.nodes[int(expr_id)]) or { return none }
	return info.optional_type
}

fn sql_aggregate_result_info(aggregate string, field_type_id u32) SqlAggregateResultInfo {
	mut clean := trimmed_transform_text(flat.source_type_name(field_type_id))
	if clean.starts_with('?') {
		clean = clean[1..]
	}
	if aggregate == 'avg' || clean in ['f32', 'f64'] {
		return SqlAggregateResultInfo{
			optional_type: flat.source_type_id('?f64')
			method:        'as_f64'
		}
	}
	if clean == 'string' {
		return SqlAggregateResultInfo{
			optional_type: flat.source_type_id('?string')
			method:        'as_string'
		}
	}
	if clean == 'time.Time' {
		return SqlAggregateResultInfo{
			optional_type: flat.source_type_id('?time.Time')
			method:        'as_time'
		}
	}
	return SqlAggregateResultInfo{
		optional_type: flat.source_type_id('?int')
		method:        'as_int'
	}
}

fn (mut t Transformer) make_stable_sql_db_expr(db_id flat.NodeId) flat.NodeId {
	db_expr := t.transform_expr(db_id)
	mut typ := t.node_type(db_expr)
	if typ == 0 {
		typ = t.raw_checker_node_type(db_id)
	}
	if typ == 0 {
		typ = t.node_type(db_id)
	}
	name := t.new_temp('sql_db')
	t.pending_stmts << t.make_decl_assign_typed(name, db_expr, typ)
	return t.make_ident(name)
}

fn (mut t Transformer) mark_sql_db_methods(db_id flat.NodeId) {
	mut typ := t.raw_checker_node_type(db_id)
	if typ == 0 {
		typ = t.node_type(db_id)
	}
	typ = t.trim_pointer_type(t.normalize_type_alias(typ))
	if typ == 0 {
		return
	}
	mut receiver_types := [flat.source_type_name(typ)]
	if flat.source_type_name(typ) == 'orm.Connection' {
		receiver_types << 'sqlite.DB'
		receiver_types << 'db.sqlite.DB'
	}
	for receiver_type in receiver_types {
		for method in ['create', 'insert', 'select', 'update', 'delete', 'drop', 'last_id'] {
			t.mark_fn_used_name('${receiver_type}.${method}')
		}
	}
}

fn (mut t Transformer) sql_stmt_call(stmt SqlTransformStmt, db_expr flat.NodeId) ?flat.NodeId {
	match stmt.kind {
		.create {
			return t.sql_create_drop_table_call(stmt, db_expr, 'v_sql_create_table')
		}
		.drop {
			return t.sql_create_drop_table_call(stmt, db_expr, 'v_sql_drop_table')
		}
		.insert {
			value := t.sql_value_name_expr(stmt.value_name)
			qb := t.sql_new_query_call(stmt.table.name, db_expr)
			method := if stmt.insert_many {
				'v_sql_insert_many_and_last_id'
			} else {
				'v_sql_insert_with_fields_and_last_id'
			}
			args := if stmt.insert_many {
				[value]
			} else {
				[
					value,
					t.sql_string_array(t.sql_initialized_fields(stmt.value_name)),
				]
			}
			return t.sql_query_builder_method_call(qb, method, args, flat.source_type_id('!int'))
		}
		.upsert {
			value := t.sql_value_name_expr(stmt.value_name)
			qb := t.sql_new_query_call(stmt.table.name, db_expr)
			return t.sql_query_builder_method_call(qb, 'v_sql_upsert_and_last_id', [
				value,
				t.sql_string_array(t.sql_initialized_fields(stmt.value_name)),
			], flat.source_type_id('!int'))
		}
		.select {
			qb := t.sql_select_query_builder(stmt, db_expr)
			return t.sql_query_builder_method_call(qb, 'query', [], flat.source_type_id('![]${flat.source_type_name(stmt.table.name)}'))
		}
		.count {
			qb := t.sql_select_query_builder(stmt, db_expr)
			return t.sql_query_builder_method_call(qb, 'count', [], flat.source_type_id('!int'))
		}
		.aggregate {
			qb := t.sql_select_query_builder(stmt, db_expr)
			return t.sql_query_builder_method_call(qb, stmt.aggregate, [
				t.make_string_literal(t.sql_transform_field_column_name(stmt.table,
					stmt.aggregate_field)),
			], flat.source_type_id('!orm.AggregateValue'))
		}
		.update {
			if stmt.insert_many {
				qb := t.sql_new_query_call(stmt.table.name, db_expr)
				return t.sql_query_builder_method_call(qb, 'v_sql_update_many_and_zero', [
					t.sql_value_name_expr(stmt.value_name),
					t.make_string_literal(stmt.where.params[0].field),
					t.sql_string_array(stmt.sets.map(it.field)),
				], flat.source_type_id('!int'))
			}
			qb := t.sql_update_query_builder(stmt, db_expr)
			return t.sql_query_builder_method_call(qb, 'v_sql_update_and_zero', [], flat.source_type_id('!int'))
		}
		.delete {
			qb := t.sql_select_query_builder(stmt, db_expr)
			return t.sql_query_builder_method_call(qb, 'v_sql_delete_and_zero', [], flat.source_type_id('!int'))
		}
	}
}

fn (mut t Transformer) sql_select_query_builder(stmt SqlTransformStmt, db_expr flat.NodeId) flat.NodeId {
	mut qb := t.sql_new_query_call(stmt.table.name, db_expr)
	qb_type_source_id := t.sql_query_builder_type(stmt.table.name)

	if stmt.distinct {
		qb = t.sql_query_builder_method_call(qb, 'v_sql_distinct', [], qb_type_source_id)
	}
	if stmt.joins.len > 0 {
		qb = t.sql_query_builder_method_call(qb, 'v_sql_select_qualified_fields', [
			t.sql_string_array(stmt.fields),
		], qb_type_source_id)
	} else {
		qb = t.sql_query_builder_method_call(qb, 'v_sql_select_fields', [
			t.sql_string_array(stmt.fields),
		], qb_type_source_id)
	}
	for join in stmt.joins {
		qb = t.sql_query_builder_method_call(qb, 'v_sql_join_from', [
			t.sql_join_type_expr(join.kind),
			t.sql_table_literal(join.left_table),
			t.sql_table_literal(join.table),
			t.make_string_literal(join.left_field),
			t.make_string_literal(join.right_field),
		], qb_type_source_id)
	}
	if stmt.where.error.len > 0 {
		qb = t.sql_query_builder_method_call(qb, 'v_sql_error', [
			t.make_string_literal(stmt.where.error),
		], qb_type_source_id)
	} else if stmt.where.condition.len > 0 {
		if stmt.joins.len > 0 {
			where_data := t.sql_static_where_query_data(stmt) or {
				qb = t.sql_query_builder_method_call(qb, 'v_sql_error', [
					t.make_string_literal('unsupported joined SQL WHERE field'),
				], qb_type_source_id)
				return qb
			}
			qb = t.sql_query_builder_method_call(qb, 'v_sql_where_mapped_query_data', [
				where_data,
			], qb_type_source_id)
		} else {
			qb = t.sql_query_builder_method_call(qb, 'v_sql_where_condition', [
				t.make_string_literal(stmt.where.condition),
				t.sql_where_params_array(stmt.table, stmt.where.params),
			], qb_type_source_id)
		}
	} else if stmt.where.dynamic_data.name.len > 0 || stmt.where.dynamic_data.tokens.len > 0 {
		if stmt.joins.len > 0 {
			if stmt.where.dynamic_data.name.len > 0 {
				if _ := t.sql_query_data_alias_tokens(stmt.where.dynamic_data.name) {
					scope := sql_where_scope_for_stmt(stmt, true)
					qb = t.sql_query_builder_method_call(qb, 'v_sql_where_mapped_query_data', [
						t.sql_dynamic_data_arg_scoped(scope, stmt.where.dynamic_data, .where_),
					], qb_type_source_id)
				} else {
					qb = t.sql_query_builder_method_call(qb, 'v_sql_qualified_where_query_data', [
						t.sql_value_name_expr(stmt.where.dynamic_data.name),
					], qb_type_source_id)
				}
			} else {
				scope := sql_where_scope_for_stmt(stmt, true)
				qb = t.sql_query_builder_method_call(qb, 'v_sql_where_mapped_query_data', [
					t.sql_dynamic_data_arg_scoped(scope, stmt.where.dynamic_data, .where_),
				], qb_type_source_id)
			}
		} else {
			qb = t.sql_query_builder_method_call(qb, 'v_sql_where_query_data', [
				t.sql_dynamic_data_arg(stmt.table, stmt.where.dynamic_data, .where_),
			], qb_type_source_id)
		}
	}
	if stmt.order_field.len > 0 {
		if stmt.joins.len > 0 {
			scope := sql_where_scope_for_stmt(stmt, true)
			resolved := t.sql_resolve_where_field(scope, stmt.order_field) or {
				qb = t.sql_query_builder_method_call(qb, 'v_sql_error', [
					t.make_string_literal('unsupported joined SQL ORDER BY field'),
				], qb_type_source_id)
				return qb
			}
			qb = t.sql_query_builder_method_call(qb, 'v_sql_order', [
				t.make_string_literal(resolved.name),
				t.make_bool_literal(stmt.order_desc),
			], qb_type_source_id)
		} else {
			qb = t.sql_query_builder_method_call(qb, 'v_sql_order', [
				t.make_string_literal(stmt.order_field),
				t.make_bool_literal(stmt.order_desc),
			], qb_type_source_id)
		}
	}
	if stmt.limit.len > 0 {
		qb = t.sql_query_builder_method_call(qb, 'v_sql_limit', [
			t.sql_int_expr_from_token(stmt.limit),
		], qb_type_source_id)
	}
	if stmt.offset.len > 0 && stmt.limit.len == 0 {
		if stmt.where.error.len == 0 {
			qb = t.sql_query_builder_method_call(qb, 'v_sql_error', [
				t.make_string_literal('SQL OFFSET requires LIMIT'),
			], qb_type_source_id)
		}
	} else if stmt.offset.len > 0 {
		qb = t.sql_query_builder_method_call(qb, 'v_sql_offset', [
			t.sql_int_expr_from_token(stmt.offset),
		], qb_type_source_id)
	}
	return qb
}

fn (mut t Transformer) sql_update_query_builder(stmt SqlTransformStmt, db_expr flat.NodeId) flat.NodeId {
	mut qb := t.sql_new_query_call(stmt.table.name, db_expr)
	qb_type_source_id_2 := t.sql_query_builder_type(stmt.table.name)

	if stmt.dynamic_set.name.len > 0 || stmt.dynamic_set.tokens.len > 0 {
		qb = t.sql_query_builder_method_call(qb, 'v_sql_set_query_data', [
			t.sql_dynamic_data_arg(stmt.table, stmt.dynamic_set, .set_),
		], qb_type_source_id_2)
	} else {
		for set in stmt.sets {
			qb = t.sql_query_builder_method_call(qb, 'v_sql_set_primitive', [
				t.make_string_literal(set.field),
				t.sql_primitive_from_token_for_field(stmt.table, set.field, set.value),
			], qb_type_source_id_2)
		}
	}
	if stmt.where.error.len > 0 {
		qb = t.sql_query_builder_method_call(qb, 'v_sql_error', [
			t.make_string_literal(stmt.where.error),
		], qb_type_source_id_2)
	} else if stmt.where.condition.len > 0 {
		qb = t.sql_query_builder_method_call(qb, 'v_sql_where_condition', [
			t.make_string_literal(stmt.where.condition),
			t.sql_where_params_array(stmt.table, stmt.where.params),
		], qb_type_source_id_2)
	} else if stmt.where.dynamic_data.name.len > 0 || stmt.where.dynamic_data.tokens.len > 0 {
		qb = t.sql_query_builder_method_call(qb, 'v_sql_where_query_data', [
			t.sql_dynamic_data_arg(stmt.table, stmt.where.dynamic_data, .where_),
		], qb_type_source_id_2)
	}
	return qb
}

fn (mut t Transformer) sql_create_drop_table_call(stmt SqlTransformStmt, db_expr flat.NodeId, helper string) flat.NodeId {
	spec_name := 'orm.${helper}_T_${sql_generic_type_suffix(flat.source_type_name(stmt.table.name))}'
	t.mark_fn_used_name(spec_name)
	t.sql_record_generic_helper_specialization(spec_name, stmt.table.name)
	t.sql_record_table_generic_spec(stmt.table.name)
	call := t.make_call_typed(spec_name, [
		t.sql_db_connection_arg(db_expr),
		t.make_attribute_array_literal(t.sql_table_attributes(stmt.table.name)),
	], flat.source_type_id('!int'))
	t.set_node_value(int(call), flat.source_type_name(stmt.table.name))
	return call
}

fn (mut t Transformer) sql_new_query_call(table_name u32, db_expr flat.NodeId) flat.NodeId {
	spec_name := 'orm.new_query_T_${sql_generic_type_suffix(flat.source_type_name(table_name))}'
	t.mark_fn_used_name(spec_name)
	t.sql_record_generic_helper_specialization(spec_name, table_name)
	t.sql_record_table_generic_spec(table_name)
	qb_type_source_id_3 := t.sql_query_builder_type(table_name)

	mut qb := t.make_call_typed(spec_name, [t.sql_db_connection_arg(db_expr)], qb_type_source_id_3)
	t.set_node_value(int(qb), flat.source_type_name(table_name))
	attrs := t.sql_table_attributes(table_name)
	if attrs.len > 0 {
		qb = t.sql_query_builder_method_call(qb, 'v_sql_table_attrs', [
			t.make_attribute_array_literal(attrs),
		], qb_type_source_id_3)
	}
	return qb
}

fn (mut t Transformer) sql_record_generic_helper_specialization(spec_name string, type_name_id u32) {
	t.record_generic_specialization_args_for_names([spec_name], [type_name_id])
	if isnil(t.tc) {
		return
	}
	t.tc.ensure_private_transform_signatures()
	t.tc.specialized_generic_fns[spec_name] = true
	t.tc_signature_names_log << spec_name
}

fn (mut t Transformer) sql_record_table_generic_spec(table_name_id u32) {
	base, args, is_generic := generic_app_parts(flat.source_type_name(table_name_id))
	if !is_generic || args.len == 0 {
		return
	}
	mut record_base := base
	if !record_base.contains('.') {
		if info := t.lookup_struct_info(table_name_id) {
			if info.module.len > 0 && info.module !in ['main', 'builtin'] {
				record_base = '${info.module}.${base}'
			}
		}
	}
	module_name := if record_base.contains('.') {
		record_base.all_before_last('.')
	} else {
		t.cur_module
	}
	t.record_generic_specialization_args_in_module(record_base, module_name, flat.TypeTextList.from_names(args).ids)
}

fn (mut t Transformer) sql_db_connection_arg(db_expr flat.NodeId) flat.NodeId {
	if int(db_expr) < 0 || int(db_expr) >= t.a.nodes.len {
		return db_expr
	}
	typ_source_id := t.node_type(db_expr)
	typ := flat.source_type_name(typ_source_id)
	if typ_source_id == 0 || typ.starts_with('&') || typ == 'orm.Connection'
		|| typ == 'orm.TransactionalConnection' {
		return db_expr
	}
	if !t.expr_can_take_address(db_expr) {
		return db_expr
	}
	addr := t.make_prefix(.amp, db_expr)
	t.set_node_typ(int(addr), flat.source_type_id('&${typ}'))
	return addr
}

fn (mut t Transformer) sql_query_builder_method_call(qb flat.NodeId, method string, args []flat.NodeId, typ_id u32) flat.NodeId {
	t.mark_fn_used_name('orm.QueryBuilder.${method}')
	qb_type := flat.source_type_name(t.trim_pointer_type(t.node_type(qb)))
	base, type_args, is_generic := generic_app_parts(qb_type)
	if is_generic && type_args.len == 1 && base.all_after_last('.') == 'QueryBuilder' {
		method_name := '${base}_${sql_generic_type_suffix(type_args[0])}.${method}'
		t.mark_fn_used_name(method_name)
		mut call_args := []flat.NodeId{cap: args.len + 1}
		call_args << qb
		call_args << args
		return t.make_call_typed(method_name, call_args, typ_id)
	}
	selector := t.make_selector(qb, method, u32(0))
	return t.make_call_expr_typed(selector, args, typ_id)
}

fn (t &Transformer) sql_query_builder_type(table_name u32) u32 {
	return flat.source_type_id('&orm.QueryBuilder[${flat.source_type_name(table_name)}]')
}

fn (mut t Transformer) sql_qualified_selector(name string) flat.NodeId {
	parts := name.split('.')
	mut id := t.make_ident(parts[0])
	for part in parts[1..] {
		id = t.make_selector(id, part, u32(0))
	}
	return id
}

fn (mut t Transformer) sql_type_name_expr(name string) flat.NodeId {
	return t.sql_qualified_selector(name)
}

// SQL keeps value expressions as tokens, outside ordinary identifier substitution.
fn (mut t Transformer) bind_sql_array_it(lambda_param string, elem_name string) string {
	saved := t.sql_array_it_name
	if lambda_param.len == 0 {
		t.sql_array_it_name = elem_name
	} else if lambda_param == 'it' {
		t.sql_array_it_name = ''
	}
	return saved
}

fn (t &Transformer) sql_bound_value_name(name string) string {
	if t.sql_array_it_name.len > 0 && (name == 'it' || name.starts_with('it.')) {
		return t.sql_array_it_name + name[2..]
	}
	return name
}

fn (t &Transformer) sql_bound_interpolation_text(value string) string {
	if t.sql_array_it_name.len == 0 {
		return value
	}
	mut result := ''
	mut start := 0
	mut i := 0
	for i < value.len - 1 {
		if value[i] != `$` || value[i + 1] != `{`
			|| nested_interp_start_is_escaped(value, i) {
			i++
			continue
		}
		end := nested_interp_closing_brace(value, i + 2) or { return value }
		inner := value[i + 2..end]
		bound_inner := sql_replace_interpolation_it(inner, t.sql_array_it_name)
		if bound_inner != inner {
			result += value[start..i] + '$' + '{' + bound_inner + '}'
			start = end + 1
		}
		i = end + 1
	}
	return result + value[start..]
}

fn sql_replace_interpolation_it(inner string, bound_name string) string {
	mut result := ''
	mut i := 0
	mut quote := u8(0)
	for i < inner.len {
		ch := inner[i]
		if quote != 0 {
			if ch == `\\` && i + 1 < inner.len {
				result += inner[i..i + 2]
				i += 2
				continue
			}
			if ch == quote {
				quote = 0
			}
			result += inner[i..i + 1]
			i++
			continue
		}
		if ch == `'` || ch == `"` || ch == `\`` {
			quote = ch
			result += inner[i..i + 1]
			i++
			continue
		}
		if ch.is_letter() || ch == `_` {
			mut end := i + 1
			for end < inner.len && (inner[end].is_alnum() || inner[end] == `_`) {
				end++
			}
			if inner[i..end] == 'it' && (i == 0 || inner[i - 1] != `.`) {
				result += bound_name
			} else {
				result += inner[i..end]
			}
			i = end
			continue
		}
		result += inner[i..i + 1]
		i++
	}
	return result
}

fn (mut t Transformer) sql_value_name_expr(name string) flat.NodeId {
	return t.sql_qualified_selector(t.sql_bound_value_name(name))
}

fn (mut t Transformer) sql_string_array(values []string) flat.NodeId {
	mut ids := []flat.NodeId{cap: values.len}
	for value in values {
		ids << t.make_string_literal(value)
	}
	return t.make_array_literal_typed(ids, flat.source_type_id('[]string'))
}

fn (t &Transformer) sql_initialized_fields(value_name string) []string {
	if value_name == '' {
		return []string{}
	}
	if value_name.contains('.') {
		return ['*']
	}
	return t.orm_initialized_fields[value_name] or { ['*'] }
}

fn (mut t Transformer) sql_where_params_array(table SqlTransformTableInfo, params []SqlTransformWhereParam) flat.NodeId {
	mut ids := []flat.NodeId{cap: params.len}
	for param in params {
		ids << t.sql_primitive_from_token_for_field(table, param.field, param.value)
	}
	return t.make_array_literal_typed(ids, flat.source_type_id('[]orm.Primitive'))
}

fn (mut t Transformer) sql_dynamic_data_expr(table SqlTransformTableInfo, data SqlTransformDynamicData, kind SqlDynamicDataKind) flat.NodeId {
	scope := SqlTransformWhereScope{
		table: table
	}
	return t.sql_dynamic_data_expr_scoped(scope, data, kind)
}

fn (mut t Transformer) sql_dynamic_data_expr_scoped(scope SqlTransformWhereScope, data SqlTransformDynamicData, kind SqlDynamicDataKind) flat.NodeId {
	if data.name.len > 0 {
		if tokens := t.sql_query_data_alias_tokens(data.name) {
			return t.sql_dynamic_query_data_expr_scoped(scope, tokens, kind)
		}
		return t.sql_value_name_expr(data.name)
	}
	return t.sql_dynamic_query_data_expr_scoped(scope, data.tokens, kind)
}

fn (mut t Transformer) sql_dynamic_data_arg(table SqlTransformTableInfo, data SqlTransformDynamicData, kind SqlDynamicDataKind) flat.NodeId {
	scope := SqlTransformWhereScope{
		table: table
	}
	return t.sql_dynamic_data_arg_scoped(scope, data, kind)
}

fn (mut t Transformer) sql_dynamic_data_arg_scoped(scope SqlTransformWhereScope, data SqlTransformDynamicData, kind SqlDynamicDataKind) flat.NodeId {
	if data.name.len > 0 {
		if tokens := t.sql_query_data_alias_tokens(data.name) {
			expr := t.sql_dynamic_query_data_expr_scoped(scope, tokens, kind)
			name := t.new_temp('sql_dynamic_data')
			t.pending_stmts << t.make_decl_assign_typed(name, expr, flat.source_type_id('orm.QueryData'))
			return t.make_ident(name)
		}
		return t.sql_value_name_expr(data.name)
	}
	expr := t.sql_dynamic_query_data_expr_scoped(scope, data.tokens, kind)
	name := t.new_temp('sql_dynamic_data')
	t.pending_stmts << t.make_decl_assign_typed(name, expr, flat.source_type_id('orm.QueryData'))
	return t.make_ident(name)
}

fn (t &Transformer) sql_query_data_alias_tokens(name string) ?[]string {
	tokens := t.sql_query_data_aliases[name] or { return none }
	return tokens.clone()
}

fn (mut t Transformer) sql_dynamic_query_data_expr(table SqlTransformTableInfo, tokens []string, kind SqlDynamicDataKind) flat.NodeId {
	scope := SqlTransformWhereScope{
		table: table
	}
	return t.sql_dynamic_query_data_expr_scoped(scope, tokens, kind)
}

fn (mut t Transformer) sql_dynamic_query_data_expr_scoped(scope SqlTransformWhereScope, tokens []string, kind SqlDynamicDataKind) flat.NodeId {
	data_name := t.new_temp('sql_data')
	mut stmts := []flat.NodeId{}
	stmts << t.make_decl_assign_typed(data_name, t.make_struct_init(flat.source_type_id('orm.QueryData')),
		flat.source_type_id('orm.QueryData'))
	for item in sql_dynamic_split_items(tokens) {
		stmts << t.sql_dynamic_query_data_split_item_stmts(data_name, scope, item, kind, true)
	}
	stmts << t.make_expr_stmt(t.make_ident(data_name))
	block := t.make_block(stmts)
	t.set_node_typ(int(block), flat.source_type_id('orm.QueryData'))
	return block
}

fn (mut t Transformer) sql_dynamic_query_data_split_item_stmts(data_name string, scope SqlTransformWhereScope, tokens []string, kind SqlDynamicDataKind, is_and bool) []flat.NodeId {
	item := sql_trim_outer_empty(tokens)
	if item.len == 0 {
		return []flat.NodeId{}
	}
	if logic := sql_dynamic_top_level_logic(item) {
		if logic.op == '||' {
			start_name := t.new_temp('sql_par_start')
			fields_len := t.make_selector(t.make_selector(t.make_ident(data_name), 'fields',
				flat.source_type_id('[]string')), 'len', flat.source_type_int)
			mut stmts := [
				t.make_decl_assign_typed(start_name, fields_len, flat.source_type_int),
			]
			stmts << t.sql_dynamic_query_data_item_stmts(data_name, scope, item, kind, is_and)
			t.mark_fn_used_name('orm.v_sql_query_data_parentheses')
			parenthesized := t.make_call_typed('orm.v_sql_query_data_parentheses', [
				t.make_ident(data_name),
				t.make_ident(start_name),
			], flat.source_type_id('orm.QueryData'))
			stmts << t.make_assign(t.make_ident(data_name), parenthesized)
			return stmts
		}
	}
	return t.sql_dynamic_query_data_item_stmts(data_name, scope, item, kind, is_and)
}

fn (mut t Transformer) sql_dynamic_query_data_item_stmts(data_name string, scope SqlTransformWhereScope, tokens []string, kind SqlDynamicDataKind, is_and bool) []flat.NodeId {
	item := sql_trim_outer_empty(tokens)
	if item.len == 0 {
		return []flat.NodeId{}
	}
	if if_item := sql_dynamic_if_item_parts(item) {
		if guard := sql_dynamic_guard_parts(if_item.cond) {
			return t.sql_dynamic_guard_item_stmts(data_name, scope, guard, if_item.body, kind,
				is_and)
		}
		body_stmts := t.sql_dynamic_query_data_item_stmts(data_name, scope, if_item.body, kind,
			is_and)
		if body_stmts.len == 0 {
			return []flat.NodeId{}
		}
		cond_expr := t.sql_dynamic_condition_expr(if_item.cond)
		return [
			t.make_if(cond_expr, t.make_block(body_stmts), t.make_empty()),
		]
	}
	return t.sql_dynamic_query_data_expr_stmts(data_name, scope, item, kind, is_and)
}

fn (mut t Transformer) sql_dynamic_guard_item_stmts(data_name string, scope SqlTransformWhereScope, guard SqlDynamicGuard, body []string, kind SqlDynamicDataKind, is_and bool) []flat.NodeId {
	rhs_type_source_id := t.sql_dynamic_value_type(guard.expr)
	rhs_type_source_id_2 := rhs_type_source_id

	rhs_expr := t.sql_dynamic_value_expr(guard.expr, rhs_type_source_id)
	opt_type := (if rhs_type_source_id != 0 {
		rhs_type_source_id_2
	} else {
		t.node_type(rhs_expr)
	})
	value_type := (if t.is_optional_type_name(opt_type) {
		t.optional_base_type(t.qualify_optional_type(opt_type))
	} else {
		opt_type
	})
	if guard.name != '_' && value_type != 0 {
		t.set_var_type(guard.name, value_type)
	}
	body_stmts := t.sql_dynamic_query_data_item_stmts(data_name, scope, body, kind, is_and)
	if body_stmts.len == 0 {
		return []flat.NodeId{}
	}
	tmp_name := t.new_temp('sql_guard')
	mut then_stmts := []flat.NodeId{}
	if guard.name != '_' && value_type != 0 {
		then_stmts << t.make_decl_assign_typed(guard.name, t.make_selector(t.make_ident(tmp_name),
			'value', value_type), value_type)
	}
	then_stmts << body_stmts
	return [
		t.make_decl_assign_typed(tmp_name, rhs_expr, opt_type),
		t.make_if(t.make_selector(t.make_ident(tmp_name), 'ok', flat.source_type_bool), t.make_block(then_stmts),
			t.make_empty()),
	]
}

fn (mut t Transformer) sql_dynamic_query_data_expr_stmts(data_name string, scope SqlTransformWhereScope, tokens []string, kind SqlDynamicDataKind, is_and bool) []flat.NodeId {
	expr := sql_trim_outer_empty(tokens)
	if expr.len == 0 {
		return []flat.NodeId{}
	}
	if inner := sql_wrapped_tokens(expr) {
		start_name := t.new_temp('sql_par_start')
		fields_len := t.make_selector(t.make_selector(t.make_ident(data_name), 'fields', flat.source_type_id('[]string')),
			'len', flat.source_type_int)
		mut stmts := [
			t.make_decl_assign_typed(start_name, fields_len, flat.source_type_int),
		]
		stmts << t.sql_dynamic_query_data_expr_stmts(data_name, scope, inner, kind, is_and)
		t.mark_fn_used_name('orm.v_sql_query_data_parentheses')
		parenthesized := t.make_call_typed('orm.v_sql_query_data_parentheses', [
			t.make_ident(data_name),
			t.make_ident(start_name),
		], flat.source_type_id('orm.QueryData'))
		stmts << t.make_assign(t.make_ident(data_name), parenthesized)
		return stmts
	}
	if logic := sql_dynamic_top_level_logic(expr) {
		mut stmts := t.sql_dynamic_query_data_expr_stmts(data_name, scope, expr[..logic.idx], kind,
			is_and)
		stmts << t.sql_dynamic_query_data_expr_stmts(data_name, scope, expr[logic.idx + 1..], kind,
			logic.op == '&&')
		return stmts
	}
	return t.sql_dynamic_query_data_leaf_stmt(data_name, scope, expr, kind, is_and)
}

fn (mut t Transformer) sql_dynamic_query_data_leaf_stmt(data_name string, scope SqlTransformWhereScope, tokens []string, kind SqlDynamicDataKind, is_and bool) []flat.NodeId {
	operator := sql_dynamic_leaf_operator(tokens) or {
		t.record_monomorph_error('unsupported dynamic SQL predicate `${sql_value_token_text(tokens)}`')
		return []flat.NodeId{}
	}
	if operator.idx != 1 || !sql_token_is_plain_ident(tokens[0]) {
		t.record_monomorph_error('unsupported dynamic SQL predicate `${sql_value_token_text(tokens)}`')
		return []flat.NodeId{}
	}
	if kind == .set_ && operator.op !in ['=', '=='] {
		t.record_monomorph_error('unsupported dynamic SQL SET operator `${operator.op}`')
		return []flat.NodeId{}
	}
	field := tokens[0]
	resolved := t.sql_resolve_where_field(scope, field) or {
		t.record_monomorph_error('unsupported dynamic SQL field `${field}`')
		return []flat.NodeId{}
	}
	value_tokens := tokens[operator.idx + 1..]
	op_kind := sql_dynamic_operation_kind(operator.op, value_tokens, kind)
	field_type := sql_transform_field_type(resolved.table, resolved.field)
	value := if sql_operation_kind_is_unary_name(op_kind) {
		t.make_struct_init(flat.source_type_id('orm.Null'))
	} else {
		t.sql_dynamic_value_primitive(resolved.table, resolved.field, value_tokens, field_type)
	}
	t.mark_fn_used_name('orm.v_sql_query_data_add')
	call := t.make_call_typed('orm.v_sql_query_data_add', [
		t.make_ident(data_name),
		t.make_string_literal(resolved.name),
		t.sql_operation_kind_expr(op_kind),
		value,
		t.make_bool_literal(is_and),
	], flat.source_type_id('orm.QueryData'))
	return [
		t.make_assign(t.make_ident(data_name), call),
	]
}

fn (mut t Transformer) sql_dynamic_value_primitive(_table SqlTransformTableInfo, _field string, tokens []string, field_type_id u32) flat.NodeId {
	if relation_type_source_id := t.sql_relation_struct_base_type(field_type_id) {
		if sql_value_tokens_are_none_literal(tokens) {
			return t.sql_null_primitive_expr()
		}
		expr := t.sql_dynamic_value_expr(tokens, relation_type_source_id)
		return t.sql_relation_primary_primitive_expr(expr, relation_type_source_id)
	}
	mut expr := t.sql_dynamic_value_expr(tokens, field_type_id)
	if t.sql_transform_type_is_enum(field_type_id) {
		expr = t.make_cast(flat.source_type_i64, expr, flat.source_type_i64)
	}
	return t.sql_primitive_from_expr(expr)
}

fn (mut t Transformer) sql_dynamic_value_expr(tokens []string, typ_id u32) flat.NodeId {
	clean := sql_trim_outer_empty(tokens)
	if clean.len == 0 {
		return t.make_struct_init(flat.source_type_id('orm.Null'))
	}
	if inner := sql_wrapped_tokens(clean) {
		return t.sql_dynamic_value_expr(inner, typ_id)
	}
	if clean.len >= 3 && sql_token_is_plain_ident(clean[0]) && clean[1] == '('
		&& clean[clean.len - 1] == ')' {
		cast_type := clean[0]
		return t.make_cast(flat.source_type_id(cast_type), t.sql_dynamic_value_expr(clean[2..clean.len - 1], flat.source_type_id(cast_type)),
			flat.source_type_id(cast_type))
	}
	if clean[0] == 'if' {
		return t.sql_dynamic_if_value_expr(clean, typ_id)
	}
	if index_expr := t.sql_index_expr_from_tokens_for_type(clean, typ_id) {
		return index_expr
	}
	if clean.len == 1 {
		return t.sql_expr_from_token_for_type(clean[0], typ_id)
	}
	return t.sql_expr_from_token_for_type(sql_value_token_text(clean), typ_id)
}

fn (t &Transformer) sql_dynamic_value_type(tokens []string) u32 {
	clean := sql_trim_outer_empty(tokens)
	if clean.len != 1 {
		return u32(0)
	}
	token := t.sql_bound_value_name(clean[0])
	if token.contains('.') {
		root := token.all_before('.')
		field_name := token.all_after('.')
		root_type := flat.source_type_name(t.var_type(root))
		fields := t.tc.structs[root_type] or { return u32(0) }
		for field in fields {
			if field.name == field_name {
				return field.typ.source_type_id()
			}
		}
		return u32(0)
	}
	return t.var_type(token)
}

fn (mut t Transformer) sql_dynamic_if_value_expr(tokens []string, typ_id u32) flat.NodeId {
	open_idx := sql_find_top_level_token(tokens, '{', 1) or { return t.zero_value_for_type(typ_id) }
	close_idx := sql_matching_brace(tokens, open_idx) or { return t.zero_value_for_type(typ_id) }
	mut else_idx := -1
	for i in close_idx + 1 .. tokens.len {
		if tokens[i] == 'else' {
			else_idx = i
			break
		}
	}
	if else_idx < 0 || else_idx + 1 >= tokens.len || tokens[else_idx + 1] != '{' {
		return t.zero_value_for_type(typ_id)
	}
	else_close_idx := sql_matching_brace(tokens, else_idx + 1) or {
		return t.zero_value_for_type(typ_id)
	}
	cond := t.sql_dynamic_condition_expr(tokens[1..open_idx])
	then_expr := t.sql_dynamic_value_expr(tokens[open_idx + 1..close_idx], typ_id)
	else_expr := t.sql_dynamic_value_expr(tokens[else_idx + 2..else_close_idx], typ_id)
	then_block := t.make_block([t.make_expr_stmt(then_expr)])
	else_block := t.make_block([t.make_expr_stmt(else_expr)])
	node := t.make_if(cond, then_block, else_block)
	if typ_id != u32(0) {
		t.set_node_typ(int(node), typ_id)
	}
	return node
}

fn (mut t Transformer) sql_dynamic_condition_expr(tokens []string) flat.NodeId {
	cond := sql_trim_outer_empty(tokens)
	if cond.len == 0 {
		return t.make_bool_literal(false)
	}
	if assign_idx := sql_find_top_level_token(cond, ':=', 0) {
		lhs := if assign_idx > 0 { cond[assign_idx - 1] } else { '_' }
		rhs := t.sql_dynamic_value_expr(cond[assign_idx + 1..], u32(0))
		start := t.a.children.len
		t.a.children << t.make_ident(lhs)
		t.a.children << rhs
		return t.a.add_node(flat.Node{
			kind:           .decl_assign
			op:             .assign
			children_start: start
			children_count: 2
		})
	}
	if operator := sql_dynamic_condition_operator(cond) {
		lhs := t.sql_dynamic_value_expr(cond[..operator.idx], u32(0))
		rhs := t.sql_dynamic_value_expr(cond[operator.idx + 1..], u32(0))
		return t.make_infix(sql_dynamic_flat_op(operator.op), lhs, rhs)
	}
	if cond.len == 1 {
		return t.sql_expr_from_token(cond[0])
	}
	return t.sql_dynamic_value_expr(cond, flat.source_type_bool)
}

fn sql_dynamic_split_items(tokens []string) [][]string {
	mut items := [][]string{}
	mut start := 0
	mut depth := 0
	for i, tok in tokens {
		if tok in ['{', '(', '['] {
			depth++
		} else if tok in ['}', ')', ']'] {
			if depth > 0 {
				depth--
			}
		} else if tok == ',' && depth == 0 {
			items << sql_trim_outer_empty(tokens[start..i])
			start = i + 1
		}
	}
	if start <= tokens.len {
		items << sql_trim_outer_empty(tokens[start..])
	}
	return items.filter(it.len > 0)
}

fn sql_trim_outer_empty(tokens []string) []string {
	mut start := 0
	mut end := tokens.len
	for start < end && tokens[start].len == 0 {
		start++
	}
	for end > start && tokens[end - 1].len == 0 {
		end--
	}
	return tokens[start..end]
}

fn sql_dynamic_if_item_parts(tokens []string) ?SqlDynamicIfItem {
	if tokens.len < 4 || tokens[0] != 'if' {
		return none
	}
	open_idx := sql_find_top_level_token(tokens, '{', 1) or { return none }
	close_idx := sql_matching_brace(tokens, open_idx) or { return none }
	if close_idx != tokens.len - 1 {
		return none
	}
	return SqlDynamicIfItem{
		cond: tokens[1..open_idx]
		body: tokens[open_idx + 1..close_idx]
	}
}

fn sql_dynamic_guard_parts(tokens []string) ?SqlDynamicGuard {
	assign_idx := sql_find_top_level_token(tokens, ':=', 0) or { return none }
	if assign_idx != 1 || tokens.len <= assign_idx + 1 || !sql_token_is_plain_ident(tokens[0]) {
		return none
	}
	return SqlDynamicGuard{
		name: tokens[0]
		expr: tokens[assign_idx + 1..]
	}
}

fn sql_wrapped_tokens(tokens []string) ?[]string {
	if tokens.len < 2 || tokens[0] != '(' || tokens[tokens.len - 1] != ')' {
		return none
	}
	close_idx := sql_matching_pair(tokens, 0, '(', ')') or { return none }
	if close_idx == tokens.len - 1 {
		return tokens[1..tokens.len - 1]
	}
	return none
}

fn sql_dynamic_top_level_logic(tokens []string) ?SqlDynamicTokenOp {
	mut depth := 0
	for i, tok in tokens {
		if tok in ['{', '(', '['] {
			depth++
		} else if tok in ['}', ')', ']'] {
			if depth > 0 {
				depth--
			}
		} else if depth == 0 && tok in ['&&', '||', 'and', 'or'] {
			return SqlDynamicTokenOp{
				idx: i
				op:  if tok in ['&&', 'and'] { '&&' } else { '||' }
			}
		}
	}
	return none
}

fn sql_dynamic_leaf_operator(tokens []string) ?SqlDynamicTokenOp {
	for i, tok in tokens {
		if tok in ['==', '!=', '<', '>', '<=', '>=', 'in', '!in', 'like', 'ilike', 'is', '!is',
			'='] {
			return SqlDynamicTokenOp{
				idx: i
				op:  tok
			}
		}
	}
	return none
}

fn sql_dynamic_condition_operator(tokens []string) ?SqlDynamicTokenOp {
	for i, tok in tokens {
		if tok in ['==', '!=', '<', '>', '<=', '>=', '='] {
			return SqlDynamicTokenOp{
				idx: i
				op:  tok
			}
		}
	}
	return none
}

fn sql_dynamic_operation_kind(op string, value_tokens []string, kind SqlDynamicDataKind) string {
	if kind == .set_ {
		return 'eq'
	}
	if value_tokens.len == 1 && value_tokens[0] in ['none', 'nil'] {
		if op in ['==', '='] {
			return 'is_null'
		}
		if op == '!=' {
			return 'is_not_null'
		}
	}
	return match op {
		'==', '=' { 'eq' }
		'!=' { 'neq' }
		'<' { 'lt' }
		'>' { 'gt' }
		'<=' { 'le' }
		'>=' { 'ge' }
		'in' { 'in' }
		'!in' { 'not_in' }
		'like' { 'orm_like' }
		'ilike' { 'orm_ilike' }
		'is' { 'is_null' }
		'!is' { 'is_not_null' }
		else { 'eq' }
	}
}

fn sql_operation_kind_is_unary_name(kind string) bool {
	return kind in ['is_null', 'is_not_null']
}

fn sql_dynamic_flat_op(op string) flat.Op {
	return match op {
		'==', '=' { .eq }
		'!=' { .ne }
		'<' { .lt }
		'>' { .gt }
		'<=' { .le }
		'>=' { .ge }
		else { .eq }
	}
}

fn sql_find_top_level_token(tokens []string, needle string, start int) ?int {
	mut depth := 0
	for i := start; i < tokens.len; i++ {
		tok := tokens[i]
		if tok in ['{', '(', '['] {
			if depth == 0 && tok == needle {
				return i
			}
			depth++
			continue
		}
		if tok in ['}', ')', ']'] {
			if depth > 0 {
				depth--
			}
			continue
		}
		if depth == 0 && tok == needle {
			return i
		}
	}
	return none
}

fn sql_matching_brace(tokens []string, open_idx int) ?int {
	return sql_matching_pair(tokens, open_idx, '{', '}')
}

fn sql_or_value_expr_parts(tokens []string) ?([]string, []string) {
	clean := sql_trim_outer_empty(tokens)
	or_idx := sql_find_top_level_token(clean, 'or', 0) or { return none }
	if or_idx <= 0 || or_idx + 1 >= clean.len || clean[or_idx + 1] != '{' {
		return none
	}
	close_idx := sql_matching_brace(clean, or_idx + 1) or { return none }
	if close_idx != clean.len - 1 {
		return none
	}
	return clean[..or_idx], clean[or_idx + 2..close_idx]
}

fn sql_matching_pair(tokens []string, open_idx int, open string, close string) ?int {
	if open_idx < 0 || open_idx >= tokens.len || tokens[open_idx] != open {
		return none
	}
	mut depth := 0
	for i := open_idx; i < tokens.len; i++ {
		if tokens[i] == open {
			depth++
		} else if tokens[i] == close {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return none
}

fn (mut t Transformer) sql_primitive_from_token(token string) flat.NodeId {
	return t.sql_primitive_from_expr(t.sql_expr_from_token(token))
}

fn (mut t Transformer) sql_primitive_from_token_for_field(table SqlTransformTableInfo, field string, token string) flat.NodeId {
	field_type := sql_transform_field_type(table, field)
	column := t.sql_transform_field_column_name(table, field)
	if infix := t.sql_infix_primitive_from_token_for_field(field, column, token, field_type) {
		return infix
	}
	if relation_type_source_id := t.sql_relation_struct_base_type(field_type) {
		if sql_value_token_is_none_literal(token) {
			return t.sql_null_primitive_expr()
		}
		expr := t.sql_expr_from_token_for_type(token, relation_type_source_id)
		return t.sql_relation_primary_primitive_expr(expr, relation_type_source_id)
	}
	mut expr := t.sql_expr_from_token_for_type(token, field_type)
	if t.sql_transform_type_is_enum(field_type) {
		expr = t.make_cast(flat.source_type_i64, expr, flat.source_type_i64)
	}
	return t.sql_primitive_from_expr(expr)
}

fn sql_generic_type_suffix(typ string) string {
	// Keep this identical to receiver-method specialization. Sanitizing each
	// nested argument separately turns reserved builtin names such as `int` into
	// `v_int`, while the generic method name flattens the complete type first.
	return c_name(generic_type_arg_short(trimmed_transform_text(typ)))
}

fn (mut t Transformer) sql_infix_primitive_from_token_for_field(field string, column string, token string, typ_id u32) ?flat.NodeId {
	op_text, right_text := sql_split_leading_field_math_token(token, field) or { return none }
	right_expr := t.sql_expr_from_token_for_type(right_text, typ_id)
	right_primitive := t.sql_primitive_from_expr(right_expr)
	right_value := t.transform_expr(right_primitive)
	right_ref := t.heap_value(right_value, flat.source_type_id('orm.Primitive'))
	fields := [
		t.make_named_field_init('name', t.make_string_literal(column), flat.source_type_string),
		t.make_named_field_init('operator', t.sql_math_operation_kind_expr(op_text),
			flat.source_type_id('orm.MathOperationKind')),
		t.make_named_field_init('right', right_ref, flat.source_type_id('&orm.Primitive')),
	]
	start := t.a.children.len
	for item in fields {
		t.a.children << item
	}
	infix := t.a.add_node(flat.Node{
		kind:           .struct_init
		value:          'orm.InfixType'
		typ:            flat.source_type_id('orm.InfixType')
		children_start: start
		children_count: flat.child_count(fields.len)
	})
	return t.make_cast(flat.source_type_id('orm.Primitive'), infix, flat.source_type_id('orm.Primitive'))
}

fn sql_split_leading_field_math_token(token string, field string) ?(string, string) {
	tokens := sql_clean_tokens(token.split(' '))
	clean := sql_trim_outer_empty(tokens)
	if clean.len >= 3 && clean[0] == field && clean[1] in ['+', '-', '*', '/'] {
		return clean[1], sql_value_token_text(clean[2..])
	}
	if clean.len == 1 {
		text := trimmed_transform_text(clean[0])
		for op in ['+', '-', '*', '/'] {
			prefix := '${field}${op}'
			if text.starts_with(prefix) && text.len > prefix.len {
				return op, trimmed_transform_text(text[prefix.len..])
			}
		}
	}
	return none
}

fn (t &Transformer) sql_transform_type_is_struct(field_type_id u32) bool {
	field_type := flat.source_type_name(field_type_id)
	mut clean := trimmed_transform_text(field_type)
	if clean.starts_with('?') {
		clean = clean[1..]
	}
	if clean in ['bool', 'i8', 'i16', 'int', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'string',
		'rune', 'time.Time'] {
		return false
	}
	if clean.starts_with('[]') {
		return false
	}
	return clean.len > 0 && clean in t.tc.structs
}

fn (t &Transformer) sql_relation_struct_base_type(field_type_id u32) ?u32 {
	field_type := flat.source_type_name(field_type_id)
	mut clean := trimmed_transform_text(field_type)
	if t.is_optional_type_name(flat.source_type_id(clean)) {
		clean = flat.source_type_name(t.optional_base_type(t.qualify_optional_type(flat.source_type_id(clean))))
	}
	if clean == 'time.Time' || !t.sql_transform_type_is_struct(flat.source_type_id(clean)) {
		return none
	}
	return flat.source_type_id(clean)
}

fn sql_value_token_is_none_literal(token string) bool {
	return sql_value_tokens_are_none_literal(sql_clean_tokens(token.split(' ')))
}

fn sql_value_tokens_are_none_literal(tokens []string) bool {
	clean := sql_trim_outer_empty(tokens)
	if wrapped := sql_wrapped_tokens(clean) {
		return sql_value_tokens_are_none_literal(wrapped)
	}
	return clean.len == 1 && clean[0] in ['none', 'nil']
}

fn (mut t Transformer) sql_relation_primary_primitive_expr(expr flat.NodeId, relation_type_id u32) flat.NodeId {
	relation_type := flat.source_type_name(relation_type_id)
	expr_type := trimmed_transform_text(flat.source_type_name(t.node_type(expr)))
	if t.is_optional_type_name(flat.source_type_id(expr_type)) {
		spec_name := 'orm.v_sql_optional_struct_primary_primitive_T_${sql_generic_type_suffix(relation_type)}'
		t.mark_fn_used_name(spec_name)
		t.sql_record_generic_helper_specialization(spec_name, relation_type_id)
		call := t.make_call_typed(spec_name, [expr], flat.source_type_id('orm.Primitive'))
		t.set_node_value(int(call), relation_type)
		return call
	}
	spec_name := 'orm.v_sql_struct_primary_primitive_T_${sql_generic_type_suffix(relation_type)}'
	t.mark_fn_used_name(spec_name)
	t.sql_record_generic_helper_specialization(spec_name, relation_type_id)
	call := t.make_call_typed(spec_name, [expr], flat.source_type_id('orm.Primitive'))
	t.set_node_value(int(call), relation_type)
	return call
}

fn (mut t Transformer) sql_null_primitive_expr() flat.NodeId {
	return t.sql_primitive_from_expr(t.make_struct_init(flat.source_type_id('orm.Null')))
}

fn (mut t Transformer) sql_primitive_from_expr(expr flat.NodeId) flat.NodeId {
	typ := flat.source_type_name(t.node_type(expr))
	if helper := sql_option_primitive_helper(typ) {
		t.mark_fn_used_name('orm.${helper}')
		return t.make_call_typed('orm.${helper}', [expr], flat.source_type_id('orm.Primitive'))
	}
	return t.make_cast(flat.source_type_id('orm.Primitive'), expr, flat.source_type_id('orm.Primitive'))
}

fn sql_option_primitive_helper(typ string) ?string {
	clean := trimmed_transform_text(typ)
	if !clean.starts_with('?') {
		return none
	}
	return match clean[1..] {
		'bool' { 'option_bool_to_primitive' }
		'f32' { 'option_f32_to_primitive' }
		'f64' { 'option_f64_to_primitive' }
		'i8' { 'option_i8_to_primitive' }
		'i16' { 'option_i16_to_primitive' }
		'int' { 'option_int_to_primitive' }
		'i64' { 'option_i64_to_primitive' }
		'u8' { 'option_u8_to_primitive' }
		'u16' { 'option_u16_to_primitive' }
		'u32' { 'option_u32_to_primitive' }
		'u64' { 'option_u64_to_primitive' }
		'string' { 'option_string_to_primitive' }
		'time.Time' { 'option_time_to_primitive' }
		else { none }
	}
}

fn (mut t Transformer) sql_expr_from_token(token string) flat.NodeId {
	return t.sql_expr_from_token_for_type(token, u32(0))
}

fn (mut t Transformer) sql_expr_from_token_for_type(token string, typ_id u32) flat.NodeId {
	typ := flat.source_type_name(typ_id)
	if token.contains(' ') {
		clean := sql_trim_outer_empty(sql_clean_tokens(token.split(' ')))
		// `(names)[0]`, `(f()).g()`, `f((x))`: lower the parenthesised value itself.
		if inner := sql_wrapped_tokens(clean) {
			return t.sql_expr_from_token_for_type(sql_value_token_text(inner), typ_id)
		}
		if literal := sql_signed_literal_text(clean) {
			return t.sql_expr_from_token_for_type(literal, typ_id)
		}
	}
	if sql_token_is_quoted_string(token) {
		value := sql_unquote_string_token(token)
		if interp := t.simple_nested_string_interpolation(t.sql_bound_interpolation_text(value)) {
			return interp
		}
		return t.make_string_literal(value)
	}
	if or_expr := t.sql_or_expr_from_token_for_type(token, typ_id) {
		return or_expr
	}
	if infix_expr := t.sql_infix_expr_from_token_for_type(token, typ_id) {
		return infix_expr
	}
	if index_expr := t.sql_index_expr_from_token_for_type(token, typ_id) {
		return index_expr
	}
	if fn_name, args := sql_value_call_parts(token) {
		return t.sql_value_call_expr(fn_name, args, typ_id)
	}
	if token in ['none', 'nil'] {
		return t.make_struct_init(flat.source_type_id('orm.Null'))
	}
	if token == 'true' {
		return t.make_bool_literal(true)
	}
	if token == 'false' {
		return t.make_bool_literal(false)
	}
	if sql_token_is_int_literal(token) {
		return t.make_int_literal_typed(token, flat.source_type_int)
	}
	if sql_token_is_float_literal(token) {
		return t.make_float_literal_typed(token, flat.source_type_f64)
	}
	if sql_token_is_no_arg_call(token) {
		fn_name := token[..token.len - 2]
		return t.sql_value_call_expr(fn_name, []string{}, typ_id)
	}
	if token.starts_with('.') && typ_id != u32(0) {
		return t.a.add_node(flat.Node{
			kind:  .enum_val
			value: '${typ}${token}'
			typ:   typ_id
		})
	}
	// Without an expected type (a call argument such as `.hhmm24`), keep the parser's
	// shorthand shape; call lowering resolves it against the parameter type.
	if token.len > 1 && token.starts_with('.') && !token[1..].contains('.')
		&& sql_token_is_plain_ident(token[1..]) {
		return t.a.add_node(flat.Node{
			kind:  .enum_val
			value: token[1..]
		})
	}
	if typ_id != u32(0) && t.sql_transform_type_is_enum(typ_id) && token.contains('.')
		&& t.sql_token_root_is_enum_type(token.all_before('.')) {
		return t.a.add_node(flat.Node{
			kind:  .enum_val
			value: token
			typ:   typ_id
		})
	}
	if chain := sql_value_member_chain_parts(token) {
		return t.sql_value_member_chain_expr(chain, typ_id)
	}
	return t.sql_value_name_expr(token)
}

fn (mut t Transformer) sql_value_member_chain_expr(chain SqlValueMemberChain, typ_id u32) flat.NodeId {
	mut value := t.sql_expr_from_token_for_type(chain.receiver, u32(0))
	for i, member in chain.members {
		if chain.is_call && i == chain.members.len - 1 {
			mut arg_ids := []flat.NodeId{cap: chain.args.len}
			for arg in chain.args {
				arg_ids << t.sql_expr_from_token(arg)
			}
			call := t.make_call_expr_typed(t.make_selector(value, member, u32(0)), arg_ids, typ_id)
			return t.sql_transform_value_call(call)
		}
		value_type_source_id := t.node_type(value)

		field_type_source_id := t.lookup_struct_field_type(value_type_source_id, member) or {
			t.builtin_selector_type(value_type_source_id, member) or { u32(0) }
		}

		value = t.make_selector(value, member, field_type_source_id)
	}
	return t.transform_expr(value)
}

// sql_value_member_chain_parts splits a value such as `time.now().add_days(1).format_ss()`
// at its last top-level member access. The SQL token cleaner only merges selectors into
// plain names, so a member access on a call, index or literal stays a separate `.name`
// token, which would otherwise be lowered as a bogus identifier.
fn sql_value_member_chain_parts(token string) ?SqlValueMemberChain {
	clean := sql_trim_outer_empty(sql_clean_tokens(token.split(' ')))
	mut depth := 0
	mut member_idx := -1
	for i := clean.len - 1; i > 0; i-- {
		tok := clean[i]
		if tok in [')', ']', '}'] {
			depth++
		} else if tok in ['(', '[', '{'] {
			depth--
		} else if depth == 0 && tok.len > 1 && tok[0] == `.` {
			member_idx = i
			break
		}
	}
	if member_idx <= 0 {
		return none
	}
	mut member_text := clean[member_idx][1..]
	mut is_call := member_text.ends_with('()')
	if is_call {
		member_text = member_text[..member_text.len - 2]
	}
	members := member_text.split('.')
	for member in members {
		if member.len == 0 || !sql_token_is_plain_ident(member) {
			return none
		}
	}
	mut args := []string{}
	rest := clean[member_idx + 1..]
	if rest.len > 0 {
		if is_call {
			return none
		}
		close_idx := sql_matching_pair(rest, 0, '(', ')') or { return none }
		if close_idx != rest.len - 1 {
			return none
		}
		args = sql_value_call_args(rest[1..close_idx]) or { return none }
		is_call = true
	}
	// In `a + f().b` the member binds to `f()`; the caller splits the operator first.
	mut receiver := clean[..member_idx]
	if sql_tokens_have_top_level_infix(receiver) {
		return none
	}
	// `(time.now()).format_ss()`: value lowering has no parenthesised-expression case.
	for {
		inner := sql_wrapped_tokens(receiver) or { break }
		receiver = sql_trim_outer_empty(inner)
	}
	if receiver.len == 0 {
		return none
	}
	return SqlValueMemberChain{
		receiver: sql_value_token_text(receiver)
		members:  members
		is_call:  is_call
		args:     args
	}
}

fn (mut t Transformer) sql_value_call_expr(callee_name string, args []string, typ_id u32) flat.NodeId {
	mut arg_ids := []flat.NodeId{cap: args.len}
	for arg in args {
		arg_ids << t.sql_expr_from_token(arg)
	}
	mut target_type := flat.source_type_id(t.comptime_resolve_selective_import_type(callee_name))
	if imported_source_id := t.resolve_imported_type_name(target_type) {
		target_type = imported_source_id
	}
	if args.len == 1
		&& (is_plain_builtin_alias_type(target_type) || t.is_known_type_name(target_type)) {
		converted := t.transform_expr(t.make_cast(target_type, arg_ids[0], target_type))
		if t.is_type_alias_name(target_type) {
			// Cast lowering normalizes aliases to their representation. Retain the
			// named type on a wrapper for a following alias method's resolution.
			value := t.make_paren(converted)
			t.set_node_typ(int(value), target_type)
			return value
		}
		return converted
	}
	callee := t.sql_value_call_callee(callee_name)
	call := t.make_call_expr_typed(callee, arg_ids, (if typ_id != u32(0) {
		typ_id
	} else {
		u32(0)
	}))
	// Preserve the source call shape so ordinary lowering can distinguish module
	// functions, receiver methods, and static associated functions.
	return t.sql_transform_value_call(call)
}

// SQL calls are created after checking, so they have no checker-selected method.
// Select an alias's declared or inherited method before builtin collection lowering.
fn (mut t Transformer) sql_transform_value_call(call flat.NodeId) flat.NodeId {
	node := t.a.nodes[int(call)]
	mut alias_return := t.raw_call_decl_return_type(call, node) or { u32(0) }
	if !isnil(t.tc) {
		if ret := t.tc.fn_ret_types[t.resolve_call_name(node)] {
			canonical_return_source_id := t.raw_call_return_type_name(ret.source_type_id(), node)
			canonical_return_source_id_2 := canonical_return_source_id

			if t.raw_return_type_contains_alias(canonical_return_source_id) {
				alias_return = canonical_return_source_id_2
			}
		}
		callee := t.a.child_node(&node, 0)
		if callee.kind == .selector && callee.children_count > 0 {
			receiver := t.a.child(callee, 0)
			receiver_type_source_id := t.sql_indexed_element_type(receiver) or {
				t.raw_var_type_for_expr(receiver) or { t.node_type(receiver) }
			}
			receiver_type := flat.source_type_name(receiver_type_source_id)
			if method := t.tc.concrete_method_signature_key(t.trim_all_pointer_type(receiver_type_source_id),
				callee.value) {
				if method.all_before_last('.') in t.tc.type_aliases.ids
					&& !t.receiver_method_name_is_open_generic(method)
					&& !t.call_selector_base_is_namespace(receiver, callee.value, method) {
					if t.tc.sql_orm_method_is_private(method, t.cur_file, t.cur_module) {
						t.record_monomorph_error('method `${receiver_type}.${callee.value}` is private')
						return t.make_int_literal(0)
					}
					mut receiver_arg := receiver
					mut receiver_arg_type := t.normalize_type_alias(receiver_type_source_id)
					params := t.call_param_types(method)
					if params.len > 0 {
						receiver_depth, _ := t.pointer_type_depth_and_base(receiver_arg_type)
						param_depth, _ := t.pointer_type_depth_and_base(t.normalize_type_alias(t.semantic_type_name(params[0])))
						// Remove the indirections that the receiver parameter does not take. The
						// receiver can be a call the SQL lowering built (`renamed.make_ptr()`),
						// whose pointer result ordinary receiver conversion does not dereference.
						if receiver_depth > param_depth {
							for _ in param_depth .. receiver_depth {
								receiver_arg = t.make_prefix(.mul, receiver_arg)
								receiver_arg_type = flat.source_type_id(flat.source_type_name(receiver_arg_type)[1..])
								t.set_node_typ(int(receiver_arg), receiver_arg_type)
							}
						}
					}
					args := t.transform_receiver_method_args(node, receiver_arg, method)
					return t.make_receiver_method_call_typed(node, method, args,
						t.receiver_method_return_type(method, node.typ))
				}
			}
		}
	}
	result := t.transform_expr(call)
	if alias_return != 0 {
		t.set_node_typ(int(result), alias_return)
	}
	return result
}

// sql_indexed_element_type returns the declared element type of `items[i]` for an array
// variable or field, also a `[]shared T` or `shared []T` one. The index node that SQL
// lowering builds is typed with the alias-normalized element (`[]string` for `Names`), which
// would select the array's builtin method instead of the method of the declared alias.
fn (t &Transformer) sql_indexed_element_type(receiver flat.NodeId) ?u32 {
	node := t.a.nodes[int(receiver)]
	if node.kind != .index || node.children_count < 2 || node.value == 'range' {
		return none
	}
	mut base_id := t.a.child(&node, 0)
	for t.a.nodes[int(base_id)].kind == .paren && t.a.nodes[int(base_id)].children_count > 0 {
		base_id = t.a.child(t.a.nodes[int(base_id)], 0)
	}
	base := t.a.nodes[int(base_id)]
	mut raw_id := u32(0)
	if base.kind == .ident {
		raw_id = t.raw_var_type(base.value)
	} else if base.kind == .selector && base.children_count > 0 {
		owner_id := t.trim_pointer_type(t.raw_expr_type_without_smartcast(t.a.child(&base, 0)))
		raw_id, _ = t.lookup_struct_field_raw_type_with_owner(owner_id, base.value) or {
			return none
		}
	}
	mut raw := trimmed_transform_text(flat.source_type_name(raw_id)).trim_left('&')
	if raw.starts_with('shared ') {
		raw = trimmed_transform_text(raw['shared '.len..])
	}
	if !raw.starts_with('[]') {
		return none
	}
	mut elem := trimmed_transform_text(raw[2..])
	if elem.starts_with('shared ') {
		elem = trimmed_transform_text(elem['shared '.len..])
	}
	elem_id := flat.source_type_id(elem)
	return t.resolve_imported_type_name(elem_id) or { elem_id }
}

fn (mut t Transformer) sql_value_call_callee(callee_name string) flat.NodeId {
	bound_name := t.sql_bound_value_name(callee_name)
	parts := bound_name.split('.')
	if parts.len < 2 {
		return t.make_ident(bound_name)
	}
	mut receiver_type := t.sql_root_value_type_name(parts[0])
	if receiver_type == 0 {
		callee := t.resolve_imported_symbol_name(bound_name) or { bound_name }
		return t.sql_qualified_selector(callee)
	}
	mut receiver := t.make_ident(parts[0])
	t.set_node_typ(int(receiver), receiver_type)
	for field in parts[1..parts.len - 1] {
		field_type_source_id := t.lookup_struct_field_type(receiver_type, field) or {
			t.builtin_selector_type(receiver_type, field) or { u32(0) }
		}
		field_type_source_id_2 := field_type_source_id

		receiver = t.make_selector(receiver, field, field_type_source_id)
		if field_type_source_id != 0 {
			receiver_type = field_type_source_id_2
		}
	}
	return t.make_selector(receiver, parts.last(), u32(0))
}

fn sql_value_call_parts(token string) ?(string, []string) {
	clean := sql_trim_outer_empty(sql_clean_tokens(token.split(' ')))
	if clean.len < 3 || clean[clean.len - 1] != ')' {
		return none
	}
	mut open_idx := -1
	for i, part in clean {
		if part == '(' {
			open_idx = i
			break
		}
	}
	if open_idx <= 0 {
		return none
	}
	callee_name := sql_value_token_text(clean[..open_idx]).replace(' ', '')
	name_parts := callee_name.split('.')
	if name_parts.len == 0 {
		return none
	}
	for part in name_parts {
		if !sql_token_is_plain_ident(part) {
			return none
		}
	}
	close_idx := sql_matching_pair(clean, open_idx, '(', ')') or { return none }
	if close_idx != clean.len - 1 {
		return none
	}
	args := sql_value_call_args(clean[open_idx + 1..close_idx]) or { return none }
	return name_parts.join('.'), args
}

// sql_value_call_args splits the tokens between a call's parentheses into argument texts.
fn sql_value_call_args(tokens []string) ?[]string {
	mut args := []string{}
	mut arg_start := 0
	mut depth := 0
	for i, part in tokens {
		if part in ['(', '[', '{'] {
			depth++
		} else if part in [')', ']', '}'] {
			depth--
		} else if part == ',' && depth == 0 {
			if i == arg_start {
				return none
			}
			args << sql_value_token_text(tokens[arg_start..i])
			arg_start = i + 1
		}
	}
	if arg_start < tokens.len {
		args << sql_value_token_text(tokens[arg_start..])
	} else if arg_start > 0 {
		return none
	}
	return args
}

fn (mut t Transformer) sql_index_expr_from_token_for_type(token string, typ_id u32) ?flat.NodeId {
	tokens := sql_clean_tokens(sql_index_token_parts(token))
	return t.sql_index_expr_from_tokens_for_type(tokens, typ_id)
}

fn (mut t Transformer) sql_index_expr_from_tokens_for_type(tokens []string, typ_id u32) ?flat.NodeId {
	clean := sql_trim_outer_empty(tokens)
	base_len := sql_value_index_base_len(clean) or { return none }
	mut value := t.sql_expr_from_token_for_type(sql_value_token_text(clean[..base_len]), u32(0))
	mut i := base_len
	for i < clean.len {
		close_idx := sql_matching_pair(clean, i, '[', ']') or { return none }
		index_expr := t.sql_expr_from_token_for_type(sql_value_token_text(clean[i + 1..close_idx]),
			flat.source_type_int)
		mut elem_type := u32(0)
		if close_idx + 1 == clean.len {
			elem_type = typ_id
		}
		if elem_type == 0 {
			elem_type = t.sql_index_elem_type(value)
		}
		value = t.make_index(value, index_expr, elem_type)
		if elem_type != 0 {
			t.set_node_typ(int(value), elem_type)
		}
		i = close_idx + 1
	}
	return value
}

fn (t &Transformer) sql_index_elem_type(base flat.NodeId) u32 {
	mut base_type := t.node_type(base)
	if base_type == 0 {
		return u32(0)
	}
	base_type = t.normalize_type_alias(base_type)
	if flat.source_type_shape(base_type).has(.pointer) {
		base_type = t.normalize_type_alias(flat.source_type_id(flat.source_type_name(base_type)[1..]))
	}
	if flat.source_type_shape(base_type).has(.array_type) {
		return t.normalize_type_alias(flat.source_type_id(flat.source_type_name(base_type)[2..]))
	}
	if t.is_fixed_array_type(base_type) {
		return t.normalize_type_alias(for_in_fixed_array_elem_type(base_type))
	}
	if flat.source_type_shape(base_type).has(.map_type) {
		bracket_end := flat.source_type_name(base_type).index(']') or { return u32(0) }
		if bracket_end + 1 < flat.source_type_name(base_type).len {
			return t.normalize_type_alias(flat.source_type_id(flat.source_type_name(base_type)[bracket_end + 1..]))
		}
	}
	if base_type == flat.source_type_string {
		return flat.source_type_u8
	}
	return u32(0)
}

fn (mut t Transformer) sql_infix_expr_from_token_for_type(token string, typ_id u32) ?flat.NodeId {
	tokens := sql_clean_tokens(token.split(' '))
	lhs_text, op_text, rhs_text := sql_split_infix_value_tokens(tokens) or { return none }
	op := match op_text {
		'+' { flat.Op.plus }
		'-' { flat.Op.minus }
		'*' { flat.Op.mul }
		'/' { flat.Op.div }
		'%' { flat.Op.mod }
		'==' { flat.Op.eq }
		'!=' { flat.Op.ne }
		'>' { flat.Op.gt }
		'<' { flat.Op.lt }
		'>=' { flat.Op.ge }
		'<=' { flat.Op.le }
		else { return none }
	}

	lhs := t.sql_expr_from_token_for_type(lhs_text, typ_id)
	rhs := t.sql_expr_from_token_for_type(rhs_text, typ_id)
	expr := t.make_infix(op, lhs, rhs)
	result_type := (if op in [.eq, .ne, .gt, .lt, .ge, .le] {
		flat.source_type_bool
	} else if typ_id != u32(0) {
		typ_id
	} else {
		t.node_type(lhs)
	})
	if result_type != 0 {
		t.set_node_typ(int(expr), result_type)
	}
	return expr
}

fn sql_split_infix_value_tokens(tokens []string) ?(string, string, string) {
	clean := sql_trim_outer_empty(tokens)
	if clean.len == 0 {
		return none
	}
	for op_group in [
		['==', '!=', '>=', '<=', '>', '<'],
		['+', '-'],
		['*', '/', '%'],
	] {
		mut depth := 0
		for idx := clean.len - 1; idx >= 0; idx-- {
			tok := clean[idx]
			if tok in [')', ']', '}'] {
				depth++
			} else if tok in ['(', '[', '{'] {
				depth--
			}
			if depth != 0 || tok !in op_group || idx == 0 || idx + 1 >= clean.len {
				continue
			}
			// A sign after another operator is unary: `a - -1`, `a * -b`.
			if clean[idx - 1] in ['==', '!=', '>=', '<=', '>', '<', '+', '-', '*', '/', '%'] {
				continue
			}
			return sql_value_token_text(clean[..idx]), tok, sql_value_token_text(clean[idx + 1..])
		}
	}
	if clean.len == 1 {
		text := clean[0]
		for op in ['==', '!=', '>=', '<=', '+', '-', '*', '/', '%', '>', '<'] {
			if op_idx := sql_infix_operator_index(text, op) {
				return trimmed_transform_text(text[..op_idx]), op, trimmed_transform_text(text[op_idx + op.len..])
			}
		}
	}
	return none
}

fn sql_infix_operator_index(text string, op string) ?int {
	for i := 1; i + op.len < text.len; i++ {
		if text[i..i + op.len] == op {
			return i
		}
	}
	return none
}

fn (mut t Transformer) sql_or_expr_from_token_for_type(token string, typ_id u32) ?flat.NodeId {
	if !token.contains(' or ') {
		return none
	}
	tokens := sql_clean_tokens(token.split(' '))
	source_tokens, fallback_tokens := sql_or_value_expr_parts(tokens) or { return none }
	source := t.sql_expr_from_token_for_type(sql_value_token_text(source_tokens), u32(0))
	source_type_source_id := t.raw_var_type_for_expr(source) or { t.node_type(source) }

	// Method resolution needs the declared alias payload before ordinary or lowering
	// normalizes its Option/Result storage to the underlying collection.
	value_type := (if t.is_optional_type_name(source_type_source_id)
		&& t.raw_return_type_contains_alias(source_type_source_id) {
		t.optional_base_type(source_type_source_id)
	} else {
		typ_id
	})
	fallback := t.sql_expr_from_token_for_type(sql_value_token_text(fallback_tokens), value_type)
	body := t.make_block([t.make_expr_stmt(fallback)])
	start := t.a.children.len
	t.a.children << source
	t.a.children << body
	id := t.a.add_node(flat.Node{
		kind:           .or_expr
		children_start: start
		children_count: 2
		typ:            value_type
	})
	t.set_node_typ(int(id), value_type)
	return id
}

fn (mut t Transformer) sql_int_expr_from_token(token string) flat.NodeId {
	if sql_token_is_int_literal(token) {
		return t.make_int_literal_typed(token, flat.source_type_int)
	}
	return t.sql_value_name_expr(token)
}

fn (mut t Transformer) sql_operation_kind_expr(op string) flat.NodeId {
	name := match op {
		'=' {
			'eq'
		}
		'<>' {
			'neq'
		}
		'>' {
			'gt'
		}
		'<' {
			'lt'
		}
		'>=' {
			'ge'
		}
		'<=' {
			'le'
		}
		'eq', 'neq', 'gt', 'lt', 'ge', 'le', 'orm_like', 'orm_ilike', 'is_null', 'is_not_null',
		'in', 'not_in' {
			op
		}
		else {
			'eq'
		}
	}

	return t.a.add_node(flat.Node{
		kind:  .enum_val
		value: 'orm.OperationKind.${name}'
		typ:   flat.source_type_id('orm.OperationKind')
	})
}

fn (mut t Transformer) sql_join_type_expr(kind string) flat.NodeId {
	name := match kind {
		'left' { 'left' }
		'right' { 'right' }
		'full_outer' { 'full_outer' }
		else { 'inner' }
	}

	return t.a.add_node(flat.Node{
		kind:  .enum_val
		value: 'orm.JoinType.${name}'
		typ:   flat.source_type_id('orm.JoinType')
	})
}

fn (mut t Transformer) sql_table_literal(table SqlTransformTableInfo) flat.NodeId {
	fields := [
		t.make_named_field_init('name',
			t.make_string_literal(t.sql_runtime_table_name(table.name)), flat.source_type_string),
		t.make_named_field_init('attrs',
			t.make_attribute_array_literal(t.sql_table_attributes(table.name)), flat.source_type_id('[]VAttribute')),
		t.make_named_field_init('fields', t.sql_string_array(table.fields.map(it.name)), flat.source_type_id('[]string')),
		t.make_named_field_init('columns', t.sql_string_array(t.sql_table_column_names(table)),
			flat.source_type_id('[]string')),
	]
	start := t.a.children.len
	for item in fields {
		t.a.children << item
	}
	return t.a.add_node(flat.Node{
		kind:           .struct_init
		value:          'orm.Table'
		typ:            flat.source_type_id('orm.Table')
		children_start: start
		children_count: flat.child_count(fields.len)
	})
}

fn (t &Transformer) sql_table_column_names(table SqlTransformTableInfo) []string {
	metas := t.struct_field_decl_metas(table.name)
	mut columns := []string{cap: table.fields.len}
	for field in table.fields {
		meta := metas[field.name] or {
			columns << field.name
			continue
		}
		columns << t.sql_table_column_name(field.name, meta)
	}
	return columns
}

fn (t &Transformer) sql_table_column_name(field_name string, meta FieldDeclMeta) string {
	for attr in comptime_attribute_metas_from_raw(meta.attrs, []int{}) {
		if attr.name == 'sql' && attr.has_arg && attr.kind == 1 {
			return trim_attr_arg_text(attr.arg)
		}
	}
	return field_name
}

fn (t &Transformer) sql_transform_field_column_name(table SqlTransformTableInfo, field_name string) string {
	metas := t.struct_field_decl_metas(table.name)
	meta := metas[field_name] or { return field_name }
	return t.sql_table_column_name(field_name, meta)
}

fn (mut t Transformer) sql_runtime_table_name(table_name_id u32) string {
	for attr in t.sql_table_attributes(table_name_id) {
		if attr.name == 'table' && attr.has_arg {
			return trim_attr_arg_text(attr.arg)
		}
	}
	table_name := flat.source_type_name(table_name_id)
	base, _, is_generic := generic_app_parts(table_name)
	mut clean := if is_generic { base } else { table_name }
	if clean.contains('.') {
		clean = clean.all_after_last('.')
	}
	return clean.to_lower()
}

fn trim_attr_arg_text(arg string) string {
	mut out := trimmed_transform_text(arg)
	if out.len >= 2 && ((out.starts_with("'") && out.ends_with("'"))
		|| (out.starts_with('"') && out.ends_with('"'))) {
		out = trimmed_transform_text(out[1..out.len - 1])
	}
	return out
}

fn (mut t Transformer) sql_math_operation_kind_expr(op string) flat.NodeId {
	name := match op {
		'-' { 'sub' }
		'*' { 'mul' }
		'/' { 'div' }
		else { 'add' }
	}

	return t.a.add_node(flat.Node{
		kind:  .enum_val
		value: 'orm.MathOperationKind.${name}'
		typ:   flat.source_type_id('orm.MathOperationKind')
	})
}

fn (t &Transformer) parse_sql_transform_stmts(tokens []string) ?[]SqlTransformStmt {
	if tokens.len == 0 {
		return none
	}
	mut i := 0
	mut stmts := []SqlTransformStmt{}
	for i < tokens.len {
		if i + 2 < tokens.len && tokens[i] == 'create' && tokens[i + 1] == 'table' {
			table_token, next_idx := sql_table_type_token_from(tokens, i + 2) or { return none }
			table := t.sql_table_info(flat.source_type_id(table_token)) or { return none }
			stmts << SqlTransformStmt{
				kind:  .create
				table: table
			}
			i = next_idx
			continue
		}
		if i + 2 < tokens.len && tokens[i] == 'drop' && tokens[i + 1] == 'table' {
			table_token, next_idx := sql_table_type_token_from(tokens, i + 2) or { return none }
			table := t.sql_table_info(flat.source_type_id(table_token)) or { return none }
			stmts << SqlTransformStmt{
				kind:  .drop
				table: table
			}
			i = next_idx
			continue
		}
		if i + 3 < tokens.len && tokens[i] == 'insert' && tokens[i + 2] == 'into' {
			table_token, next_idx := sql_table_type_token_from(tokens, i + 3) or { return none }
			table := t.sql_table_info(flat.source_type_id(table_token)) or { return none }
			insert_many := t.sql_insert_value_is_array_of_table(tokens[i + 1], table.name)
			if !insert_many && !t.sql_insert_value_is_scalar_struct(tokens[i + 1], table.name) {
				return none
			}
			stmts << SqlTransformStmt{
				kind:        .insert
				table:       table
				value_name:  tokens[i + 1]
				insert_many: insert_many
			}
			i = next_idx
			continue
		}
		if i + 3 < tokens.len && tokens[i] == 'upsert' && tokens[i + 2] == 'into' {
			table_token, next_idx := sql_table_type_token_from(tokens, i + 3) or { return none }
			table := t.sql_table_info(flat.source_type_id(table_token)) or { return none }
			if !t.sql_insert_value_is_scalar_struct(tokens[i + 1], table.name) {
				return none
			}
			stmts << SqlTransformStmt{
				kind:       .upsert
				table:      table
				value_name: tokens[i + 1]
			}
			i = next_idx
			continue
		}
		if tokens[i] == 'select'
			|| (i + 1 < tokens.len && tokens[i] == 'dynamic' && tokens[i + 1] == 'select') {
			is_dynamic := tokens[i] == 'dynamic'
			start := if is_dynamic { i + 1 } else { i }
			end := sql_stmt_end(tokens, if is_dynamic { i + 2 } else { i + 1 })
			stmt := t.parse_sql_select_stmt(tokens[start..end], is_dynamic) or { return none }
			stmts << stmt
			i = end
			continue
		}
		if tokens[i] == 'delete' {
			end := sql_stmt_end(tokens, i + 1)
			stmt := t.parse_sql_delete_stmt(tokens[i..end]) or { return none }
			stmts << stmt
			i = end
			continue
		}
		if tokens[i] == 'update'
			|| (i + 1 < tokens.len && tokens[i] == 'dynamic' && tokens[i + 1] == 'update') {
			is_dynamic := tokens[i] == 'dynamic'
			start := if is_dynamic { i + 1 } else { i }
			end := sql_stmt_end(tokens, if is_dynamic { i + 2 } else { i + 1 })
			stmt := t.parse_sql_update_stmt(tokens[start..end], is_dynamic) or { return none }
			stmts << stmt
			i = end
			continue
		}
		return none
	}
	return stmts
}

fn sql_stmt_end(tokens []string, start int) int {
	mut depth := 0
	for i in start .. tokens.len {
		tok := tokens[i]
		if tok in ['{', '(', '['] {
			depth++
		} else if tok in ['}', ')', ']'] {
			if depth > 0 {
				depth--
			}
		}
		if depth == 0 && sql_token_starts_stmt(tokens, i) {
			return i
		}
	}
	return tokens.len
}

fn sql_token_starts_stmt(tokens []string, idx int) bool {
	if idx < 0 || idx >= tokens.len {
		return false
	}
	if tokens[idx] in ['insert', 'upsert', 'select', 'delete', 'update'] {
		return true
	}
	if tokens[idx] in ['create', 'drop'] {
		return idx + 1 < tokens.len && tokens[idx + 1] == 'table'
	}
	return tokens[idx] == 'dynamic' && idx + 1 < tokens.len
		&& tokens[idx + 1] in ['select', 'update']
}

fn sql_table_type_token_from(tokens []string, start int) ?(string, int) {
	if start < 0 || start >= tokens.len {
		return none
	}
	mut table := tokens[start]
	mut next_idx := start + 1
	if start + 2 < tokens.len && tokens[start + 1] == '.' {
		table += '.' + tokens[start + 2]
		next_idx = start + 3
	}
	_, _, is_generic := generic_app_parts(table)
	if is_generic {
		return table, next_idx
	}
	if next_idx < tokens.len && tokens[next_idx] == '[' {
		close_idx := sql_matching_pair(tokens, next_idx, '[', ']') or { return none }
		if close_idx <= next_idx + 1 {
			return none
		}
		return '${table}[${sql_value_token_text(tokens[next_idx + 1..close_idx])}]', close_idx + 1
	}
	return table, next_idx
}

fn (t &Transformer) parse_sql_select_stmt(tokens []string, is_dynamic bool) ?SqlTransformStmt {
	if tokens.len < 3 || tokens[0] != 'select' {
		return none
	}
	mut distinct := false
	mut select_start := 1
	if tokens.len > 1 && tokens[1] == 'distinct' {
		distinct = true
		select_start = 2
	}
	if select_start < tokens.len && tokens[select_start] == 'count' {
		return t.parse_sql_count_stmt(tokens, is_dynamic)
	}
	if aggregate := sql_aggregate_from_tokens(tokens, select_start) {
		return t.parse_sql_aggregate_stmt(tokens, aggregate, select_start, is_dynamic)
	}
	from_idx := sql_token_index(tokens, 'from')
	if from_idx < 0 || from_idx + 1 >= tokens.len {
		return none
	}
	table_token, table_end := sql_table_type_token_from(tokens, from_idx + 1) or { return none }
	table := t.sql_table_info(flat.source_type_id(table_token)) or { return none }
	fields := sql_select_fields(tokens[select_start..from_idx]) or { return none }
	joins, tail_start := t.sql_select_joins(table, tokens[table_end..]) or { return none }
	where, order_field, order_desc, limit, offset := t.sql_select_tail(table, tokens[table_end +
		tail_start..], is_dynamic) or { return none }
	return SqlTransformStmt{
		kind:        .select
		table:       table
		is_dynamic:  is_dynamic
		distinct:    distinct
		fields:      fields
		joins:       joins
		where:       where
		order_field: order_field
		order_desc:  order_desc
		limit:       limit
		offset:      offset
	}
}

fn (t &Transformer) sql_select_joins(main_table SqlTransformTableInfo, tokens []string) ?([]SqlTransformJoin, int) {
	mut joins := []SqlTransformJoin{}
	mut available_tables := [main_table]
	mut i := 0
	for i < tokens.len {
		mut kind := ''
		if tokens[i] == 'join' {
			kind = 'inner'
			i++
		} else if tokens[i] in ['inner', 'left', 'right'] {
			if i + 1 >= tokens.len || tokens[i + 1] != 'join' {
				break
			}
			kind = tokens[i]
			i += 2
		} else if tokens[i] == 'full' {
			if i + 2 < tokens.len && tokens[i + 1] == 'outer' && tokens[i + 2] == 'join' {
				kind = 'full_outer'
				i += 3
			} else if i + 1 < tokens.len && tokens[i + 1] == 'join' {
				kind = 'full_outer'
				i += 2
			} else {
				break
			}
		} else {
			break
		}
		table_token, table_end := sql_table_type_token_from(tokens, i) or { return none }
		table := t.sql_table_info(flat.source_type_id(table_token)) or { return none }
		if table_end + 3 >= tokens.len || tokens[table_end] != 'on' || tokens[table_end + 2] != '==' {
			return none
		}
		left_table, left_field := sql_qualified_field_token(tokens[table_end + 1]) or {
			return none
		}
		right_table, right_field := sql_qualified_field_token(tokens[table_end + 3]) or {
			return none
		}
		if left_table.len == 0 || right_table.len == 0 {
			return none
		}
		mut left_join_table := SqlTransformTableInfo{}
		mut left_join_field := ''
		mut joined_field := ''
		mut matched := false
		if t.sql_table_qualifier_matches(table, right_table) {
			if prior_table := t.sql_matching_join_table(available_tables, left_table) {
				left_join_table = prior_table
				left_join_field = left_field
				joined_field = right_field
				matched = true
			}
		}
		if !matched && t.sql_table_qualifier_matches(table, left_table) {
			if prior_table := t.sql_matching_join_table(available_tables, right_table) {
				left_join_table = prior_table
				left_join_field = right_field
				joined_field = left_field
				matched = true
			}
		}
		if !matched {
			return none
		}
		joins << SqlTransformJoin{
			kind:        kind
			left_table:  left_join_table
			table:       table
			left_field:  left_join_field
			right_field: joined_field
		}
		available_tables << table
		i = table_end + 4
	}
	return joins, i
}

fn (t &Transformer) sql_matching_join_table(tables []SqlTransformTableInfo, qualifier string) ?SqlTransformTableInfo {
	for table in tables {
		if t.sql_table_qualifier_matches(table, qualifier) {
			return table
		}
	}
	return none
}

fn (t &Transformer) sql_table_qualifier_matches(table SqlTransformTableInfo, qualifier string) bool {
	table_base := sql_table_base_name(flat.source_type_name(t.sql_resolved_table_name(table.name)))
	qualifier_base := sql_table_base_name(flat.source_type_name(t.sql_resolved_table_name(flat.source_type_id(qualifier))))
	raw_qualifier_base := sql_table_base_name(qualifier)
	if raw_qualifier_base.contains('.') {
		return qualifier_base == table_base
	}
	return qualifier_base == table_base
		|| qualifier_base.all_after_last('.') == table_base.all_after_last('.')
}

fn sql_qualified_field_token(token string) ?(string, string) {
	if !token.contains('.') {
		return none
	}
	table := token.all_before_last('.')
	field := token.all_after_last('.')
	if table.len == 0 || field.len == 0 {
		return none
	}
	return table, field
}

fn (t &Transformer) parse_sql_count_stmt(tokens []string, is_dynamic bool) ?SqlTransformStmt {
	mut from_idx := 2
	if tokens.len > 2 && tokens[1] == 'distinct' {
		from_idx = 3
	}
	if tokens.len <= from_idx + 1 || tokens[from_idx] != 'from' {
		return none
	}
	table_token, table_end := sql_table_type_token_from(tokens, from_idx + 1) or { return none }
	table := t.sql_table_info(flat.source_type_id(table_token)) or { return none }
	joins, tail_start := t.sql_select_joins(table, tokens[table_end..]) or { return none }
	where, order_field, order_desc, limit, offset := t.sql_select_tail(table, tokens[table_end +
		tail_start..], is_dynamic) or { return none }
	return SqlTransformStmt{
		kind:        .count
		table:       table
		is_dynamic:  is_dynamic
		joins:       joins
		where:       where
		order_field: order_field
		order_desc:  order_desc
		limit:       limit
		offset:      offset
	}
}

fn (t &Transformer) parse_sql_aggregate_stmt(tokens []string, aggregate string, start int, is_dynamic bool) ?SqlTransformStmt {
	if start + 5 >= tokens.len || tokens[start + 1] != '(' || tokens[start + 3] != ')'
		|| tokens[start + 4] != 'from' || !sql_token_is_plain_ident(tokens[start + 2]) {
		return none
	}
	table_token, table_end := sql_table_type_token_from(tokens, start + 5) or { return none }
	table := t.sql_table_info(flat.source_type_id(table_token)) or { return none }
	joins, tail_start := t.sql_select_joins(table, tokens[table_end..]) or { return none }
	where, order_field, order_desc, limit, offset := t.sql_select_tail(table, tokens[table_end +
		tail_start..], is_dynamic) or { return none }
	return SqlTransformStmt{
		kind:            .aggregate
		table:           table
		is_dynamic:      is_dynamic
		joins:           joins
		where:           where
		order_field:     order_field
		order_desc:      order_desc
		limit:           limit
		offset:          offset
		aggregate:       aggregate
		aggregate_field: tokens[start + 2]
	}
}

fn (t &Transformer) parse_sql_update_stmt(tokens []string, is_dynamic bool) ?SqlTransformStmt {
	if tokens.len < 4 || tokens[0] != 'update' {
		return none
	}
	table_token, table_end := sql_table_type_token_from(tokens, 1) or { return none }
	if table_end >= tokens.len || tokens[table_end] != 'set' {
		return none
	}
	table := t.sql_table_info(flat.source_type_id(table_token)) or { return none }
	mut sets := []SqlTransformSet{}
	mut dynamic_set := SqlTransformDynamicData{}
	mut next_idx := table_end + 1
	if is_dynamic {
		dynamic_set, next_idx = sql_dynamic_update_set(tokens, table_end + 1) or { return none }
	} else {
		parsed_sets, parsed_next_idx := sql_update_sets(tokens, table_end + 1) or { return none }
		sets = parsed_sets.clone()
		next_idx = parsed_next_idx
	}
	mut where, order_field, _, limit, offset := t.sql_select_tail(table, tokens[next_idx..],
		is_dynamic) or { return none }
	where = sql_reject_mutating_tail('UPDATE', where, order_field, limit, offset)
	where = sql_require_mutating_where('UPDATE', where)
	insert_many, value_name := t.sql_bulk_update_info(table.name, sets, where)
	return SqlTransformStmt{
		kind:        .update
		table:       table
		is_dynamic:  is_dynamic
		sets:        sets
		dynamic_set: dynamic_set
		where:       where
		insert_many: insert_many
		value_name:  value_name
	}
}

fn (t &Transformer) parse_sql_delete_stmt(tokens []string) ?SqlTransformStmt {
	if tokens.len < 3 || tokens[0] != 'delete' || tokens[1] != 'from' {
		return none
	}
	table_token, table_end := sql_table_type_token_from(tokens, 2) or { return none }
	table := t.sql_table_info(flat.source_type_id(table_token)) or { return none }
	mut where, order_field, _, limit, offset := t.sql_select_tail(table, tokens[table_end..], false) or {
		return none
	}
	where = sql_reject_mutating_tail('DELETE', where, order_field, limit, offset)
	where = sql_require_mutating_where('DELETE', where)
	return SqlTransformStmt{
		kind:  .delete
		table: table
		where: where
	}
}

fn sql_require_mutating_where(kind string, where SqlTransformWhere) SqlTransformWhere {
	if where.error.len > 0 || where.condition.len > 0 || where.dynamic_data.name.len > 0
		|| where.dynamic_data.tokens.len > 0 {
		return where
	}
	return SqlTransformWhere{
		error: 'SQL ${kind} requires a WHERE clause'
	}
}

fn sql_reject_mutating_tail(kind string, where SqlTransformWhere, order_field string, limit string, offset string) SqlTransformWhere {
	if where.error.len > 0 {
		return where
	}
	if order_field != '' {
		return SqlTransformWhere{
			error: 'SQL ${kind} does not support ORDER BY'
		}
	}
	if limit != '' {
		return SqlTransformWhere{
			error: 'SQL ${kind} does not support LIMIT'
		}
	}
	if offset != '' {
		return SqlTransformWhere{
			error: 'SQL ${kind} does not support OFFSET'
		}
	}
	return where
}

fn sql_select_fields(tokens []string) ?[]string {
	if tokens.len == 0 {
		return []string{}
	}
	mut fields := []string{}
	mut expect_field := true
	for tok in tokens {
		if tok == ',' {
			expect_field = true
			continue
		}
		if !expect_field || !sql_token_is_plain_ident(tok) {
			return none
		}
		fields << tok
		expect_field = false
	}
	return fields
}

fn sql_update_sets(tokens []string, start int) ?([]SqlTransformSet, int) {
	mut sets := []SqlTransformSet{}
	mut i := start
	for i < tokens.len {
		if tokens[i] == 'where' {
			break
		}
		if i + 2 >= tokens.len || !sql_token_is_plain_ident(tokens[i]) || tokens[i + 1] != '=' {
			return none
		}
		value_start := i + 2
		value_end := sql_update_value_end(tokens, value_start)
		if value_end <= value_start {
			return none
		}
		sets << SqlTransformSet{
			field: tokens[i]
			value: sql_value_token_text(tokens[value_start..value_end])
		}
		i = value_end
		if i < tokens.len && tokens[i] == ',' {
			i++
			continue
		}
		if i < tokens.len && tokens[i] != 'where' {
			return none
		}
	}
	if sets.len == 0 {
		return none
	}
	return sets, i
}

fn sql_update_value_end(tokens []string, start int) int {
	mut depth := 0
	for i in start .. tokens.len {
		tok := tokens[i]
		if tok in ['{', '(', '['] {
			depth++
		} else if tok in ['}', ')', ']'] {
			if depth > 0 {
				depth--
			}
		}
		if depth == 0 && (tok == ',' || tok == 'where') {
			return i
		}
	}
	return tokens.len
}

fn sql_value_token_text(tokens []string) string {
	clean := sql_trim_outer_empty(tokens)
	if clean.len == 0 {
		return ''
	}
	if clean.len == 1 {
		return clean[0]
	}
	return clean.join(' ')
}

fn sql_value_tokens_are_index_expr(tokens []string) bool {
	if _ := sql_value_index_base_len(sql_trim_outer_empty(tokens)) {
		return true
	}
	return false
}

// sql_value_index_base_len returns how many of the (trimmed) `tokens` form the value that
// the trailing `[...]` groups index: a name (`names[0]`), a call (`make_names()[0]`,
// `f(x)[0][1]`), a parenthesised value or a member chain (`f().items[0]`).
fn sql_value_index_base_len(tokens []string) ?int {
	mut base_len := tokens.len
	for base_len > 0 && tokens[base_len - 1] == ']' {
		mut depth := 0
		mut open_idx := -1
		for j := base_len - 1; j >= 0; j-- {
			if tokens[j] == ']' {
				depth++
			} else if tokens[j] == '[' {
				depth--
				if depth == 0 {
					open_idx = j
					break
				}
			}
		}
		if open_idx < 0 || open_idx + 1 >= base_len - 1 {
			return none
		}
		base_len = open_idx
	}
	if base_len == 0 || base_len == tokens.len {
		return none
	}
	base := tokens[..base_len]
	if base.len == 1 {
		if sql_token_is_plain_ident(base[0]) || sql_token_is_no_arg_call(base[0]) {
			return base_len
		}
		return none
	}
	if _ := sql_wrapped_tokens(base) {
		return base_len
	}
	text := sql_value_token_text(base)
	if _, _ := sql_value_call_parts(text) {
		return base_len
	}
	if _ := sql_value_member_chain_parts(text) {
		return base_len
	}
	return none
}

fn sql_tokens_have_top_level_infix(tokens []string) bool {
	if tokens.len < 2 {
		return false
	}
	if _, _, _ := sql_split_infix_value_tokens(tokens) {
		return true
	}
	return false
}

fn sql_index_token_parts(token string) []string {
	if !token.contains('[') || token.contains(' ') {
		return token.split(' ')
	}
	mut parts := []string{}
	mut start := 0
	for i, ch in token {
		if ch == `[` || ch == `]` {
			if i > start {
				parts << token[start..i]
			}
			parts << ch.ascii_str()
			start = i + 1
		}
	}
	if start < token.len {
		parts << token[start..]
	}
	return parts
}

fn sql_dynamic_update_set(tokens []string, start int) ?(SqlTransformDynamicData, int) {
	if start >= tokens.len {
		return none
	}
	end := sql_update_set_clause_end(tokens, start)
	data := sql_dynamic_data_from_tokens(tokens[start..end]) or { return none }
	return data, end
}

fn sql_update_set_clause_end(tokens []string, start int) int {
	mut i := start
	mut depth := 0
	for i < tokens.len {
		if tokens[i] in ['{', '(', '['] {
			depth++
		} else if tokens[i] in ['}', ')', ']'] {
			if depth > 0 {
				depth--
			}
		}
		if depth == 0 && tokens[i] == 'where' {
			break
		}
		i++
	}
	return i
}

fn (t &Transformer) sql_select_tail(table SqlTransformTableInfo, tokens []string, is_dynamic bool) ?(SqlTransformWhere, string, bool, string, string) {
	mut where := SqlTransformWhere{}
	mut order_field := ''
	mut order_desc := false
	mut limit := ''
	mut offset := ''
	mut i := 0
	for i < tokens.len {
		if tokens[i] == 'where' {
			if where.condition.len > 0 {
				return none
			}
			end := sql_tail_clause_end(tokens, i + 1)
			where = if is_dynamic {
				t.sql_dynamic_where_condition(table, tokens[i + 1..end]) or { return none }
			} else {
				t.sql_where_condition(table, tokens[i + 1..end]) or { return none }
			}
			i = end
			continue
		}
		if tokens[i] == 'order' {
			if order_field != '' || i + 2 >= tokens.len || tokens[i + 1] != 'by'
				|| !sql_token_is_plain_ident(tokens[i + 2]) {
				return none
			}
			order_field = tokens[i + 2]
			i += 3
			if i < tokens.len && tokens[i] in ['asc', 'desc'] {
				order_desc = tokens[i] == 'desc'
				i++
			}
			continue
		}
		if tokens[i] == 'limit' {
			value, next := sql_tail_value_token(tokens, i + 1) or { return none }
			if limit != '' {
				return none
			}
			limit = value
			i = next
			continue
		}
		if tokens[i] == 'offset' {
			value, next := sql_tail_value_token(tokens, i + 1) or { return none }
			if offset != '' {
				return none
			}
			offset = value
			i = next
			continue
		}
		return none
	}
	return where, order_field, order_desc, limit, offset
}

fn sql_tail_value_token(tokens []string, start int) ?(string, int) {
	if start >= tokens.len {
		return none
	}
	if tokens[start] in ['-', '+'] && start + 1 < tokens.len
		&& sql_token_is_int_literal(tokens[start + 1]) {
		value := if tokens[start] == '-' { '-${tokens[start + 1]}' } else { tokens[start + 1] }
		return value, start + 2
	}
	if sql_token_is_value(tokens[start]) {
		return tokens[start], start + 1
	}
	return none
}

fn (t &Transformer) sql_where_condition(_table SqlTransformTableInfo, tokens []string) ?SqlTransformWhere {
	if tokens.len == 0 {
		return none
	}
	mut parts := []string{}
	mut params := []SqlTransformWhereParam{}
	mut items := []SqlTransformWhereParam{}
	mut is_and := []bool{}
	mut parentheses := [][]int{}
	mut paren_stack := []int{}
	mut next_is_and := true
	mut i := 0
	for i < tokens.len {
		tok := tokens[i]
		if tok == '(' {
			parts << tok
			paren_stack << items.len
			i++
			continue
		}
		if tok == ')' {
			parts << tok
			if paren_stack.len > 0 {
				start := paren_stack.pop()
				if items.len > start {
					parentheses << [start, items.len - 1]
				}
			}
			i++
			continue
		}
		if tok == '&&' || tok == 'and' {
			parts << 'AND'
			next_is_and = true
			i++
			continue
		}
		if tok == '||' || tok == 'or' {
			parts << 'OR'
			next_is_and = false
			i++
			continue
		}
		if i + 2 >= tokens.len || !sql_token_is_plain_ident(tok) {
			return none
		}
		field := tok
		op_tok := tokens[i + 1]
		value_tokens, value_len := sql_condition_value_tokens(tokens, i + 2) or { return none }
		value_tok := sql_condition_value_text(value_tokens)
		if op_tok == 'is' || op_tok == '!is' {
			if value_tokens.len != 1 || value_tok !in ['none', 'nil'] {
				return SqlTransformWhere{
					error: 'unsupported static SQL `${op_tok}` predicate value `${value_tok}`'
				}
			}
			parts << '${field} ${if op_tok == 'is' { 'IS NULL' } else { 'IS NOT NULL' }}'
			if items.len > 0 {
				is_and << next_is_and
			}
			items << SqlTransformWhereParam{
				field: field
				op:    if op_tok == 'is' { 'is_null' } else { 'is_not_null' }
				value: value_tok
			}
			next_is_and = true
			i += 2 + value_len
			continue
		}
		op := sql_condition_op(op_tok) or { return none }
		if value_tok in ['none', 'nil'] {
			if op !in ['=', '<>'] {
				return none
			}
			null_op := if op == '=' { 'is_null' } else { 'is_not_null' }
			parts << '${field} ${if op == '=' { 'IS NULL' } else { 'IS NOT NULL' }}'
			if items.len > 0 {
				is_and << next_is_and
			}
			items << SqlTransformWhereParam{
				field: field
				op:    null_op
				value: value_tok
			}
			next_is_and = true
			i += 2 + value_len
			continue
		}
		if !sql_static_where_value_is_supported(value_tokens) {
			return SqlTransformWhere{
				error: 'unsupported static SQL WHERE value expression `${value_tok}`'
			}
		}
		parts << '${field} ${op} ?'
		if items.len > 0 {
			is_and << next_is_and
		}
		items << SqlTransformWhereParam{
			field: field
			op:    op
			value: value_tok
		}
		next_is_and = true
		params << SqlTransformWhereParam{
			field: field
			op:    op
			value: value_tok
		}
		i += 2 + value_len
	}
	return SqlTransformWhere{
		condition:   parts.join(' ')
		params:      params
		items:       items
		is_and:      is_and
		parentheses: parentheses
	}
}

fn sql_transform_table_qualified_field(table_name string, column_name string) string {
	return '${table_name}${sql_transform_table_qualified_field_separator}${column_name}'
}

fn sql_where_scope_for_stmt(stmt SqlTransformStmt, qualify bool) SqlTransformWhereScope {
	mut joined := []SqlTransformTableInfo{cap: stmt.joins.len}
	for join in stmt.joins {
		joined << join.table
	}
	return SqlTransformWhereScope{
		table:   stmt.table
		joined:  joined
		qualify: qualify
	}
}

fn (mut t Transformer) sql_resolve_where_field(scope SqlTransformWhereScope, raw_field string) ?SqlTransformResolvedWhereField {
	mut table := scope.table
	mut field_name := raw_field
	if qualifier, field := sql_qualified_field_token(raw_field) {
		mut matched := false
		if t.sql_table_qualifier_matches(scope.table, qualifier) {
			table = scope.table
			matched = true
		} else {
			for joined_table in scope.joined {
				if t.sql_table_qualifier_matches(joined_table, qualifier) {
					table = joined_table
					matched = true
					break
				}
			}
		}
		if !matched {
			return none
		}
		field_name = field
	}
	v_field := t.sql_resolve_table_field_name(table, field_name) or { return none }
	column := t.sql_transform_field_column_name(table, v_field)
	name := if scope.qualify {
		sql_transform_table_qualified_field(t.sql_runtime_table_name(table.name), column)
	} else {
		column
	}
	return SqlTransformResolvedWhereField{
		table:  table
		field:  v_field
		column: column
		name:   name
	}
}

fn (t &Transformer) sql_resolve_table_field_name(table SqlTransformTableInfo, field_name string) ?string {
	for field in table.fields {
		column := t.sql_transform_field_column_name(table, field.name)
		if field.name == field_name || column == field_name
			|| field.name.ends_with('.${field_name}') {
			return field.name
		}
	}
	return none
}

fn sql_static_operation_kind_name(op string) string {
	return match op {
		'=' { 'eq' }
		'<>' { 'neq' }
		'>' { 'gt' }
		'<' { 'lt' }
		'>=' { 'ge' }
		'<=' { 'le' }
		'LIKE' { 'orm_like' }
		'ILIKE' { 'orm_ilike' }
		'IN' { 'in' }
		'NOT IN' { 'not_in' }
		'is_null', 'is_not_null' { op }
		else { 'eq' }
	}
}

fn (mut t Transformer) sql_static_where_query_data(stmt SqlTransformStmt) ?flat.NodeId {
	scope := sql_where_scope_for_stmt(stmt, true)
	mut data := t.make_struct_init(flat.source_type_id('orm.QueryData'))
	for idx, item in stmt.where.items {
		resolved := t.sql_resolve_where_field(scope, item.field) or { return none }
		kind_name := sql_static_operation_kind_name(item.op)
		value := if sql_operation_kind_is_unary_name(kind_name) {
			t.make_struct_init(flat.source_type_id('orm.Null'))
		} else {
			t.sql_primitive_from_token_for_field(resolved.table, resolved.field, item.value)
		}
		t.mark_fn_used_name('orm.v_sql_query_data_add')
		data = t.make_call_typed('orm.v_sql_query_data_add', [
			data,
			t.make_string_literal(resolved.name),
			t.sql_operation_kind_expr(kind_name),
			value,
			t.make_bool_literal(if idx == 0 { true } else { stmt.where.is_and[idx - 1] }),
		], flat.source_type_id('orm.QueryData'))
		for par in stmt.where.parentheses {
			if par.len == 2 && par[1] == idx {
				t.mark_fn_used_name('orm.v_sql_query_data_parentheses')
				data = t.make_call_typed('orm.v_sql_query_data_parentheses', [
					data,
					t.make_int_literal(par[0]),
				], flat.source_type_id('orm.QueryData'))
			}
		}
	}
	return data
}

fn sql_condition_value_tokens(tokens []string, start int) ?([]string, int) {
	if start >= tokens.len {
		return none
	}
	mut depth := 0
	mut i := start
	for i < tokens.len {
		tok := tokens[i]
		if depth == 0 && (tok in ['&&', 'and', '||'] || tok == ')') {
			break
		}
		if depth == 0 && tok == 'or' {
			if i <= start || i + 1 >= tokens.len || tokens[i + 1] != '{' {
				break
			}
			i++
			continue
		}
		if tok in ['(', '[', '{'] {
			depth++
		} else if tok in [')', ']', '}'] {
			if depth == 0 {
				break
			}
			depth--
		}
		i++
	}
	if i == start {
		return none
	}
	return tokens[start..i], i - start
}

fn sql_condition_value_text(tokens []string) string {
	clean := sql_trim_outer_empty(tokens)
	if wrapped := sql_wrapped_tokens(clean) {
		return sql_condition_value_text(wrapped)
	}
	if literal := sql_signed_literal_text(clean) {
		return literal
	}
	return sql_value_token_text(clean)
}

// sql_signed_literal_text joins a sign that the SQL tokens split from its numeric
// literal, so `- 1` becomes `-1` (and `+ 1` becomes `1`).
fn sql_signed_literal_text(tokens []string) ?string {
	clean := sql_trim_outer_empty(tokens)
	if clean.len == 2 && clean[0] in ['-', '+']
		&& (sql_token_is_int_literal(clean[1]) || sql_token_is_float_literal(clean[1])) {
		if clean[0] == '-' {
			return '-${clean[1]}'
		}
		return clean[1]
	}
	return none
}

fn sql_static_where_value_is_supported(tokens []string) bool {
	clean := sql_trim_outer_empty(tokens)
	if clean.len == 0 {
		return false
	}
	text := sql_condition_value_text(clean)
	if sql_token_is_value(text) {
		return true
	}
	if supported := sql_static_where_index_is_supported(sql_clean_tokens(sql_index_token_parts(text))) {
		return supported
	}
	if wrapped := sql_wrapped_tokens(clean) {
		return sql_static_where_value_is_supported(wrapped)
	}
	if source_tokens, fallback_tokens := sql_or_value_expr_parts(clean) {
		return sql_static_where_value_is_supported(source_tokens)
			&& sql_static_where_value_is_supported(fallback_tokens)
	}
	if supported := sql_static_where_index_is_supported(clean) {
		return supported
	}
	if lhs_text, _, rhs_text := sql_split_infix_value_tokens(clean) {
		return sql_static_where_value_is_supported(sql_clean_tokens(sql_index_token_parts(lhs_text)))
			&& sql_static_where_value_is_supported(sql_clean_tokens(sql_index_token_parts(rhs_text)))
	}
	if _, args := sql_value_call_parts(text) {
		return sql_static_where_values_are_supported(args)
	}
	if chain := sql_value_member_chain_parts(text) {
		return sql_static_where_values_are_supported([chain.receiver])
			&& sql_static_where_values_are_supported(chain.args)
	}
	return false
}

fn sql_static_where_index_is_supported(tokens []string) ?bool {
	clean := sql_trim_outer_empty(tokens)
	base_len := sql_value_index_base_len(clean) or { return none }
	return sql_static_where_value_is_supported(clean[..base_len])
}

fn sql_static_where_values_are_supported(values []string) bool {
	for value in values {
		if !sql_static_where_value_is_supported(sql_clean_tokens(value.split(' '))) {
			return false
		}
	}
	return true
}

fn (t &Transformer) sql_dynamic_where_condition(_table SqlTransformTableInfo, tokens []string) ?SqlTransformWhere {
	data := sql_dynamic_data_from_tokens(tokens) or { return none }
	if data.tokens.len > 0 {
		if message := sql_dynamic_invalid_predicate(data.tokens) {
			return SqlTransformWhere{
				error: message
			}
		}
	}
	return SqlTransformWhere{
		dynamic_data: data
	}
}

fn sql_dynamic_invalid_predicate(tokens []string) ?string {
	for item in sql_dynamic_split_items(tokens) {
		if message := sql_dynamic_invalid_expr(item) {
			return message
		}
	}
	return none
}

fn sql_dynamic_invalid_expr(tokens []string) ?string {
	expr := sql_trim_outer_empty(tokens)
	if expr.len == 0 {
		return none
	}
	if inner := sql_wrapped_tokens(expr) {
		return sql_dynamic_invalid_expr(inner)
	}
	if if_item := sql_dynamic_if_item_parts(expr) {
		return sql_dynamic_invalid_predicate(if_item.body)
	}
	if logic := sql_dynamic_top_level_logic(expr) {
		if message := sql_dynamic_invalid_expr(expr[..logic.idx]) {
			return message
		}
		return sql_dynamic_invalid_expr(expr[logic.idx + 1..])
	}
	operator := sql_dynamic_leaf_operator(expr) or {
		return 'unsupported dynamic SQL predicate `${sql_value_token_text(expr)}`'
	}
	if operator.idx != 1 || !sql_token_is_plain_ident(expr[0]) {
		return 'unsupported dynamic SQL predicate `${sql_value_token_text(expr)}`'
	}
	return sql_dynamic_is_predicate_error(operator.op, expr[operator.idx + 1..])
}

fn sql_dynamic_is_predicate_error(op string, value_tokens []string) ?string {
	if op !in ['is', '!is'] {
		return none
	}
	clean := sql_trim_outer_empty(value_tokens)
	if clean.len == 1 && clean[0] in ['none', 'nil'] {
		return none
	}
	return 'unsupported dynamic SQL `${op}` predicate value `${sql_value_token_text(clean)}`'
}

fn sql_dynamic_data_from_tokens(tokens []string) ?SqlTransformDynamicData {
	if tokens.len == 0 {
		return none
	}
	if tokens.len == 1 && sql_token_is_plain_ident(tokens[0]) {
		return SqlTransformDynamicData{
			name: tokens[0]
		}
	}
	if tokens.len >= 2 && tokens[0] == '{' && tokens[tokens.len - 1] == '}' {
		return SqlTransformDynamicData{
			tokens: tokens[1..tokens.len - 1]
		}
	}
	return SqlTransformDynamicData{
		tokens: tokens
	}
}

fn sql_tail_clause_end(tokens []string, start int) int {
	mut i := start
	mut depth := 0
	for i < tokens.len {
		if tokens[i] == '{' || tokens[i] == '(' || tokens[i] == '[' {
			depth++
		} else if tokens[i] == '}' || tokens[i] == ')' || tokens[i] == ']' {
			if depth > 0 {
				depth--
			}
		}
		if depth == 0 && tokens[i] in ['order', 'limit', 'offset'] {
			break
		}
		i++
	}
	return i
}

fn sql_condition_op(op string) ?string {
	return match op {
		'==' { '=' }
		'!=' { '<>' }
		'!in' { 'NOT IN' }
		'in' { 'IN' }
		'like' { 'LIKE' }
		'ilike' { 'ILIKE' }
		'=', '<', '>', '<=', '>=' { op }
		else { none }
	}
}

fn sql_aggregate_from_tokens(tokens []string, start int) ?string {
	if start >= tokens.len {
		return none
	}
	if tokens[start] in ['sum', 'avg', 'min', 'max'] {
		return tokens[start]
	}
	return none
}

fn (t &Transformer) sql_table_fields(table u32) ?[]types.StructField {
	info := t.sql_table_info(table) or { return none }
	return info.fields
}

fn (t &Transformer) sql_resolved_table_name(table u32) u32 {
	mut table_name_id := if t.active_specialization_args.ids.len > 0 {
		t.subst_type(table, t.active_specialization_args.ids)
	} else {
		table
	}
	// Selective imports can also denote values elsewhere; this query supplies a source type.
	table_name_id = flat.source_type_id(t.comptime_resolve_selective_import_type(flat.source_type_name(table_name_id)))
	if imported := t.resolve_imported_type_name(table_name_id) {
		table_name_id = imported
	}
	table_name := flat.source_type_name(table_name_id)
	base, args, is_generic := generic_app_parts(table_name)
	if !is_generic {
		return table_name_id
	}
	base_id := flat.source_type_id(base)
	resolved_base := t.sql_resolved_table_name(base_id)
	mut resolved_args := []u32{cap: args.len}
	mut changed := table_name_id != table || resolved_base != base_id
	for arg in args {
		arg_id := flat.source_type_id(arg)
		resolved_arg := t.sql_resolved_table_name(arg_id)
		if resolved_arg != arg_id {
			changed = true
		}
		resolved_args << resolved_arg
	}
	if !changed {
		return table_name_id
	}
	arg_names := flat.TypeTextList{ ids: resolved_args }.names()
	return flat.source_type_id('${flat.source_type_name(resolved_base)}[${arg_names.join(', ')}]')
}

fn (t &Transformer) sql_table_info(table u32) ?SqlTransformTableInfo {
	table_name_id := t.sql_resolved_table_name(table)
	table_name := flat.source_type_name(table_name_id)
	base, _, is_generic := generic_app_parts(table_name)
	if t.tc.cur_module.len > 0 && t.tc.cur_module !in ['', 'main', 'builtin']
		&& !table_name.contains('.') {
		qualified := '${t.tc.cur_module}.${table_name}'
		if fields := t.tc.structs[qualified] {
			return SqlTransformTableInfo{
				name:   flat.source_type_id(qualified)
				fields: fields
			}
		}
		if is_generic {
			qualified_base := '${t.tc.cur_module}.${base}'
			if fields := t.tc.structs[qualified_base] {
				return SqlTransformTableInfo{
					name:   flat.source_type_id(qualified)
					fields: fields
				}
			}
		}
	}
	if is_generic && !table_name.contains('.') {
		if info := t.lookup_struct_info(table_name_id) {
			if info.fields.len > 0 {
				name := if info.module.len > 0 && info.module !in ['main', 'builtin'] {
					'${info.module}.${table_name}'
				} else {
					table_name
				}
				return SqlTransformTableInfo{
					name:   flat.source_type_id(name)
					fields: t.sql_struct_info_fields(info)
				}
			}
		}
	}
	if is_generic && !base.contains('.') {
		mut found_name := u32(0)
		mut found_fields := []types.StructField{}
		for candidate, fields in t.tc.structs {
			if candidate.ends_with('.${base}') && fields.len > 0 {
				if found_name != 0 {
					found_name = u32(0)
					found_fields = []types.StructField{}
					break
				}
				found_name = flat.source_type_id('${candidate.all_before_last('.')}.${table_name}')
				found_fields = fields.clone()
			}
		}
		if found_name != 0 {
			return SqlTransformTableInfo{
				name:   found_name
				fields: found_fields
			}
		}
	}
	if fields := t.tc.structs[table_name] {
		return SqlTransformTableInfo{
			name:   table_name_id
			fields: fields
		}
	}
	if is_generic {
		if fields := t.tc.structs[base] {
			return SqlTransformTableInfo{
				name:   table_name_id
				fields: fields
			}
		}
	}
	if t.tc.cur_module.len > 0 && t.tc.cur_module !in ['', 'main', 'builtin'] {
		qualified := '${t.tc.cur_module}.${table_name}'
		if fields := t.tc.structs[qualified] {
			return SqlTransformTableInfo{
				name:   flat.source_type_id(qualified)
				fields: fields
			}
		}
		if is_generic {
			qualified_base := '${t.tc.cur_module}.${base}'
			if fields := t.tc.structs[qualified_base] {
				return SqlTransformTableInfo{
					name:   flat.source_type_id(qualified)
					fields: fields
				}
			}
		}
	}
	return none
}

fn (t &Transformer) sql_struct_info_fields(info StructInfo) []types.StructField {
	mut fields := []types.StructField{cap: info.fields.len}
	for field in info.fields {
		fields << types.StructField{
			name: field.name
			typ:  t.tc.parse_resolution_type(field.typ)
		}
	}
	return fields
}

fn (mut t Transformer) sql_table_attributes(table u32) []AttributeMeta {
	base := sql_table_base_name(flat.source_type_name(t.sql_resolved_table_name(table)))
	if base.contains('.') {
		module_name := base.all_before_last('.')
		struct_name := base.all_after_last('.')
		return t.sql_table_attributes_for_decl(module_name, struct_name)
	}
	for idx in 0 .. t.a.nodes.len {
		// This scan does not modify the AST arena.
		node := unsafe { &t.a.nodes[idx] }
		if node.kind == .struct_decl && node.value == base {
			return t.sql_decl_attribute_metas(idx, node)
		}
	}
	return []AttributeMeta{}
}

fn (mut t Transformer) sql_table_attributes_for_decl(module_name string, struct_name string) []AttributeMeta {
	mut cur_module := ''
	if !isnil(t.tc) && t.tc.top_level_idx.len > 0 {
		for idx in t.tc.top_level_idx {
			// Context indexes can change here, but the AST arena cannot.
			node := unsafe { &t.a.nodes[idx] }
			match node.kind {
				.file {
					cur_module = t.tc.file_modules[node.value] or { '' }
				}
				.module_decl {
					cur_module = node.value
				}
				.struct_decl {
					if node.value == struct_name && sql_module_name_matches(cur_module, module_name) {
						return t.sql_decl_attribute_metas(idx, node)
					}
				}
				else {}
			}
		}
		return []AttributeMeta{}
	}
	for idx in 0 .. t.a.nodes.len {
		// This scan does not modify the AST arena.
		node := unsafe { &t.a.nodes[idx] }
		match node.kind {
			.file {
				if !isnil(t.tc) {
					cur_module = t.tc.file_modules[node.value] or { '' }
				} else {
					cur_module = ''
				}
			}
			.module_decl {
				cur_module = node.value
			}
			.struct_decl {
				if node.value == struct_name && sql_module_name_matches(cur_module, module_name) {
					return t.sql_decl_attribute_metas(idx, node)
				}
			}
			else {}
		}
	}
	return []AttributeMeta{}
}

fn (mut t Transformer) sql_decl_attribute_metas(node_id int, node &flat.Node) []AttributeMeta {
	attrs := t.comptime_node_attribute_metas(node_id)
	if attrs.len > 0 {
		return attrs
	}
	marker_id := node_id + 1
	if marker_id >= t.a.nodes.len {
		return []AttributeMeta{}
	}
	marker := t.a.nodes[marker_id]
	if marker.kind != .directive || !marker.value.starts_with('@attributes:') {
		return []AttributeMeta{}
	}
	target := marker.value['@attributes:'.len..].int()
	mut targets_decl := target == node_id
	for i in 0 .. node.children_count {
		if int(t.a.child(node, i)) == target {
			targets_decl = true
			break
		}
	}
	if !targets_decl {
		return []AttributeMeta{}
	}
	kinds := if marker.typ != 0 {
		marker.typ_text().split(',').map(it.int())
	} else {
		[]int{}
	}
	return comptime_attribute_metas_from_raw(marker.generic_params(), kinds)
}

fn sql_module_name_matches(decl_module string, requested_module string) bool {
	decl := if decl_module == '' { 'main' } else { decl_module }
	requested := if requested_module == '' { 'main' } else { requested_module }
	return decl == requested
}

fn sql_table_base_name(table string) string {
	base, _, is_generic := generic_app_parts(table)
	if is_generic {
		return base
	}
	return table
}

fn (t &Transformer) sql_insert_value_is_scalar_struct(value_name string, table_name u32) bool {
	if !sql_token_is_plain_ident(value_name) {
		return false
	}
	mut typ := t.sql_insert_value_type_name(value_name)
	typ = t.trim_all_pointer_type(typ)
	if typ == 0 || flat.source_type_shape(typ).has(.array_type) {
		return false
	}
	if typ == table_name {
		return true
	}
	table_info := t.sql_table_info(table_name) or { return false }
	return t.sql_table_type_names_match(t.sql_resolved_table_name(typ), table_info.name)
}

fn (t &Transformer) sql_insert_value_is_array_of_table(value_name string, table_name u32) bool {
	if !sql_token_is_plain_ident(value_name) {
		return false
	}
	typ := t.sql_insert_value_type_name(value_name)
	if !flat.source_type_shape(typ).has(.array_type) {
		return false
	}
	elem_typ := t.array_elem_type(typ)
	if elem_typ == table_name {
		return true
	}
	table_info := t.sql_table_info(table_name) or { return false }
	return t.sql_table_type_names_match(t.sql_resolved_table_name(elem_typ), table_info.name)
}

fn (t &Transformer) sql_table_type_names_match(actual u32, expected u32) bool {
	if actual == expected {
		return true
	}
	if t.cur_module in ['', 'main'] {
		return type_text_without_main_locks(actual) == type_text_without_main_locks(expected)
	}
	return false
}

fn (t &Transformer) sql_bulk_update_info(table_name u32, sets []SqlTransformSet, where SqlTransformWhere) (bool, string) {
	if where.params.len != 1 {
		return false, ''
	}
	where_param := where.params[0]
	if where_param.op != '=' {
		return false, ''
	}
	root := sql_selector_root(where_param.value)
	if root.len == 0 || sql_selector_field(where_param.value) != where_param.field
		|| !t.sql_insert_value_is_array_of_table(root, table_name) {
		return false, ''
	}
	for set in sets {
		if sql_selector_root(set.value) != root || sql_selector_field(set.value) != set.field {
			return false, ''
		}
	}
	return true, root
}

fn (t &Transformer) sql_insert_value_type_name(value_name string) u32 {
	if value_name.contains('.') {
		return t.sql_selector_value_type_name(value_name)
	}
	return t.sql_root_value_type_name(value_name)
}

fn (t &Transformer) sql_selector_value_type_name(value_name string) u32 {
	parts := value_name.split('.')
	if parts.len < 2 {
		return u32(0)
	}
	mut typ := t.sql_root_value_type_name(parts[0])
	for field_name in parts[1..] {
		if typ == 0 {
			return u32(0)
		}
		typ = t.lookup_struct_field_type(typ, field_name) or { return u32(0) }
	}
	return typ
}

fn (t &Transformer) sql_root_value_type_name(value_name string) u32 {
	bound_name := t.sql_bound_value_name(value_name)
	if smartcast := t.find_smartcast(bound_name) {
		narrowed_source_id := t.smartcast_target_type(smartcast)
		narrowed_source_id_2 := narrowed_source_id

		if narrowed_source_id != 0 {
			return narrowed_source_id_2
		}
	}
	mut typ := t.var_type(bound_name)
	if typ == 0 && !isnil(t.tc) {
		if current := t.tc.cur_scope.lookup(bound_name) {
			typ = current.source_type_id()
		} else if file := t.tc.file_scope.lookup(bound_name) {
			typ = file.source_type_id()
		}
	}
	return typ
}

fn sql_clean_tokens(tokens []string) []string {
	mut clean := []string{cap: tokens.len}
	mut i := 0
	for i < tokens.len {
		token := tokens[i]
		if token.len > 0 {
			if sql_token_is_plain_ident(token) && i + 2 < tokens.len && tokens[i + 1] == '('
				&& tokens[i + 2] == ')' {
				clean << '${token}()'
				i += 3
				continue
			}
			if token == '.' && clean.len > 0 && i + 1 < tokens.len {
				mut selector := '.${tokens[i + 1]}'
				mut consumed := 2
				if i + 3 < tokens.len && tokens[i + 2] == '(' && tokens[i + 3] == ')' {
					selector += '()'
					consumed = 4
				}
				table_module := clean.len > 1
					&& clean[clean.len - 2] in ['table', 'into', 'from', 'update']
					&& sql_token_is_plain_ident(clean[clean.len - 1])
				if sql_token_can_precede_selector(clean[clean.len - 1]) || table_module {
					clean[clean.len - 1] += selector
				} else {
					clean << selector
				}
				i += consumed
				continue
			}
			if token == '.' && clean.len == 0 && i + 1 < tokens.len {
				mut selector := '.${tokens[i + 1]}'
				mut consumed := 2
				if i + 3 < tokens.len && tokens[i + 2] == '(' && tokens[i + 3] == ')' {
					selector += '()'
					consumed = 4
				}
				clean << selector
				i += consumed
				continue
			}
			clean << token
		}
		i++
	}
	return clean
}

fn sql_token_can_precede_selector(token string) bool {
	return sql_token_is_plain_ident(token)
		&& token !in ['create', 'drop', 'delete', 'table', 'insert', 'upsert', 'into', 'select',
			'from', 'where', 'update', 'set', 'order', 'by', 'limit', 'offset', 'dynamic', 'distinct',
			'and', 'or', 'in', 'is', 'none', 'nil', 'true', 'false']
}

fn sql_token_index(tokens []string, needle string) int {
	for i, tok in tokens {
		if tok == needle {
			return i
		}
	}
	return -1
}

fn sql_token_is_value(token string) bool {
	return sql_token_is_plain_ident(token) || sql_token_is_no_arg_call(token)
		|| sql_token_is_int_literal(token) || sql_token_is_float_literal(token)
		|| sql_token_is_quoted_string(token) || token.starts_with('.')
		|| token in ['true', 'false', 'none', 'nil']
}

fn sql_token_is_int_literal(token string) bool {
	if token == '' {
		return false
	}
	start := if token.len > 1 && token[0] in [`-`, `+`] { 1 } else { 0 }
	if start >= token.len {
		return false
	}
	return token[start..].bytes().all(it >= `0` && it <= `9`)
}

fn sql_token_is_float_literal(token string) bool {
	if token == '' || !token.contains('.') {
		return false
	}
	mut dot_count := 0
	start := if token.len > 1 && token[0] in [`-`, `+`] { 1 } else { 0 }
	if start >= token.len {
		return false
	}
	for ch in token[start..].bytes() {
		if ch == `.` {
			dot_count++
			continue
		}
		if ch < `0` || ch > `9` {
			return false
		}
	}
	return dot_count == 1
}

fn sql_token_is_no_arg_call(token string) bool {
	return token.ends_with('()') && sql_token_is_plain_ident(token[..token.len - 2])
}

fn sql_token_is_quoted_string(token string) bool {
	return token.len >= 2 && ((token[0] == 39 && token[token.len - 1] == 39)
		|| (token[0] == `"` && token[token.len - 1] == `"`))
}

fn sql_unquote_string_token(token string) string {
	if sql_token_is_quoted_string(token) {
		return token[1..token.len - 1]
	}
	return token
}

fn sql_token_is_plain_ident(token string) bool {
	if token == '' {
		return false
	}
	for ch in token.bytes() {
		if !((ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch == `_` || ch == `.`) {
			return false
		}
	}
	return true
}

fn sql_selector_root(token string) string {
	if !token.contains('.') {
		return ''
	}
	return token.all_before('.')
}

fn sql_selector_field(token string) string {
	if !token.contains('.') {
		return ''
	}
	return token.all_after_last('.')
}

fn sql_transform_field_type(table SqlTransformTableInfo, field_name string) u32 {
	for field in table.fields {
		if field.name == field_name {
			return field.typ.source_type_id()
		}
	}
	return u32(0)
}

fn (t &Transformer) sql_transform_type_is_enum(typ_id u32) bool {
	typ := flat.source_type_name(typ_id)
	if typ in t.enum_types {
		return true
	}
	if t.tc.cur_module.len > 0 && t.tc.cur_module !in ['', 'main', 'builtin'] {
		return '${t.tc.cur_module}.${typ}' in t.enum_types
	}
	return false
}

fn (t &Transformer) sql_token_root_is_enum_type(root string) bool {
	if root in t.enum_types {
		return true
	}
	if t.tc.cur_module.len > 0 && t.tc.cur_module !in ['', 'main', 'builtin'] {
		return '${t.tc.cur_module}.${root}' in t.enum_types
	}
	return false
}

// optional_type_text resolves the indexed source type spelling.
@[inline]
fn (value &SqlAggregateResultInfo) optional_type_text() string {
	return flat.source_type_name(value.optional_type)
}
