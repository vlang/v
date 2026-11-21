module transform

import v.flat
import strconv

// Full-SQL lowering. The legacy `sql_select_tail` parser only understands a
// single flat SELECT; this file lowers the richer forms (subqueries, CTEs,
// grouping, set operations) into calls that build an orm.SqlQuery plan.

fn sql_has_full_feature(tokens []string) bool {
	for i, tok in tokens {
		// A `select` after index 0 normally marks a subquery. `dynamic select`
		// is legacy DSL syntax where `select` is a keyword, not a nested query;
		// lowering it would make the legacy parser treat `select` as a field.
		if tok == 'select' && i > 0 && tokens[i - 1] != 'dynamic' {
			return true
		}
		if tok == 'with' && i == 0 {
			return true
		}
		if tok == 'group' && i + 1 < tokens.len && tokens[i + 1] == 'by' {
			return true
		}
		if tok == 'having' {
			return true
		}
		if tok in ['union', 'intersect', 'except'] {
			return true
		}
		if tok == 'order' && i + 2 < tokens.len && tokens[i + 2].ends_with('()') {
			return true
		}
	}
	return false
}

fn sql_full_static_call(mut t Transformer, name string, args []flat.NodeId) flat.NodeId {
	return t.make_call('orm.${name}', args)
}

fn sql_full_method(mut t Transformer, base flat.NodeId, name string, args []flat.NodeId) flat.NodeId {
	sel := t.make_selector(base, name, u32(0))
	return t.make_call_expr_typed(sel, args, u32(0))
}

fn sql_full_literal(mut t Transformer, tok string) flat.NodeId {
	if tok.len >= 2 && tok.starts_with("'") && tok.ends_with("'") {
		return t.make_string_literal(tok[1..tok.len - 1])
	}
	if n := strconv.atoi(tok) {
		return t.make_int_literal(n)
	}
	return t.make_string_literal(tok)
}

// sql_full_parse_select parses one SELECT from `tokens` (starting at the token
// after `select`) and returns an expression building an *orm.SqlQuery.
struct FullCTE {
	name  string
	query flat.NodeId
}

fn sql_full_parse_select(mut t Transformer, tokens []string, nested bool) ?flat.NodeId {
	return sql_full_parse_select_ctes(mut t, tokens, nested, [])
}

fn sql_full_parse_select_ctes(mut t Transformer, tokens []string, nested bool, ctes []FullCTE) ?flat.NodeId {
	mut i := 0
	mut fields := []string{}
	for i < tokens.len && tokens[i] != 'from' {
		if tokens[i] != ',' {
			fields << tokens[i]
		}
		i++
	}
	if i >= tokens.len || tokens[i] != 'from' || i + 1 >= tokens.len {
		return none
	}
	if fields.len == 0 && nested {
		fields << 'id'
	}
	table_name := tokens[i + 1]
	i += 2

	mut plan := sql_full_static_call(mut t, 'new_sql_query_heap', [t.make_string_literal(table_name)])
	for cte in ctes {
		plan = sql_full_method(mut t, plan, 'with_cte', [
			t.make_string_literal(cte.name),
			cte.query,
		])
	}
	for field in fields {
		plan = sql_full_method(mut t, plan, 'item', [t.make_string_literal(field)])
	}

	mut where_tokens := []string{}
	mut order_expr := ''
	mut order_desc := false
	mut limit := -1
	mut offset := -1
	mut group_tokens := []string{}
	mut having_tokens := []string{}
	mut set_op := ''
	mut set_tokens := []string{}

	for i < tokens.len {
		tok := tokens[i]
		match tok {
			'join' {
				if i + 3 >= tokens.len || tokens[i + 1] != '(' || tokens[i + 2] != 'select' {
					return none
				}
				close := sql_matching_pair(tokens, i + 1, '(', ')') or { return none }
				sub := sql_full_parse_select(mut t, tokens[i + 3..close], true) or { return none }
				mut alias := ''
				mut on_expr := flat.NodeId(0)
				mut j := close + 1
				if j + 1 < tokens.len && tokens[j] == 'as' {
					alias = tokens[j + 1]
					j += 2
				}
				if j + 1 < tokens.len && tokens[j] == 'on' {
					on_expr = sql_full_parse_expr(mut t, tokens[j + 1..]) or { return none }
				}
				from_args := [sub, t.make_string_literal(alias)]
				from_call := sql_full_static_call(mut t, 'sql_from_query', from_args)
				kind_id := t.make_ident('orm.SqlJoinKind.inner')
				args := [kind_id, from_call, on_expr]
				plan = sql_full_method(mut t, plan, 'join_from', args)
				i = tokens.len
				continue
			}
			'where' {
				start := i + 1
				i = sql_full_clause_end(tokens, start)
				where_tokens = tokens[start..i]
				continue
			}
			'group' {
				if i + 1 < tokens.len && tokens[i + 1] == 'by' {
					start := i + 2
					i = sql_full_clause_end(tokens, start)
					group_tokens = tokens[start..i]
					continue
				}
			}
			'having' {
				start := i + 1
				i = sql_full_clause_end(tokens, start)
				having_tokens = tokens[start..i]
				continue
			}
			'order' {
				if i + 2 >= tokens.len || tokens[i + 1] != 'by' {
					return none
				}
				order_expr = tokens[i + 2]
				i += 3
				if i + 1 < tokens.len && tokens[i] == '(' && tokens[i + 1] == ')' {
					order_expr += '()'
					i += 2
				}
				if i < tokens.len && tokens[i] in ['asc', 'desc'] {
					order_desc = tokens[i] == 'desc'
					i++
				}
				continue
			}
			'limit', 'offset' {
				if i + 1 >= tokens.len {
					return none
				}
				value := strconv.atoi(tokens[i + 1]) or { return none }
				if tok == 'limit' {
					limit = value
				} else {
					offset = value
				}
				i += 2
				continue
			}
			'union', 'intersect', 'except' {
				set_op = tok
				set_tokens = tokens[i + 1..]
				break
			}
			else {}
		}
		i++
	}

	if where_tokens.len > 0 {
		expr := sql_full_parse_expr(mut t, where_tokens) or { return none }
		plan = sql_full_method(mut t, plan, 'where_expr', [expr])
	}
	for g in group_tokens {
		raw := sql_full_static_call(mut t, 'sql_raw', [t.make_string_literal(g)])
		plan = sql_full_method(mut t, plan, 'group', [raw])
	}
	if having_tokens.len > 0 {
		expr := sql_full_parse_expr(mut t, having_tokens) or { return none }
		plan = sql_full_method(mut t, plan, 'having_expr', [expr])
	}
	if order_expr != '' {
		plan = sql_full_method(mut t, plan, 'order', [
			t.make_string_literal(order_expr),
			t.make_bool_literal(order_desc),
		])
	}
	if limit >= 0 {
		plan = sql_full_method(mut t, plan, 'take', [t.make_int_literal(limit)])
	}
	if offset >= 0 {
		plan = sql_full_method(mut t, plan, 'skip', [t.make_int_literal(offset)])
	}
	if set_op != '' && set_tokens.len > 2 && set_tokens[0] == 'select' {
		right := sql_full_parse_select(mut t, set_tokens[1..], true) or { return none }
		method := match set_op {
			'union' { 'union' }
			'intersect' { 'intersect' }
			else { 'except' }
		}
		args := if set_op == 'union' { [right, t.make_bool_literal(false)] } else { [right] }
		plan = sql_full_method(mut t, plan, method, args)
	}
	return plan
}

fn sql_full_clause_end(tokens []string, start int) int {
	mut depth := 0
	mut i := start
	for i < tokens.len {
		tok := tokens[i]
		if tok == '(' {
			depth++
		} else if tok == ')' {
			if depth == 0 {
				break
			}
			depth--
		} else if depth == 0 && tok in [
			'where',
			'group',
			'having',
			'order',
			'limit',
			'offset',
			'union',
			'intersect',
			'except',
		] {
			break
		}
		i++
	}
	return i
}

// sql_full_parse_expr builds an orm.SqlExpr for a simple predicate or for
// `field in (select ...)`, `exists (select ...)` and `field = (select ...)`.
fn sql_full_parse_expr(mut t Transformer, tokens []string) ?flat.NodeId {
	if tokens.len < 3 {
		return none
	}
	if tokens[0] == 'exists' && tokens[1] == '(' && tokens[2] == 'select'
		&& tokens[tokens.len - 1] == ')' {
		inner := sql_full_parse_select(mut t, tokens[3..tokens.len - 1], true) or { return none }
		return sql_full_static_call(mut t, 'sql_exists', [inner])
	}
	field := tokens[0]
	op := tokens[1]
	if op == 'in' || op == 'not_in' || op == '!in' {
		if tokens.len >= 5 && tokens[2] == '(' && tokens[3] == 'select' {
			inner_tokens := tokens[4..tokens.len - 1]
			inner := sql_full_parse_select(mut t, inner_tokens, true) or { return none }
			name := if op == 'in' { 'sql_in_query' } else { 'sql_not_in_query' }
			return sql_full_static_call(mut t, name, [t.make_string_literal(field), inner])
		}
	}
	if op == '=' && tokens.len >= 5 && tokens[2] == '(' && tokens[3] == 'select' {
		inner := sql_full_parse_select(mut t, tokens[4..tokens.len - 1], true) or { return none }
		return sql_full_static_call(mut t, 'sql_eq_query', [
			t.make_string_literal(field),
			inner,
		])
	}
	if tokens[2].contains('.') && op in ['==', '='] {
		return sql_full_static_call(mut t, 'sql_field_eq', [
			t.make_string_literal(field),
			t.make_string_literal(tokens[2]),
		])
	}
	kind := match op {
		'==', '=' { 'eq' }
		'!=', '<>' { 'neq' }
		'>' { 'gt' }
		'<' { 'lt' }
		'>=' { 'ge' }
		'<=' { 'le' }
		else { return none }
	}
	return sql_full_static_call(mut t, 'sql_predicate', [
		t.make_string_literal(field),
		t.make_ident('orm.OperationKind.${kind}'),
		sql_full_literal(mut t, tokens[2]),
	])
}

// sql_full_ctes parses leading `with name as (select ...)` clauses and returns
// the CTE expressions plus the index of the main select token.
fn sql_full_ctes(mut t Transformer, tokens []string) ?([]FullCTE, int) {
	mut ctes := []FullCTE{}
	mut i := 0
	if tokens.len == 0 || tokens[0] != 'with' {
		return ctes, 0
	}
	i = 1
	for i < tokens.len && tokens[i] != 'select' {
		name := tokens[i]
		if i + 2 >= tokens.len || tokens[i + 1] != 'as' || tokens[i + 2] != '(' {
			return none
		}
		if !sql_token_is_plain_ident(name) {
			return none
		}
		close := sql_matching_pair(tokens, i + 2, '(', ')') or { return none }
		// Skip the nested `select` keyword; the parser expects tokens after it.
		start := if i + 3 < close && tokens[i + 3] == 'select' { i + 4 } else { i + 3 }
		inner := tokens[start..close]
		query := sql_full_parse_select(mut t, inner, true) or { return none }
		ctes << FullCTE{ name: name, query: query }
		i = close + 1
		if i < tokens.len && tokens[i] == ',' {
			i++
		}
	}
	if i >= tokens.len || tokens[i] != 'select' {
		return none
	}
	return ctes, i
}

// try_full_sql_tokens lowers a full-SQL block directly from raw tokens. It runs
// before the legacy statement parser, which rejects clauses it does not know.
fn (mut t Transformer) try_full_sql_tokens(tokens []string, db_expr flat.NodeId) ?flat.NodeId {
	if !sql_has_full_feature(tokens) {
		return none
	}
	_, select_idx := sql_full_ctes(mut t, tokens) or { return none }
	if select_idx >= tokens.len {
		return none
	}
	// Find the FROM table after the SELECT keyword.
	mut from_idx := -1
	for i := select_idx + 1; i < tokens.len; i++ {
		if tokens[i] == 'from' && i + 1 < tokens.len {
			from_idx = i
			break
		}
	}
	if from_idx < 0 {
		return none
	}
	table_name := tokens[from_idx + 1]
	if !sql_token_is_plain_ident(table_name) {
		return none
	}
	// The FROM table may be a CTE name; the QueryBuilder still needs a real
	// model struct for result metadata, so pick the first token that names one.
	mut model_name := table_name
	for tok in tokens {
		if sql_token_is_plain_ident(tok) && t.sql_table_info(flat.source_type_id(tok)) != none {
			model_name = tok
			break
		}
	}
	table_id := flat.source_type_id(model_name)
	mut fields := []string{}
	for i := select_idx + 1; i < from_idx; i++ {
		if sql_token_is_plain_ident(tokens[i]) {
			fields << tokens[i]
		}
	}
	mut stmt := SqlTransformStmt{
		kind:   .select
		table:  SqlTransformTableInfo{ name: table_id }
		fields: fields
	}
	return t.try_full_sql_select(tokens, stmt, db_expr)
}

// try_full_sql_select lowers a SELECT statement that needs full-SQL features.
// It returns none when the statement is a plain legacy SELECT.
fn (mut t Transformer) try_full_sql_select(tokens []string, stmt SqlTransformStmt, db_expr flat.NodeId) ?flat.NodeId {
	if !sql_has_full_feature(tokens) {
		return none
	}
	ctes, select_idx := sql_full_ctes(mut t, tokens) or { return none }
	if select_idx >= tokens.len {
		return none
	}
	plan := sql_full_parse_select_ctes(mut t, tokens[select_idx + 1..], false, ctes) or { return none }
	mut qb := t.sql_new_query_call(stmt.table.name, db_expr)
	// Preserve the result type metadata used by map_row.
	qb = t.sql_query_builder_method_call(qb, 'v_sql_select_fields', [
		t.sql_string_array(stmt.fields),
	], flat.source_type_id('QueryBuilder'))
	with_plan := sql_full_method(mut t, qb, 'v_sql_plan', [plan])
	result := sql_full_method(mut t, with_plan, 'query', [])
	return result
}
