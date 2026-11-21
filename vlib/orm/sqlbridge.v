module orm

// PlanConnection is an optional extension of Connection for drivers that can
// execute a full SqlQuery plan (subqueries, CTEs, set operations, grouping).
// Connection itself is unchanged, so third-party drivers keep compiling.
pub interface PlanConnection {
	select_plan(config SelectConfig, plan &SqlQuery) ![][]Primitive
}

// orm_select_gen_plan renders a plan and returns the bind parameters in SQL
// order, wrapped in a QueryData that drivers can pass to their existing binder.
pub fn orm_select_gen_plan(dialect SQLDialect, q string, num bool, qm string, start_pos int, plan &SqlQuery) (string, QueryData) {
	sql, params := render_sql_query(dialect, q, num, qm, start_pos, plan)
	return sql, QueryData{ data: params }
}

// new_sql_query_heap allocates a plan on the heap so the compiler can chain
// method calls on it when lowering `sql db { ... }` blocks.
pub fn new_sql_query_heap(table string) &SqlQuery {
	return &SqlQuery{
		table:  table
		from:   SqlFromItem{ table: table }
		limit:  -1
		offset: -1
	}
}

pub fn (mut q SqlQuery) item(field string) &SqlQuery {
	q.items << SqlSelectItem{ field: field }
	return q
}

pub fn (mut q SqlQuery) star() &SqlQuery {
	q.items << SqlSelectItem{ star: true }
	return q
}

pub fn (mut q SqlQuery) where_expr(expr SqlExpr) &SqlQuery {
	q.where = expr
	return q
}

pub fn (mut q SqlQuery) group(expr SqlExpr) &SqlQuery {
	q.group_by << expr
	return q
}

pub fn (mut q SqlQuery) having_expr(expr SqlExpr) &SqlQuery {
	q.having = expr
	return q
}

pub fn (mut q SqlQuery) order(expr string, desc bool) &SqlQuery {
	q.order_by << SqlOrderItem{ expr: expr, desc: desc }
	return q
}

pub fn (mut q SqlQuery) take(n int) &SqlQuery {
	q.limit = n
	return q
}

pub fn (mut q SqlQuery) skip(n int) &SqlQuery {
	q.offset = n
	return q
}

pub fn (mut q SqlQuery) distinct_rows() &SqlQuery {
	q.distinct = true
	return q
}

pub fn (mut q SqlQuery) with_cte(name string, sub &SqlQuery) &SqlQuery {
	q.ctes << SqlCTE{ name: name, query: sub }
	return q
}

pub fn (mut q SqlQuery) union(sub &SqlQuery, all bool) &SqlQuery {
	kind := if all { SqlSetOpKind.union_all } else { SqlSetOpKind.union }
	q.set_ops << SqlSetOp{ kind: kind, query: sub }
	return q
}

pub fn (mut q SqlQuery) intersect(sub &SqlQuery) &SqlQuery {
	q.set_ops << SqlSetOp{ kind: .intersect, query: sub }
	return q
}

pub fn (mut q SqlQuery) except(sub &SqlQuery) &SqlQuery {
	q.set_ops << SqlSetOp{ kind: .except, query: sub }
	return q
}

// sql_from_query builds a derived-table FROM item from a subquery plan.
pub fn sql_from_query(sub &SqlQuery, alias string) SqlFromItem {
	return SqlFromItem{ query: sub, alias: alias }
}

pub fn (mut q SqlQuery) join_from(kind SqlJoinKind, from SqlFromItem, on SqlExpr) &SqlQuery {
	q.joins << SqlJoin{ kind: kind, from: from, on: on }
	return q
}

// sql_query_from_select converts a legacy SelectConfig/QueryData pair into a
// plan. It is used by drivers that have not yet switched to native plans.
pub fn sql_query_from_select(cfg SelectConfig, where QueryData, data QueryData) SqlQuery {
	mut q := new_sql_query(cfg.table.name)
	q.distinct = cfg.has_distinct
	for i, field in cfg.fields {
		mut item := SqlSelectItem{ field: field }
		if i < cfg.select_exprs.len && cfg.select_exprs[i] != '' && cfg.select_exprs[i] != field {
			item.expr = sql_raw(cfg.select_exprs[i])
			item.field = ''
		}
		q.items << item
	}
	if cfg.has_where {
		q.where = sql_expr_from_data(where)
	}
	if cfg.has_order {
		expr := if cfg.order.contains('(') { cfg.order } else { render_qname(cfg.order, '"') }
		q.order_by << SqlOrderItem{ expr: expr, desc: cfg.order_type == .desc }
	}
	// LIMIT/OFFSET values live in data.data after the where params in the
	// legacy representation.
	mut idx := 0
	if cfg.has_limit && idx < data.data.len {
		q.limit = primitive_int(data.data[idx])
		idx++
	}
	if cfg.has_offset && idx < data.data.len {
		q.offset = primitive_int(data.data[idx])
	}
	return q
}

fn primitive_int(p Primitive) int {
	return match p {
		int { p }
		i64 { int(p) }
		u64 { int(p) }
		else { 0 }
	}
}

pub fn sql_expr_from_data(d QueryData) SqlExpr {
	mut root := SqlExpr{ kind: .and }
	for i, field in d.fields {
		op := if i < d.kinds.len { d.kinds[i] } else { .eq }
		value := if i < d.data.len { d.data[i] } else { Primitive(Null{}) }
		pred := if op in [.in, .not_in]! && value is []int {
			sql_in_values(field, value)
		} else {
			sql_predicate(field, op, value)
		}
		root.children << pred
	}
	if root.children.len == 1 {
		return root.children[0]
	}
	return root
}
