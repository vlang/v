module orm

// This file defines a backend-neutral SQL query plan. It is deliberately a
// superset of the older SelectConfig/QueryData pair: those remain the public
// compatibility view, while the plan is what the full-SQL renderer consumes.
// Keeping the plan in its own file means new SQL constructs can be added
// without touching the legacy generator code.

pub enum SqlQueryKind {
	select
}

pub enum SqlExprKind {
	predicate
	and
	or
	not
	raw
	exists
	in_query
	not_in_query
	eq_query
	field_eq
	in_values
	scalar
}

// SqlExpr is a recursive SQL expression. Only the fields relevant to `kind`
// are read by the renderer.
pub struct SqlExpr {
pub mut:
	kind     SqlExprKind = .predicate
	field    string
	op       OperationKind = .eq
	value    Primitive     = null_primitive
	children []SqlExpr
	query    ?&SqlQuery
	values   []Primitive
	text     string
}

pub fn sql_predicate(field string, op OperationKind, value Primitive) SqlExpr {
	return SqlExpr{
		kind:  .predicate
		field: field
		op:    op
		value: value
	}
}

pub fn sql_raw(text string) SqlExpr {
	return SqlExpr{
		kind: .raw
		text: text
	}
}

pub fn sql_and(left SqlExpr, right SqlExpr) SqlExpr {
	return SqlExpr{
		kind:     .and
		children: [left, right]
	}
}

pub fn sql_or(left SqlExpr, right SqlExpr) SqlExpr {
	return SqlExpr{
		kind:     .or
		children: [left, right]
	}
}

pub fn sql_not(inner SqlExpr) SqlExpr {
	return SqlExpr{
		kind:     .not
		children: [inner]
	}
}

pub fn sql_exists(query &SqlQuery) SqlExpr {
	return SqlExpr{
		kind:  .exists
		query: query
	}
}

pub fn sql_in_query(field string, query &SqlQuery) SqlExpr {
	return SqlExpr{
		kind:  .in_query
		field: field
		query: query
	}
}

pub fn sql_not_in_query(field string, query &SqlQuery) SqlExpr {
	return SqlExpr{
		kind:  .not_in_query
		field: field
		query: query
	}
}

pub fn sql_field_eq(left string, right string) SqlExpr {
	return SqlExpr{
		kind:  .field_eq
		field: left
		text:  right
	}
}

pub fn sql_eq_query(field string, query &SqlQuery) SqlExpr {
	return SqlExpr{
		kind:  .eq_query
		field: field
		query: query
	}
}

pub fn sql_in_values(field string, values []Primitive) SqlExpr {
	return SqlExpr{
		kind:   .in_values
		field:  field
		values: values
	}
}

pub fn sql_scalar(query &SqlQuery) SqlExpr {
	return SqlExpr{
		kind:  .scalar
		query: query
	}
}

pub struct SqlSelectItem {
pub mut:
	field string
	expr  ?SqlExpr
	alias string
	star  bool
}

pub struct SqlOrderItem {
pub mut:
	expr string
	desc bool
}

pub struct SqlFromItem {
pub mut:
	table string
	query ?&SqlQuery
	alias string
}

pub enum SqlJoinKind {
	inner
	left
	right
	full
}

pub struct SqlJoin {
pub mut:
	kind SqlJoinKind = .inner
	from SqlFromItem
	on   ?SqlExpr
}

pub enum SqlSetOpKind {
	union
	union_all
	intersect
	except
}

pub struct SqlSetOp {
pub mut:
	kind  SqlSetOpKind = .union
	query &SqlQuery
}

pub struct SqlCTE {
pub mut:
	name  string
	query &SqlQuery
}

// SqlQuery is one SELECT statement. `set_ops` are applied after it, in order.
@[heap]
pub struct SqlQuery {
pub mut:
	table    string
	items    []SqlSelectItem
	from     SqlFromItem
	joins    []SqlJoin
	where    ?SqlExpr
	group_by []SqlExpr
	having   ?SqlExpr
	order_by []SqlOrderItem
	limit    int = -1
	offset   int = -1
	distinct bool
	ctes     []SqlCTE
	set_ops  []SqlSetOp
}

// new_sql_query creates an empty SELECT plan with sensible defaults.
pub fn new_sql_query(table string) SqlQuery {
	return SqlQuery{
		table:  table
		from:   SqlFromItem{ table: table }
		limit:  -1
		offset: -1
	}
}
