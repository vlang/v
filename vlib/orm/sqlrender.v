module orm

// The full-SQL renderer turns a SqlQuery into SQL text for a dialect and,
// crucially, returns the bind parameters in the exact order they appear in the
// generated SQL. Recursive constructs (subqueries, CTEs, derived tables) share
// one placeholder counter, so numbered dialects stay correct at every level.

struct RowRenderState {
mut:
	position int
	params   []Primitive
}

fn render_qname(name string, q string) string {
	mut parts := name.split('.')
	for i, part in parts {
		parts[i] = '${q}${part}${q}'
	}
	return parts.join('.')
}

fn render_placeholder(num bool, qm string, mut c RowRenderState) string {
	if num {
		marker := '${qm}${c.position}'
		c.position++
		return marker
	}
	return qm
}

fn render_bind(value Primitive, num bool, qm string, mut c RowRenderState) string {
	marker := render_placeholder(num, qm, mut c)
	c.params << value
	return marker
}

fn render_expr(dialect SQLDialect, q string, num bool, qm string, mut c RowRenderState, e SqlExpr) string {
	match e.kind {
		.raw {
			return e.text
		}
		.predicate {
			field := render_qname(e.field, q)
			if e.op.is_unary() {
				return '${field} ${e.op.to_str()}'
			}
			marker := render_bind(e.value, num, qm, mut c)
			return '${field} ${e.op.to_str()} ${marker}'
		}
		.and, .or {
			if e.children.len == 0 {
				return '1 = 1'
			}
			joiner := if e.kind == .and { ' AND ' } else { ' OR ' }
			mut parts := []string{cap: e.children.len}
			for child in e.children {
				parts << render_expr(dialect, q, num, qm, mut c, child)
			}
			return '(${parts.join(joiner)})'
		}
		.not {
			if e.children.len == 0 {
				return '1 = 1'
			}
			return 'NOT (${render_expr(dialect, q, num, qm, mut c, e.children[0])})'
		}
		.in_values {
			field := render_qname(e.field, q)
			if e.values.len == 0 {
				return '${field} IN (NULL)'
			}
			mut markers := []string{cap: e.values.len}
			for value in e.values {
				markers << render_bind(value, num, qm, mut c)
			}
			return '${field} IN (${markers.join(', ')})'
		}
		.in_query, .not_in_query {
			field := render_qname(e.field, q)
			inner := if sub := e.query {
				render_query_inner(dialect, q, num, qm, mut c, sub)
			} else {
				'SELECT NULL'
			}
			op := if e.kind == .in_query { 'IN' } else { 'NOT IN' }
			return '${field} ${op} (${inner})'
		}
		.field_eq {
			return '${render_qname(e.field, q)} = ${render_qname(e.text, q)}'
		}
		.eq_query {
			field := render_qname(e.field, q)
			inner := if sub := e.query {
				render_query_inner(dialect, q, num, qm, mut c, sub)
			} else {
				'SELECT NULL'
			}
			return '${field} = (${inner})'
		}
		.exists {
			inner := if sub := e.query {
				render_query_inner(dialect, q, num, qm, mut c, sub)
			} else {
				'SELECT NULL'
			}
			return 'EXISTS (${inner})'
		}
		.scalar {
			inner := if sub := e.query {
				render_query_inner(dialect, q, num, qm, mut c, sub)
			} else {
				'SELECT NULL'
			}
			return '(${inner})'
		}
	}
	return '1 = 1'
}

fn render_from(dialect SQLDialect, q string, num bool, qm string, mut c RowRenderState, item SqlFromItem) string {
	if sub := item.query {
		base := '(${render_query_inner(dialect, q, num, qm, mut c, sub)})'
		if item.alias != '' {
			return '${base} ${render_qname(item.alias, q)}'
		}
		return base
	}
	return render_qname(item.table, q)
}

fn render_join_kind(kind SqlJoinKind) string {
	return match kind {
		.inner { 'INNER JOIN' }
		.left { 'LEFT JOIN' }
		.right { 'RIGHT JOIN' }
		.full { 'FULL OUTER JOIN' }
	}
}

fn render_limit_offset(dialect SQLDialect, num bool, qm string, mut c RowRenderState, query &SqlQuery) string {
	if query.limit < 0 && query.offset < 0 {
		return ''
	}
	// Standard SQL spells OFFSET before FETCH NEXT; the legacy dialects use
	// LIMIT/OFFSET. Bind order always follows the emitted marker order.
	if dialect == .vsql {
		offset_marker := if query.offset >= 0 {
			render_bind(query.offset, num, qm, mut c)
		} else {
			'0'
		}
		mut out := ' OFFSET ${offset_marker} ROWS'
		if query.limit >= 0 {
			out += ' FETCH FIRST ${render_bind(query.limit, num, qm, mut c)} ROWS ONLY'
		}
		return out
	}
	mut out := ''
	if query.limit >= 0 {
		out += ' LIMIT ${render_bind(query.limit, num, qm, mut c)}'
	}
	if query.offset >= 0 {
		out += ' OFFSET ${render_bind(query.offset, num, qm, mut c)}'
	}
	return out
}

fn render_query_inner(dialect SQLDialect, q string, num bool, qm string, mut c RowRenderState, query &SqlQuery) string {
	mut out := ''
	if query.ctes.len > 0 {
		mut ctes := []string{cap: query.ctes.len}
		for cte in query.ctes {
			ctes << '${render_qname(cte.name, q)} AS (${render_query_inner(dialect, q, num, qm, mut c, cte.query)})'
		}
		out += 'WITH ${ctes.join(', ')} '
	}

	out += 'SELECT '
	if query.distinct {
		out += 'DISTINCT '
	}
	if query.items.len == 0 {
		out += '*'
	} else {
		mut items := []string{cap: query.items.len}
		for item in query.items {
			mut text := if item.star {
				'*'
			} else if expr := item.expr {
				render_expr(dialect, q, num, qm, mut c, expr)
			} else {
				render_qname(item.field, q)
			}
			if item.alias != '' {
				text += ' AS ${render_qname(item.alias, q)}'
			}
			items << text
		}
		out += items.join(', ')
	}

	from := if query.from.table == '' && query.from.query == none {
		render_qname(query.table, q)
	} else {
		render_from(dialect, q, num, qm, mut c, query.from)
	}
	out += ' FROM ${from}'

	for join in query.joins {
		out += ' ${render_join_kind(join.kind)} ${render_from(dialect, q, num, qm, mut c, join.from)}'
		if on := join.on {
			out += ' ON ${render_expr(dialect, q, num, qm, mut c, on)}'
		}
	}

	if where := query.where {
		out += ' WHERE ${render_expr(dialect, q, num, qm, mut c, where)}'
	}
	if query.group_by.len > 0 {
		mut parts := []string{cap: query.group_by.len}
		for expr in query.group_by {
			parts << render_expr(dialect, q, num, qm, mut c, expr)
		}
		out += ' GROUP BY ${parts.join(', ')}'
	}
	if having := query.having {
		out += ' HAVING ${render_expr(dialect, q, num, qm, mut c, having)}'
	}
	if query.order_by.len > 0 {
		mut parts := []string{cap: query.order_by.len}
		for item in query.order_by {
			parts << '${item.expr}${if item.desc { ' DESC' } else { '' }}'
		}
		out += ' ORDER BY ${parts.join(', ')}'
	}
	out += render_limit_offset(dialect, num, qm, mut c, query)

	for set_op in query.set_ops {
		op := match set_op.kind {
			.union { 'UNION' }
			.union_all { 'UNION ALL' }
			.intersect { 'INTERSECT' }
			.except { 'EXCEPT' }
		}
		out += ' ${op} ${render_query_inner(dialect, q, num, qm, mut c, set_op.query)}'
	}
	return out
}

// render_sql_query renders one complete SELECT plan. The returned params are
// in the same order as the `?`/`$n` markers in the returned SQL.
pub fn render_sql_query(dialect SQLDialect, q string, num bool, qm string, start_pos int, query SqlQuery) (string, []Primitive) {
	mut c := RowRenderState{ position: start_pos }
	sql := render_query_inner(dialect, q, num, qm, mut c, &query)
	return sql, c.params
}
