module transform

import v3.flat

// transform_sql_expr transforms SQL/ORM expressions into function calls.
// V's ORM allows writing SQL-like queries directly in code:
//   users := sql db { select from User where age > 18 order by name }
// This would be lowered into the appropriate database driver function calls.
// For now, returns the node unchanged as a hook for future ORM support.
fn (mut t Transformer) transform_sql_expr(id flat.NodeId, _node flat.Node) flat.NodeId {
	// TODO: When ORM support is implemented:
	//   1. Parse the SQL expression structure from children
	//   2. Determine the target table and struct type
	//   3. Build the appropriate db.exec() or db.select() call
	//   4. Generate result mapping from rows to struct instances
	//   5. Handle where clauses, order by, limit, etc.
	return id
}
