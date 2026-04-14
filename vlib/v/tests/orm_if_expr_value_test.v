import db.sqlite

struct IfExprUser {
	name string
}

fn test_orm_select_as_if_expr_value() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}
	use_empty_branch := false
	users := if use_empty_branch {
		[]IfExprUser{}
	} else {
		sql db {
			select from IfExprUser
		} or { []IfExprUser{} }
	}
	assert users.len == 0
}

fn test_orm_insert_as_if_expr_value() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}
	user := IfExprUser{
		name: 'Grace'
	}
	use_insert_branch := true
	_ = if use_insert_branch {
		sql db {
			insert user into IfExprUser
		} or { 1 }
	} else {
		0
	}
	assert true
}
