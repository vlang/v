// vtest retry: 3
import db.sqlite
import orm

struct NoScopeUser {
	id        int @[primary; sql: serial]
	name      string
	tenant_id int
}

@[table: 'noscope_users2']
struct NoScopeUserMulti {
	id      int @[primary; sql: serial]
	name    string
	org_id  int
	deleted bool
}

fn test_unscoped_skips_tenant_filter_in_select() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	mut db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
		]
	})

	// Without unscoped - scope filter applies, only Alice (tenant_id=1)
	users_filtered := sql db {
		select from NoScopeUser
	}!
	assert users_filtered.len == 1
	assert users_filtered[0].name == 'Alice'

	// With db.unscoped('tenant_id') - scope skipped, all users visible
	users_all := sql db unscoped(tenant_id) {
		select from NoScopeUser
	}!
	assert users_all.len == 2
}

fn test_unscoped_skip_all_in_select() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	mut db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
		]
	})

	// db.unscoped() skips ALL scope filters
	users_all := sql db unscoped() {
		select from NoScopeUser
	}!
	assert users_all.len == 2
}

fn test_unscoped_selective_skip_in_multi_field_scope() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUserMulti
	}!

	u1 := NoScopeUserMulti{
		name:    'A'
		org_id:  1
		deleted: false
	}
	u2 := NoScopeUserMulti{
		name:    'B'
		org_id:  1
		deleted: true
	}
	u3 := NoScopeUserMulti{
		name:    'C'
		org_id:  2
		deleted: false
	}
	u4 := NoScopeUserMulti{
		name:    'D'
		org_id:  2
		deleted: true
	}

	sql raw_db {
		insert u1 into NoScopeUserMulti
		insert u2 into NoScopeUserMulti
		insert u3 into NoScopeUserMulti
		insert u4 into NoScopeUserMulti
	}!

	mut db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'org_id'
				value: orm.Primitive(1)
			},
			orm.QueryFilter{
				field: 'deleted'
				value: orm.Primitive(false)
			},
		]
	})

	// Both scope filters apply - only org_id=1 AND deleted=false
	users_filtered := sql db {
		select from NoScopeUserMulti
	}!
	assert users_filtered.len == 1
	assert users_filtered[0].name == 'A'

	// Skip only 'org_id' - 'deleted' filter still applies
	users := sql db unscoped(org_id) {
		select from NoScopeUserMulti
	}!
	// Should match deleted=false regardless of org_id
	assert users.len == 2
	assert users[0].name in ['A', 'C']
	assert users[1].name in ['A', 'C']
}

fn test_unscoped_skips_tenant_in_insert() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	mut db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(99)
			},
		]
	})

	// With db.unscoped('tenant_id') - scope does NOT inject tenant_id=99
	alice := NoScopeUser{
		name: 'Alice'
	}
	sql db unscoped(tenant_id) {
		insert alice into NoScopeUser
	}!

	// Alice was inserted with tenant_id=0 (no auto-inject), so scope won't find her
	users := sql db {
		select from NoScopeUser
	}!
	assert users.len == 0
}

fn test_unscoped_skip_all_in_insert() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	mut db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(99)
			},
		]
	})

	bob := NoScopeUser{
		name: 'Bob'
	}

	// db.unscoped() skips ALL scope field injection in insert
	sql db unscoped() {
		insert bob into NoScopeUser
	}!

	// Bob was inserted with tenant_id=0 (not auto-injected), not visible under scope
	users := sql db {
		select from NoScopeUser
	}!
	assert users.len == 0
}

fn test_unscoped_skips_tenant_in_update() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	mut db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
		]
	})

	// With scope tenant_id=1, update where name='Bob' normally won't match
	// because scope ANDs tenant_id=1, making it (name='Bob' AND tenant_id=1)
	sql db {
		update NoScopeUser set name = 'UpdatedByScope' where name == 'Bob'
	}!

	// Bob (tenant_id=2) should NOT have been updated
	bob_check := sql raw_db {
		select from NoScopeUser where name == 'Bob' && tenant_id == 2
	}!
	assert bob_check.len == 1
	assert bob_check[0].name == 'Bob'

	// With db.unscoped('tenant_id'), the scope filter is skipped in the WHERE,
	// so name='Bob' matches regardless of tenant_id
	sql db unscoped(tenant_id) {
		update NoScopeUser set name = 'UpdatedByNoScope' where name == 'Bob'
	}!

	bob_updated := sql raw_db {
		select from NoScopeUser where tenant_id == 2
	}!
	assert bob_updated.len == 1
	assert bob_updated[0].name == 'UpdatedByNoScope'
}

fn test_unscoped_skip_all_in_update() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	mut db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
			orm.QueryFilter{
				field: 'name'
				value: orm.Primitive('Alice')
			},
		]
	})

	// db.unscoped() skips ALL scope filters in the WHERE clause
	// Without unscoped, the WHERE would include both tenant_id=1 AND name='Alice',
	// making WHERE name='Bob' become (name='Bob' AND tenant_id=1 AND name='Alice'), which
	// would not match anything due to the name conflict.
	// With unscoped(), no scope filters are added, so name='Bob' matches directly.
	sql db unscoped() {
		update NoScopeUser set name = 'UpdatedAll' where name == 'Bob'
	}!

	bob_updated := sql raw_db {
		select from NoScopeUser where tenant_id == 2
	}!
	assert bob_updated.len == 1
	assert bob_updated[0].name == 'UpdatedAll'
}

fn test_unscoped_skips_tenant_in_delete() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	mut db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
		]
	})

	// With scope tenant_id=1, delete where name='Bob' won't match
	sql db {
		delete from NoScopeUser where name == 'Bob'
	}!

	// Bob should still exist (scope prevented deletion)
	bob_check := sql raw_db {
		select from NoScopeUser where tenant_id == 2
	}!
	assert bob_check.len == 1

	// With db.unscoped('tenant_id'), scope filter skipped, Bob gets deleted
	sql db unscoped(tenant_id) {
		delete from NoScopeUser where name == 'Bob'
	}!

	bob_gone := sql raw_db {
		select from NoScopeUser where tenant_id == 2
	}!
	assert bob_gone.len == 0
}

fn test_unscoped_skip_all_in_delete() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	mut db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
			orm.QueryFilter{
				field: 'name'
				value: orm.Primitive('Alice')
			},
		]
	})

	// db.unscoped() skips ALL scope filters
	// Without it, delete where name='Bob' becomes (name='Bob' AND tenant_id=1 AND name='Alice')
	// which can't match (Bob != Alice).
	// With unscoped(), delete where name='Bob' matches directly.
	sql db unscoped() {
		delete from NoScopeUser where name == 'Bob'
	}!

	bob_gone := sql raw_db {
		select from NoScopeUser where tenant_id == 2
	}!
	assert bob_gone.len == 0
}

// ---- Method call syntax tests: db.unscoped(fields...) ----------------

fn test_unscoped_method_call_in_select() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	mut db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
		]
	})

	// Without scope skip - only Alice (tenant_id=1) visible
	users_filtered := sql db {
		select from NoScopeUser
	}!
	assert users_filtered.len == 1
	assert users_filtered[0].name == 'Alice'

	// Using db.unscoped('tenant_id') - scope skipped, all users visible
	db.unscoped('tenant_id')
	users_all := sql db {
		select from NoScopeUser
	}!
	assert users_all.len == 2

	// Using db.unscoped() - all scope filters skipped
	db.reset_scopes()
	db.unscoped()
	users_all2 := sql db {
		select from NoScopeUser
	}!
	assert users_all2.len == 2
}

fn test_unscoped_method_call_in_update() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	mut db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
		]
	})

	// With scope, update where name='Bob' doesn't match (scope adds tenant_id=1)
	sql db {
		update NoScopeUser set name = 'Changed' where name == 'Bob'
	}!

	bob_still := sql raw_db {
		select from NoScopeUser where tenant_id == 2
	}!
	assert bob_still.len == 1
	assert bob_still[0].name == 'Bob'

	// Using db.unscoped('tenant_id') - scope skipped, Bob gets updated
	db.unscoped('tenant_id')
	sql db {
		update NoScopeUser set name = 'UpdatedViaMethod' where name == 'Bob'
	}!

	bob_updated := sql raw_db {
		select from NoScopeUser where tenant_id == 2
	}!
	assert bob_updated.len == 1
	assert bob_updated[0].name == 'UpdatedViaMethod'
}

fn test_unscoped_method_call_in_delete() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	mut db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
		]
	})

	// With scope, delete where name='Bob' doesn't match
	sql db {
		delete from NoScopeUser where name == 'Bob'
	}!

	bob_still := sql raw_db {
		select from NoScopeUser where tenant_id == 2
	}!
	assert bob_still.len == 1

	// Using db.unscoped('tenant_id') - scope skipped, Bob gets deleted
	db.unscoped('tenant_id')
	sql db {
		delete from NoScopeUser where name == 'Bob'
	}!

	bob_gone := sql raw_db {
		select from NoScopeUser where tenant_id == 2
	}!
	assert bob_gone.len == 0
}

// ---- DataScope tests -------------------------------------------------

fn empty_scope() orm.DataScope {
	return orm.DataScope{}
}

fn scope_single_tenant(tenant_id int) orm.DataScope {
	return orm.DataScope{
		filters: [
			orm.QueryFilter{
				field:    'tenant_id'
				value:    orm.Primitive(int(tenant_id))
				operator: .eq
			},
		]
	}
}

fn scope_disabled() orm.DataScope {
	return orm.DataScope{
		enabled: false
		filters: [
			orm.QueryFilter{
				field:    'tenant_id'
				value:    orm.Primitive(int(1))
				operator: .eq
			},
		]
	}
}

fn test_apply_data_scope_single_filter() {
	scope := scope_single_tenant(5)
	where := orm.QueryData{}
	table := orm.Table{
		name: 'users'
	}
	result := orm.apply_data_scope(scope, table, where, [])
	assert result.fields == ['tenant_id']
	assert result.data == [orm.Primitive(int(5))]
	assert result.kinds == [.eq]
}

fn test_apply_data_scope_appends_to_existing_where() {
	scope := scope_single_tenant(42)
	where := orm.QueryData{
		fields: ['id']
		data:   [orm.Primitive(int(1))]
		kinds:  [.eq]
	}
	table := orm.Table{
		name: 'users'
	}
	result := orm.apply_data_scope(scope, table, where, [])
	assert result.fields == ['id', 'tenant_id']
	assert result.data == [orm.Primitive(int(1)), orm.Primitive(int(42))]
	assert result.kinds == [.eq, .eq]
	assert result.is_and == [true]
}

fn test_apply_data_scope_no_duplicate_field() {
	scope := scope_single_tenant(5)
	where := orm.QueryData{
		fields: ['tenant_id']
		data:   [orm.Primitive(int(10))]
		kinds:  [.eq]
	}
	table := orm.Table{
		name: 'users'
	}
	result := orm.apply_data_scope(scope, table, where, [])
	assert result.fields == ['tenant_id']
	assert result.data == [orm.Primitive(int(10))]
}

fn test_apply_data_scope_empty_or_disabled() {
	where := orm.QueryData{
		fields: ['id']
		data:   [orm.Primitive(int(1))]
		kinds:  [.eq]
	}
	table := orm.Table{
		name: 'users'
	}
	empty_result := orm.apply_data_scope(empty_scope(), table, where, [])
	assert empty_result.fields == ['id']
	disabled_result := orm.apply_data_scope(scope_disabled(), table, where, [])
	assert disabled_result.fields == ['id']
}

fn test_apply_data_scope_multi_field() {
	scope := orm.DataScope{
		filters: [
			orm.QueryFilter{
				field:    'org_id'
				value:    orm.Primitive(int(1))
				operator: .eq
			},
			orm.QueryFilter{
				field:    'deleted'
				value:    orm.Primitive(false)
				operator: .eq
			},
		]
	}
	where := orm.QueryData{}
	table := orm.Table{
		name: 'users'
	}
	result := orm.apply_data_scope(scope, table, where, [])
	assert result.fields == ['org_id', 'deleted']
	assert result.data == [orm.Primitive(int(1)), orm.Primitive(false)]
	assert result.kinds == [.eq, .eq]
}

fn test_apply_data_scope_wraps_parentheses() {
	scope := scope_single_tenant(42)
	where := orm.QueryData{
		fields: ['a', 'b']
		kinds:  [.eq, .eq]
		is_and: [false]
	}
	table := orm.Table{
		name: 'users'
	}
	result := orm.apply_data_scope(scope, table, where, [])
	assert result.fields == ['a', 'b', 'tenant_id']
	assert result.is_and == [false, true]
	assert result.parentheses.len == 1
	assert result.parentheses[0] == [0, 1]
}

fn test_apply_data_scope_insert_adds_fields() {
	scope := scope_single_tenant(99)
	data := orm.QueryData{
		fields: ['name']
		data:   [orm.Primitive('alice')]
	}
	table := orm.Table{
		name: 'users'
	}
	result := orm.apply_data_scope_insert(scope, table, data, [])
	assert result.fields == ['name', 'tenant_id']
	assert result.data == [orm.Primitive('alice'), orm.Primitive(int(99))]
}

fn test_apply_data_scope_insert_no_override() {
	scope := scope_single_tenant(99)
	data := orm.QueryData{
		fields: ['tenant_id', 'name']
		data:   [orm.Primitive(int(7)), orm.Primitive('bob')]
	}
	table := orm.Table{
		name: 'users'
	}
	result := orm.apply_data_scope_insert(scope, table, data, [])
	assert result.fields == ['tenant_id', 'name']
	assert result.data == [orm.Primitive(int(7)), orm.Primitive('bob')]
}

fn test_apply_data_scope_insert_empty_or_disabled() {
	data := orm.QueryData{
		fields: ['name']
		data:   [orm.Primitive('alice')]
	}
	table := orm.Table{
		name: 'users'
	}
	empty_result := orm.apply_data_scope_insert(empty_scope(), table, data, [])
	assert empty_result.fields == ['name']
	disabled_result := orm.apply_data_scope_insert(scope_disabled(), table, data, [])
	assert disabled_result.fields == ['name']
}

// ---- scope_skip_fields unit tests ----------------------------------------

fn test_apply_data_scope_skip_single_field() {
	scope := scope_single_tenant(5)
	where := orm.QueryData{}
	table := orm.Table{
		name: 'users'
	}
	// Skip 'tenant_id' - it should not be applied
	result := orm.apply_data_scope(scope, table, where, ['tenant_id'])
	assert result.fields == []
	assert result.data == []
}

fn test_apply_data_scope_skip_all_with_wildcard() {
	scope := scope_single_tenant(5)
	where := orm.QueryData{}
	table := orm.Table{
		name: 'users'
	}
	// Skip all with ['*']
	result := orm.apply_data_scope(scope, table, where, ['*'])
	assert result.fields == []
	assert result.data == []
}

fn test_apply_data_scope_skip_field_still_applies_others() {
	scope := orm.DataScope{
		filters: [
			orm.QueryFilter{
				field:    'org_id'
				value:    orm.Primitive(int(1))
				operator: .eq
			},
			orm.QueryFilter{
				field:    'deleted'
				value:    orm.Primitive(false)
				operator: .eq
			},
		]
	}
	where := orm.QueryData{}
	table := orm.Table{
		name: 'users'
	}
	// Skip only 'org_id', 'deleted' should still be applied
	result := orm.apply_data_scope(scope, table, where, ['org_id'])
	assert result.fields == ['deleted']
	assert result.data == [orm.Primitive(false)]
}

fn test_apply_data_scope_skip_non_existent_field() {
	scope := scope_single_tenant(5)
	where := orm.QueryData{}
	table := orm.Table{
		name: 'users'
	}
	// Skip a non-existent field - all filters should still be applied
	result := orm.apply_data_scope(scope, table, where, ['nonexistent'])
	assert result.fields == ['tenant_id']
	assert result.data == [orm.Primitive(int(5))]
}

fn test_apply_data_scope_insert_skip_single_field() {
	scope := scope_single_tenant(99)
	data := orm.QueryData{
		fields: ['name']
		data:   [orm.Primitive('alice')]
	}
	table := orm.Table{
		name: 'users'
	}
	// Skip 'tenant_id' in insert - should not inject it
	result := orm.apply_data_scope_insert(scope, table, data, ['tenant_id'])
	assert result.fields == ['name']
	assert result.data == [orm.Primitive('alice')]
}

fn test_apply_data_scope_insert_skip_all_with_wildcard() {
	scope := scope_single_tenant(99)
	data := orm.QueryData{
		fields: ['name']
		data:   [orm.Primitive('alice')]
	}
	table := orm.Table{
		name: 'users'
	}
	// Skip all with ['*'] in insert
	result := orm.apply_data_scope_insert(scope, table, data, ['*'])
	assert result.fields == ['name']
	assert result.data == [orm.Primitive('alice')]
}

// ---- Middleware pattern tests: db configured per-request, business code is scope-unaware ----

// Simulates a request context with a per-request db.
// In a real middleware, the db would be configured once at request entry,
// and all subsequent handlers use ctx.db without knowing about scopes.
struct RequestCtx {
mut:
	db orm.DB
}

fn test_middleware_admin_skips_all_scopes() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	base_db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
		]
	})

	// --- Middleware: on request entry, configure per-request db by role ---
	// Admin role: skip all scopes
	mut admin_db := base_db
	admin_db.unscoped()
	mut ctx := RequestCtx{
		db: admin_db
	}
	// --- End middleware ---

	// --- Business handler: just extract db from ctx, use it like always ---
	db := ctx.db
	users := sql db {
		select from NoScopeUser
	}!
	// Admin sees all users because middleware configured no scopes
	assert users.len == 2

	users2 := sql db {
		select from NoScopeUser
	}!
	// Second query also sees all - middleware config persists
	assert users2.len == 2
}

fn test_middleware_manager_skips_specific_scope() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	base_db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
		]
	})

	// --- Middleware: manager skips tenant_id filter ---
	mut manager_db := base_db
	manager_db.unscoped('tenant_id')
	mut ctx := RequestCtx{
		db: manager_db
	}

	// --- Business handler: just extract db from ctx ---
	db := ctx.db
	users := sql db {
		select from NoScopeUser
	}!
	// Manager sees all because tenant_id scope is skipped
	assert users.len == 2
}

fn test_middleware_normal_user_has_full_scopes() {
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	base_db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
		]
	})

	// --- Middleware: normal user, no scope skipping ---
	mut ctx := RequestCtx{
		db: base_db
	}

	// --- Business handler: just extract db from ctx ---
	db := ctx.db
	users := sql db {
		select from NoScopeUser
	}!
	// Normal user sees only Alice (tenant_id=1) - scope is fully applied
	assert users.len == 1
	assert users[0].name == 'Alice'
}

fn test_middleware_mixed_roles_produce_isolated_results() {
	// Simulates multiple concurrent requests with different role configurations.
	// Each request has its own ctx.db, so they don't interfere.
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	base_db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
		]
	})

	// Request 1: admin - no scopes
	mut admin_db := base_db
	admin_db.unscoped()
	mut admin_ctx := RequestCtx{
		db: admin_db
	}
	// Request 2: normal user - full scopes
	mut normal_ctx := RequestCtx{
		db: base_db
	}

	// Both "handlers" execute with their own ctx.db - results are isolated
	db := admin_ctx.db
	admin_users := sql db {
		select from NoScopeUser
	}!
	assert admin_users.len == 2 // admin sees all

	normal_db := normal_ctx.db
	normal_users := sql normal_db {
		select from NoScopeUser
	}!
	assert normal_users.len == 1 // normal user scoped
	assert normal_users[0].name == 'Alice'

	// Admin still sees all - persistent on per-request db
	admin_users2 := sql db {
		select from NoScopeUser
	}!
	assert admin_users2.len == 2
}

fn test_middleware_ignores_scope_affects_all_crud_operations() {
	// Verifies that middleware-configured db.unscoped() works
	// for all CRUD operations without business code awareness.
	mut raw_db := sqlite.connect(':memory:') or { panic(err) }

	sql raw_db {
		create table NoScopeUser
	}!

	alice := NoScopeUser{
		name:      'Alice'
		tenant_id: 1
	}
	bob := NoScopeUser{
		name:      'Bob'
		tenant_id: 2
	}

	sql raw_db {
		insert alice into NoScopeUser
		insert bob into NoScopeUser
	}!

	base_db := orm.new_db(raw_db, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(1)
			},
		]
	})

	// Middleware configures admin db at request entry
	mut ctx := RequestCtx{
		db: base_db
	}
	ctx.db.unscoped('tenant_id')

	// Business handler: just extract db from ctx, use it like always
	db := ctx.db

	// UPDATE, SELECT, DELETE are all scope-unaware
	sql db {
		update NoScopeUser set name = 'AdminUpdated' where name == 'Bob'
	}!

	bob_updated := sql raw_db {
		select from NoScopeUser where tenant_id == 2
	}!
	assert bob_updated.len == 1
	assert bob_updated[0].name == 'AdminUpdated'

	// SELECT
	users := sql db {
		select from NoScopeUser
	}!
	assert users.len == 2

	// DELETE: also scope-unaware
	sql db {
		delete from NoScopeUser where name == 'Alice'
	}!

	alice_gone := sql raw_db {
		select from NoScopeUser where name == 'Alice'
	}!
	assert alice_gone.len == 0
}
