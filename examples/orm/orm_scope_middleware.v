// orm_scope_middleware.v - ORM middleware pattern demo
//
// Shows how middleware manages DataScope (multi-tenant) transparently.
// Business code only changes acquire() -> pool_acquire_scoped().
//
// Run: v run orm_scope_middleware.v

module main

import db.sqlite
import orm

// =============================================================================
// Model
// =============================================================================

@[table: 'sys_users']
struct SysUser {
	id        int @[primary; sql: serial]
	name      string
	tenant_id int
	org_id    int
}

// =============================================================================
// Request context
// =============================================================================

struct UserInfo {
mut:
	role      string
	tenant_id int
}

pub struct Context {
pub mut:
	user   UserInfo
	dbpool &DatabasePool
}

// =============================================================================
// Connection pool (SQLite version)
// In real projects, use adapter.dbpool.DatabasePoolable
// =============================================================================

pub struct DatabasePool {
	conn &sqlite.DB
}

pub fn new_pool() &DatabasePool {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	return &DatabasePool{
		conn: &db
	}
}

// acquire gets a connection from pool
pub fn (mut p DatabasePool) acquire() !&sqlite.DB {
	return p.conn
}

// release returns a connection to pool
pub fn (mut p DatabasePool) release(conn &sqlite.DB) ! {
	// no-op for single-connection SQLite
	// real pool: p.inner.put(conn)!
}

// =============================================================================
// Middleware layer -- pool_acquire_scoped
//
// Responsibilities:
//   1. Acquire connection from pool
//   2. Create orm.DB with DataScope injected
//   3. Skip scope filters based on user role
//   4. Return configured orm.DB
//
// Business code just replaces acquire() -> pool_acquire_scoped()
// =============================================================================

pub fn pool_acquire_scoped(mut ctx Context) !(orm.DB, &sqlite.DB) {
	// 1. acquire raw connection
	raw_conn := ctx.dbpool.acquire()!

	// 2. create base orm.DB with tenant scope
	base_db := orm.new_db(raw_conn, orm.DataScope{
		filters: [
			orm.QueryFilter{
				field: 'tenant_id'
				value: orm.Primitive(ctx.user.tenant_id)
				mode:  .dynamic
			},
		]
	})

	// 3. skip scope filters by role
	mut scoped_db := base_db
	match ctx.user.role {
		'admin' {
			scoped_db = scoped_db.unscoped()
		}
		'manager' {
			scoped_db = scoped_db.unscoped('org_id')
		}
		'normal' {}
		else {}
	}

	// 4. return scoped DB
	return scoped_db, raw_conn
}

// =============================================================================
// Business layer -- Repository
//
// No awareness of DataScope at all.
// Only change: acquire() -> pool_acquire_scoped()
// =============================================================================

fn get_users(mut ctx Context) ![]SysUser {
	db, conn := pool_acquire_scoped(mut ctx)!
	defer {
		ctx.dbpool.release(conn) or {}
	}

	return sql db {
		select from SysUser
	}!
}

fn get_user_by_name(mut ctx Context, name string) ![]SysUser {
	db, conn := pool_acquire_scoped(mut ctx)!
	defer {
		ctx.dbpool.release(conn) or {}
	}

	return sql db {
		select from SysUser where name == name
	}!
}

fn count_users(mut ctx Context) !int {
	db, conn := pool_acquire_scoped(mut ctx)!
	defer {
		ctx.dbpool.release(conn) or {}
	}

	return sql db {
		select count from SysUser
	}!
}

// =============================================================================
// Demo
// =============================================================================

fn main() {
	// setup
	mut pool := new_pool()
	db := pool.conn

	sql db {
		create table SysUser
	}!

	users := [
		SysUser{
			name:      'Alice'
			tenant_id: 1
			org_id:    10
		},
		SysUser{
			name:      'Bob'
			tenant_id: 1
			org_id:    20
		},
		SysUser{
			name:      'Charlie'
			tenant_id: 2
			org_id:    10
		},
		SysUser{
			name:      'Diana'
			tenant_id: 2
			org_id:    20
		},
	]
	for u in users {
		sql db {
			insert u into SysUser
		}!
	}

	println('=== test data ===')
	println('Alice  : tenant=1, org=10')
	println('Bob    : tenant=1, org=20')
	println('Charlie: tenant=2, org=10')
	println('Diana  : tenant=2, org=20')

	mut ctx := Context{
		user:   UserInfo{
			role:      'normal'
			tenant_id: 1
		}
		dbpool: pool
	}

	// scenario 1: normal user, tenant=1
	println('\n--- normal user (tenant=1) ---')
	users1 := get_users(mut ctx) or { panic(err) }
	println('got ${users1.len} rows:')
	for u in users1 {
		println('  - ${u.name} (tenant=${u.tenant_id})')
	}

	// scenario 2: manager, tenant=1 (skip org_id, no effect since only tenant_id scope)
	println('\n--- manager user (tenant=1) ---')
	ctx.user.role = 'manager'

	users2 := get_users(mut ctx) or { panic(err) }
	println('got ${users2.len} rows:')
	for u in users2 {
		println('  - ${u.name} (tenant=${u.tenant_id})')
	}

	// scenario 3: admin (skip all scopes)
	println('\n--- admin user (skip all scopes) ---')
	ctx.user.role = 'admin'

	users3 := get_users(mut ctx) or { panic(err) }
	println('got ${users3.len} rows:')
	for u in users3 {
		println('  - ${u.name} (tenant=${u.tenant_id})')
	}

	// scenario 4: normal user, tenant=2
	println('\n--- normal user (tenant=2) ---')
	ctx.user.role = 'normal'
	ctx.user.tenant_id = 2

	users4 := get_users(mut ctx) or { panic(err) }
	println('got ${users4.len} rows:')
	for u in users4 {
		println('  - ${u.name} (tenant=${u.tenant_id})')
	}

	// scenario 5: get_user_by_name
	println('\n--- get_user_by_name (admin, name=Alice) ---')
	ctx.user.role = 'admin'
	users_by_name := get_user_by_name(mut ctx, 'Alice') or { panic(err) }
	println('got ${users_by_name.len} rows:')
	for u in users_by_name {
		println('  - ${u.name} (tenant=${u.tenant_id})')
	}

	// scenario 6: count
	println('\n--- count (admin) ---')
	total := count_users(mut ctx) or { panic(err) }
	println('total: ${total}')

	// assertions
	println('\n=== assertions ===')
	assert users1.len == 2 // normal/tenant=1: Alice + Bob
	assert users2.len == 2 // manager/tenant=1: Alice + Bob
	assert users3.len == 4 // admin: all 4
	assert users4.len == 2 // normal/tenant=2: Charlie + Diana
	assert users_by_name.len == 1 // admin: Alice
	assert total == 4 // admin: all 4
	println('all passed ✓')
}
