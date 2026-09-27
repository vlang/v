import orm

struct TestDB {}

fn (db TestDB) select(_ orm.SelectConfig, _ orm.QueryData, _ orm.QueryData) ![][]orm.Primitive {
	return []
}

fn (db TestDB) insert(_ orm.Table, _ orm.QueryData) ! {}

fn (db TestDB) update(_ orm.Table, _ orm.QueryData, _ orm.QueryData) ! {}

fn (db TestDB) delete(_ orm.Table, _ orm.QueryData) ! {}

fn (db TestDB) create(_ orm.Table, _ []orm.TableField) ! {}

fn (db TestDB) drop(_ orm.Table) ! {}

fn (db TestDB) last_id() int {
	return 0
}

fn (db TestDB) execute(_ string) ![]orm.Row {
	return []
}

struct App {
pub:
	db &TestDB
mut:
	post    Repo[Post]
	profile Repo[Profile]
}

struct Post {
	id string
}

struct Profile {
	id string
}

struct Repo[T] {
mut:
	app &App
}

fn (r Repo[T]) get[T](id string) !T {
	rows := sql r.app.db {
		select from T where id == id
	}!
	if rows.len < 1 {
		return error('not found')
	}
	return rows.first()
}

fn (r Repo[T]) fetch[T](id string) T {
	return r.get(id) or { T{} }
}

fn main() {
	mut app := App{
		db:      &TestDB{}
		post:    Repo[Post]{}
		profile: Repo[Profile]{}
	}
	_ = app
}
