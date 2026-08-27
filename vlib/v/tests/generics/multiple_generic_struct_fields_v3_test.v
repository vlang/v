$if macos || linux {
	import db.pg

	struct App {
	pub:
		db &pg.DB
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

	fn test_multiple_generic_struct_fields_keep_independent_types() {
		mut app := App{
			db:      unsafe { nil }
			post:    Repo[Post]{}
			profile: Repo[Profile]{}
		}
		_ = app
	}
} $else {
	fn test_multiple_generic_struct_fields_keep_independent_types() {
		assert true
	}
}
