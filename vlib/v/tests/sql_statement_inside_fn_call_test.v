import db.sqlite

struct Movie {
	id   int @[primary]
	name string
}

fn x(m Movie) int {
	return m.id
}

fn test_sql_statement_inside_fn_call() {
	mut db := sqlite.connect(':memory:') or { panic('failed') }
	sql db {
		create table Movie
	}!
	m := Movie{1, 'Maria'}
	sql db {
		insert m into Movie
	}!
	dump(x(sql db {
		select from Movie where id == 1
	}!.first()))
	db.close()!
}
