// vtest build: present_sqlite3? && gcc && !windows
// vtest vflags: -prod
import db.sqlite

@[table: 'issue_24800_foos']
struct Issue24800Foo {
	id   int @[primary; sql: serial]
	name string
}

struct Issue24800App {
mut:
	db sqlite.DB
}

fn issue_24800_memory_app() !Issue24800App {
	db := sqlite.connect(':memory:')!
	sql db {
		create table Issue24800Foo
	}!
	return Issue24800App{
		db: db
	}
}

fn test_issue_24800_prod_gcc_orm_create_table() {
	mut app := issue_24800_memory_app()!
	assert app.db.q_int("select count(*) from sqlite_master where type='table' and name='issue_24800_foos'")! == 1
	app.db.close()!
}
