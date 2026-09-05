import db.sqlite

// A backtick delimited `default:` attribute value is a plain string, and reaches
// the DB as a quoted SQL literal; anything else stays verbatim SQL.
// See https://github.com/vlang/v/issues/27987
@[table: 'demo_default']
struct DemoDefault {
	id        int    @[primary; sql: serial]
	home_path string @[default: '`/dashboard`']
	tags      string @[default: '`["all"]`']
	method    string @[default: '`POST`']
	quoted    string @[default: "`o'brien`"]
	amount    int    @[default: 42]
}

fn test_string_defaults_are_emitted_as_quoted_sql_literals() {
	mut db := sqlite.connect(':memory:')!
	defer {
		db.close() or {}
	}
	sql db {
		create table DemoDefault
	}!
	ddl_rows := db.exec("SELECT sql FROM sqlite_master WHERE name = 'demo_default'")!
	assert ddl_rows.len == 1
	ddl := ddl_rows[0].vals[0] or { '' }
	assert ddl.contains("DEFAULT '/dashboard'"), ddl
	assert ddl.contains('DEFAULT \'["all"]\''), ddl
	assert ddl.contains("DEFAULT 'POST'"), ddl
	assert ddl.contains("DEFAULT 'o''brien'"), ddl
	assert ddl.contains('DEFAULT 42'), ddl

	db.exec('INSERT INTO demo_default (id) VALUES (1)')!
	rows := sql db {
		select from DemoDefault where id == 1
	}!
	assert rows.len == 1
	assert rows[0].home_path == '/dashboard'
	assert rows[0].tags == '["all"]'
	assert rows[0].method == 'POST'
	assert rows[0].quoted == "o'brien"
	assert rows[0].amount == 42
}
