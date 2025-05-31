import db.sqlite

@[table: 'prefixed_records']
struct Record {
	id   int    @[primary; sql: 'CustomId']
	name string @[sql: 'named_name']
}

fn test_main() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }

	prepare(db)!

	last := sql db {
		select from Record where name == 'first' order by id limit 1
	}!
	assert last.len == 1
	assert last[0].name == 'first'
}

fn prepare(db sqlite.DB) ! {
	db.exec('
  CREATE TABLE prefixed_records (
    CustomId INTEGER PRIMARY KEY AUTOINCREMENT,
    named_name TEXT
  );
  ')!

	db.exec("
  INSERT INTO prefixed_records
    (named_name)
  VALUES
    ('first'),
    ('last');
  ")!
}
