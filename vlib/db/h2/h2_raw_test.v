// vtest build: present_h2?
import db.h2

fn test_h2_raw_jdbc_roundtrip() {
	mut conn := h2.connect(':memory:') or { panic(err) }
	defer {
		conn.close()
	}

	created := conn.exec('CREATE TABLE "People" ("id" BIGINT AUTO_INCREMENT PRIMARY KEY, "name" VARCHAR, "age" BIGINT, "note" VARCHAR)', []) or {
		panic(err)
	}
	assert created.vals.len == 0

	conn.exec('INSERT INTO "People" ("name", "age", "note") VALUES (?, ?, ?)', ['Alice', '30',
		'hello']) or {
		panic(err)
	}
	conn.exec('INSERT INTO "People" ("name", "age", "note") VALUES (?, ?, ?)', ['Bob', '25', '']) or {
		panic(err)
	}
	conn.exec_nullable('INSERT INTO "People" ("name", "age", "note") VALUES (?, ?, ?)', [
		'Carol',
		'40',
		'',
	], [false, false, true]) or { panic(err) }

	rows := conn.exec('SELECT "id", "name", "age", "note" FROM "People" WHERE "age" > ? ORDER BY "age" DESC', ['26']) or {
		panic(err)
	}
	assert rows.names == ['id', 'name', 'age', 'note']
	assert rows.vals.len == 2
	assert rows.vals[0] == ['3', 'Carol', '40', '']
	assert rows.nulls[0] == [false, false, false, true]
	assert rows.vals[1] == ['1', 'Alice', '30', 'hello']
	assert rows.nulls[1] == [false, false, false, false]
	assert conn.last_id() == 3

	empty_rows := conn.exec('SELECT "name" FROM "People" WHERE "age" > ?', ['100']) or {
		panic(err)
	}
	assert empty_rows.vals.len == 0
}
