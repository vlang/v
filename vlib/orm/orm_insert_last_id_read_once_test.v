import orm

// ConsumingIdDb mimics a pooled `db.pg.DB`, where `last_id()` hands out the id
// stashed by the previous insert once and returns 0 afterwards.
struct ConsumingIdDb {
mut:
	next_id    int
	stashed_id int
	reads      int
}

struct Item {
	id   int @[primary; sql: serial]
	name string
}

fn (mut db ConsumingIdDb) select(_ orm.SelectConfig, _ orm.QueryData, _ orm.QueryData) ![][]orm.Primitive {
	return [][]orm.Primitive{}
}

fn (mut db ConsumingIdDb) insert(_ orm.Table, _ orm.QueryData) ! {
	db.next_id++
	db.stashed_id = db.next_id
}

fn (mut db ConsumingIdDb) update(_ orm.Table, _ orm.QueryData, _ orm.QueryData) ! {}

fn (mut db ConsumingIdDb) delete(_ orm.Table, _ orm.QueryData) ! {}

fn (mut db ConsumingIdDb) create(_ orm.Table, _ []orm.TableField) ! {}

fn (mut db ConsumingIdDb) drop(_ orm.Table) ! {}

fn (mut db ConsumingIdDb) last_id() int {
	db.reads++
	id := db.stashed_id
	db.stashed_id = 0
	return id
}

fn (mut db ConsumingIdDb) execute(_ string) ![]orm.Row {
	return []orm.Row{}
}

fn test_insert_reads_last_id_once_per_row() {
	mut db := ConsumingIdDb{}
	item := Item{
		name: 'a'
	}
	first_id := sql db {
		insert item into Item
	}!
	second_id := sql db {
		insert item into Item
	}!
	assert first_id == 1
	assert second_id == 2
	assert db.reads == 2

	items := [item, item]
	last_id := sql db {
		insert items into Item
	}!
	assert last_id == 4
	assert db.reads == 4
}
