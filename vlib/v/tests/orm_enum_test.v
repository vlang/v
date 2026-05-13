import db.sqlite

enum Number {
	zero
	one
	two
	four = 4
	five
}

enum RecordStatus as u8 {
	created
	running
	finished
}

struct Counter {
	id     int @[primary; sql: serial]
	number Number
}

struct Recording {
	id     string @[primary]
	status RecordStatus
}

fn test_orm_enum() {
	db := sqlite.connect(':memory:') or { panic(err) }
	sql db {
		create table Counter
	}!

	counter1 := Counter{
		number: .two
	}
	sql db {
		insert counter1 into Counter
	}!

	mut counters := sql db {
		select from Counter
	}!

	assert counters.first().number == counter1.number

	// test short enum syntax
	sql db {
		update Counter set number = .five where number == .two
	}!

	counters = sql db {
		select from Counter
	}!

	assert counters.first().number == .five
}

fn test_orm_u8_enum_update_from_selector_expr() {
	db := sqlite.connect(':memory:') or { panic(err) }
	sql db {
		create table Recording
	}!

	initial := Recording{
		id:     'record-1'
		status: .created
	}
	sql db {
		insert initial into Recording
	}!

	update_data := Recording{
		id:     initial.id
		status: .finished
	}
	sql db {
		update Recording set status = update_data.status where id == update_data.id
	}!

	rows := sql db {
		select from Recording where id == update_data.id
	}!

	assert rows.len == 1
	assert rows[0].status == .finished
}
