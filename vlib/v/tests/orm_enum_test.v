import db.sqlite

enum Number {
	zero
	one
	two
	four = 4
	five
}

struct Counter {
	id     int @[primary; sql: serial]
	number Number
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
