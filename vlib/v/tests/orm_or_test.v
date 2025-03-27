import db.sqlite
import math

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
		drop table Counter
	} or { println(math.e) } // this should compile

	x := sql db {
		select from Counter
	} or {
		println(math.pi) // this should compile
		[]Counter{}
	}
}
