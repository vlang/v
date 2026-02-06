import db.sqlite
import math

struct Counter {
	id int @[primary; sql: serial]
	f  f64
}

fn test_orm_or_block() {
	db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		drop table Counter
	} or { println(math.e) } // this should compile

	x := sql db {
		select from Counter
	} or {
		[Counter{
			f: math.pi
		}]
	}
	assert x[0].f == math.pi
}
