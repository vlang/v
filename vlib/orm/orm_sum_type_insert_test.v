import db.sqlite

struct SomeStruct {
	foo int
	bar string
}

struct OtherStruct {
	baz f64
}

type SomeSum = SomeStruct | OtherStruct

fn test_sum_type_insert() {
	db := sqlite.connect(':memory:')!
	sql db {
		create table SomeStruct
	}!

	some := SomeSum(SomeStruct{})
	if some is SomeStruct {
		sql db {
			insert some into SomeStruct
		}!
	}
}
