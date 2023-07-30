import db.sqlite

struct Foo {
	age  int
	name string
}

fn do_create[T](db sqlite.DB) ! {
	sql db {
		create table T
	}!
}

fn do_insert[T](db sqlite.DB, val T) ! {
	sql db {
		insert val into T
	}!
}

fn do_select[T](db sqlite.DB) ![]T {
	vals := sql db {
		select from T
	}!

	return vals
}

fn test_orm_generic() {
	db := sqlite.connect(':memory:')!

	do_create[Foo](db)!

	foo := Foo{
		age: 20
		name: 'Veasal'
	}
	do_insert(db, foo)!

	selected := do_select[Foo](db)!
	assert selected.len == 1
	assert selected[0] == foo
}
