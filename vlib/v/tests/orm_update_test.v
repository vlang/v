import db.sqlite

struct Person {
	name   string
	height Height
}

enum Height as u8 {
	tall
	small
}

fn test_main() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Person
	}!

	a := Person{'A', Height.small}
	b := Person{'A', Height.tall}

	sql db {
		insert a into Person
	}!

	sql db {
		insert b into Person
	}!

	new_height := Height.small
	sql db {
		update Person set height = new_height where height == Height.tall
	}!

	rows := sql db {
		select from Person where height == Height.small
	}!

	assert rows.len == 2
}
