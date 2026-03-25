import db.sqlite

struct Person {
	name   string
	height Height
}

enum Height as u8 {
	tall
	small
}

struct Account {
	id     string @[primary]
	status u8
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

fn test_update_with_if_expr() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Account
	}!

	account := Account{
		id:     'acc-1'
		status: 0
	}
	sql db {
		insert account into Account
	}!

	req_status := true
	sql db {
		update Account set status = if req_status { 1 } else { 0 } where id == 'acc-1'
	}!

	rows := sql db {
		select from Account where id == 'acc-1'
	}!

	assert rows.len == 1
	assert rows[0].status == 1
}
