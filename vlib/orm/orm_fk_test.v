// vtest retry: 3
import db.sqlite

struct Person {
	id                int @[primary; sql: serial]
	age               int
	brothers          []Brother @[fkey: 'person_id']
	sisters           []Sister  @[fkey: 'person_id']
	field_after_fkeys string
}

struct Brother {
	id        int @[primary; sql: serial]
	person_id int
	name      string
}

struct Sister {
	id        int @[primary; sql: serial]
	person_id int
	name      string
}

fn test_field_after_fkeys() {
	db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table Brother
		create table Sister
		create table Person
	}!

	person := Person{
		age:               21
		brothers:          [Brother{
			name: 'aaa'
		}, Brother{
			name: 'bbb'
		}]
		sisters:           [Sister{
			name: 'ccc'
		}, Sister{
			name: 'ddd'
		}]
		field_after_fkeys: 'eee'
	}

	sql db {
		insert person into Person
	}!

	persons := sql db {
		select from Person
	}!

	assert persons[0].age == 21
	assert persons[0].field_after_fkeys == 'eee'
}
