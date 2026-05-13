// vtest build: present_sqlite3?
import db.sqlite

struct Upper {
	id  int @[primary; sql: serial]
	sub SubStruct
}

struct SubStruct {
	id   int @[primary; sql: serial]
	name string
}

struct OrmDuplicateHuman {
	id   int @[primary; sql: serial]
	name string
}

struct OrmDuplicateRelation {
	id     int @[primary; sql: serial]
	kind   string
	human1 OrmDuplicateHuman @[fkey: 'id']
	human2 OrmDuplicateHuman @[fkey: 'id']
}

fn test_orm_sub_structs() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	sql db {
		create table Upper
	}!
	sql db {
		create table SubStruct
	}!

	upper_1 := Upper{
		sub: SubStruct{
			name: 'test123'
		}
	}

	sql db {
		insert upper_1 into Upper
	}!

	uppers := sql db {
		select from Upper where id == 1
	}!

	assert uppers.first().sub.name == upper_1.sub.name
	db.close()!
}

fn test_orm_sub_structs_with_duplicate_foreign_type_fields() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	sql db {
		create table OrmDuplicateHuman
	}!
	sql db {
		create table OrmDuplicateRelation
	}!

	relation := OrmDuplicateRelation{
		kind:   'couple'
		human1: OrmDuplicateHuman{
			name: 'adam'
		}
		human2: OrmDuplicateHuman{
			name: 'ewa'
		}
	}

	sql db {
		insert relation into OrmDuplicateRelation
	}!

	humans := sql db {
		select from OrmDuplicateHuman order by id
	}!
	assert humans.len == 2
	assert humans[0].name == 'adam'
	assert humans[1].name == 'ewa'

	relations := sql db {
		select from OrmDuplicateRelation where id == 1
	}!
	assert relations.len == 1
	assert relations[0].human1.name == 'adam'
	assert relations[0].human2.name == 'ewa'
	assert relations[0].human1.id != relations[0].human2.id
	db.close()!
}
