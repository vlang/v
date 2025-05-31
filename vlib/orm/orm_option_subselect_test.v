import db.sqlite

fn test_main() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Commit
		create table Measurement
	}!

	c := Commit{
		commit_hash: 'hash'
	}
	sql db {
		insert c into Commit
	}!

	c2 := sql db {
		select from Commit
	}!
	assert c2[0].v_self_default == none

	c3 := Commit{
		commit_hash:    'hash1'
		v_self_default: Measurement{
			id: 123
		}
	}
	sql db {
		insert c3 into Commit
	}!

	c4 := sql db {
		select from Commit
	}!
	assert c4[0].v_self_default == none
	assert c4[1].v_self_default != none
}

@[table: 'commits']
struct Commit {
	commit_hash    string @[primary]
	v_self_default ?Measurement
}

@[table: 'measurements']
struct Measurement {
	id int @[primary; serial]
}
