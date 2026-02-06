// vtest retry: 3
import db.sqlite

struct Member {
	id       int      @[primary]
	children ?[]Child @[fkey: parent_id]
}

struct Child {
	id        int @[primary]
	parent_id int
}

fn test_main() {
	mut db := sqlite.connect(':memory:') or { panic(err) }
	defer {
		db.close() or { panic(err) }
	}
	sql db {
		create table Member
	} or { println(err) }
	sql db {
		create table Child
	} or { println(err) }
	new_member := Member{
		id:       1
		children: [Child{
			id:        1
			parent_id: 1
		}, Child{
			id:        2
			parent_id: 1
		}]
	}
	sql db {
		insert new_member into Member
	}!

	rows := sql db {
		select from Member
	}!

	assert rows[0].children?.len == 2
	assert rows[0].children?[0].id == 1
	assert rows[0].children?[1].id == 2
}
