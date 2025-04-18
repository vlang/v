module main

import db.sqlite

pub struct SiteCredential {
pub:
	id int @[primary; sql: serial]
}

fn test_main() {
	shared db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table SiteCredential
	}!

	a := SiteCredential{
		id: 123
	}

	sql db {
		delete from SiteCredential where id != 0
	}!

	sql db {
		insert a into SiteCredential
	}!

	sql db {
		update SiteCredential set id = 321 where id != 0
	}!

	sql db {
		delete from SiteCredential where id != 0
	}!

	sql db {
		drop table SiteCredential
	}!
}
