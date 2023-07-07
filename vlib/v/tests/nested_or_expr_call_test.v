import db.sqlite

// sets a custom table name. Default is struct name (case-sensitive)
[table: 'customers']
struct Customer {
	id   int    [primary; sql: serial] // a field named `id` of integer type must be the first field
	name string
}

fn test_nested_or_expr_call() {
	db := sqlite.connect(':memory:')!

	sql db {
		create table Customer
	}!

	uid_map := map[int]string{}
	uid := 2
	username := if uid <= 0 {
		'unknown'
	} else {
		uid_map[uid] or {
			users := sql db {
				select from Customer where id == uid
			} or {
				[Customer{
					id: uid
					name: 'unknown'
				}]
			}
			name := if users.len == 1 { users[0].name } else { 'unknown' }
			name
		}
	}
	println('${uid} is ${username}')
	assert username == 'unknown'
}
