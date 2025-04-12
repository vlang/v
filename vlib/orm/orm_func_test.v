import orm
import db.sqlite
import time

@[table: 'sys_users']
struct User {
	id         int @[primary; serial]
	name       string
	age        int
	role       string
	status     int
	salary     int
	title      string
	score      int
	created_at ?time.Time @[sql_type: 'TIMESTAMP']
}

fn test_orm_func_where() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	mut qb := orm.new_query[User](db)

	// single_condition
	qb.reset()
	qb.where('age > ?', 25)
	assert qb.where.fields == ['age']
	assert qb.where.kinds == [.gt]
	assert qb.where.data == [25]

	// chain_condition
	qb.reset()
	qb.where('age > ?', 25).where('salary < ?', 1000)
	assert qb.where.fields == ['age', 'salary']
	assert qb.where.kinds == [.gt, .lt]
	assert qb.where.data == [25, 1000]

	// and_or_combination
	qb.reset()
	qb.where('name = ? AND status = ? OR role = ? || id = ? && title = ?', 'Alice', 1,
		'admin', 1, 'st')
	assert qb.where.fields == ['name', 'status', 'role', 'id', 'title']
	assert qb.where.kinds == [.eq, .eq, .eq, .eq, .eq]
	assert qb.where.is_and == [true, false, false, true]

	// nested_parentheses
	qb.reset()
	qb.where('(salary >= ? AND (age <= ? OR title LIKE ?))', 50000, 35, '%Manager%')
	assert qb.where.parentheses == [[1, 2], [0, 2]]

	// complex_nesting
	qb.reset()
	qb.where('((age = ? OR (salary > ? AND id < ?)) AND (name LIKE ?))', 1, 2, 3, '%test%')
	assert qb.where.parentheses == [[1, 2], [0, 2], [3, 3], [0, 3]]
}

fn test_orm_func_stmts() {
	users := [
		User{
			name:       'Tom'
			age:        30
			role:       'admin'
			status:     1
			salary:     5000
			title:      'manager'
			score:      90
			created_at: time.now()
		},
		User{
			name:       'Alice'
			age:        20
			role:       'employee'
			status:     2
			salary:     2000
			title:      'doctor'
			score:      95
			created_at: time.now()
		},
		User{
			name:       'Mars'
			age:        40
			role:       'employer'
			status:     3
			salary:     1000
			title:      'doctor'
			score:      85
			created_at: time.now()
		},
		User{
			name:       'Kitty'
			age:        18
			role:       'employer'
			status:     1
			salary:     1500
			title:      'doctor'
			score:      87
			created_at: time.now()
		},
		User{
			name:   'Silly'
			age:    27
			role:   'employer'
			status: 5
			salary: 2500
			title:  'doctor'
			score:  81
		},
		User{
			name:       'Smith'
			age:        37
			role:       'employer'
			status:     1
			salary:     4500
			title:      'doctor'
			score:      89
			created_at: time.now()
		},
		User{
			name:       'Bob'
			age:        26
			role:       'employer'
			status:     2
			salary:     6500
			title:      'doctor'
			score:      81
			created_at: time.now()
		},
		User{
			name:       'Peter'
			age:        29
			role:       'employer'
			status:     1
			salary:     3500
			title:      'doctor'
			score:      80
			created_at: time.now()
		},
		User{
			name:   'See'
			age:    45
			role:   'employer'
			status: 2
			salary: 8500
			title:  'doctor'
			score:  82
		},
		User{
			name:   'John'
			age:    42
			role:   'employer'
			status: 1
			salary: 10000
			title:  'doctor'
			score:  88
		},
	]
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	mut qb := orm.new_query[User](db)

	// create table
	qb.create()

	// insert many records
	qb.insert_many(users)

	// select count(*)
	mut count := qb.count()

	// last_id
	mut last_id := qb.last_id()
	assert count == last_id
	assert count == users.len

	// insert a single record
	qb.insert(users[0])

	// select * from table
	all_users := qb.query()
	assert all_users.len == users.len + 1

	// select `name` from table
	only_names := qb.select('name').query()
	assert only_names[0].name != ''
	assert only_names[0].id == 0
	assert only_names[0].age == 0
	assert only_names[0].role == ''
	assert only_names[0].status == 0
	assert only_names[0].salary == 0
	assert only_names[0].title == ''
	assert only_names[0].score == 0
	assert only_names[0].created_at == none

	// update
	qb.set('age = ?, title = ?', 71, 'boss').where('name = ?', 'John').update()
	john := qb.where('name = ?', 'John').query()
	assert john[0].name == 'John'
	assert john[0].age == 71
	assert john[0].title == 'boss'

	// delete
	qb.where('name = ?', 'John').delete()
	no_john := qb.where('name = ?', 'John').query()
	assert no_john.len == 0

	// complex select
	selected_users := qb.where('created_at IS NULL && ((salary > ? && age < ?) || (role LIKE ?))',
		2000, 30, '%employee%').query()
	assert selected_users[0].name == 'Silly'
	assert selected_users.len == 1

	// chain calls
	final_users := qb
		.drop()
		.create()
		.insert_many(users)
		.set('name = ?', 'haha').where('name = ?', 'Tom').update()
		.where('age >= ?', 30).delete()
		.query()
	assert final_users.len == 5
}
