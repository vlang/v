// vtest retry: 3
// vtest build: present_sqlite3? && !windows
import db.sqlite

// Department table for testing JOINs - using dept_id to avoid column name conflicts
struct Department {
	dept_id   int @[primary; sql: serial]
	dept_name string
}

// User table with a foreign key reference to Department
struct User {
	user_id       int @[primary; sql: serial]
	user_name     string
	department_id int
}

fn test_inner_join() {
	db := sqlite.connect(':memory:') or { panic(err) }

	// Create tables
	sql db {
		create table Department
		create table User
	}!

	// Insert departments
	engineering := Department{
		dept_name: 'Engineering'
	}
	sales := Department{
		dept_name: 'Sales'
	}
	sql db {
		insert engineering into Department
		insert sales into Department
	}!

	// Insert users
	alice := User{
		user_name:     'Alice'
		department_id: 1
	}
	bob := User{
		user_name:     'Bob'
		department_id: 2
	}
	charlie := User{
		user_name:     'Charlie'
		department_id: 1
	}
	sql db {
		insert alice into User
		insert bob into User
		insert charlie into User
	}!

	// Test basic INNER JOIN
	users := sql db {
		select from User
		join Department on User.department_id == Department.dept_id
	}!

	assert users.len == 3
}

fn test_inner_join_with_where() {
	db := sqlite.connect(':memory:') or { panic(err) }

	// Create tables
	sql db {
		create table Department
		create table User
	}!

	// Insert departments
	engineering := Department{
		dept_name: 'Engineering'
	}
	sales := Department{
		dept_name: 'Sales'
	}
	sql db {
		insert engineering into Department
		insert sales into Department
	}!

	// Insert users
	alice := User{
		user_name:     'Alice'
		department_id: 1
	}
	bob := User{
		user_name:     'Bob'
		department_id: 2
	}
	charlie := User{
		user_name:     'Charlie'
		department_id: 1
	}
	sql db {
		insert alice into User
		insert bob into User
		insert charlie into User
	}!

	// Test INNER JOIN with WHERE clause - use simple field name (not Table.field)
	engineering_users := sql db {
		select from User
		join Department on User.department_id == Department.dept_id where department_id == 1
	}!

	assert engineering_users.len == 2
	assert engineering_users[0].user_name == 'Alice' || engineering_users[0].user_name == 'Charlie'
}

fn test_left_join() {
	db := sqlite.connect(':memory:') or { panic(err) }

	// Create tables
	sql db {
		create table Department
		create table User
	}!

	// Insert departments
	engineering := Department{
		dept_name: 'Engineering'
	}
	sql db {
		insert engineering into Department
	}!

	// Insert users - one with a department, one without (orphan)
	alice := User{
		user_name:     'Alice'
		department_id: 1
	}
	bob := User{
		user_name:     'Bob'
		department_id: 999 // No matching department
	}
	sql db {
		insert alice into User
		insert bob into User
	}!

	// Test LEFT JOIN - should return all users, even those without matching department
	users := sql db {
		select from User
		left join Department on User.department_id == Department.dept_id
	}!

	// Both users should be returned since it's a LEFT JOIN
	assert users.len == 2
}
