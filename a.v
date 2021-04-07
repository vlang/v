import sqlite

struct Customer {
	// struct name has to be the same as the table name (for now)
	id        int    [primary; sql: serial] // a field named `id` of integer type must be the first field
	name      string [nonull]
	nr_orders int
	country   string [nonull]
}

db := sqlite.connect('customers.db') ?

// you can create tables
// CREATE TABLE IF NOT EXISTS `Customer` (`id` INTEGER PRIMARY KEY, `name` TEXT NOT NULL, `nr_orders` INTEGER, `country` TEXT NOT NULL)
sql db {
	create table Customer
}

// select count(*) from Customer
nr_customers := sql db {
	select count from Customer
}
println('number of all customers: $nr_customers')
// V syntax can be used to build queries
uk_customers := sql db {
	select from Customer where country == 'uk' && nr_orders > 0
}
println(uk_customers.len)
for customer in uk_customers {
	println('$customer.id - $customer.name')
}
// by adding `limit 1` we tell V that there will be only one object
customer := sql db {
	select from Customer where id == 1 limit 1
}
println('$customer.id - $customer.name')
// insert a new customer
new_customer := Customer{
	name: 'Bob'
	nr_orders: 10
}
sql db {
	insert new_customer into Customer
}
