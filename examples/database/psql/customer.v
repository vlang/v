// vtest build: linux
module main

import db.pg

struct Customer {
	id        int
	name      string
	nr_orders int
	country   string
}

const dash = '----------------------------------------------------------------'

fn main() {
	db := pg.connect(
		host:   'localhost' // or '127.0.0.1'
		user:   'postgres'
		dbname: 'customerdb'
	) or {
		eprintln('failed to connect, error: ${err}')
		return
	}

	nr_customers := sql db {
		select count from Customer
	}!
	println('Total customers: ${nr_customers}')

	println(dash)
	bg_country := 'Bulgaria'
	// V syntax can be used to build queries
	bg_customers := sql db {
		select from Customer where country == bg_country && id != 2
	}!
	for customer in bg_customers {
		println('${customer.country} | ${customer.id} - ${customer.name}')
	}

	println(dash)
	ru_customers := sql db {
		select from Customer where country == 'Russia'
	}!
	for customer in ru_customers {
		println('${customer.country} | ${customer.id} - ${customer.name}')
	}

	// by adding `limit 1` we tell V that there will be only one object
	println(dash)
	existing := sql db {
		select from Customer where id == 1 limit 1
	}!
	println('Existing customer name: ${existing}[0].name')
	println('Existing customer full information:')
	println(existing)

	println(dash)
	q := Customer{}
	// It's easy to handle queries that don't return any data
	_ := sql db {
		select from Customer where id == 12345 && name == q.name && nr_orders > q.nr_orders limit 1
	}!
	// Insert a new customer
	nc := Customer{
		name:      'John Doe'
		nr_orders: 10
	}
	sql db {
		insert nc into Customer
	}!
}
