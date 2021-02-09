module main

import pg

const dash = '----------------------------------------------------------------'

struct Customer {
	id int
	name string
	nr_orders int
	country string
}

fn main() {
	db := pg.connect(pg.Config{
		host: 'localhost' //'127.0.0.1'
		user: 'postgres'
		dbname: 'customerdb'
	}) or {
		println('failed to connect')
		println(err)
		return
	}

	nr_customers := db.select count from Customer
	println('Total customers: $nr_customers')

	// V syntax can be used to build queries
	println(dash)
	bg_country := 'Bulgaria'
	bg_customers := db.select from Customer where country == bg_country  && id != 2
	for customer in bg_customers {
		println('$customer.country | $customer.id - $customer.name')
	}

	println(dash)
	ru_customers := db.select from Customer where country == 'Russia'
	for customer in ru_customers {
		println('$customer.country | $customer.id - $customer.name')
	}

	// by adding `limit 1` we tell V that there will be only one object
	println(dash)
	existing := db.select from Customer where id == 1 limit 1 or { panic(err) }
	println('Existing customer name: $existing.name')
	println('Existing customer full information:')
	println(existing)

	println(dash)
	q := Customer{}
	// It's easy to handle queries that don't return any data
	if anon := db.select from Customer where id == 12345 && name == q.name &&
			nr_orders > q.nr_orders limit 1 {
		println('Non existing customer name: $anon.name')
	}
	// Insert a new customer
	nc := Customer{
		name: 'John Doe'
		nr_orders: 10
	}
	db.insert(nc)
}


