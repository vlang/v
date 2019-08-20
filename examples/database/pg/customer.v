module main

import pg

struct Customer {
	id int
	name string
	nr_orders int
	country string
}

fn main() {
	db := pg.connect(pg.Config{host: '127.0.0.1' user: 'myuser' dbname: 'mydb'})
	
	nr_customers := db.select count from Customer
	println('Total customers: $nr_customers')
	
	// V syntax can be used to build queries
	println('------------------------------------------------------------------------')
	bg_customers := db.select from Customer where country = 'Bulgaria' && id != 2
	for customer in bg_customers {
		println('$customer.country | $customer.id - $customer.name')
	}
	
	println('------------------------------------------------------------------------')
	ru_customers := db.select from Customer where country = 'Russia'
	for customer in ru_customers {
		println('$customer.country | $customer.id - $customer.name')
	}
	
	// by adding `limit 1` we tell V that there will be only one object
	println('------------------------------------------------------------------------')	
	existing := db.select from Customer where id = 1 limit 1 or { panic(err) }	
	println('Existing customer name: $existing.name')
	println('Existing customer full information:')
	println(existing)
	
	println('------------------------------------------------------------------------')
  q := Customer{}
	for {
		anon := db.select from Customer where id = 12345 && name = q.name && nr_orders > q.nr_orders limit 1 or { eprintln('No such customer. Error: $err') break }
		println('Non existing customer name: $anon.name')
		break
	}
	
	// TODO: insert a new customer
	/*
	nc := Customer{
		name: 'John Doe'
		nr_orders: 10
	}
	db.insert(nc)
  */
  
  
}
