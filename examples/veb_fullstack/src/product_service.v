module main

import databases

fn (mut app App) service_add_product(product_name string, user_id int) ! {
	mut db := databases.create_db_connection()!

	defer {
		db.close() or { panic(err) }
	}

	product_model := Product{
		name:    product_name
		user_id: user_id
	}

	mut insert_error := ''

	sql db {
		insert product_model into Product
	} or { insert_error = err.msg() }

	if insert_error != '' {
		return error(insert_error)
	}
}

fn (mut app App) service_get_all_products_from(user_id int) ![]Product {
	mut db := databases.create_db_connection() or {
		println(err)
		return err
	}

	defer {
		db.close() or { panic(err) }
	}

	results := sql db {
		select from Product where user_id == user_id
	}!

	return results
}
