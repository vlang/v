module main

import json
import net.http

pub fn get_products(token string) ![]Product {
	mut header := http.new_header()
	header.add_custom('token', token)!
	url := 'http://localhost:8082/controller/products'

	mut config := http.FetchConfig{
		header: header
	}

	resp := http.fetch(http.FetchConfig{ ...config, url: url })!
	products := json.decode([]Product, resp.body)!

	return products
}

pub fn get_product(token string) ![]User {
	mut header := http.new_header()
	header.add_custom('token', token)!

	url := 'http://localhost:8082/controller/product'

	mut config := http.FetchConfig{
		header: header
	}

	resp := http.fetch(http.FetchConfig{ ...config, url: url })!
	products := json.decode([]User, resp.body)!

	return products
}
