module main

import json
import net.http

pub fn get_users(token string) ![]User {
	mut header := http.new_header()
	header.add_custom('token', token)!

	url := 'http://localhost:8082/controller/users'

	mut config := http.FetchConfig{
		header: header
	}

	resp := http.fetch(http.FetchConfig{ ...config, url: url })!
	users := json.decode([]User, resp.body)!

	return users
}

pub fn get_user(token string) !User {
	mut header := http.new_header()
	header.add_custom('token', token)!

	url := 'http://localhost:8082/controller/user'

	mut config := http.FetchConfig{
		header: header
	}

	resp := http.fetch(http.FetchConfig{ ...config, url: url })!
	users := json.decode(User, resp.body)!

	return users
}
