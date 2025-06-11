module main

import vanilla.http_server
import vanilla.request_parser
import db.pg

fn handle_request(req_buffer []u8, client_conn_fd int, mut pool ConnectionPool) ![]u8 {
	req := request_parser.decode_http_request(req_buffer)!

	method := unsafe { tos(&req.buffer[req.method.start], req.method.len) }
	path := unsafe { tos(&req.buffer[req.path.start], req.path.len) }

	if method == 'GET' {
		if path == '/' {
			return home_controller([])
		} else if path.starts_with('/user/') {
			id := path[6..]
			return get_user_controller([id], mut pool)
		} else if path == '/user' {
			return get_users_controller([], mut pool)
		}
	} else if method == 'POST' {
		if path == '/user' {
			return create_user_controller([], mut pool)
		}
	}

	return http_server.tiny_bad_request_response
}

fn main() {
	mut pool := new_connection_pool(pg.Config{
		host:     'localhost'
		port:     5435
		user:     'username'
		password: 'password'
		dbname:   'example'
	}, 5) or { panic('Failed to create pg pool: ${err}') }

	db := pool.acquire() or { panic(err) }
	db.exec('create table if not exists users (id serial primary key, name text not null)') or {
		panic('Failed to create table users: ${err}')
	}
	pool.release(db)

	// Create and run the server with the handle_request function
	mut vanilla := http_server.Server{
		request_handler: fn [mut pool] (req_buffer []u8, client_conn_fd int) ![]u8 {
			return handle_request(req_buffer, client_conn_fd, mut pool)
		}
		port:            3001
	}

	vanilla.run()

	pool.close()
}
