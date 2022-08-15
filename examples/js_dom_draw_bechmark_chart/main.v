module main

import vweb
import os
import sqlite
// import pg
// import mysql
import time
import arrays

[table: 'benchmark']
struct Task {
mut:
	id     u32    [primary; serial; sql: serial]
	title  string
	status string
}

const (
	http_port             = 3001
	benchmark_loop_length = 10
)

struct App {
	vweb.Context
}

enum SqliteDbConnection {
	sqlite_memory
	sqlite_file
}

fn main() {
	vweb.run(new_app(), http_port)
}

pub fn (mut app App) before_request() {
	os.execute_or_panic('v -b js_browser draw.js.v ')
}

fn new_app() &App {
	mut app := &App{}
	app.serve_static('/favicon.ico', 'favicon.ico')
	app.serve_static('/draw.js', 'draw.js')
	app.mount_static_folder_at(os.resource_abs_path('.'), '/')
	return app
}

['/'; get]
pub fn (mut app App) controller_get_all_task() vweb.Result {
	// attribute_names := ['sqlite_memory_insert_times', 'sqlite_file_insert_times',
	// 	'postgres_insert_times', 'mysql_insert_times']
	attribute_names := ['sqlite_memory_insert_times', 'sqlite_file_insert_times']
	chart_colors := ['gray', 'black', 'red', 'orange', 'purple']
	mut insert_times := [][]int{}

	mut max_times := []int{}
	mut ten_perc_max_times := []int{}
	mut min_times := []int{}
	mut ten_perc_min_times := []int{}

	mut max_fast := []int{}
	mut ten_perc_max_fast := []int{}
	mut min_fast := []int{}
	mut ten_perc_min_fast := []int{}

	insert_times << factory_sqlite_memory_insert_benchmark(.sqlite_memory)
	insert_times << factory_sqlite_memory_insert_benchmark(.sqlite_file)
	// insert_times << factory_postgres_insert_benchmark()
	// insert_times << factory_mysql_insert_benchmark()

	sqlite_memory_insert_times := insert_times[0].str().replace(' ', '')
	sqlite_file_insert_times := insert_times[1].str().replace(' ', '')
	// postgres_insert_times := insert_times[2].str().replace(' ', '')
	// mysql_insert_times := insert_times[3].str().replace(' ', '')

	for i := 0; i < attribute_names.len; i++ {
		println('insert_times[i]: ${insert_times[i]}')
		ten_perc := int(insert_times[i].len / 10)

		mut min_ten_array := insert_times[i].clone()
		min_ten_array.sort()
		min_ten_array.trim(ten_perc)

		mut max_ten_array := insert_times[i].clone()
		max_ten_array.sort(a > b)
		max_ten_array.trim(ten_perc)

		max_times << arrays.max(insert_times[i]) or { 0 }
		ten_perc_max_times << arrays.sum(max_ten_array) or { 0 } / ten_perc
		min_times << arrays.min(insert_times[i]) or { 0 }
		ten_perc_min_times << arrays.sum(min_ten_array) or { 0 } / ten_perc
	}

	for i := 0; i < attribute_names.len; i++ {
		max_fast << int(100 - (f64(max_times[i] * 100) / f64(arrays.max(max_times) or {
			panic('deu ruim no max_fas')
		})))
		ten_perc_max_fast << int(100 - (f64(ten_perc_max_times[i] * 100) / f64(arrays.max(ten_perc_max_times) or {
			panic('deu ruim no max_fas')
		})))
		min_fast << int(100 - (f64(min_times[i] * 100) / f64(arrays.max(min_times) or {
			panic('deu ruim no max_fas')
		})))
		ten_perc_min_fast << int(100 - (f64(ten_perc_min_times[i] * 100) / f64(arrays.max(ten_perc_min_times) or {
			panic('deu ruim no max_fas')
		})))
	}

	return $vweb.html()
}

fn factory_sqlite_memory_insert_benchmark(db_connection SqliteDbConnection) []int {
	mut result := []int{}
	mut sw := time.new_stopwatch()
	mut db := sqlite.connect(':memory:') or { panic(err) }

	if db_connection == .sqlite_file {
		db.close() or { println('text: $err') }
		db = sqlite.connect('salada.db') or { panic(err) }
	}

	sql db {
		create table Task
	}

	task_model := Task{
		title: 'a'
		status: 'done'
	}

	for i := 0; i < benchmark_loop_length; i++ {
		sw.start()
		sql db {
			insert task_model into Task
		}
		sw.stop()
		result << int(sw.end - sw.start)
	}

	sql db {
		drop table Task
	}
	return result
}

// fn factory_postgres_insert_benchmark() []int {
// 	mut result := []int{}
// 	mut sw := time.new_stopwatch()

// 	mut db := pg.connect(pg.Config{
// 		host: '127.0.0.1'
// 		port: 5432
// 		user: 'hitalo'
// 		password: 'password'
// 		dbname: 'username'
// 	}) or { panic(err) }
// 	sql db {
// 		create table Task
// 	}

// 	task_model := Task{
// 		title: 'a'
// 		status: 'done'
// 	}

// 	for i := 0; i < benchmark_loop_length; i++ {
// 		sw.start()
// 		sql db {
// 			insert task_model into Task
// 		}
// 		sw.stop()
// 		result << int(sw.end - sw.start)
// 	}

// 	sql db {
// 		drop table Task
// 	}
// 	return result
// }

// fn factory_mysql_insert_benchmark() []int {
// 	mut result := []int{}
// 	mut sw := time.new_stopwatch()

// 	mut db := mysql.Connection{
// 		host: '127.0.0.1'
// 		port: 3306
// 		username: 'username'
// 		password: 'password'
// 		dbname: 'benchmark'
// 	}
// 	db.connect() or { println(err) }

// 	sql db {
// 		create table Task
// 	}

// 	task_model := Task{
// 		title: 'a'
// 		status: 'done'
// 	}

// 	for i := 0; i < benchmark_loop_length; i++ {
// 		sw.start()
// 		sql db {
// 			insert task_model into Task
// 		}
// 		sw.stop()
// 		result << int(sw.end - sw.start)
// 	}

// 	sql db {
// 		drop table Task
// 	}
// 	return result
// }
