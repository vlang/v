module main

import vweb
import time
import sqlite

struct App {
	vweb.Context
}

[table: 'benchmark']
struct Task {
mut:
	id     u32    [primary; serial; sql: serial]
	title  string
	status string
}

struct Response {
	insert  []int
	@select []int
	update  []int
}

fn main() {
	vweb.run_at(new_app(), vweb.RunParams{
		port: 4000
	}) or { panic(err) }
}

fn new_app() &App {
	mut app := &App{}
	return app
}

['/hello-world']
pub fn (mut app App) hello_world() vweb.Result {
	return app.text('hello world')
}

['/sqlite-memory/:count']
pub fn (mut app App) sqlite_memory(count int) vweb.Result {
	mut insert_stopwatchs := []int{}
	mut select_stopwatchs := []int{}
	mut update_stopwatchs := []int{}

	mut sw := time.new_stopwatch()

	mut db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table Task
	}

	task_model := Task{
		title: 'a'
		status: 'done'
	}

	// inserts
	for i := 0; i < count; i++ {
		sw.start()
		sql db {
			insert task_model into Task
		}
		sw.stop()
		insert_stopwatchs << int(sw.end - sw.start)
	}

	// selects
	for i := 0; i < count; i++ {
		sw.start()
		result := sql db {
			select from Task
		}
		sw.stop()
		eprintln(result)
		select_stopwatchs << int(sw.end - sw.start)
	}

	// updates
	for i := 0; i < count; i++ {
		sw.start()
		sql db {
			update Task set title = 'b', status = 'finish' where id == i
		}
		sw.stop()
		update_stopwatchs << int(sw.end - sw.start)
	}

	sql db {
		drop table Task
	}

	response := Response{
		insert: insert_stopwatchs
		@select: select_stopwatchs
		update: update_stopwatchs
	}
	return app.json(response)
}

['/sqlite-file/:count']
pub fn (mut app App) sqlite_file(count int) vweb.Result {
	response := Response{
		insert: []
		@select: []
		update: []
	}
	return app.json(response)
}

['/postgres/:count']
pub fn (mut app App) postgres(count int) vweb.Result {
	response := Response{
		insert: []
		@select: []
		update: []
	}
	return app.json(response)
}

['/mysql/:count']
pub fn (mut app App) mysql(count int) vweb.Result {
	response := Response{
		insert: []
		@select: []
		update: []
	}
	return app.json(response)
}
