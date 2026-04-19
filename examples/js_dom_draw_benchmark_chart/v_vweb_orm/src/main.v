module main

import veb
import time
import db.sqlite

pub struct Context {
	veb.Context
}

struct App {
	veb.StaticHandler
}

@[table: 'benchmark']
struct Task {
mut:
	id     u32 @[primary; serial; sql: serial]
	title  string
	status string
}

struct Response {
	insert []int
	select []int
	update []int
}

fn main() {
	mut app := &App{}
	veb.run_at[App, Context](mut app, port: 4000)
}

@['/hello-world']
pub fn (mut app App) hello_world(mut ctx Context) veb.Result {
	return ctx.text('hello world')
}

@['/sqlite-memory/:count']
pub fn (mut app App) sqlite_memory(mut ctx Context, count int) veb.Result {
	mut insert_stopwatchs := []int{}
	mut select_stopwatchs := []int{}
	mut update_stopwatchs := []int{}

	mut sw := time.new_stopwatch()

	mut db := sqlite.connect(':memory:') or { panic(err) }

	sql db {
		create table Task
	} or { panic(err) }

	task_model := Task{
		title:  'a'
		status: 'done'
	}

	// inserts
	for i := 0; i < count; i++ {
		sw.start()
		sql db {
			insert task_model into Task
		} or { panic(err) }
		sw.stop()
		insert_stopwatchs << int(sw.end - sw.start)
	}

	// selects
	for i := 0; i < count; i++ {
		sw.start()
		result := sql db {
			select from Task
		} or { []Task{} }
		sw.stop()
		eprintln(result)
		select_stopwatchs << int(sw.end - sw.start)
	}

	// updates
	for i := 0; i < count; i++ {
		sw.start()
		sql db {
			update Task set title = 'b', status = 'finish' where id == i
		} or { panic(err) }
		sw.stop()
		update_stopwatchs << int(sw.end - sw.start)
	}

	sql db {
		drop table Task
	} or { panic(err) }

	response := Response{
		insert: insert_stopwatchs
		select: select_stopwatchs
		update: update_stopwatchs
	}
	return ctx.json(response)
}

@['/sqlite-file/:count']
pub fn (mut app App) sqlite_file(mut ctx Context, count int) veb.Result {
	response := Response{
		insert: []
		select: []
		update: []
	}
	return ctx.json(response)
}

@['/postgres/:count']
pub fn (mut app App) postgres(mut ctx Context, count int) veb.Result {
	response := Response{
		insert: []
		select: []
		update: []
	}
	return ctx.json(response)
}

@['/mysql/:count']
pub fn (mut app App) mysql(mut ctx Context, count int) veb.Result {
	response := Response{
		insert: []
		select: []
		update: []
	}
	return ctx.json(response)
}
