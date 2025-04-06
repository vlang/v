module main

import time
import db.sqlite

pub enum MessageStatus as u8 {
	ready
	locked
	done
	failed
}

// Struct Message
pub struct Message[T] {
	data       string
	message_id int @[primary; sql: serial]
	status     MessageStatus
	in_time    time.Time @[default: 'CURRENT_TIME']
	lock_time  ?time.Time
	done_time  ?time.Time
	payload    T
}

// Struct Queue
pub struct Queue[T] {
pub mut:
	conn    &sqlite.DB
	handler ?fn (message Message[T]) !
}

fn C.sqlite3_config(int)

// Function NewQueue (Constructor)
pub struct Queue_config {
pub:
	path             string
	db               ?sqlite.DB @[omitempty]
	pooling_interval i64 = time.second
}

pub fn new[T](config Queue_config) !Queue[T] {
	C.sqlite3_config(3)

	// use exist db connection or create file self
	mut conn := if db := config.db {
		db
	} else {
		sqlite.connect(config.path) or { return error('Failed to connect to database: ${err}') }
	}
	conn.busy_timeout(10 * 60_000)

	mut queue := Queue[T]{
		conn: &conn
	}

	sql queue.conn {
		create table Message[T]
	}!

	return queue
}

// Method put
pub fn (mut self Queue[T]) add(payload T) !Message[T] {
	message := Message[T]{
		payload: payload
		status:  MessageStatus.ready
		in_time: time.now()
	}
	dump(message)

	sql self.conn {
		insert message into Message[T]
	} or { return err }
	return message
}

// Method pop_transaction
pub fn (mut self Queue[T]) take() ?[]Message[T] {
	messages := sql self.conn {
		select from Message[T] where status == MessageStatus.ready order by message_id limit 1
	} or { return none }

	if messages.len == 0 {
		return none
	}

	return messages
}

// usage
struct Payload {
	some_filed_1 string
	some_filed_2 int
}

fn test_handler(message Message[Payload]) ! {
	return error('AAAA')
}

fn test_main() {
	db_path := ':memory:'
	_ := new[Payload](
		path: db_path
	) or { panic('Failed to create queue: ${err}') }
}
