// vtest build: present_sqlite3? && !sanitize-memory-clang
import db.sqlite

pub enum MessageStatus as u8 {
	ready
}

pub struct Message[T] {
	message_id int @[primary; sql: serial]
	status     MessageStatus
}

pub struct Queue[T] {
	conn &sqlite.DB
}

pub struct Queue_config {
	path string
	db   ?sqlite.DB @[omitempty]
}

pub fn new[T](config Queue_config) !Queue[T] {
	mut conn := if db := config.db {
		db
	} else {
		sqlite.connect(config.path) or { return error('Failed to connect to database: ${err}') }
	}
	mut queue := Queue[T]{
		conn: &conn
	}
	return queue
}

pub fn (mut self Queue[T]) take() !Message[T] {
	messages := sql self.conn {
		select from Message[T] where status == MessageStatus.ready order by message_id limit 1
	} or { return error('') }
	message := messages.first()
	return message
}

struct Payload {}

fn test_main() {
	_ := new[Payload](path: ':memory:')!
}
