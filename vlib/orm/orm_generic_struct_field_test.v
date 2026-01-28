// vtest retry: 3
// Tests for generic structs where the type parameter is used as a field type
import db.sqlite

// Test struct that will be used as a generic type parameter
struct Payload {
	some_field_1 string
	some_field_2 int
}

// Generic struct with a field of type T - this requires proper ORM configuration
// The payload field will be treated as a foreign key reference
pub struct Message[T] {
	id      int @[primary; sql: serial]
	data    string
	payload T @[fkey: 'id']
}

// Payload needs to be a proper ORM table with a primary key
struct PayloadTable {
	id           int @[primary; sql: serial]
	some_field_1 string
	some_field_2 int
}

// Generic struct with proper ORM configuration
pub struct MessageWithPayload[T] {
	id      int @[primary; sql: serial]
	data    string
	payload T @[fkey: 'id']
}

fn test_generic_struct_with_struct_field_and_primary_key() {
	mut db := sqlite.connect(':memory:')!

	// Create tables - PayloadTable first since it's referenced
	sql db {
		create table PayloadTable
	}!

	// Insert a payload
	payload := PayloadTable{
		some_field_1: 'test'
		some_field_2: 42
	}
	sql db {
		insert payload into PayloadTable
	}!

	// Verify the payload was inserted
	payloads := sql db {
		select from PayloadTable
	}!
	assert payloads.len == 1
	assert payloads[0].some_field_1 == 'test'
	assert payloads[0].some_field_2 == 42

	db.close()!
}

// Test that generic structs with simple fields work correctly
pub struct SimpleMessage[T] {
	id     int @[primary; sql: serial]
	data   string
	status int
}

fn test_generic_struct_with_simple_fields() {
	mut db := sqlite.connect(':memory:')!

	sql db {
		create table SimpleMessage[Payload]
	}!

	// The table should be created successfully
	// Note: SimpleMessage[Payload] doesn't actually use Payload as a field type,
	// so this works fine

	db.close()!
}

// Test that skipping struct fields with @[sql: '-'] works
pub struct MessageWithSkippedField[T] {
	id      int @[primary; sql: serial]
	data    string
	payload T @[sql: '-'] // Skip this field in ORM
}

fn test_generic_struct_with_skipped_field() {
	mut db := sqlite.connect(':memory:')!

	// This should work because the payload field is skipped
	sql db {
		create table MessageWithSkippedField[Payload]
	}!

	db.close()!
}

// Test generic struct creation inside a generic function
// This tests the fix for issue #26433
pub struct GenericMessage[T] {
	id      int @[primary; sql: serial]
	data    string
	payload T @[sql: '-'] // Skipped to avoid needing fkey setup
}

pub struct Queue[T] {
pub mut:
	conn &sqlite.DB
}

pub fn create_queue[T](path string) !Queue[T] {
	mut conn := sqlite.connect(path)!

	mut queue := Queue[T]{
		conn: &conn
	}

	// This should work: creating a table with a generic struct inside a generic function
	sql queue.conn {
		create table GenericMessage[T]
	}!

	return queue
}

fn test_create_table_in_generic_function() {
	mut queue := create_queue[Payload](':memory:')!

	// Verify the table was created by inserting and selecting data
	msg := GenericMessage[Payload]{
		data: 'test message'
	}
	sql queue.conn {
		insert msg into GenericMessage[Payload]
	}!

	messages := sql queue.conn {
		select from GenericMessage[Payload]
	}!

	assert messages.len == 1
	assert messages[0].data == 'test message'

	queue.conn.close()!
}

// Test inserting from within a generic function
// This tests the fix for the "cannot use `Message` as `Message[Payload]`" error
pub fn (mut q Queue[T]) add_message(data string) !GenericMessage[T] {
	msg := GenericMessage[T]{
		data: data
	}
	sql q.conn {
		insert msg into GenericMessage[T]
	}!
	return msg
}

fn test_insert_in_generic_function() {
	mut queue := create_queue[Payload](':memory:')!

	// Insert from within a generic function
	msg := queue.add_message('inserted from generic fn')!
	assert msg.data == 'inserted from generic fn'

	// Verify it was inserted
	messages := sql queue.conn {
		select from GenericMessage[Payload]
	}!

	assert messages.len == 1
	assert messages[0].data == 'inserted from generic fn'

	queue.conn.close()!
}
