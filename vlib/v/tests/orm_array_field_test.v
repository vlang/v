import time
import db.sqlite

@[table: 'task_metadata']
struct TaskMetadata {
	id         string @[primary]
	task_id    string
	key        string
	value      string
	created_at time.Time @[default: 'CURRENT_TIME']
	updated_at time.Time @[default: 'CURRENT_TIME']
}

@[table: 'tasks']
struct Task {
	id       string @[primary]
	name     string
	metadata []TaskMetadata @[fkey: 'task_id']
}

struct MyService {
mut:
	db sqlite.DB
}

pub fn (s MyService) create(record Task) int {
	result := sql s.db {
		insert record into Task
	} or { return -1 }
	return result
}

fn test_main() {
	assert true
}
