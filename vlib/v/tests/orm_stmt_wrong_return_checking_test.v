import db.sqlite

struct Target {
pub mut:
	id   int @[primary; sql: serial]
	kind ?string
}

fn add_target(repo Target) !int {
	mut dbs := sqlite.connect(':memory:')!
	defer {
		dbs.close() or { panic(err) }
	}
	sql dbs {
		insert repo into Target
	} or {
		println(err)
		assert true
		return 1
	}
	assert true
	return 2
}

fn test_main() {
	assert add_target(Target{1, 'foo'}) or { 3 } == 1
}
