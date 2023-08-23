import db.sqlite

struct Target {
pub mut:
	id   int    [primary; sql: serial]
	kind string [nonull]
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
		return 1
	}

	assert true

	return 1
}

fn test_main() {
	add_target(Target{1, 'foo'}) or {
		println('>> ${err}')
		return
	}
	assert true
}
