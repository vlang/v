// vtest vflags: -autofree -gc none
// A local moved on only some paths must not be freed when it is reassigned
// afterwards: on the path that moved it, the old value now belongs elsewhere.

struct Test {
	stmts []string
}

fn word(i int) string {
	return 'w${i}'.clone()
}

fn test_struct_init_inside_if() {
	mut tests := []Test{}
	mut stmts := []string{}
	for i in 0 .. 4 {
		stmts << word(i)
		if i % 2 == 1 {
			tests << Test{stmts}
		}
		stmts = []
	}
	assert tests.len == 2
	assert tests[0].stmts == ['w1']
	assert tests[1].stmts == ['w3']
}

fn test_push_inside_if() {
	mut all := [][]string{}
	mut stmts := []string{}
	for i in 0 .. 3 {
		stmts << word(i)
		if i != 1 {
			all << stmts
		}
		stmts = []string{}
	}
	assert all == [['w0'], ['w2']]
}

fn test_move_in_one_match_branch() {
	mut all := [][]string{}
	mut stmts := [word(1), word(2)]
	n := all.len
	match n {
		0 { all << stmts }
		else {}
	}
	stmts = [word(3)]
	assert all == [['w1', 'w2']]
	assert stmts == ['w3']
}

fn test_move_in_else_branch_only() {
	mut tests := []Test{}
	mut stmts := [word(5)]
	if tests.len > 0 {
		println('not taken')
	} else {
		tests << Test{
			stmts: stmts
		}
	}
	stmts = [word(6)]
	assert tests[0].stmts == ['w5']
	assert stmts == ['w6']
}

fn test_branch_not_taken_keeps_value() {
	mut all := [][]string{}
	mut stmts := [word(7)]
	if all.len > 0 {
		all << stmts
	}
	assert stmts == ['w7']
	stmts = [word(8)]
	assert all.len == 0
	assert stmts == ['w8']
}
