// Regression test for https://github.com/vlang/v/issues/27485
// Assigning an `if`/`match` expression whose branches are `spawn` calls used to
// emit a C ternary with statement-level spawn setup inside it, which failed to
// compile (and, once that was fixed, produced an unjoinable/detached thread).

fn ret(x int) int {
	return x
}

fn test_spawn_in_if_expr() {
	c := true
	t := if c { spawn ret(11) } else { spawn ret(22) }
	assert t.wait() == 11
}

fn test_spawn_in_if_expr_false_branch() {
	c := false
	t := if c { spawn ret(11) } else { spawn ret(22) }
	assert t.wait() == 22
}

fn test_spawn_in_match_expr() {
	x := 2
	t := match x {
		1 { spawn ret(1) }
		2 { spawn ret(2) }
		else { spawn ret(99) }
	}

	assert t.wait() == 2
}

fn test_spawn_in_if_expr_collected_into_array() {
	mut threads := []thread int{}
	for i in 0 .. 4 {
		threads << if i % 2 == 0 { spawn ret(i) } else { spawn ret(i * 10) }
	}
	assert threads.wait() == [0, 10, 2, 30]
}
