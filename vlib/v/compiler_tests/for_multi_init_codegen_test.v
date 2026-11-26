import os
import v.tempname

fn for_multi_init_fixture_dir() string {
	root := os.join_path(os.vtmp_dir(), 'v3_for_multi_init_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn for_multi_init_run_bad(name string, source string) string {
	root := for_multi_init_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_for_multi_init_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(root, 'v3_for_multi_init_${name}')
	result := os.exec([@VEXE, src, '-b', 'c', '-nocolor', '-o', bin])
	assert result.exit_code != 0, result.output
	assert !result.output.contains('C compilation failed'), result.output
	return result.output
}

fn for_multi_init_gen_c(name string, source string) string {
	root := for_multi_init_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_for_multi_init_${name}.v')
	os.write_file(src, source) or { panic(err) }
	c_path := os.join_path(root, 'v3_for_multi_init_${name}.c')
	result := os.exec([@VEXE, '${src}', '-b', 'c', '-o', c_path])
	assert result.exit_code == 0, result.output
	assert os.exists(c_path), result.output
	return os.read_file(c_path) or { panic(err) }
}

// The empty-post loop must leave following_iterator as a top-level declaration.
fn test_c_style_for_runtime_fixtures() {
	root := for_multi_init_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_for_loop_runtime.v')
	os.write_file(src, 'struct Iter {
	label string
}

fn scan_empty_post() int {
	mut total := 0
	for h, t := 0, 3; h <= t; {
		total += h + t
		h += 1
		t -= 1
	}
	return total
}

fn following_iterator() Iter {
	return Iter{
		label: "ok"
	}
}

fn scan_with_post() int {
	mut total := 0
	for h, t := 0, 3; h <= t; h += 1 {
		total += h + t
		t -= 1
	}
	return total
}

fn indexed_for_in_score() int {
	xs := [2, 4, 6]
	mut total := 0
	for i, v in xs {
		total += i * v
	}
	return total
}

fn pair() (int, int) {
	return 1, 2
}

fn check_init_call() {
	mut total := 0
	for a, b := pair(); a == 1; {
		total = a + b
		break
	}
	assert total == 3
}

fn step(a int, b int) (int, int) {
	return a + 1, b + 2
}

fn check_post_call() {
	mut a := 0
	mut b := 0
	mut n := 0
	for ; n < 3; a, b = step(a, b) {
		n++
		if n < 3 {
			continue
		}
	}
	assert a == 3
	assert b == 6
}

struct Pair {
mut:
	x int
	y int
}

fn check_init_selectors() {
	mut a := Pair{}
	mut total := 0
	for a.x, a.y = 0, 1; a.x < 3; {
		total += a.x + a.y
		a.x += 1
		a.y += 2
	}
	assert total == 12
	assert a.x == 3
	assert a.y == 7
}

fn check_multi_init_and_post_assignments() {
	assert scan_empty_post() == 6
	assert following_iterator().label == "ok"
	assert scan_with_post() == 6
	assert indexed_for_in_score() == 16
	check_init_call()
	check_post_call()
	check_init_selectors()
	println("ok")
}

struct Calls {
mut:
	value int
}

fn next_bound(mut calls Calls) u8 {
	calls.value++
	return u8(2)
}

fn check_bound_evaluation() {
	mut calls := Calls{}
	mut sum := 0
	for i := u8(0); i <= next_bound(mut calls); i++ {
		sum += int(i)
	}
	assert sum == 3
	assert calls.value == 4
}

fn check_post_after_mutation() {
	mut hi := u8(1)
	mut i := u8(0)
	mut hits := 0
	for ; i <= hi; i++ {
		hits++
		hi = 0
	}
	assert hits == 1
	assert i == 1

	hi = 0
	i = 0
	hits = 0
	for ; i <= hi; i++ {
		hits++
		hi = 0
	}
	assert hits == 1
	assert i == 1

	i = 0
	for ; i <= u8(5); i++ {
		i = 5
	}
	assert i == 6

	mut j := u8(5)
	for ; j >= u8(1); j-- {
		j = 1
	}
	assert j == 0
}

fn check_unsigned_inclusive_for() {
	check_bound_evaluation()
	check_post_after_mutation()
	println("ok")
}

fn check_labeled_init_flow() {
	mut out := ""
	outer: for x, stop := 0, 3; x < stop; x++ {
		for y := 0; y < 3; y++ {
			if x == 1 && y == 0 {
				continue outer
			}
			if x == 2 && y == 1 {
				break outer
			}
			out += "\${x}:\${y};"
		}
	}
	assert out == "0:0;0:1;0:2;2:0;"

	mut gx := 0
	mut hits := 0
	mut guarded := ""
	guarded_outer: for gx, hits = hits, hits + 1; gx < 3; gx++ {
		for gy := 0; gy < 3; gy++ {
			if gx == 1 && gy == 0 {
				continue guarded_outer
			}
			if gx == 2 && gy == 1 {
				break guarded_outer
			}
			guarded += "\${gx}:\${gy};"
		}
	}
	assert guarded == "0:0;0:1;0:2;2:0;"
	assert hits == 1
	println(out)
}

struct Posts {
mut:
	value int
}

fn counted_step(mut posts Posts, a int, b int) (int, int) {
	posts.value++
	return a + 1, b + 1
}

fn continue_same_loop() {
	mut posts := Posts{}
	mut a := 0
	mut b := 0
	mut hits := 0
	loop: for ; a < 3; a, b = counted_step(mut posts, a, b) {
		hits++
		if a < 2 {
			continue loop
		}
	}
	assert a == 3
	assert b == 3
	assert hits == 3
	assert posts.value == 3
}

fn continue_outer_loop() {
	mut inner_posts := Posts{}
	mut outer := 0
	mut hits := 0
	outer_loop: for ; outer < 2; outer++ {
		mut a := 0
		mut b := 0
		for ; a < 2; a, b = counted_step(mut inner_posts, a, b) {
			hits++
			continue outer_loop
		}
	}
	assert outer == 2
	assert hits == 2
	assert inner_posts.value == 0
}

fn control_flow_step(a int, b int) (int, int) {
	return a + 1, b + 1
}

fn continue_in_match() {
	mut a := 0
	mut b := 0
	mut guard := 0
	for ; a < 3; a, b = control_flow_step(a, b) {
		guard++
		if guard > 5 {
			break
		}
		match a {
			0 {
				continue
			}
			else {}
		}
	}
	assert a == 3
	assert b == 3
	assert guard == 3
}

fn continue_in_select() {
	mut a := 0
	mut b := 0
	mut guard := 0
	for ; a < 3; a, b = control_flow_step(a, b) {
		guard++
		if guard > 5 {
			break
		}
		select {
			else {
				continue
			}
		}
	}
	assert a == 3
	assert b == 3
	assert guard == 3
}

fn check_loop_control_flow_and_post_updates() {
	check_labeled_init_flow()
	continue_same_loop()
	continue_outer_loop()
	println("ok")
	continue_in_match()
	continue_in_select()
	println("ok")
}

fn main() {
	check_multi_init_and_post_assignments()
	check_unsigned_inclusive_for()
	check_loop_control_flow_and_post_updates()
}
') or {
		panic(err)
	}

	bin := os.join_path(root, 'v3_for_loop_runtime')
	compile := os.exec([@VEXE, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == [
		'ok',
		'ok',
		'0:0;0:1;0:2;2:0;',
		'ok',
		'ok',
	], run.output
}

fn test_c_style_for_rejects_invalid_assignments_and_smartcasts() {
	source := 'fn bump(n int) int {
	println(n.str())
	return n
}

fn extra_init_rhs() {
	for a, b := bump(1), bump(2), bump(3); false; {
		println(a)
		println(b)
	}
}

fn extra_post_rhs() {
	mut i := 0
	for ; i < 1; i = bump(1), bump(2) {
		i++
	}
}

fn scalar_post_rhs() {
	mut a := 0
	mut b := 0
	for ; a < 1; a, b = 1 {
		break
	}
	println(b)
}

struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

fn stale_post_smartcast() {
	mut x := Value(Foo{
		foo: 1
	})
	for ; x is Foo; x.foo {
		x = Bar{}
	}
}

fn missing_init_rhs() {
	for a, b := 1; false; {
		println(a)
		println(b)
	}
}

fn main() {
	extra_init_rhs()
	extra_post_rhs()
	scalar_post_rhs()
	stale_post_smartcast()
	missing_init_rhs()
}
'
	output := for_multi_init_run_bad('header_rejections', source)
	expected := {
		'for a, b := bump(1), bump(2), bump(3); false; {': 'multi-return assignment mismatch: 3 variables but `int` has 1 values'
		'for ; i < 1; i = bump(1), bump(2) {':             'for post assignment mismatch: 1 variables but 2 values'
		'for ; a < 1; a, b = 1 {':                         'multi-return assignment mismatch: 2 variables but `int` has 1 values'
		'for ; x is Foo; x.foo {':                         'field `foo` does not exist or have the same type in these sumtype `Value` variants'
		'for a, b := 1; false; {':                         'multi-return assignment mismatch: 2 variables but `int` has 1 values'
	}
	lines := source.split_into_lines()
	diagnostics := output.split_into_lines().filter(it.contains(': error: '))
	for header, message in expected {
		line := lines.index('\t${header}') + 1
		assert line > 0, header
		location := 'v3_for_multi_init_header_rejections.v:${line}:'
		assert diagnostics.any(it.contains(location) && it.contains(message)), 'expected ${message} at ${location}\n${output}'
	}
}

fn test_c_style_for_post_continue_in_lock_runs_post_after_unlock() {
	c_source := for_multi_init_gen_c('post_continue_lock_cleanup', 'struct Box {
mut:
	x shared int
}

fn step(mut b Box, i int, j int) (int, int) {
	lock b.x {}
	return i + 1, j + 1
}

fn main() {
	mut b := Box{}
	mut i := 0
	mut j := 0
	for ; i < 1; i, j = step(mut b, i, j) {
		lock b.x {
			if i == 0 {
				continue
			}
		}
	}
}
')
	continue_idx := c_source.index('if (i == 0)') or {
		assert false, c_source
		return
	}
	continue_tail := c_source[continue_idx..]
	goto_idx := continue_tail.index('goto __for_post_') or {
		assert false, continue_tail
		return
	}
	continue_branch := continue_tail[..goto_idx]
	assert continue_branch.contains('sync__RwMutex__unlock'), continue_branch
	assert !continue_branch.contains('step('), continue_branch
	post_tail := continue_tail[goto_idx..]
	label_idx := post_tail.index('_continue:') or {
		assert false, post_tail
		return
	}
	step_idx := post_tail.index('step(') or {
		assert false, post_tail
		return
	}
	assert label_idx < step_idx, post_tail
}

fn test_for_in_rejects_selector_value_var_after_comma() {
	output := for_multi_init_run_bad('for_in_selector_value', 'struct Box {
mut:
	x int
}

fn main() {
	xs := [1, 2]
	mut b := Box{}
	for i, b.x in xs {
		println(i)
	}
}
')
	assert output.contains('invalid for-in header: expected identifiers before `in`'), output
}
