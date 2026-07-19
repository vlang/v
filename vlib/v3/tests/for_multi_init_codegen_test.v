import os

const for_multi_init_vexe = @VEXE
const for_multi_init_tests_dir = os.dir(@FILE)
const for_multi_init_v3_dir = os.dir(for_multi_init_tests_dir)
const for_multi_init_vlib_dir = os.dir(for_multi_init_v3_dir)
const for_multi_init_v3_src = os.join_path(for_multi_init_v3_dir, 'v3.v')

fn for_multi_init_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_for_multi_init_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${for_multi_init_vexe} -gc none -path "${for_multi_init_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${for_multi_init_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn for_multi_init_run_bad(v3_bin string, name string, source string, expected string) {
	src := os.join_path(os.temp_dir(), 'v3_for_multi_init_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_for_multi_init_${name}_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	result := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert result.exit_code != 0, result.output
	assert result.output.contains(expected), result.output
	assert !result.output.contains('C compilation failed'), result.output
}

fn for_multi_init_gen_c(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_for_multi_init_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	c_path := os.join_path(os.temp_dir(), 'v3_for_multi_init_${name}_${os.getpid()}.c')
	os.rm(c_path) or {}
	result := os.execute('${v3_bin} ${src} -b c -o ${c_path}')
	assert result.exit_code == 0, result.output
	assert os.exists(c_path), result.output
	return os.read_file(c_path) or { panic(err) }
}

fn test_c_style_for_multi_init_does_not_swallow_following_fn() {
	v3_bin := for_multi_init_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_for_multi_init_input_${os.getpid()}.v')
	os.write_file(src, "struct Iter {
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
		label: 'ok'
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

fn main() {
	assert scan_empty_post() == 6
	assert following_iterator().label == 'ok'
	assert scan_with_post() == 6
	assert indexed_for_in_score() == 16
	println('ok')
}
") or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_for_multi_init_input_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_unsigned_inclusive_for_bound_evaluates_once_per_iteration() {
	v3_bin := for_multi_init_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_for_unsigned_inclusive_bound_${os.getpid()}.v')
	os.write_file(src, 'fn next_bound(mut calls int) u8 {
	calls++
	return u8(2)
}

fn main() {
	mut calls := 0
	mut sum := 0
	for i := u8(0); i <= next_bound(mut calls); i++ {
		sum += int(i)
	}
	assert sum == 3
	assert calls == 4
	println("ok")
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_for_unsigned_inclusive_bound_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_unsigned_inclusive_for_mutable_bound_keeps_post_semantics() {
	v3_bin := for_multi_init_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_for_unsigned_mutable_bound_${os.getpid()}.v')
	os.write_file(src, 'fn main() {
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
	println("ok")
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_for_unsigned_mutable_bound_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_labeled_c_style_for_multi_init_flow_targets_named_loop() {
	v3_bin := for_multi_init_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_for_multi_init_labeled_${os.getpid()}.v')
	os.write_file(src, 'fn main() {
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
	') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_for_multi_init_labeled_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '0:0;0:1;0:2;2:0;'
}

fn test_c_style_for_post_labeled_continue_runs_post_once_for_same_loop() {
	v3_bin := for_multi_init_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_for_post_labeled_continue_${os.getpid()}.v')
	os.write_file(src, 'fn step(mut posts int, a int, b int) (int, int) {
	posts++
	return a + 1, b + 1
}

fn main() {
	mut posts := 0
	mut a := 0
	mut b := 0
	mut hits := 0
	loop: for ; a < 3; a, b = step(mut posts, a, b) {
		hits++
		if a < 2 {
			continue loop
		}
	}
	assert a == 3
	assert b == 3
	assert hits == 3
	assert posts == 3
	println("ok")
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_for_post_labeled_continue_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_c_style_for_post_labeled_continue_to_outer_skips_inner_post() {
	v3_bin := for_multi_init_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_for_post_outer_labeled_continue_${os.getpid()}.v')
	os.write_file(src, 'fn step(mut posts int, a int, b int) (int, int) {
	posts++
	return a + 1, b + 1
}

fn main() {
	mut inner_posts := 0
	mut outer := 0
	mut hits := 0
	outer_loop: for ; outer < 2; outer++ {
		mut a := 0
		mut b := 0
		for ; a < 2; a, b = step(mut inner_posts, a, b) {
			hits++
			continue outer_loop
		}
	}
	assert outer == 2
	assert hits == 2
	assert inner_posts == 0
	println("ok")
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_for_post_outer_labeled_continue_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_c_style_for_multi_init_rejects_extra_rhs() {
	v3_bin := for_multi_init_build_v3()
	for_multi_init_run_bad(v3_bin, 'extra_rhs', 'fn bump(n int) int {
	println(n.str())
	return n
}

fn main() {
	for a, b := bump(1), bump(2), bump(3); false; {
		println(a)
		println(b)
	}
}
',
		'for init assignment mismatch: 2 variables but 3 values')
}

fn test_c_style_for_multi_init_allows_multi_return_call() {
	v3_bin := for_multi_init_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_for_multi_init_pair_${os.getpid()}.v')
	os.write_file(src, 'fn pair() (int, int) {
	return 1, 2
}

fn main() {
	mut total := 0
	for a, b := pair(); a == 1; {
		total = a + b
		break
	}
	assert total == 3
	println("ok")
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_for_multi_init_pair_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_c_style_for_post_allows_multi_return_call() {
	v3_bin := for_multi_init_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_for_post_pair_${os.getpid()}.v')
	os.write_file(src, 'fn step(a int, b int) (int, int) {
	return a + 1, b + 2
}

fn main() {
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
	println("ok")
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_for_post_pair_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_c_style_for_post_runs_before_match_branch_continue() {
	v3_bin := for_multi_init_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_for_post_match_continue_${os.getpid()}.v')
	os.write_file(src, 'fn step(a int, b int) (int, int) {
	return a + 1, b + 1
}

fn main() {
	mut a := 0
	mut b := 0
	mut guard := 0
	for ; a < 3; a, b = step(a, b) {
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
	println("ok")
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_for_post_match_continue_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_c_style_for_post_runs_before_select_branch_continue() {
	v3_bin := for_multi_init_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_for_post_select_continue_${os.getpid()}.v')
	os.write_file(src, 'fn step(a int, b int) (int, int) {
	return a + 1, b + 1
}

fn main() {
	mut a := 0
	mut b := 0
	mut guard := 0
	for ; a < 3; a, b = step(a, b) {
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
	println("ok")
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_for_post_select_continue_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_c_style_for_post_continue_in_lock_runs_post_after_unlock() {
	v3_bin := for_multi_init_build_v3()
	c_source := for_multi_init_gen_c(v3_bin, 'post_continue_lock_cleanup', 'struct Box {
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

fn test_c_style_for_post_rejects_extra_rhs() {
	v3_bin := for_multi_init_build_v3()
	for_multi_init_run_bad(v3_bin, 'post_extra_rhs', 'fn bump(n int) int {
	println(n.str())
	return n
}

fn main() {
	mut i := 0
	for ; i < 1; i = bump(1), bump(2) {
		i++
	}
}
',
		'for post assignment mismatch: 1 variables but 2 values')
}

fn test_c_style_for_post_rejects_scalar_rhs_for_multi_assign() {
	v3_bin := for_multi_init_build_v3()
	for_multi_init_run_bad(v3_bin, 'post_scalar_rhs', 'fn main() {
	mut a := 0
	mut b := 0
	for ; a < 1; a, b = 1 {
		break
	}
	println(b)
}
',
		'multi-return assignment mismatch: 2 variables but `int` has 1 values')
}

fn test_c_style_for_post_does_not_use_condition_smartcast() {
	v3_bin := for_multi_init_build_v3()
	for_multi_init_run_bad(v3_bin, 'post_stale_condition_smartcast', 'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

fn main() {
	mut x := Value(Foo{
		foo: 1
	})
	for ; x is Foo; x.foo {
		x = Bar{}
	}
}
',
		'unknown field `foo`')
}

fn test_c_style_for_multi_init_allows_selector_lhs_after_comma() {
	v3_bin := for_multi_init_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_for_multi_init_selector_lhs_${os.getpid()}.v')
	os.write_file(src, 'struct Pair {
mut:
	x int
	y int
}

fn main() {
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
	println("ok")
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_for_multi_init_selector_lhs_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_c_style_for_multi_init_rejects_missing_rhs() {
	v3_bin := for_multi_init_build_v3()
	for_multi_init_run_bad(v3_bin, 'missing_rhs', 'fn main() {
	for a, b := 1; false; {
		println(a)
		println(b)
	}
}
',
		'for init assignment mismatch: 2 variables but 1 values')
}

fn test_for_in_rejects_selector_value_var_after_comma() {
	v3_bin := for_multi_init_build_v3()
	for_multi_init_run_bad(v3_bin, 'for_in_selector_value', 'struct Box {
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
',
		'invalid for-in header: expected identifiers before `in`')
}
