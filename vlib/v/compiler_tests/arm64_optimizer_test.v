module main

import os
import v.cmdexec

// The optimizer runs for `-prod`. Each program must print the same with and
// without it, and matches the expected text.
fn optimized_and_plain(name string, source string) (string, string) {
	root := os.join_path(os.vtmp_dir(), 'arm64_optimizer_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	os.write_file(path, source) or { panic(err) }
	mut outputs := []string{}
	for args in [['-b', 'arm64'], ['-prod', '-b', 'arm64']] {
		output := os.join_path(root, 'main_${outputs.len}')
		mut build := args.clone()
		build << ['-w', '-o', output, path]
		compiled := cmdexec.run(@VEXE, build)
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		outputs << run.output.trim_space()
	}
	return outputs[0], outputs[1]
}

fn check(name string, source string, expected string) {
	$if macos && arm64 {
		plain, optimized := optimized_and_plain(name, source)
		assert plain == expected, plain
		assert optimized == expected, optimized
	}
}

fn test_inlining_keeps_control_flow_and_results() {
	check('inline', "fn add(a int, b int) int {
	return a + b
}

fn clamp(x int, low int, high int) int {
	if x < low {
		return low
	}
	if x > high {
		return high
	}
	return x
}

fn twice(x int) int {
	return add(x, x)
}

fn sum_to(n int) int {
	mut total := 0
	for i in 0 .. n {
		total += i
	}
	return total
}

fn main() {
	mut acc := 0
	for i in 0 .. 5 {
		acc += clamp(twice(i), 1, 6)
	}
	println('\${acc} \${sum_to(10)} \${add(sum_to(3), clamp(9, 0, 4))}')
}
", '19 45 7')
}

fn test_inlining_a_function_that_does_not_return() {
	check('noreturn', "fn stop(code int) int {
	if code > 100 {
		exit(code)
	}
	panic('never')
}

fn pick(flag bool) int {
	if flag {
		return 3
	}
	return stop(200)
}

fn main() {
	println(pick(true))
}
", '3')
}

// A `return` inside `$if` leaves the following `return` in the same block.
fn test_inlining_stops_at_the_first_return() {
	check('dead_return', 'fn truth(a int) bool {
	return a > 0
}

fn wrapper(a int) bool {
	\$if macos {
		return truth(a)
	}
	\$if !macos {
		return truth(a)
	}
	return false
}

fn main() {
	println(wrapper(3))
}
', 'true')
}

fn test_common_subexpressions_across_branches() {
	check('cse', "fn f(a int, b int, flag bool) int {
	x := a * b + 7
	y := if flag { a * b + 7 } else { b - a }
	z := (a * b + 7) ^ (b * a + 7)
	return x + y + z
}

fn main() {
	println('\${f(3, 4, true)} \${f(3, 4, false)}')
}
", '38 20')
}

fn test_strength_reduction_only_where_it_holds() {
	check('strength', "fn main() {
	a := u32(1000)
	b := -1000
	println('\${a * 8} \${a / 8} \${a % 8} \${b * 8} \${b / 8} \${b % 8}')
}
", '8000 125 0 -8000 -125 0')
}

fn test_memory_keeps_aliased_writes() {
	check('memory', "struct Pair {
mut:
	x int
	y int
}

fn bump(mut p Pair) {
	p.x += 10
}

fn write_through(ptr &int, value int) {
	unsafe {
		*ptr = value
	}
}

fn main() {
	mut p := Pair{1, 2}
	bump(mut p)
	first := p.x
	p.y = first + 1
	p.y = first + 2
	mut value := 5
	ptr := &value
	write_through(ptr, 9)
	mut other := Pair{7, 8}
	mut q := &other
	q.x = 70
	println('\${first} \${p.y} \${value} \${other.x}')
}
", '11 13 9 70')
}

fn test_floats_after_optimization() {
	check('floats', 'fn scale(a f64, b f64) f64 {
	return a * b + a * b
}

fn main() {
	mut total := 0.0
	for i in 1 .. 4 {
		total += scale(f64(i), 0.5)
	}
	println(total)
}
', '6.0')
}

// A value-position `match` stops on a value no branch names.
fn test_value_match_without_a_matching_branch() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_value_match_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, 'enum Color {
	red
	green
}

fn main() {
	bad := unsafe { Color(99) }
	x := match bad {
		.red { 1 }
		.green { 2 }
	}
	println(x)
}
') or { panic(err) }
		output := os.join_path(root, 'main')
		compiled := cmdexec.run(@VEXE, ['-w', '-b', 'arm64', '-o', output, source])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run_with_timeout(output, []string{}, 20000)
		assert run.exit_code == 1, run.output
		assert run.output.contains('match: no branch for the value'), run.output
	}
}
