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
