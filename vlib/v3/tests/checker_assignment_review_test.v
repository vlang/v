import os

const checker_assignment_vexe = @VEXE
const checker_assignment_tests_dir = os.dir(@FILE)
const checker_assignment_v3_dir = os.dir(checker_assignment_tests_dir)
const checker_assignment_vlib_dir = os.dir(checker_assignment_v3_dir)
const checker_assignment_v3_src = os.join_path(checker_assignment_v3_dir, 'v3.v')

fn checker_assignment_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_checker_assignment_review_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${checker_assignment_vexe} -gc none -path "${checker_assignment_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${checker_assignment_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn checker_assignment_run_bad(v3_bin string, name string, source string, expected string) {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains(expected), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn checker_assignment_run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	os.rm(bin) or {}
	os.rm(bin + '.c') or {}
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_assignment_invalidates_written_lhs_smartcast() {
	v3_bin := checker_assignment_build_v3()
	checker_assignment_run_bad(v3_bin, 'bad_assignment_stale_smartcast', 'struct Foo {
	foo int
}

struct Bar {}

type Value = Bar | Foo

fn main() {
	mut x := Value(Foo{
		foo: 1
	})
	if x is Foo {
		x = Bar{}
		println(int_str(x.foo))
	}
}
',
		'unknown field `foo`')
}

fn test_reject_container_stored_capturing_fn_literals() {
	v3_bin := checker_assignment_build_v3()
	checker_assignment_run_bad(v3_bin, 'bad_append_alias_capturing_fn_literal', 'fn main() {
	x := 1
	f := fn [x] () int {
		return x
	}
	mut callbacks := []fn () int{}
	callbacks << f
}
',
		'capturing fn literal cannot be stored in a container')
	checker_assignment_run_bad(v3_bin, 'bad_append_direct_capturing_fn_literal', 'fn main() {
	x := 1
	mut callbacks := []fn () int{}
	callbacks << fn [x] () int {
		return x
	}
}
',
		'capturing fn literal cannot be stored in a container')
}

fn test_reject_nonlocal_assigned_capturing_fn_literals() {
	v3_bin := checker_assignment_build_v3()
	checker_assignment_run_bad(v3_bin, 'bad_field_assign_direct_capturing_fn_literal', 'struct Holder {
mut:
	cb fn () int
}

fn plain() int {
	return 0
}

fn main() {
	x := 1
	mut holder := Holder{
		cb: plain
	}
	holder.cb = fn [x] () int {
		return x
	}
}
',
		'capturing fn literal cannot be stored in a container')
	checker_assignment_run_bad(v3_bin, 'bad_field_assign_alias_capturing_fn_literal', 'struct Holder {
mut:
	cb fn () int
}

fn plain() int {
	return 0
}

fn main() {
	x := 1
	f := fn [x] () int {
		return x
	}
	mut holder := Holder{
		cb: plain
	}
	holder.cb = f
}
',
		'capturing fn literal cannot be stored in a container')
	checker_assignment_run_bad(v3_bin, 'bad_index_assign_capturing_fn_literal', 'fn plain() int {
	return 0
}

fn main() {
	x := 1
	mut callbacks := [plain]
	callbacks[0] = fn [x] () int {
		return x
	}
}
',
		'capturing fn literal cannot be stored in a container')
}

fn test_shadowed_capturing_fn_literal_marker_uses_nearest_binding() {
	v3_bin := checker_assignment_build_v3()
	out := checker_assignment_run_good(v3_bin, 'capturing_fn_literal_shadowed_by_plain_fn', 'fn plain() int {
	return 2
}

fn main() {
	x := 1
	f := fn [x] () int {
		return x
	}
	mut callbacks := []fn () int{}
	{
		f := plain
		callbacks << f
	}
	assert callbacks[0]() == 2
	assert f() == 1
	println("ok")
}
')
	assert out == 'ok'
	checker_assignment_run_bad(v3_bin, 'capturing_fn_literal_outer_marker_survives_shadow', 'fn plain() int {
	return 2
}

fn main() {
	x := 1
	f := fn [x] () int {
		return x
	}
	mut callbacks := []fn () int{}
	{
		f := plain
		callbacks << f
	}
	callbacks << f
}
',
		'capturing fn literal cannot be stored in a container')
}
