import os
import v.tempname

const variadic_call_vexe = @VEXE
const variadic_call_tests_dir = os.dir(@FILE)
const variadic_call_v3_dir = os.dir(variadic_call_tests_dir)
const variadic_call_vlib_dir = os.dir(variadic_call_v3_dir)
const variadic_call_v3_src = os.join_path(variadic_call_v3_dir, 'v.v')

const variadic_call_fixture_dir = os.join_path(os.vtmp_dir(), 'v3_variadic_call_fixtures_${tempname.unique_token()}')
const variadic_call_compiler = os.join_path(variadic_call_fixture_dir, 'v3') + $if windows { '.exe' } $else { '' }

fn testsuite_begin() {
	os.mkdir_all(variadic_call_fixture_dir) or { panic(err) }
	build :=
		os.exec([variadic_call_vexe, '-gc', 'none', '-path',
			'${variadic_call_vlib_dir}' + '|@vlib|@vmodules', '-o', variadic_call_compiler,
			'${variadic_call_v3_src}'])
	assert build.exit_code == 0, build.output
}

fn testsuite_end() {
	os.rmdir_all(variadic_call_fixture_dir) or {}
}

fn test_fixed_slot_spread_variadic_call_expands_before_codegen() {
	src := os.join_path(variadic_call_fixture_dir, 'v3_fixed_slot_spread_variadic.v')
	os.write_file(src, 'fn take(first int, rest ...int) int {
	return first * 100 + rest[0] * 10 + rest[1]
}

fn main() {
	xs := [2, 7, 8]
	println(int_str(take(...xs)))
}
') or {
		panic(err)
	}
	bin := os.join_path(variadic_call_fixture_dir, 'v3_fixed_slot_spread_variadic')
	compile := os.exec([variadic_call_compiler, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '278', run.output
}

fn test_fixed_slot_spread_variadic_call_rejects_trailing_args() {
	src := os.join_path(variadic_call_fixture_dir, 'v3_fixed_slot_spread_variadic_trailing.v')
	os.write_file(src, 'fn take(first int, rest ...int) int {
	return first * 1000 + rest[0] * 100 + rest[1] * 10 + rest[2]
}

fn main() {
	xs := [2, 7, 8]
	println(int_str(take(...xs, 9)))
}
') or {
		panic(err)
	}
	bin := os.join_path(variadic_call_fixture_dir, 'v3_fixed_slot_spread_variadic_trailing')
	compile := os.exec([variadic_call_compiler, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains('when forwarding a variadic variable, it must be the final argument'), compile.output
}

fn test_interface_variadic_forwarded_array_is_not_repacked() {
	src := os.join_path(variadic_call_fixture_dir, 'v3_interface_variadic_forward.v')
	os.write_file(src, 'interface Displayable {
	display() string
}

interface Any {}

struct Item {
	text string
}

fn (item Item) display() string {
	return item.text
}

fn sink(items ...Displayable) string {
	mut out := ""
	for item in items {
		out += item.display()
	}
	return out
}

fn forward(items ...Displayable) string {
	return sink(items)
}

fn count(values ...Any) int {
	return values.len
}

fn main() {
	println(forward(Item{
		text: "a"
	}, Item{
		text: "b"
	}))
	xs := []Any{}
	println(count(...xs))
	println(count(xs))
}
') or {
		panic(err)
	}
	bin := os.join_path(variadic_call_fixture_dir, 'v3_interface_variadic_forward')
	compile := os.exec([variadic_call_compiler, '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// A spread array is forwarded as is, while a bare array is packed as one variadic
	// element (d2e6876e38).
	assert run.output.trim_space() == 'ab\n0\n1', run.output
}

fn test_source_var_with_generated_variadic_prefix_is_not_forwarded() {
	src := os.join_path(variadic_call_fixture_dir, 'v3_source_varargs_prefix.v')
	os.write_file(src, 'interface Any {}

fn count(values ...Any) int {
	return values.len
}

fn main() {
	__varargs_items := []Any{}
	println(count(__varargs_items))
}
') or {
		panic(err)
	}
	bin := os.join_path(variadic_call_fixture_dir, 'v3_source_varargs_prefix')
	compile := os.exec([variadic_call_compiler, '-translated', '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '1', run.output
}
