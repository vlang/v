import os

const string_int_vexe = @VEXE
const string_int_tests_dir = os.dir(@FILE)
const string_int_v3_dir = os.dir(string_int_tests_dir)
const string_int_vlib_dir = os.dir(string_int_v3_dir)
const string_int_v3_src = os.join_path(string_int_v3_dir, 'v3.v')

fn string_int_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_string_int_markused_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${string_int_vexe} -gc none -path "${string_int_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${string_int_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_top_level_string_int_roots_exact_receiver_method() {
	v3_bin := string_int_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_string_int_markused_${os.getpid()}.v')
	os.write_file(src, "module main

struct S {
	value int
}

struct U {
	value int
}

fn (s S) int() int {
	return s.value + 1
}

fn (u U) int() int {
	return u.value + 100
}

fn use_local_receiver() {
	s := S{
		value: 41
	}
	println(s.int())
}

println('42'.int())
use_local_receiver()
") or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_string_int_markused_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('implicit declaration'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42\n42', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	string_int_refs := generated.count('string__int(')
	assert string_int_refs >= 2, generated
	assert generated.contains('S__int('), generated
	assert !generated.contains('U__int('), generated
}

fn test_top_level_or_block_root_receiver_call_roots_string_int() {
	v3_bin := string_int_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_string_int_or_root_${os.getpid()}.v')
	os.write_file(src, "module main

n := arguments()[1] or { '10' }.int()
println(n)
") or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_string_int_or_root_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('implicit declaration'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '10', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('string__int('), generated
}

fn test_top_level_calls_are_ignored_when_explicit_main_exists() {
	v3_bin := string_int_build_v3()
	src := os.join_path(os.temp_dir(), 'v3_explicit_main_ignores_top_level_${os.getpid()}.v')
	os.write_file(src, "module main

fn ignored_top_level_only() {
	println('ignored')
}

ignored_top_level_only()

fn main() {
	println('entry')
}
") or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_explicit_main_ignores_top_level_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'entry', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert !generated.contains('ignored_top_level_only('), generated
}
