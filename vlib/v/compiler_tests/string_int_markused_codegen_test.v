import os
import v.tempname

fn test_top_level_string_int_roots_exact_receiver_method() {
	root := os.join_path(os.vtmp_dir(), 'v3_string_int_markused_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'main.v')
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
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('implicit declaration'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42\n42', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	string_int_refs := generated.count('string__int(')
	assert string_int_refs >= 2, generated
	assert generated.contains('S__int('), generated
	assert !generated.contains('U__int('), generated
}

fn test_top_level_or_block_root_receiver_call_roots_string_int() {
	root := os.join_path(os.vtmp_dir(), 'v3_string_int_or_root_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'main.v')
	os.write_file(src, "module main

n := arguments()[1] or { '10' }.int()
println(n)
") or {
		panic(err)
	}
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('implicit declaration'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '10', run.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('string__int('), generated
}
