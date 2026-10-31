module fastc

import os
import v.cmdexec
import v.pref

// A sum type subject narrowed to an enum variant (`value is Kind && value == .one`) is a
// pointer to its payload in C; an enum shorthand comparison must compare the value it
// points to, not the pointer.
fn test_fastc_narrowed_enum_variant_compares_its_value() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_enum_smartcast_comparison_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

enum Kind {
	one
	two
}

type Value = Kind | int

fn is_one(value Value) bool {
	return value is Kind && value == .one
}

fn is_two(value Value) bool {
	return value is Kind && .two == value
}

fn is_not_one(value Value) bool {
	if value is Kind {
		return value != .one
	}
	return false
}

fn bump(mut k Kind) bool {
	if k == .one {
		k = .two
		return true
	}
	return false
}

fn main() {
	assert is_one(Value(Kind.one))
	assert !is_one(Value(Kind.two))
	assert !is_one(Value(0))
	assert is_two(Value(Kind.two))
	assert !is_two(Value(Kind.one))
	assert is_not_one(Value(Kind.two))
	assert !is_not_one(Value(Kind.one))
	mut k := Kind.one
	assert bump(mut k)
	assert k == .two
	assert !bump(mut k)
	println("ok")
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-pthread', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
