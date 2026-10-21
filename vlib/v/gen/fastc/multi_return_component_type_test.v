module fastc

import os
import v.cmdexec
import v.pref

// A multi-return slot is read back as the component's declared type, so a returned value
// is stored as that type: numbers are converted, and a concrete value returned for an
// interface or sum type component is boxed.
fn test_fastc_multi_return_components_are_stored_as_declared_types() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_multi_return_component_type_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

interface Frame {
	marker() int
}

struct Padding {
	length int
}

fn (p Padding) marker() int {
	return p.length * 10
}

type Value = int | string

fn parse() ?(Frame, int) {
	return Padding{
		length: 3
	}, 2
}

fn frame() (Frame, int) {
	return Padding{
		length: 4
	}, 5
}

fn numbers() (f64, f32, u8) {
	return 1, 2, 3
}

fn value() (Value, int) {
	return "abc", 1
}

fn main() {
	f, n := parse() or { return }
	assert f.marker() == 30
	assert n == 2
	g, m := frame()
	assert g.marker() == 40
	assert m == 5
	a, b, c := numbers()
	assert a == 1.0
	assert b == f32(2.0)
	assert c == 3
	v, k := value()
	assert v is string
	if v is string {
		assert v == "abc"
	}
	assert k == 1
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
