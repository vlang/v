module fastc

import os
import v.cmdexec
import v.pref

// `[]Struct{len: n}` gives every element the struct's default value. A zeroed element
// would hold dynamic array fields with element_size == 0, so appending to them copied
// no data (cmd/v's parallel cgen merge crashed on that), and maps without their hash
// functions.
fn test_fastc_struct_array_elements_get_struct_defaults() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_struct_array_defaults_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

struct Defs {
	idx int
mut:
	spawn    []string
	callback []string
	counts   map[string]int
	limit    int = 7
}

fn fill(n int) []Defs {
	mut defs := []Defs{len: n}
	for i in 0 .. n {
		defs[i].spawn << "spawn " + i.str()
		defs[i].spawn << "again"
		defs[i].callback << "cb"
		defs[i].counts["k"] = i
	}
	return defs
}

fn main() {
	defs := fill(3)
	assert defs.len == 3
	for i, d in defs {
		assert d.spawn == ["spawn " + i.str(), "again"]
		assert d.callback == ["cb"]
		assert d.counts["k"] == i
		assert d.limit == 7
	}
	mut more := []Defs{len: 2, cap: 4}
	more[1].spawn << "x"
	assert more[1].spawn == ["x"]
	assert more[0].limit == 7
	empty := []Defs{}
	assert empty.len == 0
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
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
