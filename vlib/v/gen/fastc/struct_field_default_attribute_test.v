module fastc

import os
import v.cmdexec
import v.pref

// fastc_default_attribute_prefs returns the preferences every case in this file compiles with.
fn fastc_default_attribute_prefs() &pref.Preferences {
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	return prefs
}

// fastc_refusal_message returns the error the fastc backend reports for `source`, or '' when it is accepted.
fn fastc_refusal_message(name string, source string) string {
	root := os.join_path(os.vtmp_dir(), 'fastc_refusal_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	generate_files([root], fastc_default_attribute_prefs()) or { return err.msg() }
	return ''
}

// A struct field may combine a default value and an attribute, in either order.
fn test_fastc_struct_field_accepts_default_and_attribute() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_struct_field_default_attribute_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
pub struct Index {
	pub:
	x int
}
pub struct G {
	pub mut:
	n int
	index_cache &Index = unsafe { nil } @[json: "-"]
	label string = "default" @[json: "label"]
	count int @[json: "count"] = 7
	flag int = 5 @[skip]
	arr []int = [1, 2] @[json: "arr"]
	paren int = (1 + 2) @[json: "paren"]
}
pub struct Base {
	pub mut:
	x int = 4
}
pub struct Outer {
	pub mut:
	Base = Base{} @[json: "b"]
	n int
}
fn main() {
	o := Outer{}
	assert o.n == 0
	g := G{}
	assert g.n == 0
	assert g.index_cache == unsafe { nil }
	assert g.label == "default"
	assert g.count == 7
	assert g.flag == 5
	assert g.arr.len == 2
	assert g.arr[1] == 2
	assert g.paren == 3
	mut skipped := 0
	$for f in G.fields {
		$if f.attrs.contains("skip") {
			skipped++
		}
	}
	assert skipped == 1
	println("ok")
}
') or {
		panic(err)
	}
	c_source := generate_files([root], fastc_default_attribute_prefs()) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

// An embedded field with a default has no skip/required flags, so `@[skip]` or `@[required]` on it is refused.
fn test_fastc_embedded_default_rejects_skip_and_required() {
	$if !macos && !linux {
		return
	}
	for attribute in ['skip', 'required'] {
		msg := fastc_refusal_message('embedded_${attribute}', 'module main
pub struct Base {
	pub mut:
	x int = 4
}
pub struct Outer {
	pub mut:
	Base = Base{} @[${attribute}]
	n int
}
fn main() {
	o := Outer{}
	assert o.n == 0
}
')
		assert msg.contains('does not support @[skip] or @[required] on embedded field'), '@[${attribute}]: ${msg}'
	}
}
