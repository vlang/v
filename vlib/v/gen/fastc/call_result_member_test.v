module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_call_result_fields_lower_array_literal_arguments() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := 'module main
struct Result {
	output string
	status int
}
fn run(values []string) Result {
	return Result{output: values[0], status: values.len}
}
fn main() {
	flag := "value"
	version := run([flag]).output
	println(version)
	singleton := run(["literal"]).status == 1
	println(singleton)
	mut usable := false
	usable = run(["one", "two"]).status == 2
	println(usable)
}
'
	c_source := generate(source, 'call_result_member.v', prefs) or { panic(err) }
	assert !c_source.contains('run([flag])'), c_source
	root := os.join_path(os.vtmp_dir(), 'fastc_call_result_member_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source_file := os.join_path(root, 'main.v')
	program := os.join_path(root, 'program')
	os.write_file(source_file, source) or { panic(err) }
	compiled := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-no-prealloc', '-nocache', '-o',
		program, root])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(program, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output == 'value\ntrue\ntrue\n'
}

fn test_fastc_pointer_type_sizeof_and_empty_array_use_c_type_syntax() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main
struct CachedName { value int }
struct array { len int }
fn consume(size isize) {}
fn main() {
	ar := array{}
	size := isize(sizeof(&CachedName))
	consume(isize(3) * isize(sizeof(&CachedName)))
	assert sizeof(&CachedName) == sizeof(voidptr)
	println(1 + sizeof(&CachedName))
	println(ar.len)
	println(size)
}
', 'pointer_sizeof_empty_array.v', prefs) or { panic(err) }
	assert c_source.contains('(array){0}'), c_source
	assert !c_source.contains('((array)0)'), c_source
	assert c_source.contains('sizeof(CachedName*)'), c_source
	assert !c_source.contains('sizeof(&CachedName)'), c_source
}

fn test_fastc_nested_pointer_type_sizeof_executes() {
	root := os.join_path(os.vtmp_dir(), 'fastc_pointer_sizeof_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'struct CachedName { value int }
fn multiplied_size(size isize) isize { return size }
fn main() {
	assert multiplied_size(isize(3) * isize(sizeof(&CachedName))) == isize(3 * sizeof(voidptr))
	assert sizeof(&CachedName) == sizeof(voidptr)
	size := 1 + sizeof(&CachedName)
	assert size == 1 + sizeof(voidptr)
	println("pointer sizes")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	compiled := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-no-prealloc', '-nocache', '-o',
		program, root])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(program, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output == 'pointer sizes\n'
}
