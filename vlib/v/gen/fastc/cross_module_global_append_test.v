module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_cross_module_global_array_append_retains_pointer_storage() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_cross_module_append_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'registry')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'cross_module_append' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'registry', 'registry.v'), r'module registry
pub struct Local {
pub mut:
	value int
}
__global cpu_locals []&Local
pub fn count() int { return cpu_locals.len }
pub fn at(index int) &Local { return cpu_locals[index] }
pub fn append_local(item &Local) { cpu_locals << item }
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import registry
fn main() {
	first := &registry.Local{ value: 1 }
	second := &registry.Local{ value: 2 }
	third := &registry.Local{ value: 3 }
	fourth := &registry.Local{ value: 4 }
	cpu_locals << first
	cpu_locals << [second, third]
	assert registry.count() == 3 && cpu_locals.len == 3
	assert registry.at(0) == first && registry.at(1) == second && registry.at(2) == third
	mut copied := cpu_locals.clone()
	assert copied.len == 3 && copied[0] == first && copied[1] == second
	copied[1].value = 22
	assert registry.at(1).value == 22 && second.value == 22
	registry.append_local(fourth)
	assert cpu_locals.len == 4 && registry.at(3) == fourth
	assert copied.len == 3
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.enable_globals = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=incompatible-pointer-types', '-o', program,
		c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
