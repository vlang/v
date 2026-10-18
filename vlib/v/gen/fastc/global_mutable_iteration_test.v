module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_global_array_iteration_mutations_reach_original_storage() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_global_mutable_iteration_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'registry')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'global_mutable_iteration' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'registry', 'registry.v'), r'module registry
pub struct Item {
pub mut:
	value int
}
pub struct Holder {
pub mut:
	values []int
}
pub __global (
	shared_values []int = [3, 4]
	shared_items []&Item = [&Item{ value: 5 }, &Item{ value: 6 }]
	shared_holder Holder = Holder{ values: [7, 8] }
)
pub fn add_from_declaring_module() {
	for mut value in shared_values { value += 1000 }
}
pub fn item_value(index int) int { return shared_items[index].value }
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import registry
__global own_values []int = [1, 2]
fn main() {
	for mut value in own_values { value += 10 }
	assert own_values == [11, 12]
	for mut value in shared_values { value += 10 }
	for mut value in registry.shared_values { value += 100 }
	assert shared_values == [113, 114]
	registry.add_from_declaring_module()
	assert registry.shared_values == [1113, 1114]
	for mut item in shared_items { item.value += 20 }
	for mut item in registry.shared_items { item.value += 100 }
	assert shared_items[0].value == 125 && shared_items[1].value == 126
	assert registry.item_value(0) == 125 && registry.item_value(1) == 126
	for mut value in shared_holder.values { value += 10 }
	for mut value in registry.shared_holder.values { value += 100 }
	assert shared_holder.values == [117, 118]
	shared_items := [&registry.Item{ value: 900 }]
	assert shared_items[0].value == 900
	assert registry.item_value(0) == 125
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

fn test_fastc_global_iteration_keeps_immutable_local_shadow_rejection() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.enable_globals = true
	mut message := ''
	_ := generate(r'module main
__global values []int
fn main() {
	values := [1, 2]
	for mut value in values { value++ }
}
', 'immutable_global_shadow.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('mutable iteration over immutable collection'), message
}
