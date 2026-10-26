module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_global_assignment_resolves_modules_and_preserves_mutable_reference_identity() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_global_assignment_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'registry')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'global_assignment' }") or { panic(err) }
	os.write_file(os.join_path(root, 'registry', 'registry.v'), r'module registry
pub struct Node {
pub mut:
	value u64
}
__global (
	enabled bool
	root &Node
	count int
	slots [2]u64
	nodes [2]&Node
)
pub fn install(mut node Node) { root = node }
pub fn at() &Node { return root }
pub fn state() bool { return enabled }
pub fn number() int { return count }
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import registry
struct Values {
mut:
	items [2]u64
}
fn C.memcpy(voidptr, voidptr, uint) voidptr
fn C.memset(voidptr, int, uint) voidptr
fn main() {
	enabled = true
	count = int(1) << 40
	assert registry.state() && registry.number() == int(1) << 40
	mut node := registry.Node{value: u64(1) << 55}
	registry.install(mut node)
	assert registry.at() == &node
	node.value += 7
	assert registry.at().value == (u64(1) << 55) + 7
	nodes[0] = &node
	assert nodes[0] == &node && nodes[0].value == node.value
	slots = [u64(1) << 55, u64(9)]!
	assert slots[0] == u64(1) << 55 && slots[1] == 9
	mut state := Values{}
	C.memcpy(state.items, slots, uint(16))
	assert state.items[0] == u64(1) << 55 && state.items[1] == 9
	C.memset(state.items, 0, uint(16))
	assert state.items[0] == 0 && state.items[1] == 0
	slots = [2]u64{}
	assert slots[0] == 0 && slots[1] == 0
	mut slots := [u64(4), u64(6)]!
	assert slots[0] == 4 && slots[1] == 6
	mut enabled := false
	enabled = true
	enabled = false
	assert !enabled && registry.state()
	count, enabled = 42, true
	assert registry.number() == 42 && enabled && registry.state()
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.enable_globals = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=incompatible-pointer-types', '-O2', '-o',
		program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, '${run.output}\n${c_path}'
}
