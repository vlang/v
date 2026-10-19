module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_option_void_callback_fields_aliases_and_globals() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_option_void_callback_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, r'module main
struct FramebufferInfo { id int }
struct FramebufferDriver {
mut:
	init fn () = unsafe { nil }
	register_device fn (FramebufferInfo) ? = unsafe { nil }
	reset fn (bool) ! = unsafe { nil }
}
type RegisterDevice = fn (FramebufferInfo) ?
__global (global_register fn (FramebufferInfo) ? = unsafe { nil })
fn register(info FramebufferInfo) ? {
	if info.id < 0 { return none }
}
fn reset(fail bool) ! {
	if fail { return error("reset failed") }
}
fn invoke_alias(callback RegisterDevice, info FramebufferInfo) bool {
	callback(info) or { return false }
	return true
}
fn main() {
	mut driver := FramebufferDriver{}
	assert driver.init == unsafe { nil }
	assert driver.register_device == unsafe { nil }
	assert driver.reset == unsafe { nil }
	assert global_register == unsafe { nil }
	driver.register_device = register
	mut failures := 0
	driver.register_device(FramebufferInfo{ id: 7 }) or { failures++ }
	assert failures == 0
	driver.register_device(FramebufferInfo{ id: -1 }) or { failures++ }
	assert failures == 1
	assert invoke_alias(RegisterDevice(register), FramebufferInfo{ id: 9 })
	assert !invoke_alias(RegisterDevice(register), FramebufferInfo{ id: -2 })
	global_register = register
	global_register(FramebufferInfo{ id: 11 }) or { failures++ }
	assert failures == 1
	global_register(FramebufferInfo{ id: -3 }) or { failures++ }
	assert failures == 2
	driver.reset = reset
	driver.reset(false) or { failures++ }
	assert failures == 2
	driver.reset(true) or {
		assert err.msg() == "reset failed"
		failures++
	}
	assert failures == 3
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.enable_globals = true
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
