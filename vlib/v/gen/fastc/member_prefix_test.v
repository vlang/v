module fastc

import os
import v.cmdexec
import v.pref

@[unsafe]
fn test_fastc_qualified_call_and_cast_member_roots_preserve_types_and_storage() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_member_prefix_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'procdata')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'member_prefix' }") or { panic(err) }
	os.write_file(os.join_path(root, 'procdata', 'procdata.v'), r'module procdata
pub struct Process {
pub mut:
	unveil u64
}
pub struct Thread {
pub mut:
	process &Process
}
pub struct Ids {
pub:
	ids []u32
}
pub fn Ids.from_names(names []string) Ids {
	return Ids{ids: [u32(names.len)]}
}
pub fn current_thread(thread &Thread, mut calls int) &Thread {
	calls++
	return thread
}
') or { panic(err) }
	os.mkdir_all(os.join_path(root, 'filedata')) or { panic(err) }
	os.write_file(os.join_path(root, 'filedata', 'filedata.v'), r'module filedata
pub struct ForeignHandle {
pub mut:
	flags u64
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import procdata
import filedata
struct Child {
mut:
	value u64
}
interface Readable { value() u64 }
struct Handle {
mut:
	flags u64
	child &Child
	slots [2]u64
	children []&Child
}
fn (value Handle) value() u64 { return value.flags }
fn (value Handle) normalize(text string) string { return text }
fn (mut value Handle) address() u64 { return unsafe { u64(value) } }
fn (mut value Handle) erased() voidptr { return unsafe { voidptr(value) } }
fn scalar_value(mut value u64) u64 { return u64(value) }
fn describe(ids []u32) string { return if ids[0] == 2 { "fn(int)" } else { "bad" } }
fn next(mut calls int, value &Readable) &Readable {
	calls++
	return value
}
fn flags(pointer voidptr) u64 { return unsafe { &Handle(pointer).flags } }
fn flag_address(pointer voidptr) &u64 { return unsafe { &(&Handle(pointer).flags) } }
fn imported_flag(pointer voidptr) bool {
	return pointer != unsafe { nil } && unsafe { &filedata.ForeignHandle(pointer).flags & u64(1) != 0 }
}
fn main() {
	mut calls := 0
	mut process := &procdata.Process{ unveil: u64(0x1234567800000001) }
	thread := &procdata.Thread{ process: process }
	assert procdata.current_thread(thread, mut calls).process.unveil == u64(0x1234567800000001)
	assert calls == 1
	procdata.current_thread(thread, mut calls).process.unveil = u64(0x1234567800000002)
	assert calls == 2 && process.unveil == u64(0x1234567800000002)
	mut handle := &Handle{
		flags: u64(0xf123456780000001)
		child: &Child{ value: 5 }
		slots: [u64(7), 8]!
		children: [&Child{ value: 9 }]
	}
	pointer := voidptr(handle)
	assert handle.address() == u64(pointer)
	assert handle.erased() == pointer
	mut scalar := u64(7)
	assert scalar_value(mut scalar) == 7
	foreign := &filedata.ForeignHandle{ flags: 1 }
	assert imported_flag(voidptr(foreign))
	assert !imported_flag(unsafe { nil })
	assert flags(pointer) == u64(0xf123456780000001)
	assert flag_address(pointer) == &handle.flags
	assert unsafe { &Handle(pointer).flags & u64(1) != 0 }
	unsafe { &Handle(pointer).flags |= u64(2) }
	assert handle.flags == u64(0xf123456780000003)
	unsafe { &Handle(pointer).child.value = 12 }
	assert handle.child.value == 12
	assert unsafe { (&Handle(pointer)).slots[1] } == 8
	unsafe { &Handle(pointer).children[0].value = 15 }
	assert handle.children[0].value == 15
	assert unsafe { &Handle(pointer)[0].child.value } == 12
	mut readable := Readable(handle)
	concrete := unsafe { &Handle(&readable) }
	assert concrete == handle
	unsafe { &Handle(next(mut calls, &readable)).flags = 17 }
	assert calls == 3 && handle.flags == 17
	copy := unsafe { Handle(readable) }
	assert copy.flags == 17 && copy.child == handle.child
	assert unsafe { &Handle(next(mut calls, &readable)).children[0].value } == 15
	assert calls == 4
	text := handle.normalize(describe(procdata.Ids.from_names(["left", "right"]).ids))
	assert "${text.replace("fn(", "fn (")}" == "fn (int)"
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
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
