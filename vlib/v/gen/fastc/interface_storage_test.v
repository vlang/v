module fastc

import os
import v.cmdexec
import v.pref

@[unsafe]
fn test_fastc_interface_fields_mutate_concrete_storage_and_preserve_pointer_carriers() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_interface_storage_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'interface.h'), '#include <stdint.h>\n#include <stdlib.h>\nstatic int fastc_invalid_interface(void) { return getenv("FASTC_INVALID_INTERFACE") != NULL; }\nstatic void fastc_invalidate_interface(void *value) { struct carrier { void *object; uintptr_t tag; }; ((struct carrier *)value)->tag = UINTPTR_MAX; }\n') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
#include "@DIR/interface.h"
fn C.fastc_invalid_interface() int
fn C.fastc_invalidate_interface(value voidptr)
struct TimeSpec {
	sec int
	nsec int
}
struct Stat {
mut:
	mode u64
	nlink u64
	atim TimeSpec
	mtim TimeSpec
	ctim TimeSpec
}
struct Child {
mut:
	value u64
}
interface Resource {
	label() int
mut:
	stat Stat
	refcount u64
	sequence int
	child &Child
}
interface Named { label() int }
type Packet = Named | int
struct Alpha {
mut:
	padding u8
	stat Stat
	refcount u64
	sequence int
	child &Child
}
struct Beta {
mut:
	padding [2]u64
	child &Child
	refcount u64
	sequence int
	stat Stat
}
fn (value Alpha) label() int { return 1 }
fn (value Beta) label() int { return 2 }
fn bump(mut value u64) { value += 1 }
fn advance(mut value int) { value += 3 }
fn mutate(mut value Resource) {
	value.stat.mode = u64(0xf123456789abcdef)
	bump(mut value.refcount)
	bump(mut value.stat.nlink)
	advance(mut value.sequence)
	value.child.value += 7
}
fn copy_pointer(value &Resource) Resource { return Resource(value) }
fn copy_mutable(mut value Resource) Resource { return Resource(value) }
fn as_named(mut value Resource) Named { return Named(value) }
fn packed(value Named) Packet { return value }
fn packed_mutable(mut value Named) Packet { return value }
fn packed_label(value Packet) int {
	if value is Named {
		return value.label()
	}
	return 0
}
fn missing() &Resource { return unsafe { nil } }
fn boxed(value &Alpha) &Resource { return value }
fn selected(mut calls int, value &Resource) &Resource {
	calls++
	return value
}
fn borrowed(value &Resource) u64 { return value.stat.mode }
fn borrow_mutable(mut value Alpha) u64 { return borrowed(value) }
fn mutable_pointer(mut value Alpha) &Alpha { return value }
fn optional_pointer(mut value Alpha, present bool) ?&Alpha {
	if !present { return none }
	return value
}
fn concrete_pointer(value &Resource) &Alpha {
	mut actual := unsafe { value }
	if actual != unsafe { nil } {
		if mut actual is Alpha { return actual }
	}
	return unsafe { nil }
}
fn main() {
	if C.fastc_invalid_interface() != 0 {
		invalid := Resource(Alpha{})
		C.fastc_invalidate_interface(unsafe { &invalid })
		assert invalid.refcount == 0
		return
	}
	mut alpha := &Alpha{
		stat: Stat{ mode: 1, nlink: u64(0x8000000000000000) }
		refcount: u64(0xf000000000000000)
		sequence: int(0x1234567800000000)
		child: &Child{ value: 5 }
	}
	mut resource := Resource(alpha)
	resource.stat.mode = 42
	assert alpha.stat.mode == 42
	bump(mut resource.refcount)
	assert alpha.refcount == u64(0xf000000000000001)
	mutate(mut resource)
	assert alpha.stat.mode == u64(0xf123456789abcdef)
	assert alpha.stat.nlink == u64(0x8000000000000001)
	assert alpha.refcount == u64(0xf000000000000002)
	assert alpha.sequence == int(0x1234567800000003)
	assert borrow_mutable(mut alpha) == u64(0xf123456789abcdef)
	assert mutable_pointer(mut alpha) == alpha
	unwrapped := optional_pointer(mut alpha, true) or { panic("missing pointer") }
	assert unwrapped == alpha
	assert optional_pointer(mut alpha, false) == none
	assert concrete_pointer(&resource) == alpha
	old_atim, old_mtim, old_ctim := resource.stat.atim, resource.stat.mtim, resource.stat.ctim
	resource.stat.atim, resource.stat.mtim, resource.stat.ctim = TimeSpec{ sec: 5 }, TimeSpec{ sec: 6 }, TimeSpec{ sec: 7 }
	assert alpha.stat.atim.sec == 5 && alpha.stat.mtim.sec == 6 && alpha.stat.ctim.sec == 7
	resource.stat.atim, resource.stat.mtim, resource.stat.ctim = old_atim, old_mtim, old_ctim
	assert alpha.stat.atim.sec == 0 && alpha.stat.mtim.sec == 0 && alpha.stat.ctim.sec == 0
	assert alpha.child.value == 12
	mut copied := copy_pointer(&resource)
	copied.stat.mode = 99
	assert alpha.stat.mode == 99
	mut copied_mutable := copy_mutable(mut resource)
	copied_mutable.child.value = 20
	assert alpha.child.value == 20
	assert as_named(mut resource).label() == 1
	mut named := as_named(mut resource)
	packet := packed(named)
	assert packet is Named
	assert packed_label(packet) == 1
	mutable_packet := packed_mutable(mut named)
	assert mutable_packet is Named
	assert packed_label(mutable_packet) == 1
	mut beta := &Beta{ child: &Child{ value: 9 }, refcount: 50 }
	mut other := Resource(beta)
	mutate(mut other)
	assert beta.stat.mode == u64(0xf123456789abcdef)
	assert beta.stat.nlink == 1 && beta.refcount == 51
	assert beta.sequence == 3
	assert beta.child.value == 16
	assert as_named(mut other).label() == 2
	mut calls := 0
	selected(mut calls, &resource).stat.mode = 123
	assert calls == 1 && alpha.stat.mode == 123
	bump(mut selected(mut calls, &resource).stat.nlink)
	assert calls == 2 && alpha.stat.nlink == u64(0x8000000000000002)
	assert missing() == unsafe { nil }
	pointer := boxed(alpha)
	assert pointer.label() == 1
	assert pointer.stat.mode == 123
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
	invalid := cmdexec.run_in_merged_with_env(program, [], root, {
		'FASTC_INVALID_INTERFACE': '1'
	})
	assert invalid.exit_code != 0
	assert invalid.output.contains('invalid interface type'), invalid.output
}
