module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_target_static_globals_survive_early_entry_and_dynamic_init_runs_late() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_target_global_startup_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'native.h'), '#include <stdint.h>\nextern uint64_t native_runtime_value;\n') or { panic(err) }
	os.write_file(os.join_path(root, 'main.c.v'), r'@[has_globals]
module main
#include "native.h"
__global C.native_runtime_value u64
const boot_magic = u64(0xf123456789abcdef)
struct Boot {
mut:
	magic u64 = boot_magic
	words [2]u64 = [u64(11), 22]!
}
__global (
	page_size = u64(1) << 12
	counter = u64(0)
	state = unsafe { nil }
	enabled = true
	sentinel = int(-1)
	boot = Boot{}
	dynamic = C.dynamic_initialize()
	hooks int
	later_pointer voidptr = voidptr(&later_counter)
	later_callback fn() u64 = callback_body
	native_snapshot u64 = C.native_runtime_value
	later_counter = u64(5)
)
// These helpers are supplied by the native body, outside the included header.
@[c_extern]
fn C.dynamic_initialize() u64
@[c_extern]
fn C.allocator_ready() bool
@[c_extern]
fn C.dynamic_calls() int
fn C._vinit(argc int, argv voidptr)
fn init() {
	assert C.allocator_ready()
	hooks++
}
@[markused]
fn callback_body() u64 { return later_counter + 1 }
@[export: "early_entry"]
fn early_entry() {
	assert page_size == 0x1000 && counter == 0
	assert state == unsafe { nil }
	assert enabled && sentinel == -1
	assert boot.magic == boot_magic && boot.words[1] == 22
	assert dynamic == 0 && hooks == 0
	assert later_pointer == unsafe { nil } && native_snapshot == 0
	assert later_counter == 5
	page_size = 0x4000
	counter = 37
	state = voidptr(&counter)
	enabled = false
	sentinel = 9
	boot.magic = 42
	boot.words[1] = 99
	later_counter = 41
}
fn main() {
	assert page_size == 0x4000 && counter == 37
	assert dynamic == 0 && hooks == 0
	assert later_pointer == unsafe { nil } && native_snapshot == 0
	C._vinit(0, unsafe { nil })
	assert page_size == 0x4000 && counter == 37
	assert state == voidptr(&counter)
	assert !enabled && sentinel == 9
	assert boot.magic == 42 && boot.words[1] == 99
	assert dynamic == 123 && C.dynamic_calls() == 1 && hooks == 1
	assert later_pointer == voidptr(&later_counter)
	assert later_callback() == 42
	assert native_snapshot == 314
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.enable_globals = true
	prefs.target_libc_headers = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, source + r'
static bool ready;
static int calls;
uint64_t native_runtime_value = 10;
u64 dynamic_initialize(void) { if (!ready) abort(); calls++; return 123; }
bool allocator_ready(void) { return ready; }
int dynamic_calls(void) { return calls; }
__attribute__((constructor)) static void native_entry(void) {
	if (later_callback != NULL) abort();
	early_entry();
	native_runtime_value = 314;
	ready = true;
}
') or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
