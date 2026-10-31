module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_legacy_native_aliases_require_a_unique_marked_free_function() {
	mut functions := map[string]FastcFunctionSignature{}
	for module_name in ['left.port', 'right.port'] {
		functions['${module_name}.send'] = FastcFunctionSignature{
			module_name: module_name
			is_markused: true
		}
	}
	functions['C.port__send'] = FastcFunctionSignature{}
	functions['dev.port.receive'] = FastcFunctionSignature{ module_name: 'dev.port' }
	functions['C.port__receive'] = FastcFunctionSignature{}
	functions['dev.port.Packet.flush'] = FastcFunctionSignature{
		module_name: 'dev.port'
		is_markused: true
	}
	functions['C.port__flush'] = FastcFunctionSignature{}
	functions['dev.port.explicit'] = FastcFunctionSignature{
		module_name: 'dev.port'
		is_markused: true
		export_name: 'chosen_export'
	}
	functions['C.port__explicit'] = FastcFunctionSignature{}
	functions['dev.port.disabled'] = FastcFunctionSignature{
		module_name: 'dev.port'
		is_markused: true
		is_disabled: true
	}
	functions['C.port__disabled'] = FastcFunctionSignature{}
	functions['dev.port.unbound'] = FastcFunctionSignature{
		module_name: 'dev.port'
		is_markused: true
	}
	fastc_bind_legacy_native_aliases(mut functions)
	for key in ['left.port.send', 'right.port.send', 'dev.port.receive', 'dev.port.Packet.flush',
		'dev.port.disabled', 'dev.port.unbound'] {
		assert functions[key].export_name == '', key
	}
	assert functions['dev.port.explicit'].export_name == 'chosen_export'
	functions.delete('right.port.send')
	fastc_bind_legacy_native_aliases(mut functions)
	assert functions['left.port.send'].export_name == 'port__send'
}

fn test_fastc_legacy_native_alias_preserves_v_width_and_borrowed_storage() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_legacy_native_alias_${os.getpid()}')
	port_dir := os.join_path(root, 'dev', 'port')
	os.mkdir_all(port_dir) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(port_dir, 'port.c.v'), r'module port
fn C.record_alias_pointer(voidptr)
pub struct Device {
pub mut:
    calls int
}
@[markused]
pub fn send(mut device Device, value int) int {
    C.record_alias_pointer(voidptr(&device))
    device.calls++
    return value + device.calls
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.c.v'), r'module main
import dev.port
fn C.port__send(voidptr, i32) i32
fn C.expect_alias_pointer(voidptr)
fn C.native_alias_call(voidptr) i32
fn C.alias_pointer_calls() i32
@[export: "legacy_alias_entry"]
fn entry() i32 {
    mut device := port.Device{}
    C.expect_alias_pointer(voidptr(&device))
    assert port.send(mut device, int(0x100000002)) == int(0x100000003)
    assert C.port__send(voidptr(&device), i32(7)) == 9
    assert C.native_alias_call(voidptr(&device)) == 20
    assert device.calls == 3
    assert C.alias_pointer_calls() == 3
    return 42
}
fn main() {}
') or { panic(err) }
	native_c := os.join_path(root, 'native.c')
	os.write_file(native_c, r'#include <stdint.h>
#include <stdlib.h>
static void *expected_pointer;
static int32_t calls;
void expect_alias_pointer(void *pointer) { expected_pointer = pointer; calls = 0; }
void record_alias_pointer(void *pointer) {
    if (pointer != expected_pointer) abort();
    calls++;
}
int32_t alias_pointer_calls(void) { return calls; }
extern int32_t port__send(void *pointer, int32_t value);
int32_t native_alias_call(void *pointer) { return port__send(pointer, 17); }
extern int32_t legacy_alias_entry(void);
__attribute__((constructor)) static void verify_alias(void) {
    if (legacy_alias_entry() != 42) abort();
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target_libc_headers = true
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	assert c_source.contains('dev__port__send('), c_source
	program_c := os.join_path(root, 'program.c')
	os.write_file(program_c, c_source) or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration', program_c,
		native_c, '-o', program])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
