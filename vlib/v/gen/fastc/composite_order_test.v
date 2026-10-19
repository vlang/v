module fastc

import os
import v.cmdexec
import v.flat
import v.pref

fn test_fastc_composite_order_moves_complete_inline_and_attributed_definitions() {
	$if !macos && !linux {
		return
	}
	definitions := [
		r'struct Owner {
	Mode mode;
	Permission permission;
	Packet packet;
	CacheLine cache;
};',
		r'struct Mode {
	Backend backend;
};',
		r'struct Permission {
	PermissionBackend backend;
};',
		'struct Backend { void *_object; unsigned long _typ; };',
		'struct PermissionBackend { void *_object; unsigned long _typ; };',
		r'struct __attribute__((packed)) Packet {
	unsigned char tag;
	unsigned int value;
};',
		r'struct CacheLine {
	unsigned char value;
} __attribute__((aligned(64)));',
		r'struct Tail {
	union { int value; unsigned char bytes[4]; } item;
	/* An ignored } must not close this definition. */
};',
	]
	fields := {
		'Owner':      flat.TypeTextBindings.from_names({
			'mode':       'Mode'
			'permission': 'Permission'
			'packet':     'Packet'
			'cache':      'CacheLine'
		})
		'Mode':       flat.TypeTextBindings.from_names({
			'backend': 'Backend'
		})
		'Permission': flat.TypeTextBindings.from_names({
			'backend': 'PermissionBackend'
		})
	}
	mut composites := map[string]bool{}
	for name in ['Owner', 'Mode', 'Permission', 'Backend', 'PermissionBackend', 'Packet', 'CacheLine',
		'Tail'] {
		composites[name] = true
	}
	ordered := fastc_order_c_composite_definitions(definitions.join('\n\n') + '\n', fields,
		composites, flat.TypeTextBindings{})
	for definition in definitions {
		assert ordered.count(definition) == 1, ordered
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_composite_order_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, r'#include <stddef.h>
#include <stdlib.h>
typedef struct Owner Owner;
typedef struct Mode Mode;
typedef struct Permission Permission;
typedef struct Backend Backend;
typedef struct PermissionBackend PermissionBackend;
typedef struct Packet Packet;
typedef struct CacheLine CacheLine;
typedef struct Tail Tail;
' + ordered + r'
int main(void) {
	int payload = 42;
	Owner owner = {0};
	owner.mode.backend._object = &payload;
	owner.permission.backend._object = &payload;
	owner.packet.value = 42;
	owner.cache.value = 7;
	if (*(int *)owner.mode.backend._object != 42) abort();
	if (*(int *)owner.permission.backend._object != 42) abort();
	if (owner.packet.value != 42 || owner.cache.value != 7) abort();
	if (sizeof(Packet) != 5 || offsetof(Packet, value) != 1) abort();
	if (_Alignof(CacheLine) != 64 || sizeof(CacheLine) != 64) abort();
	return 0;
}
') or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}

fn test_fastc_generated_interface_and_packed_struct_dependencies_compile() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_generated_composite_order_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Owner {
	mode Mode
	permission Permission
	packet Packet
	cache CacheLine
}
struct Mode { backend Backend }
struct Permission { backend PermissionBackend }
interface Backend { read() int }
interface PermissionBackend { allowed() bool }
@[packed]
struct Packet { tag u8 value u32 }
@[aligned: 64]
struct CacheLine { value u8 }
struct Tail { value int }
struct Value { value int }
fn (value Value) read() int { return value.value }
fn (value Value) allowed() bool { return value.value == 42 }
fn main() {
	owner := Owner{
		mode: Mode{ backend: Value{ value: 42 } }
		permission: Permission{ backend: Value{ value: 42 } }
		packet: Packet{ tag: 7, value: 42 }
		cache: CacheLine{ value: 9 }
	}
	assert owner.mode.backend.read() == 42
	assert owner.permission.backend.allowed()
	assert owner.packet.value == 42
	assert owner.cache.value == 9
	assert sizeof(Packet) == 5
	assert sizeof(CacheLine) == 64
	assert Tail{ value: 11 }.value == 11
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
