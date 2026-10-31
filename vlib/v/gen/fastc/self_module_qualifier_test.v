module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_self_module_qualifiers_preserve_explicit_aliases_and_globals() {
	mut prefs := pref.new_preferences()
	prefs.target_libc_headers = true
	prefs.enable_globals = true
	sources := [FastcSourceFile{
		path:   'allocator.v'
		header: FastcSourceHeader{
			module_name: 'project.allocator'
			imports:     {
				'allocator': 'other.allocator'
			}
		}
	}, FastcSourceFile{
		path:   'storage.v'
		source: 'module storage\n__global storage int\n'
		header: FastcSourceHeader{
			module_name:             'storage'
			has_global_declarations: true
		}
	}, FastcSourceFile{
		path:   'memory.v'
		header: FastcSourceHeader{ module_name: 'project.memory' }
	}]
	qualified := fastc_sources_with_self_module_qualifiers(sources, prefs) or { panic(err) }
	assert qualified.len == sources.len
	assert qualified[0].header.imports['allocator'] == 'other.allocator'
	assert 'storage' !in qualified[1].header.imports
	assert qualified[2].header.imports['memory'] == 'project.memory'
	assert sources[2].header.imports.len == 0
}

fn test_fastc_self_module_calls_and_shadowed_values_execute() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_self_module_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'allocator')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'storage')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import allocator
import storage
fn main() {}
') or { panic(err) }
	os.write_file(os.join_path(root, 'allocator', 'allocator.v'), r'module allocator
struct Payload { value i32 }
fn deep() i32 { return 35 }
fn produce() &Payload { return &Payload{value: allocator.deep()} }
fn forward() &Payload { return allocator.produce() }
@[export: "native_self_module_entry"]
pub fn entry() i32 {
    produced := allocator.forward()
    allocator := Payload{value: 7}
    return produced.value + allocator.value
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'storage', 'storage.v'), r'module storage
struct Payload { value i32 }
__global storage = Payload{value: 42}
@[export: "native_global_shadow_entry"]
pub fn entry() i32 { return storage.value }
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target_libc_headers = true
	prefs.enable_globals = true
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	program_c := os.join_path(root, 'program.c')
	native_c := os.join_path(root, 'native.c')
	os.write_file(program_c, c_source) or { panic(err) }
	os.write_file(native_c, r'#include <stdint.h>
#include <stdlib.h>
extern int32_t native_self_module_entry(void);
extern int32_t native_global_shadow_entry(void);
extern void _vinit(int argc, void *argv);
__attribute__((constructor)) static void verify_entries(void) {
    _vinit(0, NULL);
    if (native_self_module_entry() != 42 || native_global_shadow_entry() != 42) abort();
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration', program_c,
		native_c, '-o', program])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
