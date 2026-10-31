module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_volatile_locals_preserve_scalar_and_mmio_pointer_qualifiers() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_volatile_local_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'qualifiers.h'), r'#define mmio_is_volatile(value) __builtin_types_compatible_p(__typeof__(value), volatile __typeof__(*(value))*)
#define scalar_is_volatile(value) mmio_is_volatile(value)
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
#flag -I @DIR
#include "qualifiers.h"
struct Registers {
mut:
	status u32
	command u32
}
fn C.mmio_is_volatile(value &Registers) bool
fn C.scalar_is_volatile(value &i64) bool
fn main() {
	plain := &Registers{ status: 3 }
	mut volatile regs := plain
	assert C.mmio_is_volatile(regs)
	assert !C.mmio_is_volatile(plain)
	regs.status = 42
	regs.command |= 8
	assert plain.status == 42 && plain.command == 8
	mut volatile word := i64(1) << 40
	assert C.scalar_is_volatile(&word)
	word += 2
	assert word == (i64(1) << 40) + 2
	ordinary := i64(7)
	assert !C.scalar_is_volatile(&ordinary)
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-I', root, '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
