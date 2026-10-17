module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_generic_mutable_interface_fields_retain_concrete_types() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_interface_generic_${os.getpid()}')
	atomic_dir := os.join_path(root, 'atomics')
	os.mkdir_all(atomic_dir) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(atomic_dir, 'atomic_host.h'), r'#include <stdint.h>
static uint64_t vinix_add64(void *address, uint64_t value) {
    return __atomic_fetch_add((uint64_t *)address, value, __ATOMIC_SEQ_CST);
}
static uint32_t vinix_add32(void *address, uint32_t value) {
    return __atomic_fetch_add((uint32_t *)address, value, __ATOMIC_SEQ_CST);
}
static uint32_t vinix_cas32(void *address, uint32_t expected, uint32_t desired) {
    __atomic_compare_exchange_n((uint32_t *)address, &expected, desired, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    return expected;
}
static uint64_t vinix_cas64(void *address, uint64_t expected, uint64_t desired) {
    __atomic_compare_exchange_n((uint64_t *)address, &expected, desired, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    return expected;
}
') or { panic(err) }
	os.write_file(os.join_path(atomic_dir, 'atomics.c.v'), r'module atomics
#include "@DIR/atomic_host.h"
fn C.vinix_add64(voidptr, u64) u64
fn C.vinix_cas64(voidptr, u64, u64) u64
fn C.vinix_add32(voidptr, u32) u32
fn C.vinix_cas32(voidptr, u32, u32) u32
pub fn inc[T](mut value T) T {
	if sizeof(T) == 4 { return unsafe { T(C.vinix_add32(voidptr(value), 1)) } }
	return unsafe { T(C.vinix_add64(voidptr(value), 1)) }
}
pub fn cas[T](mut value T, expected T, desired T) bool {
	if sizeof(T) == 4 {
		return unsafe { C.vinix_cas32(voidptr(value), u32(expected), u32(desired)) == u32(expected) }
	}
	return unsafe { C.vinix_cas64(voidptr(value), u64(expected), u64(desired)) == u64(expected) }
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import atomics
struct Stat { mut: nlink u64 }
interface Resource {
mut:
	refcount int
	stat Stat
}
struct Item {
mut:
	refcount int
	stat Stat
	guard u64 = 0x123456789abcdef0
}
struct Holder { resource Resource }
fn main() {
	item := &Item{refcount: 7, stat: Stat{nlink: u64(1) << 40}}
	holder := Holder{resource: item}
	assert atomics.inc(mut &holder.resource.refcount) == 7, "refcount increment result"
	assert atomics.inc(mut &holder.resource.stat.nlink) == u64(1) << 40, "nlink increment result"
	assert holder.resource.refcount == 8, "refcount changed"
	assert holder.resource.stat.nlink == (u64(1) << 40) + 1, "nlink changed"
	before := holder.resource.refcount
	assert atomics.cas(mut &holder.resource.refcount, before, before + 2), "CAS true"
	assert !atomics.cas(mut &holder.resource.refcount, before, before + 3), "CAS false"
	assert holder.resource.refcount == before + 2, "CAS stored"
	assert item.refcount == before + 2, "original refcount changed"
	assert item.stat.nlink == (u64(1) << 40) + 1, "original nlink changed"
	assert item.guard == 0x123456789abcdef0, "guard preserved"
	// The FastC full-runtime C representation widens int storage; the checked
	// ordinary backend uses a 32-bit language int. Verify this ABI separately.
	$if fastc_interface_width ? {
		holder.resource.refcount = int(1) << 40
		assert atomics.inc(mut &holder.resource.refcount) == int(1) << 40, "wide refcount result"
		assert holder.resource.refcount == (int(1) << 40) + 1, "wide refcount stored"
	}
	println("generic interface atomics work")
}
') or { panic(err) }
	ordinary := os.join_path(root, 'ordinary')
	ordinary_build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'c', '-nocache', '-gc', 'none',
		'-o', ordinary, root], @VEXEROOT, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert ordinary_build.exit_code == 0, ordinary_build.output
	ordinary_run := cmdexec.run(ordinary, [])
	assert ordinary_run.exit_code == 0, ordinary_run.output
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target_libc_headers = true
	prefs.target = pref.target_from('vinix', pref.host_arch()) or { panic(err) }
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend', 'fastc_interface_width']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	generated := generate_files([root], prefs) or { panic(err) }
	assert generated.contains('atomics__inc_mono_int(')
	assert generated.contains('atomics__inc_mono_u64(')
	assert generated.contains('atomics__cas_mono_int(')
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'fastc')
	os.write_file(c_path, generated) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-fno-strict-aliasing', '-D__vinix__', '-o',
		program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output == ordinary_run.output, run.output
	assert run.output == 'generic interface atomics work\n', run.output
}
