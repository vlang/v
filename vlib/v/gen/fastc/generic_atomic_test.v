module fastc

import os
import v.cmdexec

@[unsafe]
fn test_fastc_generic_atomic_operations_preserve_width_and_pointer_slots() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_generic_atomics_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	// These are real host atomic operations with the same raw-bit contract as
	// Vinix's width-specific CAS assembly entry points.
	os.write_file(os.join_path(root, 'atomic_host.h'), r'#include <stdint.h>
static uint8_t vinix_cas8(void *address, uint8_t expected, uint8_t desired) {
    __atomic_compare_exchange_n((uint8_t *)address, &expected, desired, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    return expected;
}
static uint32_t vinix_cas32(void *address, uint32_t expected, uint32_t desired) {
    __atomic_compare_exchange_n((uint32_t *)address, &expected, desired, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    return expected;
}
static uint64_t vinix_cas64(void *address, uint64_t expected, uint64_t desired) {
    __atomic_compare_exchange_n((uint64_t *)address, &expected, desired, 0, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST);
    return expected;
}
static uint32_t vinix_ldset32(void *address, uint32_t bits) {
    return __atomic_fetch_or((uint32_t *)address, bits, __ATOMIC_SEQ_CST);
}
static uint64_t vinix_ldset64(void *address, uint64_t bits) {
    return __atomic_fetch_or((uint64_t *)address, bits, __ATOMIC_SEQ_CST);
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.c.v'), r'module main
#include "atomic_host.h"
fn C.vinix_cas8(voidptr, u8, u8) u8
fn C.vinix_cas32(voidptr, u32, u32) u32
fn C.vinix_cas64(voidptr, u64, u64) u64
fn C.vinix_ldset32(voidptr, u32) u32
fn C.vinix_ldset64(voidptr, u64) u64

type Counter = u32

struct Thread { value u64; guard u64 }
struct State {
mut:
	flag bool
	byte u8 = 99
	number u32
	integer int
	pending_signals u64
	slot &Thread = unsafe { nil }
	guard u64 = 0x123456789abcdef0
}

// Keep the kernel width selection and bit-copying. On x86 use the
// Vinix cmpxchg constraints; ARM uses the equivalent host atomics.
fn cas[T](mut here T, _ifthis T, writethis T) bool {
	if sizeof(T) == 1 {
		mut expected := u8(0)
		mut desired := u8(0)
		unsafe { C.memcpy(&expected, &_ifthis, 1); C.memcpy(&desired, &writethis, 1) }
		$if amd64 {
			mut ret := false
			mut target := unsafe { &u8(here) }
			asm volatile amd64 {
				lock cmpxchg target, desired
				; +a (expected)
				  +m (*target) as target
				  =@ccz (ret)
				; r (desired)
				; memory
			}
			return ret
		} $else { return C.vinix_cas8(voidptr(here), expected, desired) == expected }
	}
	if sizeof(T) == 4 {
		mut expected := u32(0)
		mut desired := u32(0)
		unsafe { C.memcpy(&expected, &_ifthis, 4); C.memcpy(&desired, &writethis, 4) }
		$if amd64 {
			mut ret := false
			mut target := unsafe { &u32(here) }
			asm volatile amd64 {
				lock cmpxchg target, desired
				; +a (expected)
				  +m (*target) as target
				  =@ccz (ret)
				; r (desired)
				; memory
			}
			return ret
		} $else { return C.vinix_cas32(voidptr(here), expected, desired) == expected }
	}
	if sizeof(T) == 8 {
		mut expected := u64(0)
		mut desired := u64(0)
		unsafe { C.memcpy(&expected, &_ifthis, 8); C.memcpy(&desired, &writethis, 8) }
		$if amd64 {
			mut ret := false
			mut target := unsafe { &u64(here) }
			asm volatile amd64 {
				lock cmpxchg target, desired
				; +a (expected)
				  +m (*target) as target
				  =@ccz (ret)
				; r (desired)
				; memory
			}
			return ret
		} $else { return C.vinix_cas64(voidptr(here), expected, desired) == expected }
	}
	panic("unsupported atomic width")
}

fn selected_width[T](value T) int {
	$if T is bool { return 1 }
	$else $if T is u32 || T is i32 { return 4 }
	$else $if T is u64 || T is i64 || T is uint || T is int { return 8 }
	$else { return sizeof(T) }
}

fn bts[T](mut var T, bit u8) bool {
	$if amd64 {
		mut ret := false
		unsafe {
			asm volatile amd64 {
				lock bts var, bit
				; +m (*var) as var
				  =@ccc (ret)
				; r (u16(bit)) as bit
				; memory
			}
		}
		return ret
	} $else {
		mask := unsafe { T(1) << bit }
		$if T is u32 || T is i32 || T is int {
			old := C.vinix_ldset32(voidptr(var), u32(mask))
			return old & u32(mask) != 0
		} $else $if T is u64 || T is i64 || T is uint || T is int || T is voidptr {
			old := C.vinix_ldset64(voidptr(var), u64(mask))
			return old & u64(mask) != 0
		} $else { $compile_error("unsupported bit-set width") }
		return false
	}
}

fn optional_reference(value &u64) ?&u64 { return value }

fn pointer_depth[T](value T) int {
	$if T is &Thread { return 1 }
	$else $if T is &&Thread { return 2 }
	$else { return 0 }
}

fn forwarded_cas[T](mut slot T, expected T, desired T) bool {
	return cas(mut slot, expected, desired)
}

fn identity[T](value T) T { return value }

fn forwarded_identity[T](mut value T) T { return identity(value) }

fn read[T](value &T) T { return *value }

fn forwarded_read[T](mut value T) T { return read(&value) }

fn read_pointer_value[T](mut value T) u64 {
	copied := read(value)
	assert sizeof(copied) == sizeof(Thread)
	return copied.value
}

fn append_values(mut values []string, remaining int) {
	if remaining == 0 { return }
	values << "item"
	append_values(mut values, remaining - 1)
}

fn fill_values(mut values map[string]string, remaining int) {
	if remaining == 0 { return }
	values["item"] = "filled"
	fill_values(mut values, remaining - 1)
}

fn values_len(values []string) int { return values.len }

fn append_and_read(mut values []string) int {
	append_values(mut values, 3)
	return values_len(values)
}

fn main() {
	mut state := State{}
	assert cas[bool](mut &state.flag, false, true)
	assert state.flag
	assert state.byte == 99
	assert cas[u32](mut &state.number, u32(0), u32(513))
	assert state.number == 513
	wide := int(1) << 40
	assert cas[int](mut &state.integer, 0, wide)
	assert state.integer == wide
	first := &Thread{value: 17}
	second := &Thread{value: 18}
	assert cas[&Thread](mut &state.slot, unsafe { nil }, first)
	assert uint(state.slot) == uint(first)
	assert !cas(mut &state.slot, unsafe { nil }, second)
	assert uint(state.slot) == uint(first)
	assert cas(mut &state.slot, first, second)
	assert uint(state.slot) == uint(second)
	assert state.guard == 0x123456789abcdef0
	mut pointer := first
	// Explicit T selects pointer-slot mutation; an inferred bare mut pointer
	// refers to its pointee under the ordinary V generic inference rules.
	assert cas[&Thread](mut pointer, first, second)
	assert uint(pointer) == uint(second)
	assert forwarded_cas[&Thread](mut pointer, second, first)
	assert uint(pointer) == uint(first)
	mut other := second
	mut slot := &pointer
	assert cas[&&Thread](mut &slot, &pointer, &other)
	assert uint(slot) == uint(&other)
	assert uint(identity(second)) == uint(second)
	assert uint(forwarded_identity[&Thread](mut pointer)) == uint(pointer)
	copied_thread := forwarded_identity(mut pointer)
	assert sizeof(copied_thread) == sizeof(Thread)
	assert copied_thread.value == 17
	assert copied_thread.guard == 0
	assert forwarded_identity[int](mut state.integer) == wide
	assert uint(forwarded_read[&Thread](mut pointer)) == uint(pointer)
	assert forwarded_read[int](mut state.integer) == wide
	assert read_pointer_value[&Thread](mut pointer) == 17
	assert first.value == 17
	assert second.value == 18
	mut counter := Counter(3)
	assert cas[Counter](mut &counter, Counter(3), Counter(5))
	assert counter == Counter(5)
	mut bits := u64(0)
	mut entry := optional_reference(&bits) or { panic("missing entry") }
	assert !bts(mut entry, u8(10))
	assert bits == u64(1) << 10
	assert bts(mut entry, u8(10))
	mut copied_entry := entry
	assert !bts(mut copied_entry, u8(55))
	assert bits == (u64(1) << 10) | (u64(1) << 55)
	assert uint(entry) == uint(&bits)
	assert uint(copied_entry) == uint(&bits)
	assert !bts(mut &state.pending_signals, u8(55))
	assert state.pending_signals == u64(1) << 55
	assert bts(mut &state.pending_signals, u8(55))
	assert state.guard == 0x123456789abcdef0
	mut number_entry := &state.number
	assert !bts(mut number_entry, u8(20))
	assert state.number == 513 | (u32(1) << 20)
	assert selected_width(false) == 1
	assert selected_width(u32(1)) == 4
	assert selected_width(wide) == 8
	assert selected_width(counter) == 4
	assert selected_width(&counter) == sizeof(voidptr)
	assert selected_width(second) == sizeof(voidptr)
	assert pointer_depth(second) == 1
	assert pointer_depth(&second) == 2
	mut values := []string{}
	assert append_and_read(mut values) == 3
	assert values.len == 3
	assert values[2] == "item"
	mut mapped := map[string]string{}
	fill_values(mut mapped, 3)
	assert mapped["item"] == "filled"
	println("atomic generics work")
}
') or { panic(err) }
	vroot := os.real_path(os.join_path(@DIR, '../../../..'))
	for arch in ['amd64', 'arm64'] {
		c_path := os.join_path(root, 'atomic_${arch}.c')
		build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache',
			'-target-libc-headers', '-os', 'vinix', '-arch', arch, '-gc', 'none', '-manualfree',
			'-nofloat', '-d', 'no_backtrace', '-exclude', '@vlib/builtin/backtraces_nix.c.v', '-o',
			c_path, root], vroot, {
			'VEXE':                   @VEXE
			'VFLAGS':                 ''
			'VOSARGS':                ''
			'V_MACOS_V3_NO_FALLBACK': '1'
		})
		assert build.exit_code == 0, '${arch}: ${build.output}'
		mut flags := ['-w', '-O2', '-fno-strict-aliasing', '-D__vinix__']
		$if macos {
			flags << ['-target',
				if arch == 'amd64' { 'x86_64-apple-darwin' } else { 'arm64-apple-darwin' }]
		}
		mut can_run := false
		$if amd64 {
			can_run = arch == 'amd64'
		}
		$if arm64 {
			can_run = arch == 'arm64'
		}
		$if macos && arm64 {
			if arch == 'amd64' {
				can_run = cmdexec.run('arch', ['-x86_64', '/usr/bin/true']).exit_code == 0
			}
		}
		if !can_run {
			$if !macos {
				continue
			}
			object := os.join_path(root, '${arch}.o')
			flags << ['-c', c_path, '-o', object]
			compiled := cmdexec.run('clang', flags)
			assert compiled.exit_code == 0, '${arch}: ${compiled.output}'
			continue
		}
		program := os.join_path(root, 'atomic_${arch}')
		flags << [c_path, '-o', program]
		compiled := cmdexec.run('clang', flags)
		assert compiled.exit_code == 0, '${arch}: ${compiled.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, '${arch}: ${run.output}'
		assert run.output.trim_space() == 'atomic generics work', run.output
	}
}
