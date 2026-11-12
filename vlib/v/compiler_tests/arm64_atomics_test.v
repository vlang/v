module main

import os
import v.cmdexec

// Programs built with `-b arm64` must get real atomic operations: the counters
// below lose updates, and the spinlocks stop excluding, when an atomic operation
// is a load and a store. They run with and without the optimizer, which must keep
// these operations and must not move memory accesses across them.

const atomics_program = "import sync.stdatomic as _

fn C.alarm(u32) u32
fn C.v_prealloc_atomic_load_i32(voidptr) int
fn C.v_prealloc_atomic_add_i32(voidptr, int) int
fn C.v_prealloc_atomic_store_i32(voidptr, int) int
fn C.v_prealloc_atomic_cas_i32(voidptr, int, int) int

const workers = 8
const rounds = 100000

struct Shared {
mut:
	added        u32
	removed      u32 = u32(workers * rounds)
	narrow_byte  u8
	narrow_half  u16
	wide         u64
	cas_lock     u32
	cas_guarded  u64
	swap_lock    u32
	swap_guarded u64
	gate         u32
	seen         u32
}

fn count(mut s Shared) {
	for _ in 0 .. rounds {
		C.atomic_fetch_add_u32(&s.added, 1)
		C.atomic_fetch_sub_u32(&s.removed, 1)
		C.atomic_fetch_add_byte(&s.narrow_byte, 1)
		C.atomic_fetch_add_u16(&s.narrow_half, 1)
		C.atomic_fetch_add_u64(&s.wide, 1)
		for {
			mut expected := u32(0)
			if C.atomic_compare_exchange_weak_u32(&s.cas_lock, &expected, 1) {
				break
			}
		}
		s.cas_guarded++
		C.atomic_store_u32(&s.cas_lock, 0)
		for C.atomic_exchange_u32(&s.swap_lock, 1) != 0 {
		}
		s.swap_guarded++
		C.atomic_store_u32(&s.swap_lock, 0)
	}
}

fn wait_for_gate(mut s Shared) {
	for C.atomic_load_u32(&s.gate) == 0 {
	}
	C.atomic_store_u32(&s.seen, 1)
}

fn main() {
	C.alarm(30)
	mut s := &Shared{}
	mut counters := []thread{}
	for _ in 0 .. workers {
		counters << spawn count(mut s)
	}
	counters.wait()
	total := u64(workers) * u64(rounds)
	println('added \${u64(s.added) == total} removed \${s.removed}')
	println('byte \${u64(s.narrow_byte) == total % 256} half \${u64(s.narrow_half) == total % 65536} wide \${s.wide == total}')
	println('cas \${s.cas_guarded == total} swap \${s.swap_guarded == total}')

	waiter := spawn wait_for_gate(mut s)
	C.atomic_thread_fence(5)
	C.atomic_store_u32(&s.gate, 1)
	waiter.wait()
	println('seen \${C.atomic_load_u32(&s.seen)}')

	mut word := u32(7)
	mut expected := u32(5)
	failed := C.atomic_compare_exchange_strong_u32(&word, &expected, 9)
	println('failed \${failed} expected \${expected} word \${word}')
	replaced := C.atomic_compare_exchange_strong_u32(&word, &expected, 9)
	println('replaced \${replaced} word \${word}')

	mut bytes := [u8(1), 2, 3, 4, 5, 6, 7, 8]!
	C.atomic_store_byte(&bytes[2], 200)
	old_byte := C.atomic_fetch_add_byte(&bytes[2], 100)
	println('old \${old_byte} now \${C.atomic_load_byte(&bytes[2])} around \${bytes[1]} \${bytes[3]}')
	mut halves := [u16(11), 22, 33, 44]!
	C.atomic_store_u16(&halves[1], 60000)
	println('half \${halves[1]} around \${halves[0]} \${halves[2]}')

	mut first := 0
	mut second := 0
	mut slot := voidptr(&first)
	mut wanted := voidptr(&first)
	C.atomic_compare_exchange_strong_ptr(&slot, &wanted, C.ptrdiff_t(voidptr(&second)))
	println('ptr \${C.atomic_load_ptr(&slot) == voidptr(&second)}')
	println('swap \${C.atomic_exchange_ptr(&slot, voidptr(&first)) == voidptr(&second)} \${slot == voidptr(&first)}')

	mut ints := [i32(-1), 77]!
	println('int \${C.v_prealloc_atomic_load_i32(&ints[0])} add \${C.v_prealloc_atomic_add_i32(&ints[0], 3)}')
	println('store \${C.v_prealloc_atomic_store_i32(&ints[0], -8)} cas \${C.v_prealloc_atomic_cas_i32(&ints[0], -8, 4)} \${C.v_prealloc_atomic_cas_i32(&ints[0], 0, 5)}')
	println('ints \${ints[0]} \${ints[1]}')
}
"

const atomics_output = 'added true removed 0
byte true half true wide true
cas true swap true
seen 1
failed false expected 7 word 7
replaced true word 9
old 200 now 44 around 2 4
half 60000 around 11 33
ptr true
swap true true
int -1 add 2
store 2 cas 1 0
ints 4 77'

fn test_atomic_operations_are_atomic_on_arm64() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_atomics_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, atomics_program) or { panic(err) }
		for index, flags in [['-b', 'arm64'], ['-prod', '-b', 'arm64']] {
			output := os.join_path(root, 'main_${index}')
			mut build := flags.clone()
			build << ['-w', '-nocache', '-o', output, source]
			compiled := cmdexec.run(@VEXE, build)
			assert compiled.exit_code == 0, compiled.output
			run := cmdexec.run(output, []string{})
			assert run.exit_code == 0, run.output
			assert run.output.trim_space() == atomics_output
		}
	}
}
