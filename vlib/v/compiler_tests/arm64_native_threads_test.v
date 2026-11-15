module main

import os
import v.cmdexec

// A compiler built with `-b arm64` runs its worker pool on real threads. These
// programs cover what that depends on: the pool itself, allocators that keep their
// current block per thread, and pointer fields passed to `voidptr` parameters.

fn run_arm64_threads(name string, source string, flags []string) string {
	root := os.join_path(os.vtmp_dir(), 'arm64_native_threads_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	path := os.join_path(root, 'main.v')
	os.write_file(path, source) or { panic(err) }
	output := os.join_path(root, 'main')
	mut build := ['-b', 'arm64', '-w', '-nocache']
	build << flags
	build << ['-o', output, path]
	compiled := cmdexec.run(@VEXE, build)
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(output, []string{})
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_worker_pool_runs_on_threads() {
	$if macos && arm64 {
		output := run_arm64_threads('pool', "import v.workers

fn C.alarm(u32) u32
fn C.atomic_fetch_add_u32(voidptr, u32) u32

struct Shared {
mut:
	count u32
}

fn job(arg voidptr) voidptr {
	mut s := unsafe { &Shared(arg) }
	C.atomic_fetch_add_u32(&s.count, 1)
	return unsafe { nil }
}

fn main() {
	C.alarm(20)
	mut pool := workers.new(4)
	size := pool.size()
	mut s := &Shared{}
	for _ in 0 .. 50 {
		mut tasks := []workers.Task{}
		for i in 0 .. 8 {
			tasks << workers.Task{
				run:        job
				arg:        voidptr(s)
				force_sync: i == 0
			}
		}
		pool.run(tasks)
	}
	pool.close()
	println('size \${size} count \${s.count}')
}
", [])
		assert output == 'size 4 count 400'
	}
}

// Under -prealloc every thread bump-allocates from its own block. With one block
// shared between threads the strings below overlap and their text changes.
fn test_prealloc_blocks_are_per_thread() {
	$if macos && arm64 {
		output := run_arm64_threads('prealloc', "fn C.alarm(u32) u32

const rounds = 20000

fn fill(seed int) int {
	mut bad := 0
	mut kept := []string{cap: rounds}
	for i in 0 .. rounds {
		kept << 'thread \${seed} text \${i}'.clone()
	}
	for i, text in kept {
		if text != 'thread \${seed} text \${i}' {
			bad++
		}
	}
	return bad
}

fn main() {
	C.alarm(30)
	mut threads := []thread int{}
	for seed in 0 .. 8 {
		threads << spawn fill(seed)
	}
	mut bad := 0
	for count in threads.wait() {
		bad += count
	}
	println('bad \${bad}')
}
", ['-prealloc'])
		assert output == 'bad 0'
	}
}

// `free(table.ptrs)` releases the table, not its first entry.
fn test_pointer_to_pointer_field_goes_by_value_to_voidptr() {
	$if macos && arm64 {
		output := run_arm64_threads('voidptr_field', "struct Table {
mut:
	ptrs &voidptr = unsafe { nil }
}

fn address_of(p voidptr) u64 {
	return u64(p)
}

fn main() {
	mut text := 'x'
	mut table := Table{
		ptrs: unsafe { &voidptr(malloc(64)) }
	}
	unsafe {
		table.ptrs[0] = voidptr(text.str)
	}
	println(address_of(table.ptrs) == u64(voidptr(table.ptrs)))
	unsafe { free(table.ptrs) }
	println('freed')
}
", [])
		assert output == 'true\nfreed'
	}
}
