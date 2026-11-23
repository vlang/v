module main

import os
import runtime
import time

// Forks from a program that has another thread allocating, then allocates and collects in the
// child. Without HANDLE_FORK the child's collector still has the parent's thread in its thread
// table, tries to stop it, and aborts. `assert` is compiled out of `-prod` builds, so failures
// are reported through the exit code: 0 ok, 2 child hung, 3 child killed by a signal.
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>

fn C.fork() int
fn C.waitpid(pid int, status &int, options int) int
fn C.kill(pid int, sig int) int

// allocator keeps the collector busy in the parent for as long as the program runs,
// so that the fork usually happens while some thread is inside an allocation.
fn allocator() {
	mut keep := [][]int{cap: 64}
	for {
		keep << []int{len: 16}
		if keep.len >= 64 {
			keep = [][]int{cap: 64}
		}
		time.sleep(20 * time.microsecond)
	}
}

fn child_allocates() {
	mut objs := [][]int{}
	for i in 0 .. 300_000 {
		objs << []int{len: 4, init: i}
	}
	gc_collect()
	println('child ok: ${objs.len} objects')
}

fn probe_fork() {
	spawn allocator()
	time.sleep(30 * time.millisecond)
	pid := C.fork()
	if pid < 0 {
		eprintln('fork failed')
		exit(4)
	}
	if pid == 0 {
		child_allocates()
		exit(0)
	}
	mut status := 0
	for _ in 0 .. 1000 {
		if C.waitpid(pid, &status, C.WNOHANG) == pid {
			if status & 0x7f != 0 {
				eprintln('child killed by signal ${status & 0x7f}')
				exit(3)
			}
			exit((status >> 8) & 0xff)
		}
		time.sleep(10 * time.millisecond)
	}
	C.kill(pid, 9)
	C.waitpid(pid, &status, 0)
	eprintln('child hung')
	exit(2)
}

@[c_extern]
fn C.GC_expand_hp(uint) int

@[c_extern]
fn C.GC_set_force_unmap_on_gcollect(int)

fn probe_unmap() {
	// Grow free heap space without retaining an allocated object's address on the stack.
	if C.GC_expand_hp(uint(32 * 1024 * 1024)) == 0 {
		panic('could not expand GC heap')
	}
	C.GC_set_force_unmap_on_gcollect(1)
	for _ in 0 .. 3 {
		gc_collect()
	}
	if gc_heap_usage().unmapped_bytes == 0 {
		panic('production bundled GC did not unmap free heap pages')
	}
}

@[c_extern]
fn C.GC_get_parallel() int

// `assert` is compiled out of `-prod` builds, so failures exit explicitly.
fn fail(msg string) {
	eprintln(msg)
	exit(1)
}

fn probe_parallel_mark() {
	parallel := C.GC_get_parallel()
	cpus := runtime.nr_cpus()
	$if gc_parallel_mark ? {
		if cpus > 1 && parallel == 0 {
			fail('-d gc_parallel_mark should enable Boehm parallel marking (cpus=${cpus})')
		}
	} $else {
		if parallel != 0 {
			fail('the bundled gc.c should mark with one thread by default, got ${parallel}')
		}
	}
	// the collector must still work with marker threads running
	mut keep := [][]int{}
	for i in 0 .. 2000 {
		keep << []int{len: 1000, init: i}
	}
	gc_collect()
	mut sum := 0
	for a in keep {
		sum += a[0]
	}
	if sum != 1999 * 2000 / 2 {
		fail('live data was corrupted by a collection: ${sum}')
	}
	println('ok parallel=${parallel}')
}

fn main() {
	if os.args.len != 2 {
		panic('expected a GC probe name')
	}
	match os.args[1] {
		'fork' { probe_fork() }
		'unmap' { probe_unmap() }
		'parallel-mark' { probe_parallel_mark() }
		else { panic('unknown GC probe: ${os.args[1]}') }
	}
}
