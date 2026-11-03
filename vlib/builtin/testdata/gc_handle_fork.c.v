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

fn main() {
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
