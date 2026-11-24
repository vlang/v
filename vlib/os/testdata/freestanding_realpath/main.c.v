import os
import arena

#include "@DIR/allocations.h"

fn C.realpath_alloc_begin()
fn C.realpath_alloc_live() int
fn C.realpath_alloc_end()

fn require(ok bool, message string) {
	if !ok {
		eprintln(message)
		exit(93)
	}
}

@[manualfree]
fn check_pair(input string, expected string) {
	for _ in 0 .. 8 {
		$if realpath_track_alloc ? {
			C.realpath_alloc_begin()
		}
		actual := os.real_path(input)
		if actual != expected {
			eprintln('real_path(${input}) returned ${actual}, expected ${expected}')
			exit(93)
		}
		require(actual.len == 0 || actual.str != input.str, 'real_path borrowed its input')
		$if realpath_track_alloc ? {
			require(C.realpath_alloc_live() == if actual.len == 0 { 0 } else { 1 }, 'real_path leaked scratch storage')
		}
		unsafe { actual.free() }
		$if realpath_track_alloc ? {
			require(C.realpath_alloc_live() == 0, 'real_path result did not own its allocation')
			C.realpath_alloc_end()
		}
	}
}

fn through_autofree(input string) string {
	return os.real_path(input)
}

fn main() {
	require(os.args.len >= 3 && os.args.len % 2 == 1, 'expected path/oracle pairs')
	for i := 1; i < os.args.len; i += 2 {
		check_pair(os.args[i], os.args[i + 1])
		require(through_autofree(os.args[i]) == os.args[i + 1], 'autofree result transfer')
	}
	// A NUL terminates the path at the kernel/C boundary, not the V string.
	check_pair('\x00ignored', '\x00ignored')
	check_pair(os.args[1] + '\x00ignored', os.args[2])
	mut scope := arena.new(chunk_size: 4096)
	missing := os.args[1] + '/missing'
	scope.push()
	scoped := os.real_path(os.args[1])
	require(scoped == os.args[2] && scope.is_current() && scope.owns(scoped.str),
		'arena-owned resolution')
	failed := os.real_path(missing)
	require(failed == missing && failed.str != missing.str && scope.owns(failed.str),
		'arena-owned failure clone')
	scope.pop()
	// Clone while the result remains alive, into the now-restored parent heap.
	saved := scoped.clone()
	saved_failure := failed.clone()
	scope.free()
	require(saved == os.args[2], 'result lost after leaving its allocation scope')
	require(saved_failure == missing, 'failure clone lost after leaving its allocation scope')
	println('freestanding real_path passed')
}
