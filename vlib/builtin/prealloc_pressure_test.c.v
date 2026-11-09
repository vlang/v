// vtest build: linux || openbsd
// vtest vflags: -prealloc -gc none
module builtin

#include <sys/resource.h>
#include <sys/wait.h>
#include <unistd.h>
#include "@VEXEROOT/vlib/builtin/prealloc_pressure_test.h"

struct C.rlimit {
mut:
	rlim_cur uint
	rlim_max uint
}

fn C.getrlimit(resource int, limit &C.rlimit) int
fn C.setrlimit(resource int, limit &C.rlimit) int
fn C._exit(status int)
fn C.v_prealloc_pressure_has_shadow_memory() int

fn test_prealloc_reclaims_cached_blocks_before_reporting_out_of_memory() {
	$if prealloc {
		// Sanitizer shadow mappings already exceed the small data limit used
		// below; the child inherits them even when its live V heap is tiny.
		if C.v_prealloc_pressure_has_shadow_memory() != 0 {
			return
		}
		pid := C.fork()
		assert pid >= 0
		if pid == 0 {
			// Change only the child: Linux and OpenBSD count anonymous mappings
			// against RLIMIT_DATA, even when the mapped pages are not resident.
			mut limit := C.rlimit{}
			assert C.getrlimit(C.RLIMIT_DATA, &limit) == 0
			limit.rlim_cur = 160 * 1024 * 1024
			assert unsafe { C.setrlimit(C.RLIMIT_DATA, &limit) } == 0
			unsafe { prealloc_block_pool_free() }
			// Leave 64 MiB of reusable 256 KiB blocks. None can satisfy the
			// following 96 MiB request, but their storage is no longer live.
			mut scopes := unsafe { [256]voidptr{} }
			for i in 0 .. scopes.len {
				scopes[i] = unsafe { prealloc_scope_begin() }
				unsafe { prealloc_scope_leave(scopes[i]) }
			}
			for scope in scopes {
				unsafe { prealloc_scope_free_after(scope) }
			}
			data := unsafe { malloc(96 * 1024 * 1024) }
			// Touch only the endpoints: this tests address-space pressure without
			// requiring 96 MiB of physical memory from the test runner.
			unsafe {
				data[0] = 17
				data[96 * 1024 * 1024 - 1] = 23
			}
			assert unsafe { data[0] } == 17
			assert unsafe { data[96 * 1024 * 1024 - 1] } == 23
			C._exit(0)
		}
		mut status := 0
		assert C.waitpid(pid, &status, 0) == pid
		assert status == 0
	}
}
