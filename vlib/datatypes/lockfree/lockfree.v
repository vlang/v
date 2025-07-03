module lockfree

// copy from vlib/sync/stdatomic/1.declarations.c.v, as we don't import `sync`
fn C.atomic_load_u32(voidptr) u32
fn C.atomic_store_u32(voidptr, u32)
fn C.atomic_compare_exchange_weak_u32(voidptr, voidptr, u32) bool
fn C.atomic_thread_fence(int)
fn C.cpu_relax()

fn C.ANNOTATE_RWLOCK_CREATE(voidptr)
fn C.ANNOTATE_RWLOCK_ACQUIRED(voidptr, int)
fn C.ANNOTATE_RWLOCK_RELEASED(voidptr, int)
fn C.ANNOTATE_RWLOCK_DESTROY(voidptr)

$if valgrind ? {
	#flag -I/usr/include/valgrind
	#include <valgrind/helgrind.h>
}

// Define cache line size to prevent false sharing between CPU cores
const cache_line_size = 64

// next_power_of_two calculates the smallest power of two >= n
@[inline]
fn next_power_of_two(n u32) u32 {
	if n == 0 {
		return 1
	}
	mut x := n - 1

	// Efficient bit manipulation to find next power of two
	x |= x >> 1
	x |= x >> 2
	x |= x >> 4
	x |= x >> 8
	x |= x >> 16
	return x + 1
}
