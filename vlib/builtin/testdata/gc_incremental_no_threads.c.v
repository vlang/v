// Reports whether the collector runs in incremental mode. gc_handle_fork_test.v builds it with
// `-prod -gc boehm_incr -d no_gc_threads`: without GC threads the bundled `gc.c` must not get
// HANDLE_FORK, because on macOS that makes it refuse incremental mode, with a `GC Warning`.
fn C.GC_is_incremental_mode() i32

fn main() {
	mut objs := [][]int{}
	for i in 0 .. 20_000 {
		objs << []int{len: 4, init: i}
	}
	gc_collect()
	println('incremental=${C.GC_is_incremental_mode()} objects=${objs.len}')
}
