// Forks from a thread the collector does not know. With HANDLE_FORK the child's fork
// handler used to write through a missing thread table entry and crash while holding the
// allocator lock, then hang in the crash handler. `assert` is compiled out of `-prod`
// builds, so the verdict is the exit code: 0 ok, 2 child hung, 3 child killed by a signal.
#include "@VEXEROOT/vlib/builtin/testdata/gc_fork_unregistered_thread.h"

fn C.gc_fork_from_unregistered_thread() int

fn main() {
	// Allocate first, so the collector is initialized and has the main thread registered.
	mut keep := []string{}
	for i in 0 .. 1000 {
		keep << 'item ${i}'
	}
	verdict := C.gc_fork_from_unregistered_thread()
	if verdict != 0 {
		eprintln('fork from an unregistered thread: verdict ${verdict}')
		exit(verdict)
	}
	println('child ok, parent kept ${keep.len} strings')
}
