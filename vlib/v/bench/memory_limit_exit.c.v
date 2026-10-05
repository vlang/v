module bench

@[noreturn]
fn C._Exit(status int)

@[noreturn]
fn C._exit(status i32)

// exit_memory_limit terminates every worker before any process-exit cleanup can
// release arenas that other threads are still using.
@[noreturn]
fn exit_memory_limit() {
	C.fflush(C.stderr)
	$if windows {
		// Windows TCC exposes the immediate-exit CRT function as _exit.
		C._exit(1)
	} $else {
		C._Exit(1)
	}
}
