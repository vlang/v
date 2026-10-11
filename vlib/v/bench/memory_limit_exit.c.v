module bench

@[noreturn]
fn C._Exit(status int)

@[noreturn]
fn C._exit(status i32)

// exit_memory_limit stops all compiler threads before process-exit callbacks can
// release arenas that another thread is still using.
@[noreturn]
fn exit_memory_limit() {
	C.fflush(C.stderr)
	$if windows {
		// _exit is also available in the legacy MSVCRT used by bundled TCC.
		C._exit(1)
	} $else {
		C._Exit(1)
	}
}
