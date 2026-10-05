module bench

@[noreturn]
fn C._Exit(status int)

// exit_memory_limit terminates every worker before any process-exit cleanup can
// release arenas that other threads are still using.
@[noreturn]
fn exit_memory_limit() {
	C.fflush(C.stderr)
	C._Exit(1)
}
