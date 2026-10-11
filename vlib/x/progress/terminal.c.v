module progress

fn C._exit(status int)

// exit_immediately ends the process without running at-exit callbacks or
// flushing anything. It is async-signal-safe, unlike exit(), so it is what a
// signal handler should use.
fn exit_immediately(status int) {
	C._exit(status)
}
