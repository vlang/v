module main

__global exit_probe_ready u32

fn busy_worker(_ u64) {
	vlibc_store_u32(&exit_probe_ready, 1)
	for {}
}

fn terminating_worker(mode u64) {
	if mode == 1 { exit(8) }
	panic('worker process panic probe')
}

fn main() {
	args := arguments()
	if args.len != 2 { exit(90) }
	_, code := vlibc_thread_spawn(busy_worker, 0)
	if code != .enoerror { exit(91) }
	for vlibc_load_u32(&exit_probe_ready) != 1 {
	}
	if args[1] == 'main-exit' { exit(7) }
	if args[1] == 'worker-exit' || args[1] == 'worker-panic' {
		_, other := vlibc_thread_spawn(terminating_worker, if args[1] == 'worker-exit' {
			u64(1)
		} else {
			u64(0)
		})
		if other != .enoerror { exit(92) }
		for {}
	}
	// The busy worker remains alive: returning must terminate the whole process.
}
