module wasi

fn test_fd_sync_host() {
	assert fd_sync(1) == 0
}

fn test_random_get_host() {
	buf := []u8{len: 4}
	assert random_get(buf) == 0
}

fn test_args_sizes_host() {
	argc, size := args_sizes_get()
	assert argc == 0
	assert size == 0
}