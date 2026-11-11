module redis

$if !windows {
	fn test_select_fd_is_supported_checks_fd_set_bounds() {
		assert !select_fd_is_supported(-2)
		assert select_fd_is_supported(C.FD_SETSIZE - 1)
		assert !select_fd_is_supported(C.FD_SETSIZE)
	}
}
