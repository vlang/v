module redis

$if !windows {
	fn test_select_fd_is_supported_checks_fd_set_bounds() {
		assert !select_fd_is_supported(-2)
		assert select_fd_is_supported(C.FD_SETSIZE - 1)
		assert !select_fd_is_supported(C.FD_SETSIZE)
	}
}

$if windows {
	fn test_select_fd_is_supported_rejects_invalid_windows_socket_values() {
		assert !select_fd_is_supported(-1)
		assert !select_fd_is_supported(-2)
		assert select_fd_is_supported(0)
		// Winsock fd_set stores socket handles, not POSIX descriptor bit indices.
		assert select_fd_is_supported(4096)
	}
}
