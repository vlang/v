// vtest build: !(windows && tinyc)
module mbedtls

fn test_select_rejects_negative_handle_before_fd_set() {
	select(-2, .read, 0) or {
		assert err.code() == C.EINVAL
		assert err.msg().contains('invalid socket handle')
		return
	}
	assert false, 'select accepted a negative descriptor'
}

$if !windows {
	fn test_select_rejects_fd_setsize_before_fd_set() {
		select(C.FD_SETSIZE, .read, 0) or {
			assert err.code() == C.EINVAL
			assert err.msg().contains('FD_SETSIZE')
			return
		}
		assert false, 'select accepted a descriptor outside fd_set'
	}
}
