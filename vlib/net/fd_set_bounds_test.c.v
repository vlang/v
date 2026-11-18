module net

$if !windows {
	fn test_select_rejects_negative_handle_before_fd_set() {
		select(-2, .read, 0) or {
			assert err.code() == C.EINVAL
			assert err.msg().contains('invalid socket handle')
			return
		}
		assert false, 'select accepted a negative descriptor'
	}

	fn test_select_rejects_fd_setsize_before_fd_set() {
		select(C.FD_SETSIZE, .read, 0) or {
			assert err.code() == C.EINVAL
			assert err.msg().contains('FD_SETSIZE')
			return
		}
		assert false, 'select accepted a descriptor outside fd_set'
	}
}

$if windows {
	fn test_windows_select_checks_listener_readiness() ! {
		mut listener := listen_tcp(.ip, '127.0.0.1:0')!
		defer {
			listener.close() or {}
		}

		// Winsock fd_set holds SOCKET values, not POSIX integer bit positions.
		// A listening socket with no pending connection must not be ready.
		initially_ready := select(listener.sock.handle, .read, 0)!
		assert !initially_ready

		address := listener.sock.address()!
		mut client := dial_tcp(address.str())!
		defer {
			client.close() or {}
		}
		// A completed loopback connection must make the listener readable.
		connected_ready := select(listener.sock.handle, .read, 0)!
		assert connected_ready
	}
}
