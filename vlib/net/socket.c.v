module net

pub struct Socket {
pub:
	handle int
}

// address gets the address of a socket
pub fn (s &Socket) address() !Addr {
	return addr_from_socket_handle(s.handle)
}

// set_blocking will change the state of the socket to either blocking,
// when state is true, or non blocking (false).
pub fn set_blocking(handle int, state bool) ! {
	$if windows {
		t := if state { u32(0) } else { u32(1) }
		// winsock2.h takes u_long *, which is a different C type from u32 *;
		// voidptr converts implicitly in C, so every compiler accepts it.
		socket_error(C.ioctlsocket(handle, fionbio, voidptr(&t)))!
	} $else {
		$if vlibc ? {
			// fcntl exists in vlibc; Linux issues the raw syscall, other
			// vlibc targets keep the libc calls below. Windows is untouched.
			$if linux {
				vlibc_set_blocking(handle, state)!
				return
			}
		}
		mut flags := C.fcntl(handle, C.F_GETFL, 0)
		if state {
			flags &= ~C.O_NONBLOCK
		} else {
			flags |= C.O_NONBLOCK
		}
		socket_error(C.fcntl(handle, C.F_SETFL, flags))!
	}
}
