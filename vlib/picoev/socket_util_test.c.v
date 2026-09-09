// vtest build: macos || freebsd || netbsd || dragonfly
module picoev

#include <sys/socket.h>

fn C.close(fd i32) i32
fn C.getsockopt(sockfd i32, level i32, optname i32, optval voidptr, optlen &u32) i32
fn C.socket(domain i32, typ i32, protocol i32) i32

fn test_setup_sock_enables_so_nosigpipe() ! {
	fd := C.socket(C.AF_INET, C.SOCK_STREAM, 0)
	assert fd >= 0
	defer {
		C.close(fd)
	}
	setup_sock(fd)!
	mut enabled := 0
	mut len := u32(sizeof(int))
	assert C.getsockopt(fd, C.SOL_SOCKET, C.SO_NOSIGPIPE, &enabled, &len) == 0
	assert enabled == 1
}
