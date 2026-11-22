// vtest build: linux || termux || android || openbsd || solaris || qnx || serenity || haiku || vinix
module picohttpparser

#include <signal.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <unistd.h>

fn C._exit(status int)
fn C.close(fd int) int
fn C.fork() int
fn C.signal(signal int, handler voidptr) voidptr
fn C.shutdown(fd int, how int) int
fn C.socketpair(domain int, typ int, protocol int, sockets &int) int
fn C.waitpid(pid int, status &int, options int) int

// sigpipe_child_status forks a child that ends a response on a socket whose peer is gone, and
// returns the child's wait status: 0 when `end()` failed with -1 instead of dying of SIGPIPE.
// With `free_descriptor_zero` the child closes descriptor 0 first, so the socket pair takes it:
// 0 is a valid descriptor, and it is the one a process started with stdin closed gets.
fn sigpipe_child_status(free_descriptor_zero bool) int {
	pid := C.fork()
	assert pid >= 0
	if pid == 0 {
		C.signal(C.SIGPIPE, C.SIG_DFL)
		if free_descriptor_zero {
			C.close(0)
		}
		// `socketpair` fills a C `int[2]`, which is a V `[2]i32`, not a `[2]int`.
		mut sockets := [2]i32{}
		if C.socketpair(C.AF_UNIX, C.SOCK_STREAM, 0, &sockets[0]) != 0 {
			C._exit(1)
		}
		if sockets[0] < 0 || sockets[1] < 0 || sockets[0] > 0xffff || sockets[1] > 0xffff {
			C._exit(2)
		}
		if free_descriptor_zero && sockets[0] != 0 {
			C._exit(3)
		}
		if C.shutdown(sockets[0], C.SHUT_WR) != 0 {
			C._exit(1)
		}
		C.close(sockets[1])
		mut payload := [u8(`x`)]
		mut response := Response{
			fd:        int(sockets[0])
			buf_start: unsafe { &payload[0] }
			buf:       unsafe { &payload[0] + 1 }
		}
		result := response.end()
		C.close(sockets[0])
		C._exit(if result == -1 { 0 } else { 1 })
	}
	mut status := 0
	assert C.waitpid(pid, &status, 0) == pid
	return status
}

fn test_response_end_suppresses_sigpipe_per_send() {
	assert sigpipe_child_status(false) == 0
}

fn test_response_end_suppresses_sigpipe_on_descriptor_zero() {
	assert sigpipe_child_status(true) == 0
}
