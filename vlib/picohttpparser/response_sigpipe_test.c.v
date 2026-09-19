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
fn C.socketpair(domain int, typ int, protocol int, sockets &int) int
fn C.waitpid(pid int, status &int, options int) int

fn test_response_end_suppresses_sigpipe_per_send() {
	pid := C.fork()
	assert pid >= 0
	if pid == 0 {
		C.signal(C.SIGPIPE, C.SIG_DFL)
		mut sockets := [2]int{}
		if C.socketpair(C.AF_UNIX, C.SOCK_STREAM, 0, &sockets[0]) != 0 {
			C._exit(1)
		}
		C.close(sockets[1])
		mut payload := [u8(`x`)]
		mut response := Response{
			fd:        sockets[0]
			buf_start: unsafe { &payload[0] }
			buf:       unsafe { &payload[0] + 1 }
		}
		result := response.end()
		C.close(sockets[0])
		C._exit(if result == -1 { 0 } else { 1 })
	}
	mut status := 0
	assert C.waitpid(pid, &status, 0) == pid
	assert status == 0
}
