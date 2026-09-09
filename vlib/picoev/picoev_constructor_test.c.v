// vtest build: !windows
module picoev

import net

#include <signal.h>

fn C.signal(signal i32, handler voidptr) voidptr

fn test_failed_new_preserves_sigpipe_disposition() ! {
	original := C.signal(C.SIGPIPE, C.SIG_IGN)
	defer {
		C.signal(C.SIGPIPE, original)
	}
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	defer {
		listener.close() or {}
	}
	port := listener.addr()!.port()!
	if _ := new(Config{
		host:   '127.0.0.1'
		port:   int(port)
		family: .ip
	})
	{
		assert false, 'picoev.new unexpectedly bound an occupied port'
	}
	previous := C.signal(C.SIGPIPE, C.SIG_IGN)
	assert previous == C.SIG_IGN
}
