module net

#include <unistd.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/select.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <fcntl.h>

fn error_code() int {
	return C.errno
}

fn init() {
}

pub const (
	msg_nosignal = 0x4000
)

const (
	error_ewouldblock = C.EWOULDBLOCK
)

#flag solaris -lsocket
