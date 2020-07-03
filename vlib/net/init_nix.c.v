module net

#include <sys/socket.h>
#include <unistd.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>
fn error_code() int {
	return C.errno
}

pub const (
	msg_nosignal = 0x4000
)

#flag solaris -lsocket
