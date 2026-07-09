module fasthttp

import net

#include <errno.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/epoll.h>
#include <sys/sendfile.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <netinet/tcp.h>

fn C.socket(domain i32, typ i32, protocol i32) i32

fn C.bind(sockfd i32, addr &net.Addr, addrlen u32) i32

fn C.send(__fd i32, __buf voidptr, __n usize, __flags i32) i32

fn C.recv(__fd i32, __buf voidptr, __n usize, __flags i32) i32

fn C.setsockopt(__fd i32, __level i32, __optname i32, __optval voidptr, __optlen u32) i32

fn C.listen(__fd i32, __n i32) i32

fn C.perror(s &u8)

fn C.close(fd i32) i32

fn C.htons(__hostshort u16) u16

fn C.fcntl(fd i32, cmd i32, arg i32) i32

fn C.accept4(sockfd i32, addr &net.Addr, addrlen &u32, flags i32) i32

fn C.epoll_create1(__flags i32) i32

fn C.epoll_ctl(__epfd i32, __op i32, __fd i32, __event &C.epoll_event) i32

fn C.epoll_wait(__epfd i32, __events &C.epoll_event, __maxevents i32, __timeout i32) i32

fn C.sendfile(out_fd i32, in_fd i32, offset &i64, count usize) i32

fn C.fstat(fd i32, buf &C.stat) i32

@[typedef]
union C.epoll_data_t {
mut:
	ptr voidptr
	fd  int
	u32 u32
	u64 u64
}

struct C.epoll_event {
mut:
	events u32
	data   C.epoll_data_t
}
