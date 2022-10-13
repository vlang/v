#include "@VMODROOT/epoll.h"
#include "@VMODROOT/netdb.h"

pub struct C.epoll_event {
mut:
	events u32
	data   C.epoll_data_t
}

[typedef]
pub union C.epoll_data_t {
mut:
	ptr voidptr
	fd  int
	u32 u32
	u64 u64
}

struct Epoll {
	ev C.epoll_event
}

struct C.hostent {
	h_name      &char
	h_aliases   &&char
	h_addrtype  int
	h_length    int
	h_addr_list &&char
}

fn test_dump_c_struct() {
	ev := C.epoll_event{}
	unsafe { C.memset(&ev, 0, sizeof(ev)) }
	dump(ev)
	println(ev)

	e := Epoll{
		ev: C.epoll_event{}
	}
	dump(e)
	println(e)
	//
	mut hostent := &C.hostent{
		h_addr_list: unsafe { nil }
		h_aliases: unsafe { nil }
		h_name: unsafe { nil }
	}
	dump(hostent)
	assert true
}
