#include "@VMODROOT/epoll.h"

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
	assert true
}
