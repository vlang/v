// `&Iface(unsafe { nil })` is a null interface pointer, as in V1: code that
// starts from it and assigns a real value only on some paths relies on the nil
// check, and a boxed placeholder would be allocated and leaked on each call.
interface NilCastSock {
	recv() int
}

struct NilCastUnix {
	x int
}

fn (u &NilCastUnix) recv() int {
	return u.x
}

fn nil_cast_pick(real bool) int {
	mut s := &NilCastSock(unsafe { nil })
	if real {
		s = &NilCastUnix{
			x: 7
		}
	}
	if s == unsafe { nil } {
		return -1
	}
	return s.recv()
}

fn test_nil_interface_pointer_cast_stays_nil() {
	assert nil_cast_pick(false) == -1
	assert nil_cast_pick(true) == 7
}
