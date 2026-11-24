module main
#include "@DIR/deny.h"
fn C.set_refuse_zero_storage(int)

struct Probe {
mut:
	message string
	ptr voidptr
}

fn checked_zero(no_scan bool, mut result Probe) {
	defer { if msg := recover() { result.message = msg } }
	result.ptr = if no_scan { vcalloc_noscan(513) } else { vcalloc(513) }
}

fn main() {
	for noscan in [false, true] {
		mut result := Probe{}
		C.set_refuse_zero_storage(1)
		checked_zero(noscan, mut result)
		C.set_refuse_zero_storage(0)
		assert result.message == 'memory allocation failure'
		assert isnil(result.ptr)
	}
	assert isnil(vcalloc(0))
}
