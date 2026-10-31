// vtest vflags: -enable-globals

struct GlobalBox {
mut:
	n int
}

fn zeroed_global_value[T]() T {
	return T{}
}

__global generic_global_box = zeroed_global_value[GlobalBox]()

fn store_in_global_box(n int) {
	mut slot := unsafe { &generic_global_box }
	slot.n = n
}

fn test_address_of_a_global_initialized_by_a_generic_call() {
	assert generic_global_box.n == 0
	store_in_global_box(7)
	assert generic_global_box.n == 7
	mut slot := unsafe { &generic_global_box }
	unsafe {
		*slot = GlobalBox{
			n: 9
		}
	}
	assert generic_global_box.n == 9
}
