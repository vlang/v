// vtest vflags: -enable-globals

struct AddrManager {
mut:
	n     int
	slots [4]int
	inner AddrInner
}

struct AddrInner {
mut:
	k int
}

__global addr_manager = AddrManager{}
__global addr_manager_ptr = &addr_manager
__global addr_inner_ptr = &addr_manager.inner
__global addr_slot_ptr = &addr_manager.slots[2]

fn test_global_initialized_by_the_address_of_a_global() {
	assert addr_manager_ptr != unsafe { nil }
	addr_manager.n = 5
	assert addr_manager_ptr.n == 5
	addr_manager_ptr.n = 6
	assert addr_manager.n == 6
}

fn test_global_initialized_by_the_address_of_a_field_or_element() {
	addr_manager.inner.k = 7
	assert addr_inner_ptr.k == 7
	addr_manager.slots[2] = 8
	assert unsafe { *addr_slot_ptr } == 8
}
