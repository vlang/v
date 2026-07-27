@[has_globals]
module multiwindow

import sync

struct AppIdentityAllocator {
mut:
	mutex     &sync.Mutex = sync.new_mutex()
	next      u64         = 1
	exhausted bool
}

__global multiwindow_app_identity_allocator = AppIdentityAllocator{}

fn allocate_app_instance_id() !u64 {
	multiwindow_app_identity_allocator.mutex.lock()
	defer {
		multiwindow_app_identity_allocator.mutex.unlock()
	}
	if multiwindow_app_identity_allocator.exhausted {
		return error(err_app_identity_exhausted)
	}
	id := multiwindow_app_identity_allocator.next
	if id == u64(0xffffffffffffffff) {
		multiwindow_app_identity_allocator.exhausted = true
	} else {
		multiwindow_app_identity_allocator.next++
	}
	return id
}
