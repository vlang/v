@[has_globals]
module main

type PromotedHandle = voidptr

__global promoted_handle_slot &PromotedHandle = unsafe { nil }

fn retain_promoted_handle_slot(mut handle PromotedHandle) {
	promoted_handle_slot = unsafe { &handle }
}

fn (mut handle PromotedHandle) clear() {
	handle = PromotedHandle(unsafe { nil })
}

fn read_promoted_handle(_handle &PromotedHandle) !bool {
	return true
}

@[noinline]
fn promote_handle_slot() {
	mut handle := PromotedHandle(voidptr(u64(0x1234)))
	retain_promoted_handle_slot(mut handle)
	assert read_promoted_handle(&handle) or { false }
	assert voidptr(handle) == voidptr(u64(0x1234))
	handle.clear()
	assert voidptr(handle) == unsafe { nil }
}

fn test_promoted_pointer_alias_keeps_its_mutable_slot_and_value() {
	promote_handle_slot()
	unsafe { assert voidptr(*promoted_handle_slot) == nil }
}
