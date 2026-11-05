module imported

pub struct Item {
pub mut:
	value u32
}

// fill writes to the caller's item.
pub fn fill(mut item Item) {
	item.value = 42
}

// fill_pair writes distinct values through both borrowed item addresses.
pub fn fill_pair(mut first Item, mut second Item) {
	first.value = 41
	second.value = 42
}

// is_null examines the supplied address without reading its pointee.
pub fn is_null(mut item Item) bool {
	return isnil(&item)
}

// RawPointer is an opaque handle used to check mutable pointer-slot forwarding.
pub type RawPointer = voidptr

// clear_pointer changes the caller's raw pointer slot.
pub fn clear_pointer(mut ptr RawPointer) {
	ptr = RawPointer(unsafe { nil })
}
