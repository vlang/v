module pointer_block_args

pub struct Box {
pub mut:
	value int
}

// change_box updates the box through the caller's existing pointer.
pub fn change_box(mut box Box) {
	box.value = 42
}
