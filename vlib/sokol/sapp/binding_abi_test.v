module sapp

fn read_touch_identifier(value &uint) uint {
	return *value
}

fn test_touch_identifier_has_pointer_width() {
	touch := TouchPoint{
		identifier: uint(123)
	}
	assert sizeof(typeof(touch.identifier)) == sizeof(voidptr)
	assert read_touch_identifier(&touch.identifier) == 123
}

fn test_invalid_mouse_button_is_native_sentinel() {
	assert int(MouseButton.invalid) == 0x100
	assert int(MouseButton.left) == 0
	assert int(MouseButton.right) == 1
	assert int(MouseButton.middle) == 2
}
