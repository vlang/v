type Pixel = [4]u8

fn test_setting_of_element_of_array_to_aliased_fixed_array() {
	mut px := Pixel([u8(1), 2, 3, 4]!)
	mut pixels := []Pixel{len: 64}
	pixels[5] = px
	assert pixels[4] == Pixel{}
	assert pixels[5] == px
	assert pixels[6] == Pixel{}
}
