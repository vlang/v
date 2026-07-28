type Palette = [4]Color

enum Color {
	red
	green
	blue
}

struct Setup {
	palette Palette
}

fn test_fixed_array_alias_of_enum_as_struct_field() {
	setup := Setup{}
	assert setup.palette.len == 4
	assert setup.palette[0] == .red
}
