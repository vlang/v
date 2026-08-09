type Palette = [4]Color

enum Color {
	red
	green
	blue
}

struct Box {
	p &Palette
}

// https://github.com/vlang/v/issues/28029
fn test_reference_to_fixed_array_alias_of_struct_in_struct_field() {
	_ := Box{
		// p is never dereferenced; nil only initializes this required compile-only pointer.
		p: unsafe { nil }
	}
}
