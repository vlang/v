struct CtType1 {
	item f32
}

struct CtType2 {
	item int
}

struct CtGenericThing[T] {
	part1 T
	part2 T
}

struct CtAnotherGenericThing[T] {
	part3 T
}

fn (thing CtGenericThing[T]) inline_weird() CtAnotherGenericThing[int] {
	$if T is CtType2 {
		return CtAnotherGenericThing[int]{thing.part1.item + thing.part2.item}
	} $else $if T is CtType1 {
		return CtAnotherGenericThing[int]{int(thing.part1.item + thing.part2.item)}
	} $else {
		$compile_error('unrecognised type')
	}
}

fn (thing CtGenericThing[T]) temp_var_weird() CtAnotherGenericThing[int] {
	result := thing.part1.item + thing.part2.item
	$if T is CtType2 {
		return CtAnotherGenericThing[int]{result}
	} $else $if T is CtType1 {
		return CtAnotherGenericThing[int]{int(result)}
	} $else {
		$compile_error('unrecognised type')
	}
}

fn test_comptime_if_generic_struct_init_selector() {
	thing1 := CtGenericThing{CtType1{1}, CtType1{2}}
	assert thing1.inline_weird().part3 == 3
	assert thing1.temp_var_weird().part3 == 3

	thing2 := CtGenericThing{CtType2{1}, CtType2{2}}
	assert thing2.inline_weird().part3 == 3
	assert thing2.temp_var_weird().part3 == 3
}
