pub struct Secondary[I] {
	value I
}

pub struct Pair[T, I] {
	name   T
	second Secondary[I]
}

@[params]
pub struct Params[I] {
	second Secondary[I]
}

fn Pair.new[T, I](args Params[I]) Pair[T, I] {
	return Pair[T, I]{
		name:   'test'
		second: args.second
	}
}

fn (self Pair[T, I]) update(args Params[I]) Pair[T, I] {
	return Pair[T, I]{
		name:   self.name
		second: args.second
	}
}

fn test_generics_params_nested_generic_struct_short_syntax() {
	pair := Pair.new[string, int](
		second: Secondary[int]{
			value: 1
		}
	)
	assert pair.name == 'test'
	assert pair.second.value == 1

	updated := pair.update(
		second: Secondary[int]{
			value: 2
		}
	)
	assert updated.name == 'test'
	assert updated.second.value == 2
}
