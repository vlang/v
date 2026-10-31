import v.tests.generics.generics_from_modules.genericmodule

// A method of a generic struct of another module that repeats the type parameter
// of its receiver, `fn (b &Box[T]) get[T]() T`, called through a struct that
// embeds `Box[int]`: its `T` is the one of the embedded struct.
struct IntBox {
	genericmodule.Box[int]
}

fn test_a_method_with_its_own_type_parameter_through_an_embed_of_another_module() {
	b := IntBox{
		Box: genericmodule.Box[int]{
			value: 4
		}
	}
	assert b.get() == 4
}
