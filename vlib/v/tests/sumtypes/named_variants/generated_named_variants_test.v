@[generated]
module main

type _generated_Event = Value(int) | Empty

type _generated_Option[T] = Some(T) | Nothing

type a = Value(int) | Empty

fn test_generated_named_variant_owner() {
	event := _generated_Event.Value(7)
	assert event is _generated_Event.Value
	assert event !is _generated_Event.Empty
	assert event.str() == '_generated_Event.Value(7)'
	match event {
		_generated_Event.Value(value) {
			assert value == 7
		}
		_generated_Event.Empty {
			assert false
		}
	}
	empty := _generated_Event.Empty
	assert empty is _generated_Event.Empty
	assert empty.str() == '_generated_Event.Empty'
	single_letter := a.Value(3)
	assert single_letter is a.Value
	match single_letter {
		a.Value(payload) {
			assert payload == 3
		}
		a.Empty {
			assert false
		}
	}
}

fn test_generated_generic_named_variant_owner() {
	value := _generated_Option[int].Some(9)
	assert value is _generated_Option[int].Some
	assert value !is _generated_Option[int].Nothing
	assert value.str() == '_generated_Option[int].Some(9)'
	empty := _generated_Option[int].Nothing
	assert empty is _generated_Option[int].Nothing
	assert empty.str() == '_generated_Option[int].Nothing'
	match value {
		_generated_Option[int].Some(payload) {
			assert payload == 9
		}
		_generated_Option[int].Nothing {
			assert false
		}
	}
}
