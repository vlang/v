import gensums
import foreignselectors

fn test_imported_translated_constant_field() {
	assert foreignselectors.entry.Value == 7
}

fn test_imported_generated_named_variant_owner() {
	event := gensums._generated_Event.Value(7)
	assert event is gensums._generated_Event.Value
	assert event !is gensums._generated_Event.Empty
	assert event.str() == '_generated_Event.Value(7)'
	match event {
		gensums._generated_Event.Value(value) {
			assert value == 7
		}
		gensums._generated_Event.Empty {
			assert false
		}
	}
	empty := gensums._generated_Event.Empty
	assert empty is gensums._generated_Event.Empty
}

fn test_imported_generated_generic_named_variant_owner() {
	value := gensums._generated_Option[int].Some(9)
	assert value is gensums._generated_Option[int].Some
	assert value !is gensums._generated_Option[int].Nothing
	assert value.str() == '_generated_Option[int].Some(9)'
	empty := gensums._generated_Option[int].Nothing
	assert empty is gensums._generated_Option[int].Nothing
	assert empty.str() == '_generated_Option[int].Nothing'
	match value {
		gensums._generated_Option[int].Some(payload) {
			assert payload == 9
		}
		gensums._generated_Option[int].Nothing {
			assert false
		}
	}
}

fn test_imported_lowercase_generated_named_variant_owner() {
	value := gensums.choice[int].Some(11)
	assert value is gensums.choice[int].Some
	match value {
		gensums.choice[int].Some(payload) {
			assert payload == 11
		}
		gensums.choice[int].Nothing {
			assert false
		}
	}
}
