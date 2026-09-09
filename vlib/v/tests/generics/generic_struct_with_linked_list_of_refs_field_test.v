import datatypes

struct OtherType {}

struct SomeType {
	cfg datatypes.LinkedList[&OtherType]
}

fn test_generic_struct_with_linked_list_of_refs_field() {
	some_type := SomeType{}
	assert some_type.cfg.len() == 0
}
