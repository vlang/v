struct Data {
	field int
}

type AliasWithPtr = &Data

fn test_aliased_field_access_test() {
	data_with_ptr := AliasWithPtr(&Data{
		field: 1
	})
	assert data_with_ptr.field == 1
}
