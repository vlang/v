struct Data {
	field int
}

type AliasWithPtr = &Data

type AliasWithShared = shared Data

fn test_aliased_ptr_field_access() {
	data_with_ptr := AliasWithPtr(&Data{
		field: 1
	})
	assert data_with_ptr.field == 1

	data_with_shared := AliasWithShared(&Data{
		field: 1
	})
	assert data_with_shared.field == 1
}
