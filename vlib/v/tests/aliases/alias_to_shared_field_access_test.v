struct Base {
	embedded_field int
}

struct Data {
	Base
	field int
}

type AliasWithShared = shared Data

struct Holder {
	data AliasWithShared
}

fn field_from_alias(data AliasWithShared) int {
	return data.field
}

fn field_from_alias_pointer(data &AliasWithShared) int {
	return data.field
}

fn embedded_field_from_alias_pointer(data &AliasWithShared) int {
	return data.embedded_field
}

fn option_alias_with_shared() ?AliasWithShared {
	return &Data{
		field: 5
	}
}

fn result_alias_with_shared() !AliasWithShared {
	return &Data{
		field: 6
	}
}

fn test_alias_to_shared_field_access() {
	data_with_shared := AliasWithShared(&Data{
		field: 1
	})
	assert data_with_shared.field == 1
}

fn test_alias_to_shared_call_arg() {
	assert field_from_alias(&Data{
		field: 2
	}) == 2
}

fn test_alias_to_shared_struct_init_field() {
	holder := Holder{
		data: &Data{
			field: 3
		}
	}
	assert holder.data.field == 3
}

fn test_alias_to_shared_pointer_field_access() {
	data := AliasWithShared(&Data{
		field: 4
	})
	assert field_from_alias_pointer(&data) == 4
}

fn test_alias_to_shared_pointer_embed_field_access() {
	data := AliasWithShared(&Data{
		embedded_field: 7
	})
	assert embedded_field_from_alias_pointer(&data) == 7
}

fn test_option_alias_to_shared_cast() {
	opt := ?AliasWithShared(&Data{
		field: 5
	})
	data := opt or {
		assert false
		return
	}

	assert data.field == 5
}

fn test_option_alias_to_shared_return() {
	data := option_alias_with_shared() or {
		assert false
		return
	}
	assert data.field == 5
}

fn test_result_alias_to_shared_return() {
	data := result_alias_with_shared() or {
		assert false
		return
	}
	assert data.field == 6
}
