struct DetailError {
	Error
	position int
	expected string
	actual   string
	path     string
}

fn (e &DetailError) msg() string {
	return '${e.path}:${e.position}:${e.expected}:${e.actual}'
}

fn detail_error() DetailError {
	return DetailError{
		position: 7
		expected: 'float'
		actual:   'str'
		path:     'y'
	}
}

fn add_error_path(err IError, part string) IError {
	match err {
		DetailError {
			return DetailError{
				...err
				path: part + err.path
			}
		}
		else { return err }
	}
}

fn copy_if_error(err IError) DetailError {
	if err is DetailError {
		return DetailError{
			...err
			position: 19
		}
	}
	return DetailError{}
}

fn test_interface_smartcast_update_copies_payload_before_overriding() {
	original := IError(detail_error())
	updated := add_error_path(original, 'places[1].')
	assert updated.msg() == 'places[1].y:7:float:str'
	assert original.msg() == 'y:7:float:str'
	again := add_error_path(updated, 'root.')
	assert again.msg() == 'root.places[1].y:7:float:str'
	assert updated.msg() == 'places[1].y:7:float:str'
	assert copy_if_error(original).msg() == 'y:19:float:str'
	assert original.msg() == 'y:7:float:str'
}

fn test_interface_smartcast_update_preserves_borrowed_source() {
	value := detail_error()
	storage := &value
	original := IError(storage)
	updated := add_error_path(original, 'items[0].')
	assert updated.msg() == 'items[0].y:7:float:str'
	assert storage.msg() == 'y:7:float:str'
	assert original.msg() == 'y:7:float:str'
}

struct ErrorHolder {
	err IError
}

fn copy_nested_error(holder ErrorHolder) DetailError {
	if holder.err is DetailError {
		return DetailError{
			...holder.err
			actual: 'bool'
		}
	}
	return DetailError{}
}

fn test_nested_interface_smartcast_update_copies_payload() {
	holder := ErrorHolder{detail_error()}
	assert copy_nested_error(holder).msg() == 'y:7:float:bool'
	assert holder.err.msg() == 'y:7:float:str'
}

type ErrorChoice = DetailError | string

fn copy_choice(value ErrorChoice) DetailError {
	match value {
		DetailError {
			return DetailError{
				...value
				path: 'choice'
			}
		}
		else { return DetailError{} }
	}
}

fn test_sum_smartcast_update_copies_payload() {
	original := ErrorChoice(detail_error())
	assert copy_choice(original).msg() == 'choice:7:float:str'
	if original is DetailError {
		assert original.msg() == 'y:7:float:str'
	} else {
		assert false
	}
}

fn test_plain_value_and_pointer_updates_keep_copy_semantics() {
	original := detail_error()
	updated := DetailError{
		...original
		path: 'plain'
	}
	assert updated.msg() == 'plain:7:float:str'
	assert original.msg() == 'y:7:float:str'
	value := detail_error()
	storage := &value
	from_pointer := DetailError{
		...storage
		path: 'pointer'
	}
	assert from_pointer.msg() == 'pointer:7:float:str'
	assert storage.msg() == 'y:7:float:str'
}

struct DetailSubset {
	position int
	path     string
}

fn copy_error_subset(err IError) DetailSubset {
	if err is DetailError {
		return DetailSubset{
			...err
			path: 'subset'
		}
	}
	return DetailSubset{}
}

fn test_smartcast_update_maps_fields_into_another_struct() {
	original := IError(detail_error())
	updated := copy_error_subset(original)
	assert updated.position == 7
	assert updated.path == 'subset'
	assert original.msg() == 'y:7:float:str'
}
