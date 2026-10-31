import zerr

fn imported_error_pointer(src IError) &zerr.MyError {
	if src is zerr.MyError {
		return src as &zerr.MyError
	}
	panic('unexpected error')
}

fn imported_error_convert(src IError) IError {
	if src is zerr.MyError {
		e := src as &zerr.MyError
		return e
	}
	return src
}

fn test_imported_ierror_pointer_cast_preserves_payload() {
	for source in [zerr.make(42), IError(&zerr.MyError{ n: 24 })] {
		pointer := imported_error_pointer(source)
		assert pointer.n in [42, 24]
		assert imported_error_pointer(source) == pointer
		direct := source as &zerr.MyError
		assert direct == pointer
		converted := imported_error_convert(source)
		assert converted is zerr.MyError
		if converted is zerr.MyError {
			assert converted.n == pointer.n
		}
	}
	other := error('other')
	assert imported_error_convert(other).msg() == 'other'
}
