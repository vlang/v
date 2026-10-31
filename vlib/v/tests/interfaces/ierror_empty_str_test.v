struct EmptyErrorHolder {
	err IError
}

struct EmptyErrorEnvelope {
	holder EmptyErrorHolder
}

struct EmptyErrorSource {
mut:
	calls int
}

fn (mut source EmptyErrorSource) error() IError {
	source.calls++
	return EmptyErrorHolder{}.err
}

fn test_empty_error_stringification() {
	holder := EmptyErrorHolder{}
	assert holder.err.str() == 'nil'
	assert '${holder.err}' == 'nil'
	assert '${holder}'.contains('err: nil')
	envelope := EmptyErrorEnvelope{}
	assert '${envelope}'.contains('err: nil')
	errors := [holder.err, error('present')]
	assert '${errors}'.contains('nil')
}

fn test_error_stringification_distinguishes_empty_none_and_concrete() {
	empty := EmptyErrorHolder{}
	none_error := EmptyErrorHolder{ err: none }
	errors := [
		empty.err,
		none_error.err,
		IError(Error{}),
		error(''),
		error_sentinel,
		error_with_code('detail', 37),
	]
	expected := ['nil', 'none', '', '', 'error', 'detail; code: 37']
	for i, err in errors {
		assert err.str() == expected[i]
		assert '${err}' == expected[i]
	}
}

fn test_empty_error_stringification_evaluates_source_once() {
	mut source := EmptyErrorSource{}
	assert '${source.error()}' == 'nil'
	assert source.calls == 1
}
