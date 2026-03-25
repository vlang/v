interface EverythingImplementsThis {}

fn throw_error() ! {
	return error('bro')
}

fn test_empty_interface_accepts_ierror_in_or_block_map_assignment() {
	mut res := map[int]EverythingImplementsThis{}
	for i := 0; i < 10; i++ {
		throw_error() or { res[i] = err }
	}
	assert res.len == 10
	for i := 0; i < 10; i++ {
		assert res[i] is IError
		converted := res[i] as IError
		assert converted.msg() == 'bro'
	}
}
