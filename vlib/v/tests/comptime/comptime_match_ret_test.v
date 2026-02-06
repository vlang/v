fn test[T]() bool {
	return $match T {
		i8 { true }
		i16 { true }
		$else { $compile_error('unsupported type') }
	}
}

fn test_main() {
	assert test[i8]() == true
	assert test[i16]() == true
}
