fn test[T](val T) string {
	$if T is u32 {
		$compile_error('u32')
		return ''
	} $else $if T !is string {
		$compile_error('not string')
		return ''
	} $else {
		return val
	}
}

fn test_main() {
	assert test('str') == 'str'
}
