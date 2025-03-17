fn t[T](a T) string {
	$if a is $map {
		mut s := ''
		s += '${typeof[T]().name}\n'
		s += '${T.key_type} | ${typeof(T.key_type).name} | ${typeof[T]().key_type} | ${typeof(typeof[T]().key_type).name}\n'
		s += '${T.value_type} | ${typeof(T.value_type).name} | ${typeof[T]().value_type} | ${typeof(typeof[T]().value_type).name}\n'
		s += '${T.idx} | ${typeof(T.idx).name} | ${typeof[T]().idx} | ${typeof(typeof[T]().idx).name}'
		return s
	} $else $if a is $array {
		return '${typeof[T]().name} >> ${T.element_type} | ${typeof(T.element_type).name} | ${typeof[T]().element_type} | ${typeof(typeof[T]().element_type).name}'
	}
	return ''
}

fn test_main() {
	assert t(map[u8]string{}) == 'map[u8]string\n11 | u8 | 11 | u8\n21 | string | 21 | string\n105 | int | 105 | int'
	assert t([]rune{}) == '[]rune >> 22 | rune | 22 | rune'
}
