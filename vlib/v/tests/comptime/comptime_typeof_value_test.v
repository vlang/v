fn t[T](a T) string {
	$if a is $map {
		mut s := ''
		s += '${typeof[T]().name}\n'
		s += '${T.key_type == typeof[u8]().idx} | ${typeof(T.key_type).name} | ${typeof[T]().key_type == typeof[u8]().idx} | ${typeof(typeof[T]().key_type).name}\n'
		s += '${T.value_type == typeof[string]().idx} | ${typeof(T.value_type).name} | ${typeof[T]().value_type == typeof[string]().idx} | ${typeof(typeof[T]().value_type).name}\n'
		s += '${T.idx == typeof[T]().idx} | ${typeof(T.idx).name} | ${typeof(typeof[T]().idx).name}'
		return s
	} $else $if a is $array {
		return '${typeof[T]().name} >> ${T.element_type == typeof[rune]().idx} | ${typeof(T.element_type).name} | ${typeof[T]().element_type == typeof[rune]().idx} | ${typeof(typeof[T]().element_type).name}'
	}
	return ''
}

fn test_main() {
	assert t(map[u8]string{}) == 'map[u8]string\ntrue | u8 | true | u8\ntrue | string | true | string\ntrue | int | int'
	assert t([]rune{}) == '[]rune >> true | rune | true | rune'
}
