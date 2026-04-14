import arrays

const result = ['0: a', '1: b', '2: c', '3: d']

fn mapi[T, U](arr []T, callback fn (int, T) U) []U {
	mut mapped := []U{}
	for i, el in arr {
		mapped << callback(i, el)
	}
	return mapped
}

fn test_main() {
	arr := [`a`, `b`, `c`, `d`]
	arr_1 := mapi(arr, |i, e| '${i}: ${e}')
	assert arr_1 == result
	arr_2 := mapi[rune, string](arr, |i, e| '${i}: ${e}')
	assert arr_2 == result
	arr_3 := mapi(arr, fn (i int, e rune) string {
		return '${i}: ${e}'
	})
	assert arr_3 == result
}

fn count_chars_fold(strs []string) int {
	return arrays.fold(strs, 0, |acc, s| acc + utf8_str_visible_length(s))
}

fn test_lambda_inferred_for_multi_generic_fold_callback() {
	input := ['abc', 'def', 'здравей']
	assert count_chars_fold(input) == 13
}
