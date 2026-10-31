import strconv

fn test_l_parse_rejects_words_beginning_with_i() {
	for input in ['invalid', 'indigo', 'info', 'infinity'] {
		assert strconv.fxx_to_str_l_parse(input) == 'Float conversion error!!'
		assert strconv.fxx_to_str_l_parse_with_dot(input) == 'Float conversion error!!'
	}
}
