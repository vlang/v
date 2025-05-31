fn test_main() {
	a := r'\u306aefgh\t'

	mut expected := '\\u306a'
	expected += 'efgh\\t'

	assert a == expected
	assert a == '\\u306aefgh\\t'
}
