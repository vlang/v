struct Context {
mut:
	comparisons []string
}

fn test_sort_with_compare() {
	mut a := ['hi', '1', '5', '3']
	mut context := Context{}
	a.sort_with_compare_context(fn (a &string, b &string, mut context Context) int {
		context.comparisons << 'a: ${*a} | b: ${*b}'
		if a < b {
			return -1
		}
		if a > b {
			return 1
		}
		return 0
	}, context)
	dump(a)
	dump(context)
	assert a == ['1', '3', '5', 'hi']
	assert context.comparisons == [
		'a: hi | b: 1',
		'a: 5 | b: 3',
		'a: 1 | b: 3',
		'a: hi | b: 3',
		'a: hi | b: 5',
	]
}
