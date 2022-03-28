struct Context {
mut:
	comparisons []string
}

fn (c Context) str() string {
	mut res := []string{}
	for x in c.comparisons {
		res << x
	}
	return '\n' + res.join('\n')
}

fn sort_cb(a &string, b &string, mut context Context) int {
	context.comparisons << 'a: "${*a}" | b: "${*b}"'
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}

fn test_sort_with_compare() {
	mut a := ['hi', '1', '5', '3']
	mut context := Context{}
	a.sort_with_compare_context(sort_cb, context)
	dump(a)
	dump(context)
	assert a == ['1', '3', '5', 'hi']

	assert context.comparisons == [
		'a: "5" | b: "hi"',
		'a: "hi" | b: "1"',
		'a: "hi" | b: "3"',
		'a: "3" | b: "5"',
		'a: "5" | b: "1"',
		'a: "3" | b: "1"',
	]
	//
	mut already_sorted_context := Context{}
	a.sort_with_compare_context(sort_cb, already_sorted_context)
	dump(a)
	dump(already_sorted_context)
	assert context != already_sorted_context
	assert already_sorted_context.comparisons == [
		'a: "5" | b: "1"',
		'a: "5" | b: "3"',
		'a: "5" | b: "hi"',
		'a: "1" | b: "3"',
	]
}
