fn foo() map[int]int {
	return {
		1: 2
	}
}

fn test_main() {
	a := ?map[int]int(none)
	assert a == none

	b := ?map[int]int({
		1: 2
	})
	assert b?[1] == 2

	c := ?map[int]map[string]string({
		1: {
			'foo': 'bar'
		}
	})
	assert c?[1]['foo'] == 'bar'

	d := ?map[int]int(foo())
	assert d?[1] == 2
}
