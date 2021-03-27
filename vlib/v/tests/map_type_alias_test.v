type Test = map[string]string

fn test_index() {
	t := Test(map{
		'c': 'abc'
	})
	assert t['c'] == 'abc'
}
