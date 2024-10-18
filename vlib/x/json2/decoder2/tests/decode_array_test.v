import x.json2.decoder2 as json

fn test_array_of_strings() {
	assert json.decode[[]int]('[1, 2, 3]')! == [1, 2, 3]
}
