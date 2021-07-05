fn main() {
	x := if false {
		'foo'
	} else if true {
		'bar'
	} else {
		panic('err')
		'empty'
	}
	assert x == 'bar'
}
