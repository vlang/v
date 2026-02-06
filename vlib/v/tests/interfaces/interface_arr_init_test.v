interface Values {}

fn test_main() {
	a := []Values{len: 4, init: Values(index)}
	assert '${a}' == '[Values(0), Values(1), Values(2), Values(3)]'
}
