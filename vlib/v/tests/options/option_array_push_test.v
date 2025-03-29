pub struct Struct1 {
pub mut:
	strings ?[]string
}

fn test_main() {
	mut s1 := Struct1{}
	assert s1.strings == none
	s1.strings = []
	assert s1.strings? == []
	s1.strings? << ['foo']
	s1.strings? << ['bar']
	dump(s1)
	assert s1.strings? == ['foo', 'bar']

	s1.strings = []
	assert s1.strings? == []

	s1.strings? << ['foo']
	assert s1.strings? == ['foo']
}
