import toml

struct TestStruct {
	foo int
	bar bool   @[skip]
	baz string = 'def' @[toml: barbaz]
}

fn test_toml_attr_encode() {
	assert toml.encode(TestStruct{}) == 'foo = 0\nbarbaz = "def"'
}

fn test_toml_attr_decode() {
	assert toml.decode[TestStruct]('foo = 0\nbarbaz = "def"')! == TestStruct{}
}
