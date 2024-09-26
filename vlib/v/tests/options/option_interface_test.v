interface TestIntf {
	test()
}

struct TestStruct {
	ti ?TestIntf
}

fn TestStruct.new() TestStruct {
	return TestStruct{
		ti: none
	}
}

fn test_main() {
	t := TestStruct.new()
	assert t.ti == none
}
