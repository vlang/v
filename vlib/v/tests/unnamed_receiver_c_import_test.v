import v.tests.testdata.unnamed_receiver_c_module

struct Foo {}

fn (_ Foo) work(value int) int {
	return value
}

fn test_unnamed_receiver_with_c_backed_import() {
	assert Foo{}.work(unnamed_receiver_c_module.identity(1)) == 1
}
