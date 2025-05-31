// for issue 20436
// phenomenon:
// close_scope() is lost when selectexpr has two special names: sort and sorted.
// This problem can be tested using closure arguments.
struct Foo {
	sort []int
}

fn test_main() {
	f := Foo{}

	fn [f] () {
		assert f.sort.len == 0
	}()
}
