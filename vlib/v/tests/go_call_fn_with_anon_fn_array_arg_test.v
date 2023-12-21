// for issue 19425
interface MyInterface {}

type Func = fn () int

fn handle_fns(fns []Func, mut w MyInterface) {}

fn test_main() {
	mut w := &MyInterface(123)
	spawn handle_fns([], mut w)
	assert true
}
