// for issue 19425
type Func = fn ()

fn handle_fns(fns []Func) {
}

fn test_main() {
	mut fns := []Func{}
	t := spawn handle_fns(fns)
	t.wait()
	assert true
}
