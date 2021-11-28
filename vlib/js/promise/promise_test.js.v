fn test_promise() {
	// TODO: For some reason compiler errors: "error: unknown function: js.promise.new", fix this
	/*
	p := promise.new<int, f64>(fn (resolve_ fn (x int), reject_ fn (x f64)) {
		println('Promise code')
		assert true
		resolve_(42)
	})
	p.then(fn (val int) {
		println('resolved')
		assert val == 42
	}, fn (fail f64) {
		println('rejected')
		assert false
	})*/
}
