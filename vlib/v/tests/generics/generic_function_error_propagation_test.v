import rand

type Condition = fn () bool

fn test_generic_function_error_propagation() {
	mut list := []Condition{}
	list = [fn () bool {
		return true
	}]
	println(rand.element[Condition](list) or { panic('Err') })
	println(list)
}
