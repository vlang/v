interface Something {
	i int
}

struct Some {
	i int
}

struct App<M> {
	f M
}

fn (mut self App<M>) next<M, T>(input T) f64 {
	$if M is Something {
		return 0
	} $else {
		panic('${typeof(M.typ).name} is not supported')
		return 1
	}
	return 1
}

fn test_generic_method_with_multi_types() {
	mut app := App<Some>{
		f: Some{
			i: 10
		}
	}
	assert app.next(1) == 0
}
