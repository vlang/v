interface Something {
	i int
}

struct Some {
	i int
}

struct App<M> {
	f M
}

fn (mut self App<M>) next1<M, T>(input T) f64 {
	$if M is Something {
		return 0
	} $else {
		panic('${typeof(M.typ).name} is not supported')
		return 1
	}
	return 1
}

fn (mut self App<M>) next2<T, M>(input T) f64 {
	$if M is Something {
		return 0
	} $else {
		panic('${typeof(M.typ).name} is not supported')
		return 1
	}
	return 1
}

fn (mut self App<M>) next3<T>(input T) f64 {
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
	assert app.next1(1) == 0
	assert app.next2(1) == 0
	assert app.next3(1) == 0
}
