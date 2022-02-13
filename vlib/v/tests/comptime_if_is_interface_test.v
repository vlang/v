interface Something {
	i int
}

struct Some {
	i int
}

struct App {
mut:
	count u8
}

fn (mut self App) next<T>(input T) string {
	$if T is Something {
		return 'Something'
	} $else $if T is f64 {
		return 'f64'
	} $else {
		panic('${typeof(T.typ).name} is not supported')
	}
	panic('Unreachable')
}

fn test_comptime_if_is_interface() {
	mut app := App{}
	assert app.next(Something(Some{1})) == 'Something'
	assert app.next(1.0) == 'f64'
}
