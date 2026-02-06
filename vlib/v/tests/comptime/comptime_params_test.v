fn demo(a int, b string) {
}

fn g[T](cb T) {
	mut params := []FunctionParam{}
	$if T is $function {
		$for param in T.params {
			params << param
		}
	}
	assert params.len == 2
	assert params[0].name == 'a'
	assert params[0].typ == 8
	assert params[1].name == 'b'
	assert params[1].typ == 21
}

fn test_main() {
	g(demo)

	mut params := []FunctionParam{}
	$for param in demo.params {
		params << param
	}
	assert params.len == 2
	assert params[0].name == 'a'
	assert params[0].typ == 8
	assert params[1].name == 'b'
	assert params[1].typ == 21
}
