module main

struct App {
	test string [test]
mut:
	a    string
}

fn main() {
	println('All functions')
	$for method in App.methods {
		$if method.@type is int {
			println('hi')
		}
		println('$method.name.len')
		println('$method.name.str')
		println('Method: $method.name')
		println('Attributes: $method.attrs')
		println('Return type: $method.ret_type')
	}
	println('All integer functions')
	$for method in App.methods {
		println('Method: $method.name')
		println('Attributes: $method.attrs')
	}
	$for field in App.fields {
		$if field.@type is string {
			println(field)
		}
	}
}

fn (mut app App) method_one() {
}

fn (mut app App) method_two() {
}

fn (mut app App) method_three() int {
	return 0
}

fn (mut app App) method_four() int {
	return 1
}
