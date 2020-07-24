module main

struct App {
}

fn main() {
	println('All functions')
	$for method in App(method) {
		$if ret_type is int {
			println('hi')
		}
		println('Method: $method')
		println('Attributes: $attrs')
		println('Return type: $ret_type')
	}
	println('All integer functions')
	$for method in App(method) {
		println('Method: $method')
		println('Attributes: $attrs')
	}
}

fn (mut app App) method_one() {}
fn (mut app App) method_two() {}

fn (mut app App) method_three() int {
	return 0
}

fn (mut app App) method_four() int {
	return 1
}
