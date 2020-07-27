module main

struct App {
	test string [test]
mut:
	a    string
}

// type VoidFn = fn()

fn main() {
	println('\n\nAll functions')
	$for method in App.methods {
		println('Method: $method.name')
		println('Attributes: $method.attrs')
		println('Return type: $method.ReturnType')
		// TODO: `ReturnType.str()` should return the type name as a string

		$if method.Type is fn(string) string {
			println('$method.name is fn(string) string')
		}
		$if method.ReturnType is int {
			println('$method.name returns int')
		}
		/*
		$if method.args[0].Type is int {
			println('${method.name}\'s first arg is int')
		}
		$if method.Type is VoidFn {
			println('$method.name is a void method')
		}
		*/
		println('')
	}
	println('\n\nAll fields')
	$for field in App.fields {
		$if field.Type is string {
			println('Field `$field.name` is string')
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

fn (mut app App) method_five(s string) string {
	return s
}
