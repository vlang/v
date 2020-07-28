module main

struct App {
	test string [test]
mut:
	a    string
	b    int
}

// type VoidFn = fn()

fn main() {
	println('\nAll functions')
	$for method in App.methods {
		// println(method)
		// TODO: `Type.str()` should return the type name as a string, rather than the index

		$if method.Type is fn(string) string {
			println('$method.name IS `fn(string) string`')
		} $else {
			println('$method.name is NOT `fn(string) string`')
		}
		$if method.ReturnType is int {
			println('$method.name DOES return `int`')
		} $else {
			println('$method.name does NOT return `int`')
		}
		$if method.args[0].Type !is string {
			println("${method.name}'s first arg is NOT `string`")
		} $else {
			println("${method.name}'s first arg IS `string`")
		}
		$if method.Type !is fn() {
			println('$method.name is NOT a void method')
		} $else {
			println('$method.name IS a void method')
		}
		println('')
	}
	println('\nAll fields')
	$for field in App.fields {
		$if field.Type is string {
			println('Field `$field.name` is string')
		}
	}
}

fn (mut app App) method_one() {
}

//fn (mut app App) method_two() {
//}

fn (mut app App) method_two() int {
	return 0
}

//fn (mut app App) method_four() int {
//	return 1
//}

fn (mut app App) method_three(s string) string {
	return s
}
