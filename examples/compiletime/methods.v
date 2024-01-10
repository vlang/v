struct App {}

fn (mut app App) method_one() {}

fn (mut app App) method_two() int {
	return 0
}

fn (mut app App) method_three(s string) string {
	return s
}

fn main() {
	$for method in App.methods {
		$if method.typ is fn (string) string {
			println('${method.name} IS `fn(string) string`')
		} $else {
			println('${method.name} is NOT `fn(string) string`')
		}
		$if method.return_type !is int {
			println('${method.name} does NOT return `int`')
		} $else {
			println('${method.name} DOES return `int`')
		}
		$if method.args[0].typ !is string {
			println("${method.name}'s first arg is NOT `string`")
		} $else {
			println("${method.name}'s first arg IS `string`")
		}
		// TODO: Double inversion, should this even be allowed?
		$if method.typ is fn () {
			println('${method.name} IS a void method')
		} $else {
			println('${method.name} is NOT a void method')
		}
		println('')
	}
}
