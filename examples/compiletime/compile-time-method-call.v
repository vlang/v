module main

struct App {}

fn main() {
	app := App{}
	method := 'method1'
	app.$method('hello', method)
}

fn (app App) method1(a, b string) {
	println('hello $a $b')
}

fn (app App) method2() {
	println('hello2')
}
