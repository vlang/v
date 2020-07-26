module main

struct App {}

fn main() {
	app := App{}
	method := 'method1'
	arr := ['hello', method]
	app.$method(['hello', method], 'test')
}

fn (app App) method1(a, b, c string) {
	println('hello $a $b')
}

fn (app App) method2() {
	println('hello2')
}
