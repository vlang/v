module main

struct App {}

fn main() {
	app := App{}
	method := 'method1'
	arr := ['hello', method]
	app.method1(arr)
	println(arr)
	app.$method(arr)
}

fn (app App) method1(a, b string) {
	println('hello $a $b')
}

fn (app App) method2() {
	println('hello2')
}
