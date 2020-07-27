module main

struct App {}

fn main() {
	app := App{}
	method := 'method1'
	arr := ['hello', method]
	ints := [1, 2]
	app.$method(arr, true, false, ints)
}

fn (app App) method1(a, b string, c, d bool, e, f int) {
	println('hello $a $b $c $d $e $f')
}

fn (app App) method2() {
	println('hello2')
}
