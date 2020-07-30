module main

struct App {}

fn main() {
	app := App{}
	arr :=  ['hello', 'hi']
	ints := [1, 2]
	m := 'method2'
	$for method in App.methods {
		name := method.name
		$if method.@type is int {
			app.$('method2')()
			app.$(m)()
			app.$(name)(arr, true, false, ints)
		}
	}
}

fn (app App) method1(a, b string, c, d bool, e, f int) int {
	println('hello $a $b $c $d $e $f')
	return 0
}

fn (app App) method2() {
	println('hello2')
}
