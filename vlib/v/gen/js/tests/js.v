import hello as hl
import hello.hello1 as hl1

const (
	i_am_a_const = 21214
	super        = 'amazing keyword'
)

struct Foo {
mut:
	a hl.Aaa
}

struct Companies {
	google int
	amazon bool
	yahoo  string
}

enum POSITION {
	go_back
	dont_go_back
}

fn class(extends string, instanceof int) {
	delete := instanceof
	_ = delete
}

fn main() {
	println('Hello from V.js!')
	println(JS.Math.atan2(1, 0))
	println(JS.eval("console.log('Hello!')"))
	mut a := 1
	a *= 2
	a += 3
	println(a)
	mut b := hl.Aaa{}
	b.update('an update')
	println(b)
	mut c := Foo{hl.Aaa{}}
	c.a.update('another update')
	println(c)
	println('int(1.5) == "${int(1.5)}"')
	d := int(10) + f32(127)
	println('typeof (int + f32) == "${typeof(d)}"')
	_ = 'done'
	{
		_ = 'block'
	}
	_ = POSITION.go_back
	_ = hl.Ccc.a
	debugger := 'JS keywords'
	// TODO: Implement interpolation
	await := '$super: $debugger'
	mut finally := 'implemented'
	println('$await $finally')
	dun := i_am_a_const * 20 + 2
	dunn := hl.hello // External constant
	_ = hl1.nested()
	for i := 0; i < 10; i++ {
	}
	for i, x in 'hello' {
	}
	mut evens := []int{}
	for x in 1 .. 10 {
		y := error_if_even(x) or { x + 1 }
		evens << y
	}
	println(evens)
	arr := [1, 2, 3, 4, 5]
	for i in arr {
	}
	ma := {
		'str': 'done'
		'ddo': 'baba'
	}
	// panic('foo')
	for m, n in ma {
		iss := m
	}
	go async(0, 'hello')
	fn_in_var := fn (number int) {
		println('number: $number')
	}
	hl.debugger()
	anon_consumer(hl.excited(), fn (message string) {
		println(message)
	})
	hl.raw_js_log()
	propagation() or { println(err.msg) }
}

fn anon_consumer(greeting string, anon fn (message string)) {
	anon(greeting)
}

fn async(num int, def string) {
}

[inline]
[deprecated]
fn hello(game_on int, dummy ...string) (int, int) {
	defer {
		do := 'not'
	}
	for dd in dummy {
		l := dd
	}
	return game_on + 2, 221
}

fn (it Companies) method() int {
	ss := Companies{
		google: 2
		amazon: true
		yahoo: 'hello'
	}
	a, b := hello(2, 'google', 'not google')
	glue := if a > 2 {
		'more_glue'
	} else if a > 5 {
		'more glueee'
	} else {
		'less glue'
	}
	if a != 2 {
	}
	return 0
}

fn error_if_even(num int) ?int {
	if num % 2 == 0 { return error('number is even') }
	return num
}

fn propagation() ? {
	println('Propagation test:')
	return error('"Task failed successfully" - Windows XP')
}
