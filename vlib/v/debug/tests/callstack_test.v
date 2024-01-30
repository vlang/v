module main

import v.debug

// vtest vflags: -callstacke

struct Test {}

fn (t Test) test_method() {
	fn_test4()
}

fn Test.test_static_method() {
	fn_test3()
}

fn fn_test2() ? {
	a := 1
	debug.dump_callstack()
	dump(a)
	assert dump(debug.callstack(1)?).line == 43
	assert dump(debug.callstack(0)?).line == 51
}

fn fn_test3() ? {
	debug.dump_callstack()
	assert dump(debug.callstack(3)?).line == 8
	assert dump(debug.callstack(1)?).line == 41
	assert dump(debug.callstack(0)?).line == 51
	return
}

fn fn_test4() ? {
	debug.dump_callstack()
	assert dump(debug.callstack(1)?).line == 10
}

fn fn_test_anon(cb fn ()) {
	cb()
}

fn fn_test() {
	fn_test2()
	dump('enter')
	fn_test3()
	dump(debug.callstack(0))
}

fn test_main() {
	dump(12)
	fn_test()
	debug.dump_callstack()
	dump(debug.callstack(0))
	mut ret := debug.callstack(0)
	Test{}.test_method()
	Test.test_static_method()
	fn_test_anon(fn () {
		debug.dump_callstack()
	})

	ret = debug.callstack(0)
	assert ret == none
}
