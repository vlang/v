// vtest build: false // This should be build with: v -use-coroutines simple_coroutines.v
// Note: the Photon wrapper is not yet trivial enough to build/install on the CI.
import coroutines
import time
import os
import net.http

fn foo(a int) {
	for {
		println('1hello from foo() a=${a}')
		// C.printf(c'hello from foo() a=%d\n', a)
		coroutines.sleep(1 * time.second)
	}
}

fn foo2(a int) {
	mut i := 0
	for {
		println('hello from foo2() a=${a}')
		// C.printf(c'hello from foo2() a=%d\n', a)
		coroutines.sleep(2 * time.second)
		i++
		resp := http.get('https://vlang.io/utc_now') or { panic(err) }
		println(resp)
		mut f := os.create('/tmp/FOO2_a${i}') or { panic(err) }
		f.write_string(resp.body) or { panic(err) }
		f.close()
	}
}

fn foo3(a int) {
	for {
		println('hello from foo3() a=${a}')
		// C.printf(c'hello from foo3() a=%d\n', a)
		coroutines.sleep(3 * time.second)
	}
}

fn foo4(a int) {
	for {
		println('hello from foo4() a=${a}')
		// C.printf(c'hello from foo4() a=%d\n', a)
		coroutines.sleep(3 * time.second)
	}
}

fn main() {
	go foo(10)
	go foo2(20)
	go foo3(30)
	go foo3(40)
	$if is_coroutine ? {
		println('IS COROUTINE=true')
	} $else {
		println('IS COROUTINE=false')
	}
	for {
		println('hello from MAIN')
		coroutines.sleep(1 * time.second)
	}
	println('done')
}
