// vtest build: false // Check the README.md for detailed information; this file needs special compilation options
module test_print

@[export: 'foo']
fn show_foo(s &char) {
	println(unsafe { cstring_to_vstring(s) })
}
