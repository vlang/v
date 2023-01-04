module test_print

[export: 'foo']
fn show_foo(s &char) {
	println(unsafe { cstring_to_vstring(s) })
}
