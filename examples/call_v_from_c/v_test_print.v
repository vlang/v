module test_print

[export: 'foo']
fn foo(s &char) {
	println(unsafe { cstring_to_vstring(s) })
}
