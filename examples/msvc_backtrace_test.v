/*
 Test for MSVC backtrace display.

 Unfortunately, for now it will fail in builtin.v line 117 SymFromAddr()
 with a error 'paramter is incorrect'

*/
fn main() {
	
	test()
	//panic('hi')
}

fn test() {
	print_backtrace() 
}
