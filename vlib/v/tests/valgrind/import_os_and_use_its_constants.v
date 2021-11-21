import os

const vpaths = os.vmodules_paths()

fn main() {
	println(vpaths)
	println(os.args)
	println(os.wd_at_startup)
	//
	fullpath := os.join_path('abc', 'xyz', 'def')
	println(fullpath)
	//
	x := 'abc'
	t := x.trim_right('/')
	println(x)
	println(t)

	z := x + t
	println(z)
}
