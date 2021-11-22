import os

const vpaths = os.vmodules_paths()

fn main() {
	println(vpaths)
	println(os.args)
	println(os.wd_at_startup)
	//
	// fullpath := os.join_path('abc', 'xyz', 'def')
	// println(fullpath)
	//
	x := 'abc'
	t := x.trim_right('/')
	println(x)
	println(t)

	z := x + t
	println(z)

	me := os.executable()
	println(me)

	me_realpath := os.real_path(me)
	println(me_realpath)

	exe_realpath := os.real_path(os.executable())
	println(exe_realpath)

	// exeparent_folder := os.dir(exe_realpath)
	// println(exeparent_folder)

	cdir := os.join_path_single(os.home_dir(), '.cache')
	println(cdir)

	wd_realpath := os.real_path(os.wd_at_startup)
	println(wd_realpath)
}
