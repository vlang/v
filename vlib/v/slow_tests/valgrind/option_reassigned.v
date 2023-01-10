import os

fn main() {
	mut a := 'abc'
	a = os.find_abs_path_of_executable('ls') or { '' }
	eprintln(a)
}
