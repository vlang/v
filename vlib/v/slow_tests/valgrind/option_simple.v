import os

fn main() {
	a := os.find_abs_path_of_executable('ls') or { '' }
	eprintln(a)
}
