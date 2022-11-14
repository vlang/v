module main

// Note: This program, requires that the shared library was already compiled.
// To do so, run `v -d no_backtrace -o library -shared modules/library/library.v`
// before running this program.
import os
import dl

type FNAdder = fn (int, int) int

fn main() {
	library_file_path := os.join_path(os.dir(@FILE), dl.get_libname('library'))
	handle := dl.open_opt(library_file_path, dl.rtld_lazy)!
	eprintln('handle: ${ptr_str(handle)}')
	f := FNAdder(dl.sym_opt(handle, 'add_1')!)
	eprintln('f: ${ptr_str(f)}')
	res := f(1, 2)
	eprintln('res: ${res}')
}
