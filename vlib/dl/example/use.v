module main

import os
import dl

type FNAdder = fn (int, int) int

fn main() {
	library_file_path := os.join_path(os.getwd(), 'library$dl.dl_ext')
	handle := dl.open(library_file_path, dl.rtld_lazy)
	eprintln('handle: ${ptr_str(handle)}')
	mut f := &FNAdder(0)
	f = dl.sym(handle, 'add_1')
	eprintln('f: ${ptr_str(f)}')
	res := f(1, 2)
	eprintln('res: $res')
}
