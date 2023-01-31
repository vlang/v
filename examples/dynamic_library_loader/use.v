module main

// Note: This program, requires that the shared library was already compiled.
// To do so, run `v -d no_backtrace -o library -shared modules/library/library.v`
// before running this program.
import os
import dl.loader

type FNAdder = fn (int, int) int

const (
	cfolder       = os.dir(@FILE)
	default_paths = [
		os.join_path(cfolder, 'library.so'),
		os.join_path(cfolder, 'location1/library.so'),
		os.join_path(cfolder, 'location2/library.so'),
		os.join_path(cfolder, 'modules/library/library.so'),
	]
)

fn main() {
	mut dl_loader := loader.get_or_create_dynamic_lib_loader(
		key: 'LibExample'
		paths: default_paths
	)!
	defer {
		dl_loader.unregister()
	}
	sym := dl_loader.get_sym('add_1')!
	f := FNAdder(sym)
	eprintln('f: ${ptr_str(f)}')
	res := f(1, 2)
	eprintln('res: ${res}')
}
