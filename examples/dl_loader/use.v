module main

// Note: This program, requires that the shared library was already compiled.
// To do so, run `v -d no_backtrace -o library -shared modules/library/library.v`
// before running this program.
import os
import dl.loader

type FNAdder = fn (int, int) int

const (
	default_paths = [
		@mod + '/library.so',
		@mod + '/location1/library.so',
		@mod + '/location2/library.so',
		@mod + '/modules/library/library.so',
	]
)
fn main() {
	mut opencl_loader := loader.get_or_create_dynamic_lib_loader(
		key: 'LibExample'
		env_path: 'EXAMPLE_PATH'
		paths: default_paths
	)!
	defer {
		opencl_loader.unregister()
	}
	f := opencl_loader.get_sym('add_1')!
	eprintln('f: ${ptr_str(f)}')
	res := f(1, 2)
	eprintln('res: ${res}')
}
