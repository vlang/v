## Description

`dl.loader` is an abstraction layer over `dl` that provides a more user-friendly API in the V way.
It can be used to Dynamically Load a library during runtime in scenarios where the library to load
does not have a determined path and can be located in different places.

It also provides a way to load a library from a specific path, or from a list of paths, or from
a custom environment variable that contains a list of paths.

## Usage

```v
import dl.loader

// Load a library from a list of paths
const default_paths = [
	'not-existing-dynamic-link-library'
	// 'C:\\Windows\\System32\\shell32.dll',
	'shell32',
]

fn main() {
	mut dl_loader := loader.get_or_create_dynamic_lib_loader(
		key:      'LibExample'
		env_path: 'LIB_PATH'
		paths:    default_paths
	)!

	defer {
		dl_loader.unregister()
	}

	sym := dl_loader.get_sym('CommandLineToArgvW')!
	assert !isnil(sym)
}
```