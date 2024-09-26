@[has_globals]
module loader

import dl
import os

const dl_no_path_issue_msg = 'no paths to dynamic library'
const dl_open_issue_msg = 'could not open dynamic library'
const dl_sym_issue_msg = 'could not get optional symbol from dynamic library'
const dl_close_issue_msg = 'could not close dynamic library'
const dl_register_issue_msg = 'could not register dynamic library loader'

pub const dl_no_path_issue_code = 1
pub const dl_open_issue_code = 1
pub const dl_sym_issue_code = 2
pub const dl_close_issue_code = 3
pub const dl_register_issue_code = 4

pub const dl_no_path_issue_err = error_with_code(dl_no_path_issue_msg, dl_no_path_issue_code)
pub const dl_open_issue_err = error_with_code(dl_open_issue_msg, dl_open_issue_code)
pub const dl_sym_issue_err = error_with_code(dl_sym_issue_msg, dl_sym_issue_code)
pub const dl_close_issue_err = error_with_code(dl_close_issue_msg, dl_close_issue_code)
pub const dl_register_issue_err = error_with_code(dl_register_issue_msg, dl_register_issue_code)

__global (
	registered_dl_loaders map[string]&DynamicLibLoader
)

fn register_dl_loader(dl_loader &DynamicLibLoader) ! {
	if dl_loader.key in registered_dl_loaders {
		return dl_register_issue_err
	}
	registered_dl_loaders[dl_loader.key] = dl_loader
}

// registered_dl_loader_keys returns the keys of registered DynamicLibLoader.
pub fn registered_dl_loader_keys() []string {
	return registered_dl_loaders.keys()
}

// DynamicLibLoader is a wrapper around dlopen, dlsym and dlclose.
@[heap]
pub struct DynamicLibLoader {
pub:
	key   string
	flags int = dl.rtld_lazy
	paths []string
mut:
	handle  voidptr
	sym_map map[string]voidptr
}

// DynamicLibLoaderConfig is a configuration for DynamicLibLoader.
@[params]
pub struct DynamicLibLoaderConfig {
pub:
	// flags is the flags for dlopen.
	flags int = dl.rtld_lazy
	// key is the key to register the DynamicLibLoader.
	key string
	// env_path is the environment variable name that contains the path to the dynamic library.
	env_path string
	// paths is the list of paths to the dynamic library.
	paths []string
}

// new_dynamic_lib_loader returns a new DynamicLibLoader.
fn new_dynamic_lib_loader(conf DynamicLibLoaderConfig) !&DynamicLibLoader {
	mut paths := []string{}

	if conf.env_path != '' {
		if env_path := os.getenv_opt(conf.env_path) {
			paths << env_path.split(os.path_delimiter)
		}
	}

	paths << conf.paths

	if paths.len == 0 {
		return dl_no_path_issue_err
	}

	mut dl_loader := &DynamicLibLoader{
		key:   conf.key
		flags: conf.flags
		paths: paths
	}

	register_dl_loader(dl_loader)!
	return dl_loader
}

// get_or_create_dynamic_lib_loader returns a DynamicLibLoader.
// If the DynamicLibLoader is not registered, it creates a new DynamicLibLoader.
pub fn get_or_create_dynamic_lib_loader(conf DynamicLibLoaderConfig) !&DynamicLibLoader {
	if dl_loader := registered_dl_loaders[conf.key] {
		return dl_loader
	}
	return new_dynamic_lib_loader(conf)
}

// load loads the dynamic library.
pub fn (mut dl_loader DynamicLibLoader) open() !voidptr {
	if !isnil(dl_loader.handle) {
		return dl_loader.handle
	}

	for path in dl_loader.paths {
		if handle := dl.open_opt(path, dl_loader.flags) {
			dl_loader.handle = handle
			return handle
		}
	}

	return dl_open_issue_err
}

// close closes the dynamic library.
pub fn (mut dl_loader DynamicLibLoader) close() ! {
	if !isnil(dl_loader.handle) {
		if dl.close(dl_loader.handle) {
			dl_loader.handle = unsafe { nil }
			return
		}
	}

	return dl_close_issue_err
}

// get_sym gets a symbol from the dynamic library.
pub fn (mut dl_loader DynamicLibLoader) get_sym(name string) !voidptr {
	if sym := dl_loader.sym_map[name] {
		return sym
	}

	handle := dl_loader.open()!
	if sym := dl.sym_opt(handle, name) {
		dl_loader.sym_map[name] = sym
		return sym
	}

	dl_loader.close()!
	return dl_sym_issue_err
}

// unregister unregisters the DynamicLibLoader.
pub fn (mut dl_loader DynamicLibLoader) unregister() {
	dl_loader.close() or {}
	registered_dl_loaders.delete(dl_loader.key)
}
