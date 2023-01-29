[has_globals]
module dl

import os

pub const (
	dl_no_path_issue_code  = 1
	dl_open_issue_code     = 1
	dl_sym_issue_code      = 2
	dl_close_issue_code    = 3
	dl_register_issue_code = 4

	dl_no_path_issue_msg   = 'no paths to dynamic library'
	dl_open_issue_msg      = 'could not open dynamic library'
	dl_sym_issue_msg       = 'could not get optional symbol from dynamic library'
	dl_close_issue_msg     = 'could not close dynamic library'
	dl_register_issue_msg  = 'could not register dynamic library loader'

	dl_no_path_issue_err   = error_with_code(dl_no_path_issue_msg, dl_no_path_issue_code)
	dl_open_issue_err      = error_with_code(dl_open_issue_msg, dl_open_issue_code)
	dl_sym_issue_err       = error_with_code(dl_sym_issue_msg, dl_sym_issue_code)
	dl_close_issue_err     = error_with_code(dl_close_issue_msg, dl_close_issue_code)
	dl_register_issue_err  = error_with_code(dl_register_issue_msg, dl_register_issue_code)
)

__global (
	registerd_dl_loaders map[string]&DynamicLibLoader
)

fn register_dl_loader(key string, dl_loader &DynamicLibLoader) ! {
	if key in registerd_dl_loaders {
		return dl.dl_register_issue_err
	}
	registerd_dl_loaders[key] = dl_loader
}

// registered_dl_loader_keys returns the keys of registered DynamicLibLoader.
pub fn registered_dl_loader_keys() []string {
	return registerd_dl_loaders.keys()
}

// DynamicLibLoader is a wrapper around dlopen, dlsym and dlclose.
[heap]
pub struct DynamicLibLoader {
pub:
	flags int = rtld_lazy
	paths []string
mut:
	handle  voidptr
	sym_map map[string]voidptr
}

// DynamicLibLoaderConfig is a configuration for DynamicLibLoader.
[params]
pub struct DynamicLibLoaderConfig {
	// flags is the flags for dlopen.
	flags int = rtld_lazy
	// key is the key to register the DynamicLibLoader.
	key string
	// env_path is the environment variable name that contains the path to the dynamic library.
	env_path string
	// paths is the list of paths to the dynamic library.
	paths []string
}

// new_dynamic_lib_loader returns a new DynamicLibLoader.
fn new_dynamic_lib_loader(conf DynamicLibLoaderConfig) !&DynamicLibLoader {
	mut paths := conf.paths.clone()

	if conf.env_path.len > 0 {
		if env_path := os.getenv_opt(conf.env_path) {
			paths << env_path.split(os.path_separator)
		}
	}

	if paths.len == 0 {
		return dl.dl_no_path_issue_err
	}

	mut dl_loader := &DynamicLibLoader{
		flags: conf.flags
		paths: paths
	}

	register_dl_loader(conf.key, dl_loader)!
	return dl_loader
}

// get_or_create_dynamic_lib_loader returns a DynamicLibLoader.
// If the DynamicLibLoader is not registered, it creates a new DynamicLibLoader.
pub fn get_or_create_dynamic_lib_loader(conf DynamicLibLoaderConfig) !&DynamicLibLoader {
	if dl_loader := registerd_dl_loaders[conf.key] {
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
		if handle := open_opt(path, dl_loader.flags) {
			dl_loader.handle = handle
			return handle
		}
	}

	return dl.dl_open_issue_err
}

// close closes the dynamic library.
pub fn (mut dl_loader DynamicLibLoader) close() ! {
	if !isnil(dl_loader.handle) {
		if close(dl_loader.handle) {
			dl_loader.handle = unsafe { nil }
			return
		}
	}

	return dl.dl_close_issue_err
}

// get_sym gets a symbol from the dynamic library.
pub fn (mut dl_loader DynamicLibLoader) get_sym(name string) !voidptr {
	if sym := dl_loader.sym_map[name] {
		return sym
	}

	handle := dl_loader.open()!
	if sym := sym_opt(handle, name) {
		dl_loader.sym_map[name] = sym
		return sym
	}

	dl_loader.close()!
	return dl.dl_sym_issue_err
}
