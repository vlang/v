module builder

pub struct Preferences {
pub mut:
	// paths
	vpath string
	vlib_path string
	compile_dir string // contains os.realpath() of the dir of the final file beeing compiled, or the dir itself when doing `v .`
	mod_path string
	user_mod_path string
	module_search_paths []string
	// settings
	os OS // the OS to build for
	is_test bool
	is_verbose bool
}

pub enum OS {
	mac
	linux
	windows
	freebsd
	openbsd
	netbsd
	dragonfly
	js // TODO
	android
	solaris
	haiku
}
