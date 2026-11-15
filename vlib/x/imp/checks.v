module imp

import os

// CheckOpts configures a check.
@[params]
pub struct CheckOpts {
pub:
	panic bool // on failure, print the failed check and terminate all flows
}

// LinkCheckOpts configures `check_system_c_links`.
@[params]
pub struct LinkCheckOpts {
pub:
	flags []string // compiler and linker flags, such as `-lmpv` or `-L/opt/lib`
	panic bool     // same as `CheckOpts.panic`
}

// check_system_c_links reports whether a program including each of the headers
// compiles and links with the flags. Each header is tried in its own program.
// The program calls nothing from the header, so this shows that the flags
// resolve, such as `-lmpv` finding a library, and not that they are complete.
// The compiler is the one in $CC, or else the first of cc, gcc, clang and tcc
// in the PATH.
pub fn check_system_c_links(headers []string, opts LinkCheckOpts) bool {
	cc := c_compiler()
	if !binary_exists(cc[0]) {
		return verdict('C compiler', [cc[0]], opts.panic)
	}
	mut kind := 'C link'
	if opts.flags.len > 0 {
		kind += ' (flags: ${opts.flags.join(' ')})'
	}
	return verdict(kind, headers.filter(!header_links(cc, it, opts.flags)), opts.panic)
}

// check_v_modules reports whether V can find all the modules, which may be
// given in dotted form. It searches vmodules, vlib, and `./modules`.
pub fn check_v_modules(modules []string, opts CheckOpts) bool {
	return verdict('V module', modules.filter(!module_exists(it)), opts.panic)
}

// check_binary reports whether all the binaries are in the PATH.
pub fn check_binary(binaries []string, opts CheckOpts) bool {
	return verdict('binary', binaries.filter(!binary_exists(it)), opts.panic)
}

// verdict reports whether nothing is missing. If something is and `fatal` is
// set, it prints what is missing and exits with status 1.
fn verdict(kind string, missing []string, fatal bool) bool {
	if missing.len == 0 {
		return true
	}
	if fatal {
		eprintln('imp: check failed: missing ${kind}: ${missing.join(', ')}')
		exit(1)
	}
	return false
}

// binary_exists reports whether `name` is an executable in the PATH.
fn binary_exists(name string) bool {
	os.find_abs_path_of_executable(name) or { return false }
	return true
}

// c_compiler returns the command in $CC, or else the first compiler in the PATH,
// or else `cc`. A missing compiler is left to the caller, to report it by name.
fn c_compiler() []string {
	from_env := os.getenv('CC').fields()
	if from_env.len > 0 {
		return from_env
	}
	for name in ['cc', 'gcc', 'clang', 'tcc'] {
		if binary_exists(name) {
			return [name]
		}
	}
	return ['cc']
}

// header_links builds a program that includes the header, leaving the search
// for both the header and the libraries to the compiler.
fn header_links(cc []string, header string, flags []string) bool {
	base := os.join_path(os.vtmp_dir(), 'imp_check_${os.getpid()}')
	source, output := base + '.c', base + '.out'
	defer {
		os.rm(source) or {}
		os.rm(output) or {}
	}
	os.write_file(source, '#include <${header}>\nint main(void) { return 0; }\n') or {
		return false
	}
	mut argv := cc.clone()
	argv << [source, '-o', output]
	argv << flags // after the source, as linkers resolve libraries in order
	return os.exec(argv).exit_code == 0
}

// module_exists reports whether the module `name` is in a place V searches.
fn module_exists(name string) bool {
	path := name.replace('.', os.path_separator)
	mut roots := os.vmodules_paths()
	roots << os.join_path(@VEXEROOT, 'vlib')
	roots << 'modules'
	return roots.any(has_v_files(os.join_path(it, path)))
}

// has_v_files reports whether `dir` directly contains a .v file.
fn has_v_files(dir string) bool {
	return (os.ls(dir) or { []string{} }).any(it.ends_with('.v'))
}
