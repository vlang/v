// c-api generates a C library release (shared library + header + def + pc +
// demo + README) from the public API of a V module.
module main

import os
import v.capi

struct Options {
mut:
	dir            string = '.'
	out_dir        string = 'dist'
	lib_path       string
	lib_name       string
	version        string
	description    string
	no_build       bool
	keep_generated bool
}

fn usage() {
	eprintln('usage: v c-api <module-dir> [-o <dir>] [--lib-name <name>] [--version <v>]')
	eprintln('                       [--description <text>] [--no-build] [--keep-generated]')
}

fn parse_args(args []string) !Options {
	mut opts := Options{}
	mut i := 0
	for i < args.len {
		arg := args[i]
		if arg == '-h' || arg == '--help' {
			usage()
			exit(0)
		}
		if arg == '-o' || arg == '--out' {
			i++
			if i >= args.len {
				return error('missing value for ${arg}')
			}
			value := args[i]
			if value.ends_with('.so') || value.ends_with('.dll') || value.ends_with('.dylib') {
				opts.lib_path = value
				opts.out_dir = os.dir(value)
			} else {
				opts.out_dir = value
			}
		} else if arg == '--lib' {
			i++
			if i >= args.len {
				return error('missing value for ${arg}')
			}
			opts.lib_path = args[i]
		} else if arg == '--lib-name' {
			i++
			if i >= args.len {
				return error('missing value for ${arg}')
			}
			opts.lib_name = args[i]
		} else if arg == '--version' {
			i++
			if i >= args.len {
				return error('missing value for ${arg}')
			}
			opts.version = args[i]
		} else if arg == '--description' {
			i++
			if i >= args.len {
				return error('missing value for ${arg}')
			}
			opts.description = args[i]
		} else if arg == '--no-build' {
			opts.no_build = true
		} else if arg == '--keep-generated' {
			opts.keep_generated = true
		} else if arg.starts_with('-') {
			return error('unknown option ${arg}')
		} else if opts.dir == '.' {
			opts.dir = arg
		} else {
			return error('unexpected argument ${arg}')
		}
		i++
	}
	return opts
}

fn lib_file_name(name string) string {
	os_name := os.user_os()
	if os_name == 'windows' {
		return '${name}.dll'
	}
	if os_name == 'macos' {
		return 'lib${name}.dylib'
	}
	return 'lib${name}.so'
}

fn main() {
	mut args := os.args[1..].clone()
	if args.len > 0 && (args[0] == 'c-api' || args[0] == 'capi') {
		args = args[1..]
	}
	opts := parse_args(args) or {
		eprintln('c-api: ${err}')
		exit(1)
	}
	if !os.is_dir(opts.dir) {
		eprintln('c-api: not a directory: ${opts.dir}')
		exit(1)
	}
	model := capi.collect_model(opts.dir, opts.lib_name) or {
		eprintln('c-api: ${err}')
		exit(1)
	}
	mut warnings := []string{}
	wrappers, infos := capi.generate_wrappers(model, mut warnings)
	runtime := capi.generate_runtime(model)
	for w in warnings {
		eprintln('c-api: warning: ${w}')
	}
	wrapper_path := os.join_path(opts.dir, 'capi_generated.v')
	runtime_path := os.join_path(opts.dir, 'capi_generated.c.v')
	if os.exists(wrapper_path) || os.exists(runtime_path) {
		eprintln('c-api: generated files already exist in ${opts.dir}; remove them first')
		exit(1)
	}
	os.write_file(wrapper_path, wrappers) or {
		eprintln('c-api: cannot write ${wrapper_path}: ${err}')
		exit(1)
	}
	os.write_file(runtime_path, runtime) or {
		os.rm(wrapper_path) or {}
		eprintln('c-api: cannot write ${runtime_path}: ${err}')
		exit(1)
	}
	if !opts.keep_generated {
		defer {
			os.rm(wrapper_path) or {}
			os.rm(runtime_path) or {}
		}
	}
	name := if opts.lib_name == '' { model.prefix } else { capi.sanitize(opts.lib_name) }
	result := capi.emit_files(model, infos, capi.EmitOptions{
		out_dir:     opts.out_dir
		lib_name:    name
		version:     opts.version
		description: opts.description
	}) or {
		eprintln('c-api: ${err}')
		exit(1)
	}
	if opts.no_build {
		println('c-api: generated ${result.header}, ${result.def}, ${result.pc}, ${result.demo}, ${result.readme}')
		return
	}
	lib_path := if opts.lib_path == '' {
		os.join_path(opts.out_dir, lib_file_name(name))
	} else {
		opts.lib_path
	}
	mut build_args := [@VEXE, '-shared', '-enable-globals', '-nocache']
	if os.user_os() == 'windows' {
		// GCC on Windows does not honour dllexport attributes; export all symbols
		// so the generated .def names are actually present in the DLL.
		build_args << '-cflags'
		build_args << '-Wl,--export-all-symbols'
	}
	build_args << '-o'
	build_args << lib_path
	build_args << opts.dir
	res := os.exec(build_args)
	if res.exit_code != 0 {
		eprintln('c-api: shared library build failed:')
		eprintln(res.output)
		exit(1)
	}
	println('c-api: built ${lib_path}')
	println('c-api: header ${result.header}')
	println('c-api: def    ${result.def}')
	println('c-api: pkgcfg ${result.pc}')
	println('c-api: demo   ${result.demo}')
	println('c-api: readme ${result.readme}')
}
