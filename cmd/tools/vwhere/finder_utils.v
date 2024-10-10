module main

import os
import term { bright_green, bright_red, bright_yellow, ecolorize }
import os.cmdline

// Symbol type to search
enum Symbol {
	fn
	method
	struct
	interface
	enum
	const
	var
	regexp
}

// Visibility of the symbols to search
enum Visibility {
	all
	pub
	pri
}

// Mutability of the symbols to search
enum Mutability {
	any
	yes
	not
}

const _args = os.args
const verbose = '-v' in cmdline.only_options(_args)
const header = '-h' in cmdline.only_options(_args)
const format = '-f' in cmdline.only_options(_args)
const symbols = {
	'fn':        Symbol.fn
	'method':    .method
	'struct':    .struct
	'interface': .interface
	'enum':      .enum
	'const':     .const
	'var':       .var
	'regexp':    .regexp
}
const visibilities = {
	'all': Visibility.all
	'pub': .pub
	'pri': .pri
}
const mutabilities = {
	'any': Mutability.any
	'yes': .yes
	'not': .not
}
const vexe = os.real_path(os.getenv_opt('VEXE') or { @VEXE })
const vlib_dir = os.join_path(os.dir(vexe), 'vlib')
const vmod_dir = os.vmodules_dir()
const vmod_paths = os.vmodules_paths()[1..]
const current_dir = os.abs_path('.')

fn (mut cfg Symbol) set_from_str(str_in string) {
	if str_in !in symbols {
		invalid_option(cfg, str_in)
	}
	cfg = symbols[str_in]
}

fn (mut cfg Visibility) set_from_str(str_in string) {
	if str_in !in visibilities {
		invalid_option(cfg, str_in)
	}
	cfg = visibilities[str_in]
}

fn (mut cfg Mutability) set_from_str(str_in string) {
	if str_in !in mutabilities {
		invalid_option(cfg, str_in)
	}
	cfg = mutabilities[str_in]
}

type ParamOption = Mutability | Symbol | Visibility

// General helpers
fn invalid_option(invalid ParamOption, arg string) {
	match invalid.type_name() {
		'Symbol' {
			msg := 'First arg (symbol type) must be one of the following:'
			make_and_print_error(msg, symbols.keys(), arg)
		}
		'Visibility' {
			msg := '"-vis" (visibility) must be one of the following:'
			make_and_print_error(msg, visibilities.keys(), arg)
		}
		'Mutability' {
			msg := '"-mut" (mutability) must be one of the following:'
			make_and_print_error(msg, mutabilities.keys(), arg)
		}
		else {
			exit(1)
		}
	}
}

fn make_and_print_error(msg string, opts []string, arg string) {
	if verbose || format {
		eprintln('\n' + ecolorize(bright_yellow, msg))
		if opts.len > 0 {
			eprint(opts.map(ecolorize(bright_green, it)).join(' | '))
		}
		eprintln(' ...can not be ${ecolorize(bright_red, arg)}')
	} else {
		eprint(ecolorize(bright_yellow, msg) + ' ')
		if opts.len > 0 {
			eprint(opts.map(ecolorize(bright_green, it)).join(' | '))
		}
		eprintln(' ...can not be ${ecolorize(bright_red, arg)}')
	}
	exit(1)
}

fn collect_v_files(path string, recursive bool) ![]string {
	if path == '' || !os.is_dir(path) {
		return error('path `${path}` does not exist or is not a directory')
	}
	mut all_files := []string{}
	mut entries := os.ls(path)!
	for entry in entries {
		if entry == '.git' {
			continue
		}
		if entry == 'tests' {
			continue
		}
		file := os.join_path_single(path, entry)
		if os.file_ext(entry) in ['.v', '.vsh'] {
			all_files << file
			continue
		}
		if recursive && os.is_dir(file) && !os.is_link(file) {
			all_files << collect_v_files(file, recursive)!
			continue
		}
	}
	return all_files
}

fn resolve_module(path string) !string {
	return match true {
		os.is_dir(path) { path }
		os.is_dir(os.join_path(vmod_dir, path)) { os.join_path(vmod_dir, path) }
		os.is_dir(os.join_path(vlib_dir, path)) { os.join_path(vlib_dir, path) }
		else { error('Path: ${path} not found') }
	}
}
