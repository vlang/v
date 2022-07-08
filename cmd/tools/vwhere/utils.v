import os
import term
import v.pref

// Symbol type to search
enum Symbol {
	@fn
	@struct
	@interface
	@enum
	@const
	var
}

// Visibility of the symbols to search
enum Visibility {
	all
	@pub
	pri
}

// Mutability of the symbols to search
enum Mutability {
	any
	yes
	not
}

const (
	allowed_params = ['']
	symbols        = {
		'fn':        Symbol.@fn
		'struct':    .@struct
		'interface': .@interface
		'enum':      .@enum
		'const':     .@const
		'var':       .var
	}
	visibilities = {
		'all': Visibility.all
		'pub': .@pub
		'pri': .pri
	}
	mutabilities = {
		'any': Mutability.any
		'yes': .yes
		'not': .not
	}
	vexe      = pref.vexe_path()
	vroot     = os.dir(vexe)
	vmod_dir  = os.vmodules_dir()
	vmod_path = os.vmodules_paths()
	color_out = term.can_show_color_on_stdout()
)

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

fn valid_args_quantity_or_show_help() {
	if true in [
		os.args.len < 3,
		'-h' in os.args,
		'-help' in os.args,
		'--help' in os.args,
		os.args[1..] == ['where', 'help'],
	] {
		os.system('${os.quoted_path(vexe)} help where')
		exit(0)
	}
}

fn make_and_print_error(msg string, opts []string, arg string) {
	eprintln('\n' + maybe_color(term.bright_yellow, msg))
	if opts.len > 0 {
		eprint(opts.map(maybe_color(term.bright_green, it)).join(' | '))
	}
	eprintln(' ...can not be ${maybe_color(term.bright_red, arg)}')
	exit(1)
}

fn maybe_color(term_color fn (string) string, str string) string {
	if color_out {
		return term_color(str)
	} else {
		return str
	}
}
