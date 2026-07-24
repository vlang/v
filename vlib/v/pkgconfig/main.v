module pkgconfig

import flag
import strings

pub struct Main {
pub mut:
	opt         &MainOptions = unsafe { nil }
	res         string
	has_actions bool
}

pub struct MainResult {
pub:
	output     string
	cflags     []string
	link_flags []string
}

struct MainOptions {
	modversion        bool
	description       bool
	help              bool
	debug             bool
	listall           bool
	exists            bool
	variables         bool
	requires          bool
	atleast           string
	atleastpc         string
	exactversion      string
	version           bool
	cflags_only_path  bool
	cflags_only_other bool
	libs_only_link    bool
	libs_only_path    bool
	libs_only_other   bool
	args              []string
mut:
	cflags bool
	stat1c bool
	libs   bool
}

struct MainActionState {
mut:
	has_actions bool
}

fn parse_action_bool(mut fp flag.FlagParser, mut state MainActionState, name string, abbr u8, usage string) bool {
	args_before := fp.args.clone()
	value := fp.bool(name, abbr, false, usage)
	if fp.args != args_before {
		state.has_actions = true
	}
	return value
}

fn parse_action_string(mut fp flag.FlagParser, mut state MainActionState, name string, abbr u8, usage string) string {
	args_before := fp.args.clone()
	value := fp.string(name, abbr, '', usage)
	if fp.args != args_before {
		state.has_actions = true
	}
	return value
}

fn desc(mod string) !string {
	options := Options{
		only_description: true
	}
	mut pc := load(mod, options) or { return error('cannot parse') }
	return pc.description
}

pub fn main(args []string) !&Main {
	mut fp := flag.new_flag_parser(args)
	fp.application('pkgconfig')
	fp.version(version)
	parsed_options, has_actions := parse_options(mut fp)
	mut m := &Main{
		opt:         parsed_options
		has_actions: has_actions
	}
	opt := m.opt
	if opt.help {
		m.res = fp.usage()
	} else if opt.version {
		m.res = version
	} else if opt.listall {
		mut modules := list()
		modules.sort()
		if opt.description {
			for mod in modules {
				d := desc(mod) or { continue }
				pad := strings.repeat(` `, 20 - mod.len)
				m.res += '${mod} ${pad} ${d}\n'
			}
		} else {
			m.res = modules.join('\n')
		}
	} else if opt.args.len == 0 {
		return error('No packages given')
	}
	return m
}

// link_mode returns the parsed pkg-config link mode.
pub fn (m &Main) link_mode() LinkMode {
	return if m.opt.stat1c { .static_ } else { .dynamic }
}

// force_static enables static pkg-config resolution.
pub fn (mut m Main) force_static() {
	m.opt.stat1c = true
}

// apply_default_actions enables cflags and libs when no action was supplied.
pub fn (mut m Main) apply_default_actions() {
	if m.has_actions {
		return
	}
	m.opt.cflags = true
	m.opt.libs = true
	m.has_actions = true
}

fn escape_static_output_arg(arg string) string {
	if arg == '' {
		return "''"
	}
	mut escaped := strings.new_builder(arg.len + 4)
	for ch in arg {
		if ch.is_space() || ch in [`\\`, `"`, `'`] {
			escaped.write_u8(`\\`)
		}
		escaped.write_u8(ch)
	}
	return escaped.str()
}

pub fn (mut m Main) run_result() !MainResult {
	if !m.opt.stat1c {
		return MainResult{
			output: m.run()!
		}
	}
	options := Options{
		debug:     m.opt.debug
		link_mode: .static_
	}
	opt := m.opt
	mut pc := &PkgConfig(unsafe { nil })
	mut res := m.res
	if opt.args.len == 0 {
		return MainResult{
			output: res
		}
	}
	for arg in opt.args {
		mut pcdep := load_direct(arg, options) or {
			if !opt.exists {
				return err
			}
			continue
		}
		if opt.description {
			if res != '' {
				res += '\n'
			}
			res += pcdep.description
		}
		if unsafe { pc != 0 } {
			pc.extend(pcdep)
		} else {
			pc = pcdep
		}
	}
	needs_flags := opt.cflags || opt.cflags_only_path || opt.cflags_only_other || opt.libs
		|| opt.libs_only_link || opt.libs_only_path || opt.libs_only_other
	mut flags := ResolvedFlags{}
	if needs_flags || opt.exists {
		flags = resolve(opt.args, options)!
	}
	if opt.exists {
		return MainResult{
			output: res
		}
	}
	if opt.exactversion != '' {
		if pc.version != opt.exactversion {
			return error('version mismatch')
		}
		return MainResult{
			output: res
		}
	}
	if opt.atleast != '' {
		if pc.atleast(opt.atleast) {
			return error('version mismatch')
		}
		return MainResult{
			output: res
		}
	}
	if opt.atleastpc != '' {
		if atleast(opt.atleastpc) {
			return error('version mismatch')
		}
		return MainResult{
			output: res
		}
	}
	if opt.variables {
		res = pc.vars.keys().join('\n')
	}
	if opt.requires {
		res += pc.requires.join('\n')
	}
	mut cflags := []string{}
	if opt.cflags_only_path {
		cflags << filter_static(flags.cflags, '-I', '')
	}
	if opt.cflags_only_other {
		cflags << filter_static(flags.cflags, '-I', '-I')
	}
	if opt.cflags {
		cflags << flags.cflags
	}
	mut link_flags := []string{}
	if opt.libs_only_link {
		link_flags << filter_static(flags.libs, '-l', '')
	}
	if opt.libs_only_path {
		link_flags << filter_static(flags.libs, '-L', '')
	}
	if opt.libs_only_other {
		link_flags << filter_static(flags.libs, '-l', '-L')
	}
	if opt.libs {
		link_flags << flags.libs
	}
	mut output_parts := []string{cap: cflags.len + link_flags.len + 1}
	output_parts << cflags
	output_parts << link_flags
	if opt.modversion {
		output_parts << pc.version
	}
	return MainResult{
		output:     res + output_parts.map(escape_static_output_arg).join(' ')
		cflags:     cflags
		link_flags: link_flags
	}
}

pub fn (mut m Main) run() !string {
	if m.opt.stat1c {
		return m.run_result()!.output
	}
	options := Options{
		debug: m.opt.debug
	}
	// m.opt = options
	opt := m.opt
	mut pc := &PkgConfig(unsafe { nil })
	mut res := m.res
	for arg in opt.args {
		mut pcdep := load(arg, options) or {
			if !opt.exists {
				return err
			}
			continue
		}
		if opt.description {
			if res != '' {
				res += '\n'
			}
			res += pcdep.description
		}
		if unsafe { pc != 0 } {
			pc.extend(pcdep)
		} else {
			pc = pcdep
		}
	}
	if opt.exists {
		return res
	}
	if opt.exactversion != '' {
		if pc.version != opt.exactversion {
			return error('version mismatch')
		}
		return res
	}
	if opt.atleast != '' {
		if pc.atleast(opt.atleast) {
			return error('version mismatch')
		}
		return res
	}
	if opt.atleastpc != '' {
		if atleast(opt.atleastpc) {
			return error('version mismatch')
		}
		return res
	}
	if opt.variables {
		res = pc.vars.keys().join('\n')
	}
	if opt.requires {
		res += pc.requires.join('\n')
	}
	mut r := []string{}
	if opt.cflags_only_path {
		r << filter(pc.cflags, '-I', '')
	}
	if opt.cflags_only_other {
		r << filter(pc.cflags, '-I', '-I')
	}
	if opt.cflags {
		r << pc.cflags.join(' ')
	}
	if opt.libs_only_link {
		r << filter(pc.libs, '-l', '')
	}
	if opt.libs_only_path {
		r << filter(pc.libs, '-L', '')
	}
	if opt.libs_only_other {
		r << filter(pc.libs, '-l', '-L')
	}
	if opt.libs {
		r << pc.libs.join(' ')
	}
	if opt.modversion {
		r << pc.version
	}
	return res + r.join(' ')
}

fn filter_static(flags []string, prefix string, prefix2 string) []string {
	mut res := []string{}
	if prefix2 != '' {
		for flag in flags {
			if !flag.starts_with(prefix) && !flag.starts_with(prefix2) {
				res << flag
			}
		}
	} else {
		for flag in flags {
			if flag.starts_with(prefix) {
				res << flag
			}
		}
	}
	return res
}

fn filter(libs []string, prefix string, prefix2 string) string {
	mut res := ''
	if prefix2 != '' {
		for lib in libs {
			if !lib.starts_with(prefix) && !lib.starts_with(prefix2) {
				res += ' ${lib}'
			}
		}
	} else {
		for lib in libs {
			if lib.starts_with(prefix) {
				res += ' ${lib}'
			}
		}
	}
	return res
}

fn parse_options(mut fp flag.FlagParser) (&MainOptions, bool) {
	mut state := MainActionState{}
	options := &MainOptions{
		description:       parse_action_bool(mut fp, mut state, 'description', `d`,
			'show pkg module description')
		modversion:        parse_action_bool(mut fp, mut state, 'modversion', `V`,
			'show version of module')
		help:              parse_action_bool(mut fp, mut state, 'help', `h`,
			'show this help message')
		debug:             fp.bool('debug', `D`, false, 'show debug information')
		listall:           parse_action_bool(mut fp, mut state, 'list-all', `p`,
			'list all pkgmodules')
		exists:            parse_action_bool(mut fp, mut state, 'exists', `e`,
			'return 0 if pkg exists')
		variables:         parse_action_bool(mut fp, mut state, 'print-variables', `P`,
			'display variable names')
		requires:          parse_action_bool(mut fp, mut state, 'print-requires', `r`,
			'display requires of the module')
		atleast:           parse_action_string(mut fp, mut state, 'atleast-version', `a`,
			'return 0 if pkg version is at least the given one')
		atleastpc:         parse_action_string(mut fp, mut state, 'atleast-pkgconfig-version', `A`,
			'return 0 if pkgconfig version is at least the given one')
		exactversion:      parse_action_string(mut fp, mut state, 'exact-version', ` `,
			'return 0 if pkg version is at least the given one')
		version:           parse_action_bool(mut fp, mut state, 'version', `v`,
			'show version of this tool')
		cflags:            parse_action_bool(mut fp, mut state, 'cflags', `c`,
			'output all pre-processor and compiler flags')
		cflags_only_path:  parse_action_bool(mut fp, mut state, 'cflags-only-I', `I`,
			'show only -I flags from CFLAGS')
		cflags_only_other: parse_action_bool(mut fp, mut state, 'cflags-only-other', ` `,
			'show cflags without -I')
		stat1c:            fp.bool('static', `s`, false, 'show --libs for static linking')
		libs:              parse_action_bool(mut fp, mut state, 'libs', `l`,
			'output all linker flags')
		libs_only_link:    parse_action_bool(mut fp, mut state, 'libs-only-l', ` `,
			'show only -l from ldflags')
		libs_only_path:    parse_action_bool(mut fp, mut state, 'libs-only-L', `L`,
			'show only -L from ldflags')
		libs_only_other:   parse_action_bool(mut fp, mut state, 'libs-only-other', ` `,
			'show flags not containing -l or -L')
		args:              fp.args
	}
	return options, state.has_actions
}
