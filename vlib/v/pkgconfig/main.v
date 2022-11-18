module pkgconfig

import flag
import strings

pub struct Main {
pub mut:
	opt         &MainOptions = unsafe { nil }
	res         string
	has_actions bool
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
	cflags            bool
	cflags_only_path  bool
	cflags_only_other bool
	stat1c            bool
	libs              bool
	libs_only_link    bool
	libs_only_path    bool
	libs_only_other   bool
	args              []string
}

fn desc(mod string) ?string {
	options := Options{
		only_description: true
	}
	mut pc := load(mod, options) or { return error('cannot parse') }
	return pc.description
}

pub fn main(args []string) ?&Main {
	mut fp := flag.new_flag_parser(args)
	fp.application('pkgconfig')
	fp.version(version)
	mut m := &Main{
		opt: parse_options(mut fp)
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

pub fn (mut m Main) run() ?string {
	options := Options{
		debug: m.opt.debug
	}
	// m.opt = options
	opt := m.opt
	mut pc := &PkgConfig(0)
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
			pc.extend(pcdep)?
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
		if opt.stat1c {
			r << pc.libs_private.join(' ')
		} else {
			r << pc.libs.join(' ')
		}
	}
	if opt.modversion {
		r << pc.version
	}
	return res + r.join(' ')
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

fn parse_options(mut fp flag.FlagParser) &MainOptions {
	return &MainOptions{
		description: fp.bool('description', `d`, false, 'show pkg module description')
		modversion: fp.bool('modversion', `V`, false, 'show version of module')
		help: fp.bool('help', `h`, false, 'show this help message')
		debug: fp.bool('debug', `D`, false, 'show debug information')
		listall: fp.bool('list-all', `p`, false, 'list all pkgmodules')
		exists: fp.bool('exists', `e`, false, 'return 0 if pkg exists')
		variables: fp.bool('print-variables', `P`, false, 'display variable names')
		requires: fp.bool('print-requires', `r`, false, 'display requires of the module')
		atleast: fp.string('atleast-version', `a`, '', 'return 0 if pkg version is at least the given one')
		atleastpc: fp.string('atleast-pkgconfig-version', `A`, '', 'return 0 if pkgconfig version is at least the given one')
		exactversion: fp.string('exact-version', ` `, '', 'return 0 if pkg version is at least the given one')
		version: fp.bool('version', `v`, false, 'show version of this tool')
		cflags: fp.bool('cflags', `c`, false, 'output all pre-processor and compiler flags')
		cflags_only_path: fp.bool('cflags-only-I', `I`, false, 'show only -I flags from CFLAGS')
		cflags_only_other: fp.bool('cflags-only-other', ` `, false, 'show cflags without -I')
		stat1c: fp.bool('static', `s`, false, 'show --libs for static linking')
		libs: fp.bool('libs', `l`, false, 'output all linker flags')
		libs_only_link: fp.bool('libs-only-l', ` `, false, 'show only -l from ldflags')
		libs_only_path: fp.bool('libs-only-L', `L`, false, 'show only -L from ldflags')
		libs_only_other: fp.bool('libs-only-other', ` `, false, 'show flags not containing -l or -L')
		args: fp.args
	}
}
