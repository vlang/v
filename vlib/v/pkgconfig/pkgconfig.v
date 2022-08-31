module pkgconfig

import semver
import os

const (
	default_paths = [
		'/usr/local/lib/x86_64-linux-gnu/pkgconfig',
		'/usr/local/lib64/pkgconfig',
		'/usr/local/lib/pkgconfig',
		'/usr/local/share/pkgconfig',
		'/usr/lib/x86_64-linux-gnu/pkgconfig',
		'/usr/lib/aarch64-linux-gnu/pkgconfig',
		'/usr/lib64/pkgconfig',
		'/usr/lib/pkgconfig',
		'/usr/share/pkgconfig',
		'/opt/homebrew/lib/pkgconfig', // Brew on macOS
		'/usr/local/libdata/pkgconfig', // FreeBSD
		'/usr/lib/i386-linux-gnu/pkgconfig', // Debian 32bit
	]
	version       = '0.3.2'
)

pub struct Options {
pub:
	path              string
	debug             bool
	norecurse         bool
	only_description  bool
	use_default_paths bool = true
}

pub struct PkgConfig {
pub mut:
	options          Options
	name             string
	modname          string
	url              string
	version          string
	description      string
	libs             []string
	libs_private     []string
	cflags           []string
	paths            []string // TODO: move to options?
	vars             map[string]string
	requires         []string
	requires_private []string
	conflicts        []string
}

fn (mut pc PkgConfig) parse_list_no_comma(s string) []string {
	return pc.parse_list(s.replace(',', ' '))
}

fn (mut pc PkgConfig) parse_list(s string) []string {
	operators := ['=', '<', '>', '>=', '<=']
	r := pc.parse_line(s.replace('  ', ' ').replace(', ', ' ')).split(' ')
	mut res := []string{}
	mut skip := false
	for a in r {
		b := a.trim_space()
		if skip {
			skip = false
		} else if b in operators {
			skip = true
		} else if b != '' {
			res << b
		}
	}
	return res
}

fn (mut pc PkgConfig) parse_line(s string) string {
	mut r := s.split('#')[0]
	for r.contains('\${') {
		tok0 := r.index('\${') or { break }
		mut tok1 := r[tok0..].index('}') or { break }
		tok1 += tok0
		v := r[tok0 + 2..tok1]
		r = r.replace('\${$v}', pc.vars[v])
	}
	return r.trim_space()
}

fn (mut pc PkgConfig) setvar(line string) {
	kv := line.trim_space().split('=')
	if kv.len == 2 {
		k := kv[0]
		v := pc.parse_line(kv[1])
		pc.vars[k] = pc.parse_line(v)
	}
}

fn (mut pc PkgConfig) parse(file string) bool {
	data := os.read_file(file) or { return false }
	if pc.options.debug {
		eprintln(data)
	}
	lines := data.split('\n')
	if pc.options.only_description {
		// 2x faster than original pkg-config for --list-all --description
		for line in lines {
			if line.starts_with('Description: ') {
				pc.description = pc.parse_line(line[13..])
			}
		}
	} else {
		for line in lines {
			if line.starts_with('#') {
				continue
			}
			if line.contains('=') && !line.contains(' ') {
				pc.setvar(line)
				continue
			}
			if line.starts_with('Name:') {
				pc.name = pc.parse_line(line[5..])
			} else if line.starts_with('Description:') {
				pc.description = pc.parse_line(line[12..])
			} else if line.starts_with('Version:') {
				pc.version = pc.parse_line(line[8..])
			} else if line.starts_with('Requires:') {
				pc.requires = pc.parse_list_no_comma(line[9..])
			} else if line.starts_with('Requires.private:') {
				pc.requires_private = pc.parse_list_no_comma(line[17..])
			} else if line.starts_with('Conflicts:') {
				pc.conflicts = pc.parse_list_no_comma(line[10..])
			} else if line.starts_with('Cflags:') {
				pc.cflags = pc.parse_list(line[7..])
			} else if line.starts_with('Libs:') {
				pc.libs = pc.parse_list(line[5..])
			} else if line.starts_with('Libs.private:') {
				pc.libs_private = pc.parse_list(line[13..])
			} else if line.starts_with('URL:') {
				pc.url = pc.parse_line(line[4..])
			}
		}
	}
	return true
}

fn (mut pc PkgConfig) resolve(pkgname string) ?string {
	if pkgname.ends_with('.pc') {
		if os.exists(pkgname) {
			return pkgname
		}
	} else {
		if pc.paths.len == 0 {
			pc.paths << '.'
		}
		for path in pc.paths {
			file := '$path/${pkgname}.pc'
			if os.exists(file) {
				return file
			}
		}
	}
	return error('Cannot find "$pkgname" pkgconfig file')
}

pub fn atleast(v string) bool {
	v0 := semver.from(pkgconfig.version) or { return false }
	v1 := semver.from(v) or { return false }
	return v0.gt(v1)
}

pub fn (mut pc PkgConfig) atleast(v string) bool {
	v0 := semver.from(pc.version) or { return false }
	v1 := semver.from(v) or { return false }
	return v0.gt(v1)
}

pub fn (mut pc PkgConfig) extend(pcdep &PkgConfig) ?string {
	for flag in pcdep.cflags {
		if pc.cflags.index(flag) == -1 {
			pc.cflags << flag
		}
	}
	for lib in pcdep.libs {
		if pc.libs.index(lib) == -1 {
			pc.libs << lib
		}
	}
	for lib in pcdep.libs_private {
		if pc.libs_private.index(lib) == -1 {
			pc.libs_private << lib
		}
	}
	return none
}

fn (mut pc PkgConfig) load_requires() ? {
	for dep in pc.requires {
		pc.load_require(dep)?
	}
	for dep in pc.requires_private {
		pc.load_require(dep)?
	}
}

fn (mut pc PkgConfig) load_require(dep string) ? {
	mut pcdep := PkgConfig{
		paths: pc.paths
	}
	depfile := pcdep.resolve(dep) or {
		if pc.options.debug {
			eprintln('cannot resolve $dep')
		}
		return error('could not resolve dependency $dep')
	}
	if !pcdep.parse(depfile) {
		return error('required file "$depfile" could not be parsed')
	}
	pcdep.load_requires()?
	pc.extend(pcdep)?
}

fn (mut pc PkgConfig) add_path(path string) {
	p := if path.ends_with('/') { path[0..path.len - 1] } else { path }
	if !os.exists(p) {
		return
	}
	if pc.paths.index(p) == -1 {
		pc.paths << p
	}
}

fn (mut pc PkgConfig) load_paths() {
	if pc.options.use_default_paths {
		for path in pkgconfig.default_paths {
			pc.add_path(path)
		}
	}
	for path in pc.options.path.split(':') {
		pc.add_path(path)
	}
	env_var := os.getenv('PKG_CONFIG_PATH')
	if env_var != '' {
		env_paths := env_var.trim_space().split(':')
		for path in env_paths {
			pc.add_path(path)
		}
	}
}

pub fn load(pkgname string, options Options) ?&PkgConfig {
	mut pc := &PkgConfig{
		modname: pkgname
		options: options
	}
	pc.load_paths()
	file := pc.resolve(pkgname) or { return err }
	if !pc.parse(file) {
		return error('file "$file" could not be parsed')
	}
	if !options.norecurse {
		pc.load_requires()?
	}
	return pc
}

pub fn list() []string {
	mut pc := &PkgConfig{
		options: Options{}
	}
	pc.load_paths()
	mut modules := []string{}
	for path in pc.paths {
		files := os.ls(path) or { continue }
		for file in files {
			if file.ends_with('.pc') {
				name := file.replace('.pc', '')
				if modules.index(name) == -1 {
					modules << name
				}
			}
		}
	}
	return modules
}
