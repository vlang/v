// venv.v prints the environment variables that steer the V compiler and its
// tools. It reports the value V would actually use, so an unset variable shows
// its default instead of an empty string.
module main

import os
import json2
import flag
import runtime
import v.pref
import v.util.version

// EnvEntry is one reported setting: its name, what it controls, and whether the
// value shown is the raw environment value or one V derives from it.
struct EnvEntry {
	name        string
	description string
	derived     bool
}

const settings = [
	EnvEntry{
		name:        'VEXE'
		description: 'the V executable that is running'
		derived:     true
	},
	EnvEntry{
		name:        'VROOT'
		description: 'the V source tree that VEXE belongs to'
		derived:     true
	},
	EnvEntry{
		name:        'VOS'
		description: 'the operating system V targets by default'
		derived:     true
	},
	EnvEntry{
		name:        'VARCH'
		description: 'the architecture V targets by default'
		derived:     true
	},
	EnvEntry{
		name:        'VVERSION'
		description: 'the version and hash of the running compiler'
		derived:     true
	},
	EnvEntry{
		name:        'VMODULES'
		description: 'where vpm modules are installed and looked up'
		derived:     true
	},
	EnvEntry{
		name:        'VTMP'
		description: 'the writable folder for temporary files'
		derived:     true
	},
	EnvEntry{
		name:        'V3CACHE'
		description: 'the base folder for the v3 module and object caches'
		derived:     true
	},
	EnvEntry{
		name:        'VTOOLS_CACHE_DIR'
		description: 'where compiled cmd/tools binaries are cached'
		derived:     true
	},
	EnvEntry{
		name:        'VCACHE'
		description: 'the object cache folder that `v wipe-cache` clears'
		derived:     true
	},
	EnvEntry{
		name:        'VFLAGS'
		description: 'extra flags applied to every V invocation'
		derived:     false
	},
	EnvEntry{
		name:        'VOSARGS'
		description: 'replaces the whole command line of every V invocation'
		derived:     false
	},
	EnvEntry{
		name:        'CC'
		description: 'the C compiler, instead of the one V picks'
		derived:     false
	},
	EnvEntry{
		name:        'CFLAGS'
		description: 'extra flags for the C compiler'
		derived:     false
	},
	EnvEntry{
		name:        'LDFLAGS'
		description: 'extra flags for the C linker'
		derived:     false
	},
	EnvEntry{
		name:        'VJOBS'
		description: 'how many parallel jobs V runs'
		derived:     true
	},
	EnvEntry{
		name:        'VERROR_PATHS'
		description: 'set to `absolute` to keep full paths in error messages'
		derived:     false
	},
	EnvEntry{
		name:        'VCOLORS'
		description: 'set to `always` or `never` to control colored output'
		derived:     false
	},
	EnvEntry{
		name:        'VCOVDIR'
		description: 'the folder coverage reports are written to'
		derived:     false
	},
	EnvEntry{
		name:        'VSTARTUP'
		description: 'a file the REPL runs at startup'
		derived:     false
	},
	EnvEntry{
		name:        'VQUIET'
		description: 'set to any value to silence the REPL'
		derived:     false
	},
]

// setting returns the reported entry named `name`.
fn setting(name string) ?EnvEntry {
	for entry in settings {
		if entry.name == name {
			return entry
		}
	}
	return none
}

// setting_names returns every reported name, space separated.
fn setting_names() string {
	return settings.map(it.name).join(' ')
}

// v_executable returns the V compiler the user invoked. This tool normally runs
// as a cached copy of itself under the tool cache, so `os.executable()` is that
// copy; the launcher exports VEXE to point at the real compiler instead.
fn v_executable() string {
	return os.real_path(os.getenv_opt('VEXE') or { os.executable() })
}

// value returns the value in use for `entry`. A derived setting is resolved the
// way the compiler resolves it, so an unset variable shows its default.
fn value(entry EnvEntry) string {
	if !entry.derived {
		return os.getenv(entry.name)
	}
	return match entry.name {
		'VEXE' {
			v_executable()
		}
		'VROOT' {
			os.dir(v_executable())
		}
		'VOS' {
			@OS
		}
		'VARCH' {
			pref.host_arch()
		}
		'VVERSION' {
			version.full_v_version(true)
		}
		'VMODULES' {
			os.vmodules_paths().join(os.path_delimiter)
		}
		'VTMP' {
			os.vtmp_dir()
		}
		'V3CACHE' {
			os.getenv_opt('V3CACHE') or { os.vtmp_dir() }
		}
		'VTOOLS_CACHE_DIR' {
			os.getenv_opt('VTOOLS_CACHE_DIR') or { os.join_path(os.cache_dir(), 'v', 'tools') }
		}
		'VCACHE' {
			os.getenv_opt('VCACHE') or { os.join_path(os.vmodules_dir(), '.cache') }
		}
		'VJOBS' {
			runtime.nr_jobs().str()
		}
		else {
			os.getenv(entry.name)
		}
	}
}

// values returns the value in use for every reported setting.
fn values() map[string]string {
	mut result := map[string]string{}
	for entry in settings {
		result[entry.name] = value(entry)
	}
	return result
}

fn print_one(name string) {
	entry := setting(name) or {
		eprintln('v env: unknown setting `${name}`.')
		eprintln('Known settings: ${setting_names()}')
		exit(1)
	}
	println(value(entry))
}

fn print_all() {
	for entry in settings {
		println('${entry.name}=${json2.encode(value(entry))}')
	}
}

fn print_all_json() {
	println(json2.encode(values()))
}

// normalize_flags rewrites the single dash spellings in `args`, because Go writes
// its options with one dash and `v env -json` has to work next to `--json`.
fn normalize_flags(args []string) []string {
	mut result := []string{cap: args.len}
	for arg in args {
		result << match arg {
			'-json' { '--json' }
			'-h', '-help' { '--help' }
			else { arg }
		}
	}
	return result
}

fn main() {
	passed := os.args[1..]
	// `v env ...` reaches this tool with the `env` word still in the arguments.
	stripped := if passed.len > 0 && passed[0] == 'env' { passed[1..] } else { passed }
	mut fp := flag.new_flag_parser(normalize_flags(stripped))
	fp.application('v env')
	fp.version('0.0.1')
	fp.description('Print the environment variables that steer the V compiler and its tools.')
	fp.arguments_description('[NAME]')
	is_json := fp.bool('json', ` `, false, 'Print the output as JSON.')
	free_args := fp.finalize() or {
		eprintln('v env: ${err.msg()}')
		println(fp.usage())
		exit(1)
	}
	if free_args.len > 1 {
		eprintln('v env: expected at most one setting name, got ${free_args.len}.')
		eprintln('Known settings: ${setting_names()}')
		exit(1)
	}
	if free_args.len == 1 {
		print_one(free_args[0])
		return
	}
	if is_json {
		print_all_json()
	} else {
		print_all()
	}
}
