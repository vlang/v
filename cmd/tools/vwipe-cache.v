module main

import os

// The compiler implements `v wipe-cache` itself, in cmd/v/toolcache.v: a compiled tool lives in
// the tool cache, and a wipe should not run from a folder that it removes. This program hands its
// arguments to that command, for whatever still builds or runs the tool directly, so that there
// is one implementation of the wipe.

// wipe_cache_forwarded_env marks a `v wipe-cache` that this program started.
const wipe_cache_forwarded_env = 'V_WIPE_CACHE_FORWARDED'

fn main() {
	passed := os.args[1..]
	command_args := if passed.len > 0 && passed[0] == 'wipe-cache' {
		passed[1..]
	} else {
		passed
	}
	if os.getenv(wipe_cache_forwarded_env) != '' {
		// The V executable handed the command back to this tool instead of running it.
		eprintln('v wipe-cache: the V executable does not implement `wipe-cache` itself; update V.')
		exit(1)
	}
	vexe := wipe_cache_compiler() or {
		eprintln('v wipe-cache: ${err.msg()}')
		exit(1)
	}
	mut environment := os.environ()
	environment[wipe_cache_forwarded_env] = '1'
	environment['VEXE'] = vexe
	mut forwarded := ['wipe-cache']
	forwarded << command_args
	mut process := os.new_process(vexe)
	process.set_args(forwarded)
	process.set_environment(environment)
	process.run()
	process.wait()
	code := process.code
	process.close()
	exit(code)
}

// wipe_cache_compiler returns the V executable that runs the command: the one that built this
// program, or else the one that `VEXE` names.
fn wipe_cache_compiler() !string {
	for candidate in [@VEXE, os.getenv('VEXE')] {
		if candidate != '' && os.is_file(candidate) && os.is_executable(candidate) {
			return candidate
		}
	}
	return error('cannot find the V executable to run `wipe-cache` with')
}
