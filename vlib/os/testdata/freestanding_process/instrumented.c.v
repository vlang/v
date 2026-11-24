module main

import os

#include "@DIR/oracles.h"

fn C.process_track_allocator()

fn main() {
	$if process_track_alloc ? {
		C.process_track_allocator()
	}
	for mode in ['exec', 'missing', 'stdin', 'stdin_missing', 'cwd', 'group'] {
		executable := if mode == 'missing' { '/v_process_missing_78231' } else { '/usr/bin/true' }
		mut p := os.new_process(executable)
		p.set_environment({})
		p.set_redirect_stdio()
		if mode == 'stdin' { p.set_stdin_path('/dev/null') }
		if mode == 'stdin_missing' { p.set_stdin_path('/v_process_missing_78231') }
		if mode == 'cwd' { p.work_folder = '/v_process_missing_78231' }
		if mode == 'group' { p.use_pgroup = true }
		p.wait()
		message := p.stderr_slurp()
		expected := if mode in ['missing', 'stdin_missing', 'cwd'] { 1 } else { 0 }
		if p.code != expected || (expected == 0 && message != '') || (expected == 1 && !message.ends_with('; code: 2\n')) || message.contains('> trace') {
			eprintln('instrumented child ${p.code}: ${message}')
			exit(79)
		}
		p.close()
	}
	println('instrumented Process passed')
}
