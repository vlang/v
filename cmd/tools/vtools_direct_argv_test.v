import os

const vexe = @VEXE
const vroot = os.dir(vexe)

// The launcher and a direct run of the compiled tool give the flag parser the same arguments.
// The first argument has to reach the flag parser either way, so
// `--version` must print the version rather than fail because it was dropped.
fn test_tools_keep_their_first_argument_on_a_direct_run() {
	tpath := os.join_path(os.vtmp_dir(), 'vtools direct argv ${os.getpid()}')
	os.rmdir_all(tpath) or {}
	os.mkdir_all(tpath)!
	defer {
		os.rmdir_all(tpath) or {}
	}
	mut tools := {
		'cover':    'vcover'
		'diff':     'vdiff.v'
		'download': 'vdownload.v'
		'repeat':   'vrepeat.v'
		'time':     'vtime.v'
		'timeout':  'vtimeout.v'
	}
	// vgret only runs on Linux: its host check fails before any argument is read.
	$if linux {
		tools['gret'] = 'vgret.v'
	}
	for name, source in tools {
		mut binary := os.join_path(tpath, name)
		$if windows {
			binary += '.exe'
		}
		build := os.exec([vexe, '-cc', @CCOMPILER, '-o', binary,
			os.join_path(vroot, 'cmd', 'tools', source)])
		assert build.exit_code == 0, '${name}: ${build.output}'
		res := os.exec([binary, '--version'])
		assert res.exit_code == 0, '${name}: ${res.output}'
		launched := os.exec([vexe, name, '--version'])
		assert launched.exit_code == 0, '${name}: ${launched.output}'
		assert launched.output.trim_space().ends_with(res.output.trim_space()), '${name}: ${launched.output}'
		$if linux {
			if name == 'gret' {
				for args in [[binary], [vexe, name]] {
					missing := os.exec(args)
					assert missing.exit_code == 1, missing.output
					assert missing.output.starts_with('Usage: vgret PATH'), missing.output
				}
			}
		}
	}
}
