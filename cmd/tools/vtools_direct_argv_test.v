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
	tools := {
		'diff':     'vdiff.v'
		'download': 'vdownload.v'
		'repeat':   'vrepeat.v'
		'time':     'vtime.v'
		'timeout':  'vtimeout.v'
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
	}
}
