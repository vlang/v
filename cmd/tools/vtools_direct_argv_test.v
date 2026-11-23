import os
import v.cmdexec

const vexe = @VEXE
const vroot = os.dir(vexe)

fn argv_tool_binary(output string, tool string) ?string {
	// The launcher reports the selected path on a cache hit or a cold build.
	prefixes := ['> reusing the cached `${tool}` at `',
		'> recompiling `${tool}`, because there is no cached binary at `']
	for line in output.split_into_lines() {
		for prefix in prefixes {
			if !line.starts_with(prefix) || !line.ends_with('`') || line.len <= prefix.len + 1 {
				continue
			}
			path := line[prefix.len..line.len - 1]
			if os.is_executable(path) {
				return path
			}
		}
	}
	return none
}

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
	suffix := $if windows { '.exe' } $else { '' }
	cwd := os.getwd()
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
		launched := cmdexec.run_in_merged_with_env(vexe, [name, '--version'], cwd, {
			'VTOOLS_CACHE_VERBOSE': '1'
		})
		assert launched.exit_code == 0, '${name}: ${launched.output}'
		binary := os.join_path(tpath, name + suffix)
		if cached := argv_tool_binary(launched.output, 'v${name}') {
			os.cp(cached, binary)!
		} else {
			// The direct invocation still needs a binary when the launcher runs uncached.
			build := os.exec([vexe, '-cc', @CCOMPILER, '-o', binary,
				os.join_path(vroot, 'cmd', 'tools', source)])
			assert build.exit_code == 0, '${name}: ${build.output}'
		}
		res := os.exec([binary, '--version'])
		assert res.exit_code == 0, '${name}: ${res.output}'
		assert launched.output.trim_space().ends_with(res.output.trim_space()), '${name}: ${launched.output}'
		$if linux {
			if name == 'gret' {
				for missing in [os.exec([binary]), cmdexec.run_in_merged_with_env(vexe, [name], cwd, {
					'VTOOLS_CACHE_VERBOSE': ''
				})] {
					assert missing.exit_code == 1, missing.output
					assert missing.output.starts_with('Usage: vgret PATH'), missing.output
				}
			}
		}
	}
}
