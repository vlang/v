import os

fn test_native_helpers_share_freestanding_capabilities_and_cache_identity() ! {
	$if (linux || macos) && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_native_flags_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		fixtures := os.join_path(@VEXEROOT, 'vlib', 'v', 'compiler_tests', 'testdata', 'freestanding_native_flags')
		// Build the same native source hosted, freestanding, then warm again.
		// Distinct capability results prove the cache does not reuse its hosted object.
		for role in ['hosted', 'bare', 'bare_warm'] {
			program := os.join_path(work, role)
			mut command := [@VEXE, '-cc', 'cc', '-gc', 'none']
			if role != 'hosted' { command << '-freestanding' }
			command << ['-o', program, os.join_path(fixtures, 'main.c.v')]
			compiled := os.exec(command)
			assert compiled.exit_code == 0, '${role}: ${compiled.output}'
			result := os.exec([program])
			assert result.exit_code == 0, '${role}: ${result.output}'
			assert result.output.trim_space() == 'native runtime flags passed'
		}
		mut library := os.join_path(work, 'libflags.so')
		$if macos {
			library = os.join_path(work, 'libflags.dylib')
		}
		compiled := os.exec([@VEXE, '-cc', 'cc', '-freestanding', '-gc', 'none', '-shared', '-o',
			library, os.join_path(fixtures, 'library.c.v')])
		assert compiled.exit_code == 0, compiled.output
		host := os.join_path(work, 'host')
		mut command := ['cc', '-o', host, os.join_path(fixtures, 'host.c')]
		$if linux {
			command << '-ldl'
		}
		linked := os.exec(command)
		assert linked.exit_code == 0, linked.output
		result := os.exec([host, library])
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == 'native library runtime flags passed'
	}
}
