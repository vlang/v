import os

fn test_freestanding_arena_scope_ownership_lifetimes_and_threads() ! {
	$if (linux || macos) && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_arena_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(@VEXEROOT, 'vlib', 'arena', 'testdata', 'freestanding', 'main.v')
		for role in ['debug', 'prod'] {
			program := os.join_path(work, role)
			mut command := [@VEXE, '-freestanding', '-gc', 'none', '-enable-globals', '-d', 'vlibc']
			if role == 'prod' { command << '-prod' }
			command << ['-o', program, source]
			compiled := os.exec(command)
			assert compiled.exit_code == 0, '${role}: ${compiled.output}'
			result := os.exec([program])
			assert result.exit_code == 0, '${role}: ${result.output}'
			assert result.output.trim_space() == 'freestanding arena passed'
		}
	}
}

fn test_freestanding_arena_library_on_foreign_host_threads() ! {
	$if linux && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_arena_host_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		fixtures := os.join_path(@VEXEROOT, 'vlib', 'arena', 'testdata', 'freestanding')
		library := os.join_path(work, 'libarena.so')
		compiled := os.exec([@VEXE, '-freestanding', '-gc', 'none', '-shared', '-o', library,
			os.join_path(fixtures, 'library.v')])
		assert compiled.exit_code == 0, compiled.output
		host := os.join_path(work, 'host')
		linked := os.exec(['cc', '-pthread', '-o', host, os.join_path(fixtures, 'host.c'), '-ldl'])
		assert linked.exit_code == 0, linked.output
		result := os.exec([host, library])
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == 'host threads arena passed'
	}
}

fn test_freestanding_arena_backing_allocation_failure() ! {
	$if linux && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_arena_oom_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(@VEXEROOT, 'vlib', 'arena', 'testdata', 'freestanding', 'oom.v')
		program := os.join_path(work, 'oom')
		compiled := os.exec([@VEXE, '-freestanding', '-gc', 'none', '-o', program, source])
		assert compiled.exit_code == 0, compiled.output
		result := os.exec([program])
		assert result.exit_code == 1, result.output
		// Arena OOM now uses the nonallocating memory-panic path.
		assert result.output.trim_space() == 'arena_grow(size)\nmemory allocation failure', result.output
	}
}
