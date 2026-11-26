import os

fn test_canonical_freestanding_heap_concurrency_and_alignment() ! {
	$if (linux || macos) && (amd64 || arm64) {
		work := os.join_path(os.vtmp_dir(), 'freestanding_heap_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		source := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'freestanding_heap', 'main.c.v')
		for role in ['debug', 'prod'] {
			program := os.join_path(work, role)
			mut command := [@VEXE, '-freestanding', '-gc', 'none', '-enable-globals', '-d', 'vlibc']
			if role == 'prod' { command << '-prod' }
			command << ['-o', program, source]
			compiled := os.exec(command)
			assert compiled.exit_code == 0, '${role}: ${compiled.output}'
			result := os.exec([program])
			assert result.exit_code == 0, '${role}: ${result.output}'
			assert result.output.trim_space() == 'freestanding heap passed'
		}
	}
}
