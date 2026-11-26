import os

fn test_persistent_heap_survives_scoped_allocators() ! {
	root := os.join_path(os.vtmp_dir(), 'persistent_heap_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixtures := os.join_path(@VMODROOT, 'vlib/builtin/testdata/persistent_allocation')
	for scope in ['arena', 'prealloc'] {
		output := os.join_path(root, scope)
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', 'clang', '-o', output]
		if scope == 'prealloc' { command << '-prealloc' }
		command << os.join_path(fixtures, scope, 'main.v')
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := os.exec([output])
		assert ran.exit_code == 0, ran.output
		assert ran.output.trim_space() == 'persistent ${scope} ok', ran.output
	}
	output := os.join_path(root, 'negative')
	built := os.exec([@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', 'clang', '-o', output,
		os.join_path(fixtures, 'negative', 'main.v')])
	assert built.exit_code == 0, built.output
	ran := os.exec([output])
	assert ran.exit_code != 0 && ran.output.contains('vcalloc(') && ran.output.contains('< 0'), ran.output
}

fn test_persistent_heap_is_scanned_by_boehm() ! {
	$if !gcboehm ? {
		return
	}
	$if !linux && !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'persistent_heap_gc_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for use_prealloc in [false, true] {
		output := os.join_path(root, 'retention_${use_prealloc}')
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'boehm', '-cc', 'clang', '-o', output]
		if use_prealloc { command << '-prealloc' }
		command << os.join_path(@VMODROOT, 'vlib/builtin/testdata/persistent_allocation/gc/main.c.v')
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := os.exec([output])
		assert ran.exit_code == 0, ran.output
		assert ran.output.trim_space() == 'persistent GC retention ok', ran.output
	}
}

fn test_persistent_heap_releases_once_inside_prealloc_scope() ! {
	$if !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'persistent_heap_release_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	output := os.join_path(root, 'release')
	built := os.exec([@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', 'clang', '-prealloc',
		'-o', output,
		os.join_path(@VMODROOT, 'vlib/builtin/testdata/persistent_allocation/release/main.c.v')])
	assert built.exit_code == 0, built.output
	ran := os.exec([output])
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space() == 'persistent release ok', ran.output
}

fn test_persistent_heap_manual_frees_balance_tracking() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'persistent_heap_tracking_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for bare in [false, true] {
		output := os.join_path(root, 'tracking_${bare}')
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-d', 'track_heap', '-cc',
			'clang', '-o', output]
		if bare { command << '-freestanding' }
		command << os.join_path(@VMODROOT, 'vlib/builtin/testdata/persistent_allocation/tracking/main.c.v')
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := os.exec([output])
		assert ran.exit_code == 0 && ran.output.trim_space() == 'persistent tracking ok', ran.output
	}
}
