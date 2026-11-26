// vtest build: linux || macos
import os

fn weak_checked(args []string) os.Result {
	result := os.exec(args)
	if result.exit_code != 0 { panic('command failed: ${args}\n${result.output}') }
	return result
}

fn test_gc_weak_reference_supported_and_unsupported_collectors() {
	root := @VEXEROOT
	work := os.join_path(os.vtmp_dir(), 'gc_weak_reference_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	fixture := os.join_path(root, 'vlib/builtin/testdata/gc_weak_reference/main.c.v')
	for gc in ['boehm', 'boehm_full_opt', 'boehm_incr_opt', 'boehm_leak', 'none'] {
		for optimized in [false, true] {
			exe := os.join_path(work, '${gc}_${optimized}')
			mut command := [@VEXE, '-no-retry-compilation', '-cc', 'cc', '-gc', gc]
			if gc != 'none' { command << ['-d', 'use_bundled_libgc'] }
			if optimized { command << '-prod' }
			command << ['-o', exe, fixture]
			weak_checked(command)
			if gc in ['boehm', 'boehm_full_opt', 'boehm_incr_opt'] {
				for selector in ['lifetime', 'rejection', 'find-leak'] {
					weak_checked([exe, selector])
				}
			} else {
				weak_checked([exe, 'unsupported'])
			}
		}
	}
}

fn test_gc_weak_reference_failure_does_not_publish_or_explicitly_free_cell() {
	$if linux {
		work := os.join_path(os.vtmp_dir(), 'gc_weak_failure_${os.getpid()}')
		os.mkdir_all(work)!
		defer { os.rmdir_all(work) or {} }
		fixture := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/gc_weak_reference/faults/main.c.v')
		for optimized in [false, true] {
			exe := os.join_path(work, 'fault_${optimized}')
			mut command := [@VEXE, '-no-retry-compilation', '-cc', 'cc', '-gc', 'boehm', '-d',
				'dynamic_boehm']
			if optimized { command << '-prod' }
			command << ['-o', exe, fixture]
			weak_checked(command)
			for selector in ['1', '2', '3'] { weak_checked([exe, selector]) }
		}
	}
}
