module fastc

import os

fn test_discard_prestarted_workers_without_feeding_source() {
	$if !macos && !linux {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	root := os.join_path(os.vtmp_dir(), 'fastc_unused_workers_${os.getpid()}')
	mut workers := fastc_prestart_c_units(tcc, ['-std=gnu11'], root, 2)
	assert workers.active
	fastc_discard_prestarted_c_units(mut workers)
	assert !workers.active
	assert !os.exists(root)
}
