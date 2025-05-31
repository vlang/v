import os
import time

const compare_prod = '-prod' in os.args

const cleanup_tmp = '-no-cleanup' !in os.args

fn gbranch() string {
	return os.execute(r'git branch --list|grep ^\*').output.trim_left('* ').trim_space()
}

fn gcommit() string {
	return os.execute(r'git rev-parse --short=7 HEAD').output.trim_left('* ').trim_space()
}

fn r(cmd string) {
	res := os.system(cmd)
	if res != 0 {
		eprintln('> failed running: `${cmd}`')
		exit(1)
	}
}

fn xtime(cmd string) {
	$if linux {
		r('/usr/bin/time -f "CPU: %Us\tReal: %es\tElapsed: %E\tRAM: %MKB\t%C" ${cmd}')
		return
	}
	$if macos {
		r('/opt/homebrew/bin/gtime -f "CPU: %Us\tReal: %es\tElapsed: %E\tRAM: %MKB\t%C" ${cmd}')
		return
	}
	// Pure V fallback - no memory stats, but better than nothing ...
	before := time.now()
	r(cmd)
	after := time.now()
	delta_time := after - before
	println('> Elapsed time: ${delta_time.milliseconds()} ms, for cmd: ${cmd}')
}

fn show_size(fpath string) {
	println('>Size of ${fpath:20s}: ${os.file_size(fpath):10}')
}

fn compare_size(fpath1 string, fpath2 string) {
	size1 := os.file_size(fpath1)
	size2 := os.file_size(fpath2)
	diff_ := i64(size2) - i64(size1)
	println('>>>>>> size("${fpath2:15}") - size("${fpath1:15}") = ${size2:10} - ${size1:10} = ${diff_:10}')
}

fn vcompare(vold string, vnew string) {
	r("v repeat --nmaxs 7 -R 3 '${vold} -check-syntax           examples/hello_world.v' '${vnew} -check-syntax           examples/hello_world.v'")
	r("v repeat --nmaxs 7 -R 3 '${vold} -check                  examples/hello_world.v' '${vnew} -check                  examples/hello_world.v'")
	r("v repeat --nmaxs 7 -R 3 '${vold} -no-parallel -o ohw.c   examples/hello_world.v' '${vnew} -no-parallel -o nhw.c   examples/hello_world.v'")
	compare_size('ohw.c', 'nhw.c')
	r("v repeat --nmaxs 7 -R 3 '${vold} -no-parallel -o ohw.exe examples/hello_world.v' '${vnew} -no-parallel -o nhw.exe examples/hello_world.v'")
	compare_size('ohw.exe', 'nhw.exe')

	r("v repeat --nmaxs 7 -R 3 '${vold} -check-syntax           cmd/v' '${vnew} -check-syntax           cmd/v'")
	r("v repeat --nmaxs 7 -R 3 '${vold} -check                  cmd/v' '${vnew} -check                  cmd/v'")
	r("v repeat --nmaxs 7 -R 3 '${vold} -no-parallel -o ov.c    cmd/v' '${vnew} -no-parallel -o nv.c    cmd/v'")
	compare_size('ov.c', 'nv.c')
	r("v repeat --nmaxs 7 -R 3 '${vold} -no-parallel -o ov.exe  cmd/v' '${vnew} -no-parallel -o nv.exe  cmd/v'")
	compare_size('ov.exe', 'nv.exe')
}

fn hline(header_message string) {
	println('='.repeat(98))
	println(header_message)
}

fn main() {
	// The starting point, when this program should be started, is just after `gh pr checkout NUMBER`.
	start := time.now()

	pr_branch := gbranch()
	hline('Current git branch: ${pr_branch}, commit: ${gcommit()}')
	println('    Compiling new V executables from PR branch: ${pr_branch}, commit: ${gcommit()} ...')
	// *not* using v self here is deliberate, so that the `v` executable itself, is not changed after running this script
	xtime('./v     -o vnew1 cmd/v')
	xtime('./vnew1 -o vnew2 cmd/v')
	xtime('./vnew2 -no-parallel -o vnew cmd/v')
	xtime('./vnew -no-parallel -o nhw_current.c examples/hello_world.v')
	xtime('./vnew -no-parallel -o nv_current.c cmd/v')
	if compare_prod {
		xtime('./vnew -no-parallel -prod -o vnew_prod cmd/v')
	}
	show_size('nhw_current.c')
	show_size('nv_current.c')
	show_size('vnew')
	if compare_prod {
		show_size('vnew_prod')
	}
	r('rm -rf vnew1 vnew2')

	r('git checkout master')
	master_branch := gbranch()
	hline('    Compiling old V executables from branch: ${master_branch}, commit: ${gcommit()} ...')
	xtime('./v     -o vold1 cmd/v')
	xtime('./vold1 -o vold2 cmd/v')
	xtime('./vold2 -no-parallel -o vold cmd/v')
	xtime('./vold -no-parallel -o ohw_master.c examples/hello_world.v')
	xtime('./vold -no-parallel -o ov_master.c  cmd/v')
	if compare_prod {
		xtime('./vold -no-parallel -prod -o vold_prod cmd/v')
		show_size('vold_prod')
	}
	show_size('ohw_master.c')
	show_size('ov_master.c')
	show_size('vold')
	if compare_prod {
		show_size('vold_prod')
	}
	r('rm -rf vold1 vold2')

	hline('File sizes so far ...')
	compare_size('ohw_master.c', 'nhw_current.c')
	compare_size('ov_master.c', 'nv_current.c')
	compare_size('vold', 'vnew')

	r('git checkout ${pr_branch}')
	// we are on the PR branch again
	hline('    Measuring at PR branch: ${pr_branch}, commit: ${gcommit()} ...')
	if compare_prod {
		vcompare('./vold_prod', './vnew_prod')
	} else {
		vcompare('./vold', './vnew')
	}
	println('Done. Total time: ${(time.now() - start).seconds()} s.')

	hline('Final summary for file diff sizes on their own branches:')
	compare_size('ohw_master.c', 'nhw_current.c')
	compare_size('ov_master.c', 'nv_current.c')
	compare_size('vold', 'vnew')
	if compare_prod {
		compare_size('vold_prod', 'vnew_prod')
	}

	hline('Final summary for file diff sizes for generated files on the *current* branch:')
	compare_size('ohw.c', 'nhw.c')
	compare_size('ov.c', 'nv.c')
	compare_size('ohw.exe', 'nhw.exe')
	compare_size('ov.exe', 'nv.exe')

	// After all the measurements are done, delete all the generated temporary files,
	// except the `vold` and `vnew` compilers, so that they can be used later in manual
	// experiments:
	if cleanup_tmp {
		r('rm -rf ohw* nhw* nv* ov*')
	}
}
