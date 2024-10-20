import os
import time

const compare_prod = '-prod' in os.args

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
	$if windows {
		before := time.now()
		r(cmd)
		after := time.now()
		delta_time := after - before
		println('> Elapsed time: ${delta_time.milliseconds()} ms, for cmd: ${cmd}')
	} $else {
		r('/usr/bin/time -f "CPU: %Us\tReal: %es\tElapsed: %E\tRAM: %MKB\t%C" ${cmd}')
	}
}

fn vcompare(vold string, vnew string) {
	r("v repeat -R 3 '${vold} -check-syntax examples/hello_world.v' '${vnew} -check-syntax examples/hello_world.v'")
	r("v repeat -R 3 '${vold} -check        examples/hello_world.v' '${vnew} -check        examples/hello_world.v'")
	r("v repeat -R 3 '${vold} -o    hw.c    examples/hello_world.v' '${vnew} -o    hw.c    examples/hello_world.v'")
	r("v repeat -R 3 '${vold} -o    hw      examples/hello_world.v' '${vnew} -o    hw      examples/hello_world.v'")
	r("v repeat -R 3 '${vold} -check-syntax cmd/v'                  '${vnew} -check-syntax cmd/v'")
	r("v repeat -R 3 '${vold} -check        cmd/v'                  '${vnew} -check        cmd/v'")
	r("v repeat -R 3 '${vold} -o    ov.c    cmd/v'                  '${vnew} -o    nv.c    cmd/v'")
	r("v repeat -R 3 '${vold} -o    ov      cmd/v'                  '${vnew} -o    nv      cmd/v'")
}

fn main() {
	// The starting point, when this program should be started, is just after `gh pr checkout NUMBER`.
	pr_branch := gbranch()
	println('Current git branch: ${pr_branch}, commit: ${gcommit()}')
	println('    Compiling new V executables from PR branch: ${pr_branch}, commit: ${gcommit()} ...')
	xtime('./v -g self')
	xtime('./v     -o vnew1 cmd/v')
	xtime('./vnew1 -o vnew2 cmd/v')
	xtime('./vnew2 -o vnew  cmd/v')
	r('rm -rf vnew1 vnew2')
	if compare_prod {
		xtime('./vnew -prod -o vnew_prod cmd/v')
	}

	r('git checkout master')
	master_branch := gbranch()
	println('    Compiling old V executables from branch: ${master_branch}, commit: ${gcommit()} ...')
	xtime('./v -g self')
	xtime('./v     -o vold1 cmd/v')
	xtime('./vold1 -o vold2 cmd/v')
	xtime('./vold2 -o vold  cmd/v')
	r('rm -rf vold1 vold2')
	if compare_prod {
		xtime('./vold -prod -o vold_prod cmd/v')
	}

	r('git checkout ${pr_branch}') // we are on the PR branch again

	println('    Measuring at PR branch: ${pr_branch}, commit: ${gcommit()} ...')
	if compare_prod {
		vcompare('./vold_prod', './vnew_prod')
	} else {
		vcompare('./vold', './vnew')
	}
	r('rm -rf hw nv ov hw.exe nv.exe ov.exe hw.c nv.c ov.c')
	println('Done.')
}
