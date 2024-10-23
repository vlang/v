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
	println('>Size of ${vold}: ${os.file_size(vold)}')
	println('>Size of ${vnew}: ${os.file_size(vnew)}')

	r("v repeat --nmaxs 7 -R 3 '${vold} -check-syntax examples/hello_world.v' '${vnew} -check-syntax examples/hello_world.v'")
	r("v repeat --nmaxs 7 -R 3 '${vold} -check        examples/hello_world.v' '${vnew} -check        examples/hello_world.v'")
	r("v repeat --nmaxs 7 -R 3 '${vold} -o  ohw.c     examples/hello_world.v' '${vnew} -o  nhw.c     examples/hello_world.v'")
	println('>Size of ohw.c: ${os.file_size('ohw.c')}')
	println('>Size of nhw.c: ${os.file_size('nhw.c')}')
	r("v repeat --nmaxs 7 -R 3 '${vold} -o  ohw.exe   examples/hello_world.v' '${vnew} -o  nhw.exe   examples/hello_world.v'")
	println('>Size of ohw.exe: ${os.file_size('ohw.exe')}')
	println('>Size of nhw.exe: ${os.file_size('nhw.exe')}')

	r("v repeat --nmaxs 7 -R 3 '${vold} -check-syntax cmd/v'                  '${vnew} -check-syntax cmd/v'")
	r("v repeat --nmaxs 7 -R 3 '${vold} -check        cmd/v'                  '${vnew} -check        cmd/v'")
	r("v repeat --nmaxs 7 -R 3 '${vold} -o  ov.c      cmd/v'                  '${vnew} -o  nv.c      cmd/v'")
	println('>Size of ov.c: ${os.file_size('ov.c')}')
	println('>Size of nv.c: ${os.file_size('nv.c')}')
	r("v repeat --nmaxs 7 -R 3 '${vold} -o  ov.exe    cmd/v'                  '${vnew} -o  nv.exe    cmd/v'")
	println('>Size of ov.exe: ${os.file_size('ov.exe')}')
	println('>Size of nv.exe: ${os.file_size('nv.exe')}')

	// After all the measurements are done, delete all the generated temporary files,
	// except the `vold` and `vnew` compilers, so that they can be used later in manual
	// experiments:
	r('rm -rf ohw* nhw* nv* ov*')
}

fn main() {
	start := time.now()
	// The starting point, when this program should be started, is just after `gh pr checkout NUMBER`.
	pr_branch := gbranch()
	println('Current git branch: ${pr_branch}, commit: ${gcommit()}')
	println('    Compiling new V executables from PR branch: ${pr_branch}, commit: ${gcommit()} ...')
	// *not* using v self here is deliberate, so that the `v` executable itself, is not changed after running this script
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
	println('Done. Total time: ${(time.now() - start).seconds()} s.')
}
