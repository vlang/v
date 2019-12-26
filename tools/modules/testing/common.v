module testing

import (
	os
	term
	benchmark
	filepath
)

pub struct TestSession {
pub mut:
	files []string
	vexe string
	vargs string
	failed bool
	benchmark benchmark.Benchmark

	ok string
	fail string
}

pub fn new_test_session(vargs string) TestSession {
	return TestSession{
		vexe: vexe_path()
		vargs: vargs
	}
}

pub fn vexe_path() string {
	// NB: tools extracted from v require that the VEXE 
	// environment variable contains the path to the v executable location.
	// They are usually launched by vlib/compiler/vtools.v,
	// launch_tool/1 , which provides it.
	return os.getenv('VEXE')
}


pub fn (ts mut TestSession) init() {
	ts.ok   = term.ok_message('OK')
	ts.fail = term.fail_message('FAIL')
	ts.benchmark = benchmark.new_benchmark()
}

pub fn (ts mut TestSession) test() {
	ts.init()
	show_stats := '-stats' in ts.vargs.split(' ')
	for dot_relative_file in ts.files {
		relative_file := dot_relative_file.replace('./', '')
		file := os.realpath( relative_file )
		$if windows {
			if file.contains('sqlite') { continue }
		}
		$if !macos {
			if file.contains('customer') { continue }
		}
		$if msvc {
			if file.contains('asm') { continue }
		}
		$if tinyc {
			if file.contains('asm') { continue }
		}
		tmpc_filepath := file.replace('.v', '.tmp.c')

		cmd := '"$ts.vexe" $ts.vargs "$file"'
		//eprintln('>>> v cmd: $cmd')

		ts.benchmark.step()
		if show_stats {
			eprintln('-------------------------------------------------')
			status := os.system(cmd)
			if status == 0 {
				ts.benchmark.ok()
			}else{
				ts.benchmark.fail()
				ts.failed = true
				continue
			}
		}else{
			r := os.exec(cmd) or {
				ts.benchmark.fail()
				ts.failed = true
				eprintln(ts.benchmark.step_message('$relative_file ${ts.fail}'))
				continue
			}
			if r.exit_code != 0 {
				ts.benchmark.fail()
				ts.failed = true
				eprintln(ts.benchmark.step_message('$relative_file ${ts.fail}\n`$file`\n (\n$r.output\n)'))
			} else {
				ts.benchmark.ok()
				eprintln(ts.benchmark.step_message('$relative_file ${ts.ok}'))
			}
		}
		os.rm( tmpc_filepath )
	}
	ts.benchmark.stop()
	eprintln(term.h_divider())
}

pub fn vlib_should_be_present( parent_dir string ) {
	vlib_dir := filepath.join( parent_dir, 'vlib' )
	if !os.is_dir( vlib_dir ){
		eprintln('$vlib_dir is missing, it must be next to the V executable')
		exit(1)
	}
}

pub fn v_build_failing(zargs string, folder string) bool {
	main_label := 'Building $folder ...'
	finish_label := 'building $folder'
	vexe := vexe_path()
	parent_dir := filepath.dir(vexe)
	vlib_should_be_present( parent_dir )
	vargs := zargs.replace(vexe, '')

	eprintln(main_label)
	eprintln('   v compiler args: "$vargs"')

	mut session := new_test_session( vargs )
	files := os.walk_ext(filepath.join(parent_dir, folder),'.v')
	mains := files.filter(!it.contains('modules'))
	mut rebuildable_mains := mains
	if os.user_os() == 'windows' {
		// on windows, an executable can not be rebuilt, while it is running
		myself := os.executable().replace('.exe', '') + '.v'
		mains_without_myself := mains.filter(!it.contains(myself))
		rebuildable_mains = mains_without_myself // workaround a bug in it.contains generation
	}
	session.files << rebuildable_mains
	session.test()
	eprintln( session.benchmark.total_message( finish_label ) )

	return session.failed
}

pub fn build_v_cmd_failed (cmd string) bool {
	res := os.exec(cmd) or {
		return true
	}
	if res.exit_code != 0 {
		eprintln('')
		eprintln( res.output )
		return true
	}
	return false
}

pub fn building_any_v_binaries_failed() bool {
	eprintln('Building V binaries...')
	eprintln('VFLAGS is: "' + os.getenv('VFLAGS') + '"')
	vexe := testing.vexe_path()
	parent_dir := filepath.dir(vexe)
	testing.vlib_should_be_present( parent_dir )
	os.chdir( parent_dir )

	mut failed := false
	v_build_commands := [
		'$vexe -o v_g             -g  v.v',
		'$vexe -o v_prod_g  -prod -g  v.v',
		'$vexe -o v_cg            -cg v.v',
		'$vexe -o v_prod_cg -prod -cg v.v',
		'$vexe -o v_prod    -prod     v.v',
	]

	mut bmark := benchmark.new_benchmark()
	bok   := term.ok_message('OK')
	bfail := term.fail_message('FAIL')
	for cmd in v_build_commands {
		bmark.step()
		if build_v_cmd_failed(cmd) {
			bmark.fail()
			failed = true
			eprintln(bmark.step_message('$cmd => ${bfail} . See details above ^^^^^^^'))
			eprintln('')
			continue
		}
		bmark.ok()
		eprintln(bmark.step_message('$cmd => ${bok}'))
	}
	bmark.stop()
	eprintln(term.h_divider())  
	eprintln( bmark.total_message( 'building v binaries' ) )

	return failed
}
