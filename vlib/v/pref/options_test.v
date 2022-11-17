// vtest flaky: true
// vtest retry: 3
import os
import time

const vexe = @VEXE

const tfolder = os.join_path(os.vtmp_dir(), 'v', 'custom_compile')

fn testsuite_begin() {
	os.mkdir_all(tfolder) or {}
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

fn test_cflags() {
	os.chdir(os.real_path(@VMODROOT)) or {}
	mut debug_arg := '-g3 -O0'
	mut optimised_arg := '-O1'
	$if msvc {
		debug_arg = '/MDd /D_DEBUG'
		optimised_arg = '/O1'
	}
	//
	println('> test whether -cflags is passed to the backend C compiler')
	fail := custom_compile('failing.exe', 'NONSENSE_OPTION')
	assert fail.compilation.exit_code != 0
	println('> NONSENSE_OPTION failed the C build, OK')
	//
	dbg := custom_compile('debug_hw.exe', debug_arg)
	assert dbg.compilation.exit_code == 0
	assert dbg.file_size > 0
	//
	opt := custom_compile('optimised_hw.exe', optimised_arg)
	assert opt.compilation.exit_code == 0
	assert opt.file_size > 0
	//
	$if !tinyc {
		// tcc does almost no optimisations, so the differences are very insignificant
		// optimised_file_size should be smaller in general, but not on the Ubuntu CI for some reason :-|
		// assert opt.file_size != dbg.file_size
		// assert optimised_delta >= debug_delta // this is not reliable on the CIs :-|
	}
	os.rm(opt.exe) or {}
	os.rm(dbg.exe) or {}
}

fn custom_compile(fname string, cflags_options string) Results {
	mut res := Results{}
	res.exe = os.join_path(tfolder, fname)
	res.sw = time.new_stopwatch()
	res.compilation = os.execute('${os.quoted_path(vexe)} -cflags "${cflags_options}" -o ${os.quoted_path(res.exe)} examples/hello_world.v')
	res.delta = res.sw.elapsed().milliseconds()
	res.file_size = os.file_size(res.exe)
	println('> ${fname} build took: ${res.delta} ms with "${cflags_options}", file size: ${res.file_size}')
	return res
}

struct Results {
mut:
	exe         string
	sw          time.StopWatch
	compilation os.Result
	delta       i64
	file_size   u64
}
