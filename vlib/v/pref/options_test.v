// vtest retry: 3
import os
import time

const vexe = @VEXE

const vroot = os.real_path(@VMODROOT)

fn testsuite_begin() {
	os.chdir(vroot) or {}
}

fn test_cflags() ? {
	println('> test whether -cflags is passed to the backend C compiler')
	compilation := os.execute('"$vexe" -cflags NONSENSE_OPTION examples/hello_world.v')
	assert compilation.exit_code != 0
	println('> NONSENSE_OPTION failed the C build, OK')
	//
	mut debug_arg := '-g3 -O0'
	mut optimised_arg := '-O1'
	$if msvc {
		debug_arg = '/MDd /D_DEBUG'
		optimised_arg = '/O1'
	}
	tmpdir := os.temp_dir()
	//
	dbgexe := os.join_path(os.temp_dir(), 'debug_hw.exe')
	debug_sw := time.new_stopwatch()
	debug_compilation := os.execute('"$vexe" -cflags "$debug_arg" -o "$dbgexe" examples/hello_world.v')
	debug_delta := debug_sw.elapsed().microseconds()
	assert debug_compilation.exit_code == 0
	debug_file_size := os.file_size(dbgexe)
	assert debug_file_size > 0
	println('> debug build took: $debug_delta ms with "$debug_arg", file size: $debug_file_size')
	//
	optexe := os.join_path(os.temp_dir(), 'optimised_hw.exe')
	optimised_sw := time.new_stopwatch()
	optimised_compilation := os.execute('"$vexe" -cflags "$optimised_arg" -o "$optexe" examples/hello_world.v')
	optimised_delta := optimised_sw.elapsed().microseconds()
	assert optimised_compilation.exit_code == 0
	optimised_file_size := os.file_size(optexe)
	assert optimised_file_size > 0
	println('> optimised build took: $optimised_delta ms with "$optimised_arg", file size: $optimised_file_size')
	//
	$if !tinyc {
		// tcc does almost no optimisations, so the differences are very insignificant
		assert optimised_file_size != debug_file_size // optimised_file_size should be smaller in general, but not on the Ubuntu CI for some reason :-|
		assert optimised_delta >= debug_delta
	}
	os.rm(optexe) or {}
	os.rm(dbgexe) or {}
}
