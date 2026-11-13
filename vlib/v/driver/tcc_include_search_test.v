module driver

import os
import v.modulecache

fn test_compile_input_args_are_the_compile_args_without_output_inputs_and_link_options() {
	args := ['-std=gnu11', '-fPIC', '-B/tcc/lib', '-I/tcc/lib/include', '-L/tcc/lib', '-w',
		'-Werror=implicit-function-declaration', '-bt25', '-o', 'out', 'src.c', '/cache/builtin.o',
		'-DGC_THREADS=1', '-I', '/gc/include', '/tcc/lib/libgc.dylib', '-Wl,-rpath,/tcc/lib', '-ldl',
		'-lpthread', '-L', '/usr/local/lib', '-framework', 'Cocoa']
	assert v3_tcc_compile_input_args(args, 'src.c') == ['-std=gnu11', '-fPIC', '-B/tcc/lib',
		'-I/tcc/lib/include', '-w', '-Werror=implicit-function-declaration', '-DGC_THREADS=1',
		'-I', '/gc/include']
	first, system, own := v3_tcc_include_dirs(v3_tcc_compile_input_args(args, 'src.c'))
	assert first == ['/tcc/lib/include', '/gc/include']
	assert system == []
	assert own == ['/tcc/lib/include']
	first_only, system_only, own_only := v3_tcc_include_dirs(['-B/tcc/lib', '-I/a', '-isystem',
		'/sys', '-isystem/other', '-I', '/b'])
	assert first_only == ['/a', '/b']
	assert system_only == ['/sys', '/other']
	assert own_only == ['/tcc/lib/include']
}

fn test_has_include_names_are_found() {
	source := '#if __has_include(<wchar.h>)
#include <wchar.h>
#endif
#if defined(__has_include) && __has_include( "local/config.h" )
#endif
#if __has_include_next(<stdint.h>)
#endif
int __has_include_is_not_asked_here;
'
	assert v3_has_include_names(source) == ['wchar.h', 'local/config.h', 'stdint.h']
}

fn test_include_directories_of_the_environment_come_after_those_of_the_command() {
	saved := ['CPATH', 'C_INCLUDE_PATH'].map(os.getenv_opt(it))
	defer {
		for i, name in ['CPATH', 'C_INCLUDE_PATH'] {
			if value := saved[i] {
				os.setenv(name, value, true)
			} else {
				os.unsetenv(name)
			}
		}
	}
	os.setenv('CPATH', ['/env/first', '', '/env/second', '/env/first'].join(os.path_delimiter),
		true)
	assert v3_environment_dirs('CPATH') == ['/env/first', '/env/second']
	os.unsetenv('CPATH')
	assert v3_environment_dirs('CPATH') == []
	assert v3_tcc_absolute_include_dirs(['/a', 'inc', '../shared', '/a'], '/build/dir') == [
		'/a',
		'/build/dir/inc',
		'/build/shared',
	]
	// TinyCC searches the directories of `-I`, of CPATH, of `-isystem`, of
	// C_INCLUDE_PATH and its own, in that order, and then those of the system.
	os.setenv('CPATH', '/env/cpath', true)
	os.setenv('C_INCLUDE_PATH', '/env/c_include', true)
	manager := modulecache.new_manager(os.join_path(os.vtmp_dir(), 'v3_driver_tcc_search_${os.getpid()}'),
		'salt', false, '', '')
	search := v3_tcc_include_search(&manager, '/nonexistent/tcc', ['-B/tcc/lib', '-isystem', '/sys',
		'-I/option'], '/build/dir')
	assert search.dirs == ['/option', '/env/cpath', '/sys', '/env/c_include', '/tcc/lib/include']
	assert search.frameworks == []
	assert search.absent == []
}
