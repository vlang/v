module driver

fn test_msvc_cl_args_translate_an_executable_build() {
	args := msvc_cl_args(['-std=gnu11', '-w', '-fwrapv', '-Wno-int-conversion', '-O3', '-I',
		'C:/v/thirdparty/include', '-DGC_THREADS=1', '-Wl,/STACK:33554432', '-o', 'out.exe', 'src.c',
		'C:/v/thirdparty/obj.o', '-l', 'dbghelp', '-lws2_32', '-lm', '-LC:/libs'], 'windows')
	assert args == ['/nologo', '/volatile:ms', '/we4013', '/utf-8', '/MD', '/w', '/O2',
		'/IC:/v/thirdparty/include', '/DGC_THREADS=1', '/std:c11', '/Feout.exe', 'src.c',
		'C:/v/thirdparty/obj.o', 'kernel32.lib', 'user32.lib', 'advapi32.lib', 'dbghelp.lib',
		'ws2_32.lib', '/link', '/STACK:33554432', '/LIBPATH:C:/libs']
}

fn test_msvc_cl_args_translate_an_object_build() {
	args := msvc_cl_args(['-std=gnu11', '-w', '-DNDEBUG', '-x', 'c', '-o', 'C:/tmp/cJSON.obj',
		'-c', 'C:/v/thirdparty/cJSON/cJSON.c'], 'windows')
	assert args == ['/nologo', '/volatile:ms', '/we4013', '/utf-8', '/MD', '/w', '/DNDEBUG', '/std:c11',
		'/c', '/FoC:/tmp/cJSON.obj', 'C:/v/thirdparty/cJSON/cJSON.c']
}

fn test_msvc_cl_args_translate_shared_and_debug_builds() {
	args := msvc_cl_args(['-g', '-shared', '-o', 'out', 'src.c', '-mwindows'], 'windows')
	assert '/LD' in args
	assert '/Zi' in args
	assert '/Feout' in args
	link := args[args.index('/link') + 1..]
	assert link == ['/SUBSYSTEM:WINDOWS', '/DEBUG']
}

fn test_msvc_cl_args_keep_cpp_sources_out_of_c11_mode() {
	args := msvc_cl_args(['-std=gnu++11', '-x', 'c++', '-o', 'x.obj', '-c', 'x.cpp'], 'windows')
	assert '/std:c11' !in args
	assert args.contains('x.cpp')
}

fn test_v3_msvc_link_flags() {
	assert v3_msvc_link_flags('windows', false, false, .auto, false) == ['-Wl,/STACK:33554432']
	assert v3_msvc_link_flags('windows', false, false, .auto, true) == [
		'-Wl,/STACK:33554432',
		'-Wl,/SUBSYSTEM:WINDOWS',
	]
	assert v3_msvc_link_flags('windows', false, false, .console, true) == [
		'-Wl,/STACK:33554432',
		'-Wl,/SUBSYSTEM:CONSOLE',
	]
	assert v3_msvc_link_flags('windows', true, false, .windows, true) == []
	assert v3_msvc_link_flags('linux', false, false, .windows, true) == []
}
