module driver

import v.pref

fn test_msvc_cl_args_translate_an_executable_build() {
	args := msvc_cl_args(['-std=gnu11', '-w', '-fwrapv', '-Wno-int-conversion', '-O3', '-I',
		'C:/v/thirdparty/include', '-DGC_THREADS=1', '-Wl,/STACK:33554432', '-o', 'out.exe', 'src.c',
		'C:/v/thirdparty/obj.o', '-l', 'dbghelp', '-lws2_32', '-lm', '-LC:/libs'], 'windows')
	assert args == ['/nologo', '/volatile:ms', '/bigobj', '/MD', '/we4013', '/utf-8', '/w', '/O2',
		'/IC:/v/thirdparty/include', '/DGC_THREADS=1', '/std:c11', '/D_CRT_DECLARE_NONSTDC_NAMES=1',
		'/Feout.exe', 'src.c', 'C:/v/thirdparty/obj.o', 'kernel32.lib', 'user32.lib', 'advapi32.lib',
		'dbghelp.lib', 'ws2_32.lib', '/link', '/STACK:33554432', '/LIBPATH:C:/libs']
}

fn test_msvc_cl_object_args_translate_a_third_party_object_build() {
	args := msvc_cl_object_args(['-std=gnu11', '-w', '-DNDEBUG', '-x', 'c', '-o', 'C:/tmp/cJSON.obj',
		'-c', 'C:/v/thirdparty/cJSON/cJSON.c'], 'windows')
	assert args == ['/nologo', '/volatile:ms', '/bigobj', '/MD', '/w', '/DNDEBUG', '/c',
		'/FoC:/tmp/cJSON.obj', 'C:/v/thirdparty/cJSON/cJSON.c']
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

fn test_c_compiler_is_msvc() {
	assert c_compiler_is_msvc('cl')
	assert c_compiler_is_msvc('C:/Program Files/Microsoft Visual Studio/VC/bin/CL.EXE')
	assert c_compiler_is_msvc('msvc')
	assert !c_compiler_is_msvc('clang')
	assert !c_compiler_is_msvc('cc')
}

fn test_msvc_missing_cl_message_targets_windows_off_windows() {
	macos_arm := pref.Target{
		os:   'macos'
		arch: 'arm64'
	}
	assert msvc_missing_cl_message('msvc', 'macos', macos_arm).contains('use `-os windows -arch amd64 -cc msvc -o file.c`')
	linux_x64 := pref.Target{
		os:   'linux'
		arch: 'amd64'
	}
	assert msvc_missing_cl_message('msvc', 'linux', linux_x64).contains('use `-os windows -cc msvc -o file.c`')
	// An explicit Windows target, including its architecture, is kept.
	windows_arm := pref.Target{
		os:   'windows'
		arch: 'arm64'
	}
	assert msvc_missing_cl_message('msvc', 'linux', windows_arm).contains('use `-cc msvc -o file.c`')
	windows_x64 := pref.Target{
		os:   'windows'
		arch: 'amd64'
	}
	assert msvc_missing_cl_message('cl', 'windows', windows_x64).contains('Developer Command Prompt')
}
