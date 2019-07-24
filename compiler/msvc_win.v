module main

import os

#flag -I @VROOT/thirdparty/microsoft_craziness
#flag windows @VROOT/thirdparty/microsoft_craziness/microsoft_craziness.o
#flag windows -l ole32
#flag windows -l oleaut32
#flag windows -l shell32

// Emily: If these arent included then msvc assumes that 
// these return int (which should be 64bit)
// but then it goes and sign extends our pointer types anyway
// which breaks everything
#include <microsoft_craziness.h>

struct MsvcResult {
    sdk_ver int
    
    windows_sdk_root_path string
    exe_path string

    um_lib_path string
    ucrt_lib_path string
    vs_lib_path string

    um_include_path string
    ucrt_include_path string
    vs_include_path string
    shared_include_path string
}

struct FindResult {
	sdk_ver int
	windows_sdk_root byteptr
	windows_sdk_um_library_path byteptr
	windows_sdk_ucrt_library_path byteptr
	vs_exe_path byteptr
	vs_library_path byteptr
}

fn C.find_visual_studio_and_windows_sdk() *FindResult
fn C.wide_string_to_narrow_temp(byteptr) byteptr

fn find_msvc() *MsvcResult {
	$if windows {
		r := C.find_visual_studio_and_windows_sdk()

		windows_sdk_root := tos_clone(C.wide_string_to_narrow_temp(r.windows_sdk_root))
		ucrt_lib_folder := tos_clone(C.wide_string_to_narrow_temp(r.windows_sdk_ucrt_library_path))
		um_lib_folder := tos_clone(C.wide_string_to_narrow_temp(r.windows_sdk_um_library_path))
		vs_lib_folder := tos_clone(C.wide_string_to_narrow_temp(r.vs_library_path))
		exe_folder := tos_clone(C.wide_string_to_narrow_temp(r.vs_exe_path))

		mut ucrt_include_folder := ucrt_lib_folder.replace('Lib', 'Include')
		mut vs_include_folder := vs_lib_folder.replace('lib', 'include')

		if ucrt_include_folder.ends_with('\\x64') {
			ucrt_include_folder = ucrt_include_folder.left(ucrt_include_folder.len - 4)
		}
		if vs_include_folder.ends_with('\\x64') {
			vs_include_folder = vs_include_folder.left(vs_include_folder.len - 4)
		}

		um_include_folder := ucrt_include_folder.replace('ucrt', 'um')
		shared_include_folder := ucrt_include_folder.replace('ucrt', 'shared')

		return &MsvcResult {
			sdk_ver: r.sdk_ver,
			windows_sdk_root_path: windows_sdk_root,
			exe_path: exe_folder,

			um_lib_path: um_lib_folder,
			ucrt_lib_path: ucrt_lib_folder,
			vs_lib_path: vs_lib_folder,

			um_include_path: um_include_folder,
			ucrt_include_path: ucrt_include_folder,
			vs_include_path: vs_include_folder,
			shared_include_path: shared_include_folder,
		}
	}
	$else {
		panic('Cannot find msvc on this platform')
	}
}

pub fn cc_msvc(v *V) {
	r := find_msvc()

	mut a := ['-w', '/volatile:ms'] // arguments for the C compiler

	// cl.exe is stupid so these are in a different order to the ones below!

	if v.pref.is_prod {
		a << '/O2'
		a << '/MD'
	} else {
		a << '/Z7'
		a << '/MDd'
	}

	if v.pref.is_so {
		// Dont think we have to do anything for this
		if !v.out_name.ends_with('.dll') {
			v.out_name = v.out_name + '.dll'
		}

		// Build dll
		a << '/LD'
	} else if !v.out_name.ends_with('.exe') {
		v.out_name = v.out_name + '.exe'
	}

	mut libs := ''// builtin.o os.o http.o etc
	if v.pref.build_mode == .build {
	}
	else if v.pref.build_mode == .embed_vlib {
		// 
	}
	else if v.pref.build_mode == .default_mode {
		libs = '"$ModPath/vlib/builtin.obj"'
		if !os.file_exists(libs) {
			println('`builtin.obj` not found')
			exit(1)
		}
		for imp in v.table.imports {
			if imp == 'webview' {
				continue
			}
			libs += ' "$ModPath/vlib/${imp}.obj"'
		}
	}

	if v.pref.sanitize {
		println('Sanitize not supported on msvc.')
	}

	// The C file we are compiling
	//a << '"$TmpPath/$v.out_name_c"'
	// this isnt correct for some reason
	// so fix that now

	a << '".$v.out_name_c"'

	mut other_flags := []string{}
	mut real_libs := []string{}
	mut lib_paths := []string{}

	for f in v.table.flags {
		// We need to see if the flag contains -l
		// -l isnt recognised and these libs will be passed straight to the linker
		// by the compiler
		if f.starts_with('-l') {
			lib_base := f.right(2).trim_space()

			// MSVC has no method of linking against a .dll
			// TODO: we should look for .defs aswell
			lib_lib := lib_base + '.lib'
			real_libs << lib_lib
		} 
		else if f.starts_with('-L') {
			lib_paths << f.right(2).trim_space()
		}
		else if f.ends_with('.o') {
			// msvc expects .obj not .o
			other_flags << f + 'bj'
		} 
		else {
			other_flags << f
		}
	}

	default_libs := [
		'kernel32.lib',
		'user32.lib',
		'gdi32.lib',
		'winspool.lib',
		'comdlg32.lib',
		'advapi32.lib',
		'shell32.lib',
		'ole32.lib',
		'oleaut32.lib',
		'uuid.lib',
		'odbc32.lib',
		'odbccp32.lib',
		'vcruntime.lib',
		'kernel32.lib',
	]

	for l in default_libs {
		real_libs << l
	}


	// flags := v.table.flags.join(' ')

	// Include the base paths
	a << '-I "$r.ucrt_include_path" -I "$r.vs_include_path" -I "$r.um_include_path" -I "$r.shared_include_path"'

	// Msvc also doesnt have atomic
	// TODO: dont rely on gcc's _Atomic semantics!
	a << other_flags

	// TODO: libs will need to be actually handled properly
	a << real_libs.join(' ')

	a << '/link'
	a << '/NOLOGO'
	a << '/OUT:$v.out_name'
	a << '/LIBPATH:"$r.ucrt_lib_path"'
	a << '/LIBPATH:"$r.um_lib_path"'
	a << '/LIBPATH:"$r.vs_lib_path"'
	a << '/INCREMENTAL:NO' // Disable incremental linking

	for l in lib_paths {
		a << '/LIBPATH:"$l"'
	}

	if !v.pref.is_prod {
		a << '/DEBUG:FULL'
	}

	args := a.join(' ')

	// println('$args')
	// println('$exe_path')

	escaped_path := r.exe_path

	cmd := '""$escaped_path\\cl.exe" $args"'

	// println('$cmd')

	res := os.exec(cmd)
	// println(res)
	// println('C OUTPUT:')
	if res.contains('error') {
		println(res)
		panic('msvc error')
	}

	if !v.pref.is_debug && v.out_name_c != 'v.c' && v.out_name_c != 'v_macos.c' {
		os.rm('.$v.out_name_c') 
	} 

}

fn build_thirdparty_obj_file_with_msvc(flag string) {
	msvc := find_msvc()

	mut obj_path := flag.all_after(' ')

	if obj_path.ends_with('.o') {
		// msvc expects .obj not .o
		obj_path = obj_path + 'bj'
	}

	if os.file_exists(obj_path) {
		return 
	} 
	println('$obj_path not found, building it (with msvc)...') 
	parent := obj_path.all_before_last('/').trim_space() 
	files := os.ls(parent)

	mut cfiles := '' 
	for file in files {
		if file.ends_with('.c') { 
			cfiles += parent + '/' + file + ' ' 
		} 
	}

	include_string := '-I "$msvc.ucrt_include_path" -I "$msvc.vs_include_path" -I "$msvc.um_include_path" -I "$msvc.shared_include_path"'

	println('$cfiles')

	res := os.exec('""$msvc.exe_path\\cl.exe" /volatile:ms /Z7 $include_string /c $cfiles /Fo"$obj_path""')
	println(res)
}