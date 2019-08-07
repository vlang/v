module main

import os

#flag windows -l shell32

struct MsvcResult {
    exe_path string

    um_lib_path string
    ucrt_lib_path string
    vs_lib_path string

    um_include_path string
    ucrt_include_path string
    vs_include_path string
    shared_include_path string
}

// Mimics a HKEY
type RegKey voidptr

// Taken from the windows SDK
const (
	HKEY_LOCAL_MACHINE = RegKey(0x80000002)
	KEY_QUERY_VALUE = (0x0001)
	KEY_WOW64_32KEY = (0x0200)
	KEY_ENUMERATE_SUB_KEYS = (0x0008)
)

// Given a root key look for the subkey 'version' and get the path 
fn find_windows_kit_internal(key RegKey, version string) ?string {
	required_bytes := 0 // TODO mut 
	result := C.RegQueryValueExW(key, version.to_wide(), 0, 0, 0, &required_bytes)

	length := required_bytes / 2

	if result != 0 {
		return error('')
	}

	alloc_length := (required_bytes + 2)

	mut value := &u16(malloc(alloc_length))
	if !value {
		return error('')
	}

	result2 := C.RegQueryValueExW(key, version.to_wide(), 0, 0, value, &alloc_length)

	if result2 != 0 {
		return error('')
	}

	// We might need to manually null terminate this thing
	// So just make sure that we do that
	if (value[length - 1] != u16(0)) {
		value[length] = u16(0)
	}

	return string_from_wide(value)
}

struct WindowsKit {
	um_lib_path string
	ucrt_lib_path string

	um_include_path string
	ucrt_include_path string
	shared_include_path string
}

// Try and find the root key for installed windows kits
fn find_windows_kit_root() ?WindowsKit {
	root_key := RegKey(0)
	rc := C.RegOpenKeyExA(
		HKEY_LOCAL_MACHINE, 'SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots', 0, KEY_QUERY_VALUE | KEY_WOW64_32KEY | KEY_ENUMERATE_SUB_KEYS, &root_key)
	
	defer {C.RegCloseKey(root_key)}

	if rc != 0 {
		return error('Unable to open root key')
	}
	// Try and find win10 kit
	kit_root := find_windows_kit_internal(root_key, 'KitsRoot10') or {
		// Fallback to windows 8
		k := find_windows_kit_internal(root_key, 'KitsRoot81') or {
			println('Unable to find windows sdk')
			return error('Unable to find a windows kit')
		}
		k
	}

	kit_lib := kit_root + 'Lib'

	// println(kit_lib)

	files := os.ls(kit_lib)
	mut highest_path := ''
	mut highest_int := 0
	for f in files {
		no_dot := f.replace('.', '')
		v_int := no_dot.int()

		if v_int > highest_int {
			highest_int = v_int
			highest_path = f
		}
	}

	kit_lib_highest := kit_lib + '\\$highest_path'
	kit_include_highest := kit_lib_highest.replace('Lib', 'Include')

	// println('$kit_lib_highest $kit_include_highest')

	return WindowsKit {
		um_lib_path: kit_lib_highest + '\\um\\x64'
		ucrt_lib_path: kit_lib_highest + '\\ucrt\\x64'

		um_include_path: kit_include_highest + '\\um'
		ucrt_include_path: kit_include_highest + '\\ucrt'
		shared_include_path: kit_include_highest + '\\shared'
	}
}

struct VsInstallation {
	include_path string
	lib_path string
	exe_path string
}

fn find_vs() ?VsInstallation {
	// Emily:
	// VSWhere is guaranteed to be installed at this location now
	// If its not there then end user needs to update their visual studio 
	// installation!
	res := os.exec('""%ProgramFiles(x86)%\\Microsoft Visual Studio\\Installer\\vswhere.exe" -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath"') or {
		panic(err)
		return error(err)// TODO remove return
	}
	// println('res: "$res"')

	version := os.read_file('$res\\VC\\Auxiliary\\Build\\Microsoft.VCToolsVersion.default.txt') or {
		println('Unable to find msvc version')
		return error('Unable to find vs installation')
	}

	// println('version: $version')

	v := if version.ends_with('\n') {
		version.left(version.len - 2)
	} else {
		version
	}

	lib_path := '$res\\VC\\Tools\\MSVC\\$v\\lib\\x64'
	include_path := '$res\\VC\\Tools\\MSVC\\$v\\include'

	if os.file_exists('$lib_path\\vcruntime.lib') {
		p := '$res\\VC\\Tools\\MSVC\\$v\\bin\\Hostx64\\x64'

		// println('$lib_path $include_path')

		return VsInstallation{
			exe_path: p
			lib_path: lib_path
			include_path: include_path
		}
	}

	println('Unable to find vs installation (attempted to use lib path "$lib_path")')
	return error('Unable to find vs exe folder')
}

fn find_msvc() ?MsvcResult {
	$if windows {
		wk := find_windows_kit_root() or {
			return error('Unable to find windows sdk')
		}
		vs := find_vs() or {
			return error('Unable to find visual studio')
		}

		return MsvcResult {
			exe_path: vs.exe_path,

			um_lib_path: wk.um_lib_path,
			ucrt_lib_path: wk.ucrt_lib_path,
			vs_lib_path: vs.lib_path,

			um_include_path: wk.um_include_path,
			ucrt_include_path: wk.ucrt_include_path,
			vs_include_path: vs.include_path,
			shared_include_path: wk.shared_include_path,
		}
	}
	$else {
		panic('Cannot find msvc on this platform')
	}
}

pub fn cc_msvc(v *V) {
	r := find_msvc() or {
		println('Could not find MSVC')
		
		// TODO: code reuse
		if !v.pref.is_debug && v.out_name_c != 'v.c' && v.out_name_c != 'v_macos.c' {
			os.rm('.$v.out_name_c') 
		}
		return
	}

	// Default arguments
	mut a := ['-w', '/volatile:ms', '/D_UNICODE', '/DUNICODE']

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
	a << '".$v.out_name_c"'

	mut real_libs :=  [
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
	]

	mut lib_paths := []string{}
	mut other_flags := []string{}

	// Emily:
	// this is a hack to try and support -l -L and object files
	// passed on the command line
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

	res := os.exec(cmd) or {
		panic(err)
		return // TODO remove return
	}
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
	msvc := find_msvc() or {
		println('Could not find visual studio')
		return
	}

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

	res := os.exec('""$msvc.exe_path\\cl.exe" /volatile:ms /Z7 $include_string /c $cfiles /Fo"$obj_path" /D_UNICODE /DUNICODE"') or {
		panic(err)
		return // TODO remove return
	}
	println(res)
} 

