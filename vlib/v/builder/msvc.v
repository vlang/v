module builder

import os
import v.pref
import v.util
import v.cflag

#flag windows -l shell32
#flag windows -l dbghelp
#flag windows -l advapi32

struct MsvcResult {
	full_cl_exe_path    string
	exe_path            string
	um_lib_path         string
	ucrt_lib_path       string
	vs_lib_path         string
	um_include_path     string
	ucrt_include_path   string
	vs_include_path     string
	shared_include_path string
	valid               bool
}

// shell32 for RegOpenKeyExW etc
// Mimics a HKEY
type RegKey = voidptr

// Taken from the windows SDK
const (
	hkey_local_machine     = RegKey(0x80000002)
	key_query_value        = (0x0001)
	key_wow64_32key        = (0x0200)
	key_enumerate_sub_keys = (0x0008)
)

// Given a root key look for one of the subkeys in 'versions' and get the path
fn find_windows_kit_internal(key RegKey, versions []string) ?string {
	$if windows {
		unsafe {
			for version in versions {
				required_bytes := u32(0) // TODO mut
				result := C.RegQueryValueEx(key, version.to_wide(), 0, 0, 0, &required_bytes)
				length := required_bytes / 2
				if result != 0 {
					continue
				}
				alloc_length := (required_bytes + 2)
				mut value := &u16(malloc_noscan(int(alloc_length)))
				if isnil(value) {
					continue
				}
				//
				else {
				}
				result2 := C.RegQueryValueEx(key, version.to_wide(), 0, 0, value, &alloc_length)
				if result2 != 0 {
					continue
				}
				// We might need to manually null terminate this thing
				// So just make sure that we do that
				if value[length - 1] != u16(0) {
					value[length] = u16(0)
				}
				res := string_from_wide(value)
				return res
			}
		}
	}
	return error('windows kit not found')
}

struct WindowsKit {
	um_lib_path         string
	ucrt_lib_path       string
	um_include_path     string
	ucrt_include_path   string
	shared_include_path string
}

// Try and find the root key for installed windows kits
fn find_windows_kit_root(target_arch string) ?WindowsKit {
	$if windows {
		wkroot := find_windows_kit_root_by_reg(target_arch) or {
			if wkroot := find_windows_kit_root_by_env(target_arch) {
				return wkroot
			}
			return err
		}

		return wkroot
	} $else {
		return error('Host OS does not support finding a windows kit')
	}
}

// Try to find the root key for installed windows kits from registry
fn find_windows_kit_root_by_reg(target_arch string) ?WindowsKit {
	$if windows {
		root_key := RegKey(0)
		path := 'SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots'
		rc := C.RegOpenKeyEx(builder.hkey_local_machine, path.to_wide(), 0, builder.key_query_value | builder.key_wow64_32key | builder.key_enumerate_sub_keys,
			&root_key)

		if rc != 0 {
			return error('Unable to open root key')
		}
		// Try and find win10 kit
		kit_root := find_windows_kit_internal(root_key, ['KitsRoot10', 'KitsRoot81']) or {
			C.RegCloseKey(root_key)
			return error('Unable to find a windows kit')
		}
		C.RegCloseKey(root_key)
		return new_windows_kit(kit_root, target_arch)
	} $else {
		return error('Host OS does not support finding a windows kit')
	}
}

fn new_windows_kit(kit_root string, target_arch string) ?WindowsKit {
	kit_lib := kit_root + 'Lib'
	files := os.ls(kit_lib) ?
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
	return WindowsKit{
		um_lib_path: kit_lib_highest + '\\um\\$target_arch'
		ucrt_lib_path: kit_lib_highest + '\\ucrt\\$target_arch'
		um_include_path: kit_include_highest + '\\um'
		ucrt_include_path: kit_include_highest + '\\ucrt'
		shared_include_path: kit_include_highest + '\\shared'
	}
}

fn find_windows_kit_root_by_env(target_arch string) ?WindowsKit {
	kit_root := os.getenv('WindowsSdkDir')
	if kit_root == '' {
		return error('empty WindowsSdkDir')
	}
	return new_windows_kit(kit_root, target_arch)
}

struct VsInstallation {
	include_path string
	lib_path     string
	exe_path     string
}

fn find_vs(vswhere_dir string, host_arch string, target_arch string) ?VsInstallation {
	$if windows {
		vsinst := find_vs_by_reg(vswhere_dir, host_arch, target_arch) or {
			if vsinst := find_vs_by_env(host_arch, target_arch) {
				return vsinst
			}
			return err
		}
		return vsinst
	} $else {
		return error('Host OS does not support finding a Visual Studio installation')
	}
}

fn find_vs_by_reg(vswhere_dir string, host_arch string, target_arch string) ?VsInstallation {
	$if windows {
		// Emily:
		// VSWhere is guaranteed to be installed at this location now
		// If its not there then end user needs to update their visual studio
		// installation!
		res := os.execute('"$vswhere_dir\\Microsoft Visual Studio\\Installer\\vswhere.exe" -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath')
		// println('res: "$res"')
		if res.exit_code != 0 {
			return error_with_code(res.output, res.exit_code)
		}
		res_output := res.output.trim_space()
		version := os.read_file('$res_output\\VC\\Auxiliary\\Build\\Microsoft.VCToolsVersion.default.txt') or {
			// println('Unable to find msvc version')
			return error('Unable to find vs installation')
		}
		// println('version: $version')
		v := version.trim_space()
		lib_path := '$res_output\\VC\\Tools\\MSVC\\$v\\lib\\$target_arch'
		include_path := '$res_output\\VC\\Tools\\MSVC\\$v\\include'
		if os.exists('$lib_path\\vcruntime.lib') {
			p := '$res_output\\VC\\Tools\\MSVC\\$v\\bin\\Host$host_arch\\$target_arch'
			// println('$lib_path $include_path')
			return VsInstallation{
				exe_path: p
				lib_path: lib_path
				include_path: include_path
			}
		}
		println('Unable to find vs installation (attempted to use lib path "$lib_path")')
		return error('Unable to find vs exe folder')
	} $else {
		return error('Host OS does not support finding a Visual Studio installation')
	}
}

fn find_vs_by_env(host_arch string, target_arch string) ?VsInstallation {
	vs_dir := os.getenv('VSINSTALLDIR')
	if vs_dir == '' {
		return error('empty VSINSTALLDIR')
	}

	vc_tools_dir := os.getenv('VCToolsInstallDir')
	if vc_tools_dir == '' {
		return error('empty VCToolsInstallDir')
	}

	bin_dir := '${vc_tools_dir}bin\\Host$host_arch\\$target_arch'
	lib_path := '${vc_tools_dir}lib\\$target_arch'
	include_path := '${vc_tools_dir}include'

	return VsInstallation{
		exe_path: bin_dir
		lib_path: lib_path
		include_path: include_path
	}
}

fn find_msvc(m64_target bool) ?MsvcResult {
	$if windows {
		processor_architecture := os.getenv('PROCESSOR_ARCHITECTURE')
		vswhere_dir := if processor_architecture == 'x86' {
			'%ProgramFiles%'
		} else {
			'%ProgramFiles(x86)%'
		}
		host_arch := if processor_architecture == 'x86' { 'X86' } else { 'X64' }
		target_arch := if !m64_target { 'X86' } else { 'X64' }
		wk := find_windows_kit_root(target_arch) or { return error('Unable to find windows sdk') }
		vs := find_vs(vswhere_dir, host_arch, target_arch) or {
			return error('Unable to find visual studio')
		}
		return MsvcResult{
			full_cl_exe_path: os.real_path(vs.exe_path + os.path_separator + 'cl.exe')
			exe_path: vs.exe_path
			um_lib_path: wk.um_lib_path
			ucrt_lib_path: wk.ucrt_lib_path
			vs_lib_path: vs.lib_path
			um_include_path: wk.um_include_path
			ucrt_include_path: wk.ucrt_include_path
			vs_include_path: vs.include_path
			shared_include_path: wk.shared_include_path
			valid: true
		}
	} $else {
		// This hack allows to at least see the generated .c file with `-os windows -cc msvc -o x.c`
		// Please do not remove it, unless you also check that the above continues to work.
		return MsvcResult{
			full_cl_exe_path: '/usr/bin/true'
			valid: true
		}
	}
}

pub fn (mut v Builder) cc_msvc() {
	r := v.cached_msvc
	if r.valid == false {
		verror('Cannot find MSVC on this OS')
	}
	out_name_obj := os.real_path(v.out_name_c + '.obj')
	out_name_pdb := os.real_path(v.out_name_c + '.pdb')
	out_name_cmd_line := os.real_path(v.out_name_c + '.rsp')
	mut a := []string{}
	//
	env_cflags := os.getenv('CFLAGS')
	mut all_cflags := '$env_cflags $v.pref.cflags'
	if all_cflags != ' ' {
		a << all_cflags
	}
	//
	// Default arguments
	// `-w` no warnings
	// `/we4013` 2 unicode defines, see https://docs.microsoft.com/en-us/cpp/error-messages/compiler-warnings/compiler-warning-level-3-c4013?redirectedfrom=MSDN&view=msvc-170
	// `/volatile:ms` enables atomic volatile (gcc _Atomic)
	// `/Fo` sets the object file name - needed so we can clean up after ourselves properly
	// `/F 16777216` changes the stack size to 16MB, see https://docs.microsoft.com/en-us/cpp/build/reference/f-set-stack-size?view=msvc-170
	a << ['-w', '/we4013', '/volatile:ms', '/Fo"$out_name_obj"', '/F 16777216']
	if v.pref.is_prod {
		a << '/O2'
	}
	if v.pref.is_debug {
		a << '/MDd'
		a << '/D_DEBUG'
		// /Zi generates a .pdb
		// /Fd sets the pdb file name (so its not just vc140 all the time)
		a << ['/Zi', '/Fd"$out_name_pdb"']
	} else {
		a << '/MD'
		a << '/DNDEBUG'
	}
	if v.pref.is_shared {
		if !v.pref.out_name.ends_with('.dll') {
			v.pref.out_name += '.dll'
		}
		// Build dll
		a << '/LD'
	} else if !v.pref.out_name.ends_with('.exe') {
		v.pref.out_name += '.exe'
	}
	v.pref.out_name = os.real_path(v.pref.out_name)
	// alibs := []string{} // builtin.o os.o http.o etc
	if v.pref.build_mode == .build_module {
		// Compile only
		a << '/c'
	} else if v.pref.build_mode == .default_mode {
		/*
		b := os.real_path( '${pref.default_module_path}/vlib/builtin.obj' )
		alibs << '"$b"'
		if !os.exists(b) {
			println('`builtin.obj` not found')
			exit(1)
		}
		for imp in v.ast.imports {
			if imp == 'webview' {
				continue
			}
			alibs << '"' + os.real_path( '${pref.default_module_path}/vlib/${imp}.obj' ) + '"'
		}
		*/
	}
	if v.pref.sanitize {
		eprintln('Sanitize not supported on msvc.')
	}
	// The C file we are compiling
	// a << '"$TmpPath/$v.out_name_c"'
	a << '"' + os.real_path(v.out_name_c) + '"'
	// Emily:
	// Not all of these are needed (but the compiler should discard them if they are not used)
	// these are the defaults used by msbuild and visual studio
	mut real_libs := ['kernel32.lib', 'user32.lib', 'advapi32.lib']
	sflags := msvc_string_flags(v.get_os_cflags())
	real_libs << sflags.real_libs
	inc_paths := sflags.inc_paths
	lib_paths := sflags.lib_paths
	defines := sflags.defines
	other_flags := sflags.other_flags
	// Include the base paths
	a << '-I "$r.ucrt_include_path"'
	a << '-I "$r.vs_include_path"'
	a << '-I "$r.um_include_path"'
	a << '-I "$r.shared_include_path"'
	a << defines
	a << inc_paths
	a << other_flags
	// Libs are passed to cl.exe which passes them to the linker
	a << real_libs.join(' ')
	a << '/link'
	a << '/NOLOGO'
	a << '/OUT:"$v.pref.out_name"'
	a << '/LIBPATH:"$r.ucrt_lib_path"'
	a << '/LIBPATH:"$r.um_lib_path"'
	a << '/LIBPATH:"$r.vs_lib_path"'
	if !all_cflags.contains('/DEBUG') {
		// only use /DEBUG, if the user *did not* provide its own:
		a << '/DEBUG:FULL' // required for prod builds to generate a PDB file
	}
	if v.pref.is_prod {
		a << '/INCREMENTAL:NO' // Disable incremental linking
		a << '/OPT:REF'
		a << '/OPT:ICF'
	}
	a << lib_paths
	env_ldflags := os.getenv('LDFLAGS')
	if env_ldflags != '' {
		a << env_ldflags
	}
	args := a.join(' ')
	// write args to a file so that we dont smash createprocess
	os.write_file(out_name_cmd_line, args) or {
		verror('Unable to write response file to "$out_name_cmd_line"')
	}
	cmd := '"$r.full_cl_exe_path" "@$out_name_cmd_line"'
	// It is hard to see it at first, but the quotes above ARE balanced :-| ...
	// Also the double quotes at the start ARE needed.
	v.show_cc(cmd, out_name_cmd_line, args)
	if os.user_os() != 'windows' && !v.pref.out_name.ends_with('.c') {
		verror('Cannot build with msvc on $os.user_os()')
	}
	util.timing_start('C msvc')
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln(res.output)
		verror('msvc error')
	}
	util.timing_measure('C msvc')
	if v.pref.show_c_output {
		v.show_c_compiler_output(res)
	} else {
		v.post_process_c_compiler_output(res)
	}
	// println(res)
	// println('C OUTPUT:')
	// Always remove the object file - it is completely unnecessary
	os.rm(out_name_obj) or {}
}

fn (mut v Builder) build_thirdparty_obj_file_with_msvc(path string, moduleflags []cflag.CFlag) {
	msvc := v.cached_msvc
	if msvc.valid == false {
		verror('Cannot find MSVC on this OS')
	}
	// msvc expects .obj not .o
	path_without_o_postfix := path[..path.len - 2] // remove .o
	mut obj_path := '${path_without_o_postfix}.obj'
	obj_path = os.real_path(obj_path)
	if os.exists(obj_path) {
		// println('$obj_path already built.')
		return
	}
	println('$obj_path not found, building it (with msvc)...')
	cfile := '${path_without_o_postfix}.c'
	// println('cfile: $cfile')
	flags := msvc_string_flags(moduleflags)
	inc_dirs := flags.inc_paths.join(' ')
	defines := flags.defines.join(' ')
	include_string := '-I "$msvc.ucrt_include_path" -I "$msvc.vs_include_path" -I "$msvc.um_include_path" -I "$msvc.shared_include_path" $inc_dirs'
	mut oargs := []string{}
	env_cflags := os.getenv('CFLAGS')
	mut all_cflags := '$env_cflags $v.pref.cflags'
	if all_cflags != ' ' {
		oargs << all_cflags
	}
	//
	if v.pref.is_prod {
		oargs << '/O2'
		oargs << '/MD'
		oargs << '/DNDEBUG'
	} else {
		oargs << '/MDd'
		oargs << '/D_DEBUG'
	}
	env_ldflags := os.getenv('LDFLAGS')
	if env_ldflags != '' {
		oargs << env_ldflags
	}
	str_oargs := oargs.join(' ')
	cmd := '"$msvc.full_cl_exe_path" /volatile:ms $str_oargs $defines $include_string /c "$cfile" /Fo"$obj_path"'
	// Note: the quotes above ARE balanced.
	$if trace_thirdparty_obj_files ? {
		println('>>> build_thirdparty_obj_file_with_msvc cmd: $cmd')
	}
	res := os.execute(cmd)
	if res.exit_code != 0 {
		println('msvc: failed to build a thirdparty object; cmd: $cmd')
		verror(res.output)
	}
	println(res.output)
}

struct MsvcStringFlags {
mut:
	real_libs   []string
	inc_paths   []string
	lib_paths   []string
	defines     []string
	other_flags []string
}

// pub fn (cflags []CFlag) msvc_string_flags() MsvcStringFlags {
pub fn msvc_string_flags(cflags []cflag.CFlag) MsvcStringFlags {
	mut real_libs := []string{}
	mut inc_paths := []string{}
	mut lib_paths := []string{}
	mut defines := []string{}
	mut other_flags := []string{}
	for flag in cflags {
		// println('fl: $flag.name | flag arg: $flag.value')
		// We need to see if the flag contains -l
		// -l isnt recognised and these libs will be passed straight to the linker
		// by the compiler
		if flag.name == '-l' {
			if flag.value.ends_with('.dll') {
				verror('MSVC cannot link against a dll (`#flag -l $flag.value`)')
			}
			// MSVC has no method of linking against a .dll
			// TODO: we should look for .defs aswell
			lib_lib := flag.value + '.lib'
			real_libs << lib_lib
		} else if flag.name == '-I' {
			inc_paths << flag.format()
		} else if flag.name == '-D' {
			defines << '/D$flag.value'
		} else if flag.name == '-L' {
			lib_paths << flag.value
			lib_paths << flag.value + os.path_separator + 'msvc'
			// The above allows putting msvc specific .lib files in a subfolder msvc/ ,
			// where gcc will NOT find them, but cl will do...
			// Note: gcc is smart enough to not need .lib files at all in most cases, the .dll is enough.
			// When both a msvc .lib file and .dll file are present in the same folder,
			// as for example for glfw3, compilation with gcc would fail.
		} else if flag.value.ends_with('.o') {
			// msvc expects .obj not .o
			other_flags << '"${flag.value}bj"'
		} else if flag.value.starts_with('-D') {
			defines << '/D${flag.value[2..]}'
		} else {
			other_flags << flag.value
		}
	}
	mut lpaths := []string{}
	for l in lib_paths {
		lpaths << '/LIBPATH:"' + os.real_path(l) + '"'
	}
	return MsvcStringFlags{
		real_libs: real_libs
		inc_paths: inc_paths
		lib_paths: lpaths
		defines: defines
		other_flags: other_flags
	}
}
