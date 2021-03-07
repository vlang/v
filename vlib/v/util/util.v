// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

import os
import time
import v.pref
import v.vmod
import v.util.recompilation

pub const (
	v_version = '0.2.2'
)

// math.bits is needed by strconv.ftoa
pub const (
	builtin_module_parts = ['math.bits', 'strconv', 'strconv.ftoa', 'hash', 'strings', 'builtin']
	bundle_modules       = ['clipboard', 'fontstash', 'gg', 'gx', 'sokol', 'szip', 'ui']
)

pub const (
	external_module_dependencies_for_tool = map{
		'vdoc': ['markdown']
	}
)

// vhash() returns the build string C.V_COMMIT_HASH . See cmd/tools/gen_vc.v .
pub fn vhash() string {
	mut buf := [50]byte{}
	buf[0] = 0
	unsafe {
		bp := &buf[0]
		C.snprintf(charptr(bp), 50, '%s', C.V_COMMIT_HASH)
		return tos_clone(bp)
	}
}

pub fn full_hash() string {
	build_hash := vhash()
	current_hash := githash(false)
	if build_hash == current_hash {
		return build_hash
	}
	return '${build_hash}.$current_hash'
}

// full_v_version() returns the full version of the V compiler
pub fn full_v_version(is_verbose bool) string {
	if is_verbose {
		return 'V $util.v_version $full_hash()'
	}
	hash := githash(false)
	return 'V $util.v_version $hash'
}

// githash(x) returns the current git commit hash.
// When x is false, it is very fast - it just returns a predefined C constant.
// When x is true, it tries to get the current commit hash, by parsing the
// relevant files in the .git/ folder, or if that is not possible
// for example when using a V from a V binary release, that does not have .git/
// defaults to getting the predefined C constant again.
// NB: githash(true) must be called only when v detects that it builds itself.
// For all other programs, githash(false) should be used.
pub fn githash(should_get_from_filesystem bool) string {
	for {
		// The `for` construct here is used as a goto substitute.
		// The code in this function will break out of the `for`
		// if it detects an error and can not continue.
		if should_get_from_filesystem {
			vexe := os.getenv('VEXE')
			vroot := os.dir(vexe)
			// .git/HEAD
			git_head_file := os.join_path(vroot, '.git', 'HEAD')
			if !os.exists(git_head_file) {
				break
			}
			// 'ref: refs/heads/master' ... the current branch name
			head_content := os.read_file(git_head_file) or { break }
			mut current_branch_hash := head_content
			if head_content.starts_with('ref: ') {
				gcbranch_rel_path := head_content.replace('ref: ', '').trim_space()
				gcbranch_file := os.join_path(vroot, '.git', gcbranch_rel_path)
				// .git/refs/heads/master
				if !os.exists(gcbranch_file) {
					break
				}
				// get the full commit hash contained in the ref heads file
				branch_hash := os.read_file(gcbranch_file) or { break }
				current_branch_hash = branch_hash
			}
			desired_hash_length := 7
			if current_branch_hash.len > desired_hash_length {
				return current_branch_hash[0..desired_hash_length]
			}
		}
		break
	}
	mut buf := [50]byte{}
	buf[0] = 0
	unsafe {
		bp := &buf[0]
		C.snprintf(charptr(bp), 50, '%s', C.V_CURRENT_COMMIT_HASH)
		return tos_clone(bp)
	}
}

//
pub fn set_vroot_folder(vroot_path string) {
	// Preparation for the compiler module:
	// VEXE env variable is needed so that compiler.vexe_path()
	// can return it later to whoever needs it:
	vname := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	os.setenv('VEXE', os.real_path(os.join_path(vroot_path, vname)), true)
	os.setenv('VCHILD', 'true', true)
}

pub fn resolve_vroot(str string, dir string) ?string {
	mut mcache := vmod.get_cache()
	vmod_file_location := mcache.get_by_folder(dir)
	if vmod_file_location.vmod_file.len == 0 {
		// There was no actual v.mod file found.
		return error('To use @VROOT, you need to have a "v.mod" file in $dir, or in one of its parent folders.')
	}
	vmod_path := vmod_file_location.vmod_folder
	return str.replace('@VROOT', os.real_path(vmod_path))
}

// resolve_env_value replaces all occurrences of `$env('ENV_VAR_NAME')`
// in `str` with the value of the env variable `$ENV_VAR_NAME`.
pub fn resolve_env_value(str string, check_for_presence bool) ?string {
	env_ident := "\$env('"
	at := str.index(env_ident) or {
		return error('no "$env_ident' + '...\')" could be found in "$str".')
	}
	mut ch := byte(`.`)
	mut env_lit := ''
	for i := at + env_ident.len; i < str.len && ch != `)`; i++ {
		ch = byte(str[i])
		if ch.is_letter() || ch.is_digit() || ch == `_` {
			env_lit += ch.ascii_str()
		} else {
			if !(ch == `\'` || ch == `)`) {
				if ch == `$` {
					return error('cannot use string interpolation in compile time \$env() expression')
				}
				return error('invalid environment variable name in "$str", invalid character "$ch.ascii_str()"')
			}
		}
	}
	if env_lit == '' {
		return error('supply an env variable name like HOME, PATH or USER')
	}
	mut env_value := ''
	if check_for_presence {
		env_value = os.environ()[env_lit] or {
			return error('the environment variable "$env_lit" does not exist.')
		}
		if env_value == '' {
			return error('the environment variable "$env_lit" is empty.')
		}
	} else {
		env_value = os.getenv(env_lit)
	}
	rep := str.replace_once(env_ident + env_lit + "'" + ')', env_value)
	if rep.contains(env_ident) {
		return resolve_env_value(rep, check_for_presence)
	}
	return rep
}

// launch_tool - starts a V tool in a separate process, passing it the `args`.
// All V tools are located in the cmd/tools folder, in files or folders prefixed by
// the letter `v`, followed by the tool name, i.e. `cmd/tools/vdoc/` or `cmd/tools/vpm.v`.
// The folder variant is suitable for larger and more complex tools, like `v doc`, because
// it provides you the ability to split their source in separate .v files, organized by topic,
// as well as have resources like static css/text/js files, that the tools can use.
// launch_tool uses a timestamp based detection mechanism, so that after `v self`, each tool
// will be recompiled too, before it is used, which guarantees that it would be up to date with
// V itself. That mechanism can be disabled by package managers by creating/touching a small
// `cmd/tools/.disable_autorecompilation` file, OR by changing the timestamps of all executables
// in cmd/tools to be < 1024 seconds (in unix time).
pub fn launch_tool(is_verbose bool, tool_name string, args []string) {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	set_vroot_folder(vroot)
	tool_args := args_quote_paths(args)
	tools_folder := os.join_path(vroot, 'cmd', 'tools')
	tool_basename := os.real_path(os.join_path(tools_folder, tool_name))
	mut tool_exe := ''
	mut tool_source := ''
	if os.is_dir(tool_basename) {
		tool_exe = path_of_executable(os.join_path(tool_basename, tool_name))
		tool_source = tool_basename
	} else {
		tool_exe = path_of_executable(tool_basename)
		tool_source = tool_basename + '.v'
	}
	tool_command := '"$tool_exe" $tool_args'
	if is_verbose {
		println('launch_tool vexe        : $vroot')
		println('launch_tool vroot       : $vroot')
		println('launch_tool tool_args   : $tool_args')
		println('launch_tool tool_source : $tool_source')
		println('launch_tool tool_command: $tool_command')
	}
	disabling_file := recompilation.disabling_file(vroot)
	is_recompilation_disabled := os.exists(disabling_file)
	should_compile := !is_recompilation_disabled
		&& should_recompile_tool(vexe, tool_source, tool_name, tool_exe)
	if is_verbose {
		println('launch_tool should_compile: $should_compile')
	}
	if should_compile {
		emodules := util.external_module_dependencies_for_tool[tool_name]
		for emodule in emodules {
			check_module_is_installed(emodule, is_verbose) or { panic(err) }
		}
		mut compilation_command := '"$vexe" '
		if tool_name in ['vself', 'vup', 'vdoctor', 'vsymlink'] {
			// These tools will be called by users in cases where there
			// is high chance of there being a problem somewhere. Thus
			// it is better to always compile them with -g, so that in
			// case these tools do crash/panic, their backtraces will have
			// .v line numbers, to ease diagnostic in #bugs and issues.
			compilation_command += ' -g '
		}
		compilation_command += '"$tool_source"'
		if is_verbose {
			println('Compiling $tool_name with: "$compilation_command"')
		}
		tool_compilation := os.exec(compilation_command) or { panic(err) }
		if tool_compilation.exit_code != 0 {
			eprintln('cannot compile `$tool_source`: \n$tool_compilation.output')
			exit(1)
		}
	}
	if is_verbose {
		println('launch_tool running tool command: $tool_command ...')
	}
	exit(os.system(tool_command))
}

// NB: should_recompile_tool/4 compares unix timestamps that have 1 second resolution
// That means that a tool can get recompiled twice, if called in short succession.
// TODO: use a nanosecond mtime timestamp, if available.
pub fn should_recompile_tool(vexe string, tool_source string, tool_name string, tool_exe string) bool {
	if os.is_dir(tool_source) {
		source_files := os.walk_ext(tool_source, '.v')
		mut newest_sfile := ''
		mut newest_sfile_mtime := 0
		for sfile in source_files {
			mtime := os.file_last_mod_unix(sfile)
			if mtime > newest_sfile_mtime {
				newest_sfile_mtime = mtime
				newest_sfile = sfile
			}
		}
		single_file_recompile := should_recompile_tool(vexe, newest_sfile, tool_name,
			tool_exe)
		// eprintln('>>> should_recompile_tool: tool_source: $tool_source | $single_file_recompile | $newest_sfile')
		return single_file_recompile
	}
	// TODO Caching should be done on the `vlib/v` level.
	mut should_compile := false
	if !os.exists(tool_exe) {
		should_compile = true
	} else {
		mtime_vexe := os.file_last_mod_unix(vexe)
		mtime_tool_exe := os.file_last_mod_unix(tool_exe)
		mtime_tool_source := os.file_last_mod_unix(tool_source)
		if mtime_tool_exe <= mtime_vexe {
			// v was recompiled, maybe after v up ...
			// rebuild the tool too just in case
			should_compile = true
			if tool_name == 'vself' || tool_name == 'vup' {
				// The purpose of vself/up is to update and recompile v itself.
				// After the first 'v self' execution, v will be modified, so
				// then a second 'v self' will detect, that v is newer than the
				// vself executable, and try to recompile vself/up again, which
				// will slow down the next v recompilation needlessly.
				should_compile = false
			}
		}
		if mtime_tool_exe <= mtime_tool_source {
			// the user changed the source code of the tool, or git updated it:
			should_compile = true
		}
		// GNU Guix and possibly other environments, have bit for bit reproducibility in mind,
		// including filesystem attributes like modification times, so they set the modification
		// times of executables to a small number like 0, 1 etc. In this case, we should not
		// recompile even if other heuristics say that we should. Users in such environments,
		// have to explicitly do: `v cmd/tools/vfmt.v`, and/or install v from source, and not
		// use the system packaged one, if they desire to develop v itself.
		if mtime_vexe < 1024 && mtime_tool_exe < 1024 {
			should_compile = false
		}
	}
	return should_compile
}

fn tool_source2name_and_exe(tool_source string) (string, string) {
	sfolder := os.dir(tool_source)
	tool_name := os.base(tool_source).replace('.v', '')
	tool_exe := os.join_path(sfolder, path_of_executable(tool_name))
	return tool_name, tool_exe
}

pub fn quote_path(s string) string {
	mut qs := s
	if qs.contains('&') {
		qs = qs.replace('&', '\\&')
	}
	if qs.contains(' ') {
		return '"$qs"'
	}
	return qs
}

pub fn args_quote_paths(args []string) string {
	mut res := []string{}
	for a in args {
		res << quote_path(a)
	}
	return res.join(' ')
}

pub fn path_of_executable(path string) string {
	$if windows {
		return path + '.exe'
	}
	return path
}

pub fn read_file(file_path string) ?string {
	raw_text := os.read_file(file_path) or { return error('failed to open $file_path') }
	return skip_bom(raw_text)
}

pub fn skip_bom(file_content string) string {
	mut raw_text := file_content
	// BOM check
	if raw_text.len >= 3 {
		unsafe {
			c_text := raw_text.str
			if c_text[0] == 0xEF && c_text[1] == 0xBB && c_text[2] == 0xBF {
				// skip three BOM bytes
				offset_from_begin := 3
				raw_text = tos(c_text[offset_from_begin], vstrlen(c_text) - offset_from_begin)
			}
		}
	}
	return raw_text
}

[inline]
pub fn imin(a int, b int) int {
	return if a < b { a } else { b }
}

[inline]
pub fn imax(a int, b int) int {
	return if a > b { a } else { b }
}

[inline]
pub fn iabs(v int) int {
	return if v > 0 { v } else { -v }
}

pub fn replace_op(s string) string {
	if s.len == 1 {
		last_char := s[s.len - 1]
		suffix := match last_char {
			`+` { '_plus' }
			`-` { '_minus' }
			`*` { '_mult' }
			`/` { '_div' }
			`%` { '_mod' }
			`<` { '_lt' }
			`>` { '_gt' }
			else { '' }
		}
		return s[..s.len - 1] + suffix
	} else {
		suffix := match s {
			'==' { '_eq' }
			else { '' }
		}
		return s[..s.len - 2] + suffix
	}
}

pub fn join_env_vflags_and_os_args() []string {
	vosargs := os.getenv('VOSARGS')
	if vosargs != '' {
		return non_empty(vosargs.split(' '))
	}
	mut args := []string{}
	vflags := os.getenv('VFLAGS')
	if vflags != '' {
		args << os.args[0]
		args << vflags.split(' ')
		if os.args.len > 1 {
			args << os.args[1..]
		}
		return non_empty(args)
	}
	return non_empty(os.args)
}

fn non_empty(arg []string) []string {
	return arg.filter(it != '')
}

pub fn check_module_is_installed(modulename string, is_verbose bool) ?bool {
	mpath := os.join_path(os.vmodules_dir(), modulename)
	mod_v_file := os.join_path(mpath, 'v.mod')
	murl := 'https://github.com/vlang/$modulename'
	if is_verbose {
		eprintln('check_module_is_installed: mpath: $mpath')
		eprintln('check_module_is_installed: mod_v_file: $mod_v_file')
		eprintln('check_module_is_installed: murl: $murl')
	}
	if os.exists(mod_v_file) {
		vexe := pref.vexe_path()
		update_cmd := '"$vexe" update "$modulename"'
		if is_verbose {
			eprintln('check_module_is_installed: updating with $update_cmd ...')
		}
		update_res := os.exec(update_cmd) or {
			return error('can not start $update_cmd, error: $err')
		}
		if update_res.exit_code != 0 {
			eprintln('Warning: `$modulename` exists, but is not updated.
V will continue, since updates can fail due to temporary network problems,
and the existing module `$modulename` may still work.')
			if is_verbose {
				eprintln('Details:')
				eprintln(update_res.output)
			}
			eprintln('-'.repeat(50))
		}
		return true
	}
	if is_verbose {
		eprintln('check_module_is_installed: cloning from $murl ...')
	}
	cloning_res := os.exec('git clone $murl $mpath') or {
		return error('git is not installed, error: $err')
	}
	if cloning_res.exit_code != 0 {
		return error('cloning failed, details: $cloning_res.output')
	}
	if !os.exists(mod_v_file) {
		return error('even after cloning, $mod_v_file is still missing')
	}
	if is_verbose {
		eprintln('check_module_is_installed: done')
	}
	return true
}

pub fn ensure_modules_for_all_tools_are_installed(is_verbose bool) {
	for tool_name, tool_modules in util.external_module_dependencies_for_tool {
		if is_verbose {
			eprintln('Installing modules for tool: $tool_name ...')
		}
		for emodule in tool_modules {
			check_module_is_installed(emodule, is_verbose) or { panic(err) }
		}
	}
}

pub fn strip_mod_name(name string) string {
	return name.all_after_last('.')
}

pub fn strip_main_name(name string) string {
	return name.replace('main.', '')
}

pub fn no_dots(s string) string {
	return s.replace('.', '__')
}

const (
	map_prefix = 'map[string]'
)

// no_cur_mod - removes cur_mod. prefix from typename,
// but *only* when it is at the start, i.e.:
// no_cur_mod('vproto.Abdcdef', 'proto') == 'vproto.Abdcdef'
// even though proto. is a substring
pub fn no_cur_mod(typename string, cur_mod string) string {
	mut res := typename
	mod_prefix := cur_mod + '.'
	has_map_prefix := res.starts_with(util.map_prefix)
	if has_map_prefix {
		res = res.replace_once(util.map_prefix, '')
	}
	no_symbols := res.trim_left('&[]')
	should_shorten := no_symbols.starts_with(mod_prefix)
	if should_shorten {
		res = res.replace_once(mod_prefix, '')
	}
	if has_map_prefix {
		res = util.map_prefix + res
	}
	return res
}

pub fn prepare_tool_when_needed(source_name string) {
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	stool := os.join_path(vroot, 'cmd', 'tools', source_name)
	tool_name, tool_exe := tool_source2name_and_exe(stool)
	if should_recompile_tool(vexe, stool, tool_name, tool_exe) {
		time.sleep(1001 * time.millisecond) // TODO: remove this when we can get mtime with a better resolution
		recompile_file(vexe, stool)
	}
}

pub fn recompile_file(vexe string, file string) {
	cmd := '$vexe $file'
	$if trace_recompilation ? {
		println('recompilation command: $cmd')
	}
	recompile_result := os.system(cmd)
	if recompile_result != 0 {
		eprintln('could not recompile $file')
		exit(2)
	}
}

pub fn get_vtmp_folder() string {
	mut vtmp := os.getenv('VTMP')
	if vtmp.len > 0 {
		return vtmp
	}
	vtmp = os.join_path(os.temp_dir(), 'v')
	if !os.exists(vtmp) || !os.is_dir(vtmp) {
		os.mkdir_all(vtmp) or { panic(err) }
	}
	os.setenv('VTMP', vtmp, true)
	return vtmp
}

pub fn should_bundle_module(mod string) bool {
	return mod in util.bundle_modules
		|| (mod.contains('.') && mod.all_before('.') in util.bundle_modules)
}
