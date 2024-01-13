// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

import os
import time
import v.pref
import v.vmod
import v.util.recompilation
import runtime

// math.bits is needed by strconv.ftoa
pub const builtin_module_parts = ['math.bits', 'strconv', 'dlmalloc', 'strconv.ftoa', 'strings',
	'builtin']
pub const bundle_modules = ['clipboard', 'fontstash', 'gg', 'gx', 'sokol', 'szip', 'ui']

pub const external_module_dependencies_for_tool = {
	'vdoc': ['markdown']
}

const const_tabs = [
	'',
	'\t',
	'\t\t',
	'\t\t\t',
	'\t\t\t\t',
	'\t\t\t\t\t',
	'\t\t\t\t\t\t',
	'\t\t\t\t\t\t\t',
	'\t\t\t\t\t\t\t\t',
	'\t\t\t\t\t\t\t\t\t',
	'\t\t\t\t\t\t\t\t\t\t',
]

pub const nr_jobs = runtime.nr_jobs()

pub fn module_is_builtin(mod string) bool {
	return mod in util.builtin_module_parts
}

pub fn tabs(n int) string {
	return if n < util.const_tabs.len { util.const_tabs[n] } else { '\t'.repeat(n) }
}

//
pub fn set_vroot_folder(vroot_path string) {
	// Preparation for the compiler module:
	// VEXE env variable is needed so that compiler.vexe_path() can return it
	// later to whoever needs it. Note: guessing is a heuristic, so only try to
	// guess the V executable name, if VEXE has not been set already.
	vexe := os.getenv('VEXE')
	if vexe == '' {
		vname := if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
		os.setenv('VEXE', os.real_path(os.join_path_single(vroot_path, vname)), true)
	}
	os.setenv('VCHILD', 'true', true)
}

pub fn resolve_vmodroot(str string, dir string) !string {
	mut mcache := vmod.get_cache()
	vmod_file_location := mcache.get_by_folder(dir)
	if vmod_file_location.vmod_file.len == 0 {
		// There was no actual v.mod file found.
		return error('To use @VMODROOT, you need to have a "v.mod" file in ${dir}, or in one of its parent folders.')
	}
	vmod_path := vmod_file_location.vmod_folder
	return str.replace('@VMODROOT', os.real_path(vmod_path))
}

// resolve_env_value replaces all occurrences of `$env('ENV_VAR_NAME')`
// in `str` with the value of the env variable `$ENV_VAR_NAME`.
pub fn resolve_env_value(str string, check_for_presence bool) !string {
	env_ident := "\$env('"
	at := str.index(env_ident) or {
		return error('no "${env_ident}' + '...\')" could be found in "${str}".')
	}
	mut ch := u8(`.`)
	mut env_lit := ''
	for i := at + env_ident.len; i < str.len && ch != `)`; i++ {
		ch = u8(str[i])
		if ch.is_letter() || ch.is_digit() || ch == `_` {
			env_lit += ch.ascii_str()
		} else {
			if !(ch == `'` || ch == `)`) {
				if ch == `$` {
					return error('cannot use string interpolation in compile time \$env() expression')
				}
				return error('invalid environment variable name in "${str}", invalid character "${ch.ascii_str()}"')
			}
		}
	}
	if env_lit == '' {
		return error('supply an env variable name like HOME, PATH or USER')
	}
	mut env_value := ''
	if check_for_presence {
		env_value = os.environ()[env_lit] or {
			return error('the environment variable "${env_lit}" does not exist.')
		}
		if env_value == '' {
			return error('the environment variable "${env_lit}" is empty.')
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
@[noreturn]
pub fn launch_tool(is_verbose bool, tool_name string, args []string) {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	set_vroot_folder(vroot)
	tool_args := args_quote_paths(args)
	tools_folder := os.join_path(vroot, 'cmd', 'tools')
	tool_basename := os.real_path(os.join_path_single(tools_folder, tool_name))
	mut tool_exe := ''
	mut tool_source := ''
	if os.is_dir(tool_basename) {
		tool_exe = path_of_executable(os.join_path_single(tool_basename, os.file_name(tool_name)))
		tool_source = tool_basename
	} else {
		tool_exe = path_of_executable(tool_basename)
		tool_source = tool_basename + '.v'
	}
	if is_verbose {
		println('launch_tool vexe        : ${vexe}')
		println('launch_tool vroot       : ${vroot}')
		println('launch_tool tool_source : ${tool_source}')
		println('launch_tool tool_exe    : ${tool_exe}')
		println('launch_tool tool_args   : ${tool_args}')
	}
	disabling_file := recompilation.disabling_file(vroot)
	is_recompilation_disabled := os.exists(disabling_file)
	should_compile := !is_recompilation_disabled
		&& should_recompile_tool(vexe, tool_source, tool_name, tool_exe)
	if is_verbose {
		println('launch_tool should_compile: ${should_compile}')
	}
	if should_compile {
		emodules := util.external_module_dependencies_for_tool[tool_name]
		for emodule in emodules {
			check_module_is_installed(emodule, is_verbose) or { panic(err) }
		}
		mut compilation_command := '${os.quoted_path(vexe)} '
		if tool_name in ['vself', 'vup', 'vdoctor', 'vsymlink'] {
			// These tools will be called by users in cases where there
			// is high chance of there being a problem somewhere. Thus
			// it is better to always compile them with -g, so that in
			// case these tools do crash/panic, their backtraces will have
			// .v line numbers, to ease diagnostic in #bugs and issues.
			compilation_command += ' -g '
		}
		if tool_name == 'vfmt' {
			compilation_command += ' -d vfmt '
		}
		compilation_command += os.quoted_path(tool_source)
		if is_verbose {
			println('Compiling ${tool_name} with: "${compilation_command}"')
		}

		current_work_dir := os.getwd()
		retry_max_count := 3
		for i in 0 .. retry_max_count {
			// ensure a stable and known working folder, when compiling V's tools, to avoid module lookup problems:
			os.chdir(vroot) or {}
			tool_compilation := os.execute(compilation_command)
			os.chdir(current_work_dir) or {}
			if tool_compilation.exit_code == 0 {
				break
			} else {
				if i == retry_max_count - 1 {
					eprintln('cannot compile `${tool_source}`: \n${tool_compilation.output}')
					exit(1)
				}
				time.sleep(20 * time.millisecond)
			}
		}
	}
	$if windows {
		exit(os.system('${os.quoted_path(tool_exe)} ${tool_args}'))
	} $else $if js {
		// no way to implement os.execvp in JS backend
		exit(os.system('${tool_exe} ${tool_args}'))
	} $else {
		os.execvp(tool_exe, args) or { panic(err) }
	}
	exit(2)
}

// Note: should_recompile_tool/4 compares unix timestamps that have 1 second resolution
// That means that a tool can get recompiled twice, if called in short succession.
// TODO: use a nanosecond mtime timestamp, if available.
pub fn should_recompile_tool(vexe string, tool_source string, tool_name string, tool_exe string) bool {
	if os.is_dir(tool_source) {
		source_files := os.walk_ext(tool_source, '.v')
		mut newest_sfile := ''
		mut newest_sfile_mtime := i64(0)
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
	tool_exe := os.join_path_single(sfolder, path_of_executable(tool_name))
	return tool_name, tool_exe
}

pub fn quote_path(s string) string {
	return os.quoted_path(s)
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

@[heap]
struct SourceCache {
mut:
	sources map[string]string
}

@[unsafe]
pub fn cached_read_source_file(path string) !string {
	mut static cache := &SourceCache(unsafe { nil })
	if cache == unsafe { nil } {
		cache = &SourceCache{}
	}

	$if trace_cached_read_source_file ? {
		println('cached_read_source_file            ${path}')
	}
	if path.len == 0 {
		unsafe { cache.sources.free() }
		unsafe { free(cache) }
		cache = &SourceCache(unsafe { nil })
		return error('memory source file cache cleared')
	}

	// eprintln('>> cached_read_source_file path: $path')
	if res := cache.sources[path] {
		// eprintln('>> cached')
		$if trace_cached_read_source_file_cached ? {
			println('cached_read_source_file     cached ${path}')
		}
		return res
	}
	// eprintln('>> not cached | cache.sources.len: $cache.sources.len')
	$if trace_cached_read_source_file_not_cached ? {
		println('cached_read_source_file not cached ${path}')
	}
	raw_text := os.read_file(path) or { return error('failed to open ${path}') }
	res := skip_bom(raw_text)
	cache.sources[path] = res
	return res
}

pub fn replace_op(s string) string {
	return match s {
		'+' { '_plus' }
		'-' { '_minus' }
		'*' { '_mult' }
		'/' { '_div' }
		'%' { '_mod' }
		'<' { '_lt' }
		'>' { '_gt' }
		'==' { '_eq' }
		else { '' }
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
	return os.args
}

fn non_empty(arg []string) []string {
	return arg.filter(it != '')
}

pub fn check_module_is_installed(modulename string, is_verbose bool) !bool {
	mpath := os.join_path_single(os.vmodules_dir(), modulename)
	mod_v_file := os.join_path_single(mpath, 'v.mod')
	murl := 'https://github.com/vlang/${modulename}'
	if is_verbose {
		eprintln('check_module_is_installed: mpath: ${mpath}')
		eprintln('check_module_is_installed: mod_v_file: ${mod_v_file}')
		eprintln('check_module_is_installed: murl: ${murl}')
	}
	if os.exists(mod_v_file) {
		vexe := pref.vexe_path()
		update_cmd := "${os.quoted_path(vexe)} update '${modulename}'"
		if is_verbose {
			eprintln('check_module_is_installed: updating with ${update_cmd} ...')
		}
		update_res := os.execute(update_cmd)
		if update_res.exit_code < 0 {
			return error('can not start ${update_cmd}, error: ${update_res.output}')
		}
		if update_res.exit_code != 0 {
			eprintln('Warning: `${modulename}` exists, but is not updated.
V will continue, since updates can fail due to temporary network problems,
and the existing module `${modulename}` may still work.')
			if is_verbose {
				eprintln('Details:')
				eprintln(update_res.output)
			}
			eprintln('-'.repeat(50))
		}
		return true
	}
	if is_verbose {
		eprintln('check_module_is_installed: cloning from ${murl} ...')
	}
	cloning_res := os.execute('git clone ${os.quoted_path(murl)} ${os.quoted_path(mpath)}')
	if cloning_res.exit_code < 0 {
		return error_with_code('git is not installed, error: ${cloning_res.output}', cloning_res.exit_code)
	}
	if cloning_res.exit_code != 0 {
		return error_with_code('cloning failed, details: ${cloning_res.output}', cloning_res.exit_code)
	}
	if !os.exists(mod_v_file) {
		return error('even after cloning, ${mod_v_file} is still missing')
	}
	if is_verbose {
		eprintln('check_module_is_installed: done')
	}
	return true
}

pub fn ensure_modules_for_all_tools_are_installed(is_verbose bool) {
	for tool_name, tool_modules in util.external_module_dependencies_for_tool {
		if is_verbose {
			eprintln('Installing modules for tool: ${tool_name} ...')
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

const map_prefix = 'map[string]'

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
	cmd := '${os.quoted_path(vexe)} ${os.quoted_path(file)}'
	$if trace_recompilation ? {
		println('recompilation command: ${cmd}')
	}
	recompile_result := os.system(cmd)
	if recompile_result != 0 {
		eprintln('could not recompile ${file}')
		exit(2)
	}
}

// get_vtmp_folder returns the path to a folder, that is writable to V programs,
// and specific to the user. It can be overridden by setting the env variable `VTMP`.
pub fn get_vtmp_folder() string {
	return os.vtmp_dir()
}

pub fn should_bundle_module(mod string) bool {
	return mod in util.bundle_modules
		|| (mod.contains('.') && mod.all_before('.') in util.bundle_modules)
}

// find_all_v_files - given a list of files/folders, finds all .v/.vsh files
// if some of the files/folders on the list does not exist, or a file is not
// a .v or .vsh file, returns an error instead.
pub fn find_all_v_files(roots []string) ![]string {
	mut files := []string{}
	for file in roots {
		if os.is_dir(file) {
			files << os.walk_ext(file, '.v')
			files << os.walk_ext(file, '.vsh')
			continue
		}
		if !file.ends_with('.v') && !file.ends_with('.vv') && !file.ends_with('.vsh') {
			return error('v fmt can only be used on .v files.\nOffending file: "${file}"')
		}
		if !os.exists(file) {
			return error('"${file}" does not exist')
		}
		files << file
	}
	return files
}

// free_caches knows about all `util` caches and makes sure that they are freed
// if you add another cached unsafe function using static, do not forget to add
// a mechanism to clear its cache, and call it here.
pub fn free_caches() {
	unsafe {
		cached_file2sourcelines('')
		cached_read_source_file('') or { '' }
	}
}

pub fn read_file(file_path string) !string {
	return unsafe { cached_read_source_file(file_path) }
}
