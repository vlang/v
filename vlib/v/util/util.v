// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module util

import os
import os.filelock
import term
import rand
import time
import v.pref
import v.util.recompilation
import v.util.vflags
import runtime

// math.bits is needed by strconv.ftoa
pub const builtin_module_parts = ['math.bits', 'strconv', 'dlmalloc', 'strconv.ftoa', 'strings',
	'builtin']
pub const bundle_modules = ['clipboard', 'fontstash', 'gg', 'gx', 'sokol', 'szip', 'ui']!

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
]!

pub const nr_jobs = runtime.nr_jobs()

pub fn module_is_builtin(mod string) bool {
	return mod in builtin_module_parts
}

@[direct_array_access]
pub fn tabs(n int) string {
	return if n >= 0 && n < const_tabs.len { const_tabs[n] } else { '\t'.repeat(n) }
}

pub const stable_build_time = get_build_time()
// get_build_time returns the current build time, while taking into account SOURCE_DATE_EPOCH
// to support transparent reproducible builds. See also https://reproducible-builds.org/docs/source-date-epoch/
// When SOURCE_DATE_EPOCH is not set, it will return the current UTC time.
pub fn get_build_time() time.Time {
	sde := os.getenv('SOURCE_DATE_EPOCH')
	if sde == '' {
		return time.utc()
	}
	return time.unix_nanosecond(sde.i64(), 0)
}

// set_vroot_folder sets the VCHILD env variable to 'true', and VEXE to the location of the V executable
// It is called very early by launch_tool/3, so that those env variables can be available by all tools,
// like `v doc`, `v fmt` etc, so they can use them to find how they were started.
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

// is_escape_sequence returns `true` if `c` is considered a valid escape sequence denoter.
@[inline]
pub fn is_escape_sequence(c u8) bool {
	return c in [`x`, `u`, `e`, `n`, `r`, `t`, `v`, `a`, `f`, `b`, `\\`, `\``, `$`, `@`, `?`, `{`,
		`}`, `'`, `"`, `U`]
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
		emodules := external_module_dependencies_for_tool[tool_name]
		for emodule in emodules {
			check_module_is_installed(emodule, is_verbose, false) or { panic(err) }
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
		tlog('recompiling ${tool_source}')
		lockfile := tool_exe + '.lock'
		tlog('lockfile: ${lockfile}')
		mut l := filelock.new(lockfile)
		if l.try_acquire() {
			tlog('lockfile acquired')
			tool_recompile_retry_max_count := 7
			for i in 0 .. tool_recompile_retry_max_count {
				tlog('looping i: ${i} / ${tool_recompile_retry_max_count}')
				// ensure a stable and known working folder, when compiling V's tools, to avoid module lookup problems:
				os.chdir(vroot) or {}
				tool_compilation := os.execute(compilation_command)
				os.chdir(current_work_dir) or {}
				tlog('tool_compilation.exit_code: ${tool_compilation.exit_code}')
				if tool_compilation.exit_code == 0 {
					break
				} else {
					if tool_name == 'vup' {
						eprintln('Cannot recompile the new version of `vup`: ${tool_compilation.exit_code}\n${tool_compilation.output}')
						if os.exists(tool_exe) {
							// Compilation failed, but we still have an already existing old `vup.exe`, that *probably* works.
							// It is better to pretend the compilation succeeded, and try the old executable, then let it fail
							// on its own, if it can not work too (it will produce a nicer diagnostic message), than to fail here
							// right away, just because the new source is too breaking, for the older V frontend process,
							// that is currently running :-|
							eprintln('Trying an already existing old version of the `vup` tool instead...')
							break
						} else {
							// No pre-existing `vup.exe` ... No choice but to show a message to the user :-( .
							// Note: running `make` here from within the old V frontend process, is not reliable, since it can fail
							// on windows and probably other systems, because the currently running executable is locked.
							// `vup.exe` has logic to workaround that, and duplicating it here, is hard to debug/diagnose.
							eprintln('Failed compilation of the `vup` tool, using the new V source code.')
							eprintln('The new source code, is likely to be unsupported, by your existing older V executable.')
							eprintln('Try running `make` or `make.bat` manually.')
							eprintln('If that fails, clone V from source in a new folder, and run `make` or `make.bat` manually again there.')
							l.release()
							exit(1)
						}
					}
					if i == tool_recompile_retry_max_count - 1 {
						eprintln('cannot compile `${tool_source}`: ${tool_compilation.exit_code}\n${tool_compilation.output}')
						l.release()
						exit(1)
					}
				}
				time.sleep((20 + rand.intn(40) or { 0 }) * time.millisecond)
			}
			tlog('lockfile releasing')
			l.release()
			tlog('lockfile released')
		} else {
			tlog('another process got the lock')
			// wait till the other V tool recompilation process finished:
			if l.wait_acquire(10 * time.second) {
				tlog('the other process finished')
				l.release()
			} else {
				tlog('timeout...')
			}
			time.sleep((50 + rand.intn(40) or { 0 }) * time.millisecond)
			tlog('result of the other process compiling ${tool_exe}: ${os.exists(tool_exe)}')
		}
	}
	tlog('executing: ${tool_exe} with ${tool_args}')
	$if windows {
		cmd_system('${os.quoted_path(tool_exe)} ${tool_args}')
	} $else $if js {
		// no way to implement os.execvp in JS backend
		cmd_system('${tool_exe} ${tool_args}')
	} $else {
		os.execvp(tool_exe, args) or {
			eprintln('> error while executing: ${tool_exe} ${args}')
			panic(err)
		}
	}
	exit(2)
}

@[if trace_launch_tool ?]
fn tlog(s string) {
	ts := time.now().format_ss_micro()
	eprintln('${term.yellow(ts)} ${term.gray(s)}')
}

@[noreturn]
fn cmd_system(cmd string) {
	res := os.system(cmd)
	if res != 0 {
		tlog('> error ${res}, while executing: ${cmd}')
	}
	exit(res)
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
	// TODO: Caching should be done on the `vlib/v` level.
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
	return args.map(quote_path(it)).join(' ')
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
	if path == '' {
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

// join_env_vflags_and_os_args returns all the arguments (the ones from the env variable VFLAGS too), passed on the command line.
pub fn join_env_vflags_and_os_args() []string {
	return vflags.join_env_vflags_and_os_args()
}

pub fn check_module_is_installed(modulename string, is_verbose bool, need_update bool) !bool {
	mpath := os.join_path_single(os.vmodules_dir(), modulename)
	mod_v_file := os.join_path_single(mpath, 'v.mod')
	murl := 'https://github.com/vlang/${modulename}'
	if is_verbose {
		eprintln('check_module_is_installed: mpath: ${mpath}')
		eprintln('check_module_is_installed: mod_v_file: ${mod_v_file}')
		eprintln('check_module_is_installed: murl: ${murl}')
	}
	vexe := pref.vexe_path()
	if os.exists(mod_v_file) {
		if need_update {
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
		}
		return true
	}
	if is_verbose {
		eprintln('check_module_is_installed: cloning from ${murl} ...')
	}
	cloning_res := os.execute('${os.quoted_path(vexe)} retry -- git clone ${os.quoted_path(murl)} ${os.quoted_path(mpath)}')
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
	for tool_name, tool_modules in external_module_dependencies_for_tool {
		if is_verbose {
			eprintln('Installing modules for tool: ${tool_name} ...')
		}
		for emodule in tool_modules {
			check_module_is_installed(emodule, is_verbose, false) or { panic(err) }
		}
	}
}

@[inline]
pub fn strip_mod_name(name string) string {
	return name.all_after_last('.')
}

@[inline]
pub fn strip_main_name(name string) string {
	return name.replace('main.', '')
}

@[inline]
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
	has_map_prefix := res.starts_with(map_prefix)
	if has_map_prefix {
		res = res.replace_once(map_prefix, '')
	}
	no_symbols := res.trim_left('&[]')
	should_shorten := no_symbols.starts_with(mod_prefix)
	if should_shorten {
		res = res.replace_once(mod_prefix, '')
	}
	if has_map_prefix {
		res = map_prefix + res
	}
	return res
}

pub fn prepare_tool_when_needed(source_name string) {
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	stool := os.join_path(vroot, 'cmd', 'tools', source_name)
	tool_name, tool_exe := tool_source2name_and_exe(stool)
	if should_recompile_tool(vexe, stool, tool_name, tool_exe) {
		time.sleep((1001 + rand.intn(20) or { 0 }) * time.millisecond) // TODO: remove this when we can get mtime with a better resolution
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
	return mod in bundle_modules || (mod.contains('.') && mod.all_before('.') in bundle_modules)
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
