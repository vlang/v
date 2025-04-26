module main

import os
import log
import flag
import time
import runtime
import regex
import strings
import strings.textscanner
import toml

// This tool generate prebuilt versions
// (or latest version by default) of:
// A) TCC from git://repo.or.cz/tinycc.git
// B) libgc from https://github.com/ivmai/bdwgc/

// pre-install packages:
// windows: vs tools
// freebsd: git,automake,libtool,gmake,boehm-gc-threaded-8.2.8,
// openbsd: git,automake,libtool,gmake,boehm-gc,

// available command line flags:
// --restore : restore from backup dir

const machine = os.uname().machine
const os_detail = os.user_os()
const os_kind = if os_detail == 'windows' { 'windows' } else { 'nix' }
const git_commit_info_format = 'commit %H%nDate: %aI%nDate Unix: %ct%nSubject: %s'

// builder
// name
const app_name = 'builder'

// version
const app_version = '0.1.0'

// description
const app_description = 'This tool generate prebuilt versions of tcc and libgc.'

// storage for flag options
struct FlagOptions {
	restore bool
}

struct Builder {
mut:
	logger             &log.ThreadSafeLog = unsafe { nil }
	doc                toml.Doc
	all_tool_available bool = true
	pwd                string
	work_dir           string
	win_sys_dir        string
	amalgamate         string
	skip_git           bool
	all_v_ref_funcs    []string
}

fn main() {
	log.use_stdout()
	mut fp := flag.new_flag_parser(os.args.clone())
	fp.application(app_name)
	fp.version(app_version)
	fp.description(app_description)
	fp.skip_executable()
	show_help := fp.bool('help', 0, false, 'Show this help screen\n')
	flag_options := parse_flags(mut fp)
	if show_help {
		println(fp.usage())
		exit(0)
	}
	fp.finalize() or {
		eprintln(err)
		println(fp.usage())
		return
	}
	doc := toml.parse_file('updater.toml')!
	mut builder := new_builder(doc)
	if flag_options.restore {
		builder.logger.info('restore tcc/libgc from backup dirs')
		if !os.is_dir(@VEXEROOT + '/thirdparty/tcc.work') {
			builder.logger.info('restore tcc fail, no dir : ${@VEXEROOT}/thirdparty/tcc.work')
		}
		if !os.is_dir(@VEXEROOT + '/thirdparty/libgc.work') {
			builder.logger.info('restore libgc fail, no dir : ${@VEXEROOT}/thirdparty/libgc.work')
		}
		if os.is_dir(@VEXEROOT + '/thirdparty/tcc') {
			os.rmdir_all(@VEXEROOT + '/thirdparty/tcc')!
		}
		if os.is_dir(@VEXEROOT + '/thirdparty/libgc') {
			os.rmdir_all(@VEXEROOT + '/thirdparty/libgc')!
		}
		os.cp_all(@VEXEROOT + '/thirdparty/tcc.work', @VEXEROOT + '/thirdparty/tcc', true)!
		os.cp_all(@VEXEROOT + '/thirdparty/libgc.work', @VEXEROOT + '/thirdparty/libgc',
			true)!
		return
	}
	builder.pwd = os.getwd()
	if os_kind == 'windows' {
		env := os.environ()
		builder.win_sys_dir = if runtime.is_32bit() {
			env['windir'] + '\\system32'
		} else {
			env['windir'] + '\\SysWOW64'
		}
	}
	builder.init()
	if builder.all_tool_available {
		start_time := time.now()
		builder.backup_current('tcc')!
		builder.backup_current('libgc')!
		builder.download('tcc', builder.doc.value('download.commit_tcc').string())!
		builder.download('bdwgc', builder.doc.value('download.commit_bdwgc').string())!
		builder.apply_patch('tcc')!
		builder.build_tcc()!
		if os_kind == 'windows' {
			builder.rebuild_tcc_def_files()!
		}
		builder.copy_tcc()!
		builder.amalgamate_bdwgc()!
		builder.apply_patch('bdwgc')!

		builder.copy_extra_files()!

		builder.build_bdwgc()!
		builder.copy_bdwgc()!

		if os_kind == 'windows' && builder.doc.value('windows.opt_def').bool() {
			builder.scan_all_v_files_for_c_function_names()!
			builder.cross_check_func_name_and_rewrite_def_files()!
		}

		if builder.doc.value('global.clean_up').bool() {
			// clean up, delete all files
			if os.is_dir(builder.work_dir) {
				os.rmdir_all(builder.work_dir)!
			}
		}
		used_time := time.now() - start_time
		builder.logger.info('complete in ${used_time}')
	}
	builder.logger.info('done')
}

// new Builder
fn new_builder(doc toml.Doc) &Builder {
	mut logger := log.new_thread_safe_log()
	mut log_level := doc.value('global.log_level').int()
	if log_level <= 0 {
		log_level = 0
	} else if log_level >= 5 {
		log_level = 5
	}
	logger.set_level(unsafe { log.Level(log_level) })
	logger.set_time_format(.tf_ss)
	logger.set_short_tag(true)
	logger.set_local_time(true)
	if doc.value('global.log_to').string() == 'file' {
		logger.set_full_logpath(doc.value('global.log_file').string())
	}
	return &Builder{
		doc:      doc
		logger:   logger
		work_dir: doc.value('global.work_dir').string().replace('\${vtmp}', os.vtmp_dir())
		skip_git: doc.value('global.skip_git').bool()
	}
}

// parse flags to FlagOptions struct
fn parse_flags(mut fp flag.FlagParser) FlagOptions {
	return FlagOptions{
		restore: fp.bool('restore', 0, false, 'restore from backup dir')
	}
}

fn (mut builder Builder) init() {
	builder.logger.info('system info: machine=${machine} os=${os_detail}')
	// check for tool available
	builder.logger.info('check for tool availablity')
	for tool_array in builder.doc.value('tools_need.${os_kind}').array() {
		tool := tool_array.array()
		output := builder.cmd_exec(tool[1].string(), force_show: true)
		if !output.contains(tool[2].string()) {
			builder.logger.error('checking for tool fail: ${tool[0].string()}')
			builder.all_tool_available = false
		}
	}

	// build amalgamate tool if need
	builder.amalgamate = '${@VEXEROOT}/cmd/tools/amalgamate'
	mut need_build_amalgamate := false
	if os_kind == 'windows' && !os.exists(builder.amalgamate + '.exe') {
		need_build_amalgamate = true
	} else if !os.exists(builder.amalgamate) {
		need_build_amalgamate = true
	}

	if need_build_amalgamate {
		builder.logger.info('build amalgamate tool')
		builder.cmd_exec('v ${builder.amalgamate}.v')
	}

	if os_kind == 'windows' {
		builder.amalgamate += '.exe'
	}
	builder.amalgamate = os.from_slash(builder.amalgamate)

	if os.exists(builder.amalgamate) {
		builder.logger.debug('found amalgamate : ${builder.amalgamate}')
	} else {
		builder.logger.error('not found amalgamate : ${builder.amalgamate}')
		builder.all_tool_available = false
	}
}

fn (mut builder Builder) backup_current(tool string) ! {
	// backup
	builder.logger.info('backup current ${tool}')
	if os.is_dir(@VEXEROOT + '/thirdparty/${tool}.work') {
		os.rmdir_all(@VEXEROOT + '/thirdparty/${tool}.work')!
	}
	os.rename_dir(@VEXEROOT + '/thirdparty/${tool}', @VEXEROOT + '/thirdparty/${tool}.work')!
	os.mkdir(@VEXEROOT + '/thirdparty/${tool}')!
}

fn (mut builder Builder) download(tool string, commit string) ! {
	builder.logger.info('download ${tool}')

	// check if builder dir exists
	if !os.is_dir(builder.work_dir) {
		// try create
		os.mkdir(builder.work_dir)!

		// still doesn't exist... we have a problem
		if !os.is_dir(builder.work_dir) {
			builder.logger.error('error creating directory: ${builder.work_dir}')
			return
		}
	}

	// cd to builder dir
	os.chdir(builder.work_dir)!

	git_repo_url := builder.doc.value('download.${tool}').string()
	git_repo_dir := os.from_slash(builder.work_dir + '/' + tool)
	if !builder.skip_git || !os.is_dir(git_repo_dir) {
		// clone repos
		builder.cmd_exec('git clone --filter=blob:none ${git_repo_url} ${git_repo_dir}')
	}

	// checkout
	builder.cmd_exec('git -C ${git_repo_dir} checkout -f ${commit}')

	// get output of git log -1 (last commit)
	git_log := builder.cmd_exec('git -C ${git_repo_dir} log -1 --format="${git_commit_info_format}"')

	// date of last commit in each repo
	ts := git_log.find_between('Date:', '\n').trim_space()

	// parse time as string to time.Time
	last_commit_time := time.parse_iso8601(ts)!

	// last commit hash in repo
	last_commit_hash := git_log.find_between('commit', '\n').trim_space()

	// subject
	last_commit_subject := git_log.find_between('Subject:', '\n').trim_space().replace("'",
		'"')

	// log some info
	builder.logger.debug('[${tool:8}]last commit time: ' + last_commit_time.format_ss())
	builder.logger.debug('[${tool:8}]last commit hash: ${last_commit_hash}')
	builder.logger.debug('[${tool:8}]last commit subject: ${last_commit_subject}')
}

fn (mut builder Builder) get_doc_value(key_string string) !toml.Any {
	// priority: machine > os_detail > os_kind > default
	// for example, on a linux/aarch64, aarch64 > linux > nix > default
	if value_machine := builder.doc.value_opt('${machine}.${key_string}') {
		return value_machine
	} else if value_os_detail := builder.doc.value_opt('${os_detail}.${key_string}') {
		return value_os_detail
	} else if value_os_kind := builder.doc.value_opt('${os_kind}.${key_string}') {
		return value_os_kind
	} else if value_default := builder.doc.value_opt('default.${key_string}') {
		return value_default
	} else {
		return error('can not find a key [${key_string}] in toml file')
	}
}

fn (mut builder Builder) replace_string(input string) string {
	mut output := input
	output = output.replace('\${vtmp}', os.vtmp_dir())
		.replace('\${work_dir_tcc}', builder.work_dir + '/tcc')
		.replace('\${work_dir_bdwgc}', builder.work_dir + '/bdwgc')
		.replace('\${amalgamate}', builder.amalgamate)
		.replace('\${tcc}', @VEXEROOT + '/thirdparty/tcc/tcc.exe')
		.replace('\${machine}', machine)
		.trim_space()
	if output.contains('\$') {
		builder.logger.error('unknown \${} in string: ${output}')
	}
	return output
}

fn (mut builder Builder) amalgamate_bdwgc() ! {
	builder.logger.info('amalgamate bdwgc')
	bdwgc_dir := builder.work_dir + '/bdwgc'
	os.chdir(bdwgc_dir)!

	// because we did not run `configure` yet, so there has no `config.h`, create a dummy `config.h` for amalgamate work.
	config_file := '${bdwgc_dir}/include/config.h'
	if !os.exists(config_file) {
		os.write_file(config_file, 'dummy')!
	}
	cmd := builder.replace_string(builder.get_doc_value('amalgamate_bdwgc')!.string())
	builder.cmd_exec(cmd)
}

fn (mut builder Builder) apply_patch(tool string) ! {
	builder.logger.info('apply patch for ${tool}')
	// check for patches
	file_list := os.ls('${builder.pwd}/patches/')!

	// cd to builder dir
	tool_dir := os.from_slash(builder.work_dir + '/' + tool)
	os.chdir(tool_dir)!

	for f in file_list {
		if f.starts_with('${os_kind}-${tool}-') {
			patch_file_name := os.from_slash(builder.pwd + '/patches/' + f)
			msg := f.all_after('${os_kind}-${tool}-')
			builder.logger.debug('[${tool:8}]git apply patch=>[${msg}]')
			builder.cmd_exec('git apply ${patch_file_name}')
		}
	}
}

fn (mut builder Builder) build_tcc() ! {
	// try build tcc for current os
	builder.logger.info('build tcc for ${os_detail}')
	work_dir_tcc := os.from_slash(builder.work_dir + '/tcc')
	os.chdir(work_dir_tcc)!
	config_cmd := builder.replace_string(builder.get_doc_value('config_tcc')!.string())
	if os_kind == 'nix' {
		builder.cmd_exec(config_cmd)
		builder.cmd_exec('gmake')
		builder.cmd_exec('gmake install')
	} else {
		// windows
		os.chdir(work_dir_tcc + '\\win32')!
		builder.cmd_exec(os.from_slash(config_cmd))
	}
}

// regenerate
fn (mut builder Builder) build_bdwgc() ! {
	// try build bdwgc for current os
	builder.logger.info('build bdwgc for ${os_detail}')
	work_dir_bdwgc := os.from_slash(builder.work_dir + '/bdwgc')
	os.chdir(work_dir_bdwgc)!
	config_cmd := builder.replace_string(builder.get_doc_value('config_bdwgc')!.string())
	if os_kind == 'nix' {
		if os.exists('./Makefile') {
			builder.cmd_exec('gmake distclean')
		} else {
			builder.cmd_exec('./autogen.sh')
		}
		builder.cmd_exec(config_cmd)
		builder.cmd_exec('gmake')
	} else {
		// windows
		builder.cmd_exec(config_cmd)
		tcc := @VEXEROOT + '/thirdparty/tcc/tcc.exe'
		builder.cmd_exec('${tcc} -ar rcs ${work_dir_bdwgc}/extra/amalgamated_gc.a ${work_dir_bdwgc}/extra/amalgamated_gc.o')
	}
}

fn (mut builder Builder) copy_tcc() ! {
	// copy tcc for current os
	builder.logger.info('copy tcc for ${os_detail}')
	build_dir_tcc := os.from_slash(builder.work_dir + '/tcc/build')

	os.cp_all(build_dir_tcc, @VEXEROOT + '/thirdparty/tcc', true)!
	if os.is_dir(@VEXEROOT + '/thirdparty/tcc/examples') {
		os.rmdir_all(@VEXEROOT + '/thirdparty/tcc/examples')!
	}
	if os.is_dir(@VEXEROOT + '/thirdparty/tcc/include') && os_kind == 'windows' {
		// windows need a new include dir, `windows_copy_extra_files()`
		os.rmdir_all(@VEXEROOT + '/thirdparty/tcc/include')!
	}
	if os.exists(@VEXEROOT + '/thirdparty/tcc/tcc') {
		// nix need to rename `tcc` to `tcc.exe`
		os.rename(@VEXEROOT + '/thirdparty/tcc/tcc', @VEXEROOT + '/thirdparty/tcc/tcc.exe')!
	}
}

fn (mut builder Builder) copy_bdwgc() ! {
	// copy bdwgc for current os
	builder.logger.info('copy bdwgc for ${os_detail}')
	work_dir_bdwgc := os.from_slash(builder.work_dir + '/bdwgc')

	// backup
	os.cp_all(work_dir_bdwgc + '/include', @VEXEROOT + '/thirdparty/libgc/include', true)!

	os.cp(work_dir_bdwgc + '/extra/amalgamated_gc.c', @VEXEROOT + '/thirdparty/libgc/gc.c')!
	if os_kind == 'windows' {
		os.cp(work_dir_bdwgc + '/extra/amalgamated_gc.a', @VEXEROOT + '/thirdparty/tcc/lib/libgc.a')!
	} else {
		os.cp(work_dir_bdwgc + '/.libs/libgc.a', @VEXEROOT + '/thirdparty/tcc/lib/libgc.a')!
	}
}

fn (mut builder Builder) copy_extra_files() ! {
	// try copy extra files for current os
	builder.logger.info('copy extra files for ${os_detail}')

	if list_file := builder.get_doc_value('copy_file_list') {
		for item_array in list_file.array() {
			item := item_array.array()
			src := os.from_slash(builder.pwd + '/files/' + item[0].string())
			dst := os.from_slash(@VEXEROOT + '/thirdparty/' + item[1].string())
			builder.logger.debug('copy [${src}] => [${dst}]')
			if os.is_dir(src) {
				os.cp_all(src, dst, true)!
			} else {
				os.cp(src, dst)!
			}
		}
	}
}

fn (mut builder Builder) scan_all_v_files_for_c_function_names() ! {
	// 1. scan all v files, get all C.xx function's names
	builder.logger.info("scan all .v files, get all C.xx function's names")
	mut all_v_ref_funcs := map[string]bool{}
	all_v_files := os.walk_ext(@VEXEROOT, '.v')
	for v in all_v_files {
		content := os.read_file(v)!
		// fn C.fn_name(args) => fn_name
		mut re := regex.regex_opt(r'\s*fn\s+C\.(\w+)\(')!
		lines := content.split_into_lines()
		for line in lines {
			if re.matches_string(line) {
				start := re.get_group_list()[0].start
				end := re.get_group_list()[0].end
				fn_name := line[start..end]
				all_v_ref_funcs[fn_name] = true
			}
		}
	}
	builder.logger.info("scan all .c/.h/.cpp files, get all function's names")
	mut all_c_files := os.walk_ext(@VEXEROOT, '.c')
	all_c_files << os.walk_ext(@VEXEROOT, '.h')
	all_c_files << os.walk_ext(@VEXEROOT, '.cpp')
	for c_file in all_c_files {
		content := os.read_file(c_file)!
		// fn_name( => fn_name
		mut fn_name := ''
		mut ss := textscanner.new(content)
		for ss.remaining() > 0 {
			c := ss.next()
			if c == -1 {
				break
			} else if c == `(` {
				if fn_name.len > 0 {
					all_v_ref_funcs[fn_name] = true
					fn_name = ''
				}
			} else if !u8(c).is_alnum() && c != `_` {
				ss.skip_whitespace()
				if ss.peek() != `(` {
					fn_name = ''
				}
			} else {
				fn_name += u8(c).ascii_str()
			}
		}
	}
	builder.all_v_ref_funcs = all_v_ref_funcs.keys()
}

fn (mut builder Builder) rebuild_tcc_def_files() ! {
	// 2. use `tcc` generate all system dlls' def file
	builder.logger.info('rebuild def files with new tcc')
	tcc_exe := os.from_slash(builder.work_dir + '/tcc/build/tcc.exe')

	windows_system_dlls := builder.doc.value('windows.system_dlls').array()
	for f_any in windows_system_dlls {
		f := f_any.string()
		dll_file := os.from_slash(builder.win_sys_dir + '\\' + f)
		def_file := os.from_slash(builder.work_dir + '/tcc/build/lib/' + f[..f.len - 3] + 'def')
		if os.exists(dll_file) {
			builder.cmd_exec('${tcc_exe} -impdef ${dll_file} -o ${def_file}')
			lines := os.read_lines(def_file)!
			builder.logger.debug('use tcc generate def file: ${f[..f.len - 3]}def [${lines.len - 3}] functions')
		}
	}
}

fn (mut builder Builder) cross_check_func_name_and_rewrite_def_files() ! {
	// 3. cross-check for same function name
	builder.logger.info('cross-check for used C function names in v files')
	if builder.all_v_ref_funcs.len == 0 {
		return
	}
	windows_system_dlls := builder.doc.value('windows.system_dlls').array()
	for f_any in windows_system_dlls {
		f := f_any.string()
		def_file := os.from_slash(builder.work_dir + '/tcc/build/lib/' + f[..f.len - 3] + 'def')
		if os.exists(def_file) {
			builder.logger.debug('cross check for def file: ${f[..f.len - 3] + 'def'}')
			mut sb := strings.new_builder(8192)
			sb.writeln('LIBRARY ${f}')
			sb.writeln('')
			sb.writeln('EXPORTS')
			lines := os.read_lines(def_file)!
			mut fn_count := 0
			for line in lines {
				l := line.trim_space()
				if l.starts_with('LIBRARY ') || l == '' || l == 'EXPORTS' {
					continue
				}
				if l in builder.all_v_ref_funcs {
					sb.writeln(l)
					fn_count++
				}
			}
			builder.logger.debug('write out [${fn_count}] functions for ${f[..f.len - 3] + 'def'}')
			os.write_file(def_file, sb.str())!
		} else {
			builder.logger.warn('can not read ${def_file}')
		}
	}
}

@[params]
struct CmdExecParam {
	force_show bool
}

// always execute command
fn (mut builder Builder) cmd_exec(cmd string, param CmdExecParam) string {
	builder.logger.debug('cmd: ${cmd}')
	r := os.execute(cmd)
	if r.exit_code < 0 {
		builder.logger.error('"${cmd}" could not start.')
		builder.logger.error(r.output)
		// something went wrong, better start fresh next time
		return ''
	}
	if r.exit_code != 0 {
		builder.logger.error('"${cmd}" failed.')
		builder.logger.error(r.output)

		// something went wrong, better start fresh next time
		return ''
	}
	if param.force_show {
		builder.logger.debug(r.output)
	}
	return r.output
}
