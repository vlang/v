module vgit

import os
import flag
import scripting

pub fn check_v_commit_timestamp_before_self_rebuilding(v_timestamp u64) {
	if v_timestamp >= 1561805697 {
		return
	}
	eprintln('##################################################################')
	eprintln('# WARNING: v self rebuilding, before 5b7a1e8 (2019-06-29 12:21)  #')
	eprintln('#          required the v executable to be built *inside*        #')
	eprintln('#          the toplevel compiler/ folder.                        #')
	eprintln('#                                                                #')
	eprintln('#          That is not supported by this tool.                   #')
	eprintln('#          You will have to build it manually there.             #')
	eprintln('##################################################################')
}

pub fn validate_commit_exists(commit string) {
	if commit != '' {
		return
	}
	cmd := 'git cat-file -t "${commit}" ' // windows's cmd.exe does not support ' for quoting
	if !scripting.exit_0_status(cmd) {
		eprintln('Commit: "${commit}" does not exist in the current repository.')
		exit(3)
	}
}

pub fn line_to_timestamp_and_commit(line string) (u64, string) {
	parts := line.split(' ')
	return parts[0].u64(), parts[1]
}

pub fn normalized_workpath_for_commit(workdir string, commit string) string {
	nc := 'v_at_' + commit.replace('^', '_').replace('-', '_').replace('/', '_')
	return os.real_path(workdir + os.path_separator + nc)
}

fn get_current_folder_commit_hash() string {
	vline := scripting.run('git rev-list -n1 --timestamp HEAD')
	_, v_commithash := line_to_timestamp_and_commit(vline)
	return v_commithash
}

@[noreturn]
fn fatal_error(error IError, label string) {
	eprintln('error: ${label}')
	eprintln(error)
	exit(1)
}

@[noreturn]
fn co_fail(error IError, commit string) {
	fatal_error(error, 'git could not checkout `${commit}`')
}

@[noreturn]
fn net_fail(error IError, what string) {
	fatal_error(error, 'git failed at `${what}`')
}

pub fn prepare_vc_source(vcdir string, cdir string, commit string) (string, string, u64) {
	scripting.chdir(cdir)
	// Building a historic v with the latest vc is not always possible ...
	// It is more likely, that the vc *at the time of the v commit*,
	// or slightly before that time will be able to build the historic v:
	vline := scripting.run('git rev-list -n1 --timestamp "${commit}" ')
	v_timestamp, v_commithash := line_to_timestamp_and_commit(vline)
	scripting.verbose_trace(@FN, 'v_timestamp: ${v_timestamp} | v_commithash: ${v_commithash}')
	check_v_commit_timestamp_before_self_rebuilding(v_timestamp)
	scripting.chdir(vcdir)
	scripting.frun('git checkout --quiet master') or { co_fail(err, 'master') }

	mut vccommit := ''
	mut partial_hash := v_commithash[0..7]
	if '5b7a1e8'.starts_with(partial_hash) {
		// we need the following, otherwise --grep= below would find a93ef6e, which does include 5b7a1e8 in the commit message ... 🤦‍♂️
		partial_hash = '5b7a1e84a4d283071d12cb86dc17aeda9b5306a8'
	}
	vcbefore_subject_match := scripting.run('git rev-list HEAD -n1 --timestamp --grep=${partial_hash} ')
	scripting.verbose_trace(@FN, 'vcbefore_subject_match: ${vcbefore_subject_match}')
	if vcbefore_subject_match.len > 3 {
		_, vccommit = line_to_timestamp_and_commit(vcbefore_subject_match)
	} else {
		scripting.verbose_trace(@FN, 'the v commit did not match anything in the vc log; try --timestamp instead.')
		vcbefore := scripting.run('git rev-list HEAD -n1 --timestamp --before=${v_timestamp} ')
		_, vccommit = line_to_timestamp_and_commit(vcbefore)
	}
	scripting.verbose_trace(@FN, 'vccommit: ${vccommit}')
	scripting.frun('git checkout --quiet "${vccommit}" ') or { co_fail(err, vccommit) }
	scripting.run('wc *.c')
	scripting.chdir(cdir)
	return v_commithash, vccommit, v_timestamp
}

pub fn clone_or_pull(remote_git_url string, local_worktree_path string) {
	// Note: after clone_or_pull, the current repo branch is === HEAD === master
	if os.is_dir(local_worktree_path) && os.is_dir(os.join_path_single(local_worktree_path, '.git')) {
		// Already existing ... Just pulling in this case is faster usually.
		scripting.frun('git -C "${local_worktree_path}" checkout --quiet master') or {
			co_fail(err, 'master')
		}
		scripting.frun('git -C "${local_worktree_path}" pull --quiet ') or {
			net_fail(err, 'pulling')
		}
	} else {
		// Clone a fresh local tree.
		if remote_git_url.starts_with('http') {
			// cloning an https remote with --filter=blob:none is usually much less bandwidth intensive, at the
			// expense of doing small network ops later when using checkouts.
			scripting.frun('git clone --filter=blob:none --quiet "${remote_git_url}" "${local_worktree_path}" ') or {
				net_fail(err, 'cloning')
			}
			return
		}
		mut is_blobless_clone := false
		remote_git_config_path := os.join_path(remote_git_url, '.git', 'config')
		if os.is_dir(remote_git_url) && os.is_file(remote_git_config_path) {
			lines := os.read_lines(remote_git_config_path) or { [] }
			is_blobless_clone = lines.any(it.contains('partialclonefilter = blob:none'))
		}
		if is_blobless_clone {
			// Note:
			// 1) cloning a *local folder* with `--filter=blob:none`, that *itself* was cloned with `--filter=blob:none`
			// leads to *extremely* slow checkouts for older commits later. It takes hours instead of milliseconds, for a commit
			// that is just several thousands of commits old :( .
			//
			// 2) Cloning it *without* the `--filter=blob:none`, leads to `error: unable to read sha1 file of`, later,
			// when checking out the older commits, depending on the local git client version (tested with git version 2.41.0).
			//
			// 3) => instead of cloning, it is much faster, and *bug free*, to just rsync the local repo directly,
			// at the expense of a little more space usage, which will make the new tree in local_worktree_path,
			// exactly 1:1 the same, as the one in remote_git_url, just independent from it .
			copy_cmd := if os.user_os() == 'windows' { 'robocopy /MIR' } else { 'rsync -a' }
			scripting.frun('${copy_cmd} "${remote_git_url}/" "${local_worktree_path}/"') or {
				fatal_error(err, 'copying to ${local_worktree_path}')
			}
			return
		}
		scripting.frun('git clone --quiet "${remote_git_url}"  "${local_worktree_path}" ') or {
			net_fail(err, 'cloning')
		}
	}
}

pub struct VGitContext {
pub:
	cc          string = 'cc' // what C compiler to use for bootstrapping
	cc_options  string // what additional C compiler options to use for bootstrapping
	workdir     string = '/tmp'   // the base working folder
	commit_v    string = 'master' // the commit-ish that needs to be prepared
	path_v      string // where is the local working copy v repo
	path_vc     string // where is the local working copy vc repo
	v_repo_url  string // the remote v repo URL
	vc_repo_url string // the remote vc repo URL
pub mut:
	// these will be filled by vgitcontext.compile_oldv_if_needed()
	commit_v__hash string // the git commit of the v repo that should be prepared
	commit_vc_hash string // the git commit of the vc repo, corresponding to commit_v__hash
	commit_v__ts   u64    // unix timestamp, that corresponds to commit_v__hash; filled by prepare_vc_source
	vexename       string // v or v.exe
	vexepath       string // the full absolute path to the prepared v/v.exe
	vvlocation     string // v.v or compiler/ or cmd/v, depending on v version
	make_fresh_tcc bool   // whether to do 'make fresh_tcc' before compiling an old V.
	show_vccommit  bool   // show the V and VC commits, corresponding to the V commit-ish, that can be used to build V
}

pub fn (mut vgit_context VGitContext) compile_oldv_if_needed() {
	vgit_context.vexename = if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	vgit_context.vexepath = os.real_path(os.join_path_single(vgit_context.path_v, vgit_context.vexename))
	if os.is_dir(vgit_context.path_v) && os.is_executable(vgit_context.vexepath)
		&& !vgit_context.show_vccommit {
		// already compiled, no need to compile that specific v executable again
		vgit_context.commit_v__hash = get_current_folder_commit_hash()
		return
	}
	scripting.chdir(vgit_context.workdir)
	clone_or_pull(vgit_context.v_repo_url, vgit_context.path_v)
	clone_or_pull(vgit_context.vc_repo_url, vgit_context.path_vc)
	scripting.chdir(vgit_context.path_v)
	scripting.frun('git checkout --quiet ${vgit_context.commit_v}') or {
		co_fail(err, vgit_context.commit_v)
	}
	if os.is_dir(vgit_context.path_v) && os.exists(vgit_context.vexepath)
		&& !vgit_context.show_vccommit {
		// already compiled, so no need to compile v again
		vgit_context.commit_v__hash = get_current_folder_commit_hash()
		return
	}
	v_commithash, vccommit_before, v_timestamp := prepare_vc_source(vgit_context.path_vc,
		vgit_context.path_v, 'HEAD')
	vgit_context.commit_v__hash = v_commithash
	vgit_context.commit_v__ts = v_timestamp
	vgit_context.commit_vc_hash = vccommit_before

	if vgit_context.show_vccommit {
		println('VHASH=${vgit_context.commit_v__hash}')
		println('VCHASH=${vgit_context.commit_vc_hash}')
		exit(0)
	}

	if os.exists('cmd/v') {
		vgit_context.vvlocation = 'cmd/v'
	} else {
		vgit_context.vvlocation = if os.exists('v.v') { 'v.v' } else { 'compiler' }
	}
	if os.is_dir(vgit_context.path_v) && os.exists(vgit_context.vexepath) {
		// already compiled, so no need to compile v again
		return
	}

	scripting.chdir(vgit_context.path_v)
	// Recompilation is needed. Just to be sure, clean up everything first.
	scripting.run('git clean -xf')
	if vgit_context.make_fresh_tcc {
		scripting.run('make fresh_tcc')
	}

	// compiling the C sources with a C compiler:
	mut command_for_building_v_from_c_source := ''
	mut command_for_selfbuilding := ''
	mut c_flags := '-std=gnu11 -I ./thirdparty/stdatomic/nix -w'
	mut c_ldflags := '-lm -lpthread'
	mut vc_source_file_location := os.join_path_single(vgit_context.path_vc, 'v.c')
	mut vc_v_cpermissive_flags := '${vgit_context.cc_options} -Wno-error=incompatible-pointer-types -Wno-error=implicit-function-declaration -Wno-error=int-conversion -fpermissive'
	// after 85b58b0 2021-09-28, -no-parallel is supported, and can be used to force the cgen stage to be single threaded, which increases the chances of successful bootstraps
	mut vc_v_bootstrap_flags := ''
	if vgit_context.commit_v__ts >= 1632778086 {
		vc_v_bootstrap_flags += ' -no-parallel'
	}
	vc_v_bootstrap_flags = vc_v_bootstrap_flags.trim_space()
	scripting.verbose_trace(@FN, 'vc_v_bootstrap_flags: ${vc_v_bootstrap_flags}')
	scripting.verbose_trace(@FN, 'vc_v_cpermissive_flags: ${vc_v_cpermissive_flags}')
	scripting.verbose_trace(@FN, 'vgit_context.commit_v__ts: ${vgit_context.commit_v__ts}')

	if 'windows' == os.user_os() {
		c_flags = '-std=c99 -I ./thirdparty/stdatomic/win -w'
		c_ldflags = ''
		v_win_c_location := os.join_path_single(vgit_context.path_vc, 'v_win.c')
		if os.exists(v_win_c_location) {
			vc_source_file_location = v_win_c_location
		}
	}
	if 'windows' == os.user_os() {
		if vgit_context.commit_v__ts >= 1589793086 && vgit_context.cc.contains('gcc') {
			// after 53ffee1 2020-05-18, gcc builds on windows do need `-municode`
			c_flags += '-municode'
		}
		// after 2023-11-07, windows builds need linking to ws2_32:
		if vgit_context.commit_v__ts >= 1699341818 && !vgit_context.cc.contains('msvc') {
			c_flags += '-lws2_32'
		}
		command_for_building_v_from_c_source = c(vgit_context.cc, '${vc_v_cpermissive_flags} ${c_flags} -o cv.exe "${vc_source_file_location}" ${c_ldflags}')
		command_for_selfbuilding = c('.\\cv.exe', '${vc_v_bootstrap_flags} -cflags "${vc_v_cpermissive_flags}" -o ${vgit_context.vexename} {SOURCE}')
	} else {
		command_for_building_v_from_c_source = c(vgit_context.cc, '${vc_v_cpermissive_flags} ${c_flags} -o cv "${vc_source_file_location}" ${c_ldflags}')
		command_for_selfbuilding = c('./cv', '${vc_v_bootstrap_flags} -cflags "${vc_v_cpermissive_flags}" -o ${vgit_context.vexename} {SOURCE}')
	}

	scripting.run(command_for_building_v_from_c_source)
	build_cmd := command_for_selfbuilding.replace('{SOURCE}', vgit_context.vvlocation)
	scripting.run(build_cmd)
	// At this point, there exists a file vgit_context.vexepath
	// which should be a valid working V executable.
}

fn c(cmd string, params string) string {
	// compose a command, while reducing the potential whitespaces, due to all the interpolations of optional flags above
	return '${cmd} ${params.trim_space()}'
}

pub struct VGitOptions {
pub mut:
	workdir     string = os.temp_dir() // the working folder (typically /tmp), where the tool will write
	v_repo_url  string // the url of the V repository. It can be a local folder path, if you want to eliminate network operations...
	vc_repo_url string // the url of the vc repository. It can be a local folder path, if you want to eliminate network operations...
	show_help   bool   // whether to show the usage screen
	verbose     bool   // should the tool be much more verbose
}

pub fn add_common_tool_options(mut context VGitOptions, mut fp flag.FlagParser) []string {
	context.workdir = os.real_path(fp.string('workdir', `w`, context.workdir, 'A writable base folder. Default: ${context.workdir}'))
	context.v_repo_url = fp.string('vrepo', 0, context.v_repo_url, 'The url of the V repository. You can clone it locally too. See also --vcrepo below.')
	context.vc_repo_url = fp.string('vcrepo', 0, context.vc_repo_url, 'The url of the vc repository. You can clone it
${flag.space}beforehand, and then just give the local folder
${flag.space}path here. That will eliminate the network ops
${flag.space}done by this tool, which is useful, if you want
${flag.space}to script it/run it in a restrictive vps/docker.
')
	context.show_help = fp.bool('help', `h`, false, 'Show this help screen.')
	context.verbose = fp.bool('verbose', `v`, false, 'Be more verbose.')
	if context.show_help {
		println(fp.usage())
		exit(0)
	}
	if context.verbose {
		scripting.set_verbose(true)
	}
	if os.is_dir(context.v_repo_url) {
		context.v_repo_url = os.real_path(context.v_repo_url)
	}
	if os.is_dir(context.vc_repo_url) {
		context.vc_repo_url = os.real_path(context.vc_repo_url)
	}
	commits := fp.finalize() or {
		eprintln('Error: ${err}')
		exit(1)
	}
	for commit in commits {
		validate_commit_exists(commit)
	}
	return commits
}
