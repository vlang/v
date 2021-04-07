module vgit

import os
import flag
import scripting

pub fn check_v_commit_timestamp_before_self_rebuilding(v_timestamp int) {
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
	if commit.len == 0 {
		return
	}
	cmd := "git cat-file -t '$commit' "
	if !scripting.exit_0_status(cmd) {
		eprintln('Commit: "$commit" does not exist in the current repository.')
		exit(3)
	}
}

pub fn line_to_timestamp_and_commit(line string) (int, string) {
	parts := line.split(' ')
	return parts[0].int(), parts[1]
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

pub fn prepare_vc_source(vcdir string, cdir string, commit string) (string, string) {
	scripting.chdir(cdir)
	// Building a historic v with the latest vc is not always possible ...
	// It is more likely, that the vc *at the time of the v commit*,
	// or slightly before that time will be able to build the historic v:
	vline := scripting.run('git rev-list -n1 --timestamp "$commit" ')
	v_timestamp, v_commithash := line_to_timestamp_and_commit(vline)
	scripting.verbose_trace(@FN, 'v_timestamp: $v_timestamp | v_commithash: $v_commithash')
	check_v_commit_timestamp_before_self_rebuilding(v_timestamp)
	scripting.chdir(vcdir)
	scripting.run('git checkout --quiet master')
	//
	mut vccommit := ''
	vcbefore_subject_match := scripting.run('git rev-list HEAD -n1 --timestamp --grep=${v_commithash[0..7]} ')
	scripting.verbose_trace(@FN, 'vcbefore_subject_match: $vcbefore_subject_match')
	if vcbefore_subject_match.len > 3 {
		_, vccommit = line_to_timestamp_and_commit(vcbefore_subject_match)
	} else {
		scripting.verbose_trace(@FN, 'the v commit did not match anything in the vc log; try --timestamp instead.')
		vcbefore := scripting.run('git rev-list HEAD -n1 --timestamp --before=$v_timestamp ')
		_, vccommit = line_to_timestamp_and_commit(vcbefore)
	}
	scripting.verbose_trace(@FN, 'vccommit: $vccommit')
	scripting.run('git checkout --quiet "$vccommit" ')
	scripting.run('wc *.c')
	scripting.chdir(cdir)
	return v_commithash, vccommit
}

pub fn clone_or_pull(remote_git_url string, local_worktree_path string) {
	// NB: after clone_or_pull, the current repo branch is === HEAD === master
	if os.is_dir(local_worktree_path) && os.is_dir(os.join_path(local_worktree_path, '.git')) {
		// Already existing ... Just pulling in this case is faster usually.
		scripting.run('git -C "$local_worktree_path"  checkout --quiet master')
		scripting.run('git -C "$local_worktree_path"  pull     --quiet ')
	} else {
		// Clone a fresh
		scripting.run('git clone --quiet "$remote_git_url"  "$local_worktree_path" ')
	}
}

pub struct VGitContext {
pub:
	cc          string = 'cc' // what compiler to use
	workdir     string = '/tmp' // the base working folder
	commit_v    string = 'master' // the commit-ish that needs to be prepared
	path_v      string // where is the local working copy v repo
	path_vc     string // where is the local working copy vc repo
	v_repo_url  string // the remote v repo URL
	vc_repo_url string // the remote vc repo URL
pub mut:
	// these will be filled by vgitcontext.compile_oldv_if_needed()
	commit_v__hash string // the git commit of the v repo that should be prepared
	commit_vc_hash string // the git commit of the vc repo, corresponding to commit_v__hash
	vexename       string // v or v.exe
	vexepath       string // the full absolute path to the prepared v/v.exe
	vvlocation     string // v.v or compiler/ or cmd/v, depending on v version
}

pub fn (mut vgit_context VGitContext) compile_oldv_if_needed() {
	vgit_context.vexename = if os.user_os() == 'windows' { 'v.exe' } else { 'v' }
	vgit_context.vexepath = os.real_path(os.join_path(vgit_context.path_v, vgit_context.vexename))
	mut command_for_building_v_from_c_source := ''
	mut command_for_selfbuilding := ''
	if 'windows' == os.user_os() {
		command_for_building_v_from_c_source = '$vgit_context.cc -std=c99 -municode -w -o cv.exe  "$vgit_context.path_vc/v_win.c" '
		command_for_selfbuilding = './cv.exe -o $vgit_context.vexename {SOURCE}'
	} else {
		command_for_building_v_from_c_source = '$vgit_context.cc -std=gnu11 -w -o cv "$vgit_context.path_vc/v.c"  -lm -lpthread'
		command_for_selfbuilding = './cv -o $vgit_context.vexename {SOURCE}'
	}
	scripting.chdir(vgit_context.workdir)
	clone_or_pull(vgit_context.v_repo_url, vgit_context.path_v)
	clone_or_pull(vgit_context.vc_repo_url, vgit_context.path_vc)
	scripting.chdir(vgit_context.path_v)
	scripting.run('git checkout --quiet $vgit_context.commit_v')
	if os.is_dir(vgit_context.path_v) && os.exists(vgit_context.vexepath) {
		// already compiled, so no need to compile v again
		vgit_context.commit_v__hash = get_current_folder_commit_hash()
		return
	}
	v_commithash, vccommit_before := prepare_vc_source(vgit_context.path_vc, vgit_context.path_v,
		'HEAD')
	vgit_context.commit_v__hash = v_commithash
	vgit_context.commit_vc_hash = vccommit_before
	if os.exists('cmd/v') {
		vgit_context.vvlocation = 'cmd/v'
	} else {
		vgit_context.vvlocation = if os.exists('v.v') { 'v.v' } else { 'compiler' }
	}
	if os.is_dir(vgit_context.path_v) && os.exists(vgit_context.vexepath) {
		// already compiled, so no need to compile v again
		return
	}
	// Recompilation is needed. Just to be sure, clean up everything first.
	scripting.run('git clean -xf')
	scripting.run(command_for_building_v_from_c_source)
	build_cmd := command_for_selfbuilding.replace('{SOURCE}', vgit_context.vvlocation)
	scripting.run(build_cmd)
	// At this point, there exists a file vgit_context.vexepath
	// which should be a valid working V executable.
}

pub struct VGitOptions {
pub mut:
	workdir     string // the working folder (typically /tmp), where the tool will write
	v_repo_url  string // the url of the V repository. It can be a local folder path, if you want to eliminate network operations...
	vc_repo_url string // the url of the vc repository. It can be a local folder path, if you want to eliminate network operations...
	show_help   bool   // whether to show the usage screen
	verbose     bool   // should the tool be much more verbose
}

pub fn add_common_tool_options(mut context VGitOptions, mut fp flag.FlagParser) []string {
	tdir := os.temp_dir()
	context.workdir = os.real_path(fp.string('workdir', `w`, context.workdir, 'A writable base folder. Default: $tdir'))
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
		eprintln('Error: $err')
		exit(1)
	}
	for commit in commits {
		validate_commit_exists(commit)
	}
	return commits
}
