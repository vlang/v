module vgit

import os
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
	cmd := "git cat-file -t \'$commit\' "
	if !scripting.exit_0_status(cmd) {
		eprintln('Commit: "$commit" does not exist in the current repository.')
		exit(3)
	}
}

pub fn line_to_timestamp_and_commit(line string) (int,string) {
	parts := line.split(' ')
	return parts[0].int(),parts[1]
}

pub fn normalized_workpath_for_commit(workdir string, commit string) string {
	nc := 'v_at_' + commit.replace('^', '_').replace('-', '_').replace('/', '_')
	return os.realpath(workdir + os.path_separator + nc)
}

pub fn prepare_vc_source(vcdir string, cdir string, commit string) (string,string) {
	scripting.chdir(cdir)
	// Building a historic v with the latest vc is not always possible ...
	// It is more likely, that the vc *at the time of the v commit*,
	// or slightly before that time will be able to build the historic v:
	vline := scripting.run('git rev-list -n1 --timestamp "$commit" ')
	v_timestamp,v_commithash := vgit.line_to_timestamp_and_commit(vline)
	vgit.check_v_commit_timestamp_before_self_rebuilding(v_timestamp)
	scripting.chdir(vcdir)
	scripting.run('git checkout master')
	vcbefore := scripting.run('git rev-list HEAD -n1 --timestamp --before=$v_timestamp ')
	_,vccommit_before := vgit.line_to_timestamp_and_commit(vcbefore)
	scripting.run('git checkout "$vccommit_before" ')
	scripting.run('wc *.c')
	scripting.chdir(cdir)
	return v_commithash,vccommit_before
}
