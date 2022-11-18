#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp

import os
import term

// This script can be used to ensure that all commited V files are vfmt-ed automatically.
// By default, once setup, it will run `v fmt -w` on them, before commiting them.

// To use the script in your V project, you need to be in the main folder
// of your project, then do the equivalent of:
// ```sh
// cp /PATH/TO_YOUR/V/cmd/tools/git_pre_commit_hook.vsh .git/hooks/pre-commit
// chmod 755 .git/hooks/pre-commit
// ```
//
// Note: you can use this command:
// `git config --bool --add hooks.stopCommitOfNonVfmtedVFiles true`
// ... to make it just *prevent* the commiting of unformatted .v files,
// i.e. stop the commiting, if they are not, but *without modifying them*
// automatically (you will then need to run `v fmt -w` on them manually).
//
// Note 2: Git supports skipping the hooks, by passing the `--no-verify` option.
// That can be used to commit some .v files that are not formatted, without removing
// the hook.

fn main() {
	// This hook cares only about the changed V files, that will be commited, as reported by git itself:
	changed := os.execute('git diff --cached --name-only --diff-filter=ACMR -- "*.v" "*.vsh" "*.vv"')
	if changed.output == '' {
		eprintln('>>> 0 changed V files found.')
		exit(0)
	}
	vfiles := changed.output.trim_space().split('\n')
	configured_stop_commiting := os.execute('git config --bool hooks.stopCommitOfNonVfmtedVFiles')
	if configured_stop_commiting.output.trim_space().bool() {
		verify_result := os.execute('v fmt -verify ${vfiles.join(' ')}')
		if verify_result.exit_code != 0 {
			eprintln(verify_result.output)
		}
		exit(verify_result.exit_code)
	} else {
		eprintln('The V pre commit hook will format ${vfiles.len} V file(s):')
		// vfmt off
		for vfile in vfiles {
			eprintln('    ${term.bold('$vfile')}')
		}
		// vfmt on
		all_vfiles_on_a_line := vfiles.map(os.quoted_path(it)).join(' ')
		os.system('v fmt -w ${all_vfiles_on_a_line}')
		os.system('git add ${all_vfiles_on_a_line}')
	}
}
