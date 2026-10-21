module bin

import os
import crypto.sha256

const vc_lock_v_repository = 'https://github.com/vlang/v'
const vc_lock_message_prefix = '[v:master] '

// VcSnapshotProvenance names one VC snapshot and the vlang/v master commit it was generated from.
pub struct VcSnapshotProvenance {
pub:
	vc_commit string
	v_commit  string
	title     string
}

// VcBootstrapLockUpdate is the vc.lock rendered for one verified VC snapshot, next to the lock it
// would replace. Building one never writes the lock.
pub struct VcBootstrapLockUpdate {
pub:
	provenance VcSnapshotProvenance
	current    string
	next       string
	diff       string
}

// changed reports whether the rendered lock differs from the current one.
pub fn (update VcBootstrapLockUpdate) changed() bool {
	return update.current != update.next
}

struct VcSnapshotFile {
	artifact   VcBootstrapArtifact
	first_line string
}

fn vc_lock_git(git_dir string, args []string) !os.Result {
	mut command := ['git', '--no-replace-objects', '--git-dir', git_dir, '-c', 'core.autocrlf=false',
		'-c', 'core.hooksPath=${os.path_devnull}']
	command << args
	return candidate_git(command)!
}

fn vc_lock_git_output(git_dir string, args []string, failure string) !string {
	result := vc_lock_git(git_dir, args)!
	if result.exit_code != 0 {
		return error(failure)
	}
	return result.output
}

fn vc_lock_require_commit(git_dir string, oid string, failure string) ! {
	object_type := vc_lock_git(git_dir, ['cat-file', '-t', oid])!
	if object_type.exit_code != 0 || object_type.output.trim_space() != 'commit' {
		return error(failure)
	}
}

// parse_vc_snapshot_message returns the vlang/v commit and title that a raw VC commit names in its
// one-line message, `[v:master] <sha> - <title>`.
fn parse_vc_snapshot_message(raw_commit string) !(string, string) {
	header_end := raw_commit.index('\n\n') or { return error('VC snapshot commit has no message') }
	message := raw_commit[header_end + 2..]
	if !message.starts_with(vc_lock_message_prefix) || !message.ends_with('\n')
		|| message.count('\n') != 1 || message.contains('\r') {
		return error('VC snapshot message is not `[v:master] <sha> - <title>`')
	}
	line := message[vc_lock_message_prefix.len..message.len - 1]
	if line.len < 44 || !is_lower_hex_40(line[..40]) || line[40..43] != ' - ' {
		return error('VC snapshot message is not `[v:master] <sha> - <title>`')
	}
	title := line[43..]
	if title == '' || title.trim_space() != title {
		return error('VC snapshot message has no exact title')
	}
	return line[..40], title
}

fn read_vc_snapshot_file(vc_git_dir string, commit string, path string) !VcSnapshotFile {
	listing := vc_lock_git_output(vc_git_dir, ['ls-tree', '-z', '--full-tree', commit, '--', path],
		'VC snapshot tree cannot be listed')!
	records := listing.split('\x00')
	if records.len != 2 || records[1] != '' {
		return error('VC snapshot must contain exactly one ${path}')
	}
	parts := records[0].split_nth('\t', 2)
	metadata := if parts.len == 2 { parts[0].split(' ') } else { []string{} }
	if parts.len != 2 || parts[1] != path || metadata.len != 3 || metadata[1] != 'blob' {
		return error('VC snapshot ${path} is not a blob')
	}
	content := vc_lock_git_output(vc_git_dir, ['cat-file', 'blob', metadata[2]],
		'VC snapshot ${path} cannot be read')!
	bytes := content.bytes()
	// The captured output is trusted only if it hashes back to the listed blob.
	if git_blob_oid(bytes) != metadata[2] {
		return error('VC snapshot ${path} bytes do not match their Git blob')
	}
	return VcSnapshotFile{
		artifact:   parse_vc_bootstrap_artifact('${metadata[0]} ${metadata[2]} ${bytes.len} ${sha256.sum256(bytes).hex()}',
			path)!
		first_line: content.all_before('\n')
	}
}

// plan_vc_bootstrap_lock_update renders the vc.lock for VC commit `vc_commit` and diffs it against
// `current_lock`, after checking the snapshot's provenance by content: its message is
// `[v:master] <sha> - <title>`, `<sha>` is an ancestor of `v_master` in the vlang/v Git directory
// `v_git_dir`, and the first line of both `v.c` and `v_win.c` defines V_COMMIT_HASH as `<sha>`.
// Both Git directories must already hold the objects; nothing is fetched and nothing is written.
pub fn plan_vc_bootstrap_lock_update(current_lock string, vc_git_dir string, vc_commit string,
	v_git_dir string, v_master string) !VcBootstrapLockUpdate {
	parse_vc_bootstrap_lock(current_lock)!
	if !is_lower_hex_40(vc_commit) || !is_lower_hex_40(v_master) {
		return error('VC snapshot and vlang/v master must be full lowercase commit IDs')
	}
	vc_lock_require_commit(vc_git_dir, vc_commit, 'VC snapshot commit is not present')!
	raw_commit := vc_lock_git_output(vc_git_dir, ['cat-file', 'commit', vc_commit],
		'VC snapshot commit cannot be read')!
	v_commit, title := parse_vc_snapshot_message(raw_commit)!
	vc_lock_require_commit(v_git_dir, v_master, 'vlang/v master is not present')!
	vc_lock_require_commit(v_git_dir, v_commit,
		'VC snapshot source ${v_commit} is not in the vlang/v history')!
	ancestry := vc_lock_git(v_git_dir, ['merge-base', '--is-ancestor', v_commit, v_master])!
	if ancestry.exit_code == 1 {
		return error('VC snapshot source ${v_commit} is not an ancestor of vlang/v master ${v_master}')
	}
	if ancestry.exit_code != 0 {
		return error('vlang/v ancestry cannot be determined')
	}
	tree := vc_lock_git_output(vc_git_dir, ['rev-parse', '--verify', '${vc_commit}^{tree}'],
		'VC snapshot tree cannot be resolved')!.trim_space()
	v_c := read_vc_snapshot_file(vc_git_dir, vc_commit, 'v.c')!
	v_win_c := read_vc_snapshot_file(vc_git_dir, vc_commit, 'v_win.c')!
	expected_first_line := '#define V_COMMIT_HASH "${v_commit}"'
	if v_c.first_line != expected_first_line {
		return error('VC snapshot v.c does not define V_COMMIT_HASH as ${v_commit} on its first line')
	}
	if v_win_c.first_line != expected_first_line {
		return error('VC snapshot v_win.c does not define V_COMMIT_HASH as ${v_commit} on its first line')
	}
	next := render_vc_bootstrap_lock(VcBootstrapLock{
		repository: vc_bootstrap_repository
		commit:     vc_commit
		tree:       tree
		v_c:        v_c.artifact
		v_win_c:    v_win_c.artifact
	})!
	return VcBootstrapLockUpdate{
		provenance: VcSnapshotProvenance{
			vc_commit: vc_commit
			v_commit:  v_commit
			title:     title
		}
		current:    current_lock
		next:       next
		diff:       diff_vc_bootstrap_lock(current_lock, next)!
	}
}

// diff_vc_bootstrap_lock returns a unified diff that turns the `current` vc.lock into `next`, or an
// empty string when they are equal. Both must be canonical locks, so the diff is a single hunk over
// the six records that `git apply` accepts at the repository root.
pub fn diff_vc_bootstrap_lock(current string, next string) !string {
	parse_vc_bootstrap_lock(current)!
	parse_vc_bootstrap_lock(next)!
	if current == next {
		return ''
	}
	old_lines := current[..current.len - 1].split('\n')
	new_lines := next[..next.len - 1].split('\n')
	path := 'thirdparty/tccbin_automation/${vc_bootstrap_lock_relative}'
	mut diff := ['--- a/${path}', '+++ b/${path}', '@@ -1,6 +1,6 @@']
	mut start := 0
	for start < old_lines.len {
		if old_lines[start] == new_lines[start] {
			diff << ' ${old_lines[start]}'
			start++
			continue
		}
		mut end := start
		for end < old_lines.len && old_lines[end] != new_lines[end] {
			end++
		}
		for line in old_lines[start..end] {
			diff << '-${line}'
		}
		for line in new_lines[start..end] {
			diff << '+${line}'
		}
		start = end
	}
	return diff.join('\n') + '\n'
}

fn fetch_vc_lock_source(git_dir string, repository string, fetch_options []string) ! {
	init := candidate_git(['git', '-c', 'init.defaultBranch=master', 'init', '--quiet', '--bare',
		git_dir])!
	if init.exit_code != 0 {
		return error('scratch repository for ${repository} cannot be created')
	}
	vc_lock_git_output(git_dir, ['remote', 'add', 'origin', repository],
		'scratch remote for ${repository} cannot be configured')!
	mut fetch := ['fetch', '--quiet', '--no-tags']
	fetch << fetch_options
	fetch << ['origin', '+refs/heads/master:refs/remotes/origin/master']
	vc_lock_git_output(git_dir, fetch, 'master of ${repository} cannot be fetched')!
}

// run_vc_lock_dry_run fetches the vlang/vc master tip and the vlang/v master commit history into a
// fresh `work_root`, then plans the vc.lock update for that snapshot against the lock under
// `automation_root` with plan_vc_bootstrap_lock_update. It removes `work_root` before returning,
// and it never writes vc.lock, commits, or pushes.
pub fn run_vc_lock_dry_run(automation_root string, work_root string) !VcBootstrapLockUpdate {
	validate_vc_bootstrap_contract(automation_root)!
	current_lock := os.read_file(os.join_path(automation_root, vc_bootstrap_lock_relative))!
	if os.exists(work_root) {
		return error('VC lock dry-run work root must not exist yet')
	}
	os.mkdir_all(work_root)!
	defer {
		os.rmdir_all(work_root) or {}
	}
	vc_git_dir := os.join_path(work_root, 'vc.git')
	v_git_dir := os.join_path(work_root, 'v.git')
	// vc goes first: the vlang/v commit a snapshot names was on master before the snapshot was
	// pushed, so the later vlang/v fetch contains it unless master was rewritten in between.
	fetch_vc_lock_source(vc_git_dir, vc_bootstrap_repository, ['--depth=1'])!
	// Ancestry needs only commits, so trees and blobs stay on the server.
	fetch_vc_lock_source(v_git_dir, vc_lock_v_repository, ['--filter=tree:0'])!
	vc_commit := vc_lock_git_output(vc_git_dir, ['rev-parse', '--verify',
		'refs/remotes/origin/master^{commit}'], 'vlang/vc master cannot be resolved')!.trim_space()
	v_master := vc_lock_git_output(v_git_dir, ['rev-parse', '--verify',
		'refs/remotes/origin/master^{commit}'], 'vlang/v master cannot be resolved')!.trim_space()
	return plan_vc_bootstrap_lock_update(current_lock, vc_git_dir, vc_commit, v_git_dir, v_master)!
}
