module main

import os
import time

const bot_name = 'vlang-bot'
const bot_email = 'vlang-bot@users.noreply.github.com'
const snapshot_message_prefix = '[v:master] '
const master_refspec = '+refs/heads/master:refs/remotes/origin/master'
const origin_master = 'refs/remotes/origin/master^{commit}'

enum Mode {
	dry_run
	push
}

fn (m Mode) str() string {
	return match m {
		.dry_run { 'dry-run' }
		.push { 'push' }
	}
}

enum Outcome {
	none
	published     // a new snapshot was pushed to vlang/vc
	would_publish // dry-run: a new snapshot was committed locally and not pushed
	up_to_date    // vlang/vc already holds the snapshot of vlang/v master
	verified      // dry-run: identical to the snapshot vlang/vc holds for the same commit
	differs       // dry-run: different from the snapshot vlang/vc holds for the same commit
	ignored       // only paths that cannot change the snapshot changed
	stale         // vlang/v master moved while generating, so nothing was published
	unchanged     // neither repository moved since the last run
	failed
}

struct Report {
	outcome   Outcome
	v_commit  string
	vc_commit string
	message   string
	started   time.Time
	finished  time.Time
	// key names the vlang/v and vlang/vc commits a run started from.
	key string
}

// path_is_ignored reports whether a change to `path` cannot change the snapshot. It follows the
// paths-ignore list of gen_vc_ci.yml. vc.lock and the copies of its sha are listed so that a lock
// update can never cause a new snapshot, which would leave the lock stale again.
fn path_is_ignored(path string) bool {
	if path in ['thirdparty/tccbin_automation/bootstrap/vc.lock',
		'thirdparty/tccbin_automation/tests/workflow_contract_test.v'] {
		return true
	}
	for ext in ['.vv', '.out', '.md', '.yml'] {
		if path.ends_with(ext) {
			return true
		}
	}
	return path.starts_with('cmd/tools/')
		&& !(path.starts_with('cmd/tools/builders/') && path.ends_with('.v'))
}

// parse_snapshot_source returns the vlang/v commit that a vlang/vc commit message
// `[v:master] <sha> - <title>` names, or '' for any other message.
fn parse_snapshot_source(message string) string {
	if !message.starts_with(snapshot_message_prefix) {
		return ''
	}
	rest := message[snapshot_message_prefix.len..]
	if rest.len < 43 || rest[40..43] != ' - ' {
		return ''
	}
	sha := rest[..40]
	return if sha.bytes().all((it >= `0` && it <= `9`) || (it >= `a` && it <= `f`)) {
		sha
	} else {
		''
	}
}

// available_memory_mib returns MemAvailable plus SwapFree in MiB, or -1 without /proc/meminfo.
fn available_memory_mib() int {
	meminfo := os.read_file('/proc/meminfo') or { return -1 }
	mut kib := i64(0)
	for line in meminfo.split_into_lines() {
		if line.starts_with('MemAvailable:') || line.starts_with('SwapFree:') {
			kib += line.all_after(':').trim_space().all_before(' ').i64()
		}
	}
	return int(kib / 1024)
}

// Runner performs the runs of one service. It is used by a single thread.
struct Runner {
	config Config
mut:
	log        Log
	last_key   string
	failed_key string
	failed_at  time.Time
	// current holds the commits of the run in progress, so that a failure can name them.
	current Report
}

fn (r &Runner) vc_dir() string {
	return os.join_path(r.config.work_dir, 'vc.git')
}

fn (r &Runner) snapshot_dir() string {
	return os.join_path(r.config.work_dir, 'snapshot')
}

fn (mut r Runner) git(dir string, args []string, failure string) !string {
	mut command := ['git', '-C', dir]
	command << args
	return r.log.run(command, failure)!.trim_space()
}

// run makes one attempt to bring vlang/vc to the snapshot of vlang/v master. In dry-run mode it
// stops before pushing. `force` builds even if nothing changed since the last run.
fn (mut r Runner) run(force bool) Report {
	started := time.now()
	r.log = Log{
		echo: r.log.echo
	}
	r.log.note('run started (mode ${r.config.mode}${if force { ', forced' } else { '' }})')
	r.current = Report{}
	mut report := r.prepare(force) or {
		Report{
			...r.current
			outcome: .failed
			message: err.msg()
		}
	}
	if report.outcome == .would_publish && r.config.mode == .push {
		report = r.publish(report) or {
			Report{
				...report
				outcome: .failed
				message: err.msg()
			}
		}
	}
	if report.outcome == .failed {
		if report.key != '' {
			r.failed_key = report.key
			r.failed_at = time.now()
		}
	} else if report.outcome != .stale && report.key != '' {
		r.last_key = report.key
	}
	finished := Report{
		...report
		started:  started
		finished: time.now()
	}
	r.log.note('run finished: ${finished.outcome} ${finished.message}')
	return finished
}

// sync fetches master of both repositories, cloning them first if needed, and returns the
// vlang/v master commit and the vlang/vc master commit.
fn (mut r Runner) sync() !(string, string) {
	if !os.is_dir(os.join_path(r.config.v_dir, '.git')) {
		os.mkdir_all(os.dir(r.config.v_dir))!
		r.log.run(['git', 'clone', '--quiet', '--filter=blob:none', '--no-checkout', r.config.v_url,
			r.config.v_dir], 'cloning ${r.config.v_url} failed')!
	}
	if !os.is_dir(r.vc_dir()) {
		os.mkdir_all(r.config.work_dir)!
		r.log.run(['git', 'clone', '--quiet', '--bare', '--filter=blob:none', r.config.vc_url,
			r.vc_dir()], 'cloning ${r.config.vc_url} failed')!
	}
	// vlang/vc goes first: the vlang/v commit that a snapshot names was on master before the
	// snapshot was pushed, so the later vlang/v fetch contains it.
	r.git(r.vc_dir(), ['fetch', '--quiet', '--no-tags', 'origin', master_refspec],
		'fetching ${r.config.vc_url} failed')!
	r.git(r.config.v_dir, ['fetch', '--quiet', '--no-tags', 'origin', master_refspec],
		'fetching ${r.config.v_url} failed')!
	target := r.git(r.config.v_dir, ['rev-parse', '--verify', origin_master],
		'vlang/v master cannot be resolved')!
	published := r.git(r.vc_dir(), ['rev-parse', '--verify', origin_master],
		'vlang/vc master cannot be resolved')!
	return target, published
}

// only_ignored_paths_changed reports whether `target` descends from `source` and every path that
// changed between them is ignored.
fn (mut r Runner) only_ignored_paths_changed(source string, target string) bool {
	if r.log.run_status(['git', '-C', r.config.v_dir, 'merge-base', '--is-ancestor', source, target]).exit_code != 0 {
		return false
	}
	diff := r.log.run_status(['git', '-C', r.config.v_dir, 'diff', '--name-only', '--no-renames',
		'-z', source, target])
	if diff.exit_code != 0 {
		return false
	}
	return diff.output.split('\x00').filter(it != '').all(path_is_ignored(it))
}

// prepare decides whether vlang/v master needs a new snapshot. If it does, it builds V, generates
// and checks the snapshot, and commits it on top of vlang/vc master in the local bare clone, where
// publish can push it. In dry-run mode, when vlang/vc already holds a snapshot of the same commit,
// it builds anyway and compares the two instead.
fn (mut r Runner) prepare(force bool) !Report {
	target, published := r.sync()!
	key := '${target} ${published}'
	base := Report{
		v_commit:  target
		vc_commit: published
		key:       key
	}
	r.current = base
	if !force && key == r.last_key {
		return Report{
			...base
			outcome: .unchanged
			message: 'neither vlang/v nor vlang/vc moved since the last run'
		}
	}
	if !force && key == r.failed_key
		&& time.since(r.failed_at) < r.config.retry_failed_minutes * time.minute {
		return Report{
			...base
			outcome: .unchanged
			message: 'the last run for these commits failed at ${r.failed_at.format_ss()}; it is retried after ${r.config.retry_failed_minutes} minutes or on a new commit'
		}
	}
	source := parse_snapshot_source(r.git(r.vc_dir(), ['log', '-1', '--format=%s', published],
		'vlang/vc master cannot be read')!)
	if source == target && r.config.mode == .push && !force {
		return Report{
			...base
			outcome: .up_to_date
			message: 'vlang/vc already holds the snapshot of ${target}'
		}
	}
	if source != '' && source != target && !force && r.only_ignored_paths_changed(source, target) {
		return Report{
			...base
			outcome: .ignored
			message: 'only paths that cannot change the snapshot changed since ${source}'
		}
	}
	available := available_memory_mib()
	if r.config.min_available_mib > 0 && available >= 0 && available < r.config.min_available_mib {
		return error('only ${available} MiB of memory and swap are available, and a build needs ${r.config.min_available_mib} MiB (--min-available-mib)')
	}
	subject := r.git(r.config.v_dir, ['log', '-1', '--format=%s', target],
		'the subject of ${target} cannot be read')!
	r.git(r.config.v_dir, ['checkout', '--quiet', '--force', '--detach', target],
		'checking out ${target} failed')!
	// Also removes the nested vc and thirdparty/tcc clones of the previous build, which `make`
	// clones again. A stale nested clone would make its `git pull --rebase` fail.
	r.git(r.config.v_dir, ['clean', '-ffdxq'], 'cleaning the vlang/v checkout failed')!
	r.log.run_limited(in_dir(r.config.v_dir, ['make']), r.config.build_timeout_minutes,
		'building V failed')!
	generate_snapshot(mut r.log, r.config.v_dir, target, r.snapshot_dir(),
		r.config.build_timeout_minutes)!
	tree := r.snapshot_tree(published)!
	published_tree := r.git(r.vc_dir(), ['rev-parse', '--verify', '${published}^{tree}'],
		'the vlang/vc master tree cannot be resolved')!
	if source == target {
		if tree == published_tree {
			return Report{
				...base
				outcome: .verified
				message: 'the snapshot of ${target} is identical to vlang/vc ${published}'
			}
		}
		return Report{
			...base
			outcome: .differs
			message: 'the snapshot of ${target} differs from vlang/vc ${published} in ${r.differing_files(published)}'
		}
	}
	if tree == published_tree {
		return Report{
			...base
			outcome: .up_to_date
			message: 'the new snapshot is identical to vlang/vc ${published}'
		}
	}
	vc_commit := r.git(r.vc_dir(), ['-c', 'user.name=${bot_name}', '-c', 'user.email=${bot_email}',
		'commit-tree', tree, '-p', published, '-m', '${snapshot_message_prefix}${target} - ${subject}'],
		'committing the snapshot failed')!
	return Report{
		...base
		outcome:   .would_publish
		vc_commit: vc_commit
		message:   'committed ${vc_commit} on top of vlang/vc ${published}'
	}
}

// snapshot_tree writes the generated files into the bare vlang/vc clone and returns the tree of
// `published` with them replaced. It uses a private index, so no checkout and none of the old
// snapshot blobs are needed.
fn (mut r Runner) snapshot_tree(published string) !string {
	index := os.join_path(r.config.work_dir, 'snapshot.index')
	os.rm(index) or {}
	defer {
		os.rm(index) or {}
	}
	with_index := ['env', 'GIT_INDEX_FILE=${index}', 'git', '-C', r.vc_dir()]
	mut read_tree := with_index.clone()
	read_tree << ['read-tree', published]
	r.log.run(read_tree, 'reading the vlang/vc master tree failed')!
	for name, _ in snapshot_files {
		blob := r.git(r.vc_dir(), ['hash-object', '-w', '--no-filters', '--',
			os.join_path(r.snapshot_dir(), name)], 'storing ${name} failed')!
		mut update := with_index.clone()
		update << ['update-index', '--add', '--cacheinfo', '100644,${blob},${name}']
		r.log.run(update, 'staging ${name} failed')!
	}
	// The other blobs of the tree stay on the server in the blobless clone.
	mut write_tree := with_index.clone()
	write_tree << ['write-tree', '--missing-ok']
	return r.log.run(write_tree, 'writing the snapshot tree failed')!.trim_space()
}

fn (mut r Runner) differing_files(published string) string {
	mut names := []string{}
	for name, _ in snapshot_files {
		generated := r.git(r.vc_dir(), ['hash-object', '--no-filters', '--',
			os.join_path(r.snapshot_dir(), name)], '') or { '' }
		existing := r.git(r.vc_dir(), ['rev-parse', '--verify', '${published}:${name}'], '') or {
			''
		}
		if generated == '' || generated != existing {
			names << name
		}
	}
	return if names.len == 0 { 'files other than v.c and v_win.c' } else { names.join(' and ') }
}

// publish pushes a prepared snapshot to vlang/vc master, unless vlang/v master has moved past the
// commit it was built from. A queued run would otherwise rewind vlang/vc to a compiler older than
// master; the next run builds the newer master instead.
fn (mut r Runner) publish(prepared Report) !Report {
	remote := r.log.run(['git', 'ls-remote', r.config.v_url, 'refs/heads/master'],
		'reading vlang/v master failed')!
	latest := remote.all_before('\t').trim_space()
	if latest != prepared.v_commit {
		return Report{
			...prepared
			outcome: .stale
			message: 'vlang/v master moved to ${latest}, so the snapshot of ${prepared.v_commit} is not published'
		}
	}
	mut push := ['git', '-C', r.vc_dir()]
	push << r.credential_args()
	// Not forced: if vlang/vc master moved, the push fails and the next run starts from it.
	push << ['push', '--quiet', 'origin', '${prepared.vc_commit}:refs/heads/master']
	r.log.run(push, 'pushing to ${r.config.vc_url} failed')!
	return Report{
		...prepared
		outcome: .published
		message: 'pushed ${prepared.vc_commit} to ${r.config.vc_url}'
	}
}

// credential_args make git read the push token from VCGEN_PUSH_TOKEN in its own environment, so the
// token never appears in an argument, a URL, the log, or .git/config.
fn (r &Runner) credential_args() []string {
	if !r.config.vc_url.starts_with('https://') {
		return []
	}
	helper := '!f() { test "\$1" = get || exit 0; echo username=${r.config.push_user}; echo "password=\$VCGEN_PUSH_TOKEN"; }; f'
	return ['-c', 'credential.helper=', '-c', 'credential.helper=${helper}']
}
