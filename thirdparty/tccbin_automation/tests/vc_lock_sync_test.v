module tests

import os
import crypto.sha256
import tccbin_automation.bin

fn vc_sync_lock_source() string {
	return os.read_file(os.join_path(automation_root(), 'bootstrap', 'vc.lock')) or { panic(err) }
}

fn test_vc_lock_render_reproduces_the_current_lock_byte_for_byte() {
	source := vc_sync_lock_source()
	parsed := bin.parse_vc_bootstrap_lock(source) or { panic(err) }
	rendered := bin.render_vc_bootstrap_lock(parsed) or { panic(err) }
	assert rendered == source
	assert rendered.bytes() == source.bytes()
}

fn test_vc_lock_parse_reads_back_every_rendered_lock() {
	current := bin.parse_vc_bootstrap_lock(vc_sync_lock_source()) or { panic(err) }
	variants := [
		current,
		bin.VcBootstrapLock{
			repository: current.repository
			commit:     current.tree
			tree:       current.commit
			v_c:        current.v_win_c
			v_win_c:    current.v_c
		},
		bin.VcBootstrapLock{
			repository: current.repository
			commit:     '0'.repeat(40)
			tree:       'f'.repeat(40)
			v_c:        bin.VcBootstrapArtifact{
				mode:      '100644'
				blob:      '1'.repeat(40)
				byte_size: 1
				sha256:    'a'.repeat(64)
			}
			v_win_c:    bin.VcBootstrapArtifact{
				mode:      '100644'
				blob:      '2'.repeat(40)
				byte_size: max_u64
				sha256:    '0'.repeat(64)
			}
		},
	]
	for variant in variants {
		rendered := bin.render_vc_bootstrap_lock(variant) or { panic(err) }
		assert bin.parse_vc_bootstrap_lock(rendered) or { panic(err) } == variant
		assert bin.render_vc_bootstrap_lock(bin.parse_vc_bootstrap_lock(rendered) or { panic(err) }) or {
			panic(err)
		} == rendered
	}
}

fn test_vc_lock_render_refuses_records_the_parser_rejects() {
	current := bin.parse_vc_bootstrap_lock(vc_sync_lock_source()) or { panic(err) }
	artifact := current.v_c
	invalid_artifacts := [
		bin.VcBootstrapArtifact{
			...artifact
			mode: '100755'
		},
		bin.VcBootstrapArtifact{
			...artifact
			blob: artifact.blob.to_upper()
		},
		bin.VcBootstrapArtifact{
			...artifact
			blob: '${artifact.blob[..39]} '
		},
		bin.VcBootstrapArtifact{
			...artifact
			byte_size: 0
		},
		bin.VcBootstrapArtifact{
			...artifact
			sha256: artifact.sha256[..63]
		},
		bin.VcBootstrapArtifact{
			...artifact
			sha256: '${artifact.sha256}\nextra=true'
		},
	]
	mut invalid_locks := [
		bin.VcBootstrapLock{
			...current
			repository: 'https://github.com/vlang/vc.git'
		},
		bin.VcBootstrapLock{
			...current
			commit: current.commit.to_upper()
		},
		bin.VcBootstrapLock{
			...current
			tree: current.tree[..39]
		},
		bin.VcBootstrapLock{
			...current
			commit: '${current.commit}\ntree=${current.tree}'
		},
	]
	for invalid in invalid_artifacts {
		invalid_locks << bin.VcBootstrapLock{
			...current
			v_c: invalid
		}
		invalid_locks << bin.VcBootstrapLock{
			...current
			v_win_c: invalid
		}
	}
	for invalid in invalid_locks {
		mut rejected := false
		bin.render_vc_bootstrap_lock(invalid) or { rejected = true }
		assert rejected, '${invalid}'
	}
}

// vc_sync_checkout_ref returns the `ref:` of the `with:` block that holds line `index`.
fn vc_sync_checkout_ref(lines []string, index int) string {
	indent := lines[index].len - lines[index].trim_left(' ').len
	mut first := index
	for first > 0 && lines[first - 1].trim_space() != ''
		&& lines[first - 1].len - lines[first - 1].trim_left(' ').len == indent {
		first--
	}
	mut last := index
	for last + 1 < lines.len && lines[last + 1].trim_space() != ''
		&& lines[last + 1].len - lines[last + 1].trim_left(' ').len == indent {
		last++
	}
	mut refs := []string{}
	for line in lines[first..last + 1] {
		if line.trim_space().starts_with('ref: ') {
			refs << line.trim_space().all_after('ref: ')
		}
	}
	return if refs.len == 1 { refs[0] } else { '' }
}

fn test_every_vc_snapshot_sha_copy_agrees_with_the_lock() {
	vc_lock := bin.validate_vc_bootstrap_contract(automation_root()) or { panic(err) }
	vroot := os.real_path(os.join_path(automation_root(), '..', '..'))
	workflows := os.join_path(vroot, '.github', 'workflows')
	mut names := os.ls(workflows) or { panic(err) }
	names.sort()
	mut copies := []string{}
	for name in names {
		if !name.ends_with('.yml') && !name.ends_with('.yaml') {
			continue
		}
		lines := (os.read_file(os.join_path(workflows, name)) or { panic(err) }).split_into_lines()
		for index, line in lines {
			if line.trim_space() != 'repository: vlang/vc' {
				continue
			}
			ref := vc_sync_checkout_ref(lines, index)
			assert ref == vc_lock.commit, '${name}:${index + 1} checks out vlang/vc at `${ref}`, but vc.lock pins ${vc_lock.commit}'
			copies << name
		}
	}
	assert copies == ['tccbin_automation_contract.yml', 'update_tccbin.yml', 'update_tccbin.yml']
	contract_test := os.read_file(os.join_path(automation_root(), 'tests',
		'workflow_contract_test.v')) or { panic(err) }
	mut constants := []string{}
	for line in contract_test.split_into_lines() {
		if line.trim_space().starts_with("vc_lock := '") {
			constants << line.trim_space().all_after("vc_lock := '").all_before("'")
		}
	}
	assert constants == [vc_lock.commit]
}

fn vc_sync_git(args []string) os.Result {
	// The same hermetic values that the automation's own Git calls require.
	for name, value in {
		'GIT_NO_LAZY_FETCH':   '1'
		'GIT_TERMINAL_PROMPT': '0'
		'GIT_CONFIG_NOSYSTEM': '1'
		'GIT_CONFIG_SYSTEM':   os.path_devnull
		'GIT_CONFIG_GLOBAL':   os.path_devnull
	} {
		os.setenv(name, value, true)
	}
	mut command := ['git', '-c', 'user.name=VC Lock Test', '-c', 'user.email=ci@example.invalid',
		'-c', 'core.autocrlf=false', '-c', 'commit.gpgsign=false']
	command << args
	return os.exec(command)
}

fn vc_sync_git_output(args []string) string {
	result := vc_sync_git(args)
	assert result.exit_code == 0, '${args}: ${result.output}'
	return result.output.trim_space()
}

fn vc_sync_new_repository(root string) {
	os.mkdir_all(root) or { panic(err) }
	vc_sync_git_output(['-c', 'init.defaultBranch=master', 'init', '--quiet', root])
}

fn vc_sync_empty_commit(root string, message string) string {
	vc_sync_git_output(['-C', root, 'commit', '--quiet', '--allow-empty', '-m', message])
	return vc_sync_git_output(['-C', root, 'rev-parse', 'HEAD'])
}

struct VcSyncSnapshot {
	message    string
	files      map[string]string
	executable []string
}

fn vc_sync_snapshot_commit(root string, snapshot VcSyncSnapshot) string {
	for path in ['v.c', 'v_win.c'] {
		os.rm(os.join_path(root, path)) or {}
	}
	for path, content in snapshot.files {
		os.write_file(os.join_path(root, path), content) or { panic(err) }
	}
	vc_sync_git_output(['-C', root, 'add', '--all'])
	for path in snapshot.executable {
		vc_sync_git_output(['-C', root, 'update-index', '--chmod=+x', '--', path])
	}
	vc_sync_git_output(['-C', root, 'commit', '--quiet', '--allow-empty', '-m', snapshot.message])
	return vc_sync_git_output(['-C', root, 'rev-parse', 'HEAD'])
}

fn vc_sync_c_files(v_c_commit string, v_win_c_commit string) map[string]string {
	return {
		'v.c':     '#define V_COMMIT_HASH "${v_c_commit}"\nint main(void) { return 0; }\n'
		'v_win.c': '#define V_COMMIT_HASH "${v_win_c_commit}"\n// windows\nint main(void) { return 0; }\n'
	}
}

// vc_sync_apply_diff applies `diff` to a copy of `current` with `git apply` and returns the result.
fn vc_sync_apply_diff(base string, current string, diff string) string {
	root := os.join_path(base, 'apply')
	os.rmdir_all(root) or {}
	vc_sync_new_repository(root)
	lock_path := os.join_path(root, 'thirdparty', 'tccbin_automation', 'bootstrap', 'vc.lock')
	os.mkdir_all(os.dir(lock_path)) or { panic(err) }
	os.write_file(lock_path, current) or { panic(err) }
	patch_path := os.join_path(base, 'vc-lock.diff')
	os.write_file(patch_path, diff) or { panic(err) }
	vc_sync_git_output(['-C', root, 'apply', '--whitespace=error-all', patch_path])
	return os.read_file(lock_path) or { panic(err) }
}

fn test_vc_lock_diff_is_empty_for_equal_locks_and_applies_with_git() {
	current := vc_sync_lock_source()
	assert bin.diff_vc_bootstrap_lock(current, current) or { panic(err) } == ''
	parsed := bin.parse_vc_bootstrap_lock(current) or { panic(err) }
	base := os.join_path(os.temp_dir(), 'tccbin-vc-lock-diff-${os.getpid()}')
	os.rmdir_all(base) or {}
	os.mkdir_all(base) or { panic(err) }
	defer {
		os.rmdir_all(base) or {}
	}
	for next_lock in [
		bin.VcBootstrapLock{
			...parsed
			tree: 'e'.repeat(40)
		},
		bin.VcBootstrapLock{
			...parsed
			v_win_c: parsed.v_c
		},
		bin.VcBootstrapLock{
			...parsed
			commit:  'c'.repeat(40)
			v_win_c: parsed.v_c
		},
	] {
		next := bin.render_vc_bootstrap_lock(next_lock) or { panic(err) }
		diff := bin.diff_vc_bootstrap_lock(current, next) or { panic(err) }
		assert diff.starts_with('--- a/thirdparty/tccbin_automation/bootstrap/vc.lock\n+++ b/thirdparty/tccbin_automation/bootstrap/vc.lock\n@@ -1,6 +1,6 @@\n')
		assert vc_sync_apply_diff(base, current, diff) == next
	}
	mut rejected := false
	bin.diff_vc_bootstrap_lock(current, current.replace_once('format=', 'formats=')) or {
		rejected = true
	}
	assert rejected
}

struct VcSyncRejection {
	label    string
	snapshot VcSyncSnapshot
	v_master string
	expected string
}

fn test_vc_lock_plan_renders_only_snapshots_proven_by_content() {
	base := os.join_path(os.temp_dir(), 'tccbin-vc-lock-plan-${os.getpid()}')
	os.rmdir_all(base) or {}
	defer {
		os.rmdir_all(base) or {}
	}
	v_root := os.join_path(base, 'v')
	vc_root := os.join_path(base, 'vc')
	vc_sync_new_repository(v_root)
	vc_sync_new_repository(vc_root)
	source := vc_sync_empty_commit(v_root, 'first: the snapshot source (#1)')
	v_master := vc_sync_empty_commit(v_root, 'second: master moved on (#2)')
	side := vc_sync_git_output(['-C', v_root, 'commit-tree', '-p', source, '-m', 'side',
		'${source}^{tree}'])
	os.write_file(os.join_path(vc_root, 'README.md'), 'vc\n') or { panic(err) }
	current := vc_sync_lock_source()
	vc_commit := vc_sync_snapshot_commit(vc_root, VcSyncSnapshot{
		message: '[v:master] ${source} - first: the snapshot source (#1)'
		files:   vc_sync_c_files(source, source)
	})
	update := bin.plan_vc_bootstrap_lock_update(current, os.join_path(vc_root, '.git'), vc_commit,
		os.join_path(v_root, '.git'), v_master) or { panic(err) }
	assert update.provenance == bin.VcSnapshotProvenance{
		vc_commit: vc_commit
		v_commit:  source
		title:     'first: the snapshot source (#1)'
	}
	assert update.current == current
	assert update.changed()
	next := bin.parse_vc_bootstrap_lock(update.next) or { panic(err) }
	assert next.repository == 'https://github.com/vlang/vc'
	assert next.commit == vc_commit
	assert next.tree == vc_sync_git_output(['-C', vc_root, 'rev-parse', '${vc_commit}^{tree}'])
	files := vc_sync_c_files(source, source)
	for path, artifact in {
		'v.c':     next.v_c
		'v_win.c': next.v_win_c
	} {
		assert artifact.mode == '100644'
		assert artifact.blob == vc_sync_git_output(['-C', vc_root, 'rev-parse', '${vc_commit}:${path}'])
		assert artifact.byte_size == u64(files[path].len)
		assert artifact.sha256 == sha256.sum256(files[path].bytes()).hex()
	}
	assert bin.render_vc_bootstrap_lock(next) or { panic(err) } == update.next
	os.mkdir_all(base) or { panic(err) }
	assert vc_sync_apply_diff(base, current, update.diff) == update.next
	settled := bin.plan_vc_bootstrap_lock_update(update.next, os.join_path(vc_root, '.git'),
		vc_commit, os.join_path(v_root, '.git'), v_master) or { panic(err) }
	assert settled.diff == ''
	assert !settled.changed()
	// Planning reads the lock it is given; the reviewed lock on disk never changes.
	assert vc_sync_lock_source() == current
	good_message := '[v:master] ${source} - first'
	rejections := [
		VcSyncRejection{
			label:    'other-branch'
			snapshot: VcSyncSnapshot{
				message: '[v:main] ${source} - first'
				files:   vc_sync_c_files(source, source)
			}
			v_master: v_master
			expected: 'VC snapshot message is not'
		},
		VcSyncRejection{
			label:    'short-sha'
			snapshot: VcSyncSnapshot{
				message: '[v:master] ${source[..12]} - first'
				files:   vc_sync_c_files(source, source)
			}
			v_master: v_master
			expected: 'VC snapshot message is not'
		},
		VcSyncRejection{
			label:    'no-title'
			snapshot: VcSyncSnapshot{
				message: '[v:master] ${source}'
				files:   vc_sync_c_files(source, source)
			}
			v_master: v_master
			expected: 'VC snapshot message is not'
		},
		VcSyncRejection{
			label:    'body'
			snapshot: VcSyncSnapshot{
				message: '${good_message}\n\nmore'
				files:   vc_sync_c_files(source, source)
			}
			v_master: v_master
			expected: 'VC snapshot message is not'
		},
		VcSyncRejection{
			label:    'not-an-ancestor'
			snapshot: VcSyncSnapshot{
				message: '[v:master] ${side} - side'
				files:   vc_sync_c_files(side, side)
			}
			v_master: v_master
			expected: 'is not an ancestor of vlang/v master'
		},
		VcSyncRejection{
			label:    'newer-than-master'
			snapshot: VcSyncSnapshot{
				message: '[v:master] ${v_master} - second'
				files:   vc_sync_c_files(v_master, v_master)
			}
			v_master: source
			expected: 'is not an ancestor of vlang/v master'
		},
		VcSyncRejection{
			label:    'unknown-source'
			snapshot: VcSyncSnapshot{
				message: '[v:master] ${'b'.repeat(40)} - unknown'
				files:   vc_sync_c_files('b'.repeat(40), 'b'.repeat(40))
			}
			v_master: v_master
			expected: 'is not in the vlang/v history'
		},
		VcSyncRejection{
			label:    'stale-v_win.c'
			snapshot: VcSyncSnapshot{
				message: good_message
				files:   vc_sync_c_files(source, v_master)
			}
			v_master: v_master
			expected: 'VC snapshot v_win.c does not define V_COMMIT_HASH'
		},
		VcSyncRejection{
			label:    'stale-v.c'
			snapshot: VcSyncSnapshot{
				message: good_message
				files:   vc_sync_c_files(v_master, source)
			}
			v_master: v_master
			expected: 'VC snapshot v.c does not define V_COMMIT_HASH'
		},
		VcSyncRejection{
			label:    'hash-not-first'
			snapshot: VcSyncSnapshot{
				message: good_message
				files:   {
					'v.c':     '// header\n#define V_COMMIT_HASH "${source}"\n'
					'v_win.c': '#define V_COMMIT_HASH "${source}"\n'
				}
			}
			v_master: v_master
			expected: 'VC snapshot v.c does not define V_COMMIT_HASH'
		},
		VcSyncRejection{
			label:    'missing-v_win.c'
			snapshot: VcSyncSnapshot{
				message: good_message
				files:   {
					'v.c': vc_sync_c_files(source, source)['v.c']
				}
			}
			v_master: v_master
			expected: 'VC snapshot must contain exactly one v_win.c'
		},
		VcSyncRejection{
			label:    'executable-v.c'
			snapshot: VcSyncSnapshot{
				message:    good_message
				files:      vc_sync_c_files(source, source)
				executable: ['v.c']
			}
			v_master: v_master
			expected: 'VC lock v.c mode is invalid'
		},
	]
	for rejection in rejections {
		commit := vc_sync_snapshot_commit(vc_root, rejection.snapshot)
		mut message := ''
		bin.plan_vc_bootstrap_lock_update(current, os.join_path(vc_root, '.git'), commit,
			os.join_path(v_root, '.git'), rejection.v_master) or { message = err.msg() }
		assert message.contains(rejection.expected), '${rejection.label}: ${message}'
	}
	assert vc_sync_lock_source() == current
}
