module tests

import os
import tccbin_automation.bin

struct DurablePlanTestRepository {
	root  string
	proof string
	head  string
	tree  string
}

fn plan_run(command string) string {
	result := os.execute(command)
	assert result.exit_code == 0, '${command}\n${result.output}'
	return result.output.trim_space()
}

fn plan_write(root string, relative_path string, source string) {
	path := os.join_path(root, relative_path)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn plan_fixture(name string) string {
	return os.read_file(os.join_path(automation_root(), 'tests', 'fixtures', name)) or {
		panic(err)
	}
}

fn plan_null() bin.JsonValue {
	return bin.JsonValue{
		kind: .null_value
	}
}

fn plan_string(value string) bin.JsonValue {
	return bin.JsonValue{
		kind:         .string_value
		string_value: value
	}
}

fn plan_integer(value i64) bin.JsonValue {
	return bin.JsonValue{
		kind:      .integer
		int_value: value
	}
}

fn plan_bool(value bool) bin.JsonValue {
	return bin.JsonValue{
		kind:       .boolean
		bool_value: value
	}
}

fn plan_array(values []bin.JsonValue) bin.JsonValue {
	return bin.JsonValue{
		kind:        .array
		array_value: values
	}
}

fn plan_object(keys []string, values []bin.JsonValue) bin.JsonValue {
	return bin.JsonValue{
		kind:          .object
		object_keys:   keys
		object_values: values
	}
}

fn plan_member(value bin.JsonValue, key string) bin.JsonValue {
	return value.object_value(key) or { panic('missing plan test member ${key}') }
}

fn plan_replace(value bin.JsonValue, key string, replacement bin.JsonValue) bin.JsonValue {
	index := value.object_keys.index(key)
	if value.kind != .object || index < 0 {
		panic('missing plan test replacement member ${key}')
	}
	mut values := value.object_values.clone()
	values[index] = replacement
	return bin.JsonValue{
		kind:          .object
		object_keys:   value.object_keys.clone()
		object_values: values
	}
}

fn plan_last_known_good(subject bin.JsonValue) bin.JsonValue {
	return plan_object(['sha', 'tree', 'input_fingerprint', 'artifact_fingerprint', 'manifest_hash',
		'digests'], [plan_member(subject, 'sha'), plan_member(subject, 'tree'),
		plan_member(subject, 'input_fingerprint'), plan_member(subject, 'artifact_fingerprint'),
		plan_member(subject, 'manifest_hash'), plan_member(subject, 'digests')])
}

fn plan_seeded_unknown_target(with_blockers bool) string {
	mut root := bin.parse_strict_json(plan_fixture('target-state.v-smoke-terminal-check.schema-fixture.json')) or {
		panic(err)
	}
	intent := plan_member(root, 'active_intent')
	good := plan_last_known_good(plan_member(intent, 'validation_subject'))
	for key, value in {
		'target_state':                 plan_string('unknown_blocked')
		'publication_state':            plan_string('idle')
		'bootstrap_required':           plan_bool(false)
		'last_known_good':              good
		'provisional_published':        plan_null()
		'active_intent':                plan_null()
		'post_validation_operation_id': plan_null()
		'native_gate_subject':          plan_null()
		'active_subject_hash':          plan_null()
		'native_gate_execution':        plan_null()
		'v_smoke_execution':            plan_null()
		'recovery_handoffs':            plan_array([])
		'active_recovery_handoff_id':   plan_null()
		'active_remediation_id':        plan_null()
		'active_remediation_binding':   plan_null()
		'remediation_check_sources':    plan_array([])
		'last_head_observation':        plan_null()
		'last_source_refetch':          plan_null()
		'last_native_validation':       plan_null()
	} {
		root = plan_replace(root, key, value)
	}
	if with_blockers {
		incident := plan_object(['incident_id', 'owner_repository', 'status', 'failure_class',
			'component', 'test_id', 'lane', 'input_fingerprint', 'artifact_fingerprint',
			'created_by_operation_id', 'resolved_by_sha'], [
			plan_string('8181818181818181818181818181818181818181818181818181818181818181'),
			plan_string('vlang/tccbin'),
			plan_string('active'),
			plan_string('compiler_regression'),
			plan_string('tinycc'),
			plan_string('compile-smoke'),
			plan_string('linux-amd64'),
			plan_member(root, 'input_fingerprint'),
			plan_null(),
			plan_member(root, 'last_operation_id'),
			plan_null(),
		])
		root = plan_replace(root, 'incidents', plan_array([incident]))
		root = plan_replace(root, 'owner_repository', plan_string('vlang/tccbin'))
		root = plan_replace(root, 'issue_number', plan_integer(42))
		root = plan_replace(root, 'blocking_probe_ids', plan_array([
			plan_string('compile-smoke'),
		]))
	} else {
		root = plan_replace(root, 'incidents', plan_array([]))
		root = plan_replace(root, 'issue_number', plan_null())
		root = plan_replace(root, 'blocking_probe_ids', plan_array([]))
	}
	return bin.canonical_json(root)
}

fn plan_proof_source(root string, head string) string {
	tree :=
		plan_run('git --no-replace-objects --git-dir ${os.quoted_path(root)} rev-parse ${head}^{tree}')
	line :=
		plan_run('git --no-replace-objects --git-dir ${os.quoted_path(root)} rev-list --parents -n 1 ${head}')
	parts := line.split(' ')
	assert parts.len == 2
	return '{"schema_version":1,"repository":"vlang/v","ref":"refs/heads/tccbin-automation-state","commit_sha":"${head}","remote_head":"${head}","tree_sha":"${tree}","parent_shas":["${parts[1]}"],"verification_verified":true,"verification_reason":"valid","verified_at":"2026-08-18T00:00:00Z","state_writer_app_id":1234,"actor_login":"state-writer[bot]","actor_node_id":"BOT_state_writer","actor_database_id":5678,"actor_type":"Bot"}'
}

fn prepare_plan_repository(suffix string, selected_source string,
	mutate_other_null bool) DurablePlanTestRepository {
	base := os.join_path(os.temp_dir(), 'tccbin-durable-plan-${os.getpid()}-${suffix}')
	work_root := '${base}-work'
	bare_root := '${base}.git'
	os.rmdir_all(work_root) or {}
	os.rmdir_all(bare_root) or {}
	os.mkdir_all(os.join_path(work_root, 'targets')) or { panic(err) }
	bootstrap := plan_fixture('target-state.bootstrap.schema-fixture.json')
	for target_id in ['linux-amd64', 'freebsd-amd64', 'macos-amd64', 'macos-arm64', 'openbsd-amd64',
		'windows-amd64'] {
		mut source := bootstrap.replace('linux-amd64', target_id)
		if mutate_other_null && target_id == 'freebsd-amd64' {
			root := bin.parse_strict_json(source) or { panic(err) }
			source = bin.canonical_json(plan_replace(root, 'last_native_validation',
				plan_object([], [])))
		}
		plan_write(work_root, 'targets/${target_id}.json', source)
	}
	source := plan_fixture('source-state.outage.schema-fixture.json')
	plan_write(work_root, 'sources/tinycc-mob.json', source)
	plan_write(work_root, 'sources/bdwgc-master.json', source.replace('tinycc-mob', 'bdwgc-master').replace('https://repo.or.cz/tinycc.git',
		'https://github.com/ivmai/bdwgc').replace('"ref": "mob"', '"ref": "master"'))
	plan_write(work_root, 'sources/libatomic_ops-master.json', source.replace('tinycc-mob',
		'libatomic_ops-master').replace('https://repo.or.cz/tinycc.git',
		'https://github.com/bdwgc/libatomic_ops').replace('"ref": "mob"', '"ref": "master"'))
	for command in [
		'git -C ${os.quoted_path(work_root)} init -q',
		'git -C ${os.quoted_path(work_root)} checkout -qb tccbin-automation-state',
		'git -C ${os.quoted_path(work_root)} config user.email plan@example.invalid',
		'git -C ${os.quoted_path(work_root)} config user.name "Plan Test"',
		'git -C ${os.quoted_path(work_root)} add -- targets sources',
		'git -C ${os.quoted_path(work_root)} commit -qm plan-root',
	] {
		plan_run(command)
	}
	plan_write(work_root, 'targets/linux-amd64.json', selected_source)
	plan_run('git -C ${os.quoted_path(work_root)} add -- targets/linux-amd64.json')
	plan_run('git -C ${os.quoted_path(work_root)} commit -qm plan-target')
	head := plan_run('git -C ${os.quoted_path(work_root)} rev-parse HEAD')
	tree := plan_run('git -C ${os.quoted_path(work_root)} rev-parse HEAD^{tree}')
	plan_run('git clone -q --bare ${os.quoted_path(work_root)} ${os.quoted_path(bare_root)}')
	os.rmdir_all(work_root) or {}
	real_root := os.real_path(bare_root)
	proof := os.join_path(real_root, 'plan-proof')
	os.mkdir_all(os.join_path(proof, 'historical')) or { panic(err) }
	os.write_file(os.join_path(proof, 'head.json'), plan_proof_source(real_root, head)) or {
		panic(err)
	}
	return DurablePlanTestRepository{
		root:  real_root
		proof: os.real_path(proof)
		head:  head
		tree:  tree
	}
}

fn plan_trust() bin.LiveStateTrust {
	return bin.LiveStateTrust{
		repository:          'vlang/v'
		state_writer_app_id: 1234
		actor_login:         'state-writer[bot]'
		actor_node_id:       'BOT_state_writer'
		actor_database_id:   5678
	}
}

fn plan_invocation() bin.DurableTargetPlanInvocation {
	return bin.DurableTargetPlanInvocation{
		source_id:         'tinycc'
		run_id:            9001
		run_attempt:       1
		operation_ordinal: 3
		workflow:          '.github/workflows/tccbin_automation.yml'
		workflow_sha:      'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
		observed_at:       '2026-08-18T10:11:12Z'
	}
}

fn plan_prepare(repository DurablePlanTestRepository, event bin.TransitionEvent) !bin.DurableTargetCommitPlan {
	return bin.prepare_durable_target_commit_plan(automation_root(), repository.root, plan_trust(),
		repository.proof, 'linux-amd64', event, plan_invocation())
}

fn plan_assert_rejected(repository DurablePlanTestRepository, event bin.TransitionEvent,
	expected string) {
	plan_prepare(repository, event) or {
		assert err.msg() == expected, 'expected `${expected}`, got `${err.msg()}`'
		return
	}
	panic('durable target planner unexpectedly accepted ${event}')
}

fn test_durable_target_commit_plan_whitelist_and_retry_reducer_are_closed() {
	assert bin.durable_target_event_allowed_for_test(.ledger_repaired_with_blockers)
	assert bin.durable_target_event_allowed_for_test(.ledger_repaired_without_blockers)
	refused := [bin.TransitionEvent.begin_bootstrap, .bootstrap_green, .bootstrap_red,
		.bootstrap_stale, .actionable_defect, .ledger_invalid, .begin_remediation, .remediation_green,
		.remediation_red, .validation_stale, .reserve_publish, .reserve_adopt_current, .start_build,
		.bind_candidate, .candidate_checks_green, .candidate_failed, .promotion_failed,
		.promotion_confirmed, .post_check_green, .post_check_red, .post_check_infra_exhausted,
		.source_unreachable, .source_restored, .rollback_promoted, .rollback_post_green,
		.rollback_failed, .corruption]
	assert refused.len == 27
	for event in refused {
		assert !bin.durable_target_event_allowed_for_test(event), event.str()
	}
	ambiguous := bin.reduce_durable_commit_outcome_for_test(1, 'ambiguous') or { panic(err) }
	assert ambiguous.verdict == 'reconcile_required'
	assert ambiguous.next_attempt == 1
	assert ambiguous.delay_seconds == 0
	first := bin.reduce_durable_commit_outcome_for_test(1, 'confirmed_conflict') or { panic(err) }
	second := bin.reduce_durable_commit_outcome_for_test(2, 'confirmed_conflict') or { panic(err) }
	third := bin.reduce_durable_commit_outcome_for_test(3, 'confirmed_conflict') or { panic(err) }
	assert first.verdict == 'replan_required' && first.next_attempt == 2 && first.delay_seconds == 1
	assert second.verdict == 'replan_required' && second.next_attempt == 3
		&& second.delay_seconds == 3
	assert third.verdict == 'unknown_blocked' && third.next_attempt == 3
}

fn test_durable_target_commit_plan_full_tree_parser_is_strictly_nul_framed() {
	oid := 'a'.repeat(40)
	record := '100644 blob ${oid} 12\ttargets/linux-amd64.json'
	parsed := bin.parse_durable_plan_tree_listing_for_test('${record}\x00') or { panic(err) }
	assert parsed == [record]
	for source in [record, '\x00', '${record}\x00\x00',
		'100755 blob ${oid} 12\ttargets/linux-amd64.json\x00',
		'100644 tree ${oid} 12\ttargets/linux-amd64.json\x00',
		'100644 blob ${oid} 012\ttargets/linux-amd64.json\x00',
		'100644 blob ${oid} 12\t../target.json\x00'] {
		bin.parse_durable_plan_tree_listing_for_test(source) or { continue }
		panic('durable full-tree parser accepted malformed source `${source}`')
	}
	second := '100644 blob ${'b'.repeat(40)} 9\tevidence/a.json'
	for noncanonical in ['${record}\x00${record}\x00', '${record}\x00${second}\x00'] {
		bin.parse_durable_plan_tree_listing_for_test(noncanonical) or { continue }
		panic('durable full-tree parser accepted duplicate or decreasing paths')
	}
}

fn test_durable_target_commit_plan_linear_indices_have_causal_bounds() {
	paths := ['evidence/00/a.json', 'evidence/00/b.json', 'evidence/01/c.json',
		'targets/linux-amd64.json']
	tree := bin.durable_plan_tree_linear_probe_for_test(paths) or { panic(err) }
	assert tree.parsed_records == 4
	assert tree.path_components == 11
	assert tree.tree_parts == 8
	capacity := bin.durable_plan_exact2_capacity_probe_for_test(99_999) or { panic(err) }
	assert capacity.parsed_records == 99_999
	assert capacity.exact2_predecessors == 99_999
	assert capacity.exact2_postimages == 100_000
	change_count := bin.durable_plan_exact2_change_cap_for_test([
		'targets/linux-amd64.json',
		'evidence/new.json',
	], 'targets/linux-amd64.json', 'evidence/new.json') or { panic(err) }
	assert change_count == 2
	mut third_rejected := false
	bin.durable_plan_exact2_change_cap_for_test(['targets/linux-amd64.json', 'evidence/new.json',
		'evidence/third.json'], 'targets/linux-amd64.json', 'evidence/new.json') or {
		assert err.msg() == 'durable target postimage contains more than the exact two permitted changes'
		third_rejected = true
	}
	assert third_rejected
	bin.durable_plan_exact2_capacity_probe_for_test(100_000) or {
		assert err.msg() == 'durable target predecessor inventory has no bounded slot for exact2 creation'
		return
	}
	panic('durable exact2 capacity accepted a 100001-entry postimage')
}

fn test_durable_target_commit_plan_physical_config_and_git_output_are_one_exact_join() {
	physical := '[core]\n\trepositoryformatversion = 0\n\tfilemode = true\n\tbare = true\n[remote "origin"]\n\turl = /srv/state-source\n\tfetch = +refs/heads/*:refs/remotes/origin/*\n'
	expected := ['core.repositoryformatversion', 'core.filemode', 'core.bare', 'remote.origin.url',
		'remote.origin.fetch']
	parsed := bin.parse_durable_git_physical_config_for_test(physical) or { panic(err) }
	assert parsed == expected
	git_output := '${expected.join('\x00')}\x00'
	joined := bin.join_durable_git_config_for_test(physical, git_output) or { panic(err) }
	assert joined == expected
	mut reordered := expected.clone()
	last := reordered[4]
	reordered[4] = reordered[3]
	reordered[3] = last
	bin.join_durable_git_config_for_test(physical, '${reordered.join('\x00')}\x00') or {
		assert err.msg() == 'durable Git runner Git key output differs from its physical lexical configuration'
		for poisoned in [
			physical.replace('[core]', '[include]'),
			physical.replace('\tbare = true', '\tbare = true\\'),
			physical.replace('\tfilemode = true', '# forged\n\tfilemode = true'),
			physical.replace('\tfilemode = true', '\tFileMode = true'),
			physical.replace('\tbare = true', 'bare = true'),
			physical.replace('\n\tbare = true', '\r\n\tbare = true'),
			physical.replace('\tbare = true', '\tbare = true\n\tbare = true'),
			physical.replace('[remote "origin"]', '[core]\n[remote "origin"]'),
		] {
			bin.parse_durable_git_physical_config_for_test(poisoned) or { continue }
			panic('durable Git physical config accepted `${poisoned}`')
		}
		return
	}
	panic('durable Git config join accepted reordered Git output')
}

fn test_durable_target_commit_plan_physical_config_reader_preserves_bytes_and_chunks() {
	repository := prepare_plan_repository('physical-config-reader',
		plan_seeded_unknown_target(false), false)
	defer {
		os.rmdir_all(repository.root) or {}
	}
	config_path := os.join_path(repository.root, 'config')
	physical := os.read_file(config_path) or { panic(err) }
	actual := bin.durable_git_config_snapshot_for_test(repository.root) or { panic(err) }
	expected_keys := bin.parse_durable_git_physical_config_for_test(physical) or { panic(err) }
	assert actual.source == physical
	assert actual.source.ends_with('\n')
	assert actual.keys == expected_keys
	assert actual.keys.len >= 3
	assert actual.keys[..3] == ['core.repositoryformatversion', 'core.filemode', 'core.bare']

	mut large_lines := ['[core]', '\trepositoryformatversion = 0', '\tfilemode = true',
		'\tbare = true']
	mut large_keys := ['core.repositoryformatversion', 'core.filemode', 'core.bare']
	for index in 0 .. 64 {
		remote := 'chunk-${index:03}'
		large_lines << '[remote "${remote}"]'
		large_lines << '\turl = /srv/tccbin/${remote}/${'a'.repeat(48)}'
		large_lines << '\tfetch = +refs/heads/*:refs/remotes/${remote}/*'
		large_keys << 'remote.${remote}.url'
		large_keys << 'remote.${remote}.fetch'
	}
	large_source := '${large_lines.join('\n')}\n'
	assert large_source.len > 4096
	assert large_source.len <= 64 * 1024
	os.write_file(config_path, large_source) or { panic(err) }
	large := bin.durable_git_config_snapshot_for_test(repository.root) or { panic(err) }
	assert large.source == large_source
	assert large.source.ends_with('\n')
	assert large.keys == large_keys
}

fn test_durable_target_commit_plan_abort_reducer_never_claims_a_hard_failed_kill() {
	now := u64(100)
	transient := bin.durable_git_kill_sequence_for_test(['hard', 'secured']) or { panic(err) }
	interrupted := bin.durable_git_kill_sequence_for_test(['interrupted', 'secured']) or {
		panic(err)
	}
	absent := bin.durable_git_kill_sequence_for_test(['absent']) or { panic(err) }
	persistent := bin.durable_git_kill_sequence_for_test(['hard', 'hard', 'hard']) or { panic(err) }
	assert transient && interrupted && absent && !persistent
	terminal := bin.durable_git_wait_disposition_for_test('terminal') or { panic(err) }
	running := bin.durable_git_wait_disposition_for_test('running') or { panic(err) }
	lost := bin.durable_git_wait_disposition_for_test('lost') or { panic(err) }
	wait_interrupted := bin.durable_git_wait_disposition_for_test('interrupted') or { panic(err) }
	assert terminal == 'terminal' && running == 'running' && wait_interrupted == 'retry'
		&& lost == 'lost'
	lost_error := bin.durable_git_wait_exclusivity_error_for_test('lost') or { panic(err) }
	foreign_error := bin.durable_git_wait_exclusivity_error_for_test('foreign') or { panic(err) }
	assert lost_error == 'durable Git runner lost exclusive child-reaping ownership'
	assert foreign_error == lost_error
	secured := bin.durable_git_abort_reducer_for_test(true, true, true, '', '', now, now + 1)
	assert secured.requested && secured.termination_secured && !secured.poison_required
	assert !secured.bound_expired
	pre_go := bin.durable_git_abort_reducer_for_test(false, false, true, '', '', now, now + 1)
	assert pre_go.termination_secured && !pre_go.poison_required
	drift := bin.durable_git_abort_reducer_for_test(true, true, true,
		'durable Git runner SIGCHLD ownership changed during execution', '', now, now + 1)
	assert drift.signal_drift && !drift.termination_secured && drift.poison_required
	held_pipe := bin.durable_git_abort_reducer_for_test(true, true, true, '', '', now, now +
		6_000_000_000)
	assert held_pipe.termination_secured && held_pipe.bound_expired
	assert held_pipe.poison_required
	hard := bin.durable_git_abort_reducer_for_test(true, false, true, '',
		'durable Git runner cannot terminate its reserved child identity', now, now + 6_000_000_000)
	assert hard.requested && !hard.termination_secured && hard.poison_required
	assert hard.bound_expired
	assert hard.failure.contains('cannot terminate its reserved child identity')
	bin.durable_git_post_eof_acceptance_for_test('') or { panic(err) }
	bin.durable_git_post_eof_acceptance_for_test('durable Git runner command exceeded its monotonic deadline') or {
		assert err.msg() == 'durable Git runner command exceeded its monotonic deadline'
		return
	}
	panic('durable runner accepted exit zero after a recorded abort failure')
}

fn test_durable_target_commit_plan_closed_runner_control_machine_is_causal() {
	assert bin.durable_git_planner_failure_flow_for_test('nested-runner', 'core', 'cleanup-drift') == 'nested-runner'
	assert bin.durable_git_planner_failure_flow_for_test('', 'core', 'cleanup-drift') == 'core'
	assert bin.durable_git_planner_failure_flow_for_test('', '', 'cleanup-drift') == 'cleanup-drift'
	postfork := bin.durable_git_control_machine_for_test('postfork_drift') or { panic(err) }
	assert postfork.go_count == 0 && postfork.kill_count == 0 && postfork.wait_count == 0
	assert postfork.lease_retained && postfork.second_runner_refused
	assert postfork.forbidden_rejections == 3
	assert postfork.failure == 'durable Git runner SIGCHLD ownership changed during execution'

	gate := bin.durable_git_control_machine_for_test('gate_failure') or { panic(err) }
	assert gate.go_count == 1 && gate.kill_count == 1 && gate.wait_count == 1
	assert !gate.lease_retained && !gate.second_runner_refused
	assert gate.forbidden_rejections == 0
	assert gate.failure == 'durable Git runner child release gate is invalid'

	cap := bin.durable_git_control_machine_for_test('cap_failure') or { panic(err) }
	assert cap.go_count == 1 && cap.kill_count == 2 && cap.wait_count == 1
	assert !cap.lease_retained
	assert cap.forbidden_rejections == 0
	assert cap.failure == 'durable Git runner command output exceeds its closed byte bound'

	post_eof := bin.durable_git_control_machine_for_test('post_eof_abort') or { panic(err) }
	assert post_eof.go_count == 1 && post_eof.kill_count == 2 && post_eof.wait_count == 1
	assert !post_eof.lease_retained
	assert post_eof.forbidden_rejections == 0
	assert post_eof.failure == 'durable Git runner command exceeded its monotonic deadline'

	poison := bin.durable_git_control_machine_for_test('poison_after_go') or { panic(err) }
	assert poison.go_count == 1 && poison.kill_count == 0 && poison.wait_count == 0
	assert poison.lease_retained && poison.second_runner_refused
	assert poison.forbidden_rejections == 3
	assert poison.failure == 'durable Git runner SIGCHLD ownership changed during execution'
	mut mutant_route_rejected := false
	bin.durable_git_control_machine_for_test('postfork_drift_then_go') or {
		assert err.msg() == 'durable Git control-machine test scenario is outside its closed set'
		mutant_route_rejected = true
	}
	assert mutant_route_rejected
}

fn test_durable_target_commit_plan_runner_inputs_are_closed_before_child_creation() {
	environment := bin.durable_git_environment_for_test(['PATH=/usr/bin:/bin', 'HOME=/tmp/ignored',
		'GIT_TRACE=/tmp/must-not-exist', 'LD_PRELOAD=/tmp/must-not-load']) or { panic(err) }
	assert environment == ['PATH=/usr/bin:/bin', 'LC_ALL=C', 'LANG=C', 'LANGUAGE=C',
		'GIT_NO_LAZY_FETCH=1', 'GIT_TERMINAL_PROMPT=0', 'GIT_OPTIONAL_LOCKS=0',
		'GIT_CONFIG_NOSYSTEM=1', 'GIT_CONFIG_GLOBAL=${os.path_devnull}']
	for poisoned in [
		['PATH=/usr/bin', 'Path=/bin'],
		['PATH=/usr/bin', 'GIT_CONFIG=/tmp/forged'],
		['PATH=/usr/bin', 'GIT_CONFIG_GLOBAL=/tmp/forged'],
		['PATH=/usr/bin', 'gIt_CoNfIg_NoSyStEm=0'],
		['PATH=/usr/bin', 'git_config_key_0=core.pager'],
		['PATH=/usr/bin', 'GIT_OBJECT_DIRECTORY=/tmp/objects'],
	] {
		bin.durable_git_environment_for_test(poisoned) or { continue }
		panic('durable Git environment accepted `${poisoned}`')
	}
	bin.validate_durable_git_config_for_test('core.repositoryformatversion\x00core.filemode\x00core.bare\x00remote.origin.url\x00remote.origin.fetch\x00remote.safe_name.mirror\x00') or {
		panic(err)
	}
	for poisoned in ['core.bare', 'core.bare\x00core.bare\x00', 'Core.bare\x00',
		'extensions.worktreeconfig\x00', 'include.path\x00', 'log.showsignature\x00',
		'gpg.program\x00', 'remote.origin.promisor\x00', 'remote.origin.partialclonefilter\x00',
		'diff.external\x00', 'core.pager\x00', 'remote._origin.url\x00', 'remote.origin_.url\x00',
		'remote.bad.name.url\x00', 'remote..url\x00', 'remote.origin.extra\x00'] {
		bin.validate_durable_git_config_for_test(poisoned) or { continue }
		panic('durable Git config accepted `${poisoned}`')
	}
	argv := bin.durable_git_argv_for_test('/usr/bin/git', '/srv/state.git', ['log', '--format=',
		'HEAD', '--', 'evidence'], true) or { panic(err) }
	assert argv == ['/usr/bin/git', '--no-pager', '--no-replace-objects', '--no-lazy-fetch',
		'--git-dir', '/srv/state.git', 'log', '--no-show-signature', '--no-ext-diff', '--no-textconv',
		'--no-renames', '--no-color', '--no-decorate', '--no-notes', '--no-use-mailmap',
		'--ignore-submodules=none', '-O', os.path_devnull, '--format=', 'HEAD', '--', 'evidence']
	assert bin.durable_git_fd_tuple_is_valid_for_test([3, 4, 5, 6, 7, 8, 9])
	for invalid in [[-1, 4, 5, 6, 7, 8, 9], [0, 4, 5, 6, 7, 8, 9],
		[3, 1, 5, 6, 7, 8, 9], [3, 4, 2, 6, 7, 8, 9], [3, 4, 5, 6, 7, 8, 8],
		[3, 4, 5, 6, 7, 8]] {
		assert !bin.durable_git_fd_tuple_is_valid_for_test(invalid)
	}
	mut opened := []string{}
	bin.durable_planner_platform_gate_for_test('windows', mut opened) or {
		assert err.msg() == 'durable target commit planning is unavailable on Windows without a raw-byte Git runner'
		assert opened.len == 0
		return
	}
	panic('durable planner Windows gate performed an open')
}

fn test_durable_target_commit_plan_runner_trace_reaps_only_after_both_eof() {
	repository := prepare_plan_repository('runner-trace', plan_seeded_unknown_target(false), false)
	trace_path := os.join_path(os.temp_dir(), 'tccbin-durable-git-trace-${os.getpid()}')
	os.rm(trace_path) or {}
	old_trace := os.getenv_opt('GIT_TRACE')
	os.setenv('GIT_TRACE', trace_path, true)
	defer {
		if previous := old_trace {
			os.setenv('GIT_TRACE', previous, true)
		} else {
			os.unsetenv('GIT_TRACE')
		}
		os.rmdir_all(repository.root) or {}
		os.rm(trace_path) or {}
	}
	trace := bin.durable_git_runner_trace_for_test(repository.root, ['rev-parse',
		'--is-bare-repository']) or { panic(err) }
	assert trace == ['argv-validated', 'sigchld-pre-command', 'repository-preflight-before',
		'argv-closed', 'seven-descriptors-validated', 'sigchld-prefork', 'positive-child-pid',
		'pgroup-and-sigchld-verified', 'go-write-one-guarded', 'parent-go-read-closed',
		'parent-go-write-closed', 'stdout-eof', 'stderr-eof', 'wait-after-eof', 'child-reaped',
		'sigchld-post-reap', 'repository-preflight-after']
	assert !os.exists(trace_path) && !os.is_link(trace_path)
	rejection := bin.durable_git_authority_adapter_rejection_for_test(repository.root,
		'rev-parse HEAD') or { panic(err) }
	assert rejection == 'durable Git authority adapter rejected noncanonical arguments'
	assert !os.exists(trace_path) && !os.is_link(trace_path)
}

fn test_durable_target_commit_plan_rejects_config_and_worktree_sources_before_objects() {
	configured := prepare_plan_repository('poisoned-config', plan_seeded_unknown_target(false),
		false)
	defer {
		os.rmdir_all(configured.root) or {}
	}
	config_path := os.join_path(configured.root, 'config')
	config_source := os.read_file(config_path) or { panic(err) }
	os.write_file(config_path, '${config_source}\n[remote "promisor"]\n\tpromisor = true\n') or {
		panic(err)
	}
	plan_assert_rejected(configured, .ledger_repaired_without_blockers,
		'durable Git runner physical configuration key is duplicated or outside its allowlist')

	worktree := prepare_plan_repository('poisoned-worktree', plan_seeded_unknown_target(false),
		false)
	defer {
		os.rmdir_all(worktree.root) or {}
	}
	os.write_file(os.join_path(worktree.root, 'config.worktree'), '[core]\n\tbare = true\n') or {
		panic(err)
	}
	plan_assert_rejected(worktree, .ledger_repaired_without_blockers,
		'durable Git runner repository contains a disallowed redirect or topology sidecar')
}

fn test_durable_target_commit_plan_prepares_exact2_deterministically() {
	repository := prepare_plan_repository('without-blockers', plan_seeded_unknown_target(false),
		false)
	defer {
		os.rmdir_all(repository.root) or {}
	}
	first_plan := plan_prepare(repository, .ledger_repaired_without_blockers) or { panic(err) }
	second_plan := plan_prepare(repository, .ledger_repaired_without_blockers) or { panic(err) }
	first := first_plan.observation()
	second := second_plan.observation()
	assert first == second
	assert first.state_commit == repository.head
	assert first.predecessor_tree_oid == repository.tree
	assert first.postimage_tree_oid.len == 40
	assert first.postimage_tree_oid != first.predecessor_tree_oid
	assert first.target_id == 'linux-amd64'
	assert first.event == 'ledger_repaired_without_blockers'
	assert first.result == 'passed'
	assert first.operation_id.len == 64
	assert first.plan_subject_fingerprint.len == 64
	assert first.operation_id != first.plan_subject_fingerprint
	assert first.changed_paths.len == 2
	assert first.changed_blobs.len == 2
	assert first.changed_paths.contains('targets/linux-amd64.json')
	assert first.changed_paths.contains(first.evidence_path)
	assert first.evidence_path.contains('/${first.operation_id}/')
	assert first.evidence_path.ends_with('-${first.plan_subject_fingerprint}.json')
	mut forged := first.changed_paths.clone()
	forged << 'targets/forged.json'
	assert first_plan.observation().changed_paths.len == 2
}

fn test_durable_target_commit_plan_preserves_blockers_and_refuses_cross_lane() {
	repository := prepare_plan_repository('with-blockers', plan_seeded_unknown_target(true), false)
	defer {
		os.rmdir_all(repository.root) or {}
	}
	plan := plan_prepare(repository, .ledger_repaired_with_blockers) or { panic(err) }
	observation := plan.observation()
	assert observation.result == 'blocked'
	target_blob := observation.changed_blobs.filter(it.path == 'targets/linux-amd64.json')
	assert target_blob.len == 1
	root := bin.parse_strict_json(target_blob[0].source) or { panic(err) }
	assert (root.object_value('target_state') or { panic('target_state') }).string_value == 'quarantined'
	assert (root.object_value('incidents') or { panic('incidents') }).array_value.len == 1
	plan_assert_rejected(repository, .ledger_repaired_without_blockers,
		'durable target ledger repair event differs from its exact incident lane')
}

fn test_durable_target_commit_plan_null6_fails_before_schema_authority() {
	repository := prepare_plan_repository('nonnull-peer', plan_seeded_unknown_target(false), true)
	defer {
		os.rmdir_all(repository.root) or {}
	}
	invalid_automation_root := os.join_path(repository.proof, 'absent-automation-root')
	bin.prepare_durable_target_commit_plan(invalid_automation_root, repository.root, plan_trust(),
		repository.proof, 'linux-amd64', .ledger_repaired_without_blockers, plan_invocation()) or {
		assert err.msg() == 'durable target null6 pre-scan requires last_native_validation null on all six targets'
		return
	}
	panic('durable target planner consulted invalid schema authority before null6')
}

fn test_durable_target_commit_plan_null6_checks_each_managed_target() {
	ids := bin.durable_managed_target_ids_for_test()
	assert ids.len == 6
	bootstrap := plan_fixture('target-state.bootstrap.schema-fixture.json')
	mut sources := ids.map(bootstrap.replace('linux-amd64', it))
	bin.validate_durable_null6_sources_for_test(sources) or { panic(err) }
	for index in 0 .. sources.len {
		mut mutated := sources.clone()
		root := bin.parse_strict_json(mutated[index]) or { panic(err) }
		mutated[index] = bin.canonical_json(plan_replace(root, 'last_native_validation',
			plan_object([], [])))
		bin.validate_durable_null6_sources_for_test(mutated) or {
			assert err.msg() == 'durable target null6 pre-scan requires last_native_validation null on all six targets'
			continue
		}
		panic('null6 accepted non-null target index ${index}')
	}
}

fn test_durable_target_commit_plan_independent_comparator_rejects_every_output_class() {
	repository := prepare_plan_repository('comparators', plan_seeded_unknown_target(false), false)
	defer {
		os.rmdir_all(repository.root) or {}
	}
	mutations := ['proof', 'inventory', 'schema', 'target', 'source', 'commitment', 'identity',
		'target_postimage', 'evidence', 'post_tree']
	rejected := bin.validate_durable_plan_comparator_mutations_for_test(automation_root(),
		repository.root, plan_trust(), repository.proof, 'linux-amd64',
		.ledger_repaired_without_blockers, plan_invocation()) or { panic(err) }
	assert rejected == mutations
}

fn test_durable_target_commit_plan_second_pass_physically_rereads_authority() {
	repository := prepare_plan_repository('between-pass', plan_seeded_unknown_target(false), false)
	defer {
		os.rmdir_all(repository.root) or {}
	}
	head_path := os.join_path(repository.proof, 'head.json')
	original := os.read_file(head_path) or { panic(err) }
	mutated := original.replace('"actor_login":"state-writer[bot]"',
		'"actor_login":"forged-state-writer[bot]"')
	assert mutated != original
	detected := bin.durable_plan_between_pass_physical_mutation_for_test(automation_root(),
		repository.root, plan_trust(), repository.proof, 'linux-amd64',
		.ledger_repaired_without_blockers, plan_invocation(), mutated) or { panic(err) }
	assert detected == 'live state commit proof is unsigned, stale, or outside the allowlisted ref'
	assert (os.read_file(head_path) or { panic(err) }) == original
}

fn test_durable_target_commit_plan_schema_closure_and_collision_slots_are_closed() {
	assert bin.durable_schema_closure_paths_for_test(automation_root()) or { panic(err) } == [
		'active-intent.schema.json',
		'common.schema.json',
		'evidence.schema.json',
		'lane-result.schema.json',
		'native-gate-execution.schema.json',
		'native-gate-subject.schema.json',
		'native-lane-matrix.schema.json',
		'recovery-handoff.schema.json',
		'source-state.schema.json',
		'target-state.schema.json',
		'toolchain-observation.schema.json',
	]
	operation_id := '9191919191919191919191919191919191919191919191919191919191919191'
	subject := '8'.repeat(64)
	planned_path := 'evidence/2026/08/1/1/linux-amd64/${operation_id}/1-ledger_repaired_without_blockers-${subject}.json'
	for key in ['operation_id', 'last_operation_id', 'intent_id', 'consumer_id', 'incident_id',
		'created_by_operation_id', 'handoff_id', 'active_remediation_id', 'active_intent_id',
		'predecessor_handoff_id', 'successor_handoff_id'] {
		source := '{"${key}":"${operation_id}"}'
		assert bin.durable_semantic_collision_for_test(source, operation_id, planned_path) or {
			panic(err)
		}, key
	}
	for key in ['operation_ids', 'incident_ids', 'waiting_consumers', 'consumer_ids', 'intent_ids',
		'handoff_ids'] {
		assert bin.durable_semantic_collision_for_test('{"${key}":["${operation_id}"]}',
			operation_id, planned_path) or { panic(err) }, key
	}
	assert bin.durable_semantic_collision_for_test('{"evidence_path":"${planned_path}"}',
		operation_id, planned_path) or { panic(err) }
	assert !(bin.durable_semantic_collision_for_test('{"sha256":"${operation_id}"}', operation_id,
		planned_path) or { panic(err) })
	for key in ['predecessor_sha', 'successor_sha', 'active_subject_hash', 'trigger_id',
		'subject_fingerprint', 'input_fingerprint', 'artifact_fingerprint', 'sha256'] {
		assert !(bin.durable_semantic_collision_for_test('{"${key}":"${operation_id}"}',
			operation_id, planned_path) or { panic(err) }), key
	}
}
