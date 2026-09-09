module tests

import crypto.sha256
import os
import tccbin_automation.bin

const authority_operation_id = '9191919191919191919191919191919191919191919191919191919191919191'

struct AuthorityTestRepository {
	root          string
	proof         string
	head          string
	tree          string
	parent        string
	target_source string
	target_oid    string
}

fn authority_fixture(name string) string {
	return os.read_file(os.join_path(automation_root(), 'tests', 'fixtures', name)) or {
		panic(err)
	}
}

fn authority_unknown_source() string {
	return authority_fixture('target-state.bootstrap.schema-fixture.json').replace_once('"target_state": "uninitialized"',
		'"target_state": "unknown_blocked"')
}

fn authority_trust() bin.LiveStateTrust {
	return bin.LiveStateTrust{
		repository:          'vlang/v'
		state_writer_app_id: 1234
		actor_login:         'state-writer[bot]'
		actor_node_id:       'BOT_state_writer'
		actor_database_id:   5678
	}
}

fn authority_context() bin.TransitionContext {
	return bin.TransitionContext{
		operation_id: authority_operation_id
	}
}

fn authority_run(command string) string {
	result := os.execute(command)
	assert result.exit_code == 0, '${command}\n${result.output}'
	return result.output.trim_space()
}

fn authority_write(root string, relative_path string, source string) {
	path := os.join_path(root, relative_path)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn authority_proof_source(root string, head string) string {
	tree :=
		authority_run('git --no-replace-objects --git-dir ${os.quoted_path(root)} rev-parse ${head}^{tree}')
	line :=
		authority_run('git --no-replace-objects --git-dir ${os.quoted_path(root)} rev-list --parents -n 1 ${head}')
	parts := line.split(' ')
	assert parts.len == 2
	return '{"schema_version":1,"repository":"vlang/v","ref":"refs/heads/tccbin-automation-state","commit_sha":"${head}","remote_head":"${head}","tree_sha":"${tree}","parent_shas":["${parts[1]}"],"verification_verified":true,"verification_reason":"valid","verified_at":"2026-08-02T00:00:00Z","state_writer_app_id":1234,"actor_login":"state-writer[bot]","actor_node_id":"BOT_state_writer","actor_database_id":5678,"actor_type":"Bot"}'
}

fn authority_proof_bundle(root string, head string, suffix string, old string,
	replacement string) string {
	bundle := os.join_path(root, 'authority-proof-${suffix}')
	os.rmdir_all(bundle) or {}
	os.mkdir_all(os.join_path(bundle, 'historical')) or { panic(err) }
	mut source := authority_proof_source(root, head)
	if old != '' {
		assert source.contains(old)
		source = source.replace_once(old, replacement)
	}
	os.write_file(os.join_path(bundle, 'head.json'), source) or { panic(err) }
	return os.real_path(bundle)
}

fn prepare_authority_repository(suffix string, target_source string,
	entry_variant string) AuthorityTestRepository {
	base := os.join_path(os.temp_dir(), 'tccbin-durable-authority-${os.getpid()}-${suffix}')
	work_root := '${base}-work'
	bare_root := '${base}.git'
	os.rmdir_all(work_root) or {}
	os.rmdir_all(bare_root) or {}
	os.mkdir_all(os.join_path(work_root, 'targets')) or { panic(err) }
	bootstrap := authority_fixture('target-state.bootstrap.schema-fixture.json')
	for target_id in ['linux-amd64', 'freebsd-amd64', 'macos-amd64', 'macos-arm64', 'openbsd-amd64',
		'windows-amd64'] {
		authority_write(work_root, 'targets/${target_id}.json', bootstrap.replace('linux-amd64',
			target_id))
	}
	source := authority_fixture('source-state.outage.schema-fixture.json')
	authority_write(work_root, 'sources/tinycc-mob.json', source)
	authority_write(work_root, 'sources/bdwgc-master.json', source.replace('tinycc-mob',
		'bdwgc-master').replace('https://repo.or.cz/tinycc.git', 'https://github.com/ivmai/bdwgc').replace('"ref": "mob"',
		'"ref": "master"'))
	authority_write(work_root, 'sources/libatomic_ops-master.json', source.replace('tinycc-mob',
		'libatomic_ops-master').replace('https://repo.or.cz/tinycc.git',
		'https://github.com/bdwgc/libatomic_ops').replace('"ref": "mob"', '"ref": "master"'))
	for command in [
		'git -C ${os.quoted_path(work_root)} init -q',
		'git -C ${os.quoted_path(work_root)} checkout -qb tccbin-automation-state',
		'git -C ${os.quoted_path(work_root)} config user.email authority@example.invalid',
		'git -C ${os.quoted_path(work_root)} config user.name "Authority Test"',
		'git -C ${os.quoted_path(work_root)} add -- targets sources',
		'git -C ${os.quoted_path(work_root)} commit -qm authority-root',
	] {
		authority_run(command)
	}
	target_path := os.join_path(work_root, 'targets', 'linux-amd64.json')
	match entry_variant {
		'missing' {
			os.rm(target_path) or { panic(err) }
		}
		'executable' {
			os.write_file(target_path, target_source) or { panic(err) }
			os.chmod(target_path, 0o755) or { panic(err) }
		}
		'', 'normal' {
			os.write_file(target_path, target_source) or { panic(err) }
		}
		else {
			panic('unsupported authority entry variant ${entry_variant}')
		}
	}
	authority_run('git -C ${os.quoted_path(work_root)} add -A -- targets sources')
	authority_run('git -C ${os.quoted_path(work_root)} commit -qm authority-target')
	head := authority_run('git -C ${os.quoted_path(work_root)} rev-parse HEAD')
	tree := authority_run('git -C ${os.quoted_path(work_root)} rev-parse HEAD^{tree}')
	parent := authority_run('git -C ${os.quoted_path(work_root)} rev-parse HEAD^')
	authority_run('git clone -q --bare ${os.quoted_path(work_root)} ${os.quoted_path(bare_root)}')
	os.rmdir_all(work_root) or {}
	real_root := os.real_path(bare_root)
	target_oid := if entry_variant == 'missing' {
		''
	} else {
		authority_run('git --no-replace-objects --git-dir ${os.quoted_path(real_root)} rev-parse ${head}:targets/linux-amd64.json')
	}
	proof := authority_proof_bundle(real_root, head, 'valid', '', '')
	return AuthorityTestRepository{
		root:          real_root
		proof:         proof
		head:          head
		tree:          tree
		parent:        parent
		target_source: target_source
		target_oid:    target_oid
	}
}

fn authority_prepare(repository AuthorityTestRepository) !bin.ReauthenticatedPreparedTargetStateWrite {
	return bin.prepare_reauthenticated_target_state_transition(automation_root(), repository.root,
		authority_trust(), repository.proof, 'linux-amd64', .ledger_repaired_without_blockers,
		authority_context())
}

fn authority_assert_rejected(repository AuthorityTestRepository, proof string, target_id string,
	expected string) {
	bin.prepare_reauthenticated_target_state_transition(automation_root(), repository.root,
		authority_trust(), proof, target_id, .ledger_repaired_without_blockers, authority_context()) or {
		assert err.msg().contains(expected), 'expected `${expected}`, got `${err.msg()}`'
		return
	}
	panic('reauthenticated authority unexpectedly accepted ${expected}')
}

fn authority_assert_rejected_exact(repository AuthorityTestRepository, proof string,
	target_id string, expected string) {
	bin.prepare_reauthenticated_target_state_transition(automation_root(), repository.root,
		authority_trust(), proof, target_id, .ledger_repaired_without_blockers, authority_context()) or {
		assert err.msg() == expected, 'expected exact `${expected}`, got `${err.msg()}`'
		return
	}
	panic('reauthenticated authority unexpectedly accepted exact ${expected}')
}

fn authority_assert_mutation_rejected(repository AuthorityTestRepository, mutation string,
	expected string) {
	bin.prepare_reauthenticated_target_state_transition_with_mutation_for_test(automation_root(),
		repository.root, authority_trust(), repository.proof, 'linux-amd64',
		.ledger_repaired_without_blockers, authority_context(), mutation) or {
		assert err.msg() == expected, 'mutation ${mutation}: `${err.msg()}`'
		return
	}
	panic('reauthenticated comparator accepted ${mutation}')
}

fn authority_assert_prepared_mutation_rejected(repository AuthorityTestRepository, mutation string,
	expected string) {
	bin.prepare_reauthenticated_target_state_transition_with_prepared_mutation_for_test(automation_root(),
		repository.root, authority_trust(), repository.proof, 'linux-amd64',
		.ledger_repaired_without_blockers, authority_context(), mutation) or {
		assert err.msg() == expected, 'prepared mutation ${mutation}: `${err.msg()}`'
		return
	}
	panic('reauthenticated prepared binding accepted ${mutation}')
}

fn authority_assert_distinct_snapshot_rejected(first AuthorityTestRepository,
	second AuthorityTestRepository, comparison_focus string, expected string) {
	bin.prepare_reauthenticated_target_state_transition_from_distinct_snapshots_for_test(automation_root(),
		first.root, first.proof, second.root, second.proof, authority_trust(), 'linux-amd64',
		.ledger_repaired_without_blockers, authority_context(), comparison_focus) or {
		assert err.msg() == expected, 'distinct snapshot ${comparison_focus}: `${err.msg()}`'
		return
	}
	panic('reauthenticated comparator accepted distinct snapshot ${comparison_focus}')
}

fn authority_assert_tree_parser_rejected(source string, expected string) {
	bin.parse_reauthenticated_target_tree_entry_for_test(source, 'targets/linux-amd64.json') or {
		assert err.msg() == expected, 'tree parser: `${err.msg()}`'
		return
	}
	panic('reauthenticated tree parser accepted malformed framing or entry')
}

fn test_durable_target_authority_reauthenticates_twice_and_returns_only_copies() {
	repository := prepare_authority_repository('positive', authority_unknown_source(), 'normal')
	defer {
		os.rmdir_all(repository.root) or {}
	}
	refs_before :=
		authority_run('git --no-replace-objects --git-dir ${os.quoted_path(repository.root)} show-ref')
	objects_before :=
		authority_run('git --no-replace-objects --git-dir ${os.quoted_path(repository.root)} rev-list --objects --all')
	result := authority_prepare(repository) or { panic(err) }
	proof := result.state_proof()
	prepared := result.prepared_write()
	assert proof.repository == 'vlang/v'
	assert proof.ref == 'refs/heads/tccbin-automation-state'
	assert proof.commit_sha == repository.head
	assert proof.remote_head == repository.head
	assert proof.tree_sha == repository.tree
	assert proof.parent_shas == [repository.parent]
	assert proof.verification_verified
	assert proof.verification_reason == 'valid'
	assert proof.state_writer_app_id == 1234
	assert proof.actor_login == 'state-writer[bot]'
	assert prepared.target_id == 'linux-amd64'
	assert prepared.target_path == 'targets/linux-amd64.json'
	assert prepared.transition == 'ledger_repaired_without_blockers'
	assert prepared.operation_id == authority_operation_id
	assert prepared.expected_generation == 0
	assert prepared.resulting_generation == 1
	assert prepared.expected_state_head_oid == repository.head
	assert prepared.predecessor_blob_oid == repository.target_oid
	assert prepared.predecessor_source_sha256 == sha256.sum256(repository.target_source.bytes()).hex()
	assert prepared.changed_members == ['applied_operations', 'generation', 'last_operation_id',
		'last_transition', 'target_state']
	assert prepared.source == bin.canonical_json(bin.parse_strict_json(prepared.source) or {
		panic(err)
	})
	second := authority_prepare(repository) or { panic(err) }
	assert second.state_proof() == proof
	assert second.prepared_write() == prepared
	// Immutable public structs require a test-only unsafe alias to mutate the getter-owned slices
	// directly. A fresh getter call must still return the sealed wrapper's original values.
	proof_copy := result.state_proof()
	mut proof_parent_copy := unsafe { proof_copy.parent_shas }
	proof_parent_copy[0] = 'b'.repeat(40)
	assert proof_copy.parent_shas[0] == 'b'.repeat(40)
	assert result.state_proof().parent_shas == [repository.parent]
	prepared_copy := result.prepared_write()
	mut changed_members_copy := unsafe { prepared_copy.changed_members }
	changed_members_copy[0] = 'forged'
	assert prepared_copy.changed_members[0] == 'forged'
	assert result.prepared_write().changed_members[0] == 'applied_operations'
	assert authority_run('git --no-replace-objects --git-dir ${os.quoted_path(repository.root)} show-ref') == refs_before
	assert authority_run('git --no-replace-objects --git-dir ${os.quoted_path(repository.root)} rev-list --objects --all') == objects_before
	assert authority_run('git --no-replace-objects --git-dir ${os.quoted_path(repository.root)} rev-parse ${repository.head}^{tree}') == repository.tree

	source := os.read_file(os.join_path(automation_root(), 'bin', 'durable_target_authority.v')) or {
		panic(err)
	}
	assert source.count('pub fn prepare_reauthenticated_target_state_transition(') == 1
	signature :=
		source.all_after('pub fn prepare_reauthenticated_target_state_transition(').all_before(') !ReauthenticatedPreparedTargetStateWrite')
	for forbidden in ['expected_generation', 'source string', 'head', 'path', 'hash', 'preconditions'] {
		assert !signature.contains(forbidden)
	}
	declaration :=
		source.all_after('pub struct ReauthenticatedPreparedTargetStateWrite {').all_before('}')
	assert !declaration.contains('pub:')
	for forbidden in ['net.http', 'createCommitOnBranch(', 'os.write_file', 'update-ref',
		'commit-tree', 'write-tree', 'mktree', 'hash-object -w'] {
		assert !source.contains(forbidden)
	}
}

fn test_durable_target_authority_comparator_is_causal_for_every_authenticated_projection() {
	repository := prepare_authority_repository('comparator', authority_unknown_source(), 'normal')
	defer {
		os.rmdir_all(repository.root) or {}
	}
	cases := {
		'proof_repository':        'reauthenticated state proof repository changed between passes'
		'proof_ref':               'reauthenticated state proof ref changed between passes'
		'proof_head':              'reauthenticated state proof HEAD changed between passes'
		'proof_remote_head':       'reauthenticated state proof HEAD changed between passes'
		'proof_tree':              'reauthenticated state proof tree changed between passes'
		'proof_parent':            'reauthenticated state proof parent tuple changed between passes'
		'proof_signature':         'reauthenticated state proof signature changed between passes'
		'proof_signature_reason':  'reauthenticated state proof signature changed between passes'
		'proof_time':              'reauthenticated state proof verification time changed between passes'
		'proof_app':               'reauthenticated state proof App changed between passes'
		'proof_actor_login':       'reauthenticated state proof actor changed between passes'
		'proof_actor_node_id':     'reauthenticated state proof actor changed between passes'
		'proof_actor_database_id': 'reauthenticated state proof actor changed between passes'
		'proof_actor_type':        'reauthenticated state proof actor changed between passes'
		'entry_mode':              'reauthenticated target-state tree mode changed between passes'
		'entry_type':              'reauthenticated target-state tree type changed between passes'
		'entry_oid':               'reauthenticated target-state tree blob OID changed between passes'
		'entry_size':              'reauthenticated target-state tree size changed between passes'
		'entry_path':              'reauthenticated target-state tree path changed between passes'
		'bytes':                   'reauthenticated target-state bytes changed between passes'
		'sha256':                  'reauthenticated target-state SHA-256 changed between passes'
		'schema':                  'reauthenticated target-state schema changed between passes'
		'target':                  'reauthenticated target identity changed between passes'
		'generation':              'reauthenticated target generation changed between passes'
		'semantic':                'reauthenticated target semantic projection changed between passes'
		'root':                    'reauthenticated target semantic projection changed between passes'
	}
	for mutation, expected in cases {
		authority_assert_mutation_rejected(repository, mutation, expected)
	}
}

fn test_durable_target_authority_tree_parser_closes_nul_framing_and_every_entry_field() {
	oid := 'a'.repeat(40)
	path := 'targets/linux-amd64.json'
	record := '100644 blob ${oid} 123\t${path}'
	canonical := '${record}\x00'
	parsed := bin.parse_reauthenticated_target_tree_entry_for_test(canonical, path) or {
		panic(err)
	}
	assert parsed == record
	framing_error := 'reauthenticated target-state tree lookup is not one exact NUL-terminated entry'
	for malformed in [
		record,
		'\x00${record}\x00',
		'${record}\x00\x00',
		'',
		'\x00',
		'${record}\x00${record}\x00',
	] {
		authority_assert_tree_parser_rejected(malformed, framing_error)
	}
	authority_assert_tree_parser_rejected('100644 blob ${oid} 123 ${path}\x00',
		'reauthenticated target-state tree entry is malformed')
	authority_assert_tree_parser_rejected('100644 blob ${oid}\t${path}\x00',
		'reauthenticated target-state tree metadata is malformed')
	authority_assert_tree_parser_rejected('100755 blob ${oid} 123\t${path}\x00',
		'reauthenticated target-state tree mode is not 100644')
	authority_assert_tree_parser_rejected('100644 tree ${oid} 123\t${path}\x00',
		'reauthenticated target-state tree type is not blob')
	authority_assert_tree_parser_rejected('100644 blob ${'A'.repeat(40)} 123\t${path}\x00',
		'reauthenticated target-state tree blob OID is not lowercase-40')
	for malformed_size in ['0', '-1', '01', 'x', '2097153'] {
		authority_assert_tree_parser_rejected('100644 blob ${oid} ${malformed_size}\t${path}\x00',
			'reauthenticated target-state tree size is not one canonical positive bounded decimal')
	}
	authority_assert_tree_parser_rejected('100644 blob ${oid} 123\ttargets/freebsd-amd64.json\x00',
		'reauthenticated target-state tree path differs from its derived target')
}

fn test_durable_target_authority_physically_reloads_distinct_snapshots_before_comparison() {
	first := prepare_authority_repository('physical-first', authority_unknown_source(), 'normal')
	defer {
		os.rmdir_all(first.root) or {}
	}
	second := prepare_authority_repository('physical-second', authority_unknown_source() + '\n',
		'normal')
	defer {
		os.rmdir_all(second.root) or {}
	}
	assert first.root != second.root
	assert first.proof != second.proof
	assert first.head != second.head
	assert first.tree != second.tree
	assert first.target_oid != second.target_oid
	for comparison_focus, expected in {
		'head': 'reauthenticated state proof HEAD changed between passes'
		'tree': 'reauthenticated state proof tree changed between passes'
		'blob': 'reauthenticated target-state tree blob OID changed between passes'
	} {
		authority_assert_distinct_snapshot_rejected(first, second, comparison_focus, expected)
	}
}

fn test_durable_target_authority_joins_every_prepared_field() {
	repository := prepare_authority_repository('prepared-fields', authority_unknown_source(),
		'normal')
	defer {
		os.rmdir_all(repository.root) or {}
	}
	cases := {
		'prepared_target_id':            'prepared target-state identity differs from its reauthenticated predecessor'
		'prepared_target_path':          'prepared target-state identity differs from its reauthenticated predecessor'
		'prepared_transition':           'prepared target-state transition differs from its typed request'
		'prepared_operation_id':         'prepared target-state transition differs from its typed request'
		'prepared_expected_generation':  'prepared target-state generation differs from its reauthenticated predecessor'
		'prepared_resulting_generation': 'prepared target-state generation differs from its reauthenticated predecessor'
		'prepared_expected_head':        'prepared target-state preconditions differ from reauthenticated Git bytes'
		'prepared_predecessor_oid':      'prepared target-state preconditions differ from reauthenticated Git bytes'
		'prepared_predecessor_sha256':   'prepared target-state preconditions differ from reauthenticated Git bytes'
		'prepared_resulting_oid':        'prepared target-state result differs from its content identities'
		'prepared_resulting_sha256':     'prepared target-state result differs from its content identities'
		'prepared_changed_members':      'prepared target-state changed-member set differs from its exact roots'
		'prepared_source':               'prepared target-state result differs from its content identities'
	}
	assert cases.len == 13
	for mutation, expected in cases {
		authority_assert_prepared_mutation_rejected(repository, mutation, expected)
	}
}

fn test_durable_target_authority_rejects_invalid_proof_inventory_entry_and_target() {
	repository := prepare_authority_repository('invalid-proof', authority_unknown_source(),
		'normal')
	defer {
		os.rmdir_all(repository.root) or {}
	}
	bad_ref := authority_proof_bundle(repository.root, repository.head, 'bad-ref',
		'"ref":"refs/heads/tccbin-automation-state"', '"ref":"refs/heads/main"')
	authority_assert_rejected(repository, bad_ref, 'linux-amd64', 'unsigned, stale, or outside')
	bad_signature := authority_proof_bundle(repository.root, repository.head, 'bad-signature',
		'"verification_verified":true', '"verification_verified":false')
	authority_assert_rejected(repository, bad_signature, 'linux-amd64',
		'unsigned, stale, or outside')
	bad_app := authority_proof_bundle(repository.root, repository.head, 'bad-app',
		'"state_writer_app_id":1234', '"state_writer_app_id":1235')
	authority_assert_rejected(repository, bad_app, 'linux-amd64', 'unsigned, stale, or outside')
	bad_tree := authority_proof_bundle(repository.root, repository.head, 'bad-tree',
		'"tree_sha":"${repository.tree}"', '"tree_sha":"${'b'.repeat(40)}"')
	authority_assert_rejected(repository, bad_tree, 'linux-amd64',
		'local tree cannot be authenticated')
	bad_parent := authority_proof_bundle(repository.root, repository.head, 'bad-parent',
		'"parent_shas":["${repository.parent}"]', '"parent_shas":["${'b'.repeat(40)}"]')
	authority_assert_rejected(repository, bad_parent, 'linux-amd64', 'local parent tuple differs')
	authority_assert_rejected_exact(repository, repository.proof, 'not-managed',
		'unknown managed target')

	missing := prepare_authority_repository('missing-entry', authority_unknown_source(), 'missing')
	defer {
		os.rmdir_all(missing.root) or {}
	}
	authority_assert_rejected(missing, missing.proof, 'linux-amd64',
		'does not contain exactly six targets')
	executable := prepare_authority_repository('executable-entry', authority_unknown_source(),
		'executable')
	defer {
		os.rmdir_all(executable.root) or {}
	}
	authority_assert_rejected(executable, executable.proof, 'linux-amd64', 'non-regular entry')
	schema_invalid_source := authority_unknown_source().replace_once('"target_state": "unknown_blocked"',
		'"target_state": "broken"')
	schema_invalid := prepare_authority_repository('schema-invalid', schema_invalid_source,
		'normal')
	defer {
		os.rmdir_all(schema_invalid.root) or {}
	}
	authority_assert_rejected(schema_invalid, schema_invalid.proof, 'linux-amd64',
		'schema or path identity is invalid')
	semantic_invalid_source := authority_unknown_source().replace_once('"generation": 0',
		'"generation": 1')
	semantic_invalid := prepare_authority_repository('semantic-invalid', semantic_invalid_source,
		'normal')
	defer {
		os.rmdir_all(semantic_invalid.root) or {}
	}
	authority_assert_rejected_exact(semantic_invalid, semantic_invalid.proof, 'linux-amd64',
		'live target state schema or path identity is invalid')
	target_invalid_source := authority_unknown_source().replace_once('"target_id": "linux-amd64"',
		'"target_id": "freebsd-amd64"')
	target_invalid := prepare_authority_repository('target-invalid', target_invalid_source,
		'normal')
	defer {
		os.rmdir_all(target_invalid.root) or {}
	}
	authority_assert_rejected(target_invalid, target_invalid.proof, 'linux-amd64',
		'schema or path identity is invalid')
}
