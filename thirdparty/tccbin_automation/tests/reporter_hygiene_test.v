module tests

import os
import tccbin_automation.bin

fn issue_ledger_source() string {
	return os.read_file(os.join_path(automation_root(), 'tests', 'fixtures',
		'issue-ledger.dark.json')) or { panic(err) }
}

fn issue_ledger_is_rejected(source string) bool {
	bin.project_issue_ledger(source) or { return true }
	return false
}

fn test_persisted_issue_ledger_is_strictly_reread_and_projected() {
	projection := bin.project_issue_ledger(issue_ledger_source()) or { panic(err) }
	assert projection.owner_repository == 'vlang/tccbin'
	assert projection.os == 'windows'
	assert projection.entries.len == 1
	assert projection.entries[0].status == 'active'
	assert projection.entries[0].diagnostics.len == 2
	assert projection.entries[0].diagnostics[0].test_id == 'payload-link-ansi'
	assert projection.entries[0].diagnostics[1].test_id == 'payload-link-unicode'
	assert projection.should_be_open
	assert projection.marker_hash == bin.issue_marker_hash('vlang/tccbin', 'windows')
	resolved := bin.project_issue_ledger(issue_ledger_source().replace('"status": "active"',
		'"status": "resolved_bot"').replace('"status": "validating"', '"status": "resolved_bot"')) or {
		panic(err)
	}
	assert !resolved.should_be_open
}

fn test_persisted_issue_ledger_rejects_cross_boundary_stale_or_open_input() {
	source := issue_ledger_source()
	mutations := [
		source.replace_once('"state_ref": "refs/heads/tccbin-automation-state"',
			'"state_ref": "refs/heads/master"'),
		source.replace_once('"owner_repository": "vlang/tccbin",\n  "os": "windows"',
			'"owner_repository": "vlang/v",\n  "os": "windows"'),
		source.replace_once('"os": "windows"', '"os": "linux"'),
		source.replace_once('"status": "active"', '"status": "unknown"'),
		source.replace_once('"repository": "vlang/tccbin"', '"repository": "vlang/v"'),
		source.replace_once('"input_fingerprint": "1111111111111111111111111111111111111111111111111111111111111111",\n      "artifact_fingerprint"',
			'"input_fingerprint": "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",\n      "artifact_fingerprint"'),
		source.replace_once('"schema_version": 1,', '"schema_version": 1,\n  "unknown": true,'),
	]
	for mutation in mutations {
		assert issue_ledger_is_rejected(mutation)
	}
}

fn test_failure_routing_has_one_owner_or_no_issue() {
	assert bin.route_failure_owner('patch-probe-failed', false, false, '') or { panic(err) } == 'vlang/tccbin'
	assert bin.route_failure_owner('control-plane-failed', false, false, '') or { panic(err) } == 'vlang/v'
	assert bin.route_failure_owner('v-smoke-failed', true, true, '') or { panic(err) } == 'vlang/tccbin'
	assert bin.route_failure_owner('v-smoke-failed', false, false, '') or { panic(err) } == 'vlang/v'
	assert bin.route_failure_owner('ownership-ambiguous', false, false, '') or { panic(err) } == 'vlang/v'
	assert bin.route_failure_owner('source_unreachable', false, false, '') or { panic(err) } == ''
	ambiguous := bin.classify_failure_routing('v-smoke-failed', false, false, '') or { panic(err) }
	assert ambiguous.owner_repository == 'vlang/v'
	assert ambiguous.failure_class == 'ownership-ambiguous'
	causal := bin.classify_failure_routing('v-smoke-failed', true, true, '') or { panic(err) }
	assert causal.owner_repository == 'vlang/tccbin'
	assert causal.failure_class == 'v-smoke-failed'
}

fn base_diagnostic() bin.DiagnosticRecord {
	return bin.DiagnosticRecord{
		schema_version:       1
		repository:           'vlang/tccbin'
		os:                   'windows'
		target_id:            'windows-amd64'
		architecture:         'x64'
		component:            'payload'
		failure_class:        'payload-review-required'
		test_id:              'payload-inventory'
		lane:                 'x64'
		expected:             'all declared runtime inputs are present'
		observed_summary:     'missing declared runtime input'
		subject_sha:          'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
		input_fingerprint:    '1111111111111111111111111111111111111111111111111111111111111111'
		artifact_fingerprint: '2222222222222222222222222222222222222222222222222222222222222222'
		run_url:              'https://github.com/vlang/tccbin/actions/runs/1'
		job_url:              'https://github.com/vlang/tccbin/actions/runs/1/job/2'
		artifact_url:         'https://github.com/vlang/tccbin/actions/runs/1/artifacts/3'
		human_action:         'review-change'
	}
}

fn test_issue_projection_uses_exact_owner_os_and_secondary_key() {
	base := base_diagnostic()
	other_lane := bin.DiagnosticRecord{
		...base
		test_id: 'payload-link'
		lane:    'i386'
	}
	updated_same_lane := bin.DiagnosticRecord{
		...base
		subject_sha:      'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
		observed_summary: 'same lane on a newer exact subject'
	}
	other_component := bin.DiagnosticRecord{
		...base
		component:     'native-build'
		failure_class: 'native-build-failed'
		test_id:       'compile'
	}
	projection := bin.project_issue('vlang/tccbin', 'windows', [
		bin.IncidentProjectionInput{ diagnostic: base, status: 'active' },
		bin.IncidentProjectionInput{ diagnostic: other_lane, status: 'validating' },
		bin.IncidentProjectionInput{ diagnostic: updated_same_lane, status: 'active' },
		bin.IncidentProjectionInput{ diagnostic: other_component, status: 'active' },
	]) or { panic(err) }
	assert projection.entries.len == 2
	assert projection.entries[1].diagnostics.len == 2
	assert projection.should_be_open
	assert projection.marker_hash == bin.issue_marker_hash('vlang/tccbin', 'windows')
	mut rejected := false
	bin.project_issue('vlang/tccbin', 'solaris', []) or { rejected = true }
	assert rejected
}

fn test_diagnostic_type_and_schema_vocabulary_remain_exact() {
	diagnostic := base_diagnostic()
	bin.validate_diagnostic_record(diagnostic) or { panic(err) }
	mut rejected := false
	bin.validate_diagnostic_record(bin.DiagnosticRecord{
		...diagnostic
		failure_class: 'payload'
	}) or { rejected = true }
	assert rejected
	rejected = false
	bin.validate_diagnostic_record(bin.DiagnosticRecord{
		...diagnostic
		os: 'linux'
	}) or { rejected = true }
	assert rejected
	rejected = false
	bin.validate_diagnostic_record(bin.DiagnosticRecord{
		...diagnostic
		expected: 'x'.repeat(1025)
	}) or { rejected = true }
	assert rejected
	rejected = false
	bin.validate_diagnostic_record(bin.DiagnosticRecord{
		...diagnostic
		run_url: 'https://github.com/vlang/tccbin/actions/runs/1?token=secret'
	}) or { rejected = true }
	assert rejected
}

fn test_issue_projection_enforces_the_machine_managed_byte_bound() {
	base := base_diagnostic()
	mut incidents := []bin.IncidentProjectionInput{}
	for index in 0 .. 40 {
		incidents << bin.IncidentProjectionInput{
			diagnostic: bin.DiagnosticRecord{
				...base
				test_id:          'test-${index}'
				lane:             'lane-${index}'
				observed_summary: 'x'.repeat(4000)
			}
			status:     'active'
		}
	}
	bin.project_issue('vlang/tccbin', 'windows', incidents[..39].clone()) or { panic(err) }
	mut rejection := ''
	bin.project_issue('vlang/tccbin', 'windows', incidents.clone()) or { rejection = err.msg() }
	assert rejection == 'machine-managed issue projection exceeds its strict byte bound', rejection
}

fn test_summary_sanitization_is_utf8_safe_bounded_and_redacts() {
	private_path := '/ho' + 'me/person/work/file.log'
	credential := 'Authori' + 'zation: bearer secret'
	ansi := '\x1b[31mred\x1b[0m'
	long_unicode := 'é'.repeat(5000)
	sanitized := bin.sanitize_issue_summary('${private_path}\n${credential}\n' +
		'path=/tmp/private.log\nerror:C:\\Users\\person\\private.log\n' +
		'(https://token@example.com/a)\n${ansi}\n${long_unicode}')
	assert !sanitized.contains(private_path)
	assert !sanitized.contains('/tmp/private.log')
	assert !sanitized.contains('C:\\Users')
	assert !sanitized.contains('secret')
	assert !sanitized.contains('token@example.com')
	assert !sanitized.contains('\x1b')
	assert sanitized.len <= bin.issue_summary_max_bytes
	assert sanitized.runes().len > 0
	assert bin.diagnostic_payload_size_is_valid(bin.diagnostic_max_bytes)
	assert !bin.diagnostic_payload_size_is_valid(bin.diagnostic_max_bytes + 1)
	assert bin.issue_bot_zone_size_is_valid(bin.issue_bot_zone_max_bytes)
	assert !bin.issue_bot_zone_size_is_valid(bin.issue_bot_zone_max_bytes + 1)
}

fn test_public_hygiene_detects_review_ids_paths_and_attribution() {
	bad_id := 'pullrequest' + 'review-12345'
	bad_path := '/ho' + 'me/person/source'
	bad_attribution := 'generated' + ' by an AI'
	assert bin.public_hygiene_count('${bad_id}\n${bad_path}\n${bad_attribution}') == 3
	assert bin.public_hygiene_count('technical rationale only') == 0
}
