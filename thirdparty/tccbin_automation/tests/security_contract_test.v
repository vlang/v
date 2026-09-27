module tests

import tccbin_automation.bin

const security_id_a = 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
const security_id_b = 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'

fn test_ruleset_patterns_use_exact_fnm_pathname_depth_for_all_targets() {
	patterns := bin.publisher_ruleset_patterns()
	assert patterns.len == 12
	assert patterns.all(!it.contains('**'))
	for target_id in ['freebsd-amd64', 'linux-amd64', 'macos-amd64', 'macos-arm64', 'openbsd-amd64',
		'windows-amd64'] {
		candidate := 'tccbin-candidate/${target_id}/${security_id_a}'
		trigger := 'tccbin-gate-trigger/${target_id}/${security_id_a}/${security_id_b}'
		assert bin.classify_publisher_ref(candidate) == .candidate
		assert bin.classify_publisher_ref(trigger) == .gate_trigger
		assert bin.publisher_ref_is_preflight_valid(candidate)
		assert bin.publisher_ref_is_preflight_valid(trigger)
		assert bin.classify_publisher_ref('thirdparty-${target_id}') == .canonical
	}
}

fn test_ruleset_patterns_reject_unknown_empty_and_wrong_depth_refs() {
	invalid := [
		'tccbin-candidate/unknown/${security_id_a}',
		'tccbin-candidate/windows-amd64/',
		'tccbin-candidate/windows-amd64/${security_id_a}/extra',
		'tccbin-gate-trigger/windows-amd64/${security_id_a}',
		'tccbin-gate-trigger/windows-amd64/${security_id_a}/${security_id_b}/extra',
		'tccbin-gate-trigger/windows-amd64//${security_id_b}',
		'thirdparty-windows-amd64/extra',
	]
	for reference in invalid {
		assert bin.classify_publisher_ref(reference) == .forbidden
		assert !bin.publisher_ref_is_preflight_valid(reference)
	}
	assert !bin.publisher_ref_is_preflight_valid('tccbin-candidate/windows-amd64/' +
		security_id_a.to_upper())
}

fn test_targeted_manual_publish_does_not_depend_on_monthly_target_unlock() {
	mut settings := map[string]string{}
	assert bin.publication_is_unlocked(settings, 'windows-amd64', false, false, 0)
	assert !bin.publication_is_unlocked(settings, 'windows-amd64', true, false, 2)
	settings['TCCBIN_WINDOWS_AMD64_PUBLISH_UNLOCKED'] = 'true'
	assert bin.publication_is_unlocked(settings, 'windows-amd64', false, false, 0)
	assert !bin.publication_is_unlocked(settings, 'windows-amd64', true, false, 1)
	settings[bin.global_schedule_unlock] = 'true'
	assert !bin.publication_is_unlocked(settings, 'windows-amd64', true, false, 1)
	assert bin.publication_is_unlocked(settings, 'windows-amd64', true, false, 2)
	settings['TCCBIN_MACOS_AMD64_PUBLISH_UNLOCKED'] = 'true'
	assert !bin.publication_is_unlocked(settings, 'macos-amd64', false, true, 0)
	assert !bin.publication_is_unlocked(settings, 'macos-amd64', true, true, 2)
	settings[bin.macos_amd64_libgc_unlock] = 'true'
	assert bin.publication_is_unlocked(settings, 'macos-amd64', false, true, 0)
	assert bin.publication_is_unlocked(settings, 'macos-amd64', true, true, 2)
	assert !bin.publication_is_unlocked(settings, 'unknown', false, false, 0)
}

fn test_five_runtime_apps_and_retry_limits_are_non_interchangeable() {
	bin.validate_security_contract() or { panic(err) }
	identities := bin.protected_app_identities()
	assert identities.len == 5
	assert identities.map(it.id_variable).all(it != '')
	assert bin.protected_job_identity_set_is_valid([])
	assert bin.protected_job_identity_set_is_valid(['publisher'])
	assert !bin.protected_job_identity_set_is_valid(['publisher', 'state-writer'])
	assert !bin.protected_job_identity_set_is_valid(['unknown'])
	assert bin.state_cas_backoff_seconds == [0, 1, 3]
	assert bin.publisher_create_backoff_seconds == [0, 5, 15]
	assert bin.original_run_search_seconds == [0, 15, 45, 90]
	assert bin.native_gate_timeout_seconds == 5_400
	assert bin.native_gate_max_attempts == 2
	assert bin.handoff_max_dispatch_generations == 2
	mut mutated := identities.clone()
	mutated[0] = bin.AppIdentityContract{
		...mutated[0]
		allowed_permissions: ['contents:write', 'issues:write']
	}
	mut rejected := false
	bin.validate_app_identity_matrix(mutated) or { rejected = true }
	assert rejected
}

fn test_expected_check_sources_must_be_present_and_distinct() {
	bin.validate_expected_check_sources(1, 2, 3) or { panic(err) }
	mut rejected := false
	bin.validate_expected_check_sources(1, 1, 3) or { rejected = true }
	assert rejected
}
