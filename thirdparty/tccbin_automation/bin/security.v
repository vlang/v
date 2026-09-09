module bin

pub const state_ref = 'refs/heads/tccbin-automation-state'
pub const state_writer_concurrency = 'tccbin-automation-state-writer'
pub const publication_concurrency = 'tccbin-automation-publication'
pub const state_cas_backoff_seconds = [0, 1, 3]
pub const publisher_create_backoff_seconds = [0, 5, 15]
pub const original_run_search_seconds = [0, 15, 45, 90]
pub const native_gate_timeout_seconds = 5_400
pub const native_gate_max_attempts = 2
pub const handoff_max_dispatch_generations = 2
pub const global_schedule_unlock = 'TCCBIN_SCHEDULE_PUBLISH_UNLOCKED'
pub const macos_amd64_libgc_unlock = 'MACOS_AMD64_LIBGC_PUBLISH_UNLOCKED'

// PublisherRefKind is the closed server-side namespace classification.
pub enum PublisherRefKind {
	forbidden
	candidate
	gate_trigger
	canonical
}

// AppIdentityContract documents one non-interchangeable protected runtime identity.
pub struct AppIdentityContract {
pub:
	name                string
	id_variable         string
	private_key_secret  string
	repository_scope    string
	allowed_permissions []string
	forbidden_roles     []string
	protected_jobs      []string
}

// protected_app_identities returns the five distinct GitHub App contracts.
pub fn protected_app_identities() []AppIdentityContract {
	return [
		AppIdentityContract{
			name:                'state-writer'
			id_variable:         'TCCBIN_STATE_APP_ID'
			private_key_secret:  'TCCBIN_STATE_APP_PRIVATE_KEY'
			repository_scope:    'vlang/v'
			allowed_permissions: ['contents:write']
			forbidden_roles:     ['issues', 'actions-dispatch', 'publisher']
			protected_jobs:      ['state-pre', 'state-ack', 'state-verdict', 'state-reconciler']
		},
		AppIdentityContract{
			name:                'validator-dispatcher'
			id_variable:         'TCCBIN_VALIDATOR_APP_ID'
			private_key_secret:  'TCCBIN_VALIDATOR_APP_PRIVATE_KEY'
			repository_scope:    'vlang/v+vlang/tccbin'
			allowed_permissions: ['actions:write', 'actions:read', 'contents:read', 'checks:write',
				'statuses:write']
			forbidden_roles:     ['contents:write', 'issues', 'publisher']
			protected_jobs:      ['validator-dispatch', 'v-smoke-check-publish']
		},
		AppIdentityContract{
			name:                'tccbin-gate-dispatcher'
			id_variable:         'TCCBIN_GATE_APP_ID'
			private_key_secret:  'TCCBIN_GATE_APP_PRIVATE_KEY'
			repository_scope:    'vlang/tccbin'
			allowed_permissions: ['actions:write', 'contents:read']
			forbidden_roles:     ['contents:write', 'checks', 'statuses', 'issues']
			protected_jobs:      ['native-gate-rerun']
		},
		AppIdentityContract{
			name:                'issue-reporter'
			id_variable:         'TCCBIN_REPORTER_APP_ID'
			private_key_secret:  'TCCBIN_REPORTER_APP_PRIVATE_KEY'
			repository_scope:    'one-owner-repository'
			allowed_permissions: ['issues:write']
			forbidden_roles:     ['contents', 'actions:write', 'ledger', 'publisher']
			protected_jobs:      ['issue-reconcile']
		},
		AppIdentityContract{
			name:                'publisher'
			id_variable:         'TCCBIN_PUBLISH_APP_ID'
			private_key_secret:  'TCCBIN_PUBLISH_APP_PRIVATE_KEY'
			repository_scope:    'vlang/tccbin'
			allowed_permissions: ['contents:write']
			forbidden_roles:     ['workflows', 'issues', 'ledger', 'force', 'delete']
			protected_jobs:      ['candidate-ref-create', 'gate-trigger-ref-create',
				'canonical-promote']
		},
	]
}

// validate_security_contract checks the frozen identities, retry limits, and unlock names.
pub fn validate_security_contract() ! {
	identities := protected_app_identities()
	validate_app_identity_matrix(identities)!
	if state_cas_backoff_seconds != [0, 1, 3] || publisher_create_backoff_seconds != [0, 5, 15]
		|| original_run_search_seconds != [0, 15, 45, 90] || native_gate_max_attempts != 2
		|| handoff_max_dispatch_generations != 2 {
		return error('bounded retry policy changed from the frozen contract')
	}
	expected_unlocks := managed_target_ids.map(target_unlock_variable(it) or { '' })
	if expected_unlocks.any(it == '') || expected_unlocks.len != 6 {
		return error('each managed target must have one explicit unlock variable')
	}
}

// validate_app_identity_matrix rejects any permission, job, credential, or repository drift.
pub fn validate_app_identity_matrix(identities []AppIdentityContract) ! {
	if identities.len != 5 {
		return error('the control plane requires exactly five protected App identities')
	}
	mut names := []string{}
	mut id_variables := []string{}
	mut key_secrets := []string{}
	for identity in identities {
		if identity.name in names || identity.id_variable in id_variables
			|| identity.private_key_secret in key_secrets {
			return error('protected App identities and credential names must be distinct')
		}
		names << identity.name
		id_variables << identity.id_variable
		key_secrets << identity.private_key_secret
	}
	signatures := identities.map(identity_contract_signature(it))
	expected_signatures := [
		'state-writer|TCCBIN_STATE_APP_ID|TCCBIN_STATE_APP_PRIVATE_KEY|vlang/v|contents:write|issues,actions-dispatch,publisher|state-pre,state-ack,state-verdict,state-reconciler',
		'validator-dispatcher|TCCBIN_VALIDATOR_APP_ID|TCCBIN_VALIDATOR_APP_PRIVATE_KEY|vlang/v+vlang/tccbin|actions:write,actions:read,contents:read,checks:write,statuses:write|contents:write,issues,publisher|validator-dispatch,v-smoke-check-publish',
		'tccbin-gate-dispatcher|TCCBIN_GATE_APP_ID|TCCBIN_GATE_APP_PRIVATE_KEY|vlang/tccbin|actions:write,contents:read|contents:write,checks,statuses,issues|native-gate-rerun',
		'issue-reporter|TCCBIN_REPORTER_APP_ID|TCCBIN_REPORTER_APP_PRIVATE_KEY|one-owner-repository|issues:write|contents,actions:write,ledger,publisher|issue-reconcile',
		'publisher|TCCBIN_PUBLISH_APP_ID|TCCBIN_PUBLISH_APP_PRIVATE_KEY|vlang/tccbin|contents:write|workflows,issues,ledger,force,delete|candidate-ref-create,gate-trigger-ref-create,canonical-promote',
	]
	if signatures != expected_signatures {
		return error('protected App identity matrix differs from the frozen five-role contract')
	}
}

fn identity_contract_signature(identity AppIdentityContract) string {
	return [identity.name, identity.id_variable, identity.private_key_secret, identity.repository_scope,
		identity.allowed_permissions.join(','), identity.forbidden_roles.join(','),
		identity.protected_jobs.join(',')].join('|')
}

// target_unlock_variable maps each closed managed target to its repository variable.
pub fn target_unlock_variable(target_id string) !string {
	upper := target_id.replace('-', '_').to_upper()
	if target_id !in managed_target_ids {
		return error('unknown managed target')
	}
	return 'TCCBIN_${upper}_PUBLISH_UNLOCKED'
}

// publication_is_unlocked keeps targeted manual rollout separate from monthly unlock variables.
pub fn publication_is_unlocked(settings map[string]string, target_id string, scheduled bool,
	requires_macos_amd64_libgc bool, manual_green_publications int) bool {
	target_unlock := target_unlock_variable(target_id) or { return false }
	if requires_macos_amd64_libgc
		&& (target_id != 'macos-amd64' || settings[macos_amd64_libgc_unlock] != 'true') {
		return false
	}
	if !scheduled {
		return true
	}
	if manual_green_publications < 2 || settings[global_schedule_unlock] != 'true'
		|| settings[target_unlock] != 'true' {
		return false
	}
	return true
}

// validate_expected_check_sources rejects missing, aliased, or invalid runtime source IDs.
pub fn validate_expected_check_sources(actions_integration_id i64,
	validator_integration_id i64, gate_workflow_id i64) ! {
	if actions_integration_id <= 0 || validator_integration_id <= 0 || gate_workflow_id <= 0 {
		return error('required-check source and workflow IDs must be configured')
	}
	if actions_integration_id == validator_integration_id {
		return error('native and V smoke checks must have distinct expected sources')
	}
}

// publisher_ruleset_patterns expands the six targets without wildcarding target identity.
pub fn publisher_ruleset_patterns() []string {
	mut patterns := []string{cap: managed_target_ids.len * 2}
	for target_id in managed_target_ids {
		patterns << 'tccbin-candidate/${target_id}/*'
		patterns << 'tccbin-gate-trigger/${target_id}/*/*'
	}
	return patterns
}

// fnm_pathname_match implements the exact slash-sensitive subset used by the rulesets.
pub fn fnm_pathname_match(pattern string, reference string) bool {
	pattern_segments := pattern.split('/')
	reference_segments := reference.split('/')
	if pattern_segments.len != reference_segments.len {
		return false
	}
	for index, segment in pattern_segments {
		candidate := reference_segments[index]
		if candidate == '' || (segment != '*' && segment != candidate) {
			return false
		}
	}
	return true
}

// classify_publisher_ref accepts only the two create-only namespaces or six canonical refs.
pub fn classify_publisher_ref(reference string) PublisherRefKind {
	for target_id in managed_target_ids {
		if reference == 'thirdparty-${target_id}' {
			return .canonical
		}
		if fnm_pathname_match('tccbin-candidate/${target_id}/*', reference) {
			return .candidate
		}
		if fnm_pathname_match('tccbin-gate-trigger/${target_id}/*/*', reference) {
			return .gate_trigger
		}
	}
	return .forbidden
}

// publisher_ref_is_preflight_valid adds deterministic 64-hex IDs to ruleset depth checks.
pub fn publisher_ref_is_preflight_valid(reference string) bool {
	kind := classify_publisher_ref(reference)
	if kind == .canonical {
		return true
	}
	segments := reference.split('/')
	if kind == .candidate {
		return is_lower_hex_64(segments[2])
	}
	if kind == .gate_trigger {
		return is_lower_hex_64(segments[2]) && is_lower_hex_64(segments[3])
	}
	return false
}

// protected_job_identity_set_is_valid prevents credential-role co-residence.
pub fn protected_job_identity_set_is_valid(identity_names []string) bool {
	if identity_names.len > 1 {
		return false
	}
	return identity_names.len == 0 || protected_app_identities().any(it.name == identity_names[0])
}

fn is_lower_hex_64(value string) bool {
	return is_lower_hex_length(value, 64)
}

fn is_lower_hex_40(value string) bool {
	return is_lower_hex_length(value, 40)
}

fn is_lower_hex_length(value string, expected_length int) bool {
	if value.len != expected_length {
		return false
	}
	for byte in value.bytes() {
		if !(byte >= `0` && byte <= `9`) && !(byte >= `a` && byte <= `f`) {
			return false
		}
	}
	return true
}
