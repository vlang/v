module bin

// SourceResolutionAttempt is one bounded, independently observed source lookup.
pub struct SourceResolutionAttempt {
pub:
	ordinal                 int
	backoff_seconds         int
	connect_timeout_seconds int
	total_timeout_seconds   int
	failure_kind            ?SourceFailureKind
	resolved_sha            string
	resolved_tree           string
}

// SourceStateModel is the durable, source-scoped recovery cadence.
pub struct SourceStateModel {
pub:
	schema_version        int
	generation            i64
	source_id             string
	canonical_url         string
	ref                   string
	status                string
	resolved_sha          string
	source_fingerprint    string
	last_attempt_at       string
	mode                  SourceMode
	originating_run_id    i64
	waiting_consumers     []string
	applied_operation_ids []string
}

// SourceResolutionDecision separates an external outage from an actionable defect. The caller
// may dispatch work only after a successful resolution and may report only deterministic defects.
pub struct SourceResolutionDecision {
pub:
	state           SourceStateModel
	resolved_tree   string
	may_build       bool
	should_report   bool
	external_outage bool
}

// initial_source_state creates the source-scoped ledger entry before any network lookup.
pub fn initial_source_state(source_id string, canonical_url string, ref string,
	source_fingerprint string, observed_at string) !SourceStateModel {
	state := SourceStateModel{
		schema_version:     1
		source_id:          source_id
		canonical_url:      canonical_url
		ref:                ref
		status:             'resolved'
		source_fingerprint: source_fingerprint
		last_attempt_at:    observed_at
		mode:               .monthly
	}
	validate_source_state(state)!
	return state
}

// resolve_source applies one complete resolver observation using the frozen 0/15/45 policy.
// A transient failure becomes durable only after all three attempts have been exhausted.
pub fn resolve_source(current SourceStateModel, expected_generation i64, operation_id string,
	originating_run_id i64, observed_at string, waiting_consumers []string,
	attempts []SourceResolutionAttempt) !SourceResolutionDecision {
	validate_source_state(current)!
	if expected_generation != current.generation || !is_lower_hex_64(operation_id)
		|| originating_run_id <= 0 || !timestamp_is_exact(observed_at) || attempts.len == 0
		|| attempts.len > source_resolve_backoff_seconds.len {
		return error('source resolution identity, generation, timestamp, or attempt count is invalid')
	}
	validate_waiting_consumers(waiting_consumers)!
	if operation_id in current.applied_operation_ids {
		return error('source resolution operation replay requires the persisted result')
	}
	mut resolved_index := -1
	mut deterministic_index := -1
	for index, attempt in attempts {
		validate_source_attempt(attempt, index)!
		if attempt.resolved_sha != '' {
			if resolved_index >= 0 || deterministic_index >= 0 || index != attempts.len - 1 {
				return error('source resolution attempts continued after a terminal observation')
			}
			resolved_index = index
			continue
		}
		kind := attempt.failure_kind or {
			return error('failed source attempt lacks its closed failure classification')
		}

		if !source_failure_is_transient(kind) {
			if deterministic_index >= 0 || index != attempts.len - 1 {
				return error('deterministic source failure must terminate the resolver')
			}
			deterministic_index = index
		}
	}
	mut operations := current.applied_operation_ids.clone()
	operations << operation_id
	if operations.len > 128 {
		return error('source operation ledger is outside its strict bound')
	}
	if resolved_index >= 0 {
		attempt := attempts[resolved_index]
		next := SourceStateModel{
			...current
			generation:            current.generation + 1
			status:                'resolved'
			resolved_sha:          attempt.resolved_sha
			last_attempt_at:       observed_at
			originating_run_id:    originating_run_id
			waiting_consumers:     waiting_consumers.clone()
			applied_operation_ids: operations
		}
		validate_source_state(next)!
		return SourceResolutionDecision{
			state:         next
			resolved_tree: attempt.resolved_tree
			may_build:     true
		}
	}
	terminal := attempts[attempts.len - 1]
	kind := terminal.failure_kind or {
		return error('terminal source failure lacks its closed classification')
	}

	if source_failure_is_transient(kind) {
		if attempts.len != source_resolve_backoff_seconds.len {
			return error('transient source outage requires the complete bounded retry schedule')
		}
		next := SourceStateModel{
			...current
			generation:            current.generation + 1
			status:                'source_unreachable'
			resolved_sha:          ''
			last_attempt_at:       observed_at
			mode:                  .upstream_recovery_daily
			originating_run_id:    originating_run_id
			waiting_consumers:     waiting_consumers.clone()
			applied_operation_ids: operations
		}
		validate_source_state(next)!
		return SourceResolutionDecision{
			state:           next
			external_outage: true
		}
	}
	status := if kind == .integrity { 'integrity_failed' } else { 'invalid_configuration' }
	next := SourceStateModel{
		...current
		generation:            current.generation + 1
		status:                status
		resolved_sha:          ''
		last_attempt_at:       observed_at
		mode:                  .monthly
		originating_run_id:    originating_run_id
		waiting_consumers:     waiting_consumers.clone()
		applied_operation_ids: operations
	}
	validate_source_state(next)!
	return SourceResolutionDecision{
		state:         next
		should_report: true
	}
}

// complete_source_resolution returns to monthly cadence only after the resumed functional path
// produced a definitive green/no-op or a routed deterministic defect.
pub fn complete_source_resolution(current SourceStateModel, expected_generation i64,
	operation_id string, functional_result string, observed_at string) !SourceStateModel {
	validate_source_state(current)!
	if expected_generation != current.generation || !is_lower_hex_64(operation_id)
		|| !timestamp_is_exact(observed_at)
		|| functional_result !in ['green', 'no_op', 'functional_defect_routed']
		|| current.status != 'resolved' || current.resolved_sha == ''
		|| operation_id in current.applied_operation_ids {
		return error('source completion is not bound to a fresh resolved functional result')
	}
	mut operations := current.applied_operation_ids.clone()
	operations << operation_id
	next := SourceStateModel{
		...current
		generation:            current.generation + 1
		last_attempt_at:       observed_at
		mode:                  .monthly
		originating_run_id:    0
		waiting_consumers:     []
		applied_operation_ids: operations
	}
	validate_source_state(next)!
	return next
}

// source_daily_resolution_is_due makes the daily workflow a no-op in monthly mode and forbids
// a second external lookup inside the same 24-hour period.
pub fn source_daily_resolution_is_due(state SourceStateModel, last_attempt_unix i64,
	now_unix i64) !bool {
	validate_source_state(state)!
	return source_retry_due(state.mode, last_attempt_unix, now_unix)
}

pub fn validate_source_state(state SourceStateModel) ! {
	if state.schema_version != 1 || state.generation < 0
		|| source_state_path(state.source_id)! == ''
		|| !canonical_source_url_is_valid(state.canonical_url)
		|| !source_ref_matches_identity(state.source_id, state.ref)
		|| state.status !in ['resolved', 'source_unreachable', 'invalid_configuration', 'integrity_failed']
		|| !is_lower_hex_64(state.source_fingerprint) || !timestamp_is_exact(state.last_attempt_at)
		|| state.originating_run_id < 0 || state.applied_operation_ids.len > 128 {
		return error('source state identity, status, timestamp, or bounds are invalid')
	}
	validate_waiting_consumers(state.waiting_consumers)!
	mut operation_ids := []string{}
	for operation_id in state.applied_operation_ids {
		if !is_lower_hex_64(operation_id) || operation_id in operation_ids {
			return error('source state operation identities are invalid or duplicated')
		}
		operation_ids << operation_id
	}
	if state.status == 'resolved' {
		if state.resolved_sha != '' && !is_lower_hex_40(state.resolved_sha) {
			return error('resolved source state SHA is invalid')
		}
	} else if state.resolved_sha != '' {
		return error('failed source state cannot carry a resolved SHA')
	}
	if state.status == 'source_unreachable' {
		if state.mode != .upstream_recovery_daily || state.originating_run_id <= 0 {
			return error('source outage must retain its originating run and daily cadence')
		}
	} else if state.mode == .upstream_recovery_daily && state.status != 'resolved' {
		return error('daily recovery mode is reserved for an outage or its resolved continuation')
	}
}

fn validate_source_attempt(attempt SourceResolutionAttempt, expected_index int) ! {
	if attempt.ordinal != expected_index + 1
		|| attempt.backoff_seconds != source_resolve_backoff_seconds[expected_index]
		|| attempt.connect_timeout_seconds != source_connect_timeout_seconds
		|| attempt.total_timeout_seconds != source_total_timeout_seconds {
		return error('source attempt differs from the frozen retry and timeout policy')
	}
	if attempt.resolved_sha != '' {
		if !is_lower_hex_40(attempt.resolved_sha) || !is_lower_hex_40(attempt.resolved_tree)
			|| attempt.failure_kind != none {
			return error('successful source attempt lacks an exact SHA/tree or carries a failure')
		}
	} else if attempt.resolved_tree != '' || attempt.failure_kind == none {
		return error('failed source attempt carries a tree or lacks a failure class')
	}
}

fn validate_waiting_consumers(consumers []string) ! {
	if consumers.len > managed_target_ids.len {
		return error('source waiting consumer list is outside the managed target bound')
	}
	mut seen := []string{}
	for consumer in consumers {
		if !is_lower_hex_64(consumer) || consumer in seen {
			return error('source waiting consumer identity is invalid or duplicated')
		}
		seen << consumer
	}
}

fn canonical_source_url_is_valid(value string) bool {
	return value.starts_with('https://') && value.len > 12 && value.len <= 2048
		&& !value.contains('@') && !value.contains('?') && !value.contains('#')
}

fn source_ref_matches_identity(source_id string, ref string) bool {
	return match source_id {
		'tinycc-mob' { ref == 'mob' }
		'bdwgc-master', 'libatomic_ops-master' { ref == 'master' }
		else { false }
	}
}
