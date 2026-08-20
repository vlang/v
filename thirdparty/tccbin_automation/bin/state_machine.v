module bin

import crypto.sha256

const applied_operation_limit = 128

// TargetState is the closed target lifecycle state.
pub enum TargetState {
	uninitialized
	eligible
	quarantined
	validating
	unknown_blocked
}

// PublicationState is the closed publication lifecycle state.
pub enum PublicationState {
	idle
	candidate_pending
	promotion_blocked
	post_publish_validating
	post_publish_waiting_source
	adopt_current_waiting_source
	post_publish_blocked
	rollback_pending
	rollback_waiting_source
	rollback_blocked
	restored_last_known_good
}

// TransitionEvent is the pure input vocabulary for the Phase A state machine.
pub enum TransitionEvent {
	begin_bootstrap
	bootstrap_green
	bootstrap_red
	bootstrap_stale
	actionable_defect
	ledger_invalid
	ledger_repaired_with_blockers
	ledger_repaired_without_blockers
	begin_remediation
	remediation_green
	remediation_red
	validation_stale
	reserve_publish
	reserve_adopt_current
	start_build
	bind_candidate
	candidate_checks_green
	candidate_failed
	promotion_failed
	promotion_confirmed
	post_check_green
	post_check_red
	post_check_infra_exhausted
	source_unreachable
	source_restored
	rollback_promoted
	rollback_post_green
	rollback_failed
	corruption
}

// HeadRelationship is an ancestry fact re-fetched from the canonical repository.
pub enum HeadRelationship {
	unknown
	exact_subject
	subject_ancestor
	unrelated
}

// DigestModel is one canonical path/digest pair retained in durable tuples.
pub struct DigestModel {
pub:
	path   string
	sha256 string
}

// ArtifactTupleModel is the complete last-known-good or provisional tuple.
pub struct ArtifactTupleModel {
pub:
	sha                  string
	tree                 string
	input_fingerprint    string
	artifact_fingerprint string
	manifest_hash        string
	digests              []DigestModel
}

// CandidateBindingModel is immutable after authoritative refetch of a create-only candidate ref.
pub struct CandidateBindingModel {
pub:
	sha                  string
	tree                 string
	parent               string
	artifact_fingerprint string
	manifest_hash        string
	digests              []DigestModel
}

// ValidationSubjectModel binds adoption to an existing HEAD and its create-only validation ref.
pub struct ValidationSubjectModel {
pub:
	sha                  string
	tree                 string
	input_fingerprint    string
	artifact_fingerprint string
	manifest_hash        string
	digests              []DigestModel
	candidate_ref        string
}

// ResolvedSourceModel records one source URL/ref/SHA/tree binding used by an intention.
pub struct ResolvedSourceModel {
pub:
	id         string
	repository string
	ref        string
	sha        string
	tree       string
}

// SourceCheckModel is the durable result of resolving one exact source binding.
pub struct SourceCheckModel {
pub:
	source_id       string
	resolved_sha    string
	status          string
	evidence_digest string
}

// ResolvedInputsModel keeps source, recipe, contract, and authenticated producer identity outside
// booleans. An unresolved producer keeps the entire durable resolved-inputs object absent.
pub struct ResolvedInputsModel {
pub:
	sources             []ResolvedSourceModel
	source_checks       []SourceCheckModel
	recipe_path         string
	recipe_hash         string
	contract_repository string
	contract_sha        string
	v_source_sha        string
	producer_toolchain  ProducerToolchainModel
}

// CheckSourceModel binds a required check name to its exact Integration and workflow IDs.
pub struct CheckSourceModel {
pub:
	name           string
	repository     string
	integration_id i64
	workflow_id    i64
	workflow_path  string
	event          string
}

// PersistedGateRunModel retains every correlated run fact instead of a green boolean.
pub struct PersistedGateRunModel {
pub:
	check_name                      string
	repository                      string
	integration_id                  i64
	workflow_id                     i64
	workflow_path                   string
	event                           string
	run_id                          i64
	run_attempt                     int
	check_suite_id                  i64
	check_suite_integration_id      i64
	job_id                          i64
	subject_hash                    string
	check_run_id                    i64
	external_id                     string
	run_name                        string
	run_url                         string
	job_url                         string
	details_url                     string
	ref                             string
	workflow_head_sha               string
	sha                             string
	check_sha                       string
	actor                           string
	actor_integration_id            i64
	triggering_actor                string
	triggering_actor_integration_id i64
	created_at                      string
	completed_at                    string
	run_conclusion                  string
	check_conclusion                string
	output_digest                   string
	evidence_digest                 string
}

// IntentDeadlinesModel carries the bounded build/check/promotion deadlines.
pub struct IntentDeadlinesModel {
pub:
	build_deadline     string
	checks_deadline    string
	promotion_deadline string
}

// ActiveIntentModel is the recoverable candidate/adoption/rollback transaction.
pub struct ActiveIntentModel {
pub:
	intent_id                 string
	intent_type               string
	stage                     string
	run_id                    i64
	run_attempt               int
	ordinal                   int
	input_fingerprint         string
	expected_canonical_head   string
	candidate_ref             string
	generation                i64
	resolved_inputs           ResolvedInputsModel
	expected_check_sources    []CheckSourceModel
	gate_runs                 []PersistedGateRunModel
	gate_trigger_refs         []string
	deadlines                 IntentDeadlinesModel
	infra_retry_count         int
	source_retry_count        int
	candidate_binding         CandidateBindingModel
	validation_subject        ValidationSubjectModel
	previous_last_known_good  ArtifactTupleModel
	bad_provisional           ArtifactTupleModel
	rollback_diff_fingerprint string
	rollback_provisional      CandidateBindingModel
}

// GreenVerdictProof contains the exact physical capsule and both authenticated gate runs.
pub struct GreenVerdictProof {
pub:
	expected_ledger_generation i64
	manifest                   AuthenticatedManifestModel
	native_capsule             AuthenticatedNativeValidationCapsule
	expected_check_sources     []CheckSourceModel
	native_gate                PersistedGateRunModel
	v_smoke_gate               PersistedGateRunModel
}

// RedVerdictProof authenticates the same immutable subject and complete physical capsule as green.
// A publisher failure may retain two green gates, but every other failure needs an explicit
// non-success gate or lane.
pub struct RedVerdictProof {
pub:
	expected_ledger_generation i64
	manifest                   AuthenticatedManifestModel
	native_capsule             AuthenticatedNativeValidationCapsule
	expected_check_sources     []CheckSourceModel
	native_gate                PersistedGateRunModel
	v_smoke_gate               PersistedGateRunModel
	failure_kind               string
}

// HeadObservationModel is an independently re-fetched canonical HEAD fact for exactly one CAS.
pub struct HeadObservationModel {
pub:
	target_id              string
	expected_generation    i64
	expected_previous_head string
	canonical_head         string
	subject_sha            string
	relationship           HeadRelationship
	observed_at            string
	operation_id           string
	evidence_digest        string
}

// AppliedOperationModel prevents replay of any retained operation, not just the last transition.
pub struct AppliedOperationModel {
pub:
	operation_id         string
	transition           string
	resulting_generation i64
}

// SourceRefetchModel persists the HEAD/source facts immediately used by a transition.
pub struct SourceRefetchModel {
pub:
	target_id               string
	expected_generation     i64
	expected_canonical_head string
	source_state_id         string
	source_state_generation i64
	resolution_operation_id string
	source_id               string
	source_repository       string
	requested_ref           string
	previous_sha            string
	resolved_sha            string
	resolved_tree           string
	status                  string
	failure_kind            string
	evidence_digest         string
	input_fingerprint       string
	checked_at              string
	operation_id            string
}

// TransitionContext supplies immutable facts; empty fields are never inferred by the machine.
pub struct TransitionContext {
pub:
	operation_id       string
	head_observation   HeadObservationModel
	intent             ActiveIntentModel
	candidate_binding  CandidateBindingModel
	validation_subject ValidationSubjectModel
	native_subject     NativeGateSubjectModel
	native_gate        NativeGateModel
	green_proof        GreenVerdictProof
	red_proof          RedVerdictProof
	check_sources      []CheckSourceModel
	source_state       SourceStateModel
	source_refetch     SourceRefetchModel
}

// TargetModel retains the durable tuple/state surface needed for Phase A decisions.
pub struct TargetModel {
pub mut:
	target_id                    string
	generation                   i64
	target_state                 TargetState
	publication_state            PublicationState
	bootstrap_required           bool
	canonical_observed_sha       string
	input_fingerprint            string
	artifact_fingerprint         string
	manifest_hash                string
	provenance_status            string
	affected_targets             []string
	resolved_inputs              ResolvedInputsModel
	last_known_good              ArtifactTupleModel
	provisional_published        ArtifactTupleModel
	active_intent                ActiveIntentModel
	incident_ids                 []string
	active_recovery_handoff_id   string
	active_native_subject        NativeGateSubjectModel
	active_subject_hash          string
	active_native_gate           NativeGateModel
	active_remediation_id        string
	post_validation_operation_id string
	remediation_check_sources    []CheckSourceModel
	last_source_refetch          SourceRefetchModel
	last_head_observation        HeadObservationModel
	last_native_validation       NativeValidationRecordModel
	applied_operations           []AppliedOperationModel
	last_operation_id            string
	last_transition              string
	manual_green_publications    int
}

// OperationIdentityInput is the complete injective identity material for one ledger write.
pub struct OperationIdentityInput {
pub:
	audience                string
	run_id                  i64
	run_attempt             int
	ordinal                 int
	cas_attempt             int
	subject_id              string
	transition              string
	expected_generation     i64
	expected_canonical_head string
	source_ref              string
	source_sha              string
	subject_fingerprint     string
	input_fingerprint       string
	artifact_fingerprint    string
	manifest_hash           string
	native_subject_hash     string
	intent_id               string
}

// initial_target_model constructs the only valid pre-bootstrap target state.
pub fn initial_target_model(target_id string, canonical_head string) TargetModel {
	return TargetModel{
		target_id:              target_id
		target_state:           .uninitialized
		publication_state:      .idle
		bootstrap_required:     true
		canonical_observed_sha: canonical_head
		affected_targets:       [target_id]
	}
}

// transition_target applies one fail-closed and idempotent target/publication transition.
pub fn transition_target(current TargetModel, event TransitionEvent,
	context TransitionContext) !TargetModel {
	validate_target_model(current)!
	if !is_lower_hex_64(context.operation_id) {
		return error('every target transition requires a deterministic operation ID')
	}
	event_name := event.str()
	applied := current.applied_operations.filter(it.operation_id == context.operation_id)
	if applied.len > 0 {
		if applied.len == 1 && applied[0].transition == event_name
			&& applied[0].resulting_generation == current.generation
			&& current.last_operation_id == context.operation_id {
			return current
		}
		return error('operation ID was already applied or collided with another target transition')
	}
	if current.applied_operations.len >= applied_operation_limit {
		return error('bounded applied-operation ledger is full and requires reviewed compaction')
	}
	mut next := current
	match event {
		.begin_bootstrap {
			if current.target_state !in [.uninitialized, .validating] || !current.bootstrap_required
				|| artifact_tuple_is_set(current.last_known_good)
				|| intent_is_set(current.active_intent) {
				return error('bootstrap can reserve only an unseeded target without an active intent')
			}
			validate_reserved_intent(context.intent, 'initial_adopt_current', current,
				context.validation_subject)!
			subject_hash := validate_transition_native_subject(current, context.native_subject,
				context.validation_subject, context.intent.intent_id, 'initial_adopt_current',

				current.generation + 1)!
			next.target_state = .validating
			next.publication_state = .candidate_pending
			next.active_intent = context.intent
			next.active_native_subject = context.native_subject
			next.active_subject_hash = subject_hash
			validate_transition_native_gate(context.native_gate, context.native_subject,

				current.generation + 1)!
			next.active_native_gate = context.native_gate
		}
		.bootstrap_green {
			if current.target_state != .validating || !current.bootstrap_required
				|| current.active_intent.intent_type != 'initial_adopt_current'
				|| current.active_intent.stage !in ['candidate_bound', 'checks_running', 'checks_green'] {
				return error('bootstrap green requires the bound initial adoption')
			}
			require_exact_head(current, context, current.active_intent.validation_subject.sha)!
			facts := validate_green_verdict(current, context.green_proof,
				current.active_intent.validation_subject, current.active_intent.intent_id,
				'initial_adopt_current')!
			next.last_native_validation = native_validation_record_from_facts(context.operation_id,
				event_name, current.generation + 1, 'green', facts,
				context.green_proof.native_gate, context.green_proof.v_smoke_gate)!
			next.target_state = .eligible
			next.publication_state = .idle
			next.bootstrap_required = false
			next.last_known_good =
				artifact_tuple_from_validation(current.active_intent.validation_subject)
			next.artifact_fingerprint = next.last_known_good.artifact_fingerprint
			next.manifest_hash = next.last_known_good.manifest_hash
			next.active_intent = ActiveIntentModel{}
			next.active_native_subject = NativeGateSubjectModel{}
			next.active_subject_hash = ''
			next.incident_ids = []
		}
		.bootstrap_red {
			if current.target_state != .validating || !current.bootstrap_required
				|| artifact_tuple_is_set(current.last_known_good) {
				return error('bootstrap red requires an unseeded bootstrap validation')
			}
			require_exact_head(current, context, current.active_intent.validation_subject.sha)!
			facts := validate_red_verdict(current, context.red_proof,
				current.active_intent.validation_subject, current.active_intent.intent_id,
				'initial_adopt_current', ['functional', 'infrastructure'])!
			next.last_native_validation = native_validation_record_from_facts(context.operation_id,
				event_name, current.generation + 1, context.red_proof.failure_kind, facts,
				context.red_proof.native_gate, context.red_proof.v_smoke_gate)!
			next.target_state = .quarantined
			next.publication_state = .idle
			next.active_intent = ActiveIntentModel{}
			next.active_native_subject = NativeGateSubjectModel{}
			next.active_subject_hash = ''
			next.incident_ids = require_incidents(current.incident_ids, context.operation_id)
		}
		.bootstrap_stale {
			if !current.bootstrap_required || current.target_state != .validating
				|| current.active_intent.intent_type != 'initial_adopt_current' {
				return error('only an active bootstrap can become stale')
			}
			next_head := require_changed_head(current, context,
				current.active_intent.validation_subject.sha)!
			next.active_intent = ActiveIntentModel{}
			next.publication_state = .idle
			next.active_native_subject = NativeGateSubjectModel{}
			next.active_subject_hash = ''
			next.canonical_observed_sha = next_head
			next.last_head_observation = context.head_observation
		}
		.actionable_defect {
			next.target_state = .quarantined
			next.incident_ids = require_incidents(current.incident_ids, context.operation_id)
			if current.publication_state == .candidate_pending {
				next.publication_state = .idle
				next.active_intent = ActiveIntentModel{}
				next.active_native_subject = NativeGateSubjectModel{}
				next.active_subject_hash = ''
			}
		}
		.ledger_invalid, .corruption {
			next.target_state = .unknown_blocked
			next.incident_ids = require_incidents(current.incident_ids, context.operation_id)
		}
		.ledger_repaired_with_blockers {
			if current.target_state != .unknown_blocked || current.incident_ids.len == 0 {
				return error('ledger repair with blockers requires restored incidents')
			}
			next.target_state = .quarantined
		}
		.ledger_repaired_without_blockers {
			if current.target_state != .unknown_blocked || current.incident_ids.len != 0 {
				return error('ledger repair without blockers requires a clean restored ledger')
			}
			next.target_state = .validating
		}
		.begin_remediation {
			if current.target_state !in [.quarantined, .validating] || current.incident_ids.len == 0 {
				return error('remediation requires a persisted blocking incident')
			}
			if intent_is_set(current.active_intent) || current.post_validation_operation_id != '' {
				return error('remediation cannot replace an active publication consumer')
			}
			validate_expected_check_bindings(context.check_sources)!
			validate_canonical_validation_subject(context.validation_subject, current)!
			subject_hash := validate_transition_native_subject(current, context.native_subject,
				context.validation_subject, context.operation_id, 'remediation',

				current.generation + 1)!
			next.target_state = .validating
			next.active_remediation_id = context.operation_id
			next.remediation_check_sources = context.check_sources
			next.active_native_subject = context.native_subject
			next.active_subject_hash = subject_hash
			validate_transition_native_gate(context.native_gate, context.native_subject,

				current.generation + 1)!
			next.active_native_gate = context.native_gate
		}
		.remediation_green {
			if current.target_state != .validating
				|| !artifact_tuple_is_set(current.last_known_good)
				|| !is_lower_hex_64(current.active_remediation_id) {
				return error('remediation green requires a seeded target under validation')
			}
			validate_canonical_validation_subject(context.validation_subject, current)!
			require_exact_head(current, context, context.validation_subject.sha)!
			facts := validate_green_verdict(current, context.green_proof,
				context.validation_subject, current.active_remediation_id, 'remediation')!
			next.last_native_validation = native_validation_record_from_facts(context.operation_id,
				event_name, current.generation + 1, 'green', facts,
				context.green_proof.native_gate, context.green_proof.v_smoke_gate)!
			next.target_state = .eligible
			next.publication_state = .idle
			next.incident_ids = []
			next.provisional_published = ArtifactTupleModel{}
			next.active_intent = ActiveIntentModel{}
			next.last_known_good = artifact_tuple_from_validation(context.validation_subject)
			next.input_fingerprint = context.validation_subject.input_fingerprint
			next.artifact_fingerprint = context.validation_subject.artifact_fingerprint
			next.manifest_hash = context.validation_subject.manifest_hash
			next.active_remediation_id = ''
			next.remediation_check_sources = []
			next.active_native_subject = NativeGateSubjectModel{}
			next.active_subject_hash = ''
			next.post_validation_operation_id = ''
			next.last_head_observation = context.head_observation
		}
		.remediation_red {
			if current.target_state != .validating
				|| !is_lower_hex_64(current.active_remediation_id) {
				return error('remediation red requires validation')
			}
			validate_canonical_validation_subject(context.validation_subject, current)!
			require_exact_head(current, context, context.validation_subject.sha)!
			facts := validate_red_verdict(current, context.red_proof, context.validation_subject,
				current.active_remediation_id, 'remediation', ['functional', 'infrastructure'])!
			next.last_native_validation = native_validation_record_from_facts(context.operation_id,
				event_name, current.generation + 1, context.red_proof.failure_kind, facts,
				context.red_proof.native_gate, context.red_proof.v_smoke_gate)!
			next.target_state = .quarantined
			next.incident_ids = require_incidents(current.incident_ids, context.operation_id)
			next.last_head_observation = context.head_observation
			next.active_remediation_id = ''
			next.remediation_check_sources = []
			next.active_native_subject = NativeGateSubjectModel{}
			next.active_subject_hash = ''
			next.post_validation_operation_id = ''
		}
		.validation_stale {
			if current.target_state != .validating {
				return error('only a validation can become stale')
			}
			next.canonical_observed_sha = require_changed_head(current, context,
				expected_active_validation_sha(current))!
			next.last_head_observation = context.head_observation
			next.target_state = if current.bootstrap_required {
				.uninitialized
			} else {
				.quarantined
			}
			next.publication_state = .idle
			next.active_intent = ActiveIntentModel{}
			next.active_remediation_id = ''
			next.remediation_check_sources = []
			next.active_native_subject = NativeGateSubjectModel{}
			next.active_subject_hash = ''
			next.post_validation_operation_id = ''
		}
		.reserve_publish {
			if !can_begin_normal_publication(current) || intent_is_set(current.active_intent) {
				return error('normal publication preflight rejected the target or active intent')
			}
			validate_reserved_intent(context.intent, 'publish', current, ValidationSubjectModel{})!
			next.publication_state = .candidate_pending
			next.active_intent = context.intent
		}
		.reserve_adopt_current {
			if !artifact_tuple_is_set(current.last_known_good)
				|| intent_is_set(current.active_intent)
				|| !is_lower_hex_40(context.validation_subject.sha) {
				return error('adopt-current requires a seeded target and no active intent')
			}
			validate_reserved_intent(context.intent, 'adopt-current', current,
				context.validation_subject)!
			subject_hash := validate_transition_native_subject(current, context.native_subject,
				context.validation_subject, context.intent.intent_id, 'adopt_current',

				current.generation + 1)!
			next.target_state = .validating
			next.publication_state = .candidate_pending
			next.active_intent = context.intent
			next.active_native_subject = context.native_subject
			next.active_subject_hash = subject_hash
			validate_transition_native_gate(context.native_gate, context.native_subject,

				current.generation + 1)!
			next.active_native_gate = context.native_gate
		}
		.start_build {
			if current.publication_state !in [.candidate_pending, .rollback_pending]
				|| current.active_intent.intent_type !in ['publish', 'rollback']
				|| current.active_intent.stage != 'intent_reserved'
				|| candidate_binding_is_set(current.active_intent.candidate_binding) {
				return error('only an unbound publish or rollback may enter building')
			}
			next.active_intent = intent_with_stage(current.active_intent, 'building')
		}
		.bind_candidate {
			if current.publication_state !in [.candidate_pending, .rollback_pending]
				|| !intent_is_set(current.active_intent)
				|| current.active_intent.stage !in ['intent_reserved', 'building', 'ref_unknown'] {
				return error('candidate binding requires a reserved recoverable intention')
			}
			if current.active_intent.intent_type in ['adopt-current', 'initial_adopt_current'] {
				if !validation_subject_equal(context.validation_subject,
					current.active_intent.validation_subject) {
					return error('adoption binding must preserve its validation subject')
				}
				next.active_intent = intent_with_stage(current.active_intent, 'candidate_bound')
			} else {
				validate_candidate_binding(context.candidate_binding,
					current.active_intent.expected_canonical_head)!
				if current.active_intent.intent_type == 'rollback' {
					validate_rollback_candidate_binding(context.candidate_binding,
						current.active_intent)!
				}
				expected_subject := validation_from_candidate(context.candidate_binding,
					current.active_intent.input_fingerprint, current.active_intent.candidate_ref)
				subject_hash := validate_transition_native_subject(current, context.native_subject,
					expected_subject, current.active_intent.intent_id,
					consumer_kind_for_intent(current.active_intent)!, current.generation + 1)!
				next.active_intent = intent_with_binding(current.active_intent,
					context.candidate_binding, 'candidate_bound')
				next.active_native_subject = context.native_subject
				next.active_subject_hash = subject_hash
				validate_transition_native_gate(context.native_gate, context.native_subject,

					current.generation + 1)!
				next.active_native_gate = context.native_gate
			}
		}
		.candidate_checks_green {
			if current.active_intent.stage !in ['candidate_bound', 'checks_running',
				'checks_waiting_source'] {
				return error('checks green requires a bound candidate or validation subject')
			}
			expected_subject := intent_validation_subject(current.active_intent)!
			facts := validate_green_verdict(current, context.green_proof, expected_subject,
				current.active_intent.intent_id, consumer_kind_for_intent(current.active_intent)!)!
			next.last_native_validation = native_validation_record_from_facts(context.operation_id,
				event_name, current.generation + 1, 'green', facts,
				context.green_proof.native_gate, context.green_proof.v_smoke_gate)!
			next.active_intent = intent_with_gate_proofs(current.active_intent,
				context.green_proof, 'checks_green')
		}
		.candidate_failed {
			if current.publication_state !in [.candidate_pending, .rollback_pending]
				|| current.active_intent.stage !in ['candidate_bound', 'checks_running', 'checks_waiting_source'] {
				return error('candidate failure requires candidate_pending')
			}
			expected_subject := intent_validation_subject(current.active_intent)!
			facts := validate_red_verdict(current, context.red_proof, expected_subject,
				current.active_intent.intent_id, consumer_kind_for_intent(current.active_intent)!, [
				'functional',
				'infrastructure',
			])!
			next.last_native_validation = native_validation_record_from_facts(context.operation_id,
				event_name, current.generation + 1, context.red_proof.failure_kind, facts,
				context.red_proof.native_gate, context.red_proof.v_smoke_gate)!
			next.target_state = .quarantined
			next.publication_state = if current.active_intent.intent_type == 'rollback' {
				.rollback_blocked
			} else {
				.idle
			}
			next.incident_ids = require_incidents(current.incident_ids, context.operation_id)
			next.active_intent = if current.active_intent.intent_type == 'rollback' {
				intent_with_failure_proofs(current.active_intent, context.red_proof, 'blocked')
			} else {
				ActiveIntentModel{}
			}
			if current.active_intent.intent_type != 'rollback' {
				next.active_native_subject = NativeGateSubjectModel{}
				next.active_subject_hash = ''
			}
		}
		.promotion_failed {
			if current.active_intent.stage != 'checks_green' {
				return error('promotion failure requires checks_green')
			}
			expected_subject := intent_validation_subject(current.active_intent)!
			validate_red_verdict(current, context.red_proof, expected_subject,
				current.active_intent.intent_id, consumer_kind_for_intent(current.active_intent)!, [
				'publisher',
			])!
			validate_preserved_publisher_validation(current)!
			next.target_state = .quarantined
			next.publication_state = if current.active_intent.intent_type == 'rollback' {
				.rollback_blocked
			} else {
				.promotion_blocked
			}
			next.incident_ids = require_incidents(current.incident_ids, context.operation_id)
			next.active_intent = intent_with_stage(current.active_intent, 'blocked')
		}
		.promotion_confirmed {
			if current.active_intent.stage != 'checks_green'
				|| current.active_intent.intent_type != 'publish'
				|| !candidate_binding_is_set(current.active_intent.candidate_binding)
				|| require_exact_head(current, context, current.active_intent.candidate_binding.sha)! != current.active_intent.candidate_binding.sha {
				return error('promotion confirmation requires the exact checked candidate at HEAD')
			}
			validate_checked_candidate_record(current)!
			next.canonical_observed_sha = context.head_observation.canonical_head
			next.last_head_observation = context.head_observation
			next.target_state = .validating
			next.publication_state = .post_publish_validating
			next.provisional_published = artifact_tuple_from_candidate(current.active_intent,
				current.input_fingerprint)
			post_subject := validation_from_artifact(next.provisional_published,
				canonical_ref(current.target_id))
			subject_hash := validate_transition_native_subject(current, context.native_subject,
				post_subject, context.operation_id, 'publish_post', current.generation + 1)!
			next.post_validation_operation_id = context.operation_id
			next.active_native_subject = context.native_subject
			next.active_subject_hash = subject_hash
			validate_transition_native_gate(context.native_gate, context.native_subject,

				current.generation + 1)!
			next.active_native_gate = context.native_gate
			next.active_intent = intent_with_stage(current.active_intent, 'post_checks_running')
		}
		.post_check_green {
			if current.publication_state !in [.post_publish_validating, .post_publish_blocked]
				|| !artifact_tuple_is_set(current.provisional_published)
				|| !artifact_tuple_is_set(current.last_known_good) {
				return error('post-check green requires the exact provisional HEAD and prior good tuple')
			}
			require_exact_head(current, context, current.provisional_published.sha)!
			facts := validate_green_verdict(current, context.green_proof, validation_from_artifact(current.provisional_published,
				canonical_ref(current.target_id)), current.post_validation_operation_id,
				'publish_post')!
			next.last_native_validation = native_validation_record_from_facts(context.operation_id,
				event_name, current.generation + 1, 'green', facts,
				context.green_proof.native_gate, context.green_proof.v_smoke_gate)!
			next.target_state = .eligible
			next.publication_state = .idle
			next.last_known_good = current.provisional_published
			next.provisional_published = ArtifactTupleModel{}
			next.active_intent = ActiveIntentModel{}
			next.incident_ids = []
			next.active_native_subject = NativeGateSubjectModel{}
			next.active_subject_hash = ''
			next.post_validation_operation_id = ''
			next.last_head_observation = context.head_observation
		}
		.post_check_red {
			if current.publication_state !in [.post_publish_validating, .post_publish_blocked]
				|| !artifact_tuple_is_set(current.provisional_published) {
				return error('post-check red requires the exact provisional subject')
			}
			facts := validate_red_verdict(current, context.red_proof, validation_from_artifact(current.provisional_published,
				canonical_ref(current.target_id)), current.post_validation_operation_id,
				'publish_post', [
				'functional',
			])!
			next.last_native_validation = native_validation_record_from_facts(context.operation_id,
				event_name, current.generation + 1, 'functional', facts,
				context.red_proof.native_gate, context.red_proof.v_smoke_gate)!
			observed_head := validate_head_observation(current, context,
				current.provisional_published.sha, [.exact_subject, .subject_ancestor])!
			next.target_state = .quarantined
			next.incident_ids = require_incidents(current.incident_ids, context.operation_id)
			next.post_validation_operation_id = ''
			if context.head_observation.relationship == .exact_subject
				&& observed_head == current.provisional_published.sha {
				validate_rollback_intent(context.intent, current)!
				next.publication_state = .rollback_pending
				next.active_intent = context.intent
				next.active_native_subject = NativeGateSubjectModel{}
				next.active_subject_hash = ''
			} else if context.head_observation.relationship == .subject_ancestor
				&& observed_head != current.provisional_published.sha {
				mut descendant_target := current
				descendant_target.canonical_observed_sha = observed_head
				validate_reserved_intent(context.intent, 'adopt-current', descendant_target,
					context.validation_subject)!
				subject_hash := validate_transition_native_subject(descendant_target,
					context.native_subject, context.validation_subject, context.intent.intent_id,
					'adopt_current', current.generation + 1)!
				next.publication_state = .candidate_pending
				next.active_intent = context.intent
				next.active_native_subject = context.native_subject
				next.active_subject_hash = subject_hash
				validate_transition_native_gate(context.native_gate, context.native_subject,

					current.generation + 1)!
				next.active_native_gate = context.native_gate
			} else {
				return error('red provisional cannot roll back an unrelated or unverified HEAD')
			}
			next.canonical_observed_sha = observed_head
			next.last_head_observation = context.head_observation
		}
		.post_check_infra_exhausted {
			if current.publication_state != .post_publish_validating {
				return error('post-check infrastructure exhaustion requires post validation')
			}
			facts := validate_red_verdict(current, context.red_proof, validation_from_artifact(current.provisional_published,
				canonical_ref(current.target_id)), current.post_validation_operation_id,
				'publish_post', [
				'infrastructure',
			])!
			next.last_native_validation = native_validation_record_from_facts(context.operation_id,
				event_name, current.generation + 1, 'infrastructure', facts,
				context.red_proof.native_gate, context.red_proof.v_smoke_gate)!
			require_exact_head(current, context, current.provisional_published.sha)!
			next.target_state = .quarantined
			next.publication_state = .post_publish_blocked
			next.incident_ids = require_incidents(current.incident_ids, context.operation_id)
			next.active_intent = intent_with_failure_proofs(current.active_intent,
				context.red_proof, 'blocked')
			next.last_head_observation = context.head_observation
		}
		.source_unreachable {
			validate_source_refetch(context.source_refetch, context.source_state, current,
				context.operation_id, 'unreachable')!
			next.last_source_refetch = context.source_refetch
			if current.active_intent.intent_type == 'adopt-current' {
				next.publication_state = .adopt_current_waiting_source
				next.active_intent = intent_with_stage(current.active_intent,
					'checks_waiting_source')
			} else if current.publication_state == .candidate_pending {
				next.active_intent = intent_with_stage(current.active_intent, if intent_subject_is_bound(current.active_intent) {
					'checks_waiting_source'
				} else {
					'build_waiting_source'
				})
			} else if current.publication_state == .post_publish_validating {
				next.publication_state = .post_publish_waiting_source
				next.active_intent = intent_with_stage(current.active_intent,
					'post_checks_waiting_source')
			} else if current.publication_state == .rollback_pending {
				next.publication_state = .rollback_waiting_source
			} else {
				return error('no persisted consumer can wait for source recovery')
			}
		}
		.source_restored {
			validate_source_refetch(context.source_refetch, context.source_state, current,
				context.operation_id, 'resolved')!
			if current.last_source_refetch.input_fingerprint != current.input_fingerprint
				|| current.last_source_refetch.source_id != context.source_refetch.source_id
				|| current.last_source_refetch.source_repository != context.source_refetch.source_repository
				|| current.last_source_refetch.requested_ref != context.source_refetch.requested_ref
				|| current.last_source_refetch.previous_sha != context.source_refetch.previous_sha {
				return error('source recovery cannot change the pending input fingerprint')
			}
			next.last_source_refetch = context.source_refetch
			if current.publication_state == .post_publish_waiting_source {
				next.publication_state = .post_publish_validating
				next.active_intent = intent_with_stage(current.active_intent, 'post_checks_running')
			} else if current.publication_state == .rollback_waiting_source {
				next.publication_state = .rollback_pending
			} else if current.publication_state == .adopt_current_waiting_source {
				next.publication_state = .candidate_pending
				next.active_intent = intent_with_stage(current.active_intent, 'checks_running')
			} else if current.publication_state == .candidate_pending {
				next.active_intent = intent_with_stage(current.active_intent, if intent_subject_is_bound(current.active_intent) {
					'checks_running'
				} else {
					'building'
				})
			} else {
				return error('source restoration has no exact waiting consumer')
			}
		}
		.rollback_promoted {
			if current.publication_state != .rollback_pending
				|| current.active_intent.intent_type != 'rollback'
				|| current.active_intent.stage != 'checks_green'
				|| !candidate_binding_is_set(current.active_intent.candidate_binding)
				|| current.canonical_observed_sha != current.active_intent.bad_provisional.sha {
				return error('rollback promotion requires the exact promoted revert at HEAD')
			}
			validate_checked_candidate_record(current)!
			require_exact_head(current, context, current.active_intent.candidate_binding.sha)!
			next.canonical_observed_sha = context.head_observation.canonical_head
			next.last_head_observation = context.head_observation
			next.active_intent = intent_with_rollback_provisional(current.active_intent,
				current.active_intent.candidate_binding, 'post_checks_running')
			post_subject := validation_from_candidate(current.active_intent.candidate_binding,
				current.input_fingerprint, canonical_ref(current.target_id))
			subject_hash := validate_transition_native_subject(current, context.native_subject,
				post_subject, context.operation_id, 'rollback_post', current.generation + 1)!
			next.post_validation_operation_id = context.operation_id
			next.active_native_subject = context.native_subject
			next.active_subject_hash = subject_hash
			validate_transition_native_gate(context.native_gate, context.native_subject,

				current.generation + 1)!
			next.active_native_gate = context.native_gate
		}
		.rollback_post_green {
			if current.publication_state != .rollback_pending
				|| current.active_intent.intent_type != 'rollback'
				|| current.active_intent.stage != 'post_checks_running'
				|| !candidate_binding_is_set(current.active_intent.rollback_provisional) {
				return error('rollback post-check green requires the exact promoted revert')
			}
			require_exact_head(current, context, current.active_intent.rollback_provisional.sha)!
			facts := validate_green_verdict(current, context.green_proof, validation_from_candidate(current.active_intent.rollback_provisional,
				current.input_fingerprint, canonical_ref(current.target_id)),
				current.post_validation_operation_id, 'rollback_post')!
			next.last_native_validation = native_validation_record_from_facts(context.operation_id,
				event_name, current.generation + 1, 'green', facts,
				context.green_proof.native_gate, context.green_proof.v_smoke_gate)!
			next.target_state = .quarantined
			next.publication_state = .restored_last_known_good
			next.provisional_published = ArtifactTupleModel{}
			next.last_known_good = artifact_tuple_from_binding(current.active_intent.rollback_provisional,
				current.input_fingerprint)
			next.active_intent = ActiveIntentModel{}
			next.last_head_observation = context.head_observation
			next.active_native_subject = NativeGateSubjectModel{}
			next.active_subject_hash = ''
			next.post_validation_operation_id = ''
		}
		.rollback_failed {
			if current.publication_state !in [.rollback_pending, .rollback_waiting_source] {
				return error('rollback failure requires a rollback state')
			}
			expected_subject := rollback_failure_subject(current)!
			facts := validate_red_verdict(current, context.red_proof, expected_subject,
				rollback_failure_consumer_id(current)!, rollback_failure_consumer_kind(current)!, [
				'functional',
				'infrastructure',
				'publisher',
			])!
			if context.red_proof.failure_kind == 'publisher' {
				validate_preserved_publisher_validation(current)!
			} else {
				next.last_native_validation = native_validation_record_from_facts(context.operation_id,
					event_name, current.generation + 1, context.red_proof.failure_kind, facts,
					context.red_proof.native_gate, context.red_proof.v_smoke_gate)!
			}
			next.target_state = .quarantined
			next.publication_state = .rollback_blocked
			next.incident_ids = require_incidents(current.incident_ids, context.operation_id)
			next.active_intent = if context.red_proof.failure_kind == 'publisher' {
				intent_with_stage(current.active_intent, 'blocked')
			} else {
				intent_with_failure_proofs(current.active_intent, context.red_proof, 'blocked')
			}
		}
	}
	if next.active_subject_hash == '' {
		next.active_native_gate = NativeGateModel{}
	}
	next.generation = current.generation + 1
	if native_gate_is_set(next.active_native_gate) {
		if next.active_native_gate.expected_ledger_generation == current.generation {
			next.active_native_gate = advance_gate_ledger_generation(next.active_native_gate,
				current.generation, next.generation)!
		} else if next.active_native_gate.expected_ledger_generation != next.generation {
			return error('native gate execution does not follow the target CAS generation')
		}
	}
	next.last_operation_id = context.operation_id
	next.last_transition = event_name
	next.applied_operations << AppliedOperationModel{
		operation_id:         context.operation_id
		transition:           event_name
		resulting_generation: next.generation
	}
	validate_target_model(next)!
	return next
}

// can_begin_normal_publication enforces the complete normal publication preflight.
pub fn can_begin_normal_publication(state TargetModel) bool {
	return state.target_state == .eligible && state.publication_state == .idle
		&& !state.bootstrap_required && artifact_tuple_is_set(state.last_known_good)
		&& !artifact_tuple_is_set(state.provisional_published)
		&& !intent_is_set(state.active_intent) && state.incident_ids.len == 0
		&& state.active_recovery_handoff_id == '' && state.active_subject_hash == ''
		&& state.canonical_observed_sha == state.last_known_good.sha
		&& state.input_fingerprint == state.last_known_good.input_fingerprint
		&& state.artifact_fingerprint == state.last_known_good.artifact_fingerprint
		&& state.manifest_hash == state.last_known_good.manifest_hash
		&& resolved_inputs_is_set(state.resolved_inputs)
		&& state.provenance_status in ['complete', 'opaque-accepted']
		&& native_validation_allows_publication(state)
}

// cycle_is_noop requires the complete remote tuple and all durable blockers to remain identical.
pub fn cycle_is_noop(state TargetModel, observed ArtifactTupleModel,
	canonical_head string) bool {
	return can_begin_normal_publication(state) && canonical_head == state.last_known_good.sha
		&& artifact_tuple_equal(observed, state.last_known_good)
}

// validate_target_model rejects impossible durable combinations.
pub fn validate_target_model(state TargetModel) ! {
	if state.target_id !in managed_target_ids || !is_lower_hex_40(state.canonical_observed_sha)
		|| state.affected_targets != [state.target_id] {
		return error('target identity, canonical HEAD, or affected target graph is invalid')
	}
	if state.generation < 0 || state.manual_green_publications < 0 {
		return error('target generation or manual publication count is invalid')
	}
	if state.input_fingerprint != '' && !is_lower_hex_64(state.input_fingerprint) {
		return error('active input fingerprint is invalid')
	}
	if state.artifact_fingerprint != '' && !is_lower_hex_64(state.artifact_fingerprint) {
		return error('active artifact fingerprint is invalid')
	}
	if state.manifest_hash != '' && !is_lower_hex_64(state.manifest_hash) {
		return error('active manifest hash is invalid')
	}
	if state.provenance_status != ''
		&& state.provenance_status !in ['complete', 'opaque-accepted', 'incomplete'] {
		return error('target provenance status is outside the closed contract')
	}
	resolved_inputs_present := resolved_inputs_is_set(state.resolved_inputs)
	if !state.bootstrap_required && !resolved_inputs_present {
		return error('seeded target must retain complete resolved inputs')
	}
	if !resolved_inputs_present && (state.active_remediation_id != ''
		|| state.active_subject_hash != '' || state.target_state == .eligible) {
		return error('a durable validation consumer requires complete resolved inputs')
	}
	if state.input_fingerprint == '' {
		if resolved_inputs_present || intent_is_set(state.active_intent) {
			return error('an unresolved target cannot retain resolved inputs or an active intention')
		}
	} else {
		if !resolved_inputs_present {
			return error('a resolved target fingerprint requires complete resolved inputs')
		}
		validate_resolved_inputs(state.resolved_inputs)!
	}
	if artifact_tuple_is_set(state.last_known_good) {
		validate_artifact_tuple(state.last_known_good)!
	}
	if artifact_tuple_is_set(state.provisional_published) {
		validate_artifact_tuple(state.provisional_published)!
		if !artifact_tuple_is_set(state.last_known_good) {
			return error('provisional publication cannot replace an absent prior good tuple')
		}
	}
	if state.bootstrap_required && artifact_tuple_is_set(state.last_known_good) {
		return error('bootstrap target cannot already have a last-known-good tuple')
	}
	if !state.bootstrap_required && !artifact_tuple_is_set(state.last_known_good) {
		return error('seeded target must retain a complete last-known-good tuple')
	}
	if state.target_state == .eligible
		&& (state.bootstrap_required || !artifact_tuple_is_set(state.last_known_good)
		|| state.incident_ids.len > 0 || state.provenance_status == 'incomplete') {
		return error('eligible target violates seed, provenance, or blocker invariants')
	}
	if intent_is_set(state.active_intent) {
		validate_active_intent(state.active_intent)!
		if state.active_intent.resolved_inputs != state.resolved_inputs
			|| state.active_intent.input_fingerprint != state.input_fingerprint {
			return error('active intention resolved inputs differ from the target root')
		}
	} else if state.publication_state in [.candidate_pending, .promotion_blocked,
		.post_publish_validating, .post_publish_waiting_source, .post_publish_blocked,
		.rollback_pending, .rollback_waiting_source, .rollback_blocked] {
		return error('active publication state requires a persisted intention')
	}
	if state.active_subject_hash == '' {
		if native_subject_is_set(state.active_native_subject) {
			return error('active native subject exists without its canonical hash')
		}
		if native_gate_is_set(state.active_native_gate) {
			return error('native gate execution exists without its immutable active subject')
		}
	} else {
		validate_native_gate_subject(state.active_native_subject)!
		if state.active_subject_hash != native_gate_subject_hash(state.active_native_subject)!
			|| state.active_native_subject.target_id != state.target_id
			|| state.active_native_subject.subject_generation > state.generation {
			return error('active native subject/hash/target/generation binding is invalid')
		}
		if !native_gate_is_set(state.active_native_gate) {
			return error('active native subject lacks its durable gate execution')
		}
		validate_native_gate(state.active_native_gate)!
		if state.active_native_gate.subject != state.active_native_subject
			|| state.active_native_gate.subject_hash != state.active_subject_hash
			|| state.active_native_gate.expected_ledger_generation != state.generation {
			return error('persisted native gate execution differs from target subject or generation')
		}
	}
	current_intent_projection := intent_is_set(state.active_intent)
		&& state.active_subject_hash != ''
		&& state.active_native_subject.consumer_kind !in ['publish_post', 'rollback_post']
		&& state.active_native_subject.consumer_id == state.active_intent.intent_id
	if current_intent_projection
		&& state.active_intent.gate_runs.any(it.subject_hash != state.active_subject_hash) {
		return error('collected gate proof is orphaned from the active native subject')
	}
	post_subject_active := state.active_native_subject.consumer_kind in [
		'publish_post',
		'rollback_post',
	]
	if state.post_validation_operation_id != '' {
		if !is_lower_hex_64(state.post_validation_operation_id) || !post_subject_active
			|| !intent_is_set(state.active_intent)
			|| state.active_intent.stage !in ['post_checks_running', 'post_checks_waiting_source', 'blocked']
			|| state.publication_state !in [.post_publish_validating, .post_publish_waiting_source, .post_publish_blocked, .rollback_pending, .rollback_waiting_source, .rollback_blocked] {
			return error('post-validation operation is invalid or outlives its exact consumer')
		}
	} else if post_subject_active {
		return error('post-validation subject lacks its durable operation identity')
	}
	subject_required := state.active_remediation_id != ''
		|| state.post_validation_operation_id != ''
		|| state.active_recovery_handoff_id != ''
		|| (intent_is_set(state.active_intent)
		&& (state.active_intent.intent_type in ['adopt-current', 'initial_adopt_current']
		|| state.active_intent.stage in ['candidate_bound', 'checks_running', 'checks_waiting_source', 'checks_green', 'promotion_unknown', 'post_checks_running', 'post_checks_waiting_source', 'completed', 'blocked']))
	if subject_required != (state.active_subject_hash != '') {
		return error('active native subject presence does not match its durable consumer stage')
	}
	if state.active_subject_hash != '' && (is_lower_hex_64(state.active_remediation_id)
		|| is_lower_hex_64(state.post_validation_operation_id)
		|| intent_is_set(state.active_intent)) {
		expected_consumer := if is_lower_hex_64(state.active_remediation_id) {
			state.active_remediation_id
		} else if is_lower_hex_64(state.post_validation_operation_id) {
			state.post_validation_operation_id
		} else {
			state.active_intent.intent_id
		}
		if state.active_native_subject.consumer_id != expected_consumer
			|| state.active_native_subject.intent_or_operation_id != expected_consumer {
			return error('active native subject replaced its durable semantic consumer identity')
		}
	}
	if state.publication_state in [.rollback_pending, .rollback_waiting_source, .rollback_blocked]
		&& !artifact_tuple_is_set(state.last_known_good) {
		return error('rollback is forbidden before the initial seed')
	}
	if state.last_operation_id != ''
		&& (!is_lower_hex_64(state.last_operation_id) || state.last_transition == '') {
		return error('last transition identity is incomplete')
	}
	if state.applied_operations.len > applied_operation_limit {
		return error('applied-operation ledger exceeds its fixed bound')
	}
	mut operation_ids := []string{}
	mut previous_generation := i64(0)
	for operation in state.applied_operations {
		if !is_lower_hex_64(operation.operation_id) || operation.transition == ''
			|| operation.resulting_generation <= 0
			|| operation.resulting_generation > state.generation
			|| operation.operation_id in operation_ids || (previous_generation > 0
			&& operation.resulting_generation != previous_generation + 1) {
			return error('applied-operation ledger is invalid, duplicated, or non-contiguous')
		}
		operation_ids << operation.operation_id
		previous_generation = operation.resulting_generation
	}
	if state.applied_operations.len > 0 {
		last := state.applied_operations.last()
		if last.operation_id != state.last_operation_id || last.transition != state.last_transition
			|| last.resulting_generation != state.generation {
			return error('last-operation projection differs from the bounded operation ledger')
		}
	}
	record_is_set := native_validation_record_is_set(state.last_native_validation)
	if record_is_set {
		validate_native_validation_record_for_target(state)!
	} else if state.target_state == .eligible
		|| (intent_is_set(state.active_intent) && state.active_intent.stage == 'checks_green') {
		return error('publishable or checked target lacks its durable native validation record')
	}
	if intent_is_set(state.active_intent) && state.active_intent.stage == 'checks_green' {
		validate_checked_candidate_record(state)!
	} else if intent_is_set(state.active_intent) && state.active_intent.stage == 'blocked'
		&& record_is_set {
		publisher_lane_is_exact := (state.publication_state == .promotion_blocked
			&& state.active_intent.intent_type == 'publish'
			&& state.active_native_subject.consumer_kind == 'publish_candidate')
			|| (state.publication_state == .rollback_blocked
			&& state.active_intent.intent_type == 'rollback'
			&& state.active_native_subject.consumer_kind in ['rollback_candidate', 'rollback_post'])
		publisher_preserved := state.last_native_validation.transition == 'candidate_checks_green'
			&& state.last_native_validation.verdict == 'green' && publisher_lane_is_exact
		blocked_red := state.last_native_validation.transition in ['candidate_failed',
			'post_check_infra_exhausted', 'rollback_failed']
		if publisher_preserved {
			validate_preserved_publisher_validation(state)!
		} else if blocked_red {
			validate_blocked_red_validation_record(state)!
		} else {
			return error('blocked target native validation is outside the closed publisher-preserved or red transition classes')
		}
	}
	if state.active_remediation_id != '' && (!is_lower_hex_64(state.active_remediation_id)
		|| state.target_state != .validating || intent_is_set(state.active_intent)
		|| state.remediation_check_sources.len != 2) {
		return error('active remediation identity is invalid or overlaps a publication intent')
	}
	if state.active_remediation_id != '' {
		validate_expected_check_bindings(state.remediation_check_sources)!
	}
	if state.active_remediation_id == '' && state.remediation_check_sources.len != 0 {
		return error('remediation check sources outlive their durable remediation identity')
	}
	if state.target_state == .validating && state.incident_ids.len > 0
		&& !intent_is_set(state.active_intent) && !is_lower_hex_64(state.active_remediation_id) {
		return error('incident remediation validation lacks its durable operation identity')
	}
}

fn validate_native_validation_record_for_target(state TargetModel) !NativeValidationCapsuleFacts {
	record := state.last_native_validation
	facts := native_validation_record_facts(record)!
	operations := state.applied_operations.filter(it.operation_id == record.operation_id
		&& it.transition == record.transition
		&& it.resulting_generation == record.resulting_generation)
	if operations.len != 1 || record.resulting_generation > state.generation
		|| facts.subject.target_id != state.target_id
		|| facts.subject.subject_generation > record.resulting_generation
		|| facts.subject_hash != native_gate_subject_hash(facts.subject)!
		|| facts.producer != state.resolved_inputs.producer_toolchain {
		return error('last native validation is not joined to its target, CAS operation, or producer')
	}
	gates_outcome := validate_native_validation_gate_pair(record.native_gate, record.v_smoke_gate,
		facts.subject, facts.subject_hash, state.resolved_inputs.v_source_sha)!
	if record.native_gate.run_id != facts.selected_run.run_id
		|| record.native_gate.run_attempt != facts.selected_run.run_attempt
		|| record.native_gate.check_suite_id != facts.selected_run.check_suite_id
		|| record.native_gate.output_digest != facts.matrix_digest {
		return error('last native validation differs from its selected native winner or gate sources')
	}
	outcome := combine_native_outcomes(facts.matrix_outcome, gates_outcome)
	expected_verdict := match outcome {
		.green { 'green' }
		.functional { 'functional' }
		.infrastructure { 'infrastructure' }
	}
	if record.verdict != expected_verdict {
		return error('last native validation verdict differs from its authenticated matrix and gates')
	}
	return facts
}

fn validate_native_validation_gate_pair(native_gate PersistedGateRunModel,
	v_smoke_gate PersistedGateRunModel, subject NativeGateSubjectModel, subject_hash string,
	v_source_sha string) !NativeLaneOutcome {
	native_source := gate_check_source(native_gate)
	smoke_source := gate_check_source(v_smoke_gate)
	validate_expected_check_bindings([native_source, smoke_source])!
	native_outcome := validate_gate_proof(native_gate, native_source, native_source, subject,
		subject_hash, v_source_sha)!
	smoke_outcome := validate_gate_proof(v_smoke_gate, smoke_source, native_source, subject,
		subject_hash, v_source_sha)!
	if native_gate.integration_id == v_smoke_gate.integration_id
		|| native_gate.run_id == v_smoke_gate.run_id {
		return error('native and V smoke proofs must come from distinct authenticated sources')
	}
	return combine_native_outcomes(native_outcome, smoke_outcome)
}

fn gate_check_source(gate PersistedGateRunModel) CheckSourceModel {
	return CheckSourceModel{
		name:           gate.check_name
		repository:     gate.repository
		integration_id: gate.integration_id
		workflow_id:    gate.workflow_id
		workflow_path:  gate.workflow_path
		event:          gate.event
	}
}

fn native_validation_subject_matches_validation(subject NativeGateSubjectModel,
	expected ValidationSubjectModel, consumer_id string, consumer_kind string) bool {
	return validation_subject_is_set(expected) && subject.consumer_id == consumer_id
		&& subject.intent_or_operation_id == consumer_id && subject.consumer_kind == consumer_kind
		&& subject.sha == expected.sha && subject.tree == expected.tree
		&& subject.original_ref == expected.candidate_ref
		&& subject.input_fingerprint == expected.input_fingerprint
		&& subject.artifact_fingerprint == expected.artifact_fingerprint
		&& subject.manifest_hash == expected.manifest_hash && subject.digests == expected.digests
}

fn validate_candidate_validation_record(state TargetModel,
	require_active_winner bool) !NativeValidationCapsuleFacts {
	if !intent_is_set(state.active_intent) {
		return error('checked candidate lacks its durable active intention')
	}
	facts := validate_native_validation_record_for_target(state)!
	record := state.last_native_validation
	expected := intent_validation_subject(state.active_intent)!
	expected_kind := consumer_kind_for_intent(state.active_intent)!
	if record.transition != 'candidate_checks_green' || record.verdict != 'green'
		|| !native_validation_subject_matches_validation(facts.subject, expected, state.active_intent.intent_id, expected_kind)
		|| state.active_intent.gate_runs != [record.native_gate, record.v_smoke_gate] {
		return error('checked candidate differs from its durable native validation record')
	}
	if require_active_winner {
		if facts.subject != state.active_native_subject
			|| facts.subject_hash != state.active_subject_hash {
			return error('checked candidate record differs from its active native subject')
		}
		validate_selected_native_gate_proof(state, record.native_gate, facts)!
	}
	return facts
}

fn validate_checked_candidate_record(state TargetModel) ! {
	if state.active_intent.stage != 'checks_green' {
		return error('checked candidate record requires the terminal checks_green stage')
	}
	validate_candidate_validation_record(state, true)!
}

fn validate_preserved_publisher_validation(state TargetModel) ! {
	require_active_winner := state.active_native_subject.consumer_kind !in [
		'publish_post',
		'rollback_post',
	]
	validate_candidate_validation_record(state, require_active_winner)!
}

fn validate_blocked_red_validation_record(state TargetModel) ! {
	record := state.last_native_validation
	facts := validate_native_validation_record_for_target(state)!
	branch_is_exact := match record.transition {
		'candidate_failed' {
			state.publication_state == .rollback_blocked
				&& state.active_intent.intent_type == 'rollback'
				&& native_validation_subject_matches_validation(facts.subject, intent_validation_subject(state.active_intent)!, state.active_intent.intent_id, 'rollback_candidate')
		}
		'post_check_infra_exhausted' {
			state.publication_state == .post_publish_blocked
				&& state.active_intent.intent_type == 'publish'
				&& artifact_tuple_is_set(state.provisional_published)
				&& is_lower_hex_64(state.post_validation_operation_id)
				&& native_validation_subject_matches_validation(facts.subject, validation_from_artifact(state.provisional_published, canonical_ref(state.target_id)), state.post_validation_operation_id, 'publish_post')
		}
		'rollback_failed' {
			state.publication_state == .rollback_blocked
				&& state.active_intent.intent_type == 'rollback'
				&& record.verdict in ['functional', 'infrastructure']
				&& native_validation_subject_matches_validation(facts.subject, rollback_failure_subject(state)!, rollback_failure_consumer_id(state)!, rollback_failure_consumer_kind(state)!)
		}
		else {
			false
		}
	}
	if !branch_is_exact {
		return error('blocked red validation does not match its exact transition owner')
	}
	if state.active_intent.gate_runs != [record.native_gate, record.v_smoke_gate]
		|| facts.subject != state.active_native_subject
		|| facts.subject_hash != state.active_subject_hash {
		return error('blocked red validation differs from its active subject and two gate runs')
	}
	validate_selected_native_gate_proof(state, record.native_gate, facts)!
}

fn native_validation_allows_publication(state TargetModel) bool {
	facts := validate_native_validation_record_for_target(state) or { return false }
	record := state.last_native_validation
	if record.verdict != 'green'
		|| record.transition !in ['bootstrap_green', 'remediation_green', 'post_check_green']
		|| facts.subject.target_id != state.target_id {
		return false
	}
	good := state.last_known_good
	return facts.subject.sha == good.sha && facts.subject.tree == good.tree
		&& facts.subject.input_fingerprint == good.input_fingerprint
		&& facts.subject.artifact_fingerprint == good.artifact_fingerprint
		&& facts.subject.manifest_hash == good.manifest_hash
		&& facts.subject.digests == good.digests
}

// deterministic_intent_id derives an intention before candidate outputs exist.
pub fn deterministic_intent_id(audience string, target_id string, intent_type string,
	run_id i64, run_attempt int, ordinal int, input_fingerprint string,
	expected_canonical_head string) !string {
	if audience == '' || target_id !in managed_target_ids
		|| intent_type !in ['publish', 'rollback', 'adopt-current', 'initial_adopt_current']
		|| run_id <= 0 || run_attempt <= 0 || ordinal < 0 || !is_lower_hex_64(input_fingerprint)
		|| !is_lower_hex_40(expected_canonical_head) {
		return error('intent identity material is incomplete or outside the closed contract')
	}
	material := [audience, target_id, intent_type, run_id.str(),
		run_attempt.str(), ordinal.str(), input_fingerprint, expected_canonical_head].join('\x1f')
	return sha256.sum256(material.bytes()).hex()
}

// deterministic_operation_id includes the canonical/source HEAD and every immutable subject hash.
pub fn deterministic_operation_id(input OperationIdentityInput) !string {
	if input.audience == '' || input.run_id <= 0 || input.run_attempt <= 0
		|| input.ordinal < 0 || input.cas_attempt < 1 || input.cas_attempt > 3
		|| !safe_path_segment(input.subject_id)
		|| !safe_path_segment(input.transition) || input.expected_generation < 0
		|| !is_lower_hex_40(input.expected_canonical_head) || input.source_ref == ''
		|| !is_lower_hex_40(input.source_sha)
		|| !is_lower_hex_64(input.subject_fingerprint)
		|| !is_lower_hex_64(input.input_fingerprint)
		|| !is_lower_hex_64(input.artifact_fingerprint)
		|| !is_lower_hex_64(input.manifest_hash)
		|| !is_lower_hex_64(input.native_subject_hash)
		|| (input.intent_id != '' && !is_lower_hex_64(input.intent_id)) {
		return error('operation identity material is incomplete or outside the closed contract')
	}
	material := [input.audience, input.run_id.str(), input.run_attempt.str(),
		input.ordinal.str(), input.cas_attempt.str(), input.subject_id, input.transition,
		input.expected_generation.str(), input.expected_canonical_head, input.source_ref, input.source_sha,
		input.subject_fingerprint, input.input_fingerprint, input.artifact_fingerprint, input.manifest_hash,
		input.native_subject_hash, input.intent_id].join('\x1f')
	return sha256.sum256(material.bytes()).hex()
}

// target_state_path returns the sole target-ledger location for a managed target.
pub fn target_state_path(target_id string) !string {
	if target_id !in managed_target_ids {
		return error('unknown managed target')
	}
	return 'targets/${target_id}.json'
}

// source_state_path returns the fixed state location for an allowlisted source identity.
pub fn source_state_path(source_id string) !string {
	return match source_id {
		'tinycc-mob' { 'sources/tinycc-mob.json' }
		'bdwgc-master' { 'sources/bdwgc-master.json' }
		'libatomic_ops-master' { 'sources/libatomic_ops-master.json' }
		else { error('unknown source state identity') }
	}
}

// evidence_path derives an injective, append-only proof location from immutable identity fields.
pub fn evidence_path(year int, month int, run_id i64, run_attempt int, subject_id string,
	operation_id string, generation i64, transition string, subject_fingerprint string) !string {
	if year < 2020 || month < 1 || month > 12 || run_id <= 0 || run_attempt <= 0
		|| !safe_path_segment(subject_id) || !is_lower_hex_64(operation_id) || generation < 0
		|| !safe_path_segment(transition) || !is_lower_hex_64(subject_fingerprint) {
		return error('evidence path identity is invalid')
	}
	return
		'evidence/${year:04d}/${month:02d}/${run_id}/${run_attempt}/${subject_id}/${operation_id}/' +
		'${generation}-${transition}-${subject_fingerprint}.json'
}

fn validate_reserved_intent(intent ActiveIntentModel, expected_type string, target TargetModel,
	validation ValidationSubjectModel) ! {
	validate_active_intent(intent)!
	if intent.intent_type != expected_type || intent.stage != 'intent_reserved'
		|| intent.expected_canonical_head != target.canonical_observed_sha
		|| intent.input_fingerprint != target.input_fingerprint
		|| intent.resolved_inputs != target.resolved_inputs
		|| intent.generation != target.generation
		|| intent.candidate_ref != 'tccbin-candidate/${target.target_id}/${intent.intent_id}'
		|| candidate_binding_is_set(intent.candidate_binding) {
		return error('reserved intention does not match the target generation and inputs')
	}
	if expected_type in ['adopt-current', 'initial_adopt_current'] {
		if !validation_subject_equal(intent.validation_subject, validation)
			|| validation.sha != target.canonical_observed_sha {
			return error('adoption reservation must bind the exact existing canonical HEAD')
		}
	} else if validation_subject_is_set(intent.validation_subject) {
		return error('publish and rollback reservations cannot carry an adoption subject')
	}
	if expected_type == 'initial_adopt_current' {
		if artifact_tuple_is_set(intent.previous_last_known_good) {
			return error('initial adoption cannot invent a previous last-known-good tuple')
		}
	} else if !artifact_tuple_equal(intent.previous_last_known_good, target.last_known_good) {
		return error('non-bootstrap intention must snapshot the exact prior good tuple')
	}
}

fn validate_rollback_intent(intent ActiveIntentModel, target TargetModel) ! {
	validate_reserved_intent(intent, 'rollback', target, ValidationSubjectModel{})!
	if !artifact_tuple_equal(intent.bad_provisional, target.provisional_published)
		|| intent.expected_canonical_head != target.provisional_published.sha
		|| !is_lower_hex_64(intent.rollback_diff_fingerprint)
		|| candidate_binding_is_set(intent.rollback_provisional) {
		return error('rollback intention does not bind the bad provisional and exact revert diff')
	}
}

fn validate_active_intent(intent ActiveIntentModel) ! {
	if !is_lower_hex_64(intent.intent_id)
		|| intent.intent_type !in ['publish', 'rollback', 'adopt-current', 'initial_adopt_current']
		|| intent.stage !in ['intent_reserved', 'building', 'build_waiting_source', 'ref_unknown', 'candidate_bound', 'checks_running', 'checks_waiting_source', 'checks_green', 'promotion_unknown', 'post_checks_running', 'post_checks_waiting_source', 'completed', 'aborted', 'superseded', 'blocked']
		|| intent.run_id <= 0 || intent.run_attempt <= 0 || intent.ordinal < 0
		|| !is_lower_hex_64(intent.input_fingerprint)
		|| !is_lower_hex_40(intent.expected_canonical_head) || intent.generation < 0
		|| !candidate_ref_is_exact(intent.candidate_ref, intent.intent_id)
		|| intent.infra_retry_count < 0 || intent.infra_retry_count > 1
		|| intent.source_retry_count < 0 {
		return error('active intention identity, stage, ref, or counters are invalid')
	}
	validate_resolved_inputs(intent.resolved_inputs)!
	validate_expected_check_bindings(intent.expected_check_sources)!
	early_stages := ['intent_reserved', 'building', 'build_waiting_source', 'ref_unknown',
		'candidate_bound']
	collecting_stages := ['checks_running', 'checks_waiting_source', 'aborted', 'superseded']
	proof_stages := ['checks_green', 'promotion_unknown', 'post_checks_running',
		'post_checks_waiting_source', 'completed', 'blocked']
	if intent.stage in early_stages {
		if intent.gate_runs.len != 0 {
			return error('pre-check intention cannot retain collected terminal gate proofs')
		}
	} else if intent.stage in collecting_stages {
		if intent.gate_runs.len > 2 || (intent.gate_runs.len == 2
			&& (intent.gate_runs[0].check_name != 'tccbin-candidate-gate'
			|| intent.gate_runs[1].check_name != 'v-candidate-smoke')) {
			return error('collecting intention permits at most the ordered native and V smoke proofs')
		}
		native_source := intent.expected_check_sources.filter(it.name == 'tccbin-candidate-gate')[0]
		smoke_source := intent.expected_check_sources.filter(it.name == 'v-candidate-smoke')[0]
		mut observed_names := []string{}
		mut subject_hash := ''
		for proof in intent.gate_runs {
			if proof.check_name in observed_names {
				return error('collecting intention cannot duplicate a terminal gate kind')
			}
			observed_names << proof.check_name
			if proof.check_name == 'tccbin-candidate-gate' {
				validate_persisted_gate_run_shape(proof, native_source, native_source)!
			} else if proof.check_name == 'v-candidate-smoke' {
				validate_persisted_gate_run_shape(proof, smoke_source, native_source)!
			} else {
				return error('collecting intention contains an unknown terminal gate kind')
			}
			if subject_hash == '' {
				subject_hash = proof.subject_hash
			} else if proof.subject_hash != subject_hash {
				return error('persisted gate proofs do not retain one immutable subject hash')
			}
		}
	} else if intent.stage in proof_stages {
		if intent.gate_runs.len != 2 || intent.gate_runs[0].check_name != 'tccbin-candidate-gate'
			|| intent.gate_runs[1].check_name != 'v-candidate-smoke' {
			return error('terminal intention requires exactly the ordered native and V smoke proofs')
		}
		native_source := intent.expected_check_sources.filter(it.name == 'tccbin-candidate-gate')[0]
		smoke_source := intent.expected_check_sources.filter(it.name == 'v-candidate-smoke')[0]
		native_outcome := validate_persisted_gate_run_shape(intent.gate_runs[0], native_source,
			native_source)!
		smoke_outcome := validate_persisted_gate_run_shape(intent.gate_runs[1], smoke_source,
			native_source)!
		if intent.gate_runs[0].subject_hash != intent.gate_runs[1].subject_hash {
			return error('persisted gate proofs do not retain one immutable subject hash')
		}
		if intent.stage != 'blocked'
			&& combine_native_outcomes(native_outcome, smoke_outcome) != .green {
			return error('non-blocked terminal intention requires two green gate proofs')
		}
	}
	if !timestamp_is_exact(intent.deadlines.build_deadline)
		|| !timestamp_is_exact(intent.deadlines.checks_deadline)
		|| !timestamp_is_exact(intent.deadlines.promotion_deadline)
		|| intent.deadlines.checks_deadline < intent.deadlines.build_deadline
		|| intent.deadlines.promotion_deadline < intent.deadlines.checks_deadline {
		return error('active intention deadlines are invalid or non-monotonic')
	}
	if intent.intent_type in ['adopt-current', 'initial_adopt_current']
		&& intent.stage in ['building', 'build_waiting_source', 'promotion_unknown', 'post_checks_running', 'post_checks_waiting_source'] {
		return error('adoption and bootstrap can never enter build or promotion stages')
	}
	bound_stage := intent.stage in ['candidate_bound', 'checks_running', 'checks_waiting_source',
		'checks_green', 'promotion_unknown', 'post_checks_running', 'post_checks_waiting_source',
		'completed', 'blocked']
	if intent.intent_type in ['publish', 'rollback'] {
		if bound_stage != candidate_binding_is_set(intent.candidate_binding) {
			return error('publish/rollback binding presence does not match the durable stage')
		}
	} else if !validation_subject_is_set(intent.validation_subject)
		|| candidate_binding_is_set(intent.candidate_binding) {
		return error('adoption must retain only its validation subject')
	}
	if intent.intent_type == 'rollback' {
		if !artifact_tuple_is_set(intent.bad_provisional)
			|| !is_lower_hex_64(intent.rollback_diff_fingerprint) {
			return error('rollback intent lacks the bad tuple or revert fingerprint')
		}
		rollback_post_stage := intent.stage in ['post_checks_running', 'post_checks_waiting_source',
			'completed']
		if (rollback_post_stage && !candidate_binding_is_set(intent.rollback_provisional))
			|| (candidate_binding_is_set(intent.rollback_provisional)
			&& intent.stage !in ['post_checks_running', 'post_checks_waiting_source', 'completed', 'blocked']) {
			return error('rollback provisional binding does not match its post-promotion stage')
		}
	} else if artifact_tuple_is_set(intent.bad_provisional)
		|| intent.rollback_diff_fingerprint != ''
		|| candidate_binding_is_set(intent.rollback_provisional) {
		return error('non-rollback intent contains rollback-only bindings')
	}
}

fn validate_resolved_inputs(inputs ResolvedInputsModel) ! {
	if inputs.sources.len == 0 || inputs.sources.len != inputs.source_checks.len
		|| !contract_relative_path_is_safe(inputs.recipe_path)
		|| !is_lower_hex_64(inputs.recipe_hash) || inputs.contract_repository == ''
		|| !is_lower_hex_40(inputs.contract_sha) || !is_lower_hex_40(inputs.v_source_sha)
		|| !is_toolchain_profile_id(inputs.producer_toolchain.profile_id)
		|| !is_lower_hex_64(inputs.producer_toolchain.profile_sha256)
		|| !is_lower_hex_64(inputs.producer_toolchain.observation_sha256)
		|| !is_lower_hex_64(inputs.producer_toolchain.observation_digest) {
		return error('resolved source/recipe/contract/toolchain bindings are incomplete')
	}
	mut ids := []string{}
	for source in inputs.sources {
		if source.id == '' || source.id in ids || source.repository == '' || source.ref == ''
			|| !is_lower_hex_40(source.sha) || !is_lower_hex_40(source.tree) {
			return error('resolved source bindings are invalid or duplicated')
		}
		ids << source.id
		matches := inputs.source_checks.filter(it.source_id == source.id
			&& it.resolved_sha == source.sha && it.status == 'resolved'
			&& is_lower_hex_64(it.evidence_digest))
		if matches.len != 1 {
			return error('each resolved source requires one exact source check')
		}
	}
}

fn resolved_inputs_is_set(inputs ResolvedInputsModel) bool {
	return inputs.sources.len > 0 || inputs.source_checks.len > 0 || inputs.recipe_path != ''
		|| inputs.recipe_hash != '' || inputs.contract_repository != '' || inputs.contract_sha != ''
		|| inputs.v_source_sha != '' || inputs.producer_toolchain.profile_id != ''
		|| inputs.producer_toolchain.profile_sha256 != ''
		|| inputs.producer_toolchain.observation_sha256 != ''
		|| inputs.producer_toolchain.observation_digest != ''
}

fn validate_expected_check_bindings(checks []CheckSourceModel) ! {
	if checks.len != 2 {
		return error('every intention requires exactly the native and V smoke check sources')
	}
	native := checks.filter(it.name == 'tccbin-candidate-gate')
	smoke := checks.filter(it.name == 'v-candidate-smoke')
	if native.len != 1 || smoke.len != 1 || native[0].repository != 'vlang/tccbin'
		|| native[0].workflow_path != '.github/workflows/build-and-test.yml'
		|| native[0].event != 'push' || smoke[0].repository != 'vlang/v'
		|| smoke[0].workflow_path != '.github/workflows/tccbin_revalidate.yml'
		|| smoke[0].event != 'workflow_dispatch' || native[0].integration_id <= 0
		|| smoke[0].integration_id <= 0 || native[0].integration_id == smoke[0].integration_id
		|| native[0].workflow_id <= 0 || smoke[0].workflow_id <= 0
		|| native[0].workflow_id == smoke[0].workflow_id {
		return error('expected checks must bind two distinct exact Integration IDs and workflows')
	}
}

fn validate_candidate_binding(binding CandidateBindingModel, expected_parent string) ! {
	if !candidate_binding_is_set(binding) || binding.parent != expected_parent
		|| !is_lower_hex_40(binding.sha) || !is_lower_hex_40(binding.tree)
		|| !is_lower_hex_40(binding.parent) || !is_lower_hex_64(binding.artifact_fingerprint)
		|| !is_lower_hex_64(binding.manifest_hash) || binding.digests.len == 0 {
		return error('candidate binding does not have the exact expected parent')
	}
	mut paths := []string{}
	for digest in binding.digests {
		if !contract_relative_path_is_safe(digest.path) || !is_lower_hex_64(digest.sha256)
			|| digest.path in paths {
			return error('candidate binding digest set is invalid or duplicated')
		}
		paths << digest.path
	}
}

fn validate_rollback_candidate_binding(binding CandidateBindingModel,
	intent ActiveIntentModel) ! {
	previous := intent.previous_last_known_good
	if binding.artifact_fingerprint != previous.artifact_fingerprint
		|| binding.manifest_hash != previous.manifest_hash || binding.digests != previous.digests {
		return error('rollback candidate bytes differ from the snapshotted last-known-good payload')
	}
}

fn validate_green_verdict(target TargetModel, proof GreenVerdictProof,
	expected ValidationSubjectModel, expected_consumer_id string,
	expected_kind string) !NativeValidationCapsuleFacts {
	if target.provenance_status !in ['complete', 'opaque-accepted']
		|| !validation_subject_is_set(expected) {
		return error('green verdict cannot override incomplete provenance or an absent subject')
	}
	subject := target.active_native_subject
	matrix := authenticated_native_validation_capsule_facts(proof.manifest, subject,
		proof.native_capsule)!
	subject_hash := validate_verdict_subject(target, subject, proof.manifest, expected,
		expected_consumer_id, expected_kind, proof.expected_ledger_generation)!
	if matrix.subject_hash != subject_hash {
		return error('native lane matrix subject differs from the durable active subject')
	}
	validate_verdict_check_sources(target, proof.expected_check_sources)!
	native_source := proof.expected_check_sources.filter(it.name == 'tccbin-candidate-gate')[0]
	smoke_source := proof.expected_check_sources.filter(it.name == 'v-candidate-smoke')[0]
	native_outcome := validate_gate_proof(proof.native_gate, native_source, native_source, subject,
		subject_hash, target.resolved_inputs.v_source_sha)!
	smoke_outcome := validate_gate_proof(proof.v_smoke_gate, smoke_source, native_source, subject,
		subject_hash, target.resolved_inputs.v_source_sha)!
	validate_selected_native_gate_proof(target, proof.native_gate, matrix)!
	if combine_native_outcomes(matrix.matrix_outcome, combine_native_outcomes(native_outcome,
		smoke_outcome)) != .green {
		return error('green verdict requires a green matrix and two successful run/check gates')
	}
	if proof.native_gate.integration_id == proof.v_smoke_gate.integration_id
		|| proof.native_gate.run_id == proof.v_smoke_gate.run_id {
		return error('native and V smoke proofs must come from distinct authenticated sources')
	}
	return matrix
}

fn validate_red_verdict(target TargetModel, proof RedVerdictProof,
	expected ValidationSubjectModel, expected_consumer_id string, expected_kind string,
	allowed_failure_kinds []string) !NativeValidationCapsuleFacts {
	if proof.failure_kind !in allowed_failure_kinds {
		return error('red verdict failure class is not allowed by this transition')
	}
	subject := target.active_native_subject
	matrix := authenticated_native_validation_capsule_facts(proof.manifest, subject,
		proof.native_capsule)!
	subject_hash := validate_verdict_subject(target, subject, proof.manifest, expected,
		expected_consumer_id, expected_kind, proof.expected_ledger_generation)!
	if matrix.subject_hash != subject_hash {
		return error('native lane matrix subject differs from the durable active subject')
	}
	validate_verdict_check_sources(target, proof.expected_check_sources)!
	native_source := proof.expected_check_sources.filter(it.name == 'tccbin-candidate-gate')[0]
	smoke_source := proof.expected_check_sources.filter(it.name == 'v-candidate-smoke')[0]
	native_outcome := validate_gate_proof(proof.native_gate, native_source, native_source, subject,
		subject_hash, target.resolved_inputs.v_source_sha)!
	smoke_outcome := validate_gate_proof(proof.v_smoke_gate, smoke_source, native_source, subject,
		subject_hash, target.resolved_inputs.v_source_sha)!
	validate_selected_native_gate_proof(target, proof.native_gate, matrix)!
	outcome := combine_native_outcomes(matrix.matrix_outcome, combine_native_outcomes(native_outcome,
		smoke_outcome))
	if proof.failure_kind == 'publisher' {
		if outcome != .green {
			return error('publisher failure cannot rewrite already-green lane or gate evidence')
		}
	} else if (proof.failure_kind == 'functional' && outcome != .functional)
		|| (proof.failure_kind == 'infrastructure' && outcome != .infrastructure) {
		return error('red verdict failure class differs from its authenticated matrix and gates')
	}
	return matrix
}

fn validate_verdict_subject(target TargetModel, subject NativeGateSubjectModel,
	manifest AuthenticatedManifestModel, expected ValidationSubjectModel,
	expected_consumer_id string, expected_kind string, expected_ledger_generation i64) !string {
	validate_native_gate_subject(subject)!
	validate_authenticated_manifest_resolved_inputs(manifest, target.resolved_inputs,
		target.input_fingerprint)!
	if subject.consumer_id != expected_consumer_id
		|| subject.intent_or_operation_id != expected_consumer_id
		|| subject.consumer_kind != expected_kind || subject.target_id != target.target_id
		|| expected_ledger_generation != target.generation || subject.sha != expected.sha
		|| subject.tree != expected.tree || subject.original_ref != expected.candidate_ref
		|| subject.input_fingerprint != expected.input_fingerprint
		|| subject.artifact_fingerprint != expected.artifact_fingerprint
		|| subject.manifest_hash != expected.manifest_hash || subject.digests != expected.digests
		|| subject != target.active_native_subject
		|| authenticated_manifest_target_id(manifest)! != target.target_id {
		return error('verdict is not bound to the exact subject, generation, target, or manifest')
	}
	subject_hash := native_gate_subject_hash(subject)!
	if subject_hash != target.active_subject_hash {
		return error('verdict subject hash differs from the durable pre-dispatch subject')
	}
	return subject_hash
}

fn validate_verdict_check_sources(target TargetModel, checks []CheckSourceModel) ! {
	validate_expected_check_bindings(checks)!
	expected := if intent_is_set(target.active_intent) {
		target.active_intent.expected_check_sources
	} else {
		target.remediation_check_sources
	}
	if checks != expected {
		return error('verdict check-source bindings differ from the persisted consumer')
	}
}

fn validate_persisted_gate_run_shape(proof PersistedGateRunModel,
	expected_source CheckSourceModel, native_source CheckSourceModel) !NativeLaneOutcome {
	prefix := if proof.check_name == 'tccbin-candidate-gate' {
		'tccbin-native-gate/'
	} else {
		'tccbin-v-smoke/'
	}
	consumer_id := proof.run_name.all_after(prefix)
	expected_suite_integration := if proof.check_name == 'tccbin-candidate-gate' {
		expected_source.integration_id
	} else {
		native_source.integration_id
	}
	expected_run_url := 'https://github.com/${proof.repository}/actions/runs/${proof.run_id}'
	expected_job_url := '${expected_run_url}/job/${proof.job_id}'
	audience := if proof.check_name == 'tccbin-candidate-gate' {
		'vlang/tccbin:native-gate-check:v1'
	} else {
		'vlang/tccbin:v-smoke-check:v1'
	}
	if proof.check_name != expected_source.name || proof.repository != expected_source.repository
		|| proof.integration_id != expected_source.integration_id
		|| proof.workflow_id != expected_source.workflow_id
		|| proof.workflow_path != expected_source.workflow_path
		|| proof.event != expected_source.event || proof.run_id <= 0 || proof.run_attempt !in [1, 2]
		|| proof.check_suite_id <= 0
		|| proof.check_suite_integration_id != expected_suite_integration || proof.job_id <= 0
		|| !is_lower_hex_64(proof.subject_hash) || proof.check_run_id <= 0
		|| !proof.run_name.starts_with(prefix) || !is_lower_hex_64(consumer_id)
		|| proof.external_id != deterministic_check_external_id(audience, consumer_id, proof.subject_hash, proof.run_id, proof.run_attempt)!
		|| proof.run_url != expected_run_url || proof.job_url != expected_job_url
		|| proof.details_url != expected_job_url || proof.ref == ''
		|| !is_lower_hex_40(proof.workflow_head_sha) || !is_lower_hex_40(proof.sha)
		|| !is_lower_hex_40(proof.check_sha) || proof.actor == '' || proof.actor_integration_id <= 0
		|| proof.triggering_actor == '' || proof.triggering_actor_integration_id <= 0
		|| !timestamp_is_exact(proof.created_at) || !timestamp_is_exact(proof.completed_at)
		|| proof.completed_at < proof.created_at
		|| proof.run_conclusion !in ['success', 'failure', 'cancelled', 'timed_out', 'neutral', 'skipped']
		|| proof.check_conclusion !in ['success', 'failure', 'cancelled', 'timed_out', 'neutral', 'skipped']
		|| !is_lower_hex_64(proof.output_digest) || !is_lower_hex_64(proof.evidence_digest) {
		return error('persisted gate run is not one complete common gate_run')
	}
	return gate_run_outcome(proof.run_conclusion, proof.check_conclusion)
}

fn validate_gate_proof(proof PersistedGateRunModel, expected_source CheckSourceModel,
	native_source CheckSourceModel, subject NativeGateSubjectModel, subject_hash string,
	v_source_sha string) !NativeLaneOutcome {
	shape_outcome := validate_persisted_gate_run_shape(proof, expected_source, native_source)!
	terminal_conclusions := ['success', 'failure', 'cancelled', 'timed_out', 'neutral', 'skipped']
	expected_suite_integration := if proof.check_name == 'tccbin-candidate-gate' {
		expected_source.integration_id
	} else {
		native_source.integration_id
	}
	expected_run_name := if proof.check_name == 'tccbin-candidate-gate' {
		'tccbin-native-gate/${subject.consumer_id}'
	} else {
		'tccbin-v-smoke/${subject.consumer_id}'
	}
	expected_run_url := 'https://github.com/${proof.repository}/actions/runs/${proof.run_id}'
	expected_job_url := '${expected_run_url}/job/${proof.job_id}'
	audience := if proof.check_name == 'tccbin-candidate-gate' {
		'vlang/tccbin:native-gate-check:v1'
	} else {
		'vlang/tccbin:v-smoke-check:v1'
	}
	expected_external_id := deterministic_check_external_id(audience, subject.consumer_id,
		subject_hash, proof.run_id, proof.run_attempt)!
	if proof.check_name != expected_source.name || proof.repository != expected_source.repository
		|| proof.integration_id != expected_source.integration_id
		|| proof.workflow_id != expected_source.workflow_id
		|| proof.workflow_path != expected_source.workflow_path
		|| proof.event != expected_source.event || proof.run_id <= 0 || proof.run_attempt <= 0
		|| proof.run_attempt > 2 || proof.check_suite_id <= 0
		|| proof.check_suite_integration_id != expected_suite_integration || proof.job_id <= 0
		|| proof.subject_hash != subject_hash || proof.check_run_id <= 0
		|| proof.external_id != expected_external_id || proof.run_name != expected_run_name
		|| proof.run_url != expected_run_url || proof.job_url != expected_job_url
		|| proof.details_url != expected_job_url || proof.sha != subject.sha
		|| proof.check_sha != subject.sha || !is_lower_hex_40(proof.workflow_head_sha)
		|| proof.actor == '' || proof.actor_integration_id <= 0 || proof.triggering_actor == ''
		|| proof.triggering_actor_integration_id <= 0 || !timestamp_is_exact(proof.created_at)
		|| !timestamp_is_exact(proof.completed_at) || proof.completed_at < proof.created_at
		|| proof.run_conclusion !in terminal_conclusions
		|| proof.check_conclusion !in terminal_conclusions || !is_lower_hex_64(proof.output_digest)
		|| !is_lower_hex_64(proof.evidence_digest) {
		return error('gate proof is not an exact authenticated terminal run')
	}
	if proof.check_name == 'tccbin-candidate-gate' {
		if proof.ref != subject.original_ref || proof.workflow_head_sha != subject.sha {
			return error('native gate proof does not execute on the exact immutable subject ref')
		}
	} else if proof.ref != 'master' || proof.workflow_head_sha != v_source_sha
		|| proof.actor != 'validator-dispatcher[bot]'
		|| proof.triggering_actor != 'validator-dispatcher[bot]'
		|| proof.actor_integration_id != expected_source.integration_id
		|| proof.triggering_actor_integration_id != expected_source.integration_id {
		return error('V smoke proof differs from trusted master or its validator authority')
	}
	return shape_outcome
}

fn gate_run_outcome(run_conclusion string, check_conclusion string) NativeLaneOutcome {
	if run_conclusion == 'failure' || check_conclusion == 'failure' {
		return .functional
	}
	if run_conclusion in ['cancelled', 'timed_out', 'neutral', 'skipped']
		|| check_conclusion in ['cancelled', 'timed_out', 'neutral', 'skipped'] {
		return .infrastructure
	}
	return .green
}

fn combine_native_outcomes(left NativeLaneOutcome,
	right NativeLaneOutcome) NativeLaneOutcome {
	if left == .functional || right == .functional {
		return .functional
	}
	if left == .infrastructure || right == .infrastructure {
		return .infrastructure
	}
	return .green
}

fn validate_selected_native_gate_proof(target TargetModel, proof PersistedGateRunModel,
	matrix NativeValidationCapsuleFacts) ! {
	gate := target.active_native_gate
	if !native_gate_is_set(gate) || gate.subject != target.active_native_subject
		|| gate.subject_hash != target.active_subject_hash || gate.active_gate_epoch < 0
		|| gate.active_gate_epoch >= gate.epochs.len {
		return error('native proof has no exact durable gate execution')
	}
	epoch := gate.epochs[gate.active_gate_epoch]
	if matrix.subject_hash != target.active_subject_hash
		|| matrix.selected_run.run_id != proof.run_id
		|| matrix.selected_run.run_attempt != proof.run_attempt
		|| matrix.selected_run.check_suite_id != proof.check_suite_id
		|| proof.output_digest != matrix.matrix_digest {
		return error('native gate proof differs from the sealed matrix output or selected run')
	}
	if epoch.state != .completed || gate.selected_run_id != proof.run_id
		|| gate.selected_run_attempt != proof.run_attempt
		|| gate.selected_check_suite_id != proof.check_suite_id
		|| gate.selected_conclusion != proof.run_conclusion {
		return error('native proof is not the completed write-once gate winner')
	}
	matches := gate.gate_runs.filter(it.epoch == gate.active_gate_epoch && it.run_id == proof.run_id
		&& it.run_attempt == proof.run_attempt && it.check_suite_id == proof.check_suite_id)
	if matches.len != 1 {
		return error('native proof does not resolve to one persisted gate run')
	}
	run := matches[0]
	if run.repository != proof.repository || run.workflow_id != proof.workflow_id
		|| run.workflow_path != proof.workflow_path || run.ref != proof.ref || run.sha != proof.sha
		|| run.event != proof.event || run.actor != proof.actor
		|| run.actor_integration_id != proof.actor_integration_id
		|| run.triggering_actor != proof.triggering_actor
		|| run.triggering_actor_integration_id != proof.triggering_actor_integration_id
		|| run.created_at != proof.created_at || run.conclusion != proof.run_conclusion {
		return error('native proof facts differ from the persisted authenticated gate run')
	}
}

fn validate_artifact_tuple(value ArtifactTupleModel) ! {
	if !is_lower_hex_40(value.sha) || !is_lower_hex_40(value.tree)
		|| !is_lower_hex_64(value.input_fingerprint) || !is_lower_hex_64(value.artifact_fingerprint)
		|| !is_lower_hex_64(value.manifest_hash) || value.digests.len == 0 {
		return error('artifact tuple is incomplete')
	}
	mut paths := []string{}
	for digest in value.digests {
		if !contract_relative_path_is_safe(digest.path) || !is_lower_hex_64(digest.sha256)
			|| digest.path in paths {
			return error('artifact tuple digest set is invalid or duplicated')
		}
		paths << digest.path
	}
}

fn validate_source_refetch(refetch SourceRefetchModel, source_state SourceStateModel,
	target TargetModel, operation_id string, expected_status string) ! {
	validate_source_state(source_state)!
	if refetch.target_id != target.target_id || refetch.expected_generation != target.generation
		|| refetch.expected_canonical_head != target.canonical_observed_sha
		|| refetch.input_fingerprint != target.input_fingerprint
		|| refetch.operation_id != operation_id || refetch.status != expected_status
		|| !timestamp_is_exact(refetch.checked_at) || !is_lower_hex_64(refetch.evidence_digest) {
		return error('source refetch tuple is incomplete or stale')
	}
	expected_source_status := if expected_status == 'unreachable' {
		'source_unreachable'
	} else {
		'resolved'
	}
	if source_state.source_id != refetch.source_state_id
		|| source_state.generation != refetch.source_state_generation
		|| source_state.status != expected_source_status
		|| source_state.canonical_url != refetch.source_repository
		|| source_state.ref != refetch.requested_ref
		|| source_state.last_attempt_at != refetch.checked_at
		|| refetch.resolution_operation_id !in source_state.applied_operation_ids
		|| !source_projection_matches_state(refetch.source_id, source_state.source_id) {
		return error('source refetch is not bound to the exact durable source-state generation')
	}
	if !intent_is_set(target.active_intent) {
		return error('source refetch has no persisted active consumer')
	}
	sources := target.active_intent.resolved_inputs.sources.filter(it.id == refetch.source_id
		&& it.repository == refetch.source_repository && it.ref == refetch.requested_ref
		&& it.sha == refetch.previous_sha)
	if sources.len != 1 {
		return error('source refetch request differs from the persisted resolved input')
	}
	if expected_status == 'unreachable' {
		if refetch.failure_kind !in ['dns', 'connectivity', 'tls_transient', 'timeout', 'http_429', 'http_5xx']
			|| refetch.resolved_sha != '' || refetch.resolved_tree != ''
			|| source_state.resolved_sha != '' || source_state.mode != .upstream_recovery_daily {
			return error('unreachable source refetch is not a silent transient resolver failure')
		}
	} else if refetch.failure_kind != '' || !is_lower_hex_40(refetch.resolved_sha)
		|| !is_lower_hex_40(refetch.resolved_tree)
		|| source_state.resolved_sha != refetch.resolved_sha {
		return error('restored source refetch lacks its exact resolved SHA and tree')
	}
}

fn source_projection_matches_state(projection_id string, state_id string) bool {
	return match projection_id {
		'tinycc' { state_id == 'tinycc-mob' }
		'bdwgc' { state_id == 'bdwgc-master' }
		'libatomic_ops' { state_id == 'libatomic_ops-master' }
		else { false }
	}
}

fn artifact_tuple_is_set(value ArtifactTupleModel) bool {
	return value.sha != ''
}

fn candidate_binding_is_set(value CandidateBindingModel) bool {
	return value.sha != ''
}

fn validation_subject_is_set(value ValidationSubjectModel) bool {
	return value.sha != ''
}

fn intent_is_set(value ActiveIntentModel) bool {
	return value.intent_id != ''
}

fn native_subject_is_set(value NativeGateSubjectModel) bool {
	return value.consumer_id != ''
}

fn native_gate_is_set(value NativeGateModel) bool {
	return value.subject_hash != ''
}

fn artifact_tuple_equal(left ArtifactTupleModel, right ArtifactTupleModel) bool {
	return left == right
}

fn validation_subject_equal(left ValidationSubjectModel, right ValidationSubjectModel) bool {
	return left == right
}

fn artifact_tuple_from_validation(subject ValidationSubjectModel) ArtifactTupleModel {
	return ArtifactTupleModel{
		sha:                  subject.sha
		tree:                 subject.tree
		input_fingerprint:    subject.input_fingerprint
		artifact_fingerprint: subject.artifact_fingerprint
		manifest_hash:        subject.manifest_hash
		digests:              subject.digests.clone()
	}
}

fn artifact_tuple_from_binding(binding CandidateBindingModel, input_fingerprint string) ArtifactTupleModel {
	return ArtifactTupleModel{
		sha:                  binding.sha
		tree:                 binding.tree
		input_fingerprint:    input_fingerprint
		artifact_fingerprint: binding.artifact_fingerprint
		manifest_hash:        binding.manifest_hash
		digests:              binding.digests.clone()
	}
}

fn artifact_tuple_from_candidate(intent ActiveIntentModel, input_fingerprint string) ArtifactTupleModel {
	return artifact_tuple_from_binding(intent.candidate_binding, input_fingerprint)
}

fn validation_from_artifact(value ArtifactTupleModel, reference string) ValidationSubjectModel {
	return ValidationSubjectModel{
		sha:                  value.sha
		tree:                 value.tree
		input_fingerprint:    value.input_fingerprint
		artifact_fingerprint: value.artifact_fingerprint
		manifest_hash:        value.manifest_hash
		digests:              value.digests.clone()
		candidate_ref:        reference
	}
}

fn validation_from_candidate(binding CandidateBindingModel, input_fingerprint string,
	reference string) ValidationSubjectModel {
	return validation_from_artifact(artifact_tuple_from_binding(binding, input_fingerprint),
		reference)
}

fn intent_validation_subject(intent ActiveIntentModel) !ValidationSubjectModel {
	if intent.intent_type in ['adopt-current', 'initial_adopt_current'] {
		return intent.validation_subject
	}
	if !candidate_binding_is_set(intent.candidate_binding) {
		return error('candidate intention is not bound')
	}
	return validation_from_candidate(intent.candidate_binding, intent.input_fingerprint,
		intent.candidate_ref)
}

fn intent_with_stage(intent ActiveIntentModel, stage string) ActiveIntentModel {
	return ActiveIntentModel{
		...intent
		stage: stage
	}
}

fn intent_with_binding(intent ActiveIntentModel, binding CandidateBindingModel,
	stage string) ActiveIntentModel {
	return ActiveIntentModel{
		...intent
		stage:             stage
		candidate_binding: binding
	}
}

fn intent_with_rollback_provisional(intent ActiveIntentModel, binding CandidateBindingModel,
	stage string) ActiveIntentModel {
	return ActiveIntentModel{
		...intent
		stage:                stage
		rollback_provisional: binding
	}
}

fn intent_with_gate_proofs(intent ActiveIntentModel, proof GreenVerdictProof,
	stage string) ActiveIntentModel {
	return ActiveIntentModel{
		...intent
		stage:     stage
		gate_runs: [proof.native_gate, proof.v_smoke_gate]
	}
}

fn intent_with_failure_proofs(intent ActiveIntentModel, proof RedVerdictProof,
	stage string) ActiveIntentModel {
	return ActiveIntentModel{
		...intent
		stage:     stage
		gate_runs: [proof.native_gate, proof.v_smoke_gate]
	}
}

fn intent_subject_is_bound(intent ActiveIntentModel) bool {
	return if intent.intent_type in ['adopt-current', 'initial_adopt_current'] {
		validation_subject_is_set(intent.validation_subject)
	} else {
		candidate_binding_is_set(intent.candidate_binding)
	}
}

fn candidate_ref_is_exact(reference string, intent_id string) bool {
	segments := reference.split('/')
	return segments.len == 3 && segments[0] == 'tccbin-candidate'
		&& segments[1] in managed_target_ids && segments[2] == intent_id
}

fn canonical_ref(target_id string) string {
	return 'thirdparty-${target_id}'
}

fn validate_head_observation(current TargetModel, context TransitionContext,
	expected_subject_sha string, allowed_relationships []HeadRelationship) !string {
	observation := context.head_observation
	if observation.target_id != current.target_id
		|| observation.expected_generation != current.generation
		|| observation.expected_previous_head != current.canonical_observed_sha
		|| observation.subject_sha != expected_subject_sha
		|| observation.operation_id != context.operation_id
		|| observation.relationship !in allowed_relationships
		|| !is_lower_hex_40(observation.canonical_head) || !is_lower_hex_40(observation.subject_sha)
		|| !timestamp_is_exact(observation.observed_at)
		|| !is_lower_hex_64(observation.evidence_digest) {
		return error('canonical HEAD observation is incomplete, stale, or not bound to this CAS')
	}
	if observation.relationship == .exact_subject
		&& observation.canonical_head != observation.subject_sha {
		return error('exact-subject HEAD observation does not point at the authenticated subject')
	}
	if observation.relationship == .subject_ancestor
		&& observation.canonical_head == observation.subject_sha {
		return error('descendant HEAD observation cannot equal its ancestor subject')
	}
	return observation.canonical_head
}

fn require_exact_head(current TargetModel, context TransitionContext,
	expected_subject_sha string) !string {
	return validate_head_observation(current, context, expected_subject_sha, [
		.exact_subject,
	])
}

fn require_changed_head(current TargetModel, context TransitionContext,
	expected_subject_sha string) !string {
	observed := validate_head_observation(current, context, expected_subject_sha, [
		.subject_ancestor,
		.unrelated,
	])!
	if observed == current.canonical_observed_sha {
		return error('stale-validation transition did not observe a changed canonical HEAD')
	}
	return observed
}

fn expected_active_validation_sha(target TargetModel) string {
	if validation_subject_is_set(target.active_intent.validation_subject) {
		return target.active_intent.validation_subject.sha
	}
	if artifact_tuple_is_set(target.provisional_published) {
		return target.provisional_published.sha
	}
	return target.canonical_observed_sha
}

fn validate_canonical_validation_subject(subject ValidationSubjectModel, target TargetModel) ! {
	if !validation_subject_is_set(subject) || subject.sha != target.canonical_observed_sha
		|| subject.candidate_ref != canonical_ref(target.target_id) {
		return error('canonical validation subject does not bind the exact current HEAD and branch')
	}
	validate_artifact_tuple(artifact_tuple_from_validation(subject))!
}

fn validate_transition_native_subject(target TargetModel, subject NativeGateSubjectModel,
	expected ValidationSubjectModel, expected_consumer_id string, expected_kind string,
	expected_subject_generation i64) !string {
	validate_native_gate_subject(subject)!
	if !validation_subject_is_set(expected) || subject.consumer_id != expected_consumer_id
		|| subject.intent_or_operation_id != expected_consumer_id
		|| subject.consumer_kind != expected_kind || subject.target_id != target.target_id
		|| subject.subject_generation != expected_subject_generation || subject.sha != expected.sha
		|| subject.tree != expected.tree || subject.original_ref != expected.candidate_ref
		|| subject.input_fingerprint != expected.input_fingerprint
		|| subject.artifact_fingerprint != expected.artifact_fingerprint
		|| subject.manifest_hash != expected.manifest_hash || subject.digests != expected.digests {
		return error('pre-dispatch native subject differs from its exact durable consumer tuple')
	}
	return native_gate_subject_hash(subject)!
}

fn validate_transition_native_gate(gate NativeGateModel, subject NativeGateSubjectModel,
	expected_generation i64) ! {
	validate_native_gate(gate)!
	if gate.subject != subject || gate.subject_hash != native_gate_subject_hash(subject)!
		|| gate.subject_sha != subject.sha || gate.subject_generation != subject.subject_generation
		|| gate.expected_ledger_generation != expected_generation || gate.gate_runs.len != 0
		|| gate.ack_operation_ids.len != 0 || gate.completion_operation_ids.len != 0
		|| gate.epoch_close_operation_ids.len != 0 || gate.active_gate_epoch != 0
		|| gate.epochs.len != 1 || gate.epochs[0].state != .open_unselected {
		return error('initial native gate execution does not match the pre-dispatch subject/generation')
	}
}

fn consumer_kind_for_intent(intent ActiveIntentModel) !string {
	return match intent.intent_type {
		'publish' { 'publish_candidate' }
		'rollback' { 'rollback_candidate' }
		'adopt-current' { 'adopt_current' }
		'initial_adopt_current' { 'initial_adopt_current' }
		else { error('active intent has no native-gate consumer kind') }
	}
}

fn rollback_failure_subject(target TargetModel) !ValidationSubjectModel {
	if candidate_binding_is_set(target.active_intent.rollback_provisional) {
		return validation_from_candidate(target.active_intent.rollback_provisional,
			target.input_fingerprint, canonical_ref(target.target_id))
	}
	return intent_validation_subject(target.active_intent)
}

fn rollback_failure_consumer_kind(target TargetModel) !string {
	return if candidate_binding_is_set(target.active_intent.rollback_provisional) {
		'rollback_post'
	} else {
		consumer_kind_for_intent(target.active_intent)!
	}
}

fn rollback_failure_consumer_id(target TargetModel) !string {
	if candidate_binding_is_set(target.active_intent.rollback_provisional) {
		if !is_lower_hex_64(target.post_validation_operation_id) {
			return error('rollback post-check failure lacks its durable operation identity')
		}
		return target.post_validation_operation_id
	}
	return target.active_intent.intent_id
}

fn require_incidents(existing []string, operation_id string) []string {
	mut incidents := existing.clone()
	if operation_id !in incidents {
		incidents << operation_id
	}
	return incidents
}

fn safe_path_segment(value string) bool {
	return value != '' && value != '.' && value != '..' && value != '...' && !value.contains('/')
		&& !value.contains('\\') && !value.contains('\x00')
}
