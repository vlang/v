module bin

// ReceiverTargetSnapshot is the durable target tuple re-read before a receiver can run.
pub struct ReceiverTargetSnapshot {
pub:
	target_id                  string
	generation                 i64
	canonical_head             string
	input_fingerprint          string
	artifact_fingerprint       string
	manifest_hash              string
	active_consumer_id         string
	active_recovery_handoff_id string
	active_subject_hash        string
}

// ReceiverLedgerEntry is one opaque consumer or recovery handoff projection.
pub struct ReceiverLedgerEntry {
pub:
	id                         string
	record_type                string
	target_id                  string
	consumer_type              string
	resume_capability          string
	intent_or_operation_id     string
	expected_ledger_generation i64
	expected_canonical_head    string
	subject_ref_head           string
	subject_hash               string
	subject                    RecoverySubjectModel
	receiver_repository        string
	workflow_id                i64
	workflow_path              string
	workflow_ref               string
	event                      string
	run_name                   string
	state                      string
	selected_run_id            i64
	selected_run_attempt       int
	receiver_master_sha        string
}

// ReceiverStateLedger is the bounded local representation of the protected state ref.
pub struct ReceiverStateLedger {
pub:
	schema_version int
	audience       string
	targets        []ReceiverTargetSnapshot
	entries        []ReceiverLedgerEntry
}

// ReceiverRequestFacts are trusted workflow facts, never target/ref/publish selectors.
pub struct ReceiverRequestFacts {
pub:
	opaque_id                 string
	repository                string
	workflow_id               i64
	workflow_path             string
	workflow_ref              string
	event                     string
	current_run_id            i64
	current_run_attempt       int
	current_head_sha          string
	current_run_name          string
	observed_canonical_head   string
	observed_subject_ref_head string
	requested_publish         bool
}

// ReceiverResolution exposes only the action derived from the durable ledger.
pub struct ReceiverResolution {
pub:
	target_id              string
	consumer_type          string
	resume_capability      string
	intent_or_operation_id string
	subject_hash           string
	subject_sha            string
	allowed_to_execute     bool
	publish_allowed        bool
}

// ReceiverCompletionResolution is the authenticated workflow_run projection used by recovery.
pub struct ReceiverCompletionResolution {
pub:
	handoff_id           string
	target_id            string
	resume_capability    string
	receiver_conclusion  string
	may_create_successor bool
}

// ReceiverCompletionLookup distinguishes an unrelated workflow completion from one exact,
// authenticated recovery callback. Inactive lookups carry no completion authority.
pub struct ReceiverCompletionLookup {
pub:
	active     bool
	completion ReceiverCompletionResolution
}

// resolve_receiver_request resolves one opaque ID and revalidates every durable binding.
pub fn resolve_receiver_request(ledger_source string, request ReceiverRequestFacts) !ReceiverResolution {
	ledger := parse_receiver_state_ledger(ledger_source)!
	return resolve_receiver_request_from_ledger(ledger, request)
}

fn receiver_subjects_match(left RecoverySubjectModel, right RecoverySubjectModel) bool {
	return left.consumer_id == right.consumer_id && left.consumer_kind == right.consumer_kind
		&& left.intent_or_operation_id == right.intent_or_operation_id
		&& left.target_id == right.target_id && left.subject_generation == right.subject_generation
		&& left.initial_run_mode == right.initial_run_mode
		&& left.remediation_trigger == right.remediation_trigger && left.sha == right.sha
		&& left.tree == right.tree && left.original_ref == right.original_ref
		&& left.input_fingerprint == right.input_fingerprint
		&& left.artifact_fingerprint == right.artifact_fingerprint
		&& left.manifest_hash == right.manifest_hash && left.digests == right.digests
}

fn receiver_entry_matches_target_tuple(target ReceiverTargetSnapshot,
	entry ReceiverLedgerEntry) !bool {
	return target.target_id == entry.target_id
		&& target.generation == entry.expected_ledger_generation
		&& target.canonical_head == entry.expected_canonical_head
		&& target.input_fingerprint == entry.subject.input_fingerprint
		&& target.artifact_fingerprint == entry.subject.artifact_fingerprint
		&& target.manifest_hash == entry.subject.manifest_hash
		&& target.active_consumer_id == entry.intent_or_operation_id
		&& target.active_subject_hash == entry.subject_hash
		&& entry.subject.target_id == target.target_id
		&& entry.subject_ref_head == entry.subject.sha
		&& entry.subject_hash == recovery_subject_hash(entry.subject)!
}

fn validate_receiver_consumer_recovery_companion(ledger ReceiverStateLedger,
	target ReceiverTargetSnapshot, consumer ReceiverLedgerEntry) ! {
	if target.active_recovery_handoff_id == '' {
		return
	}
	companions := ledger.entries.filter(it.id == target.active_recovery_handoff_id
		&& it.record_type == 'handoff')
	if companions.len != 1 {
		return error('active receiver consumer requires exactly one recovery handoff companion')
	}
	companion := companions[0]
	consumer_matches := receiver_entry_matches_target_tuple(target, consumer)!
	companion_matches := receiver_entry_matches_target_tuple(target, companion)!
	if !consumer_matches || !companion_matches || companion.target_id != consumer.target_id
		|| companion.intent_or_operation_id != consumer.intent_or_operation_id
		|| !receiver_subjects_match(companion.subject, consumer.subject) {
		return error('active receiver consumer and recovery handoff do not preserve one exact subject tuple')
	}
}

fn resolve_receiver_request_from_ledger(ledger ReceiverStateLedger,
	request ReceiverRequestFacts) !ReceiverResolution {
	if !is_lower_hex_64(request.opaque_id) || request.requested_publish {
		return error('receiver accepts only one opaque ID and never a publish selector')
	}
	matches := ledger.entries.filter(it.id == request.opaque_id)
	if matches.len != 1 {
		return error('opaque receiver ID must resolve to exactly one durable ledger entry')
	}
	entry := matches[0]
	targets := ledger.targets.filter(it.target_id == entry.target_id)
	if targets.len != 1 {
		return error('receiver entry must resolve to one durable target snapshot')
	}
	target := targets[0]
	if request.repository != entry.receiver_repository || request.workflow_id != entry.workflow_id
		|| request.workflow_path != entry.workflow_path
		|| request.workflow_ref != entry.workflow_ref || request.event != entry.event {
		return error('receiver workflow repository, ID, path, ref, or event is not authorized')
	}
	tuple_matches := receiver_entry_matches_target_tuple(target, entry)!
	if !tuple_matches || request.observed_canonical_head != entry.expected_canonical_head
		|| request.observed_subject_ref_head != entry.subject_ref_head {
		return error('receiver target generation, HEAD, fingerprints, or subject is stale')
	}
	if entry.record_type == 'consumer' {
		if entry.id != entry.intent_or_operation_id || target.active_consumer_id != entry.id {
			return error('receiver consumer is no longer active')
		}
		validate_receiver_consumer_recovery_companion(ledger, target, entry)!
	} else if entry.record_type == 'handoff' {
		if target.active_recovery_handoff_id != entry.id {
			return error('receiver handoff is no longer active')
		}
	} else {
		return error('receiver ledger record type is not allowlisted')
	}
	validate_recovery_routing(entry.consumer_type, entry.resume_capability)!
	expected_workflow := recovery_workflow_path(entry.resume_capability)
	if entry.workflow_path != expected_workflow {
		return error('receiver capability is routed to the wrong trusted workflow')
	}
	mut allowed_to_execute := false
	if entry.state == 'pending' {
		if request.current_run_id != 0 || request.current_run_attempt != 0
			|| request.current_head_sha != '' || request.current_run_name != '' {
			return error('pending receiver must stop before execution until its ACK is durable')
		}
	} else if entry.state == 'dispatched' {
		if request.current_run_id != entry.selected_run_id
			|| request.current_run_attempt != entry.selected_run_attempt
			|| request.current_head_sha != entry.receiver_master_sha
			|| request.current_run_name != entry.run_name {
			return error('receiver run is not the exact ACK-selected run and attempt')
		}
		allowed_to_execute = true
	} else {
		return error('terminal or blocked receiver entries cannot execute')
	}
	return ReceiverResolution{
		target_id:              target.target_id
		consumer_type:          entry.consumer_type
		resume_capability:      entry.resume_capability
		intent_or_operation_id: entry.intent_or_operation_id
		subject_hash:           entry.subject_hash
		subject_sha:            entry.subject.sha
		allowed_to_execute:     allowed_to_execute
		publish_allowed:        false
	}
}

// lookup_receiver_completion treats workflow runs without a dispatched handoff as inactive,
// while preserving fail-closed authentication for every run that names one.
pub fn lookup_receiver_completion(ledger_source string,
	event_source string) !ReceiverCompletionLookup {
	ledger := parse_receiver_state_ledger(ledger_source)!
	event := parse_strict_json(event_source)!
	workflow_run := require_object_member(event, 'workflow_run')!
	run_name := require_string_member(workflow_run, 'display_title')!
	matches := ledger.entries.filter(it.record_type == 'handoff' && it.state == 'dispatched'
		&& it.run_name == run_name)
	if matches.len == 0 {
		return ReceiverCompletionLookup{}
	}
	if matches.len > 1 {
		return error('workflow_run must resolve by deterministic run-name to one active handoff')
	}
	entry := matches[0]
	repository := require_object_member(event, 'repository')!
	repository_name := require_string_member(repository, 'full_name')!
	run_id := require_integer_member(workflow_run, 'id')!
	run_attempt := int(require_integer_member(workflow_run, 'run_attempt')!)
	workflow_id := require_integer_member(workflow_run, 'workflow_id')!
	workflow_path := require_string_member(workflow_run, 'path')!
	workflow_ref := require_string_member(workflow_run, 'head_branch')!
	run_event := require_string_member(workflow_run, 'event')!
	head_sha := require_string_member(workflow_run, 'head_sha')!
	conclusion := require_string_member(workflow_run, 'conclusion')!
	targets := ledger.targets.filter(it.target_id == entry.target_id)
	if targets.len != 1 {
		return error('workflow_run handoff is stale against the current target generation')
	}
	target := targets[0]
	tuple_matches := receiver_entry_matches_target_tuple(target, entry)!
	if !tuple_matches || target.active_recovery_handoff_id != entry.id {
		return error('workflow_run handoff is stale against the current target generation')
	}
	if repository_name != entry.receiver_repository || workflow_id != entry.workflow_id
		|| workflow_path != entry.workflow_path || workflow_ref != entry.workflow_ref
		|| run_event != entry.event || run_id != entry.selected_run_id
		|| run_attempt != entry.selected_run_attempt || head_sha != entry.receiver_master_sha
		|| conclusion !in ['success', 'failure', 'cancelled', 'timed_out'] {
		return error('workflow_run repository/workflow/ref/run/attempt/SHA binding is not exact')
	}
	return ReceiverCompletionLookup{
		active:     true
		completion: ReceiverCompletionResolution{
			handoff_id:           entry.id
			target_id:            entry.target_id
			resume_capability:    entry.resume_capability
			receiver_conclusion:  conclusion
			may_create_successor: entry.resume_capability == 'native_gate'
				&& conclusion == 'success'
		}
	}
}

// resolve_receiver_completion is the strict API for callers that require an active callback.
pub fn resolve_receiver_completion(ledger_source string,
	event_source string) !ReceiverCompletionResolution {
	lookup := lookup_receiver_completion(ledger_source, event_source)!
	if !lookup.active {
		return error('workflow_run must resolve by deterministic run-name to one active handoff')
	}
	return lookup.completion
}

// resolve_active_recovery_id proves that an opaque handoff is still the target's active pointer.
pub fn resolve_active_recovery_id(ledger_source string, opaque_id string) !ReceiverLedgerEntry {
	ledger := parse_receiver_state_ledger(ledger_source)!
	if !is_lower_hex_64(opaque_id) {
		return error('source recovery accepts only an opaque lowercase SHA-256 handoff ID')
	}
	matches := ledger.entries.filter(it.id == opaque_id && it.record_type == 'handoff')
	if matches.len != 1 {
		return error('source recovery handoff must resolve to exactly one ledger entry')
	}
	entry := matches[0]
	targets := ledger.targets.filter(it.target_id == entry.target_id)
	if targets.len != 1 {
		return error('source recovery handoff is stale against its exact target tuple')
	}
	target := targets[0]
	tuple_matches := receiver_entry_matches_target_tuple(target, entry)!
	if !tuple_matches || target.active_recovery_handoff_id != entry.id {
		return error('source recovery handoff is stale against its exact target tuple')
	}
	return entry
}

// parse_receiver_state_ledger strictly decodes the bounded dark-mode ledger fixture.
pub fn parse_receiver_state_ledger(source string) !ReceiverStateLedger {
	root := parse_strict_json(source)!
	require_exact_keys(root, ['schema_version', 'audience', 'targets', 'entries'])!
	mut targets := []ReceiverTargetSnapshot{}
	mut target_ids := []string{}
	for value in require_array_member(root, 'targets')! {
		require_exact_keys(value, ['target_id', 'generation', 'canonical_head', 'input_fingerprint',
			'artifact_fingerprint', 'manifest_hash', 'active_consumer_id',
			'active_recovery_handoff_id', 'active_subject_hash'])!
		target := ReceiverTargetSnapshot{
			target_id:                  require_string_member(value, 'target_id')!
			generation:                 require_integer_member(value, 'generation')!
			canonical_head:             require_string_member(value, 'canonical_head')!
			input_fingerprint:          require_string_member(value, 'input_fingerprint')!
			artifact_fingerprint:       require_string_member(value, 'artifact_fingerprint')!
			manifest_hash:              require_string_member(value, 'manifest_hash')!
			active_consumer_id:         require_nullable_string_member(value, 'active_consumer_id')!
			active_recovery_handoff_id: require_nullable_string_member(value,
				'active_recovery_handoff_id')!
			active_subject_hash:        require_string_member(value, 'active_subject_hash')!
		}
		if target.target_id !in managed_target_ids || target.target_id in target_ids
			|| target.generation < 0 || !is_lower_hex_40(target.canonical_head)
			|| !is_lower_hex_64(target.input_fingerprint)
			|| !is_lower_hex_64(target.artifact_fingerprint)
			|| !is_lower_hex_64(target.manifest_hash)
			|| (target.active_consumer_id != '' && !is_lower_hex_64(target.active_consumer_id))
			|| (target.active_recovery_handoff_id != ''
			&& !is_lower_hex_64(target.active_recovery_handoff_id))
			|| !is_lower_hex_64(target.active_subject_hash) {
			return error('receiver target snapshot is invalid or duplicated')
		}
		target_ids << target.target_id
		targets << target
	}
	mut entries := []ReceiverLedgerEntry{}
	mut entry_ids := []string{}
	for value in require_array_member(root, 'entries')! {
		require_exact_keys(value, ['id', 'record_type', 'target_id', 'consumer_type',
			'resume_capability', 'intent_or_operation_id', 'expected_ledger_generation',
			'expected_canonical_head', 'subject_ref_head', 'subject_hash', 'subject',
			'receiver_repository', 'workflow_id', 'workflow_path', 'workflow_ref', 'event',
			'run_name', 'state', 'selected_run_id', 'selected_run_attempt', 'receiver_master_sha'])!
		entry := ReceiverLedgerEntry{
			id:                         require_string_member(value, 'id')!
			record_type:                require_string_member(value, 'record_type')!
			target_id:                  require_string_member(value, 'target_id')!
			consumer_type:              require_string_member(value, 'consumer_type')!
			resume_capability:          require_string_member(value, 'resume_capability')!
			intent_or_operation_id:     require_string_member(value, 'intent_or_operation_id')!
			expected_ledger_generation: require_integer_member(value, 'expected_ledger_generation')!
			expected_canonical_head:    require_string_member(value, 'expected_canonical_head')!
			subject_ref_head:           require_string_member(value, 'subject_ref_head')!
			subject_hash:               require_string_member(value, 'subject_hash')!
			subject:                    parse_receiver_subject(require_object_member(value,
				'subject')!)!
			receiver_repository:        require_string_member(value, 'receiver_repository')!
			workflow_id:                require_integer_member(value, 'workflow_id')!
			workflow_path:              require_string_member(value, 'workflow_path')!
			workflow_ref:               require_string_member(value, 'workflow_ref')!
			event:                      require_string_member(value, 'event')!
			run_name:                   require_string_member(value, 'run_name')!
			state:                      require_string_member(value, 'state')!
			selected_run_id:            require_integer_member(value, 'selected_run_id')!
			selected_run_attempt:       int(require_integer_member(value, 'selected_run_attempt')!)
			receiver_master_sha:        require_nullable_string_member(value, 'receiver_master_sha')!
		}
		validate_receiver_entry(entry)!
		if entry.id in entry_ids {
			return error('receiver ledger IDs must be globally unique')
		}
		entry_ids << entry.id
		entries << entry
	}
	ledger := ReceiverStateLedger{
		schema_version: int(require_integer_member(root, 'schema_version')!)
		audience:       require_string_member(root, 'audience')!
		targets:        targets
		entries:        entries
	}
	if ledger.schema_version != 1 || ledger.audience != 'vlang/v:tccbin-automation-state'
		|| ledger.targets.len == 0 || ledger.entries.len == 0 {
		return error('receiver ledger header or bounded inventory is invalid')
	}
	for entry in ledger.entries {
		if entry.record_type != 'consumer' {
			continue
		}
		targets_for_consumer := ledger.targets.filter(it.target_id == entry.target_id)
		if targets_for_consumer.len != 1 {
			return error('receiver consumer entry must resolve to one durable target snapshot')
		}
		validate_receiver_consumer_recovery_companion(ledger, targets_for_consumer[0], entry)!
	}
	return ledger
}

fn parse_receiver_subject(value JsonValue) !RecoverySubjectModel {
	require_exact_keys(value, ['consumer_id', 'consumer_kind', 'intent_or_operation_id', 'target_id',
		'subject_generation', 'initial_run_mode', 'remediation_trigger', 'sha', 'tree',
		'original_ref', 'input_fingerprint', 'artifact_fingerprint', 'manifest_hash', 'digests'])!
	mut digests := []DigestModel{}
	for digest in require_array_member(value, 'digests')! {
		require_exact_keys(digest, ['path', 'sha256'])!
		digests << DigestModel{
			path:   require_string_member(digest, 'path')!
			sha256: require_string_member(digest, 'sha256')!
		}
	}
	trigger_value := require_member(value, 'remediation_trigger')!
	mut trigger := RemediationTriggerModel{}
	if trigger_value.kind == .object {
		require_exact_keys(trigger_value, ['repository', 'ref', 'before', 'after', 'tree',
			'diff_fingerprint', 'owner_domain'])!
		trigger = RemediationTriggerModel{
			repository:       require_string_member(trigger_value, 'repository')!
			ref:              require_string_member(trigger_value, 'ref')!
			before:           require_string_member(trigger_value, 'before')!
			after:            require_string_member(trigger_value, 'after')!
			tree:             require_string_member(trigger_value, 'tree')!
			diff_fingerprint: require_string_member(trigger_value, 'diff_fingerprint')!
			owner_domain:     require_string_member(trigger_value, 'owner_domain')!
		}
	} else if trigger_value.kind != .null_value {
		return error('recovery remediation trigger must be an object or null')
	}
	subject := RecoverySubjectModel{
		consumer_id:            require_string_member(value, 'consumer_id')!
		consumer_kind:          require_string_member(value, 'consumer_kind')!
		intent_or_operation_id: require_string_member(value, 'intent_or_operation_id')!
		target_id:              require_string_member(value, 'target_id')!
		subject_generation:     require_integer_member(value, 'subject_generation')!
		initial_run_mode:       require_string_member(value, 'initial_run_mode')!
		remediation_trigger:    trigger
		sha:                    require_string_member(value, 'sha')!
		tree:                   require_string_member(value, 'tree')!
		original_ref:           require_string_member(value, 'original_ref')!
		input_fingerprint:      require_string_member(value, 'input_fingerprint')!
		artifact_fingerprint:   require_string_member(value, 'artifact_fingerprint')!
		manifest_hash:          require_string_member(value, 'manifest_hash')!
		digests:                digests
	}
	validate_recovery_subject(subject)!
	return subject
}

fn validate_receiver_entry(entry ReceiverLedgerEntry) ! {
	expected_run_name := if entry.record_type == 'handoff' {
		'tccbin-recovery-${entry.id}'
	} else {
		'tccbin-gate-${entry.id}'
	}
	if !is_lower_hex_64(entry.id) || entry.record_type !in ['consumer', 'handoff']
		|| entry.target_id != entry.subject.target_id
		|| !is_lower_hex_64(entry.intent_or_operation_id)
		|| entry.expected_ledger_generation < entry.subject.subject_generation
		|| entry.intent_or_operation_id != entry.subject.consumer_id
		|| entry.intent_or_operation_id != entry.subject.intent_or_operation_id
		|| !is_lower_hex_40(entry.expected_canonical_head)
		|| entry.subject_ref_head != entry.subject.sha
		|| entry.subject_hash != recovery_subject_hash(entry.subject)!
		|| entry.receiver_repository != 'vlang/v' || entry.workflow_id <= 0
		|| entry.workflow_ref != 'master' || entry.event != 'workflow_dispatch'
		|| (entry.subject.original_ref == 'thirdparty-${entry.target_id}'
		&& entry.expected_canonical_head != entry.subject.sha)
		|| entry.run_name != expected_run_name {
		return error('receiver ledger entry identity or trusted workflow binding is invalid')
	}
	validate_recovery_routing(entry.consumer_type, entry.resume_capability)!
	if entry.record_type == 'consumer' && entry.id != entry.intent_or_operation_id {
		return error('receiver consumer entry must preserve its original semantic consumer ID')
	}
	if entry.workflow_path != recovery_workflow_path(entry.resume_capability) {
		return error('receiver ledger entry capability and workflow path disagree')
	}
	if entry.state == 'pending' {
		if entry.selected_run_id != 0 || entry.selected_run_attempt != 0
			|| entry.receiver_master_sha != '' {
			return error('pending receiver entry cannot contain an ACK-selected run')
		}
	} else if entry.state == 'dispatched' {
		if entry.selected_run_id <= 0 || entry.selected_run_attempt <= 0
			|| !is_lower_hex_40(entry.receiver_master_sha) {
			return error('dispatched receiver entry lacks its exact selected run binding')
		}
	} else {
		return error('receiver fixture permits only pending or dispatched active entries')
	}
}

fn require_exact_keys(value JsonValue, expected []string) ! {
	if value.kind != .object {
		return error('closed contract value must be an object')
	}
	mut actual := value.object_keys.clone()
	mut wanted := expected.clone()
	actual.sort()
	wanted.sort()
	if actual != wanted {
		return error('closed contract object has missing, duplicate, or unknown members')
	}
}
