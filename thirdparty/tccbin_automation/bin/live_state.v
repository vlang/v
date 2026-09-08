module bin

import os
import sync.stdatomic
import time

$if linux || macos || freebsd || openbsd {
	#include <errno.h>
	#include <fcntl.h>
	#include <limits.h>
	#include <poll.h>
	#include <signal.h>
	#include <sys/resource.h>
	#include <sys/types.h>
	#include <sys/wait.h>
	#include <unistd.h>

	struct C.pollfd {
	mut:
		fd      int
		events  i16
		revents i16
	}

	struct C.rlimit {
		rlim_cur u64
		rlim_max u64
	}

	fn C.poll(fds &C.pollfd, nfds u64, timeout int) int
	fn C.getpgid(pid int) int
	fn C.setpgid(pid int, pgid int) int
	fn C.getrlimit(resource int, limits &C.rlimit) int
	fn C.tccbin_sigchld_read(previous &C.sigaction) int
}

$if linux {
	#include <sys/syscall.h>
}

$if freebsd || openbsd {
	fn C.closefrom(first int)
}

const durable_git_runner_slot = stdatomic.new_atomic(voidptr(unsafe { nil }))
const durable_git_config_max_bytes = 64 * 1024
const durable_git_config_max_keys = 256
const durable_git_config_key_max_bytes = 512
const durable_git_config_read_chunk_bytes = 4096
const durable_git_stderr_max_bytes = 64 * 1024
const durable_git_capability_max_bytes = 4 * 1024
const durable_git_scalar_max_bytes = 64 * 1024
const durable_git_history_max_bytes = (100_001 * 41) + 1
const durable_git_inventory_max_bytes = 16 * 1024 * 1024
const durable_git_target_blob_max_bytes = 2 * 1024 * 1024
const durable_git_evidence_blob_max_bytes = 256 * 1024
const durable_git_capability_deadline_ns = u64(5 * 1_000_000_000)
const durable_git_regular_deadline_ns = u64(30 * 1_000_000_000)
const durable_git_large_deadline_ns = u64(120 * 1_000_000_000)
const durable_git_abort_grace_ns = u64(5 * 1_000_000_000)
const durable_git_runner_poll_backoff_ms = [1, 2, 4, 8, 16, 20]
const durable_git_sigchld_drift_message = 'durable Git runner SIGCHLD ownership changed during execution'
const durable_git_reaping_exclusivity_message = 'durable Git runner lost exclusive child-reaping ownership'

enum DurableGitReadClass {
	capability
	config
	scalar
	commit
	history
	inventory
	target_blob
	evidence_blob
	log
}

enum DurableGitKillDisposition {
	secured
	retry
	hard_failure
}

enum DurableGitWaitDisposition {
	terminal
	running
	retry
	lost
	foreign
}

enum DurableGitParentAction {
	go_release
	kill
	wait
}

struct DurableGitControlState {
mut:
	go_count   int
	kill_count int
	wait_count int
	poisoned   bool
	failure    string
}

struct DurableGitSignalSnapshot {
	handler_class int
	flags         int
}

struct DurableGitConfigSnapshot {
	identity NativeFileSnapshot
	source   string
	keys     []string
}

struct DurableGitResourceSnapshot {
	current u64
	maximum u64
}

struct DurableGitRunnerSession {
mut:
	state_git_dir  string
	state_identity NativeFileSnapshot
	git_path       string
	environment    []string
	signal         DurableGitSignalSnapshot
	resources      DurableGitResourceSnapshot
	config         DurableGitConfigSnapshot
	config_ready   bool
	poisoned       bool
	poison_reason  string
	first_failure  string
	control        DurableGitControlState
}

struct DurableGitTerminationAttempt {
	group_required     bool
	group_secured      bool
	pid_secured        bool
	checkpoint_failure string
	hard_failure       string
}

struct DurableGitKillResult {
	secured            bool
	checkpoint_failure string
	hard_failure       string
}

struct DurableGitAbortState {
mut:
	requested           bool
	termination_secured bool
	signal_drift        bool
	deadline            u64
	failure             string
}

struct DurableGitCapturedResult {
	exit_code int
	stdout    string
	stderr    string
	trace     []string
}

struct DurableGitPumpState {
mut:
	stdout          []u8
	stderr          []u8
	stdout_eof      bool
	stderr_eof      bool
	combined_bytes  int
	failure_message string
}

struct DurableGitPipeRead {
	eof      bool
	combined int
	progress bool
}

const live_target_state_max_bytes = 2 * 1024 * 1024
const live_source_evidence_max_bytes = 256 * 1024
const live_state_max_tracked_files = 100_000
const live_state_max_tracked_bytes = i64(16 * 1024 * 1024)
const live_state_commit_proof_max_bytes = 16 * 1024
const live_state_max_first_parent_commits = 100_000

struct LiveStateInventory {
	blobs map[string]string
}

// LiveStateInventoryBlob is the closed, already acquired physical input to the pure inventory
// validator. It deliberately carries no repository handle or lazy loader.
struct LiveStateInventoryBlob {
	path   string
	mode   string
	kind   string
	oid    string
	size   i64
	source string
}

struct LiveStateProofBundle {
	directory        string
	head             LiveStateCommitProof
	historical_paths map[string]string
}

struct LiveSourceTerminalBinding {
	target_id              string
	handoff_id             string
	business_operation_id  string
	business_evidence_path string
	handoff                JsonValue
	proof                  JsonValue
	transition             JsonValue
	evidence_path          string
}

struct LiveEvidenceHistory {
	creation_commits map[string]string
	first_parent     []string
	parents          map[string]string
}

struct LiveSourceAtomicContext {
	binding             LiveSourceTerminalBinding
	parent_target       JsonValue
	target_target       JsonValue
	parent_source       JsonValue
	target_source       JsonValue
	target_operations   []JsonValue
	selected_attempt    JsonValue
	base_generation     i64
	cas_attempt         int
	source_path         string
	source_operation_id string
}

pub struct LiveStateTrust {
pub:
	repository          string
	state_writer_app_id i64
	actor_login         string
	actor_node_id       string
	actor_database_id   i64
}

// LiveAtomicGroupContractFact is the closed, content-addressed shape used by the grouped T
// validator. It is public only so the 3k+s union can be tested without weakening Git/schema
// authentication around the production call site.
pub struct LiveAtomicGroupContractFact {
pub:
	target_id                     string
	source_operation_id           string
	source_signature              string
	target_evidence_operation_ids []string
}

// validate_live_atomic_group_contract proves target evidence isolation, exact source sharing and
// the complete 3k+s evidence union independently of binding order.
pub fn validate_live_atomic_group_contract(facts []LiveAtomicGroupContractFact,
	observed_evidence_operation_ids []string) ! {
	if facts.len == 0 {
		return error('live atomic group contract is empty')
	}
	mut target_ids := []string{}
	mut source_signatures := map[string]string{}
	mut expected_ids := []string{}
	for fact in facts {
		if fact.target_id in target_ids || fact.target_evidence_operation_ids.len != 3 {
			return error('live atomic group repeats a target or lacks three target evidences')
		}
		target_ids << fact.target_id
		for operation_id in fact.target_evidence_operation_ids {
			if !is_lower_hex_64(operation_id) || operation_id in expected_ids {
				return error('live atomic group shares or malforms target evidence')
			}
			expected_ids << operation_id
		}
		if fact.source_operation_id in source_signatures {
			previous_signature := source_signatures[fact.source_operation_id]
			if previous_signature != fact.source_signature {
				return error('live atomic group partially shares non-identical source facts')
			}
		} else {
			if !is_lower_hex_64(fact.source_operation_id) || !is_lower_hex_64(fact.source_signature) {
				return error('live atomic group source identity is malformed')
			}
			source_signatures[fact.source_operation_id] = fact.source_signature
		}
	}
	for source_operation_id, _ in source_signatures {
		if source_operation_id in expected_ids {
			return error('live atomic group shares source and target evidence')
		}
		expected_ids << source_operation_id
	}
	mut observed := observed_evidence_operation_ids.clone()
	mut expected := expected_ids.clone()
	observed.sort()
	expected.sort()
	if observed != expected || observed.len != 3 * facts.len + source_signatures.len {
		return error('live atomic group evidence union differs from exact 3k+s')
	}
}

// live_evidence_history_count_is_within_bound exposes the exact first-parent recovery ceiling to
// a boundary test without constructing 100001 physical Git commits.
pub fn live_evidence_history_count_is_within_bound(count int) bool {
	return count > 0 && count <= live_state_max_first_parent_commits
}

// LiveReceiverInspection is derived only from one schema-valid target file on the protected
// state ref. It never accepts target, ref, capability, or publication selectors from dispatch.
pub struct LiveReceiverInspection {
pub:
	status          string
	state_commit    string
	target          ReceiverTargetSnapshot
	entry           ReceiverLedgerEntry
	canonical_ref   string
	subject_ref     string
	publish_allowed bool
}

// LiveReceiverDecision makes absent or stale state an explicit no-op while keeping malformed
// durable state fail-closed.
pub struct LiveReceiverDecision {
pub:
	status     string
	resolution ReceiverResolution
}

// LiveNativeGateAction is the complete read-only decision for the opaque native consumer input.
// Only the two create-only publisher actions can escape dark mode; canonical publication is never
// representable by this contract.
pub struct LiveNativeGateAction {
pub:
	action                     string
	state_commit               string
	target_id                  string
	consumer_id                string
	consumer_kind              string
	intent_or_operation_id     string
	subject_hash               string
	subject_sha                string
	expected_ref               string
	expected_ledger_generation i64
	active_gate_epoch          int
	trigger_id                 string
	create_only                bool
	publish_allowed            bool
}

// LiveStateCommitProof is independently fetched from GitHub's public commit endpoint after the
// exact state ref has been resolved. The local checkout must match its commit/tree/parent tuple.
pub struct LiveStateCommitProof {
pub:
	repository            string
	ref                   string
	commit_sha            string
	remote_head           string
	tree_sha              string
	parent_shas           []string
	verification_verified bool
	verification_reason   string
	verified_at           string
	state_writer_app_id   i64
	actor_login           string
	actor_node_id         string
	actor_database_id     i64
	actor_type            string
}

// authenticate_live_state_commit binds the unique HEAD proof in a closed on-disk proof bundle to
// the clean local state checkout before any ledger file is consumed.
pub fn authenticate_live_state_commit(state_git_dir string, trust LiveStateTrust,
	proof_bundle_dir string) !LiveStateCommitProof {
	return authenticate_live_state_proof_bundle(state_git_dir, trust, proof_bundle_dir)!.head
}

fn authenticate_live_state_proof_bundle(state_git_dir string, trust LiveStateTrust,
	proof_bundle_dir string) !LiveStateProofBundle {
	validate_live_proof_directory(proof_bundle_dir, 'bundle')!
	mut bundle_entries := os.ls(proof_bundle_dir)!
	bundle_entries.sort()
	if bundle_entries != ['head.json', 'historical'] {
		return error('live state proof bundle must contain exactly head.json and historical')
	}
	head_path := os.join_path(proof_bundle_dir, 'head.json')
	historical_dir := os.join_path(proof_bundle_dir, 'historical')
	head_source := read_live_proof_file(head_path)!
	validate_live_proof_directory(historical_dir, 'historical')!
	head := live_state_commit_proof_from_value(parse_strict_json(head_source)!)!
	authenticate_live_state_commit_value(state_git_dir, trust, head, head.commit_sha, true)!
	mut historical_entries := os.ls(historical_dir)!
	historical_entries.sort()
	mut historical_paths := map[string]string{}
	for name in historical_entries {
		if !name.ends_with('.json') {
			return error('live state historical proof bundle contains a non-proof entry')
		}
		sha := name.all_before_last('.json')
		if !is_lower_hex_40(sha) || sha == head.commit_sha || sha in historical_paths {
			return error('live state historical proof filename is invalid or duplicated')
		}
		path := os.join_path(historical_dir, name)
		validate_live_proof_file(path)!
		historical_paths[sha] = path
	}
	return LiveStateProofBundle{
		directory:        proof_bundle_dir
		head:             head
		historical_paths: historical_paths
	}
}

fn validate_live_proof_directory(path string, role string) ! {
	if path == '' || !os.is_abs_path(path) || os.real_path(path) != path || os.is_link(path)
		|| !os.is_dir(path) {
		return error('live state proof ${role} must be an exact absolute non-symlink directory')
	}
}

fn validate_live_proof_file(path string) ! {
	if !os.is_abs_path(path) || os.real_path(path) != path || os.is_link(path) || !os.is_file(path) {
		return error('live state proof must be an exact absolute regular non-symlink file')
	}
}

fn read_live_proof_file(path string) !string {
	validate_live_proof_file(path)!
	source := os.read_file(path)!
	if source.len == 0 || source.len > live_state_commit_proof_max_bytes {
		return error('live state proof is absent or exceeds its per-file byte bound')
	}
	return source
}

fn authenticate_live_historical_proof(state_git_dir string, trust LiveStateTrust,
	bundle LiveStateProofBundle, commit_sha string) !LiveStateCommitProof {
	path := bundle.historical_paths[commit_sha] or {
		return error('historical source commit T lacks one proof file named by its SHA')
	}
	proof := live_state_commit_proof_from_value(parse_strict_json(read_live_proof_file(path)!)!)!
	if proof.commit_sha != commit_sha {
		return error('historical proof body differs from its filename identity')
	}
	authenticate_live_state_commit_value(state_git_dir, trust, proof, bundle.head.commit_sha, false)!
	return proof
}

fn live_state_commit_proof_from_value(root JsonValue) !LiveStateCommitProof {
	require_exact_keys(root, ['schema_version', 'repository', 'ref', 'commit_sha', 'remote_head',
		'tree_sha', 'parent_shas', 'verification_verified', 'verification_reason', 'verified_at',
		'state_writer_app_id', 'actor_login', 'actor_node_id', 'actor_database_id', 'actor_type'])!
	if require_integer_member(root, 'schema_version')! != 1 {
		return error('live state commit proof version is outside its closed contract')
	}
	mut parents := []string{}
	for value in require_array_member(root, 'parent_shas')! {
		parent := require_string(value)!
		if !is_lower_hex_40(parent) || parent in parents {
			return error('live state commit proof contains an invalid or duplicate parent')
		}
		parents << parent
	}
	return LiveStateCommitProof{
		repository:            require_string_member(root, 'repository')!
		ref:                   require_string_member(root, 'ref')!
		commit_sha:            require_string_member(root, 'commit_sha')!
		remote_head:           require_string_member(root, 'remote_head')!
		tree_sha:              require_string_member(root, 'tree_sha')!
		parent_shas:           parents
		verification_verified: require_bool_member(root, 'verification_verified')!
		verification_reason:   require_string_member(root, 'verification_reason')!
		verified_at:           require_string_member(root, 'verified_at')!
		state_writer_app_id:   require_integer_member(root, 'state_writer_app_id')!
		actor_login:           require_string_member(root, 'actor_login')!
		actor_node_id:         require_string_member(root, 'actor_node_id')!
		actor_database_id:     require_integer_member(root, 'actor_database_id')!
		actor_type:            require_string_member(root, 'actor_type')!
	}
}

fn authenticate_live_state_commit_value(state_git_dir string, trust LiveStateTrust,
	proof LiveStateCommitProof, expected_remote_head string, is_head bool) ! {
	if trust.repository !in ['vlang/v', 'GGRei/v'] || proof.repository != trust.repository
		|| proof.ref != state_ref || !is_lower_hex_40(proof.commit_sha)
		|| proof.remote_head != expected_remote_head || !is_lower_hex_40(proof.tree_sha)
		|| proof.parent_shas.len != 1 || !proof.verification_verified
		|| proof.verification_reason != 'valid' || !timestamp_is_exact(proof.verified_at)
		|| trust.state_writer_app_id <= 0 || proof.state_writer_app_id != trust.state_writer_app_id
		|| proof.actor_login != trust.actor_login || proof.actor_node_id != trust.actor_node_id
		|| proof.actor_database_id != trust.actor_database_id || proof.actor_type != 'Bot' {
		return error('live state commit proof is unsigned, stale, or outside the allowlisted ref')
	}
	if (is_head && proof.commit_sha != expected_remote_head)
		|| (!is_head && proof.commit_sha == expected_remote_head) {
		return error('live state commit proof has the wrong HEAD or historical role')
	}
	validate_live_state_bare_repository(state_git_dir, expected_remote_head)!
	shallow := live_git_args(state_git_dir, ['rev-parse', '--is-shallow-repository'], .scalar)
	if shallow.exit_code != 0 || shallow.output.trim_space() != 'false' {
		return error('history_recovery_required: live state public proof cannot authenticate a shallow history')
	}
	local_tree := live_git_args(state_git_dir, ['rev-parse', '${proof.commit_sha}^{tree}'], .scalar)
	local_line := live_git_args(state_git_dir,
		['rev-list', '--parents', '-n', '1', proof.commit_sha], .scalar)
	if local_tree.exit_code != 0 || local_tree.output.trim_space() != proof.tree_sha
		|| local_line.exit_code != 0 {
		return error('live state local tree cannot be authenticated')
	}
	parts := local_line.output.trim_space().split(' ')
	local_parents := if parts.len > 1 { parts[1..] } else { []string{} }
	if local_parents != proof.parent_shas {
		return error('live state local parent tuple differs from the public commit proof')
	}
}

// inspect_live_receiver_state reads only targets/<target-id>.json from an independently checked
// out state ref. Missing state/target/consumer is an uninitialized or dark no-op, never an error.
pub fn inspect_live_receiver_state(automation_root string, state_git_dir string,
	trust LiveStateTrust, proof_bundle_dir string, opaque_id string) !LiveReceiverInspection {
	if !is_lower_hex_64(opaque_id) {
		return error('live receiver accepts only one opaque lowercase SHA-256 identifier')
	}
	if !os.is_dir(state_git_dir) {
		return LiveReceiverInspection{
			status: 'uninitialized'
		}
	}
	proof_bundle := authenticate_live_state_proof_bundle(state_git_dir, trust, proof_bundle_dir) or {
		return LiveReceiverInspection{
			status: live_state_failure_status(err.msg())
		}
	}
	proof := proof_bundle.head
	inventory := load_live_state_inventory(automation_root, state_git_dir, trust, proof_bundle) or {
		return LiveReceiverInspection{
			status:       live_state_failure_status(err.msg())
			state_commit: proof.commit_sha
		}
	}
	mut active := []LiveReceiverInspection{}
	mut opaque_occurrences := 0
	for target_id in managed_target_ids {
		path := target_state_path(target_id)!
		source := inventory.blobs[path] or {
			return LiveReceiverInspection{
				status:       'corrupt_blocked'
				state_commit: proof.commit_sha
			}
		}
		root := parse_strict_json(source)!
		if require_integer_member(root, 'schema_version')! != 1
			|| require_string_member(root, 'target_id')! != target_id {
			return error('live target state version or filename identity is invalid')
		}
		handoffs := require_array_member(root, 'recovery_handoffs')!
		matches_any :=
			handoffs.filter(require_string_member(it, 'handoff_id') or { '' } == opaque_id)
		opaque_occurrences += matches_any.len
		active_id := require_nullable_string_member(root, 'active_recovery_handoff_id')!
		if active_id != '' {
			pointer_matches :=
				handoffs.filter(require_string_member(it, 'handoff_id') or { '' } == active_id)
			if pointer_matches.len != 1 {
				return LiveReceiverInspection{
					status:       'corrupt_blocked'
					state_commit: proof.commit_sha
				}
			}
		}
		if active_id != opaque_id {
			continue
		}
		matches := handoffs.filter(require_string_member(it, 'handoff_id') or { '' } == opaque_id)
		if matches.len != 1 {
			return LiveReceiverInspection{
				status:       'corrupt_blocked'
				state_commit: proof.commit_sha
			}
		}
		entry := receiver_entry_from_handoff(matches[0])!
		if entry.state !in ['pending', 'dispatched'] {
			continue
		}
		subject_value := require_object_member(root, 'native_gate_subject')!
		active_subject := parse_receiver_subject(subject_value)!
		target := ReceiverTargetSnapshot{
			target_id:                  target_id
			generation:                 require_integer_member(root, 'generation')!
			canonical_head:             require_string_member(root, 'canonical_observed_sha')!
			input_fingerprint:          require_string_member(root, 'input_fingerprint')!
			artifact_fingerprint:       require_string_member(root, 'artifact_fingerprint')!
			manifest_hash:              require_string_member(root, 'manifest_hash')!
			active_consumer_id:         active_subject.consumer_id
			active_recovery_handoff_id: active_id
			active_subject_hash:        require_string_member(root, 'active_subject_hash')!
		}
		active << LiveReceiverInspection{
			status:        'active'
			state_commit:  proof.commit_sha
			target:        target
			entry:         entry
			canonical_ref: canonical_ref(target_id)
			subject_ref:   entry.subject.original_ref
		}
	}
	if opaque_occurrences > 1 {
		return LiveReceiverInspection{
			status:       'corrupt_blocked'
			state_commit: proof.commit_sha
		}
	}
	if active.len > 1 {
		return LiveReceiverInspection{
			status:       'corrupt_blocked'
			state_commit: proof.commit_sha
		}
	}
	if active.len == 1 {
		return active[0]
	}
	return LiveReceiverInspection{
		status:       'dark_no_op'
		state_commit: proof.commit_sha
	}
}

// resolve_live_receiver_request reuses the exact receiver authenticator after live state
// projection. Independently observed ref drift is a stale no-op; forged workflow/run facts fail.
pub fn resolve_live_receiver_request(automation_root string, state_root string,
	trust LiveStateTrust, proof_bundle_dir string, request ReceiverRequestFacts) !LiveReceiverDecision {
	inspection := inspect_live_receiver_state(automation_root, state_root, trust, proof_bundle_dir,
		request.opaque_id)!
	if inspection.status != 'active' {
		return LiveReceiverDecision{
			status: inspection.status
		}
	}
	if request.requested_publish {
		return error('live receiver never accepts a publication selector')
	}
	entry := inspection.entry
	if request.repository != entry.receiver_repository || request.workflow_id != entry.workflow_id
		|| request.workflow_path != entry.workflow_path
		|| request.workflow_ref != entry.workflow_ref || request.event != entry.event {
		return LiveReceiverDecision{
			status: 'dark_no_op'
		}
	}
	if request.observed_canonical_head != inspection.entry.expected_canonical_head
		|| request.observed_subject_ref_head != inspection.entry.subject_ref_head {
		return LiveReceiverDecision{
			status: 'dark_no_op'
		}
	}
	if (entry.state == 'pending' && (request.current_run_id != 0
		|| request.current_run_attempt != 0 || request.current_head_sha != ''
		|| request.current_run_name != '')) || (entry.state == 'dispatched'
		&& (request.current_run_id != entry.selected_run_id
		|| request.current_run_attempt != entry.selected_run_attempt
		|| request.current_head_sha != entry.receiver_master_sha
		|| request.current_run_name != entry.run_name)) {
		return LiveReceiverDecision{
			status: 'dark_no_op'
		}
	}
	resolution := resolve_receiver_request_from_ledger(ReceiverStateLedger{
		schema_version: 1
		audience:       'vlang/v:tccbin-automation-state'
		targets:        [inspection.target]
		entries:        [inspection.entry]
	}, request)!
	return LiveReceiverDecision{
		status:     'active'
		resolution: resolution
	}
}

// resolve_live_native_gate_action derives authority only from the authenticated target state.
// An absent/stale consumer is a dark no-op; a malformed inventory is corrupt-blocked.
pub fn resolve_live_native_gate_action(automation_root string, state_git_dir string,
	trust LiveStateTrust, proof_bundle_dir string, opaque_id string) !LiveNativeGateAction {
	if !is_lower_hex_64(opaque_id) {
		return error('live native gate accepts only one opaque lowercase SHA-256 identifier')
	}
	if !os.is_dir(state_git_dir) {
		return LiveNativeGateAction{
			action: 'dark_no_op'
		}
	}
	proof_bundle := authenticate_live_state_proof_bundle(state_git_dir, trust, proof_bundle_dir) or {
		return LiveNativeGateAction{
			action: live_state_failure_status(err.msg())
		}
	}
	proof := proof_bundle.head
	inventory := load_live_state_inventory(automation_root, state_git_dir, trust, proof_bundle) or {
		return LiveNativeGateAction{
			action:       live_state_failure_status(err.msg())
			state_commit: proof.commit_sha
		}
	}
	mut matches := []LiveNativeGateAction{}
	mut consumer_ids := []string{}
	for target_id in managed_target_ids {
		source := inventory.blobs[target_state_path(target_id)!] or {
			return live_native_corrupt(proof.commit_sha)
		}
		root := parse_strict_json(source) or { return live_native_corrupt(proof.commit_sha) }
		subject_value := require_member(root, 'native_gate_subject') or {
			return live_native_corrupt(proof.commit_sha)
		}
		if subject_value.kind == .null_value {
			pre_subject := derive_live_pre_subject_adoption(proof.commit_sha, root, target_id) or {
				return live_native_corrupt(proof.commit_sha)
			}
			if pre_subject.consumer_id != '' {
				if pre_subject.consumer_id in consumer_ids {
					return live_native_corrupt(proof.commit_sha)
				}
				consumer_ids << pre_subject.consumer_id
				if pre_subject.consumer_id == opaque_id {
					matches << pre_subject
				}
			}
			continue
		}
		if subject_value.kind != .object {
			return live_native_corrupt(proof.commit_sha)
		}
		recovery_subject := parse_receiver_subject(subject_value) or {
			return live_native_corrupt(proof.commit_sha)
		}
		subject := native_subject_from_recovery(recovery_subject)
		validate_native_gate_subject(subject) or { return live_native_corrupt(proof.commit_sha) }
		if subject.consumer_id in consumer_ids {
			return live_native_corrupt(proof.commit_sha)
		}
		consumer_ids << subject.consumer_id
		execution_value := require_object_member(root, 'native_gate_execution') or {
			return live_native_corrupt(proof.commit_sha)
		}
		gate := parse_live_native_gate(execution_value, subject) or {
			return live_native_corrupt(proof.commit_sha)
		}
		owner_stage := validate_live_native_owner(root, subject) or {
			return live_native_corrupt(proof.commit_sha)
		}
		generation := require_integer_member(root, 'generation') or {
			return live_native_corrupt(proof.commit_sha)
		}
		active_hash := require_string_member(root, 'active_subject_hash') or {
			return live_native_corrupt(proof.commit_sha)
		}
		if subject.target_id != target_id || gate.subject != subject
			|| gate.subject_hash != native_gate_subject_hash(subject)!
			|| active_hash != gate.subject_hash || gate.expected_ledger_generation != generation
			|| subject.subject_generation > generation {
			return live_native_corrupt(proof.commit_sha)
		}
		if subject.consumer_id != opaque_id {
			continue
		}
		matches << derive_live_native_action(proof.commit_sha, subject, gate, owner_stage) or {
			return live_native_corrupt(proof.commit_sha)
		}
	}
	if matches.len > 1 {
		return live_native_corrupt(proof.commit_sha)
	}
	if matches.len == 1 {
		return matches[0]
	}
	return LiveNativeGateAction{
		action:       'dark_no_op'
		state_commit: proof.commit_sha
	}
}

fn live_state_failure_status(message string) string {
	if message.starts_with('history_recovery_required:') {
		return 'history_recovery_required'
	}
	if message.starts_with('unknown_blocked:') {
		return 'unknown_blocked'
	}
	return 'corrupt_blocked'
}

fn live_native_corrupt(state_commit string) LiveNativeGateAction {
	return LiveNativeGateAction{
		action:       'corrupt_blocked'
		state_commit: state_commit
	}
}

fn derive_live_native_action(state_commit string, subject NativeGateSubjectModel,
	gate NativeGateModel, _ string) !LiveNativeGateAction {
	if gate.active_gate_epoch < 0 || gate.active_gate_epoch >= gate.epochs.len {
		return error('live native gate active epoch is outside its durable history')
	}
	epoch := gate.epochs[gate.active_gate_epoch]
	mut action := 'dark_no_op'
	mut create_only := false
	if epoch.state == .open_unselected && epoch.reason != 'original_push' {
		action = 'gate_trigger_ref_create'
		create_only = true
	}
	if action !in ['candidate_ref_create', 'gate_trigger_ref_create', 'dark_no_op'] {
		return error('live native gate derived an action outside its closed set')
	}
	return LiveNativeGateAction{
		action:                     action
		state_commit:               state_commit
		target_id:                  subject.target_id
		consumer_id:                subject.consumer_id
		consumer_kind:              subject.consumer_kind
		intent_or_operation_id:     subject.intent_or_operation_id
		subject_hash:               gate.subject_hash
		subject_sha:                subject.sha
		expected_ref:               epoch.expected_ref
		expected_ledger_generation: gate.expected_ledger_generation
		active_gate_epoch:          gate.active_gate_epoch
		trigger_id:                 epoch.trigger_id
		create_only:                create_only
		publish_allowed:            false
	}
}

fn derive_live_pre_subject_adoption(state_commit string, root JsonValue,
	target_id string) !LiveNativeGateAction {
	intent_value := require_member(root, 'active_intent')!
	if intent_value.kind == .null_value {
		return LiveNativeGateAction{}
	}
	if intent_value.kind != .object {
		return error('live pre-subject intent has the wrong type')
	}
	intent_type := require_string_member(intent_value, 'intent_type')!
	if intent_type !in ['adopt-current', 'initial_adopt_current'] {
		return LiveNativeGateAction{}
	}
	intent_id := require_string_member(intent_value, 'intent_id')!
	stage := require_string_member(intent_value, 'stage')!
	generation := require_integer_member(root, 'generation')!
	intent_generation := require_integer_member(intent_value, 'generation')!
	canonical_head := require_string_member(root, 'canonical_observed_sha')!
	input_fingerprint := require_string_member(root, 'input_fingerprint')!
	candidate_ref := require_string_member(intent_value, 'candidate_ref')!
	validation := require_object_member(intent_value, 'validation_subject')!
	validation_sha := require_string_member(validation, 'sha')!
	validation_digests := parse_live_digests(require_array_member(validation, 'digests')!)!
	validate_live_digest_models(validation_digests)!
	if stage != 'intent_reserved' || !is_lower_hex_64(intent_id) || intent_generation != generation
		|| require_string_member(intent_value, 'expected_canonical_head')! != canonical_head
		|| require_string_member(intent_value, 'input_fingerprint')! != input_fingerprint
		|| validation_sha != canonical_head
		|| require_string_member(validation, 'input_fingerprint')! != input_fingerprint
		|| require_string_member(validation, 'artifact_fingerprint')! != require_string_member(root, 'artifact_fingerprint')!
		|| require_string_member(validation, 'manifest_hash')! != require_string_member(root, 'manifest_hash')!
		|| candidate_ref != 'tccbin-candidate/${target_id}/${intent_id}'
		|| require_string_member(validation, 'candidate_ref')! != candidate_ref
		|| require_member(root, 'native_gate_execution')!.kind != .null_value
		|| require_member(root, 'active_subject_hash')!.kind != .null_value {
		return error('live pre-subject adoption differs from its durable target binding')
	}
	return LiveNativeGateAction{
		action:                     'candidate_ref_create'
		state_commit:               state_commit
		target_id:                  target_id
		consumer_id:                intent_id
		consumer_kind:              if intent_type == 'adopt-current' {
			'adopt_current'
		} else {
			'initial_adopt_current'
		}
		intent_or_operation_id:     intent_id
		subject_sha:                validation_sha
		expected_ref:               candidate_ref
		expected_ledger_generation: generation
		create_only:                true
		publish_allowed:            false
	}
}

fn validate_live_native_owner(root JsonValue, subject NativeGateSubjectModel) !string {
	intent_value := require_member(root, 'active_intent')!
	active_remediation_id := require_nullable_string_member(root, 'active_remediation_id')!
	post_validation_operation_id := require_nullable_string_member(root,
		'post_validation_operation_id')!
	if require_string_member(root, 'input_fingerprint')! != subject.input_fingerprint
		|| require_string_member(root, 'artifact_fingerprint')! != subject.artifact_fingerprint
		|| require_string_member(root, 'manifest_hash')! != subject.manifest_hash {
		return error('live native subject fingerprints differ from its target projection')
	}
	if subject.consumer_kind == 'remediation' {
		if active_remediation_id != subject.consumer_id || post_validation_operation_id != ''
			|| intent_value.kind != .null_value
			|| require_string_member(root, 'canonical_observed_sha')! != subject.sha {
			return error('live remediation subject differs from its durable operation owner')
		}
		return 'remediation'
	}
	if intent_value.kind != .object || active_remediation_id != '' {
		return error('live native intent subject lacks its unique durable owner')
	}
	intent_id := require_string_member(intent_value, 'intent_id')!
	intent_type := require_string_member(intent_value, 'intent_type')!
	stage := require_string_member(intent_value, 'stage')!
	intent_generation := require_integer_member(intent_value, 'generation')!
	if !is_lower_hex_64(intent_id)
		|| require_string_member(intent_value, 'candidate_ref')! != 'tccbin-candidate/${subject.target_id}/${intent_id}' {
		return error('live native intent identity or candidate ref is invalid')
	}
	if subject.consumer_kind in ['publish_post', 'rollback_post'] {
		expected_type := if subject.consumer_kind == 'publish_post' { 'publish' } else { 'rollback' }
		if intent_type != expected_type || subject.consumer_id != subject.intent_or_operation_id
			|| post_validation_operation_id != subject.consumer_id
			|| require_string_member(intent_value, 'input_fingerprint')! != subject.input_fingerprint
			|| intent_generation >= subject.subject_generation
			|| stage !in ['post_checks_running', 'post_checks_waiting_source', 'blocked']
			|| require_string_member(root, 'canonical_observed_sha')! != subject.sha {
			return error('live post-validation subject differs from its durable operation')
		}
		post_tuple := if subject.consumer_kind == 'publish_post' {
			require_object_member(root, 'provisional_published')!
		} else {
			require_object_member(intent_value, 'rollback_provisional')!
		}
		validate_live_subject_artifact_tuple(post_tuple, subject, false)!
		return stage
	}
	if post_validation_operation_id != '' {
		return error('live candidate subject overlaps a post-validation operation')
	}
	if intent_id != subject.consumer_id || intent_id != subject.intent_or_operation_id
		|| require_string_member(intent_value, 'input_fingerprint')! != subject.input_fingerprint
		|| intent_generation >= subject.subject_generation {
		return error('live native subject differs from its durable intent identity')
	}
	expected_kind := match intent_type {
		'publish' { 'publish_candidate' }
		'rollback' { 'rollback_candidate' }
		'adopt-current' { 'adopt_current' }
		'initial_adopt_current' { 'initial_adopt_current' }
		else { return error('live native intent type is outside its closed set') }
	}
	if subject.consumer_kind != expected_kind
		|| require_string_member(intent_value, 'candidate_ref')! != subject.original_ref
		|| require_string_member(intent_value, 'expected_canonical_head')! != require_string_member(root, 'canonical_observed_sha')! {
		return error('live candidate subject kind or ref differs from its durable intent')
	}
	if intent_type in ['adopt-current', 'initial_adopt_current'] {
		validation := require_object_member(intent_value, 'validation_subject')!
		validate_live_subject_artifact_tuple(validation, subject, true)!
		if require_string_member(validation, 'candidate_ref')! != subject.original_ref {
			return error('live adoption validation ref differs from its native subject')
		}
	} else {
		binding := require_object_member(intent_value, 'candidate_binding')!
		validate_live_subject_artifact_tuple(binding, subject, false)!
		if require_string_member(binding, 'parent')! != require_string_member(intent_value,
			'expected_canonical_head')! {
			return error('live candidate binding parent differs from its durable expected HEAD')
		}
	}
	return stage
}

fn validate_live_subject_artifact_tuple(value JsonValue, subject NativeGateSubjectModel,
	includes_input bool) ! {
	if require_string_member(value, 'sha')! != subject.sha
		|| require_string_member(value, 'tree')! != subject.tree
		|| require_string_member(value, 'artifact_fingerprint')! != subject.artifact_fingerprint
		|| require_string_member(value, 'manifest_hash')! != subject.manifest_hash
		|| (includes_input
		&& require_string_member(value, 'input_fingerprint')! != subject.input_fingerprint)
		|| parse_live_digests(require_array_member(value, 'digests')!)! != subject.digests {
		return error('live native subject differs from its durable artifact tuple')
	}
}

fn parse_live_digests(values []JsonValue) ![]DigestModel {
	mut digests := []DigestModel{cap: values.len}
	for value in values {
		digests << DigestModel{
			path:   require_string_member(value, 'path')!
			sha256: require_string_member(value, 'sha256')!
		}
	}
	return digests
}

fn validate_live_digest_models(digests []DigestModel) ! {
	if digests.len == 0 {
		return error('live artifact digest set is empty')
	}
	mut paths := []string{}
	for digest in digests {
		if !contract_relative_path_is_safe(digest.path) || !is_lower_hex_64(digest.sha256)
			|| digest.path in paths {
			return error('live artifact digest set is invalid or duplicated')
		}
		paths << digest.path
	}
}

fn parse_live_string_array(values []JsonValue) ![]string {
	mut result := []string{cap: values.len}
	for value in values {
		result << require_string(value)!
	}
	return result
}

fn parse_live_native_gate(value JsonValue, expected_subject NativeGateSubjectModel) !NativeGateModel {
	require_exact_keys(value, ['subject', 'subject_hash', 'subject_sha', 'subject_generation',
		'repository', 'workflow_id', 'workflow_path', 'original_actor',
		'original_actor_integration_id', 'rerun_triggering_actor', 'rerun_triggering_integration_id',
		'expected_ledger_generation', 'active_gate_epoch', 'gate_epochs', 'gate_runs',
		'ack_operation_ids', 'completion_operation_ids', 'epoch_close_operation_ids',
		'selected_run_id', 'selected_run_attempt', 'selected_check_suite_id', 'selected_conclusion',
		'infra_retry_count', 'source_recovery_operation_id'])!
	nested_subject := native_subject_from_recovery(parse_receiver_subject(require_object_member(value,
		'subject')!)!)
	if nested_subject != expected_subject {
		return error('live native execution embeds a different immutable subject')
	}
	mut epochs := []GateEpochModel{}
	for epoch_value in require_array_member(value, 'gate_epochs')! {
		require_exact_keys(epoch_value, ['epoch', 'reason', 'expected_ref', 'trigger_id', 'state',
			'selected_run_id', 'selected_run_attempt', 'selected_check_suite_id', 'conclusion',
			'opened_at', 'closed_at', 'source_recovery_operation_id'])!
		epochs << GateEpochModel{
			epoch:                        int(require_integer_member(epoch_value, 'epoch')!)
			reason:                       require_string_member(epoch_value, 'reason')!
			expected_ref:                 require_string_member(epoch_value, 'expected_ref')!
			trigger_id:                   require_nullable_string_member(epoch_value, 'trigger_id')!
			state:                        parse_live_gate_epoch_state(require_string_member(epoch_value,
				'state')!)!
			selected_run_id:              require_nullable_integer(epoch_value, 'selected_run_id')!
			selected_run_attempt:         int(require_nullable_integer(epoch_value,
				'selected_run_attempt')!)
			selected_check_suite_id:      require_nullable_integer(epoch_value,
				'selected_check_suite_id')!
			conclusion:                   require_nullable_string_member(epoch_value, 'conclusion')!
			opened_at:                    require_string_member(epoch_value, 'opened_at')!
			closed_at:                    require_nullable_string_member(epoch_value, 'closed_at')!
			source_recovery_operation_id: require_nullable_string_member(epoch_value,
				'source_recovery_operation_id')!
		}
	}
	mut runs := []GateRunCandidate{}
	for run_value in require_array_member(value, 'gate_runs')! {
		require_exact_keys(run_value, ['gate_epoch', 'run_id', 'run_attempt', 'repository', 'ref',
			'sha', 'event', 'actor', 'actor_integration_id', 'triggering_actor',
			'triggering_actor_integration_id', 'check_suite_id', 'workflow_id', 'workflow_path',
			'created_at', 'conclusion'])!
		runs << GateRunCandidate{
			epoch:                           int(require_integer_member(run_value, 'gate_epoch')!)
			run_id:                          require_integer_member(run_value, 'run_id')!
			run_attempt:                     int(require_integer_member(run_value, 'run_attempt')!)
			repository:                      require_string_member(run_value, 'repository')!
			ref:                             require_string_member(run_value, 'ref')!
			sha:                             require_string_member(run_value, 'sha')!
			event:                           require_string_member(run_value, 'event')!
			actor:                           require_string_member(run_value, 'actor')!
			actor_integration_id:            require_integer_member(run_value,
				'actor_integration_id')!
			triggering_actor:                require_string_member(run_value, 'triggering_actor')!
			triggering_actor_integration_id: require_integer_member(run_value,
				'triggering_actor_integration_id')!
			check_suite_id:                  require_integer_member(run_value, 'check_suite_id')!
			workflow_id:                     require_integer_member(run_value, 'workflow_id')!
			workflow_path:                   require_string_member(run_value, 'workflow_path')!
			created_at:                      require_string_member(run_value, 'created_at')!
			conclusion:                      require_string_member(run_value, 'conclusion')!
		}
	}
	gate := NativeGateModel{
		subject:                      nested_subject
		subject_hash:                 require_string_member(value, 'subject_hash')!
		subject_sha:                  require_string_member(value, 'subject_sha')!
		subject_generation:           require_integer_member(value, 'subject_generation')!
		expected_ledger_generation:   require_integer_member(value, 'expected_ledger_generation')!
		authentication:               GateRunAuthentication{
			repository:                      require_string_member(value, 'repository')!
			workflow_id:                     require_integer_member(value, 'workflow_id')!
			workflow_path:                   require_string_member(value, 'workflow_path')!
			original_actor:                  require_string_member(value, 'original_actor')!
			original_actor_integration_id:   require_integer_member(value,
				'original_actor_integration_id')!
			rerun_triggering_actor:          require_string_member(value, 'rerun_triggering_actor')!
			rerun_triggering_integration_id: require_integer_member(value,
				'rerun_triggering_integration_id')!
		}
		active_gate_epoch:            int(require_integer_member(value, 'active_gate_epoch')!)
		epochs:                       epochs
		gate_runs:                    runs
		ack_operation_ids:            parse_live_string_array(require_array_member(value,
			'ack_operation_ids')!)!
		completion_operation_ids:     parse_live_string_array(require_array_member(value,
			'completion_operation_ids')!)!
		epoch_close_operation_ids:    parse_live_string_array(require_array_member(value,
			'epoch_close_operation_ids')!)!
		selected_run_id:              require_nullable_integer(value, 'selected_run_id')!
		selected_run_attempt:         int(require_nullable_integer(value, 'selected_run_attempt')!)
		selected_check_suite_id:      require_nullable_integer(value, 'selected_check_suite_id')!
		selected_conclusion:          require_nullable_string_member(value, 'selected_conclusion')!
		infra_retry_count:            int(require_integer_member(value, 'infra_retry_count')!)
		source_recovery_operation_id: require_nullable_string_member(value,
			'source_recovery_operation_id')!
	}
	validate_native_gate(gate)!
	return gate
}

fn parse_live_gate_epoch_state(value string) !GateEpochState {
	return match value {
		'open_unselected' { .open_unselected }
		'selected' { .selected }
		'closed_timed_out' { .closed_timed_out }
		'closed_not_rerunnable' { .closed_not_rerunnable }
		'completed' { .completed }
		else { error('live native gate epoch state is outside its closed set') }
	}
}

fn receiver_entry_from_handoff(value JsonValue) !ReceiverLedgerEntry {
	entry := ReceiverLedgerEntry{
		id:                         require_string_member(value, 'handoff_id')!
		record_type:                'handoff'
		target_id:                  require_string_member(require_object_member(value, 'subject')!,
			'target_id')!
		consumer_type:              require_string_member(value, 'consumer_type')!
		resume_capability:          require_string_member(value, 'resume_capability')!
		intent_or_operation_id:     require_string_member(value, 'intent_or_operation_id')!
		expected_ledger_generation: require_integer_member(value, 'expected_ledger_generation')!
		expected_canonical_head:    require_string_member(value, 'expected_canonical_head')!
		subject_ref_head:           require_string_member(value, 'subject_ref_head')!
		subject_hash:               require_string_member(value, 'subject_hash')!
		subject:                    parse_receiver_subject(require_object_member(value, 'subject')!)!
		receiver_repository:        require_string_member(value, 'receiver_repository')!
		workflow_id:                require_integer_member(value, 'workflow_id')!
		workflow_path:              require_string_member(value, 'workflow_path')!
		workflow_ref:               require_string_member(value, 'workflow_ref')!
		event:                      require_string_member(value, 'event')!
		run_name:                   require_string_member(value, 'receiver_run_name')!
		state:                      require_string_member(value, 'state')!
		selected_run_id:            require_nullable_integer(value, 'selected_run_id')!
		selected_run_attempt:       int(require_nullable_integer(value, 'selected_run_attempt')!)
		receiver_master_sha:        require_nullable_string_member(value, 'receiver_master_sha')!
	}
	validate_receiver_entry(entry)!
	return entry
}

fn require_nullable_integer(value JsonValue, key string) !i64 {
	member := require_member(value, key)!
	if member.kind == .null_value {
		return 0
	}
	if member.kind != .integer {
		return error('closed contract nullable integer member has the wrong type')
	}
	return member.int_value
}

fn validate_live_state_bare_repository(state_git_dir string, expected_state_head string) ! {
	if !is_lower_hex_40(expected_state_head) || !os.is_abs_path(state_git_dir)
		|| os.real_path(state_git_dir) != state_git_dir || os.is_link(state_git_dir) {
		return error('live state snapshot requires an exact bare repository and commit SHA')
	}
	validate_live_git_preflight(state_git_dir)!
	bare := live_git_args(state_git_dir, ['rev-parse', '--is-bare-repository'], .scalar)
	commit := live_git_args(state_git_dir, ['rev-parse', '${expected_state_head}^{commit}'],
		.scalar)
	if bare.exit_code != 0 || bare.output.trim_space() != 'true' || commit.exit_code != 0
		|| commit.output.trim_space() != expected_state_head {
		return error('live state snapshot is not the exact immutable bare commit')
	}
}

// T2c3c1 deliberately owns one child-reaping lease. The pointer is both the CAS token and the
// private session reached by nested authenticated readers; no public API exposes it.
fn durable_git_runner_session() !&DurableGitRunnerSession {
	token := durable_git_runner_lease_load()
	if token == unsafe { nil } {
		return error('durable Git runner has no active authenticated session')
	}
	return unsafe { &DurableGitRunnerSession(token) }
}

fn durable_git_runner_lease_load() voidptr {
	mut slot := durable_git_runner_slot
	return slot.load()
}

fn durable_git_sigchld_handler_class(action &C.sigaction) int {
	handler := voidptr(action.sa_handler)
	if handler == voidptr(C.SIG_DFL) {
		return 0
	}
	if handler == voidptr(C.SIG_IGN) {
		return 1
	}
	return 2
}

fn durable_git_signal_snapshot() !DurableGitSignalSnapshot {
	$if linux || macos || freebsd || openbsd {
		mut action := C.sigaction{}
		if C.tccbin_sigchld_read(&action) != 0 {
			return error('durable Git runner cannot observe SIGCHLD ownership')
		}
		return DurableGitSignalSnapshot{
			handler_class: durable_git_sigchld_handler_class(&action)
			flags:         action.sa_flags
		}
	} $else {
		return error('durable target commit planning is unavailable on this platform without a raw-byte Git runner')
	}
}

fn durable_git_require_default_signal() !DurableGitSignalSnapshot {
	$if linux || macos || freebsd || openbsd {
		mut action := C.sigaction{}
		if C.tccbin_sigchld_read(&action) != 0 || durable_git_sigchld_handler_class(&action) != 0
			|| (action.sa_flags & C.SA_NOCLDWAIT) != 0 {
			return error('durable Git runner requires exclusive default SIGCHLD ownership')
		}
		return DurableGitSignalSnapshot{
			handler_class: 0
			flags:         action.sa_flags
		}
	} $else {
		return error('durable target commit planning is unavailable on this platform without a raw-byte Git runner')
	}
}

fn durable_git_signal_checkpoint(expected DurableGitSignalSnapshot) ! {
	observed := durable_git_signal_snapshot() or { return error(durable_git_sigchld_drift_message) }
	if observed != expected {
		return error(durable_git_sigchld_drift_message)
	}
}

fn durable_git_poison_for_sigchld(mut session DurableGitRunnerSession) {
	session.poisoned = true
	session.poison_reason = 'durable Git runner retained its lease after SIGCHLD ownership drift'
	session.control.poisoned = true
	session.control.failure = durable_git_sigchld_drift_message
	if session.first_failure == '' {
		session.first_failure = durable_git_sigchld_drift_message
	}
}

fn durable_git_record_first_failure(mut session DurableGitRunnerSession, failure string) {
	if failure != '' && session.first_failure == '' {
		session.first_failure = failure
	}
}

fn durable_git_prioritized_failure(session &DurableGitRunnerSession, core_failure string,
	cleanup_failure string) string {
	if session.first_failure != '' {
		return session.first_failure
	}
	if core_failure != '' {
		return core_failure
	}
	return cleanup_failure
}

fn durable_git_control_checkpoint(mut control DurableGitControlState,
	checkpoint_failure string) ! {
	if checkpoint_failure == '' {
		return
	}
	control.poisoned = true
	control.failure = durable_git_sigchld_drift_message
	return error(durable_git_sigchld_drift_message)
}

fn durable_git_control_record(mut control DurableGitControlState,
	action DurableGitParentAction) ! {
	if control.poisoned {
		if control.failure == '' {
			return error(durable_git_sigchld_drift_message)
		}
		return error(control.failure)
	}
	match action {
		.go_release { control.go_count++ }
		.kill { control.kill_count++ }
		.wait { control.wait_count++ }
	}
}

fn durable_git_control_authorize(mut control DurableGitControlState,
	action DurableGitParentAction, checkpoint_failure string) ! {
	durable_git_control_checkpoint(mut control, checkpoint_failure)!
	durable_git_control_record(mut control, action)!
}

fn durable_git_control_route_postfork(mut control DurableGitControlState,
	checkpoint_failure string) ! {
	durable_git_control_checkpoint(mut control, checkpoint_failure)!
}

fn durable_git_require_signal_checkpoint(mut session DurableGitRunnerSession) ! {
	mut checkpoint_failure := ''
	durable_git_signal_checkpoint(session.signal) or {
		checkpoint_failure = durable_git_sigchld_drift_message
	}
	durable_git_control_checkpoint(mut session.control, checkpoint_failure) or {
		durable_git_poison_for_sigchld(mut session)
		return error(durable_git_sigchld_drift_message)
	}
}

fn durable_git_route_postfork_checkpoint(mut session DurableGitRunnerSession) ! {
	mut checkpoint_failure := ''
	durable_git_signal_checkpoint(session.signal) or {
		checkpoint_failure = durable_git_sigchld_drift_message
	}
	durable_git_control_route_postfork(mut session.control, checkpoint_failure) or {
		durable_git_poison_for_sigchld(mut session)
		return error(durable_git_sigchld_drift_message)
	}
}

fn durable_git_authorize_parent_action(mut session DurableGitRunnerSession,
	action DurableGitParentAction) ! {
	mut checkpoint_failure := ''
	durable_git_signal_checkpoint(session.signal) or {
		checkpoint_failure = durable_git_sigchld_drift_message
	}
	durable_git_control_authorize(mut session.control, action, checkpoint_failure) or {
		durable_git_poison_for_sigchld(mut session)
		return error(durable_git_sigchld_drift_message)
	}
}

fn durable_git_control_lease_releasable(poisoned bool) bool {
	return !poisoned
}

fn durable_git_lease_try_acquire(mut slot stdatomic.AtomicVal[voidptr], token voidptr) bool {
	return slot.compare_and_swap(voidptr(unsafe { nil }), token)
}

fn durable_git_lease_try_release(mut slot stdatomic.AtomicVal[voidptr], token voidptr) bool {
	return slot.compare_and_swap(token, voidptr(unsafe { nil }))
}

fn durable_git_resource_preflight() !DurableGitResourceSnapshot {
	$if linux || macos || freebsd || openbsd {
		mut limits := C.rlimit{}
		if C.getrlimit(C.RLIMIT_NOFILE, &limits) != 0 {
			return error('durable Git runner cannot read its descriptor resource bound')
		}
		current := limits.rlim_cur
		maximum := limits.rlim_max
		if current < 3 || current > maximum || maximum > 1_048_576 {
			return error('durable Git runner descriptor resource bound is not finite and closed')
		}
		return DurableGitResourceSnapshot{
			current: current
			maximum: maximum
		}
	} $else {
		return error('durable target commit planning is unavailable on this platform without a raw-byte Git runner')
	}
}

fn durable_git_raw_environment() ![]string {
	$if linux || macos || freebsd || openbsd {
		start := unsafe { &&char(voidptr(C.environ)) }
		mut entries := []string{}
		mut index := 0
		for {
			value := unsafe { start[index] }
			if value == unsafe { nil } {
				break
			}
			entry := unsafe { cstring_to_vstring(value) }
			if entry == '' || !entry.contains('=') {
				return error('durable Git runner inherited environment framing is invalid')
			}
			entries << entry
			index++
			if index > 4096 {
				return error('durable Git runner inherited environment exceeds its entry bound')
			}
		}
		return entries
	} $else {
		return error('durable target commit planning is unavailable on this platform without a raw-byte Git runner')
	}
}

fn durable_git_closed_environment() ![]string {
	return durable_git_closed_environment_from_entries(durable_git_raw_environment()!)
}

fn durable_git_closed_environment_from_entries(entries []string) ![]string {
	mut seen := map[string]bool{}
	mut path := ''
	redirecting := ['git_dir', 'git_work_tree', 'git_common_dir', 'git_object_directory',
		'git_alternate_object_directories', 'git_replace_ref_base', 'git_graft_file',
		'git_shallow_file', 'git_namespace', 'git_index_file', 'git_exec_path', 'git_config',
		'git_config_parameters', 'git_config_count', 'git_config_system', 'git_config_global',
		'git_config_nosystem']
	for entry in entries {
		equals := entry.index('=') or {
			return error('durable Git runner inherited environment framing is invalid')
		}
		name := entry[..equals]
		value := entry[equals + 1..]
		lower := name.to_lower()
		if name == '' || lower in seen {
			return error('durable Git runner inherited environment has a duplicate or empty name')
		}
		seen[lower] = true
		if lower in redirecting || lower.starts_with('git_config_key_')
			|| lower.starts_with('git_config_value_') {
			return error('durable Git runner inherited environment contains a Git redirection')
		}
		if lower == 'path' {
			if value == '' {
				return error('durable Git runner inherited PATH is empty')
			}
			path = value
		}
	}
	if path == '' {
		return error('durable Git runner requires exactly one inherited PATH')
	}
	return ['PATH=${path}', 'LC_ALL=C', 'LANG=C', 'LANGUAGE=C', 'GIT_NO_LAZY_FETCH=1',
		'GIT_TERMINAL_PROMPT=0', 'GIT_OPTIONAL_LOCKS=0', 'GIT_CONFIG_NOSYSTEM=1',
		'GIT_CONFIG_GLOBAL=${os.path_devnull}']
}

fn durable_git_resolve_binary() !string {
	path := os.find_abs_path_of_executable('git')!
	if !os.is_abs_path(path) || os.real_path(path) != path || os.is_link(path) || !os.is_file(path)
		|| !os.is_executable(path) {
		return error('durable Git runner binary is not one exact physical executable')
	}
	return path
}

fn durable_git_read_config_document(document &NativeToolchainDocument, expected int) ![]u8 {
	if expected <= 0 || expected > durable_git_config_max_bytes {
		return error('durable Git runner configuration violates its physical file bound')
	}
	chunk_size := if expected < durable_git_config_read_chunk_bytes {
		expected
	} else {
		durable_git_config_read_chunk_bytes
	}
	mut chunk := []u8{len: chunk_size}
	mut source := []u8{cap: expected}
	for source.len < expected {
		remaining := expected - source.len
		read := native_read_toolchain_document(document, mut chunk)!
		if read == 0 {
			return error('durable Git runner configuration exceeds or differs from its byte bound')
		}
		if read > remaining {
			return error('durable Git runner configuration exceeds or differs from its byte bound')
		}
		source << chunk[..read]
	}
	mut eof_probe := []u8{len: 1}
	if native_read_toolchain_document(document, mut eof_probe)! != 0 {
		return error('durable Git runner configuration exceeds or differs from its byte bound')
	}
	if source.len != expected {
		return error('durable Git runner configuration exceeds or differs from its byte bound')
	}
	return source
}

fn durable_git_config_snapshot(state_git_dir string) !DurableGitConfigSnapshot {
	$if linux || macos || freebsd || openbsd {
		path := os.join_path(state_git_dir, 'config')
		if os.real_path(path) != path || os.is_link(path) {
			return error('durable Git runner configuration is not one physical path')
		}
		path_before := posix_path_file_snapshot(path)!
		mut document, handle_before := native_open_toolchain_document(path)!
		defer {
			native_close_toolchain_document(mut document)
		}
		if !handle_before.regular || !handle_before.identity.reliable
			|| handle_before.identity.nlink != 1 || handle_before.size == 0
			|| handle_before.size > durable_git_config_max_bytes {
			return error('durable Git runner configuration violates its physical file bound')
		}
		source_bytes := durable_git_read_config_document(&document, int(handle_before.size))!
		handle_after := native_toolchain_document_snapshot(&document)!
		path_after := posix_path_file_snapshot(path)!
		if handle_before != handle_after || path_before != path_after || path_after != handle_after {
			return error('durable Git runner configuration changed during physical acquisition')
		}
		source := source_bytes.bytestr()
		keys := durable_git_parse_physical_config(source)!
		return DurableGitConfigSnapshot{
			identity: handle_after
			source:   source
			keys:     keys
		}
	} $else {
		return error('durable target commit planning is unavailable on this platform without a raw-byte Git runner')
	}
}

fn durable_git_require_no_sidecars(state_git_dir string) ! {
	if !os.is_abs_path(state_git_dir) || os.real_path(state_git_dir) != state_git_dir
		|| os.is_link(state_git_dir) || !os.is_dir(state_git_dir) {
		return error('durable Git runner state repository is not one physical directory')
	}
	for relative_path in ['commondir', 'shallow', 'config.worktree', 'info/grafts',
		'objects/info/alternates'] {
		path := os.join_path(state_git_dir, relative_path)
		if os.exists(path) || os.is_link(path) {
			return error('durable Git runner repository contains a disallowed redirect or topology sidecar')
		}
	}
}

fn durable_git_state_directory_snapshot(state_git_dir string) !NativeFileSnapshot {
	$if linux || macos || freebsd || openbsd {
		durable_git_require_no_sidecars(state_git_dir)!
		snapshot := native_directory_path_snapshot(state_git_dir)!
		if snapshot.regular || !snapshot.identity.reliable || snapshot.identity.nlink == 0 {
			return error('durable Git runner state repository directory identity is not physical')
		}
		return snapshot
	} $else {
		return error('durable target commit planning is unavailable on this platform without a raw-byte Git runner')
	}
}

fn durable_git_config_key_allowed(key string) bool {
	if key in ['core.repositoryformatversion', 'core.filemode', 'core.bare'] {
		return true
	}
	components := key.split('.')
	return components.len == 3 && components[0] == 'remote'
		&& components[2] in ['url', 'fetch', 'mirror']
		&& durable_git_safe_remote_name(components[1])
}

fn durable_git_physical_config_value_allowed(key string, value string) bool {
	if value == '' || value.contains('#') || value.contains(';') || value.contains('"')
		|| value.contains("'") || value.contains('\t') {
		return false
	}
	return match key {
		'core.repositoryformatversion' {
			value == '0'
		}
		'core.filemode' {
			value in ['true', 'false']
		}
		'core.bare' {
			value == 'true'
		}
		else {
			if key.ends_with('.mirror') {
				value in ['true', 'false']
			} else {
				true
			}
		}
	}
}

fn durable_git_parse_physical_config(source string) ![]string {
	if source == '' || source.len > durable_git_config_max_bytes || !source.ends_with('\n') {
		return error('durable Git runner physical configuration framing is invalid')
	}
	for byte in source.bytes() {
		if byte == 0 || byte == `\r` || (byte < 32 && byte !in [`\n`, `\t`]) || byte == 127 {
			return error('durable Git runner physical configuration contains a forbidden byte')
		}
	}
	mut section := ''
	mut keys := []string{}
	mut seen := map[string]bool{}
	mut sections := map[string]bool{}
	for line in source.split_into_lines() {
		if line == '' {
			continue
		}
		if line.contains('\\') || line.starts_with('#') || line.starts_with(';') {
			return error('durable Git runner physical configuration contains continuation or comment syntax')
		}
		if line.starts_with('[') {
			if !line.ends_with(']') || line.trim_space() != line {
				return error('durable Git runner physical configuration section is noncanonical')
			}
			if line == '[core]' {
				section = 'core'
			} else {
				prefix := '[remote "'
				suffix := '"]'
				if !line.starts_with(prefix) || !line.ends_with(suffix) {
					return error('durable Git runner physical configuration section is outside its allowlist')
				}
				remote := line[prefix.len..line.len - suffix.len]
				if !durable_git_safe_remote_name(remote) {
					return error('durable Git runner physical configuration remote name is noncanonical')
				}
				section = 'remote.${remote}'
			}
			if section in sections {
				return error('durable Git runner physical configuration repeats a section')
			}
			sections[section] = true
			continue
		}
		if section == '' || (line[0] != `\t` && line[0] != ` `) {
			return error('durable Git runner physical configuration member lacks its closed section')
		}
		member := line.trim_left(' \t')
		parts := member.split_nth(' = ', 2)
		if parts.len != 2 || parts[0] == '' || parts[1] == '' || parts[0] != parts[0].to_lower()
			|| parts[0].contains('.') || parts[0].trim_space() != parts[0]
			|| parts[1].trim_space() != parts[1] {
			return error('durable Git runner physical configuration member is noncanonical')
		}
		key := '${section}.${parts[0]}'
		if key.len > durable_git_config_key_max_bytes || !durable_git_config_key_allowed(key)
			|| !durable_git_physical_config_value_allowed(key, parts[1]) || key in seen {
			return error('durable Git runner physical configuration key is duplicated or outside its allowlist')
		}
		seen[key] = true
		keys << key
		if keys.len > durable_git_config_max_keys {
			return error('durable Git runner physical configuration key set exceeds its closed bound')
		}
	}
	for required in ['core.repositoryformatversion', 'core.filemode', 'core.bare'] {
		if required !in seen {
			return error('durable Git runner physical configuration lacks its required bare core keys')
		}
	}
	return keys
}

fn durable_git_validate_config_keys(source string) ![]string {
	if source == '' || !source.ends_with('\x00') {
		return error('durable Git runner configuration key framing is invalid')
	}
	mut seen := map[string]bool{}
	mut keys := []string{}
	mut start := 0
	for index, byte in source.bytes() {
		if byte != 0 {
			continue
		}
		if index == start {
			if index == source.len - 1 {
				break
			}
			return error('durable Git runner configuration key framing is invalid')
		}
		raw := source[start..index]
		key := raw.to_lower()
		if raw == '' || raw.len > durable_git_config_key_max_bytes || raw != key || key in seen {
			return error('durable Git runner configuration key is empty, duplicated, oversized, or noncanonical')
		}
		seen[key] = true
		if !durable_git_config_key_allowed(key) {
			return error('durable Git runner configuration contains a key outside its exact allowlist')
		}
		keys << key
		if keys.len > durable_git_config_max_keys {
			return error('durable Git runner configuration key set exceeds its closed bound')
		}
		start = index + 1
	}
	if start != source.len || keys.len == 0 {
		return error('durable Git runner configuration key framing is invalid')
	}
	for required in ['core.repositoryformatversion', 'core.filemode', 'core.bare'] {
		if required !in seen {
			return error('durable Git runner configuration lacks its required bare core keys')
		}
	}
	return keys
}

fn durable_git_join_config_keys(physical_keys []string, git_output string) ![]string {
	git_keys := durable_git_validate_config_keys(git_output)!
	if git_keys != physical_keys {
		return error('durable Git runner Git key output differs from its physical lexical configuration')
	}
	return git_keys
}

fn durable_git_safe_remote_name(value string) bool {
	if value == '' || value.len > 64 || !durable_git_ascii_alphanumeric(value[0])
		|| !durable_git_ascii_alphanumeric(value[value.len - 1]) {
		return false
	}
	for character in value {
		if !(character >= `a` && character <= `z`) && !(character >= `0` && character <= `9`)
			&& character !in [`-`, `_`] {
			return false
		}
	}
	return true
}

fn durable_git_ascii_alphanumeric(character u8) bool {
	return (character >= `a` && character <= `z`) || (character >= `0` && character <= `9`)
}

fn durable_git_output_limit(class DurableGitReadClass) int {
	return match class {
		.capability { durable_git_capability_max_bytes }
		.config { durable_git_config_max_bytes }
		.scalar, .commit { durable_git_scalar_max_bytes }
		.history { durable_git_history_max_bytes }
		.inventory, .log { durable_git_inventory_max_bytes }
		.target_blob { durable_git_target_blob_max_bytes }
		.evidence_blob { durable_git_evidence_blob_max_bytes }
	}
}

fn durable_git_deadline(class DurableGitReadClass) u64 {
	return match class {
		.capability { durable_git_capability_deadline_ns }
		.inventory, .history, .log { durable_git_large_deadline_ns }
		else { durable_git_regular_deadline_ns }
	}
}

fn durable_git_set_cloexec(fd int) bool {
	$if linux || macos || freebsd || openbsd {
		flags := C.fcntl(fd, C.F_GETFD, 0)
		return flags >= 0 && C.fcntl(fd, C.F_SETFD, flags | C.FD_CLOEXEC) == 0
	} $else {
		return false
	}
}

fn durable_git_standard_fds_valid() bool {
	$if linux || macos || freebsd || openbsd {
		for fd in 0 .. 3 {
			if C.fcntl(fd, C.F_GETFD, 0) < 0 {
				return false
			}
		}
		return true
	} $else {
		return false
	}
}

fn durable_git_fd_tuple_is_valid(fds []int) bool {
	if fds.len != 7 {
		return false
	}
	mut seen := map[int]bool{}
	for fd in fds {
		if fd <= 2 || fd in seen {
			return false
		}
		seen[fd] = true
	}
	return true
}

fn durable_git_accept_fd(fd int, mut acquired []int) ! {
	if fd <= 2 || fd in acquired {
		return error('durable Git runner acquired an invalid or repeated descriptor')
	}
	if !durable_git_set_cloexec(fd) {
		return error('durable Git runner cannot set close-on-exec on an acquired descriptor')
	}
	acquired << fd
}

fn durable_git_close_acquired(mut acquired []int) {
	for index := acquired.len - 1; index >= 0; index-- {
		durable_git_close_fd(acquired[index])
	}
	acquired.clear()
}

fn durable_git_close_unacquired(fds []int, acquired []int) {
	mut closed := map[int]bool{}
	for fd in fds {
		if fd > 2 && fd !in acquired && fd !in closed {
			durable_git_close_fd(fd)
			closed[fd] = true
		}
	}
}

fn durable_git_set_nonblocking(fd int) bool {
	$if linux || macos || freebsd || openbsd {
		flags := C.fcntl(fd, C.F_GETFL, 0)
		return flags >= 0 && C.fcntl(fd, C.F_SETFL, flags | C.O_NONBLOCK) == 0
	} $else {
		return false
	}
}

fn durable_git_close_fd(fd int) {
	$if linux || macos || freebsd || openbsd {
		if fd > 2 {
			C.close(fd)
		}
	}
}

fn durable_git_child_failure(message string, code int) {
	$if linux || macos || freebsd || openbsd {
		mut offset := 0
		for offset < message.len {
			remaining := unsafe { message.str + offset }
			written := C.write(2, voidptr(remaining), usize(message.len - offset))
			if written > 0 {
				offset += written
				continue
			}
			if written < 0 && C.errno == C.EINTR {
				continue
			}
			break
		}
		C._exit(code)
	}
}

fn durable_git_descriptor_is_closed(fd int) bool {
	$if linux || macos || freebsd || openbsd {
		return C.fcntl(fd, C.F_GETFD, 0) == -1 && C.errno == C.EBADF
	} $else {
		return false
	}
}

// This child-side check is deliberately scalar and nonallocating.
fn durable_git_seven_descriptors_are_closed(a int, b int, c int, d int, e int, f int,
	g int) bool {
	return durable_git_descriptor_is_closed(a) && durable_git_descriptor_is_closed(b)
		&& durable_git_descriptor_is_closed(c) && durable_git_descriptor_is_closed(d)
		&& durable_git_descriptor_is_closed(e) && durable_git_descriptor_is_closed(f)
		&& durable_git_descriptor_is_closed(g)
}

fn durable_git_child_bulk_close(descriptor_limit u64) bool {
	$if linux {
		return unsafe { C.syscall(C.SYS_close_range, u32(3), u32(0xffff_ffff), u32(0)) == 0 }
	} $else $if macos {
		// Supported macOS SDK/deployment-target pairs do not consistently declare closefrom.
		// The parent already proved this inherited descriptor bound is finite and capped.
		mut fd := 3
		for u64(fd) < descriptor_limit {
			C.close(fd)
			fd++
		}
		return true
	} $else $if freebsd || openbsd {
		C.closefrom(3)
		return true
	} $else {
		return false
	}
}

fn durable_git_termination_secured(attempt DurableGitTerminationAttempt) bool {
	return attempt.pid_secured && (!attempt.group_required || attempt.group_secured)
}

fn durable_git_classify_kill_result(exit_code int, error_code int,
	attempt int) DurableGitKillDisposition {
	$if linux || macos || freebsd || openbsd {
		if exit_code == 0 || error_code == C.ESRCH {
			return .secured
		}
		if (error_code == C.EINTR && attempt < 7) || attempt < 2 {
			return .retry
		}
		return .hard_failure
	} $else {
		return .hard_failure
	}
}

// Every signal attempt, including a retry, is independently authorized by the unchanged
// SIGCHLD snapshot. A failed checkpoint returns before C.kill and therefore before any further
// signal or wait action.
fn durable_git_kill_one(target int, mut session DurableGitRunnerSession,
	deadline u64) DurableGitKillResult {
	$if linux || macos || freebsd || openbsd {
		for attempt in 0 .. 8 {
			if time.sys_mono_now() >= deadline {
				return DurableGitKillResult{
					hard_failure: 'durable Git runner termination exceeded its monotonic retry bound'
				}
			}
			durable_git_authorize_parent_action(mut session, .kill) or {
				return DurableGitKillResult{
					checkpoint_failure: durable_git_sigchld_drift_message
				}
			}
			exit_code := C.kill(target, C.SIGKILL)
			error_code := if exit_code == 0 { 0 } else { C.errno }
			match durable_git_classify_kill_result(exit_code, error_code, attempt) {
				.secured {
					return DurableGitKillResult{
						secured: true
					}
				}
				.retry {
					// The bounded parent machine regains control before retrying. Its next
					// iteration checks both the monotonic bound and SIGCHLD again.
					time.sleep((attempt + 1) * time.millisecond)
					continue
				}
				.hard_failure {
					return DurableGitKillResult{
						hard_failure: 'durable Git runner cannot terminate its reserved child identity'
					}
				}
			}
		}
		return DurableGitKillResult{
			hard_failure: 'durable Git runner termination was interrupted beyond its retry bound'
		}
	} $else {
		return DurableGitKillResult{
			hard_failure: 'durable target commit planning is unavailable on this platform without a raw-byte Git runner'
		}
	}
}

fn durable_git_request_termination(pid int, group_required bool,
	mut session DurableGitRunnerSession, now u64) DurableGitTerminationAttempt {
	deadline := now + durable_git_abort_grace_ns
	mut group_secured := !group_required
	mut group_failure := ''
	mut checkpoint_failure := ''
	if group_required {
		group := durable_git_kill_one(-pid, mut session, deadline)
		group_secured = group.secured
		group_failure = group.hard_failure
		checkpoint_failure = group.checkpoint_failure
		if checkpoint_failure != '' {
			return DurableGitTerminationAttempt{
				group_required:     group_required
				group_secured:      group_secured
				checkpoint_failure: checkpoint_failure
			}
		}
	}
	pid_result := durable_git_kill_one(pid, mut session, deadline)
	checkpoint_failure = pid_result.checkpoint_failure
	mut hard_failure := group_failure
	if hard_failure == '' {
		hard_failure = pid_result.hard_failure
	} else if pid_result.hard_failure != '' {
		hard_failure += '; ${pid_result.hard_failure}'
	}
	return DurableGitTerminationAttempt{
		group_required:     group_required
		group_secured:      group_secured
		pid_secured:        pid_result.secured
		checkpoint_failure: checkpoint_failure
		hard_failure:       hard_failure
	}
}

fn durable_git_failure_join(first string, second string) string {
	if first == '' {
		return second
	}
	if second == '' || second == first {
		return first
	}
	return '${first}; ${second}'
}

fn durable_git_termination_requires_poison(attempt DurableGitTerminationAttempt) bool {
	return attempt.checkpoint_failure != '' || attempt.hard_failure != ''
}

fn durable_git_abort_state_from_attempt(reason string, attempt DurableGitTerminationAttempt,
	now u64) DurableGitAbortState {
	mut failure := durable_git_failure_join(reason, attempt.checkpoint_failure)
	failure = durable_git_failure_join(failure, attempt.hard_failure)
	return DurableGitAbortState{
		requested:           true
		termination_secured: durable_git_termination_secured(attempt)
		signal_drift:        attempt.checkpoint_failure != ''
		deadline:            now + durable_git_abort_grace_ns
		failure:             failure
	}
}

fn durable_git_abort_bound_expired(now u64, abort DurableGitAbortState) bool {
	return abort.requested && now >= abort.deadline
}

fn durable_git_require_clean_post_eof(abort DurableGitAbortState) ! {
	if abort.failure != '' {
		return error(abort.failure)
	}
}

fn durable_git_begin_abort(mut session DurableGitRunnerSession, pid int, group_required bool,
	reason string) DurableGitAbortState {
	now := time.sys_mono_now()
	attempt := durable_git_request_termination(pid, group_required, mut session, now)
	abort := durable_git_abort_state_from_attempt(reason, attempt, now)
	durable_git_record_first_failure(mut session, abort.failure)
	if durable_git_termination_requires_poison(attempt) {
		session.poisoned = true
		session.poison_reason = if attempt.hard_failure != '' {
			'durable Git runner retained its lease after an unproved child termination'
		} else {
			'durable Git runner retained its lease after SIGCHLD ownership drift'
		}
	}
	return abort
}

fn durable_git_confirm_process_group(pid int, deadline u64) ! {
	$if linux || macos || freebsd || openbsd {
		for attempt in 0 .. 8 {
			if time.sys_mono_now() >= deadline {
				return error('durable Git runner process-group setup exceeded its monotonic retry bound')
			}
			if C.setpgid(pid, pid) == 0 {
				break
			}
			if C.errno == C.EINTR {
				time.sleep((attempt + 1) * time.millisecond)
				continue
			}
			if C.errno != C.EACCES {
				return error('durable Git runner cannot establish its reserved process group')
			}
			break
		}
		for attempt in 0 .. 8 {
			if time.sys_mono_now() >= deadline {
				return error('durable Git runner process-group verification exceeded its monotonic retry bound')
			}
			group := C.getpgid(pid)
			if group == pid {
				return
			}
			if group < 0 && C.errno == C.EINTR {
				time.sleep((attempt + 1) * time.millisecond)
				continue
			}
			return error('durable Git runner cannot verify its reserved process group')
		}
		return error('durable Git runner cannot verify its reserved process group inside its retry bound')
	} $else {
		return error('durable target commit planning is unavailable on this platform without a raw-byte Git runner')
	}
}

fn durable_git_read_pipe(fd int, mut output []u8, eof bool, limit int,
	combined int) !DurableGitPipeRead {
	$if linux || macos || freebsd || openbsd {
		mut buffer := [8192]u8{}
		mut observed_combined := combined
		read := C.read(fd, voidptr(&buffer[0]), usize(buffer.len))
		if read > 0 {
			observed_combined += read
			if output.len + read > limit || observed_combined > limit + durable_git_stderr_max_bytes {
				return error('durable Git runner command output exceeds its closed byte bound')
			}
			output << buffer[..read]
			return DurableGitPipeRead{
				eof:      eof
				combined: observed_combined
				progress: true
			}
		}
		if read == 0 {
			return DurableGitPipeRead{
				eof:      true
				combined: observed_combined
			}
		}
		if C.errno == C.EINTR || C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
			// Return to the outer pump; it re-observes the monotonic deadline before
			// issuing another read, so EINTR cannot form an internal unbounded loop.
			return DurableGitPipeRead{
				eof:      eof
				combined: observed_combined
			}
		}
		return error('durable Git runner pipe read failed')
	} $else {
		return error('durable target commit planning is unavailable on this platform without a raw-byte Git runner')
	}
}

fn durable_git_classify_wait_result(pid int, waited int,
	error_code int) DurableGitWaitDisposition {
	$if linux || macos || freebsd || openbsd {
		if waited == pid {
			return .terminal
		}
		if waited == 0 {
			return .running
		}
		if waited < 0 && error_code == C.EINTR {
			return .retry
		}
		if waited < 0 && error_code == C.ECHILD {
			return .lost
		}
		return .foreign
	} $else {
		return .foreign
	}
}

fn durable_git_wait_exclusivity_error(disposition DurableGitWaitDisposition) string {
	if disposition in [.lost, .foreign] {
		return durable_git_reaping_exclusivity_message
	}
	return ''
}

fn durable_git_wait_after_eof(mut session DurableGitRunnerSession, pid int, command_deadline u64,
	mut abort DurableGitAbortState) !int {
	$if linux || macos || freebsd || openbsd {
		for {
			// The checkpoint precedes every waitpid, including every EINTR/running retry.
			durable_git_authorize_parent_action(mut session, .wait) or {
				abort.failure = durable_git_failure_join(abort.failure,
					durable_git_sigchld_drift_message)
				abort.signal_drift = true
				return error(abort.failure)
			}
			mut status := 0
			waited := C.waitpid(pid, &status, C.WNOHANG)
			error_code := if waited < 0 { C.errno } else { 0 }
			match durable_git_classify_wait_result(pid, waited, error_code) {
				.terminal {
					return status
				}
				.retry {}
				.lost {
					session.poisoned = true
					session.poison_reason = 'durable Git runner retained its lease after losing child reaping ownership'
					session.control.poisoned = true
					session.control.failure = durable_git_reaping_exclusivity_message
					durable_git_record_first_failure(mut session,
						durable_git_reaping_exclusivity_message)
					return error(durable_git_wait_exclusivity_error(.lost))
				}
				.foreign {
					session.poisoned = true
					session.poison_reason = 'durable Git runner retained its lease after a foreign wait identity'
					session.control.poisoned = true
					session.control.failure = durable_git_reaping_exclusivity_message
					durable_git_record_first_failure(mut session,
						durable_git_reaping_exclusivity_message)
					return error(durable_git_wait_exclusivity_error(.foreign))
				}
				.running {}
			}
			now := time.sys_mono_now()
			if now >= command_deadline && !abort.requested {
				abort = durable_git_begin_abort(mut session, pid, true,
					'durable Git runner command exceeded its monotonic deadline')
				if abort.signal_drift {
					return error(abort.failure)
				}
			}
			if durable_git_abort_bound_expired(now, abort) {
				session.poisoned = true
				if session.poison_reason == '' {
					session.poison_reason = 'durable Git runner retained its lease after bounded reaping expired'
				}
				return error(durable_git_failure_join(abort.failure,
					'durable Git runner child did not reach a reaped terminal state inside its bound'))
			}
			time.sleep(20 * time.millisecond)
		}
	} $else {
		return error('durable target commit planning is unavailable on this platform without a raw-byte Git runner')
	}
	return error('durable Git runner wait machine ended without a terminal child state')
}

fn durable_git_build_argv(git_path string, state_git_dir string, command []string,
	repository bool) ![]string {
	if !os.is_abs_path(git_path) || command.len == 0 || command.len > 64 {
		return error('durable Git runner command is outside its closed argv bound')
	}
	for argument in command {
		if argument == '' || argument.contains('\x00') || argument.len > 4096 {
			return error('durable Git runner command contains an invalid argument')
		}
	}
	mut argv := [git_path, '--no-pager', '--no-replace-objects', '--no-lazy-fetch']
	if repository {
		if !os.is_abs_path(state_git_dir) {
			return error('durable Git runner repository argv is not absolute')
		}
		argv << ['--git-dir', state_git_dir]
	}
	if command[0] == 'log' {
		argv << ['log', '--no-show-signature', '--no-ext-diff', '--no-textconv', '--no-renames',
			'--no-color', '--no-decorate', '--no-notes', '--no-use-mailmap',
			'--ignore-submodules=none', '-O', os.path_devnull]
		argv << command[1..]
	} else {
		argv << command
	}
	return argv
}

fn durable_git_write_release_gate(fd int, deadline u64) ! {
	$if linux || macos || freebsd || openbsd {
		mut marker := [u8(`G`)]!
		for attempt in 0 .. 8 {
			if time.sys_mono_now() >= deadline {
				return error('durable Git runner child release gate exceeded its monotonic retry bound')
			}
			written := C.write(fd, voidptr(&marker[0]), 1)
			if written == 1 {
				return
			}
			if written < 0 && C.errno == C.EINTR {
				time.sleep((attempt + 1) * time.millisecond)
				continue
			}
			return error('durable Git runner child release gate is invalid')
		}
		return error('durable Git runner child release gate exceeded its retry bound')
	} $else {
		return error('durable target commit planning is unavailable on this platform without a raw-byte Git runner')
	}
}

fn durable_git_capture_inner(mut session DurableGitRunnerSession, command []string,
	class DurableGitReadClass, repository bool) !DurableGitCapturedResult {
	$if linux || macos || freebsd || openbsd {
		session.control = DurableGitControlState{}
		mut trace := ['argv-validated']
		durable_git_require_signal_checkpoint(mut session)!
		trace << 'sigchld-pre-command'
		if repository {
			durable_git_require_no_sidecars(session.state_git_dir)!
			if durable_git_state_directory_snapshot(session.state_git_dir)! != session.state_identity {
				return error('durable Git runner state repository identity changed before an object read')
			}
			current_config := durable_git_config_snapshot(session.state_git_dir)!
			if current_config != session.config {
				return error('durable Git runner configuration changed before an object read')
			}
			trace << 'repository-preflight-before'
		}
		argv :=
			durable_git_build_argv(session.git_path, session.state_git_dir, command, repository)!
		trace << 'argv-closed'
		if durable_git_resource_preflight()! != session.resources {
			return error('durable Git runner descriptor resource bound changed before setup')
		}
		durable_git_require_signal_checkpoint(mut session)!
		if !durable_git_standard_fds_valid() {
			return error('durable Git runner requires valid inherited descriptors zero, one, and two')
		}
		mut cargv := []&char{cap: argv.len + 1}
		for argument in argv {
			cargv << &char(argument.str)
		}
		cargv << &char(unsafe { nil })
		mut cenv := []&char{cap: session.environment.len + 1}
		for entry in session.environment {
			cenv << &char(entry.str)
		}
		cenv << &char(unsafe { nil })
		mut stdout_pipe := [2]int{}
		mut stderr_pipe := [2]int{}
		mut gate_pipe := [2]int{}
		for index in 0 .. 2 {
			stdout_pipe[index] = -1
			stderr_pipe[index] = -1
			gate_pipe[index] = -1
		}
		mut acquired_fds := []int{cap: 7}
		devnull := int(C.open(&char(os.path_devnull.str), C.O_RDONLY, 0))
		durable_git_accept_fd(devnull, mut acquired_fds) or {
			durable_git_close_unacquired([devnull], acquired_fds)
			durable_git_close_acquired(mut acquired_fds)
			return error('durable Git runner pipe or descriptor setup failed closed')
		}
		if C.pipe(&stdout_pipe[0]) != 0 {
			durable_git_close_acquired(mut acquired_fds)
			return error('durable Git runner pipe or descriptor setup failed closed')
		}
		for fd in stdout_pipe {
			durable_git_accept_fd(fd, mut acquired_fds) or {
				durable_git_close_unacquired(stdout_pipe[..], acquired_fds)
				durable_git_close_acquired(mut acquired_fds)
				return error('durable Git runner pipe or descriptor setup failed closed')
			}
		}
		if C.pipe(&stderr_pipe[0]) != 0 {
			durable_git_close_acquired(mut acquired_fds)
			return error('durable Git runner pipe or descriptor setup failed closed')
		}
		for fd in stderr_pipe {
			durable_git_accept_fd(fd, mut acquired_fds) or {
				durable_git_close_unacquired(stderr_pipe[..], acquired_fds)
				durable_git_close_acquired(mut acquired_fds)
				return error('durable Git runner pipe or descriptor setup failed closed')
			}
		}
		if C.pipe(&gate_pipe[0]) != 0 {
			durable_git_close_acquired(mut acquired_fds)
			return error('durable Git runner pipe or descriptor setup failed closed')
		}
		for fd in gate_pipe {
			durable_git_accept_fd(fd, mut acquired_fds) or {
				durable_git_close_unacquired(gate_pipe[..], acquired_fds)
				durable_git_close_acquired(mut acquired_fds)
				return error('durable Git runner pipe or descriptor setup failed closed')
			}
		}
		all_fds := [devnull, stdout_pipe[0], stdout_pipe[1], stderr_pipe[0], stderr_pipe[1], gate_pipe[0],
			gate_pipe[1]]
		if !durable_git_fd_tuple_is_valid(all_fds) || !durable_git_set_nonblocking(stdout_pipe[0]) {
			durable_git_close_acquired(mut acquired_fds)
			return error('durable Git runner pipe or descriptor setup failed closed')
		}
		if !durable_git_set_nonblocking(stderr_pipe[0]) {
			durable_git_close_acquired(mut acquired_fds)
			return error('durable Git runner pipe or descriptor setup failed closed')
		}
		trace << 'seven-descriptors-validated'
		resource_before_fork := durable_git_resource_preflight() or {
			for fd in all_fds {
				durable_git_close_fd(fd)
			}
			return err
		}
		if resource_before_fork != session.resources {
			for fd in all_fds {
				durable_git_close_fd(fd)
			}
			return error('durable Git runner descriptor resource bound changed before fork')
		}
		durable_git_require_signal_checkpoint(mut session) or {
			for fd in all_fds {
				durable_git_close_fd(fd)
			}
			return err
		}
		trace << 'sigchld-prefork'
		pid := C.fork()
		if pid < 0 {
			for fd in all_fds {
				durable_git_close_fd(fd)
			}
			return error('durable Git runner fork failed before child ownership')
		}
		if pid == 0 {
			C.close(gate_pipe[1])
			C.close(stdout_pipe[0])
			C.close(stderr_pipe[0])
			if C.setpgid(0, 0) != 0 || C.dup2(devnull, 0) != 0 || C.dup2(stdout_pipe[1], 1) != 1
				|| C.dup2(stderr_pipe[1], 2) != 2 {
				durable_git_child_failure('durable Git runner child descriptor setup failed\n', 125)
			}
			C.close(devnull)
			C.close(stdout_pipe[1])
			C.close(stderr_pipe[1])
			mut gate := [1]u8{}
			mut got_gate := false
			for {
				read := C.read(gate_pipe[0], voidptr(&gate[0]), 1)
				if read == 1 {
					got_gate = gate[0] == `G`
					break
				}
				if read < 0 && C.errno == C.EINTR {
					continue
				}
				break
			}
			mut saw_eof := false
			if got_gate {
				for {
					read := C.read(gate_pipe[0], voidptr(&gate[0]), 1)
					if read == 0 {
						saw_eof = true
						break
					}
					if read < 0 && C.errno == C.EINTR {
						continue
					}
					break
				}
			}
			C.close(gate_pipe[0])
			if !got_gate || !saw_eof {
				durable_git_child_failure('durable Git runner child release gate is invalid\n', 126)
			}
			if !durable_git_child_bulk_close(session.resources.current)
				|| !durable_git_seven_descriptors_are_closed(all_fds[0], all_fds[1], all_fds[2], all_fds[3], all_fds[4], all_fds[5], all_fds[6]) {
				durable_git_child_failure('durable Git runner child bulk descriptor close failed\n',
					126)
			}
			C.execve(&char(session.git_path.str), cargv.data, cenv.data)
			durable_git_child_failure('durable Git runner execve failed\n', 127)
		}
		trace << 'positive-child-pid'
		durable_git_close_fd(devnull)
		durable_git_close_fd(stdout_pipe[1])
		durable_git_close_fd(stderr_pipe[1])
		mut released := false
		mut release_error := ''
		mut pre_go_signal_drift := false
		release_deadline := time.sys_mono_now() + durable_git_regular_deadline_ns
		// Once fork has returned a positive PID, any inability to observe the exact signal
		// snapshot poisons the lease immediately. The GO gate remains closed and no kill or
		// wait action is authorized under changed reaping ownership.
		durable_git_route_postfork_checkpoint(mut session) or {
			release_error = durable_git_sigchld_drift_message
			pre_go_signal_drift = true
		}
		if release_error == '' {
			durable_git_confirm_process_group(pid, release_deadline) or {
				release_error = err.msg()
			}
		}
		if release_error == '' {
			durable_git_route_postfork_checkpoint(mut session) or {
				release_error = durable_git_sigchld_drift_message
				pre_go_signal_drift = true
			}
		}
		if release_error == '' {
			trace << 'pgroup-and-sigchld-verified'
		}
		if release_error == '' {
			durable_git_authorize_parent_action(mut session, .go_release) or {
				release_error = durable_git_sigchld_drift_message
				pre_go_signal_drift = true
			}
			if release_error == '' {
				durable_git_write_release_gate(gate_pipe[1], release_deadline) or {
					release_error = err.msg()
				}
				released = release_error == ''
			}
		}
		if released {
			trace << 'go-write-one-guarded'
			durable_git_close_fd(gate_pipe[0])
			trace << 'parent-go-read-closed'
			durable_git_close_fd(gate_pipe[1])
			trace << 'parent-go-write-closed'
		} else {
			durable_git_close_fd(gate_pipe[1])
			durable_git_close_fd(gate_pipe[0])
		}
		if pre_go_signal_drift {
			durable_git_close_fd(stdout_pipe[0])
			durable_git_close_fd(stderr_pipe[0])
			return error(durable_git_sigchld_drift_message)
		}
		limit := durable_git_output_limit(class)
		deadline := time.sys_mono_now() + durable_git_deadline(class)
		mut pump := DurableGitPumpState{}
		mut abort := DurableGitAbortState{}
		if !released {
			// Before GO no descendant can exist: reserve and terminate only the positive child PID.
			abort = durable_git_begin_abort(mut session, pid, false, durable_git_failure_join(release_error,
				'durable Git runner child was not released for exec'))
			pump.failure_message = abort.failure
		}
		mut backoff_index := 0
		for !pump.stdout_eof || !pump.stderr_eof {
			if abort.signal_drift {
				break
			}
			mut progressed := false
			now := time.sys_mono_now()
			if now >= deadline && !abort.requested {
				abort = durable_git_begin_abort(mut session, pid, true,
					'durable Git runner command exceeded its monotonic deadline')
				pump.failure_message = durable_git_failure_join(pump.failure_message, abort.failure)
				if abort.signal_drift {
					break
				}
			}
			if durable_git_abort_bound_expired(now, abort) {
				session.poisoned = true
				if session.poison_reason == '' {
					session.poison_reason = 'durable Git runner retained its lease after pipes did not reach EOF'
				}
				pump.failure_message = durable_git_failure_join(pump.failure_message,
					'durable Git runner pipes did not reach EOF inside the abort bound')
				break
			}
			mut pollfds := [
				C.pollfd{
					fd:     if pump.stdout_eof { -1 } else { stdout_pipe[0] }
					events: i16(C.POLLIN)
				},
				C.pollfd{
					fd:     if pump.stderr_eof { -1 } else { stderr_pipe[0] }
					events: i16(C.POLLIN)
				},
			]!
			wait_ms := durable_git_runner_poll_backoff_ms[backoff_index]
			polled := C.poll(&pollfds[0], u64(pollfds.len), wait_ms)
			if polled < 0 && C.errno != C.EINTR {
				if !abort.requested {
					abort = durable_git_begin_abort(mut session, pid, true,
						'durable Git runner poll failed')
				}
				pump.failure_message = durable_git_failure_join(pump.failure_message, durable_git_failure_join('durable Git runner poll failed',
					abort.failure))
				if abort.signal_drift {
					break
				}
			}
			if !pump.stdout_eof && (polled > 0 || abort.requested) {
				read_state := durable_git_read_pipe(stdout_pipe[0], mut pump.stdout,
					pump.stdout_eof, limit, pump.combined_bytes) or {
					failure := err.msg()
					if !abort.requested {
						abort = durable_git_begin_abort(mut session, pid, true, failure)
					}
					pump.failure_message = durable_git_failure_join(pump.failure_message, durable_git_failure_join(failure,
						abort.failure))
					DurableGitPipeRead{
						eof:      pump.stdout_eof
						combined: pump.combined_bytes
					}
				}
				pump.stdout_eof = read_state.eof
				pump.combined_bytes = read_state.combined
				progressed = progressed || read_state.progress
				if abort.signal_drift {
					break
				}
			}
			if !pump.stderr_eof && (polled > 0 || abort.requested) {
				read_state := durable_git_read_pipe(stderr_pipe[0], mut pump.stderr,
					pump.stderr_eof, durable_git_stderr_max_bytes, pump.combined_bytes) or {
					failure := err.msg()
					if !abort.requested {
						abort = durable_git_begin_abort(mut session, pid, true, failure)
					}
					pump.failure_message = durable_git_failure_join(pump.failure_message, durable_git_failure_join(failure,
						abort.failure))
					DurableGitPipeRead{
						eof:      pump.stderr_eof
						combined: pump.combined_bytes
					}
				}
				pump.stderr_eof = read_state.eof
				pump.combined_bytes = read_state.combined
				progressed = progressed || read_state.progress
				if abort.signal_drift {
					break
				}
			}
			if progressed {
				backoff_index = 0
			} else if backoff_index + 1 < durable_git_runner_poll_backoff_ms.len {
				backoff_index++
			}
		}
		durable_git_close_fd(stdout_pipe[0])
		durable_git_close_fd(stderr_pipe[0])
		if !pump.stdout_eof || !pump.stderr_eof {
			return error(pump.failure_message)
		}
		trace << ['stdout-eof', 'stderr-eof']
		status := durable_git_wait_after_eof(mut session, pid, deadline, mut abort)!
		mut post_reap_checkpoint_failure := ''
		durable_git_require_signal_checkpoint(mut session) or {
			post_reap_checkpoint_failure = durable_git_sigchld_drift_message
		}
		// A timeout, read failure, kill failure, or checkpoint failure can never be
		// masked by later EOF, cleanup, signal drift, or a zero child status.
		durable_git_require_clean_post_eof(abort)!
		if post_reap_checkpoint_failure != '' {
			return error(post_reap_checkpoint_failure)
		}
		trace << ['wait-after-eof', 'child-reaped', 'sigchld-post-reap']
		if repository {
			durable_git_require_no_sidecars(session.state_git_dir)!
			if durable_git_state_directory_snapshot(session.state_git_dir)! != session.state_identity {
				return error('durable Git runner state repository identity changed after an object read')
			}
			if durable_git_config_snapshot(session.state_git_dir)! != session.config {
				return error('durable Git runner configuration changed after an object read')
			}
			trace << 'repository-preflight-after'
		}
		if release_error != '' {
			return error(release_error)
		}
		if pump.failure_message != '' {
			return error(pump.failure_message)
		}
		child_stderr := pump.stderr.bytestr()
		child_failure := match child_stderr {
			'durable Git runner child descriptor setup failed\n' {
				'durable Git runner child descriptor setup failed'
			}
			'durable Git runner child release gate is invalid\n' {
				'durable Git runner child release gate is invalid'
			}
			'durable Git runner child bulk descriptor close failed\n' {
				'durable Git runner child bulk descriptor close failed'
			}
			'durable Git runner execve failed\n' {
				'durable Git runner execve failed'
			}
			else {
				''
			}
		}
		if child_failure != '' {
			return error(child_failure)
		}
		exit_code := if C.WIFEXITED(status) {
			int(C.WEXITSTATUS(status))
		} else if C.WIFSIGNALED(status) {
			128 + int(C.WTERMSIG(status))
		} else {
			125
		}
		return DurableGitCapturedResult{
			exit_code: exit_code
			stdout:    pump.stdout.bytestr()
			stderr:    pump.stderr.bytestr()
			trace:     trace
		}
	} $else {
		return error('durable target commit planning is unavailable on this platform without a raw-byte Git runner')
	}
}

fn durable_git_capture(mut session DurableGitRunnerSession, command []string,
	class DurableGitReadClass, repository bool) !DurableGitCapturedResult {
	return durable_git_capture_inner(mut session, command, class, repository) or {
		durable_git_record_first_failure(mut session, err.msg())
		return err
	}
}

fn durable_git_runner_begin(state_git_dir string) !&DurableGitRunnerSession {
	mut session := &DurableGitRunnerSession{
		state_git_dir: state_git_dir
	}
	token := voidptr(session)
	mut runner_slot := durable_git_runner_slot
	if !durable_git_lease_try_acquire(mut runner_slot, token) {
		return error('durable Git runner already owns the process child-reaping lease')
	}
	signal := durable_git_require_default_signal() or {
		failure := err.msg()
		durable_git_release_lease_unchecked(session) or {}
		return error(failure)
	}
	session.signal = signal
	durable_git_signal_checkpoint(signal) or {
		failure := err.msg()
		durable_git_runner_end(mut session) or {}
		return error(failure)
	}
	session.resources = durable_git_resource_preflight() or {
		failure := err.msg()
		durable_git_runner_end(mut session) or {}
		return error(failure)
	}
	session.environment = durable_git_closed_environment() or {
		failure := err.msg()
		durable_git_runner_end(mut session) or {}
		return error(failure)
	}
	session.git_path = durable_git_resolve_binary() or {
		failure := err.msg()
		durable_git_runner_end(mut session) or {}
		return error(failure)
	}
	capability := durable_git_capture(mut session, ['--version'], .capability, false) or {
		failure := err.msg()
		durable_git_runner_end(mut session) or {}
		return error(failure)
	}
	if capability.exit_code != 0 || capability.stdout.trim_space() == '' || capability.stderr != '' {
		durable_git_runner_end(mut session) or {}
		return error('durable Git runner requires Git support for --no-lazy-fetch')
	}
	session.state_identity = durable_git_state_directory_snapshot(state_git_dir) or {
		failure := err.msg()
		durable_git_runner_end(mut session) or {}
		return error(failure)
	}
	durable_git_require_no_sidecars(state_git_dir) or {
		failure := err.msg()
		durable_git_runner_end(mut session) or {}
		return error(failure)
	}
	session.config = durable_git_config_snapshot(state_git_dir) or {
		failure := err.msg()
		durable_git_runner_end(mut session) or {}
		return error(failure)
	}
	config := durable_git_capture(mut session, ['config', '--local', '--no-includes', '--null',
		'--name-only', '--list'], .config, true) or {
		failure := err.msg()
		durable_git_runner_end(mut session) or {}
		return error(failure)
	}
	if config.exit_code != 0 || config.stderr != '' {
		durable_git_runner_end(mut session) or {}
		return error('durable Git runner cannot acquire its closed local configuration key set')
	}
	_ := durable_git_join_config_keys(session.config.keys, config.stdout) or {
		failure := err.msg()
		durable_git_runner_end(mut session) or {}
		return error(failure)
	}
	session.config_ready = true
	return session
}

fn durable_git_release_lease_unchecked(session &DurableGitRunnerSession) ! {
	token := voidptr(session)
	mut runner_slot := durable_git_runner_slot
	if !durable_git_lease_try_release(mut runner_slot, token) {
		return error('durable Git runner lease token changed during release')
	}
}

fn durable_git_runner_end(mut session DurableGitRunnerSession) ! {
	// A persistent termination/reaping failure keeps the global lease intentionally occupied. A
	// caller must terminate the dedicated process; releasing would falsely permit a competing reap.
	if !durable_git_control_lease_releasable(session.poisoned) {
		reason := if session.poison_reason == '' {
			'durable Git runner retained its poisoned child-reaping lease'
		} else {
			session.poison_reason
		}
		return error(reason)
	}
	durable_git_require_signal_checkpoint(mut session)!
	durable_git_release_lease_unchecked(session)!
}

fn durable_git_run_args(state_git_dir string, command []string,
	class DurableGitReadClass) !DurableGitCapturedResult {
	mut session := durable_git_runner_session()!
	if session.poisoned {
		failure := if session.first_failure != '' {
			session.first_failure
		} else {
			'durable Git runner child-reaping lease is poisoned'
		}
		durable_git_record_first_failure(mut session, failure)
		return error(failure)
	}
	if session.state_git_dir != state_git_dir {
		failure := 'durable Git runner state repository differs from its leased authority'
		durable_git_record_first_failure(mut session, failure)
		return error(failure)
	}
	if !session.config_ready {
		failure := 'durable Git runner local configuration is not closed before object access'
		durable_git_record_first_failure(mut session, failure)
		return error(failure)
	}
	return durable_git_capture(mut session, command, class, true)
}

fn live_git_args(state_git_dir string, command []string, class DurableGitReadClass) os.Result {
	if durable_git_runner_lease_load() != unsafe { nil } {
		result := durable_git_run_args(state_git_dir, command, class) or {
			if mut session := durable_git_runner_session() {
				durable_git_record_first_failure(mut session, err.msg())
			}
			return os.Result{
				exit_code: 125
				output:    err.msg()
			}
		}
		if result.stderr != '' {
			if mut session := durable_git_runner_session() {
				durable_git_record_first_failure(mut session,
					'durable Git runner rejected unexpected stderr output')
			}
			return os.Result{
				exit_code: 125
				output:    'durable Git runner rejected unexpected stderr output'
			}
		}
		return os.Result{
			exit_code: result.exit_code
			output:    result.stdout
		}
	}
	mut quoted := []string{cap: command.len}
	for argument in command {
		quoted << os.quoted_path(argument)
	}
	return os.execute('git --no-replace-objects --git-dir ${os.quoted_path(state_git_dir)} ${quoted.join(' ')}')
}

// This legacy adapter remains for the dormant T2c3b authority. While T2c3c1 owns the lease it
// accepts only the exact target ls-tree form; arbitrary shell text cannot cross the new runner.
fn live_git(state_git_dir string, arguments string) os.Result {
	if durable_git_runner_lease_load() != unsafe { nil } {
		prefix := 'ls-tree -l -z --full-tree '
		separator := ' -- '
		if !arguments.starts_with(prefix) || !arguments.contains(separator) {
			if mut session := durable_git_runner_session() {
				durable_git_record_first_failure(mut session,
					'durable Git authority adapter rejected noncanonical arguments')
			}
			return os.Result{
				exit_code: 125
				output:    'durable Git authority adapter rejected noncanonical arguments'
			}
		}
		remainder := arguments[prefix.len..]
		parts := remainder.split(separator)
		if parts.len != 2 || !is_lower_hex_40(parts[0]) || !contract_relative_path_is_safe(parts[1]) {
			if mut session := durable_git_runner_session() {
				durable_git_record_first_failure(mut session,
					'durable Git authority adapter rejected noncanonical arguments')
			}
			return os.Result{
				exit_code: 125
				output:    'durable Git authority adapter rejected noncanonical arguments'
			}
		}
		return live_git_args(state_git_dir, ['ls-tree', '-l', '-z', '--full-tree', parts[0], '--',
			parts[1]], .scalar)
	}
	return os.execute('git --no-replace-objects --git-dir ${os.quoted_path(state_git_dir)} ${arguments}')
}

fn validate_live_git_preflight(state_git_dir string) ! {
	if durable_git_runner_lease_load() != unsafe { nil } {
		session := durable_git_runner_session()!
		if session.state_git_dir != state_git_dir {
			return error('durable Git runner state repository differs from its leased authority')
		}
		durable_git_require_no_sidecars(state_git_dir)!
		if durable_git_state_directory_snapshot(state_git_dir)! != session.state_identity {
			return error('durable Git runner state repository identity changed during nested preflight')
		}
		if durable_git_config_snapshot(state_git_dir)! != session.config {
			return error('durable Git runner configuration changed during nested preflight')
		}
		replace_refs := live_git_args(state_git_dir, ['for-each-ref', '--format=%(refname)',
			'refs/replace'], .scalar)
		if replace_refs.exit_code != 0 || replace_refs.output.trim_space() != '' {
			return error('live state Git repository contains or cannot exclude replacement refs')
		}
		return
	}
	environment := os.environ()
	redirecting_names := ['GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR', 'GIT_OBJECT_DIRECTORY',
		'GIT_ALTERNATE_OBJECT_DIRECTORIES', 'GIT_REPLACE_REF_BASE', 'GIT_GRAFT_FILE',
		'GIT_SHALLOW_FILE', 'GIT_NAMESPACE', 'GIT_INDEX_FILE', 'GIT_EXEC_PATH',
		'GIT_CONFIG_PARAMETERS', 'GIT_CONFIG_COUNT', 'GIT_CONFIG_SYSTEM', 'GIT_CONFIG_GLOBAL']
	for name in redirecting_names {
		if name in environment {
			return error('live state Git environment contains a repository or object redirection')
		}
	}
	for name, _ in environment {
		if name.starts_with('GIT_CONFIG_KEY_') || name.starts_with('GIT_CONFIG_VALUE_') {
			return error('live state Git environment contains injected configuration')
		}
	}
	for relative_path in ['info/grafts', 'objects/info/alternates', 'commondir'] {
		if os.exists(os.join_path(state_git_dir, relative_path)) {
			return error('live state Git repository contains a graft, alternate, or common redirect')
		}
	}
	if os.exists(os.join_path(state_git_dir, 'shallow')) {
		return error('history_recovery_required: live state Git repository is shallow')
	}
	replace_refs := live_git(state_git_dir, "for-each-ref --format='%(refname)' refs/replace")
	if replace_refs.exit_code != 0 || replace_refs.output.trim_space() != '' {
		return error('live state Git repository contains or cannot exclude replacement refs')
	}
}

fn load_live_state_inventory(automation_root string, state_git_dir string, trust LiveStateTrust,
	proof_bundle LiveStateProofBundle) !LiveStateInventory {
	head := proof_bundle.head.commit_sha
	head_inventory := load_live_state_inventory_at_commit(automation_root, state_git_dir, head)!
	history := load_live_evidence_history(state_git_dir, head, head_inventory) or {
		return error('history_recovery_required: ${err.msg()}')
	}
	bindings := live_source_terminal_bindings(head_inventory)!
	mut groups := map[string][]LiveSourceTerminalBinding{}
	mut required_historical_shas := []string{}
	for binding in bindings {
		target_commit := history.creation_commits[binding.business_evidence_path] or {
			return error('history_recovery_required: terminal business evidence lacks one unique creation commit')
		}
		mut group := groups[target_commit] or { []LiveSourceTerminalBinding{} }
		group << binding
		groups[target_commit] = group
		if target_commit != head && target_commit !in required_historical_shas {
			required_historical_shas << target_commit
		}
	}
	mut provided_historical_shas := proof_bundle.historical_paths.keys()
	required_historical_shas.sort()
	provided_historical_shas.sort()
	if provided_historical_shas != required_historical_shas {
		return error('history_recovery_required: live state proof bundle does not exactly cover the required unique historical commits')
	}
	mut authenticated_historical := map[string]bool{}
	mut target_commits := groups.keys()
	target_commits.sort()
	for target_commit in target_commits {
		if target_commit != head && target_commit !in authenticated_historical {
			authenticate_live_historical_proof(state_git_dir, trust, proof_bundle, target_commit) or {
				return error('history_recovery_required: ${err.msg()}')
			}
			authenticated_historical[target_commit] = true
		}
		parent_commit := live_first_parent_for_commit(target_commit, history) or {
			return error('history_recovery_required: ${err.msg()}')
		}
		parent_inventory := load_live_state_inventory_at_commit(automation_root, state_git_dir,
			parent_commit) or { return error('unknown_blocked: ${err.msg()}') }
		target_inventory := load_live_state_inventory_at_commit(automation_root, state_git_dir,
			target_commit) or { return error('unknown_blocked: ${err.msg()}') }
		validate_live_source_terminal_group(groups[target_commit], parent_commit, target_commit,
			parent_inventory, target_inventory, head_inventory, history) or {
			return error('unknown_blocked: ${err.msg()}')
		}
	}
	return head_inventory
}

fn live_source_terminal_bindings(inventory LiveStateInventory) ![]LiveSourceTerminalBinding {
	mut business_paths := map[string]string{}
	for path, blob in inventory.blobs {
		if !path.starts_with('evidence/') {
			continue
		}
		evidence := parse_strict_json(blob)!
		target_id := require_nullable_string_member(evidence, 'target_id')!
		transition := require_string_member(evidence, 'transition')!
		if target_id == '' || !transition.starts_with('source_unreachable_') {
			continue
		}
		key := '${target_id}\x1f${require_string_member(evidence, 'operation_id')!}'
		if key in business_paths {
			return error('live state repeats one terminal business evidence binding')
		}
		business_paths[key] = path
	}
	mut bindings := []LiveSourceTerminalBinding{}
	mut used_business_keys := []string{}
	for target_id in managed_target_ids {
		target_path := target_state_path(target_id)!
		target_blob := inventory.blobs[target_path] or {
			return error('live state inventory lacks a managed target')
		}
		target := parse_strict_json(target_blob)!
		for handoff in require_array_member(target, 'recovery_handoffs')! {
			if require_string_member(handoff, 'state')! != 'complete'
				|| require_nullable_string_member(handoff, 'terminal_outcome')! != 'source_waiting' {
				continue
			}
			proof := require_object_member(handoff, 'terminal_revalidation')!
			history := require_array_member(proof, 'source_state_cas_history')!
			if history.len != 1 {
				return error('live source terminal binding lacks its single source CAS')
			}
			transition := history[0]
			path := require_string_member(transition, 'evidence_path')!
			business_operation_id := require_string_member(proof, 'business_operation_id')!
			key := '${target_id}\x1f${business_operation_id}'
			business_path := business_paths[key] or {
				return error('live source terminal handoff lacks its unique target business evidence')
			}
			business_evidence := parse_strict_json(inventory.blobs[business_path])!
			expected_business_transition := 'source_unreachable_${require_string_member(require_object_member(proof,
				'source_refetch')!, 'evidence_digest')!}'
			if key in used_business_keys
				|| require_string_member(business_evidence, 'transition')! != expected_business_transition {
				return error('live source terminal handoff and business evidence are not bijective')
			}
			used_business_keys << key
			bindings << LiveSourceTerminalBinding{
				target_id:              target_id
				handoff_id:             require_string_member(handoff, 'handoff_id')!
				business_operation_id:  business_operation_id
				business_evidence_path: business_path
				handoff:                handoff
				proof:                  proof
				transition:             transition
				evidence_path:          path
			}
		}
	}
	mut all_business_keys := business_paths.keys()
	all_business_keys.sort()
	used_business_keys.sort()
	if used_business_keys != all_business_keys {
		return error('live state contains an orphan terminal business evidence')
	}
	return bindings
}

fn load_live_evidence_history(state_git_dir string, head string,
	head_inventory LiveStateInventory) !LiveEvidenceHistory {
	shallow := live_git_args(state_git_dir, ['rev-parse', '--is-shallow-repository'], .scalar)
	if shallow.exit_code != 0 || shallow.output.trim_space() != 'false' {
		return error('live state evidence history is shallow or cannot be classified')
	}
	history_result := live_git_args(state_git_dir, ['rev-list', '--first-parent',
		'--max-count=${live_state_max_first_parent_commits + 1}', head], .history)
	if history_result.exit_code != 0 {
		return error('live state first-parent history cannot be read')
	}
	mut history := []string{}
	mut parents := map[string]string{}
	for raw_line in history_result.output.split_into_lines() {
		commit_sha := raw_line.trim_space()
		if !is_lower_hex_40(commit_sha) || commit_sha in history {
			return error('live state first-parent history contains a malformed commit identity')
		}
		history << commit_sha
	}
	if !live_evidence_history_count_is_within_bound(history.len) {
		return error('live state first-parent history exceeds its recovery bound')
	}
	for index, commit_sha in history {
		commit := live_git_args(state_git_dir, ['cat-file', 'commit', commit_sha], .commit)
		if commit.exit_code != 0 {
			return error('live state first-parent raw commit cannot be read')
		}
		mut raw_parents := []string{}
		for line in commit.output.split_into_lines() {
			if line == '' {
				break
			}
			if line.starts_with('parent ') {
				parent_sha := line.all_after('parent ')
				if !is_lower_hex_40(parent_sha) {
					return error('live state first-parent raw parent is malformed')
				}
				raw_parents << parent_sha
			}
		}
		if index == history.len - 1 && raw_parents.len != 0 {
			return error('live state first-parent evidence history did not reach its root')
		}
		if index < history.len - 1 {
			if raw_parents.len != 1 || raw_parents[0] != history[index + 1] {
				return error('live state first-parent history is non-linear or discontinuous')
			}
			parents[commit_sha] = raw_parents[0]
		}
	}
	disallowed := live_git_args(state_git_dir, ['log', '--first-parent', '--root',
		'--diff-merges=first-parent', '--format=', '--name-only', '--diff-filter=MDCRTUXB', head,
		'--', 'evidence'], .log)
	if disallowed.exit_code != 0 {
		return error('live state evidence mutation history cannot be read')
	}
	if disallowed.output.trim_space() != '' {
		return error('live state evidence history contains a modification, deletion, copy, rename, or reintroduction')
	}
	additions := live_git_args(state_git_dir, ['log', '--first-parent', '--root',
		'--diff-merges=first-parent', '--format=@@%H', '--name-only', '--diff-filter=A', head,
		'--', 'evidence'], .log)
	if additions.exit_code != 0 {
		return error('live state evidence addition history cannot be read')
	}
	mut current_commit := ''
	mut creation_commits := map[string]string{}
	for raw_line in additions.output.split_into_lines() {
		line := raw_line.trim_space()
		if line == '' {
			continue
		}
		if line.starts_with('@@') {
			current_commit = line[2..]
			if !is_lower_hex_40(current_commit) || current_commit !in history {
				return error('live state evidence addition belongs to an unauthenticated history commit')
			}
			continue
		}
		if current_commit == '' || !line.starts_with('evidence/')
			|| !contract_relative_path_is_safe(line) || line in creation_commits {
			return error('live state evidence path was added ambiguously or outside its closed grammar')
		}
		creation_commits[line] = current_commit
	}
	mut head_paths := []string{}
	for path, _ in head_inventory.blobs {
		if path.starts_with('evidence/') {
			head_paths << path
		}
	}
	mut created_paths := creation_commits.keys()
	head_paths.sort()
	created_paths.sort()
	if head_paths != created_paths {
		return error('live state evidence history and HEAD inventory are not one complete append-only set')
	}
	return LiveEvidenceHistory{
		creation_commits: creation_commits
		first_parent:     history
		parents:          parents
	}
}

fn live_first_parent_for_commit(commit_sha string, history LiveEvidenceHistory) !string {
	if commit_sha !in history.first_parent {
		return error('live state terminal commit is outside the authenticated first-parent history')
	}
	parent := history.parents[commit_sha] or {
		return error('live state terminal creation commit lacks one exact parent')
	}
	return parent
}

fn validate_live_source_terminal_group(bindings []LiveSourceTerminalBinding, parent_commit string,
	target_commit string, parent LiveStateInventory, target LiveStateInventory,
	head LiveStateInventory, history LiveEvidenceHistory) ! {
	if bindings.len == 0 {
		return error('live source terminal group is empty')
	}
	mut contexts := []LiveSourceAtomicContext{cap: bindings.len}
	mut target_ids := []string{}
	mut source_signatures := map[string]string{}
	mut source_paths_by_operation := map[string]string{}
	mut group_cas_attempt := 0
	for binding in bindings {
		if binding.target_id in target_ids {
			return error('live source terminal group repeats one target binding')
		}
		target_ids << binding.target_id
		business_creation_commit := history.creation_commits[binding.business_evidence_path] or {
			''
		}
		source_creation_commit := history.creation_commits[binding.evidence_path] or { '' }
		if business_creation_commit != target_commit || source_creation_commit != target_commit {
			return error('live source terminal atomic evidence was not uniquely added by its T commit')
		}
		context := validate_live_source_terminal_binding(binding, parent_commit, parent, target)!
		if context.cas_attempt < 1 || context.cas_attempt > 3 {
			return error('live source terminal CAS attempt is outside the closed 1..3 range')
		}
		if group_cas_attempt == 0 {
			group_cas_attempt = context.cas_attempt
		} else if context.cas_attempt != group_cas_attempt {
			return error('live source terminal group mixes CAS attempts')
		}
		signature := live_shared_source_signature(context)!
		if context.source_operation_id in source_signatures {
			previous_signature := source_signatures[context.source_operation_id]
			if previous_signature != signature
				|| source_paths_by_operation[context.source_operation_id] != context.source_path {
				return error('live source terminal bindings partially share a non-identical source operation')
			}
		} else {
			source_signatures[context.source_operation_id] = signature
			source_paths_by_operation[context.source_operation_id] = context.source_path
		}
		contexts << context
	}
	validate_live_atomic_operation_ids_absent_from_parent(parent, contexts)!
	mut consumers_by_source_operation := map[string][]string{}
	mut context_by_source_operation := map[string]LiveSourceAtomicContext{}
	for context in contexts {
		operation_id := context.source_operation_id
		consumer_id := require_string_member(require_object_member(context.binding.handoff,
			'subject')!, 'consumer_id')!
		mut consumers := consumers_by_source_operation[operation_id] or { []string{} }
		if consumer_id in consumers {
			return error('live source terminal group repeats one consumer for a shared source operation')
		}
		consumers << consumer_id
		consumers_by_source_operation[operation_id] = consumers
		if operation_id !in context_by_source_operation {
			context_by_source_operation[operation_id] = context
		}
	}
	for operation_id, source_consumers in consumers_by_source_operation {
		mut consumers := source_consumers.clone()
		context := context_by_source_operation[operation_id]
		pre_consumers := require_array_member(context.parent_source, 'waiting_consumers')!
		post_consumers := require_array_member(context.target_source, 'waiting_consumers')!
		if post_consumers.len < pre_consumers.len {
			return error('live shared source operation removed a waiting consumer')
		}
		mut pre_ids := []string{cap: pre_consumers.len}
		for index, value in pre_consumers {
			consumer_id := require_string(value)!
			if consumer_id in pre_ids || !json_equal(value, post_consumers[index]) {
				return error('live shared source operation changed or duplicated its consumer prefix')
			}
			pre_ids << consumer_id
		}
		consumers.sort()
		mut expected_suffix := []string{}
		for consumer_id in consumers {
			if consumer_id !in pre_ids {
				expected_suffix << consumer_id
			}
		}
		mut actual_suffix := []string{}
		for value in post_consumers[pre_consumers.len..] {
			consumer_id := require_string(value)!
			if consumer_id in pre_ids || consumer_id in actual_suffix {
				return error('live shared source operation duplicated a consumer in its suffix')
			}
			actual_suffix << consumer_id
		}
		if actual_suffix != expected_suffix {
			return error('live shared source operation consumer suffix differs from its canonical binding union')
		}
	}
	mut source_path_operations := map[string]string{}
	for operation_id, source_path in source_paths_by_operation {
		if source_path in source_path_operations {
			previous_operation := source_path_operations[source_path]
			if previous_operation != operation_id {
				return error('live source terminal T applies multiple incompatible operations to one source path')
			}
		} else {
			source_path_operations[source_path] = operation_id
		}
	}
	new_evidence_paths := validate_live_atomic_inventory_delta(parent, target, target_ids,
		source_path_operations.keys(), 3 * bindings.len + source_signatures.len)!
	for path in new_evidence_paths {
		creation_commit := history.creation_commits[path] or { '' }
		if creation_commit != target_commit {
			return error('live source terminal T evidence lacks its exact global addition commit')
		}
	}
	validate_live_atomic_evidence_set(new_evidence_paths, parent, target, head, contexts,
		group_cas_attempt)!
}

fn validate_live_atomic_operation_ids_absent_from_parent(parent LiveStateInventory,
	contexts []LiveSourceAtomicContext) ! {
	mut expected_ids := []string{}
	for context in contexts {
		for operation in context.target_operations {
			expected_ids << require_string_member(operation, 'operation_id')!
		}
		if context.source_operation_id !in expected_ids {
			expected_ids << context.source_operation_id
		}
	}
	for path, blob in parent.blobs {
		if path.starts_with('evidence/')
			&& require_string_member(parse_strict_json(blob)!, 'operation_id')! in expected_ids {
			return error('live source terminal atomic operation and evidence path already exist in P')
		}
	}
}

fn validate_live_source_terminal_binding(binding LiveSourceTerminalBinding, parent_commit string,
	parent LiveStateInventory, target LiveStateInventory) !LiveSourceAtomicContext {
	proof := binding.proof
	transition := binding.transition
	target_path := target_state_path(binding.target_id)!
	source_id := require_string_member(transition, 'source_id')!
	source_path := source_state_path(source_id)!
	parent_target := parse_strict_json(parent.blobs[target_path] or {
		return error('live source terminal P lacks its target blob')
	})!
	target_target := parse_strict_json(target.blobs[target_path] or {
		return error('live source terminal T lacks its target blob')
	})!
	parent_source := parse_strict_json(parent.blobs[source_path] or {
		return error('live source terminal P lacks its source blob')
	})!
	target_source := parse_strict_json(target.blobs[source_path] or {
		return error('live source terminal T lacks its source blob')
	})!
	embedded_parent_source := require_object_member(proof, 'source_state_pre_snapshot')!
	embedded_target_source := require_object_member(proof, 'source_state_snapshot')!
	embedded_atomic_projection := require_object_member(proof, 'source_atomic_pre_projection')!
	embedded_final_projection := require_object_member(proof, 'final_projection')!
	parent_projection := terminal_state_projection(parent_target)!
	target_projection := terminal_state_projection(target_target)!
	base_generation := require_integer_member(parent_target, 'generation')!
	if !json_equal(parent_source, embedded_parent_source)
		|| !json_equal(target_source, embedded_target_source)
		|| !json_equal(parent_projection, embedded_atomic_projection)
		|| !json_equal(target_projection, embedded_final_projection)
		|| require_string_member(transition, 'expected_state_parent_sha')! != parent_commit
		|| require_integer_member(target_target, 'generation')! != base_generation + 3
		|| !terminal_source_transition_anchor_is_exact(transition, parent_source, target_source, embedded_atomic_projection)!
		|| !source_state_append_is_exact(parent_source, target_source, transition)! {
		return error('live source terminal blobs differ from their authenticated atomic P to T proof')
	}
	mut terminal_matches := 0
	for handoff in require_array_member(target_target, 'recovery_handoffs')! {
		if require_string_member(handoff, 'handoff_id')! == binding.handoff_id {
			terminal_matches++
			if !json_equal(handoff, binding.handoff) {
				return error('live source terminal handoff differs between H and its creation commit T')
			}
		}
	}
	if terminal_matches != 1 {
		return error('live source terminal handoff is absent or duplicated in T')
	}
	target_operations, selected_attempt := validate_live_source_target_operations(binding,
		parent_target, target_target, proof, transition, base_generation)!
	cas_attempt_value := require_integer_member(require_object_member(transition,
		'universal_evidence')!, 'cas_attempt')!
	if cas_attempt_value < 1 || cas_attempt_value > 3 {
		return error('live source terminal CAS attempt is outside the closed 1..3 range')
	}
	cas_attempt := int(cas_attempt_value)
	return LiveSourceAtomicContext{
		binding:             binding
		parent_target:       parent_target
		target_target:       target_target
		parent_source:       parent_source
		target_source:       target_source
		target_operations:   target_operations
		selected_attempt:    selected_attempt
		base_generation:     base_generation
		cas_attempt:         cas_attempt
		source_path:         source_path
		source_operation_id: require_string_member(transition, 'operation_id')!
	}
}

// live_shared_source_contract_signature commits every source-shared fact while deliberately
// excluding the target-local refetch envelope. The snapshots commit the complete consumer set.
// Production and the k>1 contract tests use this same canonical calculation.
pub fn live_shared_source_contract_signature(transition JsonValue, refetch JsonValue,
	parent_source JsonValue, target_source JsonValue) !string {
	shared_refetch := select_object_members(refetch, ['source_state_id', 'source_state_generation',
		'resolution_operation_id', 'source_id', 'source_repository', 'requested_ref', 'previous_sha',
		'resolved_sha', 'resolved_tree', 'status', 'failure_kind', 'checked_at'])!
	facts := object_value_from_pairs(['schema_version', 'audience', 'source_transition',
		'shared_refetch', 'source_state_pre_snapshot', 'source_state_snapshot'], [
		JsonValue{
			kind:      .integer
			int_value: 1
		},
		JsonValue{
			kind:         .string_value
			string_value: 'vlang/v:tccbin-live-shared-source-operation:v1'
		},
		transition,
		shared_refetch,
		parent_source,
		target_source,
	])!
	return json_sha256(facts)
}

fn live_shared_source_signature(context LiveSourceAtomicContext) !string {
	return live_shared_source_contract_signature(context.binding.transition, require_object_member(context.binding.proof,
		'source_refetch')!, context.parent_source, context.target_source)
}

fn validate_live_source_target_operations(binding LiveSourceTerminalBinding,
	parent_target JsonValue, target_target JsonValue, proof JsonValue, transition JsonValue,
	base_generation i64) !([]JsonValue, JsonValue) {
	parent_operations := require_array_member(parent_target, 'applied_operations')!
	target_operations := require_array_member(target_target, 'applied_operations')!
	if target_operations.len != parent_operations.len + 3 {
		return error('live source terminal target delta does not contain exactly three target CAS operations')
	}
	for index, operation in parent_operations {
		if !json_equal(operation, target_operations[index]) {
			return error('live source terminal target operation history is not append-only')
		}
	}
	smoke := require_object_member(proof, 'v_smoke_execution')!
	mut selected_attempt := JsonValue{
		kind: .null_value
	}
	for attempt in require_array_member(smoke, 'attempts')! {
		if require_member(attempt, 'completion_operation_id')!.kind == .string_value {
			selected_attempt = attempt
		}
	}
	if selected_attempt.kind != .object {
		return error('live source terminal target delta lacks its completed smoke attempt')
	}
	smoke_id := require_string_member(selected_attempt, 'completion_operation_id')!
	smoke_digest := v_smoke_terminal_payload_digest(smoke, selected_attempt)!
	business_id := require_string_member(proof, 'business_operation_id')!
	completion_id := require_string_member(target_target, 'last_operation_id')!
	expected_ids := [smoke_id, business_id, completion_id]
	expected_transitions := [
		'v-smoke-complete-${require_integer_member(selected_attempt, 'attempt_index')!}_${smoke_digest}',
		'source_unreachable_${require_string_member(require_object_member(proof, 'source_refetch')!,
			'evidence_digest')!}',
		'handoff_complete_${require_string_member(proof, 'facts_digest')!}',
	]
	for offset in 0 .. 3 {
		operation := target_operations[parent_operations.len + offset]
		if require_string_member(operation, 'operation_id')! != expected_ids[offset]
			|| require_string_member(operation, 'transition')! != expected_transitions[offset]
			|| require_integer_member(operation, 'resulting_generation')! != base_generation + 1 + offset {
			return error('live source terminal target CAS chain is not the exact relative G to G+3 chain')
		}
	}
	if completion_id != require_nullable_string_member(binding.handoff, 'completion_operation_id')! {
		return error('live source terminal target completion operation differs from its handoff')
	}
	if require_string_member(transition, 'operation_id')! in expected_ids {
		return error('live source terminal source and target operations are not distinct')
	}
	return target_operations[parent_operations.len..], selected_attempt
}

fn validate_live_atomic_inventory_delta(parent LiveStateInventory, target LiveStateInventory,
	target_ids []string, source_paths []string, expected_evidence_count int) ![]string {
	mut changed_paths := []string{}
	for path, parent_blob in parent.blobs {
		target_blob := target.blobs[path] or {
			return error('live source terminal T removed a path from P')
		}
		if target_blob != parent_blob {
			changed_paths << path
		}
	}
	changed_paths.sort()
	mut expected_changed := source_paths.clone()
	for target_id in target_ids {
		expected_changed << target_state_path(target_id)!
	}
	expected_changed.sort()
	if changed_paths != expected_changed {
		return error('live source terminal T modified blobs outside its target and source CAS')
	}
	mut new_paths := []string{}
	for path, _ in target.blobs {
		if path !in parent.blobs {
			if !path.starts_with('evidence/') {
				return error('live source terminal T created a non-evidence path')
			}
			new_paths << path
		}
	}
	new_paths.sort()
	if new_paths.len != expected_evidence_count {
		return error('live source terminal T evidence cardinality differs from exact 3k+s')
	}
	return new_paths
}

fn validate_live_atomic_evidence_set(new_paths []string, parent LiveStateInventory,
	target LiveStateInventory, head LiveStateInventory, contexts []LiveSourceAtomicContext,
	group_cas_attempt int) ! {
	if group_cas_attempt < 1 || group_cas_attempt > 3 {
		return error('live source terminal evidence group has an invalid CAS attempt')
	}
	mut expected_ids := []string{}
	mut source_contexts := map[string]LiveSourceAtomicContext{}
	mut source_order := []string{}
	for context in contexts {
		smoke_id := require_string_member(context.selected_attempt, 'completion_operation_id')!
		business_id := context.binding.business_operation_id
		completion_id := require_string_member(context.target_target, 'last_operation_id')!
		for operation_id in [smoke_id, business_id, completion_id] {
			if operation_id in expected_ids {
				return error('live source terminal T shares target evidence across bindings')
			}
			expected_ids << operation_id
		}
		if context.source_operation_id !in source_contexts {
			source_contexts[context.source_operation_id] = context
			source_order << context.source_operation_id
		}
	}
	for operation_id in source_order {
		if operation_id in expected_ids {
			return error('live source terminal T reuses a target operation for SourceState')
		}
		expected_ids << operation_id
	}
	for path, blob in parent.blobs {
		if path.starts_with('evidence/')
			&& require_string_member(parse_strict_json(blob)!, 'operation_id')! in expected_ids {
			return error('live source terminal atomic operation was already evidenced in P')
		}
	}
	mut evidence_by_operation := map[string]JsonValue{}
	mut path_by_operation := map[string]string{}
	for path in new_paths {
		blob := target.blobs[path] or {
			return error('live source terminal T evidence blob disappeared')
		}
		head_blob := head.blobs[path] or {
			return error('live source terminal evidence was removed after T')
		}
		if head_blob != blob {
			return error('live source terminal evidence was modified or removed after T')
		}
		evidence := parse_strict_json(blob)!
		operation_id := require_string_member(evidence, 'operation_id')!
		if operation_id in evidence_by_operation {
			return error('live source terminal T duplicated an evidence operation')
		}
		evidence_by_operation[operation_id] = evidence
		path_by_operation[operation_id] = path
	}
	mut observed_ids := evidence_by_operation.keys()
	mut sorted_expected_ids := expected_ids.clone()
	observed_ids.sort()
	sorted_expected_ids.sort()
	if observed_ids != sorted_expected_ids {
		return error('live source terminal T evidence set differs from its exact 3k+s operations')
	}
	mut group_facts := []LiveAtomicGroupContractFact{cap: contexts.len}
	for context in contexts {
		mut target_evidence_operation_ids := []string{cap: 3}
		for operation in context.target_operations {
			target_evidence_operation_ids << require_string_member(operation, 'operation_id')!
		}
		group_facts << LiveAtomicGroupContractFact{
			target_id:                     context.binding.target_id
			source_operation_id:           context.source_operation_id
			source_signature:              live_shared_source_signature(context)!
			target_evidence_operation_ids: target_evidence_operation_ids
		}
	}
	validate_live_atomic_group_contract(group_facts, observed_ids)!
	for source_index, operation_id in source_order {
		context := source_contexts[operation_id]
		transition := context.binding.transition
		universal := require_object_member(transition, 'universal_evidence')!
		source_evidence_path := path_by_operation[operation_id] or {
			return error('live source terminal source evidence path is absent')
		}
		source_evidence := evidence_by_operation[operation_id] or {
			return error('live source terminal source evidence is absent')
		}
		expected_ordinal := contexts.len + source_index
		if source_evidence_path != context.binding.evidence_path
			|| !json_equal(source_evidence, universal)
			|| require_integer_member(universal, 'operation_ordinal')! != expected_ordinal
			|| require_integer_member(universal, 'cas_attempt')! != group_cas_attempt
			|| require_string_member(transition, 'universal_evidence_digest')! != source_state_universal_evidence_digest(universal)!
			|| require_string_member(transition, 'previous_state_digest')! != source_state_snapshot_digest(context.parent_source)!
			|| require_string_member(transition, 'resulting_state_digest')! != source_state_snapshot_digest(context.target_source)! {
			return error('live source terminal source evidence differs from the exact P to T SourceState CAS or ordinal')
		}
	}
	for binding_index, context in contexts {
		smoke_id := require_string_member(context.selected_attempt, 'completion_operation_id')!
		business_id := context.binding.business_operation_id
		completion_id := require_string_member(context.target_target, 'last_operation_id')!
		smoke_completed_at := require_string_member(context.selected_attempt, 'completed_at')!
		business_checked_at := require_string_member(require_object_member(context.binding.proof,
			'source_refetch')!, 'checked_at')!
		terminal_completed_at := require_string_member(context.binding.handoff,
			'terminal_completed_at')!
		if exact_timestamp_unix(smoke_completed_at)! >= exact_timestamp_unix(business_checked_at)!
			|| exact_timestamp_unix(business_checked_at)! >= exact_timestamp_unix(terminal_completed_at)! {
			return error('live source terminal target evidence timestamps are not strictly causal')
		}
		base_business_ordinal := contexts.len + source_order.len + 2 * binding_index
		validate_live_target_operation_evidence(evidence_by_operation[smoke_id],
			path_by_operation[smoke_id], context, context.target_operations[0], 'smoke',
			binding_index, group_cas_attempt)!
		validate_live_target_operation_evidence(evidence_by_operation[business_id],
			path_by_operation[business_id], context, context.target_operations[1], 'business',
			base_business_ordinal, group_cas_attempt)!
		validate_live_target_operation_evidence(evidence_by_operation[completion_id],
			path_by_operation[completion_id], context, context.target_operations[2], 'completion',

			base_business_ordinal + 1, group_cas_attempt)!
	}
}

fn validate_live_target_operation_evidence(evidence JsonValue, evidence_relative_path string,
	context LiveSourceAtomicContext, operation JsonValue, role string, expected_ordinal int,
	group_cas_attempt int) ! {
	target_id := context.binding.target_id
	transition := require_string_member(operation, 'transition')!
	generation_written := require_integer_member(operation, 'resulting_generation')!
	generation_read := generation_written - 1
	proof := context.binding.proof
	handoff := context.binding.handoff
	pre_projection := require_object_member(proof, 'source_atomic_pre_projection')!
	expected_run_id := if role == 'smoke' {
		require_integer_member(context.selected_attempt, 'run_id')!
	} else {
		require_nullable_integer(handoff, 'selected_run_id')!
	}
	expected_run_attempt := if role == 'smoke' {
		require_integer_member(context.selected_attempt, 'run_attempt')!
	} else {
		require_nullable_integer(handoff, 'selected_run_attempt')!
	}
	expected_workflow := if role == 'smoke' {
		require_string_member(context.selected_attempt, 'workflow_path')!
	} else {
		require_string_member(handoff, 'workflow_path')!
	}
	expected_workflow_ref := if role == 'smoke' {
		require_string_member(context.selected_attempt, 'workflow_ref')!
	} else {
		require_string_member(handoff, 'workflow_ref')!
	}
	expected_workflow_sha := if role == 'smoke' {
		require_string_member(pre_projection, 'v_source_sha')!
	} else {
		require_nullable_string_member(handoff, 'receiver_master_sha')!
	}
	intent_id := require_string_member(handoff, 'intent_or_operation_id')!
	subject_fingerprint := require_string_member(context.parent_target, 'input_fingerprint')!
	input_fingerprint := subject_fingerprint
	artifact_fingerprint := require_string_member(context.parent_target, 'artifact_fingerprint')!
	manifest_hash := require_string_member(context.parent_target, 'manifest_hash')!
	native_subject_hash := require_nullable_string_member(pre_projection, 'native_subject_hash')!
	source_kind := require_string_member(require_object_member(proof, 'source_refetch')!,
		'source_id')!
	source_ref, source_sha := live_primary_source_identity(context.parent_target, source_kind)!
	// The role transition is deliberately logical and non-recursive. The stored transition keeps
	// its full payload digest and is validated independently below; using that decorated string as
	// identity input would create an impossible operation-id <-> payload-digest fixed point.
	identity_transition := match role {
		'smoke' { 'v-smoke-complete' }
		'business' { 'source_unreachable' }
		'completion' { 'handoff_complete' }
		else { return error('live target evidence has an unknown operation role') }
	}
	expected_operation_id := deterministic_operation_id(OperationIdentityInput{
		audience:                'vlang/v:tccbin-automation-state'
		run_id:                  expected_run_id
		run_attempt:             int(expected_run_attempt)
		ordinal:                 expected_ordinal
		cas_attempt:             group_cas_attempt
		subject_id:              target_id
		transition:              identity_transition
		expected_generation:     generation_read
		expected_canonical_head: require_string_member(context.parent_target,
			'canonical_observed_sha')!
		source_ref:              source_ref
		source_sha:              source_sha
		subject_fingerprint:     subject_fingerprint
		input_fingerprint:       input_fingerprint
		artifact_fingerprint:    artifact_fingerprint
		manifest_hash:           manifest_hash
		native_subject_hash:     native_subject_hash
		intent_id:               intent_id
	})!
	evidence_timestamp := match role {
		'smoke' {
			require_string_member(context.selected_attempt, 'completed_at')!
		}
		'business' {
			require_string_member(require_object_member(proof, 'source_refetch')!, 'checked_at')!
		}
		'completion' {
			require_string_member(handoff, 'terminal_completed_at')!
		}
		else {
			return error('live target evidence has an unknown timestamp role')
		}
	}
	digests := require_array_member(evidence, 'digests')!
	if require_string_member(evidence, 'operation_id')! != require_string_member(operation, 'operation_id')!
		|| require_string_member(evidence, 'operation_id')! != expected_operation_id
		|| require_integer_member(evidence, 'operation_ordinal')! != expected_ordinal
		|| require_integer_member(evidence, 'cas_attempt')! != group_cas_attempt
		|| require_integer_member(evidence, 'run_id')! != expected_run_id
		|| require_integer_member(evidence, 'run_attempt')! != expected_run_attempt
		|| require_nullable_string_member(evidence, 'intent_id')! != intent_id
		|| require_string_member(evidence, 'workflow')! != expected_workflow
		|| require_string_member(evidence, 'workflow_ref')! != expected_workflow_ref
		|| require_string_member(evidence, 'workflow_sha')! != expected_workflow_sha
		|| require_string_member(evidence, 'subject_id')! != target_id
		|| require_nullable_string_member(evidence, 'target_id')! != target_id
		|| require_string_member(evidence, 'subject_fingerprint')! != subject_fingerprint
		|| require_nullable_string_member(evidence, 'input_fingerprint')! != input_fingerprint
		|| require_nullable_string_member(evidence, 'artifact_fingerprint')! != artifact_fingerprint
		|| require_string_member(evidence, 'transition')! != transition
		|| require_integer_member(evidence, 'generation_read')! != generation_read
		|| require_integer_member(evidence, 'generation_written')! != generation_written
		|| require_string_member(evidence, 'result')! != 'blocked' || digests.len != 1
		|| require_string_member(digests[0], 'path')! != target_state_path(target_id)!
		|| require_string_member(digests[0], 'sha256')! != json_sha256(context.target_target)
		|| !live_evidence_path_recomputes(evidence_relative_path, evidence, evidence_timestamp)! {
		return error('live target operation evidence differs from its atomic target CAS')
	}
}

fn live_primary_source_identity(target_state JsonValue, source_id string) !(string, string) {
	resolved_inputs := require_object_member(target_state, 'resolved_inputs')!
	mut matches := []JsonValue{}
	for source in require_array_member(resolved_inputs, 'sources')! {
		if require_string_member(source, 'id')! == source_id {
			matches << source
		}
	}
	if matches.len != 1 {
		return error('live target evidence lacks one exact outage source identity')
	}
	return require_string_member(matches[0], 'ref')!, require_string_member(matches[0], 'sha')!
}

fn live_evidence_path_recomputes(relative_path string, evidence JsonValue,
	authoritative_timestamp string) !bool {
	segments := relative_path.split('/')
	if segments.len != 8 {
		return false
	}
	exact_timestamp_unix(authoritative_timestamp)!
	year := authoritative_timestamp[..4].int()
	month := authoritative_timestamp[5..7].int()
	expected := evidence_path(year, month, require_integer_member(evidence, 'run_id')!, int(require_integer_member(evidence,
		'run_attempt')!), require_string_member(evidence, 'subject_id')!, require_string_member(evidence,
		'operation_id')!, require_integer_member(evidence, 'generation_written')!, require_string_member(evidence,
		'transition')!, require_string_member(evidence, 'subject_fingerprint')!)!
	return expected == relative_path
}

fn load_live_state_inventory_at_commit(automation_root string, state_git_dir string,
	expected_state_head string) !LiveStateInventory {
	validate_live_state_bare_repository(state_git_dir, expected_state_head)!
	result := live_git_args(state_git_dir, ['ls-tree', '-r', '-l', '-z', expected_state_head],
		.inventory)
	if result.exit_code != 0 || i64(result.output.len) > live_state_max_tracked_bytes {
		return error('live state Git inventory cannot be read inside its byte bound')
	}
	mut acquired := []LiveStateInventoryBlob{}
	for record in result.output.split('\x00') {
		if record == '' {
			continue
		}
		parts := record.split_nth('\t', 2)
		if parts.len != 2 {
			return error('live state Git inventory record is malformed')
		}
		metadata := parts[0].fields()
		if metadata.len != 4 || metadata[0] != '100644' || metadata[1] != 'blob'
			|| !is_lower_hex_40(metadata[2]) {
			return error('live state Git inventory contains a non-regular entry')
		}
		size_source := metadata[3]
		size := size_source.i64()
		if size < 0 || size_source != size.str() {
			return error('live state Git inventory contains a noncanonical size')
		}
		blob := live_git_args(state_git_dir, ['cat-file', 'blob', metadata[2]], .target_blob)
		if blob.exit_code != 0 || i64(blob.output.len) != size {
			return error('live state Git blob is missing or differs from its tree size')
		}
		acquired << LiveStateInventoryBlob{
			path:   parts[1]
			mode:   metadata[0]
			kind:   metadata[1]
			oid:    metadata[2]
			size:   size
			source: blob.output
		}
	}
	return validate_live_state_inventory_blobs(automation_root, acquired)
}

// validate_live_state_inventory_blobs has no filesystem or Git authority. Callers must acquire and
// bind every byte first; this function then applies the complete schema, path and semantic checks.
fn validate_live_state_inventory_blobs(automation_root string,
	records []LiveStateInventoryBlob) !LiveStateInventory {
	mut target_paths := []string{}
	mut source_paths := []string{}
	mut blobs := map[string]string{}
	mut evidence_operations := map[string]string{}
	mut tracked_count := 0
	mut tracked_bytes := i64(0)
	for record in records {
		if record.mode != '100644' || record.kind != 'blob' || !is_lower_hex_40(record.oid) {
			return error('live state Git inventory contains a non-regular entry')
		}
		path := record.path
		size := record.size
		max_size := if path.starts_with('targets/') {
			live_target_state_max_bytes
		} else {
			live_source_evidence_max_bytes
		}
		if size < 0 || size > max_size || i64(record.source.len) != size
			|| git_blob_oid(record.source.bytes()) != record.oid
			|| !contract_relative_path_is_safe(path) || path in blobs {
			return error('live state Git entry is oversized, duplicated, or has an unsafe path')
		}
		tracked_count++
		tracked_bytes += size
		if tracked_count > live_state_max_tracked_files
			|| tracked_bytes > live_state_max_tracked_bytes {
			return error('live state Git inventory exceeds its global bounds')
		}
		value := parse_strict_json(record.source)!
		if path.starts_with('targets/') {
			target_paths << path
			issues := validate_json_value(os.join_path(automation_root, 'schemas',
				'target-state.schema.json'), value)!
			if issues.len > 0 || require_integer_member(value, 'schema_version')! != 1
				|| path != target_state_path(require_string_member(value, 'target_id')!)! {
				return error('live target state schema or path identity is invalid')
			}
		} else if path.starts_with('sources/') {
			source_paths << path
			issues := validate_json_value(os.join_path(automation_root, 'schemas',
				'source-state.schema.json'), value)!
			if issues.len > 0 || require_integer_member(value, 'schema_version')! != 2
				|| path != source_state_path(require_string_member(value, 'source_id')!)! {
				return error('live source state schema or path identity is invalid')
			}
		} else if path.starts_with('evidence/') {
			validate_live_evidence_value(automation_root, value, path)!
			operation_id := require_string_member(value, 'operation_id')!
			if operation_id in evidence_operations {
				return error('live state evidence inventory repeats an operation ID')
			}
			evidence_operations[operation_id] = path
		} else {
			return error('live state ref contains a path outside targets/sources/evidence')
		}
		blobs[path] = record.source
	}
	expected_targets := managed_target_ids.map('targets/${it}.json')
	expected_sources := ['sources/tinycc-mob.json', 'sources/bdwgc-master.json',
		'sources/libatomic_ops-master.json']
	target_paths.sort()
	source_paths.sort()
	mut sorted_targets := expected_targets.clone()
	mut sorted_sources := expected_sources.clone()
	sorted_targets.sort()
	sorted_sources.sort()
	if target_paths != sorted_targets || source_paths != sorted_sources {
		return error('live state ref does not contain exactly six targets and three sources')
	}
	return LiveStateInventory{
		blobs: blobs
	}
}

fn validate_live_evidence_value(automation_root string, evidence JsonValue,
	relative_path string) ! {
	segments := relative_path.split('/')
	if segments.len != 8 || segments[0] != 'evidence' || segments[1].len != 4
		|| segments[2].len != 2 || segments[3].i64() <= 0 || segments[4].int() <= 0
		|| !safe_path_segment(segments[5]) || !is_lower_hex_64(segments[6])
		|| !segments[7].ends_with('.json') {
		return error('live evidence path does not follow the injective grammar')
	}
	stem := segments[7].all_before_last('.json')
	first_dash := stem.index('-') or { return error('live evidence filename lacks generation') }
	if stem.len < first_dash + 1 + 1 + 64 || stem[stem.len - 65] != `-` {
		return error('live evidence filename lacks transition or subject fingerprint')
	}
	generation := stem[..first_dash].i64()
	transition := stem[first_dash + 1..stem.len - 65]
	subject_fingerprint := stem[stem.len - 64..]
	if generation < 0 || !safe_path_segment(transition) || !is_lower_hex_64(subject_fingerprint) {
		return error('live evidence filename identity is invalid')
	}
	issues := validate_json_value(os.join_path(automation_root, 'schemas', 'evidence.schema.json'),
		evidence)!
	if issues.len > 0 {
		return error('live evidence failed its authoritative schema')
	}
	if require_string_member(evidence, 'operation_id')! != segments[6]
		|| require_integer_member(evidence, 'run_id')! != segments[3].i64()
		|| require_integer_member(evidence, 'run_attempt')! != segments[4].i64()
		|| require_string_member(evidence, 'subject_id')! != segments[5]
		|| require_string_member(evidence, 'transition')! != transition
		|| require_integer_member(evidence, 'generation_written')! != generation
		|| require_string_member(evidence, 'subject_fingerprint')! != subject_fingerprint {
		return error('live evidence body identity differs from its injective path')
	}
}

$if test {
	pub struct DurableGitConfigSnapshotForTest {
	pub:
		source string
		keys   []string
	}

	pub struct DurableGitControlTraceForTest {
	pub:
		scenario              string
		go_count              int
		kill_count            int
		wait_count            int
		lease_retained        bool
		second_runner_refused bool
		forbidden_rejections  int
		failure               string
	}

	pub struct DurableGitAbortStateForTest {
	pub:
		requested           bool
		termination_secured bool
		signal_drift        bool
		poison_required     bool
		bound_expired       bool
		failure             string
	}

	pub fn durable_git_planner_failure_flow_for_test(nested_runner string, core string,
	cleanup string) string {
		mut session := &DurableGitRunnerSession{}
		durable_git_record_first_failure(mut session, nested_runner)
		if core != '' {
			durable_git_record_first_failure(mut session, core)
		}
		if cleanup != '' {
			durable_git_record_first_failure(mut session, cleanup)
		}
		return durable_git_prioritized_failure(session, core, cleanup)
	}

	// This closed seam accepts no argv, executable, path, callback, or process handle. It drives
	// the same post-fork routing, action authorization, post-EOF, and atomic lease transitions used
	// by the real runner.
	pub fn durable_git_control_machine_for_test(scenario string) !DurableGitControlTraceForTest {
		mut test_slot := stdatomic.new_atomic(voidptr(unsafe { nil }))
		mut first_token_byte := u8(1)
		mut second_token_byte := u8(2)
		first_token := voidptr(&first_token_byte)
		second_token := voidptr(&second_token_byte)
		if !durable_git_lease_try_acquire(mut test_slot, first_token) {
			return error('durable Git control-machine test could not acquire its closed local lease')
		}
		mut control := DurableGitControlState{}
		mut failure := ''
		match scenario {
			'postfork_drift' {
				durable_git_control_route_postfork(mut control, durable_git_sigchld_drift_message) or {
					failure = err.msg()
				}
			}
			'gate_failure' {
				durable_git_control_route_postfork(mut control, '')!
				durable_git_control_authorize(mut control, .go_release, '')!
				failure = 'durable Git runner child release gate is invalid'
				durable_git_control_authorize(mut control, .kill, '')!
				durable_git_control_authorize(mut control, .wait, '')!
			}
			'cap_failure' {
				durable_git_control_route_postfork(mut control, '')!
				durable_git_control_authorize(mut control, .go_release, '')!
				durable_git_control_authorize(mut control, .kill, '')!
				durable_git_control_authorize(mut control, .kill, '')!
				durable_git_control_authorize(mut control, .wait, '')!
				durable_git_require_clean_post_eof(DurableGitAbortState{
					requested: true
					failure:   'durable Git runner command output exceeds its closed byte bound'
				}) or { failure = err.msg() }
			}
			'post_eof_abort' {
				durable_git_control_route_postfork(mut control, '')!
				durable_git_control_authorize(mut control, .go_release, '')!
				durable_git_control_authorize(mut control, .kill, '')!
				durable_git_control_authorize(mut control, .kill, '')!
				durable_git_control_authorize(mut control, .wait, '')!
				durable_git_require_clean_post_eof(DurableGitAbortState{
					requested: true
					failure:   'durable Git runner command exceeded its monotonic deadline'
				}) or { failure = err.msg() }
			}
			'poison_after_go' {
				durable_git_control_route_postfork(mut control, '')!
				durable_git_control_authorize(mut control, .go_release, '')!
				durable_git_control_authorize(mut control, .kill, durable_git_sigchld_drift_message) or {
					failure = err.msg()
				}
			}
			else {
				_ = durable_git_lease_try_release(mut test_slot, first_token)
				return error('durable Git control-machine test scenario is outside its closed set')
			}
		}
		mut forbidden_rejections := 0
		if control.poisoned {
			for action in [DurableGitParentAction.go_release, .kill, .wait] {
				durable_git_control_authorize(mut control, action, '') or { forbidden_rejections++ }
			}
		}
		lease_retained := !durable_git_control_lease_releasable(control.poisoned)
		if !lease_retained && !durable_git_lease_try_release(mut test_slot, first_token) {
			return error('durable Git control-machine test could not release its closed local lease')
		}
		second_acquired := durable_git_lease_try_acquire(mut test_slot, second_token)
		if second_acquired {
			if !durable_git_lease_try_release(mut test_slot, second_token) {
				return error('durable Git control-machine test could not release its second local lease')
			}
		}
		return DurableGitControlTraceForTest{
			scenario:              scenario
			go_count:              control.go_count
			kill_count:            control.kill_count
			wait_count:            control.wait_count
			lease_retained:        lease_retained
			second_runner_refused: !second_acquired
			forbidden_rejections:  forbidden_rejections
			failure:               failure
		}
	}

	pub fn durable_git_environment_for_test(entries []string) ![]string {
		return durable_git_closed_environment_from_entries(entries)
	}

	pub fn validate_durable_git_config_for_test(source string) ! {
		_ := durable_git_validate_config_keys(source)!
	}

	pub fn parse_durable_git_physical_config_for_test(source string) ![]string {
		return durable_git_parse_physical_config(source)
	}

	pub fn durable_git_config_snapshot_for_test(state_git_dir string) !DurableGitConfigSnapshotForTest {
		snapshot := durable_git_config_snapshot(state_git_dir)!
		return DurableGitConfigSnapshotForTest{
			source: snapshot.source.clone()
			keys:   snapshot.keys.clone()
		}
	}

	pub fn join_durable_git_config_for_test(physical_source string,
	git_output string) ![]string {
		return durable_git_join_config_keys(durable_git_parse_physical_config(physical_source)!,
			git_output)
	}

	pub fn durable_git_abort_reducer_for_test(group_required bool, group_secured bool,
	pid_secured bool, checkpoint_failure string, hard_failure string, now u64,
	observe_at u64) DurableGitAbortStateForTest {
		attempt := DurableGitTerminationAttempt{
			group_required:     group_required
			group_secured:      group_secured && checkpoint_failure == ''
			pid_secured:        pid_secured && checkpoint_failure == ''
			checkpoint_failure: checkpoint_failure
			hard_failure:       hard_failure
		}
		abort := durable_git_abort_state_from_attempt('test abort', attempt, now)
		bound_expired := durable_git_abort_bound_expired(observe_at, abort)
		return DurableGitAbortStateForTest{
			requested:           abort.requested
			termination_secured: abort.termination_secured
			signal_drift:        abort.signal_drift
			poison_required:     durable_git_termination_requires_poison(attempt) || bound_expired
			bound_expired:       bound_expired
			failure:             abort.failure
		}
	}

	pub fn durable_git_post_eof_acceptance_for_test(failure string) ! {
		durable_git_require_clean_post_eof(DurableGitAbortState{
			requested: failure != ''
			failure:   failure
		})!
	}

	pub fn durable_git_kill_sequence_for_test(outcomes []string) !bool {
		$if linux || macos || freebsd || openbsd {
			if outcomes.len == 0 || outcomes.len > 8 {
				return error('durable Git kill test sequence is outside its retry bound')
			}
			for attempt, outcome in outcomes {
				mut exit_code := -1
				mut error_code := 0
				match outcome {
					'secured' { exit_code = 0 }
					'absent' { error_code = C.ESRCH }
					'interrupted' { error_code = C.EINTR }
					'hard' { error_code = C.EPERM }
					else { return error('durable Git kill test outcome is unknown') }
				}
				match durable_git_classify_kill_result(exit_code, error_code, attempt) {
					.secured { return true }
					.retry { continue }
					.hard_failure { return false }
				}
			}
			return false
		} $else {
			return error('durable Git kill reducer test is unavailable on this platform')
		}
	}

	pub fn durable_git_wait_disposition_for_test(observation string) !string {
		$if linux || macos || freebsd || openbsd {
			pid := 42
			mut waited := -1
			mut error_code := 0
			match observation {
				'terminal' { waited = pid }
				'running' { waited = 0 }
				'interrupted' { error_code = C.EINTR }
				'lost' { error_code = C.ECHILD }
				'foreign' { waited = 41 }
				else { return error('durable Git wait test observation is unknown') }
			}
			return durable_git_classify_wait_result(pid, waited, error_code).str()
		} $else {
			return error('durable Git wait reducer test is unavailable on this platform')
		}
	}

	pub fn durable_git_wait_exclusivity_error_for_test(observation string) !string {
		disposition := match observation {
			'lost' { DurableGitWaitDisposition.lost }
			'foreign' { DurableGitWaitDisposition.foreign }
			else { return error('durable Git wait exclusivity test observation is unknown') }
		}
		return durable_git_wait_exclusivity_error(disposition)
	}

	pub fn durable_git_argv_for_test(git_path string, state_git_dir string, command []string,
	repository bool) ![]string {
		return durable_git_build_argv(git_path, state_git_dir, command, repository)
	}

	pub fn durable_git_fd_tuple_is_valid_for_test(fds []int) bool {
		return durable_git_fd_tuple_is_valid(fds)
	}

	pub fn durable_planner_platform_gate_for_test(platform string, mut opened []string) ! {
		if platform == 'windows' {
			return error('durable target commit planning is unavailable on Windows without a raw-byte Git runner')
		}
		if platform !in ['linux', 'macos', 'freebsd', 'openbsd'] {
			return error('durable target commit planning is unavailable on this POSIX platform without mandatory bulk descriptor close')
		}
		opened << 'runner-preflight'
	}

	pub fn durable_git_runner_trace_for_test(state_git_dir string,
	command []string) ![]string {
		mut session := durable_git_runner_begin(state_git_dir)!
		mut failure := ''
		mut trace := []string{}
		result := durable_git_capture(mut session, command, .scalar, true) or {
			failure = err.msg()
			DurableGitCapturedResult{}
		}
		if failure == '' {
			if result.exit_code != 0 || result.stderr != '' {
				failure = 'durable Git runner trace command did not exit cleanly'
			} else {
				trace = result.trace.clone()
			}
		}
		durable_git_runner_end(mut session)!
		if failure != '' {
			return error(failure)
		}
		return trace
	}

	pub fn durable_git_authority_adapter_rejection_for_test(state_git_dir string,
	arguments string) !string {
		mut session := durable_git_runner_begin(state_git_dir)!
		result := live_git(state_git_dir, arguments)
		durable_git_runner_end(mut session)!
		if result.exit_code != 125 {
			return error('durable Git authority adapter accepted noncanonical arguments')
		}
		return result.output
	}
}
