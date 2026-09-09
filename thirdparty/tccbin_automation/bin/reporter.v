module bin

import crypto.sha256

pub const diagnostic_max_bytes = 256 * 1024
pub const issue_bot_zone_max_bytes = 48 * 1024
pub const issue_summary_max_bytes = 4 * 1024
pub const issue_summary_max_lines = 80

const issue_os_allowlist = ['freebsd', 'linux', 'macos', 'openbsd', 'windows']

// DiagnosticRecord mirrors diagnostic.schema.json exactly; no reporter-only field is smuggled in.
pub struct DiagnosticRecord {
pub:
	schema_version       int
	repository           string
	os                   string
	target_id            string
	architecture         string
	component            string
	failure_class        string
	test_id              string
	lane                 string
	expected             string
	observed_summary     string
	subject_sha          string
	input_fingerprint    string
	artifact_fingerprint string
	run_url              string
	job_url              string
	artifact_url         ?string
	human_action         string
}

// IncidentProjectionInput adds ledger status outside the diagnostic schema vocabulary.
pub struct IncidentProjectionInput {
pub:
	diagnostic DiagnosticRecord
	status     string
}

// IssueProjectionEntry is one exact ABI+target+architecture+component+failure-class group.
pub struct IssueProjectionEntry {
pub:
	abi           string
	target_id     string
	architecture  string
	component     string
	failure_class string
	status        string
	diagnostics   []DiagnosticRecord
}

// IssueProjectionModel is the single bounded owner-repository and OS projection.
pub struct IssueProjectionModel {
pub:
	schema_version   int
	owner_repository string
	os               string
	marker_hash      string
	title            string
	entries          []IssueProjectionEntry
	should_be_open   bool
}

// FailureRoutingDecision normalizes both ownership and the persisted diagnostic class.
pub struct FailureRoutingDecision {
pub:
	owner_repository string
	failure_class    string
}

// project_issue_ledger strictly re-reads a bounded snapshot of the protected state ledger and
// derives the one owner+OS issue projection without any GitHub or filesystem mutation.
pub fn project_issue_ledger(source string) !IssueProjectionModel {
	if source.len > diagnostic_max_bytes {
		return error('issue projection ledger snapshot exceeds its strict byte bound')
	}
	root := parse_strict_json(source)!
	require_exact_keys(root, ['schema_version', 'audience', 'state_ref', 'state_commit_sha',
		'owner_repository', 'os', 'targets'])!
	if require_integer_member(root, 'schema_version')! != 1
		|| require_string_member(root, 'audience')! != 'vlang/v:tccbin-automation-state'
		|| require_string_member(root, 'state_ref')! != state_ref
		|| !is_lower_hex_40(require_string_member(root, 'state_commit_sha')!) {
		return error('issue projection ledger header is not the protected state contract')
	}
	owner_repository := require_string_member(root, 'owner_repository')!
	os_family := require_string_member(root, 'os')!
	target_values := require_array_member(root, 'targets')!
	if target_values.len == 0 || target_values.len > managed_target_ids.len {
		return error('issue projection ledger target inventory is empty or unbounded')
	}
	mut target_ids := []string{}
	mut incident_ids := []string{}
	mut incidents := []IncidentProjectionInput{}
	for target_value in target_values {
		require_exact_keys(target_value, ['path', 'target_id', 'generation', 'owner_repository',
			'input_fingerprint', 'artifact_fingerprint', 'incidents'])!
		target_id := require_string_member(target_value, 'target_id')!
		generation := require_integer_member(target_value, 'generation')!
		input_fingerprint := require_string_member(target_value, 'input_fingerprint')!
		artifact_fingerprint :=
			require_nullable_string_member(target_value, 'artifact_fingerprint')!
		if target_id !in managed_target_ids || target_id in target_ids
			|| generation < 0
			|| require_string_member(target_value, 'path')! != target_state_path(target_id)!
			|| require_string_member(target_value, 'owner_repository')! != owner_repository
			|| target_os(target_id)! != os_family
			|| !is_lower_hex_64(input_fingerprint)
			|| (artifact_fingerprint != '' && !is_lower_hex_64(artifact_fingerprint)) {
			return error('issue projection target snapshot is invalid, stale, or duplicated')
		}
		target_ids << target_id
		incident_values := require_array_member(target_value, 'incidents')!
		if incident_values.len > 256 {
			return error('issue projection target incident inventory is unbounded')
		}
		for incident_value in incident_values {
			require_exact_keys(incident_value, ['incident_id', 'status', 'diagnostic'])!
			incident_id := require_string_member(incident_value, 'incident_id')!
			if !is_lower_hex_64(incident_id) || incident_id in incident_ids {
				return error('issue projection incident identity is invalid or duplicated')
			}
			diagnostic :=
				parse_issue_diagnostic(require_object_member(incident_value, 'diagnostic')!)!
			if diagnostic.repository != owner_repository || diagnostic.os != os_family
				|| diagnostic.target_id != target_id
				|| diagnostic.input_fingerprint != input_fingerprint
				|| diagnostic.artifact_fingerprint != artifact_fingerprint {
				return error('issue diagnostic does not match its persisted target snapshot')
			}
			incident_ids << incident_id
			incidents << IncidentProjectionInput{
				diagnostic: diagnostic
				status:     require_string_member(incident_value, 'status')!
			}
		}
	}
	return project_issue(owner_repository, os_family, incidents)!
}

fn parse_issue_diagnostic(value JsonValue) !DiagnosticRecord {
	require_exact_keys(value, ['schema_version', 'repository', 'os', 'target_id', 'architecture',
		'component', 'failure_class', 'test_id', 'lane', 'expected', 'observed_summary',
		'subject_sha', 'input_fingerprint', 'artifact_fingerprint', 'run_url', 'job_url',
		'artifact_url', 'human_action'])!
	artifact_fingerprint := require_nullable_string_member(value, 'artifact_fingerprint')!
	artifact_url_value := require_member(value, 'artifact_url')!
	if artifact_url_value.kind != .null_value && artifact_url_value.kind != .string_value {
		return error('diagnostic artifact URL must be null or a string')
	}
	mut diagnostic := DiagnosticRecord{
		schema_version:       int(require_integer_member(value, 'schema_version')!)
		repository:           require_string_member(value, 'repository')!
		os:                   require_string_member(value, 'os')!
		target_id:            require_string_member(value, 'target_id')!
		architecture:         require_string_member(value, 'architecture')!
		component:            require_string_member(value, 'component')!
		failure_class:        require_string_member(value, 'failure_class')!
		test_id:              require_string_member(value, 'test_id')!
		lane:                 require_string_member(value, 'lane')!
		expected:             require_string_member(value, 'expected')!
		observed_summary:     require_string_member(value, 'observed_summary')!
		subject_sha:          require_string_member(value, 'subject_sha')!
		input_fingerprint:    require_string_member(value, 'input_fingerprint')!
		artifact_fingerprint: artifact_fingerprint
		run_url:              require_string_member(value, 'run_url')!
		job_url:              require_string_member(value, 'job_url')!
		human_action:         require_string_member(value, 'human_action')!
	}
	if artifact_url_value.kind == .string_value {
		diagnostic = DiagnosticRecord{
			...diagnostic
			artifact_url: artifact_url_value.string_value
		}
	}
	validate_diagnostic_record(diagnostic)!
	return diagnostic
}

// classify_failure_routing keeps an unisolated V smoke failure explicitly ambiguous.
pub fn classify_failure_routing(failure_class string, baseline_green bool,
	candidate_causal bool, workflow_owner string) !FailureRoutingDecision {
	owner := match failure_class {
		'source_unreachable', 'runner-transient' {
			''
		}
		'patch-probe-failed', 'payload-review-required', 'manifest-invalid', 'native-build-failed',
		'required-lane-missing' {
			'vlang/tccbin'
		}
		'control-plane-failed' {
			'vlang/v'
		}
		'v-smoke-failed' {
			if baseline_green && candidate_causal {
				'vlang/tccbin'
			} else {
				'vlang/v'
			}
		}
		'ci-infrastructure-exhausted' {
			if workflow_owner == 'tccbin' {
				'vlang/tccbin'
			} else if workflow_owner == 'v' {
				'vlang/v'
			} else {
				return error('infrastructure owner must be causally isolated')
			}
		}
		'ownership-ambiguous' {
			'vlang/v'
		}
		else {
			return error('failure class is outside diagnostic.schema.json')
		}
	}
	normalized_class := if failure_class == 'v-smoke-failed' && !(baseline_green
		&& candidate_causal) {
		'ownership-ambiguous'
	} else {
		failure_class
	}
	return FailureRoutingDecision{
		owner_repository: owner
		failure_class:    normalized_class
	}
}

// route_failure_owner returns the one repository that owns an actionable schema failure class.
pub fn route_failure_owner(failure_class string, baseline_green bool,
	candidate_causal bool, workflow_owner string) !string {
	return classify_failure_routing(failure_class, baseline_green, candidate_causal, workflow_owner)!.owner_repository
}

// issue_marker_hash derives the stable hidden marker from exactly owner repository and OS.
pub fn issue_marker_hash(owner_repository string, os_family string) string {
	return sha256.sum256('${owner_repository}\x1f${os_family}'.bytes()).hex()
}

// project_issue applies the exact audit key and retains all distinct tests/lanes inside each row.
pub fn project_issue(owner_repository string, os_family string,
	incidents []IncidentProjectionInput) !IssueProjectionModel {
	if owner_repository !in ['vlang/v', 'vlang/tccbin'] || os_family !in issue_os_allowlist {
		return error('issue owner repository or OS is not allowlisted')
	}
	mut entries := []IssueProjectionEntry{}
	mut secondary_keys := []string{}
	for incident in incidents {
		validate_diagnostic_record(incident.diagnostic)!
		if incident.diagnostic.repository != owner_repository || incident.diagnostic.os != os_family {
			return error('diagnostic crossed its owner repository or OS issue boundary')
		}
		if incident.status !in ['active', 'validating', 'waiting_for_source', 'resolved_bot',
			'waived'] {
			return error('incident projection status is outside the ledger vocabulary')
		}
		diagnostic := DiagnosticRecord{
			...incident.diagnostic
			observed_summary: sanitize_issue_summary(incident.diagnostic.observed_summary)
		}
		abi := target_abi(diagnostic.target_id)!
		key := [abi, diagnostic.target_id, diagnostic.architecture, diagnostic.component,
			diagnostic.failure_class].join('\x1f')
		if key in secondary_keys {
			index := secondary_keys.index(key)
			entry := entries[index]
			entries[index] = IssueProjectionEntry{
				...entry
				status:      merge_incident_status(entry.status, incident.status)
				diagnostics: merge_diagnostic_lane(entry.diagnostics, diagnostic)
			}
		} else {
			secondary_keys << key
			entries << IssueProjectionEntry{
				abi:           abi
				target_id:     diagnostic.target_id
				architecture:  diagnostic.architecture
				component:     diagnostic.component
				failure_class: diagnostic.failure_class
				status:        incident.status
				diagnostics:   [diagnostic]
			}
		}
	}
	entries.sort_with_compare(compare_issue_entries)
	if issue_projection_size(entries) > issue_bot_zone_max_bytes {
		return error('machine-managed issue projection exceeds its strict byte bound')
	}
	active := entries.any(it.status in ['active', 'validating', 'waiting_for_source', 'waived'])
	title_os := os_family[..1].to_upper() + os_family[1..]
	return IssueProjectionModel{
		schema_version:   1
		owner_repository: owner_repository
		os:               os_family
		marker_hash:      issue_marker_hash(owner_repository, os_family)
		title:            '[TCC bundles/${title_os}] Human review required'
		entries:          entries
		should_be_open:   active
	}
}

// validate_diagnostic_record keeps the V type vocabulary identical to diagnostic.schema.json.
pub fn validate_diagnostic_record(diagnostic DiagnosticRecord) ! {
	artifact_url := diagnostic.artifact_url or { '' }
	if diagnostic.schema_version != 1
		|| diagnostic.repository !in ['vlang/v', 'vlang/tccbin']
		|| diagnostic.os !in issue_os_allowlist
		|| diagnostic.target_id !in managed_target_ids
		|| diagnostic.architecture !in ['amd64', 'arm64', 'x64', 'i386']
		|| diagnostic.failure_class !in ['patch-probe-failed', 'payload-review-required', 'manifest-invalid', 'native-build-failed', 'v-smoke-failed', 'required-lane-missing', 'ci-infrastructure-exhausted', 'control-plane-failed', 'ownership-ambiguous']
		|| diagnostic.component.runes().len < 1
		|| diagnostic.component.runes().len > 128
		|| diagnostic.test_id.runes().len < 1 || diagnostic.test_id.runes().len > 128
		|| diagnostic.lane.runes().len < 1 || diagnostic.lane.runes().len > 128
		|| diagnostic.expected.runes().len < 1
		|| diagnostic.expected.runes().len > 1024
		|| diagnostic.observed_summary.runes().len < 1
		|| diagnostic.observed_summary.runes().len > 4096
		|| !is_lower_hex_40(diagnostic.subject_sha)
		|| !is_lower_hex_64(diagnostic.input_fingerprint)
		|| (diagnostic.artifact_fingerprint != ''
		&& !is_lower_hex_64(diagnostic.artifact_fingerprint))
		|| !github_url_is_safe(diagnostic.run_url)
		|| !github_url_is_safe(diagnostic.job_url)
		|| (artifact_url != '' && !github_url_is_safe(artifact_url))
		|| diagnostic.human_action !in ['keep', 'rebase', 'split', 'retire', 'fix-recipe', 'review-change'] {
		return error('diagnostic record is outside the exact schema vocabulary or bounds')
	}
	if target_os(diagnostic.target_id)! != diagnostic.os {
		return error('diagnostic OS does not match its authoritative target tuple')
	}
	if diagnostic_record_size(diagnostic) > diagnostic_max_bytes {
		return error('diagnostic record exceeds its strict transport byte bound')
	}
}

// diagnostic_payload_size_is_valid applies the strict pre-schema transport bound.
pub fn diagnostic_payload_size_is_valid(byte_count int) bool {
	return byte_count >= 0 && byte_count <= diagnostic_max_bytes
}

// issue_bot_zone_size_is_valid prevents an unbounded machine-managed issue projection.
pub fn issue_bot_zone_size_is_valid(byte_count int) bool {
	return byte_count >= 0 && byte_count <= issue_bot_zone_max_bytes
}

// sanitize_issue_summary removes credential/path/control data and preserves UTF-8 byte bounds.
pub fn sanitize_issue_summary(summary string) string {
	mut lines := strip_terminal_controls(summary).split_into_lines()
	if lines.len > issue_summary_max_lines {
		lines = lines[lines.len - issue_summary_max_lines..].clone()
	}
	mut sanitized := []string{cap: lines.len}
	for raw_line in lines {
		mut line := raw_line
		lower := line.to_lower()
		if lower.contains('authorization:') || lower.contains('proxy-authorization:')
			|| lower.contains('cookie:') || lower.contains('set-cookie:')
			|| lower.contains('token=') || lower.contains('token:') || lower.contains('api_key')
			|| lower.contains('api-key') || lower.contains('private_key')
			|| lower.contains('private key') || lower.contains('client_secret')
			|| lower.contains('github_pat_') || lower.contains('ghp_') || lower.contains('ghs_') {
			line = '<redacted credential line>'
		} else {
			line = redact_absolute_paths(line)
			line = redact_authenticated_urls(line)
		}
		line = truncate_utf8_bytes(line, 512)
		sanitized << line
	}
	return truncate_utf8_bytes(sanitized.join('\n'), issue_summary_max_bytes)
}

fn merge_diagnostic_lane(existing []DiagnosticRecord, diagnostic DiagnosticRecord) []DiagnosticRecord {
	mut merged := existing.clone()
	key := '${diagnostic.test_id}\x1f${diagnostic.lane}'
	mut replaced := false
	for index, current in merged {
		if '${current.test_id}\x1f${current.lane}' == key {
			merged[index] = diagnostic
			replaced = true
			break
		}
	}
	if !replaced {
		merged << diagnostic
	}
	merged.sort_with_compare(compare_diagnostics)
	return merged
}

fn merge_incident_status(left string, right string) string {
	priority := ['resolved_bot', 'validating', 'waiting_for_source', 'waived', 'active']
	return if priority.index(right) > priority.index(left) { right } else { left }
}

fn target_abi(target_id string) !string {
	return match target_id {
		'linux-amd64' { 'glibc' }
		'windows-amd64' { 'ucrt-pe' }
		'macos-amd64', 'macos-arm64' { 'darwin' }
		'freebsd-amd64', 'openbsd-amd64' { 'elf' }
		else { error('unknown target ABI') }
	}
}

fn target_os(target_id string) !string {
	return match target_id {
		'linux-amd64' { 'linux' }
		'windows-amd64' { 'windows' }
		'macos-amd64', 'macos-arm64' { 'macos' }
		'freebsd-amd64' { 'freebsd' }
		'openbsd-amd64' { 'openbsd' }
		else { error('unknown target OS') }
	}
}

fn github_url_is_safe(value string) bool {
	prefix := 'https://github.com/'
	if !value.starts_with(prefix) || value.len <= prefix.len || value.len > 2048
		|| value.contains('@') || value.contains('?') || value.contains('#') || value.contains('\\') {
		return false
	}
	for byte in value[prefix.len..].bytes() {
		if !(byte >= `0` && byte <= `9`) && !(byte >= `a` && byte <= `z`) && !(byte >= `A`
			&& byte <= `Z`) && byte !in [`-`, `_`, `.`, `/`] {
			return false
		}
	}
	return true
}

fn redact_absolute_paths(value string) string {
	mut words := value.split(' ')
	for index, word in words {
		trimmed := word.trim('"\'`()[]{}<>,;')
		if token_contains_absolute_path(trimmed) {
			words[index] = '<local-path>'
		}
	}
	return words.join(' ')
}

fn token_contains_absolute_path(value string) bool {
	lower := value.to_lower()
	if lower.starts_with('file://') {
		return true
	}
	if lower.starts_with('https://') || lower.starts_with('http://') {
		return false
	}
	bytes := value.bytes()
	for index, byte in bytes {
		if byte == `/` && (index == 0 || bytes[index - 1] in [`=`, `:`, `(`, `[`, `{`, `"`, `'`]) {
			return true
		}
		if index + 2 < bytes.len && ((byte >= `a` && byte <= `z`)
			|| (byte >= `A` && byte <= `Z`)) && bytes[index + 1] == `:`
			&& bytes[index + 2] in [`\\`, `/`] {
			return true
		}
	}
	return false
}

fn redact_authenticated_urls(value string) string {
	mut words := value.split(' ')
	for index, word in words {
		trimmed := word.trim('"\'`()[]{}<>,;')
		if (trimmed.starts_with('https://') || trimmed.starts_with('http://'))
			&& trimmed.contains('@') {
			words[index] = '<authenticated-url>'
		}
	}
	return words.join(' ')
}

fn diagnostic_record_size(diagnostic DiagnosticRecord) int {
	artifact_url := diagnostic.artifact_url or { '' }
	return 256 + diagnostic.repository.len + diagnostic.os.len + diagnostic.target_id.len +
		diagnostic.architecture.len + diagnostic.component.len + diagnostic.failure_class.len +
		diagnostic.test_id.len + diagnostic.lane.len + diagnostic.expected.len +
		diagnostic.observed_summary.len + diagnostic.subject_sha.len +
		diagnostic.input_fingerprint.len + diagnostic.artifact_fingerprint.len +
		diagnostic.run_url.len + diagnostic.job_url.len + artifact_url.len +
		diagnostic.human_action.len
}

fn issue_projection_size(entries []IssueProjectionEntry) int {
	mut total := 512
	for entry in entries {
		total += 192 + entry.abi.len + entry.target_id.len + entry.architecture.len +
			entry.component.len + entry.failure_class.len + entry.status.len
		for diagnostic in entry.diagnostics {
			total += diagnostic_record_size(diagnostic)
		}
	}
	return total
}

fn strip_terminal_controls(value string) string {
	mut result := []u8{cap: value.len}
	mut index := 0
	bytes := value.bytes()
	for index < bytes.len {
		if bytes[index] == 0x1b {
			index++
			if index < bytes.len && bytes[index] == `[` {
				index++
				for index < bytes.len && !(bytes[index] >= 0x40 && bytes[index] <= 0x7e) {
					index++
				}
				if index < bytes.len {
					index++
				}
			}
			continue
		}
		if bytes[index] == 0 || (bytes[index] < 0x20 && bytes[index] !in [`\n`, `\r`, `\t`]) {
			index++
			continue
		}
		result << bytes[index]
		index++
	}
	return result.bytestr()
}

fn truncate_utf8_bytes(value string, limit int) string {
	if value.len <= limit {
		return value
	}
	mut result := ''
	for rune_value in value.runes() {
		candidate := result + rune_value.str()
		if candidate.len > limit {
			break
		}
		result = candidate
	}
	return result
}

fn compare_diagnostics(left &DiagnosticRecord, right &DiagnosticRecord) int {
	left_key := '${left.test_id}\x1f${left.lane}'
	right_key := '${right.test_id}\x1f${right.lane}'
	return compare_strings(left_key, right_key)
}

fn compare_issue_entries(left &IssueProjectionEntry, right &IssueProjectionEntry) int {
	left_key :=
		[left.abi, left.target_id, left.architecture, left.component, left.failure_class].join('\x1f')
	right_key :=
		[right.abi, right.target_id, right.architecture, right.component, right.failure_class].join('\x1f')
	return compare_strings(left_key, right_key)
}

fn compare_strings(left string, right string) int {
	if left < right {
		return -1
	}
	if left > right {
		return 1
	}
	return 0
}
