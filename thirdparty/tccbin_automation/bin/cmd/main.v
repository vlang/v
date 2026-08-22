module main

import os
import tccbin_automation.bin

const tccbin_contract_repository = $d('tccbin_contract_repository', '')

const tccbin_contract_sha = $d('tccbin_contract_sha', '')

fn main() {
	if os.args.len < 2 {
		eprintln('usage: tccbin-automation <contract|contract-binding|dry-run|staged-preflight|candidate-preflight|candidate-compose|issue-dry-run|live-state|receiver|handoff|workflow-run|validate|canonicalize|fingerprint>')
		exit(2)
	}
	automation_root := os.real_path(os.join_path(os.getwd(), 'thirdparty', 'tccbin_automation'))
	match os.args[1] {
		'contract' {
			report := bin.run_contract_checks(automation_root) or {
				eprintln(err.msg())
				exit(1)
			}
			println('schemas=${report.schema_count} manifests=${report.manifest_count} hygiene_files=${report.hygiene_files}')
		}
		'contract-binding' {
			if os.args.len != 2 {
				eprintln('usage: tccbin-automation contract-binding')
				exit(2)
			}
			binding := bin.attest_runtime_contract_binding(bin.RuntimeContractBinding{
				repository: tccbin_contract_repository
				sha:        tccbin_contract_sha
			}) or {
				eprintln(err.msg())
				exit(1)
			}
			println('repository=${binding.repository} sha=${binding.sha}')
		}
		'dry-run' {
			bin.run_dark_mode_dry_run(automation_root) or {
				eprintln(err.msg())
				exit(1)
			}
			println('dark_mode=passed writes=0')
		}
		'staged-preflight' {
			if os.args.len != 7 || os.args[6] !in ['true', 'false'] {
				eprintln('usage: tccbin-automation staged-preflight <manifest> <staging-root> <source-git-root> <source-git-sha> <publish-requested>')
				exit(2)
			}
			decision := bin.evaluate_staged_manifest_for_execution(automation_root, os.args[2], bin.StagingContract{
				staging_root:    os.args[3]
				source_git_root: os.args[4]
				source_git_ref:  os.args[5]
			}, bin.RuntimeContractBinding{
				repository: tccbin_contract_repository
				sha:        tccbin_contract_sha
			}, os.args[6] == 'true') or {
				eprintln(err.msg())
				exit(1)
			}
			println('eligible=${decision.eligible} reason=${decision.reason} publish_allowed=${decision.publish_allowed} manifest_hash=${decision.manifest_hash} input_fingerprint=${decision.input_fingerprint} artifact_fingerprint=${decision.artifact_fingerprint}')
		}
		'candidate-preflight' {
			if os.args.len != 9 || os.args[8] !in ['true', 'false'] {
				eprintln('usage: tccbin-automation candidate-preflight <target-id> <monthly|legacy-onboard|baseline-activate> <candidate-repo-root> <base-sha> <candidate-sha> <work-root> <publish-requested>')
				exit(2)
			}
			kind := bin.parse_candidate_transition_kind(os.args[3]) or {
				eprintln(err.msg())
				exit(2)
			}
			decision := bin.evaluate_candidate_manifest_for_execution(automation_root, os.args[2],
				kind, os.args[4], os.args[5], os.args[6], os.args[7], bin.RuntimeContractBinding{
				repository: tccbin_contract_repository
				sha:        tccbin_contract_sha
			}, os.args[8] == 'true') or {
				eprintln(err.msg())
				exit(1)
			}
			println('eligible=${decision.eligible} reason=${decision.reason} publish_allowed=${decision.publish_allowed} manifest_hash=${decision.manifest_hash} input_fingerprint=${decision.input_fingerprint} artifact_fingerprint=${decision.artifact_fingerprint}')
		}
		'candidate-compose' {
			if os.args.len != 9 {
				eprintln('usage: tccbin-automation candidate-compose <target-id> <monthly|legacy-onboard|baseline-activate> <base-repo-root> <base-sha> <raw-root> <manifest> <result-root>')
				exit(2)
			}
			kind := bin.parse_candidate_transition_kind(os.args[3]) or {
				eprintln(err.msg())
				exit(2)
			}
			bin.compose_candidate_for_execution(automation_root, bin.CandidateCompositionRequest{
				target_id:      os.args[2]
				kind:           kind
				base_repo_root: os.args[4]
				base_sha:       os.args[5]
				raw_root:       os.args[6]
				manifest_path:  os.args[7]
				result_root:    os.args[8]
			}, bin.RuntimeContractBinding{
				repository: tccbin_contract_repository
				sha:        tccbin_contract_sha
			}) or {
				eprintln(err.msg())
				exit(1)
			}
			// Exit status plus the atomically exposed repository are the only CLI result.
		}
		'issue-dry-run' {
			if os.args.len != 3 {
				eprintln('usage: tccbin-automation issue-dry-run <ledger>')
				exit(2)
			}
			projection := bin.project_issue_ledger(os.read_file(os.args[2]) or {
				eprintln(err.msg())
				exit(1)
			}) or {
				eprintln(err.msg())
				exit(1)
			}
			println('resolved=true owner=${projection.owner_repository} os=${projection.os} entries=${projection.entries.len} open=${projection.should_be_open} writes=0')
		}
		'live-state' {
			run_live_state_command(automation_root)
		}
		'receiver' {
			if os.args.len != 16 {
				eprintln('usage: tccbin-automation receiver <ledger> <opaque-id> <repository> <workflow-id> <workflow-path> <workflow-ref> <event> <run-id> <run-attempt> <head-sha-or-none> <run-name-or-none> <observed-canonical-head> <observed-subject-ref-head> <publish-requested>')
				exit(2)
			}
			if os.args[15] !in ['true', 'false'] {
				eprintln('publish-requested must be true or false')
				exit(2)
			}
			resolution := bin.resolve_receiver_request(os.read_file(os.args[2]) or {
				eprintln(err.msg())
				exit(1)
			}, bin.ReceiverRequestFacts{
				opaque_id:                 os.args[3]
				repository:                os.args[4]
				workflow_id:               os.args[5].i64()
				workflow_path:             os.args[6]
				workflow_ref:              os.args[7]
				event:                     os.args[8]
				current_run_id:            os.args[9].i64()
				current_run_attempt:       os.args[10].int()
				current_head_sha:          if os.args[11] == 'none' { '' } else { os.args[11] }
				current_run_name:          if os.args[12] == 'none' { '' } else { os.args[12] }
				observed_canonical_head:   os.args[13]
				observed_subject_ref_head: os.args[14]
				requested_publish:         os.args[15] == 'true'
			}) or {
				eprintln(err.msg())
				exit(1)
			}
			println('resolved=true target=${resolution.target_id} capability=${resolution.resume_capability} execute=${resolution.allowed_to_execute} publish=false')
		}
		'workflow-run' {
			if os.args.len != 4 {
				eprintln('usage: tccbin-automation workflow-run <ledger> <event>')
				exit(2)
			}
			lookup := bin.lookup_receiver_completion(os.read_file(os.args[2]) or {
				eprintln(err.msg())
				exit(1)
			}, os.read_file(os.args[3]) or {
				eprintln(err.msg())
				exit(1)
			}) or {
				eprintln(err.msg())
				exit(1)
			}
			if !lookup.active {
				println('resolved=false active=false writes=0')
				return
			}
			resolution := lookup.completion
			println('resolved=true target=${resolution.target_id} capability=${resolution.resume_capability} successor=${resolution.may_create_successor} writes=0')
		}
		'handoff' {
			if os.args.len != 4 {
				eprintln('usage: tccbin-automation handoff <ledger> <opaque-id>')
				exit(2)
			}
			entry := bin.resolve_active_recovery_id(os.read_file(os.args[2]) or {
				eprintln(err.msg())
				exit(1)
			}, os.args[3]) or {
				eprintln(err.msg())
				exit(1)
			}
			println('resolved=true target=${entry.target_id} capability=${entry.resume_capability} writes=0')
		}
		'validate' {
			if os.args.len != 4 {
				eprintln('usage: tccbin-automation validate <schema> <document>')
				exit(2)
			}
			issues := bin.validate_json_file(os.args[2], os.args[3]) or {
				eprintln(err.msg())
				exit(1)
			}
			if issues.len > 0 {
				for issue in issues {
					eprintln('${issue.path}: ${issue.message}')
				}
				exit(1)
			}
			println('valid')
		}
		'canonicalize' {
			if os.args.len != 3 {
				eprintln('usage: tccbin-automation canonicalize <document>')
				exit(2)
			}
			value := bin.parse_strict_json(os.read_file(os.args[2]) or {
				eprintln(err.msg())
				exit(1)
			}) or {
				eprintln(err.msg())
				exit(1)
			}
			println(bin.canonical_json(value))
		}
		'fingerprint' {
			if os.args.len != 3 {
				eprintln('usage: tccbin-automation fingerprint <manifest>')
				exit(2)
			}
			registry := bin.parse_strict_json(os.read_file(os.join_path(automation_root,
				'targets.json')) or {
				eprintln(err.msg())
				exit(1)
			}) or {
				eprintln(err.msg())
				exit(1)
			}
			fingerprints := bin.manifest_fingerprints(os.read_file(os.args[2]) or {
				eprintln(err.msg())
				exit(1)
			}, registry) or {
				eprintln(err.msg())
				exit(1)
			}
			println('manifest_hash=${fingerprints.manifest_hash}')
			println('input_fingerprint=${fingerprints.input_fingerprint}')
			println('artifact_fingerprint=${fingerprints.artifact_fingerprint}')
		}
		else {
			eprintln('unknown command')
			exit(2)
		}
	}
}

fn run_live_state_command(automation_root string) {
	if os.args.len < 3 || os.args[2] !in ['inspect', 'native', 'receiver'] {
		eprintln('usage: tccbin-automation live-state <inspect|native|receiver> ...')
		exit(2)
	}
	match os.args[2] {
		'inspect' {
			if os.args.len != 6 {
				eprintln('usage: tccbin-automation live-state inspect <state-git-dir-or-none> <proof-bundle-dir-or-none> <opaque-id>')
				exit(2)
			}
			state_git_dir := if os.args[3] == 'none' { '' } else { os.args[3] }
			trust := if state_git_dir == '' {
				bin.LiveStateTrust{}
			} else {
				live_state_trust_from_environment() or {
					eprintln(err.msg())
					exit(1)
				}
			}
			proof_bundle_dir := if os.args[4] == 'none' {
				''
			} else {
				trusted_live_state_proof_bundle_path(os.args[4]) or {
					eprintln(err.msg())
					exit(1)
				}
			}
			inspection := bin.inspect_live_receiver_state(automation_root, state_git_dir, trust,
				proof_bundle_dir, os.args[5]) or {
				eprintln(err.msg())
				exit(1)
			}
			println('status=${inspection.status} state_commit=${inspection.state_commit} target=${inspection.target.target_id} canonical_ref=${inspection.canonical_ref} subject_ref=${inspection.subject_ref} publish_allowed=false')
		}
		'native' {
			if os.args.len != 6 {
				eprintln('usage: tccbin-automation live-state native <state-git-dir-or-none> <proof-bundle-dir-or-none> <opaque-id>')
				exit(2)
			}
			state_git_dir := if os.args[3] == 'none' { '' } else { os.args[3] }
			trust := if state_git_dir == '' {
				bin.LiveStateTrust{}
			} else {
				live_state_trust_from_environment() or {
					eprintln(err.msg())
					exit(1)
				}
			}
			proof_bundle_dir := if os.args[4] == 'none' {
				''
			} else {
				trusted_live_state_proof_bundle_path(os.args[4]) or {
					eprintln(err.msg())
					exit(1)
				}
			}
			decision := bin.resolve_live_native_gate_action(automation_root, state_git_dir, trust,
				proof_bundle_dir, os.args[5]) or {
				eprintln(err.msg())
				exit(1)
			}
			println('action=${decision.action} state_commit=${decision.state_commit} target=${decision.target_id} consumer=${decision.consumer_id} kind=${decision.consumer_kind} subject_hash=${decision.subject_hash} subject_sha=${decision.subject_sha} expected_ref=${decision.expected_ref} generation=${decision.expected_ledger_generation} epoch=${decision.active_gate_epoch} trigger=${decision.trigger_id} create_only=${decision.create_only} publish_allowed=false')
		}
		'receiver' {
			if os.args.len != 14 {
				eprintln('usage: tccbin-automation live-state receiver <state-git-dir> <proof-bundle-dir> <opaque-id> <workflow-id> <workflow-path> <run-id> <run-attempt> <head-sha-or-none> <run-name-or-none> <observed-canonical-head> <observed-subject-ref-head>')
				exit(2)
			}
			if os.args[3] == 'none' || os.args[4] == 'none' {
				eprintln('live state receiver requires an authenticated present state ref')
				exit(2)
			}
			trust := live_state_trust_from_environment() or {
				eprintln(err.msg())
				exit(1)
			}
			proof_bundle_dir := trusted_live_state_proof_bundle_path(os.args[4]) or {
				eprintln(err.msg())
				exit(1)
			}
			decision := bin.resolve_live_receiver_request(automation_root, os.args[3], trust,
				proof_bundle_dir, bin.ReceiverRequestFacts{
				opaque_id:                 os.args[5]
				repository:                trust.repository
				workflow_id:               os.args[6].i64()
				workflow_path:             os.args[7]
				workflow_ref:              'master'
				event:                     'workflow_dispatch'
				current_run_id:            os.args[8].i64()
				current_run_attempt:       os.args[9].int()
				current_head_sha:          if os.args[10] == 'none' { '' } else { os.args[10] }
				current_run_name:          if os.args[11] == 'none' { '' } else { os.args[11] }
				observed_canonical_head:   os.args[12]
				observed_subject_ref_head: os.args[13]
				requested_publish:         false
			}) or {
				eprintln(err.msg())
				exit(1)
			}
			println('status=${decision.status} target=${decision.resolution.target_id} capability=${decision.resolution.resume_capability} execute=${decision.resolution.allowed_to_execute} publish_allowed=false')
		}
		else {}
	}
}

fn live_state_trust_from_environment() !bin.LiveStateTrust {
	repository := required_environment('GITHUB_REPOSITORY')!
	app_id := required_environment('TCCBIN_STATE_APP_ID')!
	actor_login := required_environment('TCCBIN_STATE_ACTOR_LOGIN')!
	actor_node_id := required_environment('TCCBIN_STATE_ACTOR_NODE_ID')!
	actor_database_id := required_environment('TCCBIN_STATE_ACTOR_DATABASE_ID')!
	if repository !in ['vlang/v', 'GGRei/v'] || app_id.i64() <= 0 || actor_database_id.i64() <= 0 {
		return error('live state trust environment is outside its closed allowlist')
	}
	return bin.LiveStateTrust{
		repository:          repository
		state_writer_app_id: app_id.i64()
		actor_login:         actor_login
		actor_node_id:       actor_node_id
		actor_database_id:   actor_database_id.i64()
	}
}

fn required_environment(name string) !string {
	value := os.getenv(name)
	if value == '' || value.contains('\n') || value.contains('\r') {
		return error('required trusted environment ${name} is absent or malformed')
	}
	return value
}

fn trusted_live_state_proof_bundle_path(path string) !string {
	real_path := os.real_path(path)
	if !os.is_abs_path(path) || real_path != path || !os.is_dir(path) || os.is_link(path) {
		return error('live state proof bundle must be an exact absolute non-symlink directory')
	}
	return path
}
