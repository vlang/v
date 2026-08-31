module tests

import os
import tccbin_automation.bin

fn workflow_source(name string) string {
	vroot := os.real_path(os.join_path(automation_root(), '..', '..'))
	return os.read_file(os.join_path(vroot, '.github', 'workflows', name)) or { panic(err) }
}

fn workflow_receiver_command(source string) string {
	mut command := ''
	mut collecting := false
	for raw_line in source.split_into_lines() {
		mut line := raw_line.trim_space()
		if !collecting {
			if !line.contains('thirdparty/tccbin_automation/bin/cmd receiver') {
				continue
			}
			collecting = true
		}
		continued := line.ends_with(' \\')
		if continued {
			line = line[..line.len - 2]
		}
		command += if command == '' { line } else { ' ${line}' }
		if !continued {
			return command
		}
	}
	panic('continued receiver command missing')
}

fn workflow_is_lower_hex_40(value string) bool {
	if value.len != 40 {
		return false
	}
	for byte in value.bytes() {
		if !((byte >= `0` && byte <= `9`) || (byte >= `a` && byte <= `f`)) {
			return false
		}
	}
	return true
}

fn workflow_index_after(source string, needle string, offset int) int {
	if offset < 0 || offset > source.len {
		panic('workflow search offset is invalid')
	}
	relative := source[offset..].index(needle) or { panic('ordered workflow marker missing') }
	return offset + relative
}

fn test_pr_contract_workflow_always_exposes_both_required_checks() {
	source := workflow_source('tccbin_automation_contract.yml')
	vc_lock := bin.validate_vc_bootstrap_contract(automation_root()) or { panic(err) }
	tcc_lock := 'ece46f06fbe6eb701d52442f11dd59c48d166cae'
	assert source.contains('  pull_request:')
	assert source.contains('  merge_group:')
	assert !source.contains('paths:')
	assert !source.contains('paths-ignore:')
	assert source.contains('name: tccbin-automation-contract')
	assert source.contains('name: tccbin-automation-dry-run')
	assert source.count('if: always()') == 2
	contract_worker_index := source.index('  contract-worker:') or {
		panic('contract worker missing')
	}
	contract_check_index := workflow_index_after(source, '\n  contract:\n', contract_worker_index)
	dry_run_worker_index := workflow_index_after(source, '\n  dry-run-worker:\n',
		contract_check_index)
	dry_run_check_index := workflow_index_after(source, '\n  dry-run:\n', dry_run_worker_index)
	assert source[contract_worker_index..contract_check_index].count('timeout-minutes: 45') == 1
	assert source[contract_check_index..dry_run_worker_index].count('timeout-minutes: 5') == 1
	assert source[dry_run_worker_index..dry_run_check_index].count('timeout-minutes: 30') == 1
	assert source[dry_run_check_index..].count('timeout-minutes: 5') == 1
	assert source.count('timeout-minutes: 45') == 1
	assert source.count('timeout-minutes: 30') == 1
	assert source.count('timeout-minutes: 5') == 2
	assert source.count('persist-credentials: false') == 4
	assert source.count('uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1') == 4
	assert source.count('repository: vlang/vc') == 1
	assert source.count('ref: ${vc_lock.commit}') == 1
	assert source.count('path: vc') == 1
	assert source.count('repository: vlang/tccbin') == 1
	assert source.count('ref: ${tcc_lock}') == 1
	assert source.count('path: thirdparty/tcc') == 1
	assert source.count('fetch-depth: 0') == 3
	assert source.count('filter: blob:none') == 2
	assert source.count('git -C vc config --local core.autocrlf false') == 1
	assert source.count('git -C thirdparty/tcc config --local core.autocrlf false') == 1
	assert source.count('make local=1') == 1
	assert !source.contains('make latest_tcc')
	tested_checkout := '      - name: Checkout the tested revision\n' +
		'        uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1\n' +
		'        with:\n' + '          fetch-depth: 0\n' + '          persist-credentials: false\n'
	assert source.count(tested_checkout) == 1
	vc_checkout_index := source.index('      - name: Checkout the immutable VC bootstrap snapshot') or {
		panic('immutable VC checkout missing')
	}
	tcc_checkout_index := workflow_index_after(source,
		'      - name: Checkout the immutable TCC bootstrap bundle', vc_checkout_index)
	tcc_verify_index := workflow_index_after(source,
		'      - name: Configure and verify the immutable TCC bootstrap bundle', tcc_checkout_index)
	build_index := workflow_index_after(source, '      - name: Build the local compiler',
		tcc_verify_index)
	assert vc_checkout_index < tcc_checkout_index
	assert tcc_checkout_index < tcc_verify_index
	assert tcc_verify_index < build_index
	vc_checkout := source[vc_checkout_index..tcc_checkout_index]
	assert vc_checkout.count('uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1') == 1
	assert vc_checkout.count('repository: vlang/vc') == 1
	assert vc_checkout.count('ref: ${vc_lock.commit}') == 1
	assert vc_checkout.count('path: vc') == 1
	assert vc_checkout.count('fetch-depth: 0') == 1
	assert vc_checkout.count('filter: blob:none') == 1
	assert vc_checkout.count('persist-credentials: false') == 1
	assert vc_checkout.count('git -C vc config --local core.autocrlf false') == 1
	tcc_checkout := source[tcc_checkout_index..tcc_verify_index]
	assert tcc_checkout.count('uses: actions/checkout@3d3c42e5aac5ba805825da76410c181273ba90b1') == 1
	assert tcc_checkout.count('repository: vlang/tccbin') == 1
	assert tcc_checkout.count('ref: ${tcc_lock}') == 1
	assert tcc_checkout.count('path: thirdparty/tcc') == 1
	assert tcc_checkout.count('fetch-depth: 0') == 1
	assert tcc_checkout.count('filter: blob:none') == 1
	assert tcc_checkout.count('persist-credentials: false') == 1
	tcc_verify := source[tcc_verify_index..build_index]
	assert tcc_verify.count('git -C thirdparty/tcc config --local core.autocrlf false') == 1
	assert tcc_verify.count('test "$(git -C thirdparty/tcc rev-parse HEAD)" = "${tcc_lock}"') == 1
	assert tcc_verify.count('tcc_symbolic_ref_rc=0') == 1
	assert tcc_verify.count('git -C thirdparty/tcc symbolic-ref --quiet HEAD >/dev/null || tcc_symbolic_ref_rc=$?') == 1
	assert tcc_verify.count('test "$tcc_symbolic_ref_rc" -eq 1') == 1
	assert tcc_verify.count('tcc_status="$(git -C thirdparty/tcc status --porcelain --untracked-files=all)"') == 1
	assert tcc_verify.count('test -z "$tcc_status"') == 1
	assert !tcc_verify.contains('symbolic-ref --quiet HEAD || true')
	assert !tcc_verify.contains('test -z "$(git -C thirdparty/tcc status')
	assert tcc_verify.count('test "$(git -C thirdparty/tcc config --local --get core.autocrlf)" = "false"') == 1
	assert tcc_verify.count('test -x thirdparty/tcc/tcc.exe') == 1
	assert tcc_verify.count('test -f thirdparty/tcc/lib/libgc.a') == 1
	assert tcc_verify.count('thirdparty/tcc/tcc.exe --version') == 1
	assert !source.contains(r'${{ secrets.')
	assert !source.contains('contents: write')
}

fn test_dark_mode_workflows_have_no_write_permissions_or_secrets() {
	for name in ['tccbin_revalidate.yml', 'tccbin_source_recovery.yml', 'tccbin_issue_reconcile.yml'] {
		source := workflow_source(name)
		assert source.contains('permissions:\n  contents: read')
		assert !source.contains('contents: write')
		assert !source.contains('issues: write')
		assert !source.contains('actions: write')
		assert !source.contains(r'${{ secrets.')
		assert !source.contains('workflow_call')
	}
}

fn test_issue_reconcile_rereads_and_projects_the_ledger_before_tests() {
	source := workflow_source('tccbin_issue_reconcile.yml')
	projection_command := 'thirdparty/tccbin_automation/bin/cmd issue-dry-run thirdparty/tccbin_automation/tests/fixtures/issue-ledger.dark.json'
	reporter_test := './vnew -silent test thirdparty/tccbin_automation/tests/reporter_hygiene_test.v'
	projection_index := source.index(projection_command) or { panic('issue projection missing') }
	test_index := source.index(reporter_test) or { panic('reporter test missing') }
	assert projection_index < test_index
	assert source.contains('persist-credentials: false')
	assert source.contains('Confirm that Phase A cannot write issues')
	assert !source.contains('TCCBIN_REPORTER_APP_PRIVATE_KEY')
	assert !source.contains('issues: write')
}

fn test_recovery_jobs_are_separated_and_publisher_is_absent() {
	source := workflow_source('tccbin_source_recovery.yml')
	assert source.contains('  resolver:')
	assert source.contains('  state-pre-dark-mode:')
	assert source.contains('  dispatch-dark-mode:')
	assert source.contains('  state-ack-dark-mode:')
	assert !source.contains('TCCBIN_PUBLISH_APP_PRIVATE_KEY')
	assert !source.contains('canonical-promote')
	assert source.contains('thirdparty/tccbin_automation/bin/cmd handoff')
	assert source.contains('thirdparty/tccbin_automation/bin/cmd workflow-run')
	assert source.contains('"$GITHUB_EVENT_PATH"')
	assert source.contains("if: needs.resolver.outputs.mode == 'workflow_run'")
	assert !source.contains('UPSTREAM_CONCLUSION')
	assert !source.contains('success|failure|cancelled|timed_out')
}

fn test_recovery_control_plane_uses_the_max_pending_queue() {
	queue_contract := 'concurrency:\n  group: tccbin-control-plane\n  cancel-in-progress: false\n  queue: max'
	for name in ['tccbin_source_recovery.yml', 'tccbin_revalidate.yml'] {
		source := workflow_source(name)
		assert source.count(queue_contract) == 1
	}
}

fn test_recovery_receivers_accept_only_their_opaque_identifier() {
	revalidate := workflow_source('tccbin_revalidate.yml')
	assert revalidate.contains('      resume_handoff_id:')
	assert !revalidate.contains('      native_gate_consumer_id:')
	update := workflow_source('update_tccbin.yml')
	assert update.contains('      native_gate_consumer_id:')
	assert update.contains('      resume_handoff_id:')
	assert update.contains('select exactly one persisted consumer or recovery handoff')
	assert revalidate.contains(r'run-name: tccbin-recovery-${{ inputs.resume_handoff_id')
	assert revalidate.contains('thirdparty/tccbin_automation/bin/cmd receiver')
	assert revalidate.contains('"$GITHUB_RUN_ID" "$GITHUB_RUN_ATTEMPT"')
	update_command := workflow_receiver_command(update)
	revalidate_command := workflow_receiver_command(revalidate)
	assert update_command.fields().len == 18
	assert revalidate_command.fields().len == 18
	assert update_command.fields()[15] == 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
	assert update_command.fields()[16] == 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
	assert revalidate_command.fields()[15] == 'cccccccccccccccccccccccccccccccccccccccc'
	assert revalidate_command.fields()[16] == 'cccccccccccccccccccccccccccccccccccccccc'
	assert update_command.fields()[17] == 'false'
	assert revalidate_command.fields()[17] == 'false'
}

fn test_update_receiver_is_explicitly_no_write() {
	source := workflow_source('update_tccbin.yml')
	assert source.contains('native_gate_consumer_id:')
	assert source.contains('resume_handoff_id:')
	assert source.contains('phase_a_dark_mode=true')
	assert source.contains('Validate the persisted-consumer receiver without any write capability')
	assert source.contains('thirdparty/tccbin_automation/bin/cmd receiver')
	assert source.contains('receiver-ledger.dark.json')
	assert source.contains('master workflow_dispatch 0 0 none none \\')
	assert source.contains('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa false')
	assert !source.contains('TCCBIN_STATE_WRITER_APP_PRIVATE_KEY')
	assert !source.contains('TCCBIN_PUBLISH_APP_PRIVATE_KEY')
}

fn test_update_authenticates_real_staging_before_every_upload_or_publication() {
	source := workflow_source('update_tccbin.yml')
	checkout_pin := '3d3c42e5aac5ba805825da76410c181273ba90b1'
	upload_pin := '043fb46d1a93c77aae656e7c1c64a875d1fc6a0a'
	vc_lock := 'dfc458a13ba8923ebc249e262c331f8169aa728b'
	cpa_pin := '24ef01df165c76df1ed2b9f9e9212e78dc2fc963'
	mut action_calls := 0
	for raw_line in source.split_into_lines() {
		mut line := raw_line.trim_space()
		if line.starts_with('- ') {
			line = line[2..]
		}
		if !line.starts_with('uses: ') {
			continue
		}
		action_calls++
		parts := line['uses: '.len..].split('@')
		assert parts.len == 2
		assert workflow_is_lower_hex_40(parts[1])
	}
	assert action_calls == 8
	assert source.count('uses: actions/checkout@${checkout_pin}') == 5
	assert source.count('repository: vlang/vc') == 2
	assert source.count('ref: ${vc_lock}') == 2
	assert source.count(r'ref: ${{ github.sha }}') == 2
	assert source.count('fetch-depth: 0') == 4
	assert source.count('uses: actions/upload-artifact@${upload_pin}') == 2
	assert source.count('uses: cross-platform-actions/action@${cpa_pin}') == 1
	assert !source.contains('actions/checkout@v7')
	assert !source.contains('actions/upload-artifact@v7')
	assert !source.contains('cross-platform-actions/action@v1.4.0')
	assert !source.contains('dfc4586f0ec60ef020927b24d97380403f74e42b')
	assert source.count('"$TCCBIN_VALIDATOR_CLI" candidate-preflight') == 2
	assert !source.contains('"$TCCBIN_VALIDATOR_CLI" staged-preflight')
	assert source.count('Bootstrap the contract-bound Phase A validator before') == 2
	assert source.count('thirdparty/tccbin_automation/bootstrap/bootstrap.sh') == 2
	assert source.count('contract-binding') == 2
	assert source.count('TCCBIN_VALIDATOR_CLI=') == 2
	assert source.count('TCCBIN_VALIDATOR_CONTRACT_ROOT=') == 2
	assert source.count('cd "$TCCBIN_VALIDATOR_CONTRACT_ROOT"') == 2
	assert source.count('git -c protocol.file.allow=always clone --quiet --no-checkout --no-local') == 2
	assert source.count('--no-hardlinks "$GITHUB_WORKSPACE" "$contract_input"') == 2
	assert source.count('git -C "$contract_input" remote set-url origin \\') == 2
	assert source.count('git -C "$contract_input" checkout --quiet --detach --force "$GITHUB_SHA"') == 2
	assert source.count('"$contract_input" "$GITHUB_REPOSITORY" "$GITHUB_SHA" \\') == 2
	assert source.count('"$vc_root" "$validator_work"') == 2
	assert source.count('binding="$("$validator_cli" contract-binding)"') == 2
	assert source.count(r'"repository=${GITHUB_REPOSITORY} sha=${GITHUB_SHA}"') == 2
	assert source.count('candidate_root="$GITHUB_WORKSPACE/thirdparty/tcc"') == 2
	assert source.count('preflight_root="$RUNNER_TEMP/tccbin-candidate-preflight"') == 2
	assert source.count('monthly "$candidate_root"') == 2
	assert source.count('"$base_sha" "$candidate_sha" "$preflight_root" "$PUBLISH"') == 2
	assert source.count("result='eligible=false reason=phase_a_material_absent publish_allowed=false manifest_hash= input_fingerprint= artifact_fingerprint='") == 2
	assert source.count('manifest_entry="$(git -C "$candidate_root" ls-tree "$candidate_sha" -- "$manifest_path")"') == 2
	assert source.count('"100644 blob "*$\'\\t\'"$manifest_path"') == 2
	assert source.count('payload_root="$preflight_root/payload"') == 2
	assert source.count('echo "payload_root=$payload_root" >> "$GITHUB_OUTPUT"') == 2
	assert source.count('git -C thirdparty/tcc config --local core.autocrlf false') == 2
	assert !source.contains('--depth=1')
	assert source.count('publish_allowed=true') >= 4
	assert source.count("steps.phase_a.outputs.eligible == 'true'") >= 6
	hosted_bootstrap := source.index('Bootstrap the contract-bound Phase A validator before staging') or {
		panic('contract-bound validator bootstrap missing')
	}
	hosted_clone := source.index('name: Clone tccbin branch') or { panic('tccbin clone missing') }
	hosted_preflight := source.index('name: Authenticate the Phase A candidate with an independent payload export') or {
		panic('hosted candidate preflight missing')
	}
	hosted_upload := source.index('name: Upload tar archive artifact') or {
		panic('hosted upload missing')
	}
	hosted_push := source.index('name: Push tccbin branch') or { panic('hosted publisher missing') }
	assert hosted_bootstrap < hosted_clone
	assert hosted_clone < hosted_preflight
	assert hosted_preflight < hosted_upload
	assert hosted_preflight < hosted_push
	bsd_bootstrap := source.index('Bootstrap the contract-bound Phase A validator before BSD staging') or {
		panic('BSD contract-bound validator bootstrap missing')
	}
	bsd_cpa := source.index(r'name: Start ${{ matrix.id }} VM') or { panic('BSD CPA step missing') }
	bsd_build := source.index(r'name: Build ${{ matrix.id }} TCC bundle') or {
		panic('BSD build step missing')
	}
	bsd_preflight := source.index('name: Authenticate the BSD Phase A candidate with an independent payload export') or {
		panic('BSD candidate preflight missing')
	}
	bsd_push := workflow_index_after(source, 'name: Push tccbin branch', bsd_preflight)
	assert bsd_bootstrap < bsd_cpa
	assert bsd_cpa < bsd_build
	assert bsd_build < bsd_preflight
	assert bsd_preflight < bsd_push
	assert source.contains('candidate_root=/tmp/tccbin-fresh-checkout')
	assert source.contains('git -C /tmp/tccbin-fresh-checkout config --local core.autocrlf false')
	assert source.contains(r'path: ${{ steps.phase_a.outputs.payload_root }}')
	assert source.contains('tar -czf /tmp/tccbin-macos-amd64.tar.gz -C "$PAYLOAD_ROOT" .')
	assert !source.contains('"$candidate_root" "$candidate_root"')
	assert !source.contains('./vnew run thirdparty/tccbin_automation/bin/cmd staged-preflight')
	assert !source.contains('git archive HEAD')
	assert !source.contains('make -C "$validator_source"')
	assert !source.contains('-o "$RUNNER_TEMP/tccbin-automation"')
}

fn test_scheduled_publication_requires_each_target_unlock() {
	source := workflow_source('update_tccbin.yml')
	target_unlocks := {
		'linux-amd64':   'TCCBIN_LINUX_AMD64_PUBLISH_UNLOCKED'
		'macos-amd64':   'TCCBIN_MACOS_AMD64_PUBLISH_UNLOCKED'
		'macos-arm64':   'TCCBIN_MACOS_ARM64_PUBLISH_UNLOCKED'
		'freebsd-amd64': 'TCCBIN_FREEBSD_AMD64_PUBLISH_UNLOCKED'
		'openbsd-amd64': 'TCCBIN_OPENBSD_AMD64_PUBLISH_UNLOCKED'
	}
	for target_id, unlock_variable in target_unlocks {
		matrix_binding := '"id":"${target_id}","publish_unlock_variable":"${unlock_variable}"'
		assert source.count(matrix_binding) == 2
	}
	scheduled_target_guard := "(github.event_name != 'schedule' || vars[matrix.publish_unlock_variable] == 'true')"
	assert source.count(scheduled_target_guard) == 4
	assert source.count(r'TARGET_PUBLISH_UNLOCKED: ${{ vars[matrix.publish_unlock_variable] }}') == 2
	assert source.count(r'&& [ "$TARGET_PUBLISH_UNLOCKED" != true ]; then') == 2
	assert source.count(r'scheduled publication for ${{ matrix.id }} is locked') == 2
}
