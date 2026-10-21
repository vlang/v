// vtest build: !windows
module main

import os
import net.http
import crypto.hmac
import crypto.sha256

const test_sha = 'a'.repeat(40)

fn good_snapshot(commit string) (string, string) {
	body := '// Turned ON custom defines: no_backtrace,cross\n#define CUSTOM_DEFINE_cross\n' +
		'// filler\n'.repeat(snapshot_min_lines)
	v_c := commit_hash_line(commit) + body +
		'typedef i32 __v_int;\n#define __V_HEADERLESS_LIBC 0\nint main(void) { return 0; }\n'
	return v_c, commit_hash_line(commit) + body
}

fn test_check_snapshot_accepts_a_good_snapshot_and_names_each_defect() {
	v_c, v_win_c := good_snapshot(test_sha)
	check_snapshot(test_sha, v_c, v_win_c) or { panic(err) }
	cases := {
		'v.c does not start with':                      [
			v_c.replace_once(test_sha, 'b'.repeat(40)),
			v_win_c,
		]
		'v_win.c does not start with':                  [v_c, v_win_c.all_after('\n')]
		'v.c was not generated with `-cross`':          [
			v_c.replace_once('#define CUSTOM_DEFINE_cross', ''),
			v_win_c,
		]
		'v_win.c was not generated with `-cross`':      [
			v_c,
			v_win_c.replace_once('no_backtrace,cross', 'no_backtrace'),
		]
		'v_win.c contains the FastC backend':           [v_c, v_win_c + 'void fastc__gen(void);\n']
		'v.c has only':                                 [
			v_c.replace('// filler\n', ''),
			v_win_c,
		]
		'v.c does not define `int`':                    [
			v_c.replace_once('typedef i32 __v_int;', 'typedef i64 __v_int;'),
			v_win_c,
		]
		'v.c does not merge in the system-header libc': [
			v_c.replace_once('#define __V_HEADERLESS_LIBC 0', '#define __V_HEADERLESS_LIBC 1'),
			v_win_c,
		]
	}
	for expected, files in cases {
		mut message := ''
		check_snapshot(test_sha, files[0], files[1]) or { message = err.msg() }
		assert message.starts_with(expected), '${expected}: got `${message}`'
	}
}

fn test_lock_and_unrelated_paths_never_trigger_a_snapshot() {
	for path in [
		'thirdparty/tccbin_automation/bootstrap/vc.lock',
		'thirdparty/tccbin_automation/tests/workflow_contract_test.v',
		'.github/workflows/update_tccbin.yml',
		'.github/workflows/tccbin_automation_contract.yml',
		'.github/workflows/gen_vc_ci.yml',
		'README.md',
		'doc/docs.md',
		'vlib/v/slow_tests/inout/x.vv',
		'vlib/v/slow_tests/inout/x.out',
		'cmd/tools/gen_vc/pipeline.v',
		'cmd/tools/vdoc/vdoc.v',
	] {
		assert path_is_ignored(path), path
	}
	for path in [
		'cmd/v/v.v',
		'vlib/builtin/array.v',
		'vlib/v/gen/c/cleanc.v',
		'cmd/tools/builders/c_builder.v',
		'thirdparty/tccbin_automation/bootstrap/bootstrap.sh',
		'GNUmakefile',
	] {
		assert !path_is_ignored(path), path
	}
}

fn test_snapshot_messages_name_their_source_commit() {
	assert parse_snapshot_source('[v:master] ${test_sha} - cgen: x (#1)') == test_sha
	assert parse_snapshot_source('[v:master] ${test_sha} - ') == test_sha
	for message in ['[v:main] ${test_sha} - x', '[v:master] ${test_sha[..12]} - x',
		'[v:master] ${test_sha.to_upper()} - x', '[v:master] ${test_sha}', 'regenerate v.c', ''] {
		assert parse_snapshot_source(message) == '', message
	}
}

fn test_webhook_signature_follows_github() {
	// The test vector from GitHub's "Validating webhook deliveries" documentation.
	secret := "It's a Secret to Everybody"
	assert valid_signature(secret, 'Hello, World!',
		'sha256=757107ea0eb2509fc211221cce984b8a37570b6d7586c22c46f4379c8b043e17')
	assert !valid_signature(secret, 'Hello, World!',
		'sha256=857107ea0eb2509fc211221cce984b8a37570b6d7586c22c46f4379c8b043e17')
	assert !valid_signature(secret, 'Hello, World?',
		'sha256=757107ea0eb2509fc211221cce984b8a37570b6d7586c22c46f4379c8b043e17')
	assert !valid_signature(secret, 'Hello, World!',
		'sha1=757107ea0eb2509fc211221cce984b8a37570b6d7586c22c46f4379c8b043e17')
	assert !valid_signature('', 'Hello, World!', 'sha256=')
}

fn signed_request(secret string, event string, body string) http.Request {
	mut header := http.new_header()
	header.add_custom('X-GitHub-Event', event) or { panic(err) }
	signature := hmac.new(secret.bytes(), body.bytes(), sha256.sum, sha256.block_size).hex()
	header.add_custom('X-Hub-Signature-256', 'sha256=${signature}') or { panic(err) }
	return http.Request{
		method: .post
		url:    '/genhook'
		header: header
		data:   body
	}
}

fn test_webhook_queues_one_run_only_for_signed_pushes_to_master() {
	secret := 'webhook secret'
	mut service := &Service{
		config: Config{
			mode:           .dry_run
			repository:     'vlang/v'
			webhook_secret: secret
		}
	}
	mut handler := WebHandler{
		service: service
	}
	master_push := '{"ref":"refs/heads/master","before":"x","repository":{"full_name":"vlang/v","id":1},"commits":[]}'
	mut forged := signed_request('another secret', 'push', master_push)
	assert handler.handle(forged).status() == .unauthorized
	mut unsigned := signed_request(secret, 'push', master_push)
	unsigned.header.delete_custom('X-Hub-Signature-256')
	assert handler.handle(unsigned).status() == .unauthorized
	assert handler.handle(signed_request(secret, 'ping', '{"zen":"x"}')).status() == .ok
	assert handler.handle(signed_request(secret, 'issues', master_push)).status() == .ok
	branch := handler.handle(signed_request(secret, 'push', master_push.replace('refs/heads/master',
		'refs/heads/feature')))
	assert branch.status() == .ok && branch.body.starts_with('ignored')
	fork := handler.handle(signed_request(secret, 'push', master_push.replace('vlang/v', 'evil/v')))
	assert fork.status() == .ok && fork.body.starts_with('ignored')
	assert handler.handle(signed_request(secret, 'push', '{"ref":')).status() == .bad_request
	select {
		reason := <-service.trigger {
			assert false, 'nothing may be queued yet, got ${reason}'
		}
		else {
		}
	}
	first := handler.handle(signed_request(secret, 'push', master_push))
	assert first.status() == .accepted && first.body == 'queued\n'
	second := handler.handle(signed_request(secret, 'push', master_push))
	assert second.status() == .accepted && second.body == 'a run is already queued\n'
	assert <-service.trigger == 'push'
	mut get := signed_request(secret, 'push', master_push)
	get.method = .get
	assert handler.handle(get).status() == .method_not_allowed
}

fn test_webhook_is_disabled_without_a_secret_and_status_pages_answer() {
	mut handler := WebHandler{
		service: &Service{
			config: Config{
				mode:       .push
				repository: 'vlang/v'
			}
		}
	}
	disabled := handler.handle(signed_request('', 'push', '{}'))
	assert disabled.status() == .service_unavailable
	status := handler.handle(http.Request{ url: '/' })
	assert status.status() == .ok
	assert status.body.contains('mode: push\nstate: idle\nruns: 0\n')
	json := handler.handle(http.Request{ url: '/status.json?x=1' })
	assert json.status() == .ok
	assert json.body.contains('"mode":"push"') && json.body.contains('"runs":0')
	assert handler.handle(http.Request{ url: '/log' }).status() == .ok
	assert handler.handle(http.Request{ url: '/.git/config' }).status() == .not_found
	assert handler.handle(http.Request{ method: .post, url: '/' }).status() == .method_not_allowed
}

fn test_config_defaults_are_a_dry_run_and_push_needs_a_token() {
	config, force := parse_config('serve', []) or { panic(err) }
	assert config.mode == .dry_run
	assert !force
	assert config.host == '127.0.0.1'
	assert config.v_dir == '/var/lib/vcgen/v'
	assert config.repository == 'vlang/v'
	assert github_repository('https://github.com/vlang/v.git') == 'vlang/v'
	assert github_repository('/srv/v') == ''
	saved_token := os.getenv('VCGEN_PUSH_TOKEN')
	defer {
		if saved_token != '' {
			os.setenv('VCGEN_PUSH_TOKEN', saved_token, true)
		}
	}
	os.unsetenv('VCGEN_PUSH_TOKEN')
	mut message := ''
	parse_config('serve', ['--mode', 'push']) or { message = err.msg() }
	assert message == 'push mode needs VCGEN_PUSH_TOKEN in the environment'
	pushed, _ := parse_config('run-once', ['--mode', 'push', '--vc-url', '/srv/vc.git']) or {
		panic(err)
	}
	assert pushed.mode == .push
	for args in [['--mode', 'publish'], ['--push-user', 'bot; rm -rf /'], ['--work-dir', 'relative'],
		['extra']] {
		mut rejected := false
		parse_config('serve', args) or { rejected = true }
		assert rejected, '${args}'
	}
}
