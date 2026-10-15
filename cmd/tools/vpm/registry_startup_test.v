module main

import crypto.ed25519
import json2
import net
import net.http
import os
import time

const startup_root = os.join_path(os.vtmp_dir(), 'vpm_registry_startup_${os.getpid()}')
const startup_executable = os.join_path(startup_root, if os.user_os() == 'windows' {
	'vpm.exe'
} else {
	'vpm'
})

fn testsuite_begin() {
	os.mkdir_all(startup_root)!
	result := os.exec([@VEXE, '-cc', @CCOMPILER, '-gc', 'none', '-o', startup_executable,
		os.join_path(@VEXEROOT, 'cmd', 'tools', 'vpm')])
	assert result.exit_code == 0, result.output
}

fn testsuite_end() {
	os.rmdir_all(startup_root) or {}
}

fn start_registry_process(port int) &os.Process {
	return start_registry_process_in(port, startup_root, startup_environment())
}

fn startup_environment() map[string]string {
	mut environment := os.environ()
	for name in environment.keys() {
		// Windows environment variable names are case insensitive.
		if name.to_upper() == registry_key_env {
			environment.delete(name)
		}
	}
	environment['CI'] = '1'
	environment['VMODULES'] = os.join_path(startup_root, 'modules')
	return environment
}

fn start_registry_process_in(port int, directory string, environment map[string]string) &os.Process {
	mut process := os.new_process(startup_executable)
	process.set_args(['registry', 'serve', '--port', port.str()])
	process.set_work_folder(directory)
	process.set_environment(environment)
	process.set_redirect_stdio()
	process.run()
	return process
}

fn stop_registry_process(mut process os.Process) {
	if process.is_alive() {
		process.signal_kill()
	}
	process.wait()
	process.close()
}

fn test_registry_reports_an_occupied_port_without_claiming_startup() {
	// The incumbent deliberately uses net's reusable socket. On Windows the
	// registry must still refuse the bind rather than join or hijack this port.
	mut listener := net.listen_tcp(.ip, ':0')!
	defer { listener.close() or {} }
	port := listener.addr()!.port()!
	mut process := start_registry_process(port)
	defer { stop_registry_process(mut process) }
	for _ in 0 .. 500 {
		if !process.is_alive() {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert !process.is_alive(), 'registry did not report the occupied port'
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	assert process.code == 1, output
	assert output.contains('failed to listen on port ${port}'), output
	assert !output.contains('Serving '), output
	assert !output.contains('Listening on http://'), output
}

fn test_registry_listener_rejects_a_second_owner_and_releases_an_unused_port() {
	mut listener := registry_listen_tcp(0)!
	port := listener.addr()!.port()!
	assert port > 0
	assert listener.accept_timeout() == net.infinite_timeout
	mut rejected := false
	if mut contender := registry_listen_tcp(port) {
		contender.close()!
	} else {
		rejected = true
	}
	assert rejected, 'a second registry listener acquired an occupied port'
	// No connections were accepted, so Windows has no active connection whose
	// lifetime could legitimately delay rebinding an exclusive socket.
	listener.close()!
	mut replacement := registry_listen_tcp(port)!
	defer { replacement.close() or {} }
	assert replacement.addr()!.port()! == port
}

fn test_registry_listener_blocks_a_reusable_contender() {
	mut listener := registry_listen_tcp(0)!
	defer { listener.close() or {} }
	port := listener.addr()!.port()!
	mut rejected := false
	if mut contender := net.listen_tcp(.ip, ':${port}') {
		contender.close()!
	} else {
		rejected = true
	}
	assert rejected, 'a reusable socket acquired the registry port'
}

fn test_registry_reports_startup_after_binding_and_serves_requests() {
	mut listener := net.listen_tcp(.ip, ':0')!
	port := listener.addr()!.port()!
	listener.close()!
	mut process := start_registry_process(port)
	defer { stop_registry_process(mut process) }
	mut output := ''
	for _ in 0 .. 500 {
		output += process.stdout_read()
		if output.contains('Listening on http://') || !process.is_alive() {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert process.is_alive(), output + process.stderr_read()
	assert output.contains('Serving 0 module(s) on port ${port}'), output
	assert output.contains('Listening on http://0.0.0.0:${port}'), output
	assert output.contains('all IPv4 interfaces; no authentication'), output
	response := http.fetch(
		url:           'http://127.0.0.1:${port}/config.json'
		read_timeout:  2 * time.second
		write_timeout: 2 * time.second
	)!
	assert response.status_code == 200
	config := json2.decode[RegistryConfig](response.body)!
	assert config.public_key == ''
	signature := startup_fetch(port, '/signature.sig')!
	assert json2.decode[string](signature.body)! == ''
}

fn startup_case_directory(name string) !string {
	directory := os.join_path(startup_root, name)
	os.mkdir_all(directory)!
	return directory
}

fn startup_case_key_path(directory string) string {
	return os.join_path(directory, '.vpm-registry', registry_key_file_name)
}

fn startup_write_key(directory string, raw string) ! {
	path := startup_case_key_path(directory)
	os.mkdir_all(os.dir(path))!
	os.write_file(path, raw)!
}

fn startup_unused_port() !int {
	mut listener := net.listen_tcp(.ip, ':0')!
	port := listener.addr()!.port()!
	listener.close()!
	return port
}

fn startup_fetch(port int, path string) !http.Response {
	response := http.fetch(
		url:           'http://127.0.0.1:${port}${path}'
		read_timeout:  2 * time.second
		write_timeout: 2 * time.second
	)!
	assert response.status_code == 200, response.body
	return response
}

fn assert_registry_key_rejected_at_port(port int, directory string,
	environment map[string]string, expected string, secret string) ! {
	mut process := start_registry_process_in(port, directory, environment)
	defer { stop_registry_process(mut process) }
	for _ in 0 .. 500 {
		if !process.is_alive() {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert !process.is_alive(), 'registry ignored its invalid signing configuration'
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	assert process.code == 1, output
	assert output.contains(expected), output
	assert !output.contains('Serving '), output
	assert !output.contains('Listening on http://'), output
	if secret.len > 10 {
		assert !output.contains(secret), 'the startup error disclosed the configured seed'
	}
}

fn assert_registry_key_rejected(directory string, environment map[string]string,
	expected string, secret string) ! {
	port := startup_unused_port()!
	assert_registry_key_rejected_at_port(port, directory, environment, expected, secret)!
	// The failed CLI must leave no registry listening on its requested port.
	mut connection := net.dial_tcp('127.0.0.1:${port}') or { return }
	connection.close()!
	assert false, 'invalid signing configuration left a listener running'
}

fn assert_registry_signs_with_key(directory string, environment map[string]string,
	trusted_public_key string) ! {
	port := startup_unused_port()!
	mut process := start_registry_process_in(port, directory, environment)
	defer { stop_registry_process(mut process) }
	mut output := ''
	for _ in 0 .. 500 {
		output += process.stdout_read()
		if output.contains('Listening on http://') || !process.is_alive() {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert process.is_alive(), output + process.stderr_read()
	assert output.contains('Listening on http://0.0.0.0:${port}'), output
	assert output.contains('all IPv4 interfaces; no authentication'), output
	config_response := startup_fetch(port, '/config.json')!
	config := json2.decode[RegistryConfig](config_response.body)!
	assert config.public_key == trusted_public_key
	signature_response := startup_fetch(port, '/signature.sig')!
	signature := json2.decode[string](signature_response.body)!
	assert new_registry().verify_signature(trusted_public_key, signature), 'the running registry did not sign with the expected key'
}

fn test_registry_rejects_empty_malformed_and_wrong_length_environment_keys() {
	_, valid_file_key := ed25519.generate_key()!
	for i, raw in ['', ' \t\n', 'not-a-hexadecimal-private-seed', '0', '00'.repeat(31),
		'00'.repeat(33)] {
		directory := startup_case_directory('invalid-environment-${i}')!
		// A valid fallback must never hide an explicitly configured bad value.
		startup_write_key(directory, valid_file_key.seed().hex())!
		mut environment := startup_environment()
		environment[registry_key_env] = raw
		assert_registry_key_rejected(directory, environment, registry_key_env, raw)!
	}
}

fn test_registry_rejects_empty_malformed_and_wrong_length_key_files() {
	for i, raw in ['', ' \t\n', 'not-a-hexadecimal-private-seed', '0', '00'.repeat(31),
		'00'.repeat(33)] {
		directory := startup_case_directory('invalid-file-${i}')!
		startup_write_key(directory, raw)!
		assert_registry_key_rejected(directory, startup_environment(), 'signing key file', raw)!
	}
}

fn test_registry_rejects_a_signing_key_directory() {
	directory := startup_case_directory('key-directory')!
	os.mkdir_all(startup_case_key_path(directory))!
	assert_registry_key_rejected(directory, startup_environment(), 'not a regular file', '')!
}

fn test_registry_rejects_a_dangling_signing_key_link() {
	$if !windows {
		directory := startup_case_directory('dangling-key')!
		path := startup_case_key_path(directory)
		os.mkdir_all(os.dir(path))!
		os.symlink(os.join_path(directory, 'missing-seed'), path)!
		assert_registry_key_rejected(directory, startup_environment(), 'failed to inspect registry signing key file', '')!
	}
}

fn test_registry_rejects_an_unreadable_signing_key_file() {
	$if !windows {
		if os.getuid() == 0 {
			// Root can read a mode-000 file; this fixture needs normal user access.
			return
		}
		directory := startup_case_directory('unreadable-key')!
		_, private := ed25519.generate_key()!
		startup_write_key(directory, private.seed().hex())!
		path := startup_case_key_path(directory)
		os.chmod(path, 0)!
		defer { os.chmod(path, 0o600) or {} }
		assert_registry_key_rejected(directory, startup_environment(), 'failed to read registry signing key file', private.seed().hex())!
	}
}

fn test_registry_signs_with_a_valid_environment_key() {
	directory := startup_case_directory('valid-environment')!
	public, private := ed25519.generate_key()!
	mut environment := startup_environment()
	environment[registry_key_env] = private.seed().hex()
	assert_registry_signs_with_key(directory, environment, public.hex())!
}

fn test_registry_signs_with_a_valid_key_file() {
	directory := startup_case_directory('valid-file')!
	public, private := ed25519.generate_key()!
	startup_write_key(directory, private.seed().hex() + '\n')!
	assert_registry_signs_with_key(directory, startup_environment(), public.hex())!
}

fn test_a_valid_environment_key_takes_precedence_over_the_key_file() {
	directory := startup_case_directory('environment-precedence')!
	public, private := ed25519.generate_key()!
	_, other_private := ed25519.generate_key()!
	startup_write_key(directory, other_private.seed().hex())!
	mut environment := startup_environment()
	environment[registry_key_env] = private.seed().hex()
	assert_registry_signs_with_key(directory, environment, public.hex())!
	// A configured environment key must also bypass an invalid fallback file.
	startup_write_key(directory, 'not-a-valid-fallback-seed')!
	assert_registry_signs_with_key(directory, environment, public.hex())!
}

fn test_registry_accepts_a_signing_key_link_to_a_regular_file() {
	$if !windows {
		directory := startup_case_directory('valid-key-link')!
		public, private := ed25519.generate_key()!
		path := startup_case_key_path(directory)
		os.mkdir_all(os.dir(path))!
		target := os.join_path(directory, 'trusted-seed')
		os.write_file(target, private.seed().hex())!
		os.symlink(target, path)!
		assert_registry_signs_with_key(directory, startup_environment(), public.hex())!
	}
}

fn test_registry_validates_its_signing_key_before_binding() {
	mut listener := net.listen_tcp(.ip, ':0')!
	defer { listener.close() or {} }
	port := listener.addr()!.port()!
	directory := startup_case_directory('key-before-listen')!
	mut environment := startup_environment()
	environment[registry_key_env] = 'invalid-pre-bind-secret-seed'
	assert_registry_key_rejected_at_port(port, directory, environment, registry_key_env,
		environment[registry_key_env])!
}
