module main

import json2
import net
import net.http
import os
import time

const registry_home_root = os.join_path(os.vtmp_dir(), 'vpm_registry_home_${os.getpid()}')
const registry_home_executable = os.join_path(registry_home_root, if os.user_os() == 'windows' {
	'vpm.exe'
} else {
	'vpm'
})

fn testsuite_begin() {
	os.mkdir_all(registry_home_root)!
	result := os.exec([@VEXE, '-cc', @CCOMPILER, '-gc', 'none', '-o', registry_home_executable,
		os.join_path(@VEXEROOT, 'cmd', 'tools', 'vpm')])
	assert result.exit_code == 0, result.output
}

fn testsuite_end() {
	os.rmdir_all(registry_home_root) or {}
}

fn registry_home_case(name string) !string {
	base := os.join_path(registry_home_root, name)
	os.mkdir_all(os.join_path(base, 'work'))!
	os.mkdir_all(os.join_path(base, 'temp'))!
	return base
}

fn registry_home_environment(base string) map[string]string {
	mut environment := os.environ()
	for name in environment.keys() {
		if name.to_upper() in ['CI', 'VPM_DEBUG', registry_key_env, 'HOME', 'USERPROFILE', 'HOMEDRIVE',
			'HOMEPATH', 'LOCALAPPDATA', 'APPDATA', 'VMODULES', 'XDG_CACHE_HOME'] {
			environment.delete(name)
		}
	}
	environment['HOME'] = os.join_path(base, 'home')
	environment['USERPROFILE'] = os.join_path(base, 'home')
	environment['TMPDIR'] = os.join_path(base, 'temp')
	environment['TEMP'] = os.join_path(base, 'temp')
	environment['TMP'] = os.join_path(base, 'temp')
	environment['VTMP'] = os.join_path(base, 'temp')
	return environment
}

fn registry_home_process(base string, environment map[string]string, args []string) &os.Process {
	mut process := os.new_process(registry_home_executable)
	process.set_work_folder(os.join_path(base, 'work'))
	process.set_environment(environment)
	process.set_args(args)
	process.set_redirect_stdio()
	process.run()
	return process
}

fn registry_home_stop(mut process os.Process) {
	if process.is_alive() {
		process.signal_kill()
	}
	process.wait()
	process.close()
}

fn assert_registry_serves_without_module_cache(base string, environment map[string]string) ! {
	mut reservation := net.listen_tcp(.ip, ':0')!
	port := reservation.addr()!.port()!
	reservation.close()!
	// The mirror's value is another command name. Only the actual parsed
	// command should determine whether module/cache initialization is skipped.
	mut process := registry_home_process(base, environment, ['--mirror', 'install', 'registry',
		'serve', '--port', port.str()])
	defer { registry_home_stop(mut process) }
	mut output := ''
	for _ in 0 .. 1000 {
		output += process.stdout_read()
		if output.contains('Listening on http://') || !process.is_alive() {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert process.is_alive(), output + process.stderr_read()
	assert output.contains('Listening on http://'), output
	assert !output.contains('Creating `'), output
	response := http.fetch(
		url:           'http://127.0.0.1:${port}/config.json'
		read_timeout:  2 * time.second
		write_timeout: 2 * time.second
	)!
	assert response.status_code == 200, response.body
	config := json2.decode[RegistryConfig](response.body)!
	assert config.public_key == ''
	assert !os.exists(os.join_path(base, 'work', '.vpm-registry'))
	assert !os.exists(os.join_path(base, 'work', '.cache'))
	assert !os.exists(os.join_path(base, 'temp', '.vmodules'))
}

fn test_registry_serve_ignores_a_vmodules_regular_file_without_ci() {
	base := registry_home_case('blocked-modules')!
	modules := os.join_path(base, 'modules')
	os.write_file(modules, 'preserve the module-store blocker')!
	mut environment := registry_home_environment(base)
	environment['VMODULES'] = modules
	assert_registry_serves_without_module_cache(base, environment)!
	assert os.read_file(modules)! == 'preserve the module-store blocker'
	assert !os.exists(os.join_path(base, 'home'))
}

fn test_registry_serve_does_not_create_a_missing_home_or_module_cache_without_ci() {
	base := registry_home_case('missing-home')!
	assert_registry_serves_without_module_cache(base, registry_home_environment(base))!
	assert !os.exists(os.join_path(base, 'home'))
}

fn test_registry_serve_works_without_home_or_vmodules_environment() {
	base := registry_home_case('no-home-environment')!
	mut environment := registry_home_environment(base)
	environment.delete('HOME')
	environment.delete('USERPROFILE')
	assert_registry_serves_without_module_cache(base, environment)!
	assert !os.exists(os.join_path(base, 'home'))
}

fn test_registry_serve_ignores_a_home_path_that_is_a_regular_file() {
	base := registry_home_case('blocked-home')!
	home := os.join_path(base, 'home')
	os.write_file(home, 'preserve the home-path blocker')!
	assert_registry_serves_without_module_cache(base, registry_home_environment(base))!
	assert os.read_file(home)! == 'preserve the home-path blocker'
}

fn test_registry_serve_does_not_write_a_read_only_home_or_module_store() {
	$if !windows {
		if os.getuid() == 0 {
			return
		}
		base := registry_home_case('read-only-home')!
		home := os.join_path(base, 'home')
		modules := os.join_path(base, 'modules')
		os.mkdir_all(home)!
		os.mkdir_all(modules)!
		os.chmod(home, 0o555)!
		os.chmod(modules, 0o555)!
		defer {
			os.chmod(home, 0o700) or {}
			os.chmod(modules, 0o700) or {}
		}
		mut environment := registry_home_environment(base)
		environment['VMODULES'] = modules
		assert_registry_serves_without_module_cache(base, environment)!
		assert os.ls(home)!.len == 0
		assert os.ls(modules)!.len == 0
	}
}

fn test_module_commands_still_initialize_cache_when_a_mirror_value_is_registry() {
	base := registry_home_case('normal-module-command')!
	modules := os.join_path(base, 'modules')
	mut environment := registry_home_environment(base)
	environment['VMODULES'] = modules
	mut process := registry_home_process(base, environment, ['--mirror', 'registry', 'list'])
	defer { registry_home_stop(mut process) }
	for _ in 0 .. 1000 {
		if !process.is_alive() {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert !process.is_alive(), 'module list did not exit'
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	assert process.code == 0, output
	assert output.contains('You have no modules installed.'), output
	assert os.is_dir(os.join_path(modules, '.cache'))
}
