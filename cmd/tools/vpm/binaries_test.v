module main

import crypto.sha256
import json2
import net.http
import os
import rand
import test_utils

struct BinaryFixture {}

fn (_ BinaryFixture) handle(request http.Request) http.Response {
	return http.Response{
		body: match request.url {
			'/library.a' { 'native binary\x00payload' }
			'/library-v2.a' { 'updated native binary' }
			else { 'wrong payload' }
		}
	}
}

fn binary_test_run_vpm(executable string, project string, environment map[string]string, args []string) string {
	mut process := os.new_process(executable)
	process.set_environment(environment)
	process.set_work_folder(project)
	process.set_args(args)
	process.set_redirect_stdio()
	process.run()
	output := process.stdout_slurp() + process.stderr_slurp()
	process.wait()
	assert process.code == 0, output
	process.close()
	return output
}

fn binary_test_manifest(address string, target string, sha string) string {
	return '{"version":1,"targets":' + json2.encode({
		target: [BinaryFile{
			path:   'lib/libexample.a'
			url:    address
			sha256: sha
		}]
	}) + '}'
}

fn test_binary_manifest_selects_host_and_rejects_unpinned_or_unsafe_files() {
	sha := sha256.hexhash('native binary\x00payload')
	text := binary_test_manifest('https://example.com/library.a', 'macos-arm64', sha)
	assert binary_files(text, 'macos-arm64')!.len == 1
	assert binary_files(text, 'linux-amd64')!.len == 0
	for path in ['', '/absolute', '../escape', 'a/../../escape', 'a//b', 'a/./b', 'a\\b', 'C:/file',
		'installed.json'] {
		bad := text.replace('lib/libexample.a', path.replace('\\', '\\\\'))
		if _ := binary_files(bad, 'macos-arm64') {
			assert false, 'accepted path ${path}'
		}
	}
	for bad in [text.replace(sha, ''), text.replace(sha, 'a'.repeat(63) + 'z'),
		text.replace('"version":1', '"version":2'),
		text.replace('https://example.com', 'http://example.com'),
		text.replace('https://example.com', 'https://user:secret@example.com')] {
		if _ := binary_files(bad, 'macos-arm64') {
			assert false, 'accepted invalid binary manifest'
		}
	}
	assert binary_url_valid('http://127.0.0.1:8000/library.a')
	assert !binary_url_valid('file:///tmp/library.a')
	assert !binary_url_valid('https:///no-host')
	assert !binary_url_valid('https://example.com/library.a#fragment')
	for path in ['a/.. /escape', 'con', 'NUL.a', 'COM1.dll', 'Lpt9', 'a\x00b', 'file.', 'file ',
		'installed.JSON'] {
		assert !binary_relative_path(path) || path.to_lower() == 'installed.json', path
	}
	if _ := binary_redirect(&http.Request{ url: 'https://example.com/library.a' }, 1,
		'http://127.0.0.1/library.a') {
		assert false, 'accepted HTTPS downgrade'
	}
}

fn test_binary_download_is_checked_cached_and_published_only_after_success() ! {
	root := os.join_path(os.temp_dir(), 'vpm-binary-' + rand.ulid())
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut server := test_utils.start_mock_http_server(BinaryFixture{})!
	sha := sha256.hexhash('native binary\x00payload')
	manifest := binary_test_manifest('${server.base_url}/library.a', binary_host_target(), sha)
	os.write_file(os.join_path(root, binary_manifest_name), manifest)!
	before := dir_sha256(root)!
	prepare_binary_artifacts(root, binary_host_target())!
	library := os.join_path(root, binary_directory, 'lib', 'libexample.a')
	assert os.read_file(library)! == 'native binary\x00payload'
	assert dir_sha256(root)! == before
	server.close()
	// An offline repeated preparation must verify and reuse the existing bytes.
	prepare_binary_artifacts(root, binary_host_target())!
	// A failed replacement cannot clobber the last verified installation.
	mut wrong := test_utils.start_mock_http_server(BinaryFixture{})!
	defer { wrong.close() }
	os.write_file(os.join_path(root, binary_manifest_name), binary_test_manifest('${wrong.base_url}/wrong', binary_host_target(), sha))!
	if _ := prepare_binary_artifacts(root, binary_host_target()) {
		assert false, 'accepted wrong checksum'
	}
	assert os.read_file(library)! == 'native binary\x00payload'
	assert dir_sha256(root)! != before
	// Corruption is detected rather than blessed by the existing receipt.
	os.write_file(library, 'corrupt')!
	assert !binary_file_matches(os.join_path(root, binary_directory), BinaryFile{ path: 'lib/libexample.a', sha256: sha })
}

fn test_binary_install_refuses_symlink_destinations() ! {
	$if windows {
		return
	}
	root := os.join_path(os.temp_dir(), 'vpm-binary-link-' + rand.ulid())
	os.mkdir_all(os.join_path(root, 'outside'))!
	os.mkdir_all(os.join_path(root, 'module'))!
	defer { os.rmdir_all(root) or {} }
	module_path := os.join_path(root, 'module')
	os.symlink(os.join_path(root, 'outside'), os.join_path(module_path, binary_directory))!
	os.write_file(os.join_path(module_path, binary_manifest_name), binary_test_manifest('https://example.com/library.a', binary_host_target(), 'a'.repeat(64)))!
	if _ := prepare_binary_artifacts(module_path, binary_host_target()) {
		assert false, 'followed binary symlink'
	}
	assert os.ls(os.join_path(root, 'outside'))!.len == 0
}

fn test_vpm_install_fetches_binary_assets_and_keeps_locked_source_hash_stable() ! {
	root := os.join_path(os.temp_dir(), 'vpm-binary-install-' + rand.ulid())
	module_path := os.join_path(root, 'origin')
	os.mkdir_all(module_path)!
	defer { os.rmdir_all(root) or {} }
	mut server := test_utils.start_mock_http_server(BinaryFixture{})!
	mut server_closed := false
	defer { if !server_closed { server.close() } }
	os.write_file(os.join_path(module_path, 'v.mod'), "Module { name: 'binary_demo' version: '1.0.0' }")!
	os.write_file(os.join_path(module_path, 'demo.v'), 'module binary_demo\npub fn value() int { return 42 }\n')!
	os.write_file(os.join_path(module_path, binary_manifest_name), binary_test_manifest('${server.base_url}/library.a', binary_host_target(), sha256.hexhash('native binary\x00payload')))!
	for args in [['init', '--quiet'], ['add', '.'], ['-c', 'user.name=VPM test', '-c',
		'user.email=vpm@example.invalid', 'commit', '--quiet', '-m', 'fixture']] {
		result := os.exec(['git', '-C', module_path, ...args])
		assert result.exit_code == 0, result.output
	}
	executable := os.join_path(root, 'vpm' + $if windows { '.exe' } $else { '' })
	compiled := os.exec([@VEXE, '-b', 'c', '-memory-limit', '10G', '-o', executable, os.dir(@FILE)])
	assert compiled.exit_code == 0, compiled.output
	project := os.join_path(root, 'project')
	os.mkdir_all(project)!
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'client' dependencies: ['${module_path.replace('\\', '/')}'] }")!
	mut environment := os.environ()
	environment['VMODULES'] = os.join_path(root, 'modules')
	environment['VTMP'] = os.join_path(root, 'tmp')
	environment['VEXE'] = @VEXE
	environment['VPM_NO_INCREMENT'] = '1'
	environment['VPM_FAIL_ON_PROMPT'] = '1'
	binary_test_run_vpm(executable, project, environment, ['install'])
	installed := os.join_path(root, 'modules', 'binary_demo')
	assert os.read_file(os.join_path(installed, binary_directory, 'lib', 'libexample.a'))! == 'native binary\x00payload'
	assert dir_sha256(installed)! == dir_sha256(module_path)!
	assert local_git_changes_reason(installed) == ''
	// Reinstall repairs a missing binary even when the source revision is current.
	os.rm(os.join_path(installed, binary_directory, 'lib', 'libexample.a'))!
	binary_test_run_vpm(executable, project, environment, ['install', '--locked'])
	assert os.read_file(os.join_path(installed, binary_directory, 'lib', 'libexample.a'))! == 'native binary\x00payload'
	// Updating the source revision must also replace its associated native assets.
	os.write_file(os.join_path(module_path, binary_manifest_name), binary_test_manifest('${server.base_url}/library-v2.a', binary_host_target(), sha256.hexhash('updated native binary')))!
	for args in [['add', '.'], ['-c', 'user.name=VPM test', '-c', 'user.email=vpm@example.invalid',
		'commit', '--quiet', '-m', 'updated binary']] {
		result := os.exec(['git', '-C', module_path, ...args])
		assert result.exit_code == 0, result.output
	}
	binary_test_run_vpm(executable, project, environment, ['update', 'binary_demo'])
	assert os.read_file(os.join_path(installed, binary_directory, 'lib', 'libexample.a'))! == 'updated native binary'
	server.close()
	server_closed = true
	lock_before := os.read_file(os.join_path(project, lockfile_name))!
	binary_test_run_vpm(executable, project, environment, ['install', '--locked'])
	assert os.read_file(os.join_path(project, lockfile_name))! == lock_before
}
