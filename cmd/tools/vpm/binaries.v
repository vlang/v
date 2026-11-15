module main

import crypto.sha256
import json2
import net.http
import net.urllib
import os
import rand
import time

const binary_manifest_name = 'vpm-binaries.json'
const binary_directory = '.vpm-binaries'

struct BinaryFile {
	path   string
	url    string
	sha256 string
}

struct BinaryManifest {
	version int
	targets map[string][]BinaryFile
}

fn binary_host_target() string {
	arch := $if arm64 { 'arm64' } $else $if amd64 { 'amd64' } $else $if i386 { 'i386' } $else { 'unknown' }
	return '${os.user_os()}-${arch}'
}

fn binary_relative_path(path string) bool {
	if path == '' || path.contains('\\') || path.contains(':') { return false }
	for part in path.split('/') {
		if part == '' || part in ['.', '..'] || part != part.trim_right(' .') { return false }
		if !part.bytes().all((it >= `a` && it <= `z`) || (it >= `A` && it <= `Z`)
			|| (it >= `0` && it <= `9`) || it in [`-`, `_`, `.`]) {
			return false
		}
		// These device names bypass ordinary file handling on Windows, including
		// when followed by an extension. Keep manifests portable across hosts.
		base := part.all_before('.').to_upper()
		if base in ['CON', 'PRN', 'AUX', 'NUL']
			|| (base.len == 4 && (base.starts_with('COM') || base.starts_with('LPT'))
				&& base[3] >= `1` && base[3] <= `9`) {
			return false
		}
	}
	return true
}

fn binary_url_valid(address string) bool {
	url := urllib.parse(address) or { return false }
	if url.hostname() == '' || url.fragment != '' {
		return false
	}
	if user := url.user {
		if user.username != '' || user.password != '' || user.password_set { return false }
	}
	return url.scheme == 'https' || (url.scheme == 'http' && url.hostname() in [
		'127.0.0.1',
		'localhost',
		'::1',
	])
}

fn binary_redirect(request &http.Request, _ int, next string) ! {
	if !binary_url_valid(next) { return error('invalid binary download redirect') }
	if request.url.starts_with('https://') && !next.starts_with('https://') {
		return error('refusing an HTTPS binary download downgrade')
	}
}

fn binary_files(text string, target string) ![]BinaryFile {
	manifest := json2.decode[BinaryManifest](text)!
	if manifest.version != 1 { return error('unsupported ${binary_manifest_name} version') }
	mut files := []BinaryFile{}
	for name, entries in manifest.targets {
		mut paths := map[string]bool{}
		for entry in entries {
			if !binary_relative_path(entry.path) || entry.path.to_lower() == 'installed.json' {
				return error('invalid binary path `${entry.path}`')
			}
			if entry.path in paths { return error('duplicate binary path `${entry.path}`') }
			paths[entry.path] = true
			if entry.sha256.len != 64 || !entry.sha256.bytes().all((it >= `0` && it <= `9`) || (it >= `a` && it <= `f`)) {
				return error('binary `${entry.path}` requires a lowercase SHA256 checksum')
			}
			if !binary_url_valid(entry.url) { return error('invalid binary URL `${entry.url}`') }
		}
		if name == target { files = entries.clone() }
	}
	return files
}

fn binary_file_matches(root string, file BinaryFile) bool {
	mut path := root
	if os.is_link(path) { return false }
	for component in file.path.split('/') {
		path = os.join_path(path, component)
		if os.is_link(path) { return false }
	}
	if !os.is_file(path) { return false }
	data := os.read_bytes(path) or { return false }
	return sha256.sum(data).hex() == file.sha256
}

fn binary_receipt(text string, target string) string {
	return json2.encode({
		'manifest_sha256': sha256.hexhash(text)
		'target':          target
	})
}

fn binary_install_matches(root string, files []BinaryFile, receipt string) bool {
	if os.is_link(root) || os.is_link(os.join_path(root, 'installed.json')) { return false }
	if (os.read_file(os.join_path(root, 'installed.json')) or { return false }) != receipt {
		return false
	}
	return files.all(binary_file_matches(root, it))
}

// Binary payloads are derived, host-specific inputs. Git and project source
// hashes must continue to describe the checkout, including the pinned manifest.
fn ignore_binary_directory(root string) ! {
	metadata := os.join_path(root, '.git')
	if os.is_link(metadata) { return error('refusing symlinked Git metadata') }
	if !os.is_dir(metadata) { return }
	info := os.join_path(metadata, 'info')
	if os.is_link(info) { return error('refusing symlinked Git info directory') }
	os.mkdir_all(info)!
	exclude := os.join_path(info, 'exclude')
	if os.is_link(exclude) { return error('refusing symlinked Git exclude file') }
	previous := os.read_file(exclude) or { '' }
	if '/${binary_directory}/' !in previous.split_into_lines() {
		os.write_file(exclude, previous.trim_right('\n') + '\n/${binary_directory}/\n')!
	}
}

fn publish_binary_directory(stage string, destination string) ! {
	if os.is_link(destination) { return error('refusing symlinked binary directory') }
	backup := destination + '-previous-' + rand.ulid()
	previous := os.exists(destination)
	if previous { os.rename(destination, backup)! }
	os.rename(stage, destination) or {
		if previous { os.rename(backup, destination) or {} }
		return err
	}
	if previous { os.rmdir_all(backup)! }
}

fn clear_binary_artifacts(root string) ! {
	destination := os.join_path(root, binary_directory)
	if os.is_link(destination) { return error('refusing symlinked binary directory') }
	if os.exists(destination) { os.rmdir_all(destination)! }
}

// Copy only declared, verified files into a newly created directory. Neither
// existing destination symlinks nor unlisted cache entries participate.
fn copy_binary_artifacts(source string, root string, files []BinaryFile, receipt string) ! {
	destination := os.join_path(root, binary_directory)
	if os.is_link(destination) { return error('refusing symlinked binary directory') }
	stage := destination + '-stage-' + rand.ulid()
	os.mkdir(stage)!
	defer { os.rmdir_all(stage) or {} }
	for file in files {
		if !binary_file_matches(source, file) { return error('cached binary inputs changed') }
		path := os.join_path(stage, file.path)
		os.mkdir_all(os.dir(path))!
		os.cp(os.join_path(source, file.path), path)!
	}
	os.write_file(os.join_path(stage, 'installed.json'), receipt)!
	if !binary_install_matches(stage, files, receipt) {
		return error('staged binary inputs changed')
	}
	publish_binary_directory(stage, destination)!
}

fn prepare_binary_artifacts(root string, target string) ! {
	manifest_path := os.join_path(root, binary_manifest_name)
	if os.is_link(manifest_path) { return error('refusing symlinked binary manifest') }
	if !os.exists(manifest_path) { return clear_binary_artifacts(root) }
	text := os.read_file(manifest_path)!
	files := binary_files(text, target)!
	if files.len == 0 { return clear_binary_artifacts(root) }
	destination := os.join_path(root, binary_directory)
	if os.is_link(destination) { return error('refusing symlinked binary directory') }
	ignore_binary_directory(root)!
	receipt := binary_receipt(text, target)
	if binary_install_matches(destination, files, receipt) { return }
	stage := destination + '-stage-' + rand.ulid()
	os.mkdir(stage)!
	defer { os.rmdir_all(stage) or {} }
	for file in files {
		response := http.fetch(
			url:           file.url
			validate:      true
			read_timeout:  60 * time.second
			write_timeout: 60 * time.second
			max_retries:   1
			on_redirect:   binary_redirect
		)!
		if response.status_code != 200 {
			return error('binary download returned HTTP ${response.status_code}')
		}
		if sha256.hexhash(response.body) != file.sha256 {
			return error('binary checksum mismatch for `${file.path}`')
		}
		path := os.join_path(stage, file.path)
		os.mkdir_all(os.dir(path))!
		os.write_file(path, response.body)!
	}
	os.write_file(os.join_path(stage, 'installed.json'), receipt)!
	if !binary_install_matches(stage, files, receipt) { return error('conflicting binary paths') }
	publish_binary_directory(stage, destination)!
}

fn (m Module) prepare_binaries() ! {
	manifest := os.join_path(m.tmp_path, binary_manifest_name)
	if os.is_link(manifest) { return error('refusing symlinked binary manifest') }
	if !os.is_file(manifest) { return clear_binary_artifacts(m.tmp_path) }
	text := os.read_file(manifest)!
	target := binary_host_target()
	files := binary_files(text, target)!
	installed := os.join_path(m.install_path, binary_directory)
	if files.len > 0 && binary_install_matches(installed, files, binary_receipt(text, target)) {
		copy_binary_artifacts(installed, m.tmp_path, files, binary_receipt(text, target))!
	}
	prepare_binary_artifacts(m.tmp_path, target)!
}

// Copy an already verified staged payload when installation reuses/restores an
// existing checkout. Never redownload assets merely because source is current.
fn (m Module) install_prepared_binaries() ! {
	manifest := os.join_path(m.tmp_path, binary_manifest_name)
	if os.is_link(manifest) { return error('refusing symlinked binary manifest') }
	if !os.is_file(manifest) { return clear_binary_artifacts(m.install_path) }
	source := os.join_path(m.tmp_path, binary_directory)
	text := os.read_file(manifest)!
	target := binary_host_target()
	files := binary_files(text, target)!
	if files.len == 0 { return clear_binary_artifacts(m.install_path) }
	receipt := binary_receipt(text, target)
	if !binary_install_matches(source, files, receipt) {
		return error('staged binary inputs changed')
	}
	destination := os.join_path(m.install_path, binary_directory)
	if binary_install_matches(destination, files, receipt) { return }
	ignore_binary_directory(m.install_path)!
	copy_binary_artifacts(source, m.install_path, files, receipt)!
}
