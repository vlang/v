module main

import json2
import net
import os
import test_utils { cmd_fail_args, cmd_ok_args }

const override_test_root = os.join_path(os.vtmp_dir(), 'vpm_overrides_${os.getpid()}')
const override_original_dir = os.getwd()
const override_vpm_exe = os.join_path(override_test_root, if os.user_os() == 'windows' {
	'vpm.exe'
} else {
	'vpm'
})

fn testsuite_begin() {
	os.mkdir_all(override_test_root)!
	test_utils.set_test_env(os.join_path(override_test_root, 'build-store'))
	os.setenv('VEXE', @VEXE, true)
	cmd_ok_args(@LOCATION, [@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', 'clang', '-gc',
		'none', '-o', override_vpm_exe, os.join_path(@VEXEROOT, 'cmd', 'tools', 'vpm')])
}

fn testsuite_end() {
	os.chdir(override_original_dir)!
	os.rmdir_all(override_test_root) or {}
}

fn override_git(repo string, args []string) string {
	return cmd_ok_args(@LOCATION, ['git', '-C', repo, '-c', 'user.email=ci@vlang.io', '-c',
		'user.name=V CI', ...args]).output.trim_space()
}

fn override_repo(directory string) !string {
	repo := os.join_path(override_test_root, directory)
	os.mkdir_all(repo)!
	override_git(repo, ['init', '-b', 'main'])
	return repo
}

fn override_tag(repo string, name string, tag string, min_v string, dependencies []string, overrides []string) !string {
	deps := dependencies.map("'${it.replace('\\', '/')}'").join(', ')
	forced := overrides.map("'${it}'").join(', ')
	os.write_file(os.join_path(repo, 'v.mod'), "Module {\nname: '${name}'\nmin_v: '${min_v}'\ndependencies: [${deps}]\ndependency_overrides: [${forced}]\n}\n")!
	os.write_file(os.join_path(repo, 'marker.txt'), tag)!
	override_git(repo, ['add', 'v.mod', 'marker.txt'])
	override_git(repo, ['commit', '-m', tag])
	override_git(repo, ['tag', tag])
	return override_git(repo, ['rev-parse', 'HEAD'])
}

fn override_project(directory string, deps []string, overrides []string) !string {
	project := os.join_path(override_test_root, directory)
	os.mkdir_all(project)!
	dep_text := deps.map("'${it.replace('\\', '/')}'").join(', ')
	forced := overrides.map("'${it}'").join(', ')
	os.write_file(os.join_path(project, 'v.mod'), "Module {\nname: 'root'\ndependencies: [${dep_text}]\ndependency_overrides: [${forced}]\n}\n")!
	os.chdir(project)!
	return project
}

fn test_override_selects_source_manifest_dependencies_and_locked_revision() {
	leaf := override_repo('leaf-source')!
	leaf_one := override_tag(leaf, 'leaf', 'v1.0.0', '0.0.1', [], [])!
	override_tag(leaf, 'leaf', 'v2.0.0', '0.0.1', [], [])!
	parent := override_repo('different-repository-basename')!
	override_tag(parent, 'pkg', 'v1.0.0', '99.0.0', ['does.not.exist'], [])!
	parent_two := override_tag(parent, 'pkg', 'v2.0.0', '0.0.1', [leaf + '@v1.0.0'], ['leaf: missing-tag'])!
	dep := parent + '@v1.0.0'
	project := override_project('exact-project', [dep], ['pkg: v2.0.0'])!
	store := os.join_path(override_test_root, 'exact-store')
	test_utils.set_test_env(store)
	cmd_ok_args(@LOCATION, [override_vpm_exe, 'install'])
	assert os.read_file(os.join_path(store, 'pkg', 'marker.txt'))! == 'v2.0.0'
	assert override_git(os.join_path(store, 'pkg'), ['rev-parse', 'HEAD']) == parent_two
	assert override_git(os.join_path(store, 'leaf'), ['rev-parse', 'HEAD']) == leaf_one
	assert os.read_file(os.join_path(store, 'leaf', 'marker.txt'))! == 'v1.0.0'
	entry := read_lockfile(project)!.modules[lockfile_module_key(dep)]!
	assert entry.requested == parent + '@v2.0.0'
	assert entry.resolved == 'v2.0.0'
	assert entry.revision == parent_two
	override_tag(parent, 'pkg', 'v3.0.0', '0.0.1', [leaf + '@v2.0.0'], [])!
	// A warm store and a fresh store both keep the exact selected revision.
	cmd_ok_args(@LOCATION, [override_vpm_exe, 'install', '--locked'])
	assert override_git(os.join_path(store, 'pkg'), ['rev-parse', 'HEAD']) == parent_two
	assert override_git(os.join_path(store, 'leaf'), ['rev-parse', 'HEAD']) == leaf_one
	// Matching locks also avoid prompts for a regular warm reinstall.
	cmd_ok_args(@LOCATION, [override_vpm_exe, 'install'])
	assert override_git(os.join_path(store, 'leaf'), ['rev-parse', 'HEAD']) == leaf_one
	fresh := os.join_path(override_test_root, 'exact-fresh-store')
	test_utils.set_test_env(fresh)
	cmd_ok_args(@LOCATION, [override_vpm_exe, 'install', '--locked'])
	assert override_git(os.join_path(fresh, 'pkg'), ['rev-parse', 'HEAD']) == parent_two
	override_project('exact-project', [dep], ['pkg: v3.0.0'])!
	failed := cmd_fail_args(@LOCATION, [override_vpm_exe, 'install', '--locked'])
	assert failed.output.contains('records'), failed.output
	assert override_git(os.join_path(fresh, 'pkg'), ['rev-parse', 'HEAD']) == parent_two
	cmd_ok_args(@LOCATION, [override_vpm_exe, 'install', '-f'])
	assert os.read_file(os.join_path(fresh, 'pkg', 'marker.txt'))! == 'v3.0.0'
	assert os.read_file(os.join_path(fresh, 'leaf', 'marker.txt'))! == 'v2.0.0'
	assert read_lockfile(project)!.modules[lockfile_module_key(dep)]!.requested == parent + '@v3.0.0'
}

fn test_override_range_resolves_and_locks_actual_tag() {
	repo := override_repo('range_pkg')!
	override_tag(repo, 'range_pkg', 'v1.0.0', '0.0.1', [], [])!
	selected := override_tag(repo, 'range_pkg', 'v2.1.0', '0.0.1', [], [])!
	project := override_project('range-project', [repo + '@missing-tag'], ['range_pkg: ^2.0.0'])!
	test_utils.set_test_env(os.join_path(override_test_root, 'range-store'))
	cmd_ok_args(@LOCATION, [override_vpm_exe, 'install'])
	entry := read_lockfile(project)!.modules[repo]!
	assert entry.requested == repo + '@^2.0.0'
	assert entry.resolved == 'v2.1.0'
	assert entry.revision == selected
	override_tag(repo, 'range_pkg', 'v2.2.0', '0.0.1', [], [])!
	store := os.join_path(override_test_root, 'range-fresh-store')
	test_utils.set_test_env(store)
	cmd_ok_args(@LOCATION, [override_vpm_exe, 'install', '--locked'])
	assert override_git(os.join_path(store, 'range_pkg'), ['rev-parse', 'HEAD']) == selected
	assert os.read_file(os.join_path(store, 'range_pkg', 'marker.txt'))! == 'v2.1.0'
}

fn test_selector_override_selects_the_requiring_edges_source_before_min_v() {
	leaf := override_repo('selector-leaf')!
	selected := override_tag(leaf, 'c', 'v1.0.0', '0.0.1', [], [])!
	override_tag(leaf, 'c', 'v2.0.0', '99.0.0', ['does.not.exist'], [])!
	parent := override_repo('selector-parent')!
	override_tag(parent, 'requiring', 'v1.0.0', '0.0.1', [leaf + '@v2.0.0'], [])!
	project := override_project('selector-project', [parent + '@v1.0.0'], ['requiring>c: v1.0.0'])!
	store := os.join_path(override_test_root, 'selector-store')
	test_utils.set_test_env(store)
	cmd_ok_args(@LOCATION, [override_vpm_exe, 'install'])
	assert override_git(os.join_path(store, 'c'), ['rev-parse', 'HEAD']) == selected
	assert os.read_file(os.join_path(store, 'c', 'marker.txt'))! == 'v1.0.0'
	assert read_lockfile(project)!.modules[leaf]!.requested == leaf + '@v1.0.0'
	other := override_repo('selector-other')!
	override_tag(other, 'other', 'v1.0.0', '0.0.1', [leaf + '@v2.0.0'], [])!
	override_project('selector-other-project', [other + '@v1.0.0'], ['requiring>c: v1.0.0'])!
	other_store := os.join_path(override_test_root, 'selector-other-store')
	test_utils.set_test_env(other_store)
	rejected := cmd_fail_args(@LOCATION, [override_vpm_exe, 'install'])
	assert rejected.output.contains('requires V 99.0.0'), rejected.output
	assert !os.exists(os.join_path(other_store, 'c'))
}

fn override_metadata_once(mut listener net.TcpListener, repo string) {
	mut conn := listener.accept() or { panic(err) }
	defer { conn.close() or {} }
	mut buffer := []u8{len: 2048}
	count := conn.read(mut buffer) or { panic(err) }
	assert buffer[..count].bytestr().starts_with('GET /api/packages/publisher.pkg ')
	body := json2.encode(ModuleVpmInfo{ name: 'publisher.pkg', url: repo, vcs: 'git' })
	conn.write_string('HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: ${body.len}\r\nConnection: close\r\n\r\n${body}') or { panic(err) }
}

fn test_min_v_rejection_for_registered_and_direct_repositories() {
	for index, required in ['99.0.0', 'not-a-version'] {
		repo := override_repo('min-v-${index}')!
		override_tag(repo, 'pkg', 'v1.0.0', required, ['does.not.exist'], [])!
		override_project('min-v-project-${index}', [], [])!
		store := os.join_path(override_test_root, 'min-v-store-${index}')
		test_utils.set_test_env(store)
		direct := cmd_fail_args(@LOCATION, [override_vpm_exe, 'install', repo])
		assert direct.output.contains('requires V ${required}'), direct.output
		assert !direct.output.contains('Scanning `does.not.exist`'), direct.output
		assert !os.exists(os.join_path(store, 'pkg'))
		mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
		address := listener.addr()!.str()
		server := spawn override_metadata_once(mut listener, repo)
		registered := cmd_fail_args(@LOCATION, [override_vpm_exe, '--server-url', 'http://${address}',
			'install', 'publisher.pkg'])
		server.wait()
		listener.close()!
		assert registered.output.contains('requires V ${required}'), registered.output
		assert !registered.output.contains('Scanning `does.not.exist`'), registered.output
		assert !os.exists(os.join_path(store, 'publisher', 'pkg'))
	}
}
