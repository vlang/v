import os

fn test_nested_project_manifest_precedes_enclosing_search_path() {
	for base_url in ['', 'src'] {
		root := os.join_path(os.vtmp_dir(), 'v3_nested_manifest_identity_${os.getpid()}_${base_url}')
		project := os.join_path(root, 'project')
		source_root := if base_url == '' { project } else { os.join_path(project, base_url) }
		source := os.join_path(source_root, 'foo', 'foo.v')
		dependency := os.join_path(source_root, 'net', 'foo', 'foo.v')
		os.mkdir_all(os.dir(source))!
		os.mkdir_all(os.dir(dependency))!
		defer { os.rmdir_all(root) or {} }
		os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'project', base_url: '${base_url}' }")!
		os.write_file(dependency, 'module foo\npub fn value() int { return 42 }\n')!
		search_path := '${root}|@vlib|@vmodules'
		for alias in ['', ' as other'] {
			name := if alias == '' { 'foo' } else { 'other' }
			os.write_file(source, 'module foo\nimport net.foo${alias}\npub fn answer() int { return ${name}.value() }\n')!
			result := os.exec([@VEXE, '-path', search_path, '-shared', '-check', source])
			if alias == '' {
				assert result.exit_code != 0, result.output
				assert result.output.contains('same name'), result.output
			} else {
				assert result.exit_code == 0, result.output
			}
		}
	}
}

fn test_initial_module_can_import_a_distinct_same_basename_module() {
	root := os.join_path(os.vtmp_dir(), 'v3_initial_import_identity_${os.getpid()}')
	source := os.join_path(root, 'app', 'html', 'html.v')
	dependency := os.join_path(root, 'net', 'html', 'html.v')
	os.mkdir_all(os.dir(source))!
	os.mkdir_all(os.dir(dependency))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'project' }")!
	os.write_file(os.join_path(os.dir(source), 'v.mod'), "Module { name: 'html' }")!
	os.write_file(dependency, 'module html\npub fn value() int { return 42 }\n')!
	for alias in ['', ' as other'] {
		name := if alias == '' { 'html' } else { 'other' }
		os.write_file(source, 'module html\nimport net.html${alias}\npub fn answer() int { return ${name}.value() }\n')!
		search_path := '${root}|@vlib|@vmodules'
		result := os.exec([@VEXE, '-path', search_path, '-shared', '-check', source])
		assert result.exit_code == 0, result.output
	}
}

fn test_initial_module_identity_does_not_suppress_an_aliased_dependency() {
	root := os.join_path(os.vtmp_dir(), 'v3_initial_declared_identity_${os.getpid()}')
	source := os.join_path(root, 'app', 'page.v')
	dependency := os.join_path(root, 'net', 'html', 'html.v')
	os.mkdir_all(os.dir(source))!
	os.mkdir_all(os.dir(dependency))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'project' }")!
	os.write_file(source, 'module html\nimport net.html as other\npub fn answer() int { return other.value() }\n')!
	os.write_file(dependency, 'module html\npub fn value() int { return 42 }\n')!
	result := os.exec([@VEXE, '-shared', '-check', source])
	assert result.exit_code == 0, result.output
}

fn test_global_import_does_not_take_the_identity_of_a_manifestless_ancestor() {
	root := os.join_path(os.vtmp_dir(), 'v3_ancestor_import_identity_${os.getpid()}')
	source := os.join_path(root, 'work', 'identity_project', 'layers', 'layer.v')
	global_root := os.join_path(root, 'global')
	dependency := os.join_path(global_root, 'identity_project', 'layers', 'layer.v')
	sibling := os.join_path(global_root, 'layers', 'layer.v')
	for path in [source, dependency, sibling] {
		os.mkdir_all(os.dir(path))!
	}
	defer { os.rmdir_all(root) or {} }
	os.write_file(source, 'module layers\nimport identity_project.layers as dependency\npub fn answer() int { return dependency.value() }\n')!
	os.write_file(dependency, 'module layers\npub fn value() int { return 42 }\n')!
	os.write_file(sibling, 'module layers\npub fn value() int { return 99 }\n')!
	search_path := '${global_root}|@vlib|@vmodules'
	result := os.exec([@VEXE, '-path', search_path, '-shared', '-check', source])
	assert result.exit_code == 0, result.output
}

fn test_resolved_ancestor_import_still_rejects_self_import() {
	root := os.join_path(os.vtmp_dir(), 'v3_resolved_self_import_${os.getpid()}')
	source := os.join_path(root, 'identity_project', 'layers', 'layer.v')
	os.mkdir_all(os.dir(source))!
	os.mkdir_all(os.join_path(root, 'layers'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'layers', 'layer.v'), 'module layers\n')!
	os.write_file(source, 'module layers\nimport identity_project.layers as own\npub fn value() int { return 42 }\n')!
	result := os.exec([@VEXE, '-shared', '-check', source])
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot import `identity_project.layers` into a module with the same name'), result.output
}

fn test_module_main_test_can_import_the_module_of_its_directory() {
	root := os.join_path(os.vtmp_dir(), 'v3_main_test_import_identity_${os.getpid()}')
	source := os.join_path(root, 'mymod', 'mymod.v')
	test_file := os.join_path(root, 'mymod', 'mymod_test.v')
	os.mkdir_all(os.dir(source))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(source, 'module mymod\npub fn value() int { return 42 }\n')!
	os.write_file(test_file, 'module main\nimport mymod\nfn test_value() { assert mymod.value() == 42 }\n')!
	search_path := '${root}|@vlib|@vmodules'
	result := os.exec([@VEXE, '-path', search_path, '-check', test_file])
	assert result.exit_code == 0, result.output
}
