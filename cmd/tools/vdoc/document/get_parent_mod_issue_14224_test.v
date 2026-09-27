module document

import os

fn test_get_parent_mod_stops_at_current_vmod_issue_14224() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'vdoc_get_parent_mod_issue_14224_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	project_dir := os.join_path(tmp_dir, 'project')
	os.mkdir_all(project_dir)!
	os.write_file(os.join_path(tmp_dir, 'test.v'), 'l := []fn')!
	os.write_file(os.join_path(project_dir, 'v.mod'), '')!
	os.write_file(os.join_path(project_dir, 'project.v'), 'module project')!
	assert get_parent_mod(project_dir)! == ''
}

fn test_lookup_module_with_path_finds_current_project_root_issue_9170() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'vdoc_lookup_module_issue_9170_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	project_dir := os.join_path(tmp_dir, 'issue9170')
	os.mkdir_all(project_dir)!
	os.write_file(os.join_path(project_dir, 'v.mod'), "Module {\n\tname: 'issue9170'\n}\n")!
	os.write_file(os.join_path(project_dir, 'main.v'), 'module main\n')!
	assert lookup_module_with_path('issue9170', project_dir)! == os.real_path(project_dir)
}

fn test_lookup_module_with_path_uses_vmod_source_root_issue_9170() {
	tmp_dir := os.join_path(os.vtmp_dir(),
		'vdoc_lookup_module_source_root_issue_9170_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	project_dir := os.join_path(tmp_dir, 'project_dir')
	source_root := os.join_path(project_dir, 'src')
	os.mkdir_all(source_root)!
	os.write_file(os.join_path(project_dir, 'v.mod'),
		"Module {\n\tname: 'issue9170'\n\tbase_url: 'src'\n}\n")!
	os.write_file(os.join_path(source_root, 'main.v'), 'module main\n')!
	assert lookup_module_with_path('issue9170', project_dir)! == os.real_path(source_root)
	assert lookup_module_with_path('issue9170', source_root)! == os.real_path(source_root)
}

fn test_lookup_module_with_path_stops_at_module_search_boundary() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'vdoc_lookup_boundary_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	parent_dir := os.join_path(tmp_dir, 'parent')
	project_dir := os.join_path(parent_dir, 'project')
	mod_name := 'vdoc_boundary_${os.getpid()}'
	mod_dir := os.join_path(parent_dir, mod_name)
	os.mkdir_all(project_dir)!
	os.mkdir_all(mod_dir)!
	os.write_file(os.join_path(mod_dir, 'module.v'), 'module ${mod_name}\n')!
	assert lookup_module_with_path(mod_name, project_dir)! == os.real_path(mod_dir)
	os.write_file(os.join_path(project_dir, '.v.mod.stop'), '')!
	if path := lookup_module_with_path(mod_name, project_dir) {
		assert false, 'vdoc found a module above the boundary: ${path}'
	} else {
		assert err.msg().contains('not found')
	}
}

fn test_module_parent_for_docs_stops_at_module_search_boundary() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'vdoc_module_name_boundary_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	parent_dir := os.join_path(tmp_dir, 'parent')
	project_dir := os.join_path(parent_dir, 'project')
	mod_dir := os.join_path(project_dir, 'foo')
	os.mkdir_all(mod_dir)!
	os.write_file(os.join_path(parent_dir, 'main.v'), 'module main\nfn main() {}\n')!
	os.write_file(os.join_path(mod_dir, 'foo.v'), 'module foo\npub fn value() int { return 1 }\n')!
	assert module_parent_for_docs(mod_dir, 'foo') == 'project'
	doc_before := generate(mod_dir, false, true, .auto) or { panic(err) }
	assert doc_before.head.name == 'project.foo'
	os.write_file(os.join_path(project_dir, '.v.mod.stop'), '')!
	assert module_parent_for_docs(mod_dir, 'foo') == ''
	doc_after := generate(mod_dir, false, true, .auto) or { panic(err) }
	assert doc_after.head.name == 'foo'
}

fn test_module_parent_for_docs_vlib_shortcut_stops_at_module_search_boundary() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'vdoc_vlib_module_name_boundary_${os.getpid()}')
	os.rmdir_all(tmp_dir) or {}
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	project_dir := os.join_path(tmp_dir, 'vlib', 'project')
	mod_dir := os.join_path(project_dir, 'foo')
	os.mkdir_all(mod_dir)!
	os.write_file(os.join_path(mod_dir, 'foo.v'), 'module foo\npub fn value() int { return 1 }\n')!
	assert module_parent_for_docs(mod_dir, 'foo') == 'project'
	os.write_file(os.join_path(project_dir, '.v.mod.stop'), '')!
	assert module_parent_for_docs(mod_dir, 'foo') == ''
	doc := generate(mod_dir, false, true, .auto) or { panic(err) }
	assert doc.head.name == 'foo'
}
