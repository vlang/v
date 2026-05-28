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
