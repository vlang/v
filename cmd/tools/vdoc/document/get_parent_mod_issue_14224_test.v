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
