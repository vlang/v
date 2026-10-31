module skills

import os

fn test_swap_into_restores_the_old_directory_when_the_move_fails() {
	dir := os.join_path(os.vtmp_dir(), 'v_skills_restore_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	dest := os.join_path_single(dir, 'alpha')
	os.mkdir_all(dest)!
	os.write_file(os.join_path_single(dest, 'SKILL.md'), 'the old copy')!
	// A missing source fails after the previous installation has been moved aside.
	mut failed := false
	swap_into(os.join_path_single(dir, 'not-there'), dest) or { failed = true }
	assert failed
	assert os.read_file(os.join_path_single(dest, 'SKILL.md'))! == 'the old copy'
	assert os.ls(dir)! == ['alpha']
}

fn test_install_into_preserves_the_old_directory_when_staging_fails() {
	dir := os.join_path(os.vtmp_dir(), 'v_skills_staging_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	dest := os.join_path_single(dir, 'alpha')
	source := os.join_path_single(dir, 'source')
	os.mkdir_all(dest)!
	os.mkdir_all(source)!
	os.write_file(os.join_path_single(dest, 'SKILL.md'), 'the old copy')!
	os.write_file(os.join_path_single(source, 'SKILL.md'), 'the new copy')!
	mut failed := false
	install_into(dest, ['SKILL.md', 'missing.md'], source) or { failed = true }
	assert failed
	assert os.read_file(os.join_path_single(dest, 'SKILL.md'))! == 'the old copy'
	mut entries := os.ls(dir)!
	entries.sort()
	assert entries == ['alpha', 'source']
}
