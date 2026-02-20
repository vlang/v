import os

const vexe = @VEXE
const git_exe = os.find_abs_path_of_executable('git') or { '' }

fn test_check_md_respects_vcheckignore() {
	if git_exe == '' {
		eprintln('git is required for this test; skipping')
		return
	}
	original_wd := os.getwd()
	mut repo_dir := os.join_path(os.vtmp_dir(), 'vcheckignore_test_${os.getpid()}')
	os.rmdir_all(repo_dir) or {}
	os.mkdir_all(repo_dir)!
	defer {
		os.chdir(original_wd) or {}
		os.rmdir_all(repo_dir) or {}
	}
	os.execute_or_exit('${os.quoted_path(git_exe)} init ${os.quoted_path(repo_dir)}')

	write_text_file(os.join_path(repo_dir, 'README.md'), '# Root\n')!
	write_text_file(os.join_path(repo_dir, 'docs', 'cwd_skip.md'), '# CWD skip\n')!
	write_text_file(os.join_path(repo_dir, 'docs', 'skip.md'), '# Skip me\n')!
	write_text_file(os.join_path(repo_dir, 'docs', 'sub', 'skip2.md'), '# Skip me too\n')!
	write_text_file(os.join_path(repo_dir, 'notes', 'keep2.md'), '# Keep 2\n')!

	write_text_file(os.join_path(repo_dir, '.vcheckignore'), 'docs/skip.md\ndocs/sub/*.md\n')!
	write_text_file(os.join_path(repo_dir, 'docs', '.vcheckignore'), 'cwd_skip.md\n')!

	check_cmd := '${os.quoted_path(vexe)} check-md -hide-warnings -silent .'

	root_res := run_in_dir(repo_dir, check_cmd, true)!
	assert root_res.exit_code == 0, root_res.output
	assert root_res.output.contains('SKIP: docs/cwd_skip.md'), root_res.output
	assert root_res.output.contains('SKIP: docs/skip.md'), root_res.output
	assert root_res.output.contains('SKIP: docs/sub/skip2.md'), root_res.output
	assert root_res.output.contains('from docs/.vcheckignore: cwd_skip.md'), root_res.output
	assert root_res.output.contains('from .vcheckignore: docs/skip.md'), root_res.output
	assert root_res.output.contains('from .vcheckignore: docs/sub/*.md'), root_res.output
	assert root_res.output.contains('> Found: 2 .md files.'), root_res.output
	assert root_res.output.contains('Skipped by .vcheckignore: 3.'), root_res.output
	assert root_res.output.contains('Checked .md files: 2 |'), root_res.output
	root_non_verbose_res := run_in_dir(repo_dir, check_cmd, false)!
	assert root_non_verbose_res.exit_code == 0, root_non_verbose_res.output
	assert !root_non_verbose_res.output.contains('SKIP:'), root_non_verbose_res.output

	docs_res := run_in_dir(os.join_path(repo_dir, 'docs'), check_cmd, true)!
	assert docs_res.exit_code == 0, docs_res.output
	assert docs_res.output.contains('SKIP: docs/cwd_skip.md'), docs_res.output
	assert docs_res.output.contains('SKIP: docs/skip.md'), docs_res.output
	assert docs_res.output.contains('SKIP: docs/sub/skip2.md'), docs_res.output
	assert docs_res.output.contains('from docs/.vcheckignore: cwd_skip.md'), docs_res.output
	assert docs_res.output.contains('> Found: 0 .md files.'), docs_res.output
	assert docs_res.output.contains('Skipped by .vcheckignore: 3.'), docs_res.output
	assert docs_res.output.contains('Checked .md files: 0 |'), docs_res.output

	sub_res := run_in_dir(os.join_path(repo_dir, 'docs', 'sub'), check_cmd, true)!
	assert sub_res.exit_code == 0, sub_res.output
	assert sub_res.output.contains('SKIP: docs/sub/skip2.md'), sub_res.output
	assert !sub_res.output.contains('SKIP: docs/cwd_skip.md'), sub_res.output
	assert sub_res.output.contains('from .vcheckignore: docs/sub/*.md'), sub_res.output
	assert sub_res.output.contains('> Found: 0 .md files.'), sub_res.output
	assert sub_res.output.contains('Skipped by .vcheckignore: 1.'), sub_res.output
	assert sub_res.output.contains('Checked .md files: 0 |'), sub_res.output
}

fn test_check_md_respects_vcheckignore_glob_in_scanned_dir() {
	if git_exe == '' {
		eprintln('git is required for this test; skipping')
		return
	}
	original_wd := os.getwd()
	repo_dir := os.join_path(os.vtmp_dir(), 'vcheckignore_glob_${os.getpid()}')
	os.rmdir_all(repo_dir) or {}
	os.mkdir_all(repo_dir)!
	defer {
		os.chdir(original_wd) or {}
		os.rmdir_all(repo_dir) or {}
	}
	os.execute_or_exit('${os.quoted_path(git_exe)} init ${os.quoted_path(repo_dir)}')

	write_text_file(os.join_path(repo_dir, 'doc', 'plans', 'ignored1.md'), '# ignored\n')!
	write_text_file(os.join_path(repo_dir, 'doc', 'plans', 'ignored2.md'), '# ignored\n')!
	write_text_file(os.join_path(repo_dir, 'doc', 'plans', 'ignored3.md'), '# ignored\n')!
	write_text_file(os.join_path(repo_dir, 'doc', 'plans', 'keep1.md'), '# keep\n')!
	write_text_file(os.join_path(repo_dir, 'doc', 'plans', 'keep2.md'), '# keep\n')!
	write_text_file(os.join_path(repo_dir, 'doc', 'plans', 'keep3.md'), '# keep\n')!
	write_text_file(os.join_path(repo_dir, 'doc', 'plans', '.vcheckignore'), 'ignored*.md\n')!

	res := run_in_dir(repo_dir, '${os.quoted_path(vexe)} check-md -hide-warnings -silent doc/plans',
		true)!
	assert res.exit_code == 0, res.output
	assert res.output.contains('SKIP: doc/plans/ignored1.md'), res.output
	assert res.output.contains('SKIP: doc/plans/ignored2.md'), res.output
	assert res.output.contains('SKIP: doc/plans/ignored3.md'), res.output
	assert res.output.contains('from doc/plans/.vcheckignore: ignored*.md'), res.output
	assert res.output.contains('> Found: 3 .md files.'), res.output
	assert res.output.contains('Skipped by .vcheckignore: 3.'), res.output
	assert res.output.contains('Checked .md files: 3 |'), res.output
}

fn test_check_md_respects_vcheckignore_anchored_pattern() {
	if git_exe == '' {
		eprintln('git is required for this test; skipping')
		return
	}
	original_wd := os.getwd()
	repo_dir := os.join_path(os.vtmp_dir(), 'vcheckignore_anchored_${os.getpid()}')
	os.rmdir_all(repo_dir) or {}
	os.mkdir_all(repo_dir)!
	defer {
		os.chdir(original_wd) or {}
		os.rmdir_all(repo_dir) or {}
	}
	os.execute_or_exit('${os.quoted_path(git_exe)} init ${os.quoted_path(repo_dir)}')

	write_text_file(os.join_path(repo_dir, 'docs', 'root_only.md'), '# ignored by /root_only.md\n')!
	write_text_file(os.join_path(repo_dir, 'docs', 'sub', 'root_only.md'), '# should be kept\n')!
	write_text_file(os.join_path(repo_dir, 'docs', 'keep.md'), '# keep\n')!
	write_text_file(os.join_path(repo_dir, 'docs', '.vcheckignore'), '/root_only.md\n')!

	res := run_in_dir(repo_dir, '${os.quoted_path(vexe)} check-md -hide-warnings -silent docs',
		true)!
	assert res.exit_code == 0, res.output
	assert res.output.contains('SKIP: docs/root_only.md'), res.output
	assert res.output.contains('from docs/.vcheckignore: /root_only.md'), res.output
	assert !res.output.contains('SKIP: docs/sub/root_only.md'), res.output
	assert res.output.contains('> Found: 2 .md files.'), res.output
	assert res.output.contains('Skipped by .vcheckignore: 1.'), res.output
	assert res.output.contains('Checked .md files: 2 |'), res.output
}

fn test_check_md_respects_vcheckignore_anchored_directory_pattern() {
	if git_exe == '' {
		eprintln('git is required for this test; skipping')
		return
	}
	original_wd := os.getwd()
	repo_dir := os.join_path(os.vtmp_dir(), 'vcheckignore_anchored_dir_${os.getpid()}')
	os.rmdir_all(repo_dir) or {}
	os.mkdir_all(repo_dir)!
	defer {
		os.chdir(original_wd) or {}
		os.rmdir_all(repo_dir) or {}
	}
	os.execute_or_exit('${os.quoted_path(git_exe)} init ${os.quoted_path(repo_dir)}')

	write_text_file(os.join_path(repo_dir, 'docs', 'sub', 'ignored.md'), '# ignored by /sub/\n')!
	write_text_file(os.join_path(repo_dir, 'docs', 'nested', 'sub', 'keep.md'), '# kept\n')!
	write_text_file(os.join_path(repo_dir, 'docs', 'keep.md'), '# keep\n')!
	write_text_file(os.join_path(repo_dir, 'docs', '.vcheckignore'), '/sub/\n')!

	res := run_in_dir(repo_dir, '${os.quoted_path(vexe)} check-md -hide-warnings -silent docs',
		true)!
	assert res.exit_code == 0, res.output
	assert res.output.contains('SKIP: docs/sub/ignored.md'), res.output
	assert res.output.contains('from docs/.vcheckignore: /sub/'), res.output
	assert !res.output.contains('SKIP: docs/nested/sub/keep.md'), res.output
	assert res.output.contains('> Found: 2 .md files.'), res.output
	assert res.output.contains('Skipped by .vcheckignore: 1.'), res.output
	assert res.output.contains('Checked .md files: 2 |'), res.output
}

fn test_check_md_respects_vcheckignore_non_anchored_directory_pattern() {
	if git_exe == '' {
		eprintln('git is required for this test; skipping')
		return
	}
	original_wd := os.getwd()
	repo_dir := os.join_path(os.vtmp_dir(), 'vcheckignore_non_anchored_dir_${os.getpid()}')
	os.rmdir_all(repo_dir) or {}
	os.mkdir_all(repo_dir)!
	defer {
		os.chdir(original_wd) or {}
		os.rmdir_all(repo_dir) or {}
	}
	os.execute_or_exit('${os.quoted_path(git_exe)} init ${os.quoted_path(repo_dir)}')

	write_text_file(os.join_path(repo_dir, 'docs', 'sub', 'ignored1.md'), '# ignored 1\n')!
	write_text_file(os.join_path(repo_dir, 'docs', 'nested', 'sub', 'ignored2.md'), '# ignored 2\n')!
	write_text_file(os.join_path(repo_dir, 'docs', 'keep.md'), '# keep\n')!
	write_text_file(os.join_path(repo_dir, 'docs', '.vcheckignore'), 'sub/\n')!

	res := run_in_dir(repo_dir, '${os.quoted_path(vexe)} check-md -hide-warnings -silent docs',
		true)!
	assert res.exit_code == 0, res.output
	assert res.output.contains('SKIP: docs/sub/ignored1.md'), res.output
	assert res.output.contains('SKIP: docs/nested/sub/ignored2.md'), res.output
	assert res.output.contains('from docs/.vcheckignore: sub/'), res.output
	assert res.output.contains('> Found: 1 .md files.'), res.output
	assert res.output.contains('Skipped by .vcheckignore: 2.'), res.output
	assert res.output.contains('Checked .md files: 1 |'), res.output
}

fn test_check_md_respects_vcheckignore_anchored_directory_glob_pattern() {
	if git_exe == '' {
		eprintln('git is required for this test; skipping')
		return
	}
	original_wd := os.getwd()
	repo_dir := os.join_path(os.vtmp_dir(), 'vcheckignore_anchored_dir_glob_${os.getpid()}')
	os.rmdir_all(repo_dir) or {}
	os.mkdir_all(repo_dir)!
	defer {
		os.chdir(original_wd) or {}
		os.rmdir_all(repo_dir) or {}
	}
	os.execute_or_exit('${os.quoted_path(git_exe)} init ${os.quoted_path(repo_dir)}')

	write_text_file(os.join_path(repo_dir, 'docs-a', 'sub', 'ignored1.md'), '# ignored 1\n')!
	write_text_file(os.join_path(repo_dir, 'docs-b', 'sub', 'ignored2.md'), '# ignored 2\n')!
	write_text_file(os.join_path(repo_dir, 'other', 'keep.md'), '# keep\n')!
	write_text_file(os.join_path(repo_dir, '.vcheckignore'), '/docs-*/\n')!

	res := run_in_dir(repo_dir, '${os.quoted_path(vexe)} check-md -hide-warnings -silent .',
		true)!
	assert res.exit_code == 0, res.output
	assert res.output.contains('SKIP: docs-a/sub/ignored1.md'), res.output
	assert res.output.contains('SKIP: docs-b/sub/ignored2.md'), res.output
	assert res.output.contains('from .vcheckignore: /docs-*/'), res.output
	assert !res.output.contains('SKIP: other/keep.md'), res.output
	assert res.output.contains('> Found: 1 .md files.'), res.output
	assert res.output.contains('Skipped by .vcheckignore: 2.'), res.output
	assert res.output.contains('Checked .md files: 1 |'), res.output
}

fn test_check_md_respects_vcheckignore_comments_and_blank_lines() {
	if git_exe == '' {
		eprintln('git is required for this test; skipping')
		return
	}
	original_wd := os.getwd()
	repo_dir := os.join_path(os.vtmp_dir(), 'vcheckignore_comments_${os.getpid()}')
	os.rmdir_all(repo_dir) or {}
	os.mkdir_all(repo_dir)!
	defer {
		os.chdir(original_wd) or {}
		os.rmdir_all(repo_dir) or {}
	}
	os.execute_or_exit('${os.quoted_path(git_exe)} init ${os.quoted_path(repo_dir)}')

	write_text_file(os.join_path(repo_dir, 'docs', 'ignored.md'), '# ignored\n')!
	write_text_file(os.join_path(repo_dir, 'docs', 'ignored2.md'), '# ignored2\n')!
	write_text_file(os.join_path(repo_dir, 'docs', 'keep.md'), '# keep\n')!
	write_text_file(os.join_path(repo_dir, 'docs', '.vcheckignore'), '# comment\n\nignored.md # inline comment\nignored2.md\n')!

	res := run_in_dir(repo_dir, '${os.quoted_path(vexe)} check-md -hide-warnings -silent docs',
		true)!
	assert res.exit_code == 0, res.output
	assert res.output.contains('SKIP: docs/ignored.md'), res.output
	assert res.output.contains('SKIP: docs/ignored2.md'), res.output
	assert res.output.contains('from docs/.vcheckignore: ignored.md'), res.output
	assert res.output.contains('from docs/.vcheckignore: ignored2.md'), res.output
	assert !res.output.contains('SKIP: docs/keep.md'), res.output
	assert res.output.contains('> Found: 1 .md files.'), res.output
	assert res.output.contains('Skipped by .vcheckignore: 2.'), res.output
	assert res.output.contains('Checked .md files: 1 |'), res.output
}

fn test_check_md_file_argument_does_not_use_vcheckignore_directory_filtering() {
	if git_exe == '' {
		eprintln('git is required for this test; skipping')
		return
	}
	original_wd := os.getwd()
	repo_dir := os.join_path(os.vtmp_dir(), 'vcheckignore_file_argument_${os.getpid()}')
	os.rmdir_all(repo_dir) or {}
	os.mkdir_all(repo_dir)!
	defer {
		os.chdir(original_wd) or {}
		os.rmdir_all(repo_dir) or {}
	}
	os.execute_or_exit('${os.quoted_path(git_exe)} init ${os.quoted_path(repo_dir)}')

	write_text_file(os.join_path(repo_dir, 'docs', 'ignored.md'), '# ignored by dir scan\n')!
	write_text_file(os.join_path(repo_dir, 'docs', '.vcheckignore'), 'ignored.md\n')!

	res := run_in_dir(repo_dir, '${os.quoted_path(vexe)} check-md -hide-warnings -silent docs/ignored.md',
		true)!
	assert res.exit_code == 0, res.output
	assert !res.output.contains('SKIP: docs/ignored.md'), res.output
	assert res.output.contains('> Found: 1 .md files.'), res.output
	assert res.output.contains('Skipped by .vcheckignore: 0.'), res.output
	assert res.output.contains('Checked .md files: 1 |'), res.output
}

fn test_check_md_uses_scanned_dir_repo_root_for_vcheckignore() {
	if git_exe == '' {
		eprintln('git is required for this test; skipping')
		return
	}
	original_wd := os.getwd()
	base_dir := os.join_path(os.vtmp_dir(), 'vcheckignore_repo_root_${os.getpid()}')
	repo_a := os.join_path(base_dir, 'repo_a')
	repo_b := os.join_path(base_dir, 'repo_b')
	os.rmdir_all(base_dir) or {}
	os.mkdir_all(os.join_path(repo_b, 'docs'))!
	defer {
		os.chdir(original_wd) or {}
		os.rmdir_all(base_dir) or {}
	}
	os.execute_or_exit('${os.quoted_path(git_exe)} init ${os.quoted_path(repo_a)}')
	os.execute_or_exit('${os.quoted_path(git_exe)} init ${os.quoted_path(repo_b)}')

	write_text_file(os.join_path(base_dir, '.vcheckignore'), 'outside*.md\n')!
	write_text_file(os.join_path(repo_b, 'docs', 'outside1.md'), '# outside but should not be skipped\n')!
	write_text_file(os.join_path(repo_b, 'docs', 'keep1.md'), '# keep\n')!

	res := run_in_dir(repo_a, '${os.quoted_path(vexe)} check-md -hide-warnings -silent ${os.quoted_path(os.join_path(repo_b,
		'docs'))}', true)!
	assert res.exit_code == 0, res.output
	assert !res.output.contains('SKIP: '), res.output
	assert res.output.contains('> Found: 2 .md files.'), res.output
	assert res.output.contains('Skipped by .vcheckignore: 0.'), res.output
	assert res.output.contains('Checked .md files: 2 |'), res.output
}

fn test_check_md_multiple_directories_accumulate_skipped_count() {
	if git_exe == '' {
		eprintln('git is required for this test; skipping')
		return
	}
	original_wd := os.getwd()
	repo_dir := os.join_path(os.vtmp_dir(), 'vcheckignore_multi_dirs_${os.getpid()}')
	os.rmdir_all(repo_dir) or {}
	os.mkdir_all(repo_dir)!
	defer {
		os.chdir(original_wd) or {}
		os.rmdir_all(repo_dir) or {}
	}
	os.execute_or_exit('${os.quoted_path(git_exe)} init ${os.quoted_path(repo_dir)}')

	write_text_file(os.join_path(repo_dir, 'docs', 'ignored.md'), '# ignored\n')!
	write_text_file(os.join_path(repo_dir, 'docs', 'keep.md'), '# keep\n')!
	write_text_file(os.join_path(repo_dir, 'notes', 'ignored.md'), '# ignored\n')!
	write_text_file(os.join_path(repo_dir, 'notes', 'keep.md'), '# keep\n')!
	write_text_file(os.join_path(repo_dir, '.vcheckignore'), 'docs/ignored.md\nnotes/ignored.md\n')!

	res := run_in_dir(repo_dir, '${os.quoted_path(vexe)} check-md -hide-warnings -silent docs notes',
		true)!
	assert res.exit_code == 0, res.output
	assert res.output.contains('SKIP: docs/ignored.md'), res.output
	assert res.output.contains('SKIP: notes/ignored.md'), res.output
	assert res.output.contains('> Found: 2 .md files.'), res.output
	assert res.output.contains('Skipped by .vcheckignore: 2.'), res.output
	assert res.output.contains('Checked .md files: 2 |'), res.output
}

fn run_in_dir(path string, cmd string, verbose bool) !os.Result {
	original_wd := os.getwd()
	os.chdir(path)!
	if verbose {
		os.setenv('VERBOSE', '1', true)
	} else {
		os.unsetenv('VERBOSE')
	}
	res := os.execute(cmd)
	os.unsetenv('VERBOSE')
	os.chdir(original_wd)!
	return res
}

fn write_text_file(path string, content string) ! {
	os.mkdir_all(os.dir(path))!
	os.write_file(path, content)!
}
