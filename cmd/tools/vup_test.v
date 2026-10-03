import os
import v.skills

const vexe = @VEXE
const vroot = os.dir(vexe)

// skills_fixture builds a fake V checkout holding `bundled` skill bundles, plus
// an install directory to install them into. It returns the checkout, the
// install directory, and the root holding both, which the caller removes.
fn skills_fixture(bundled []string) ! (string, string, string) {
	root := os.join_path(os.vtmp_dir(), 'vup_skills_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'vroot', 'vlib', 'v', 'skills'))!
	fake_vroot := os.join_path(root, 'vroot')
	for name in bundled {
		dir := os.join_path(fake_vroot, 'vlib', 'v', 'skills', name)
		os.mkdir_all(os.join_path(dir, 'references'))!
		os.write_file(os.join_path_single(dir, skills.entry_file),
			'---\nname: ${name}\ndescription: Test the ${name} skill.\n---\n\n# ${name}\n')!
		os.write_file(os.join_path(dir, 'references', 'note.md'), 'note\n')!
	}
	install_dir := os.join_path(root, 'installed')
	os.mkdir_all(install_dir)!
	return fake_vroot, install_dir, root
}

// install copies the bundled `name` into `dir` through the real installer, so
// the test exercises the provenance that `v skills add` would leave behind.
fn install(fake_vroot string, dir string, name string) ! {
	skill := skills.find(fake_vroot, name) or {
		panic('no bundled skill called ${name}')
	}
	skills.install(skill, dir, skills.InstallOptions{})!
}

// rebundle rewrites the bundled `SKILL.md` of `name`, leaving the installation
// behind it.
fn rebundle(fake_vroot string, name string) ! {
	os.write_file(os.join_path_single(os.join_path(fake_vroot, 'vlib', 'v', 'skills', name),
		skills.entry_file),
		'---\nname: ${name}\ndescription: Test the ${name} skill.\n---\n\n# ${name}\n\nNew body.\n')!
}

fn test_refresh_candidates_splits_refreshable_from_held_back() ! {
	fake_vroot, dir, root := skills_fixture(['alpha', 'beta', 'gamma'])!
	defer {
		os.rmdir_all(root) or {}
	}
	install(fake_vroot, dir, 'alpha')!
	install(fake_vroot, dir, 'beta')!
	install(fake_vroot, dir, 'gamma')!
	rebundle(fake_vroot, 'alpha')!
	rebundle(fake_vroot, 'beta')!
	// A skill edited after it was installed cannot be refreshed without
	// discarding the edit, so it belongs with the ones that are held back.
	os.write_file(os.join_path(os.join_path_single(dir, 'beta'), skills.entry_file), 'edited\n')!

	refreshable, held_back := skills.refresh_candidates(fake_vroot, dir)
	assert refreshable == ['alpha'], refreshable.str()
	assert held_back == ['beta'], held_back.str()
	assert skills.origin_state(fake_vroot, dir, 'gamma') == .current
}

fn test_refresh_candidates_is_empty_when_everything_matches() ! {
	fake_vroot, dir, root := skills_fixture(['alpha'])!
	defer {
		os.rmdir_all(root) or {}
	}
	install(fake_vroot, dir, 'alpha')!
	refreshable, held_back := skills.refresh_candidates(fake_vroot, dir)
	assert refreshable.len == 0, refreshable.str()
	assert held_back.len == 0, held_back.str()
}

fn test_refresh_candidates_is_empty_for_an_empty_directory() ! {
	_, dir, root := skills_fixture([])!
	defer {
		os.rmdir_all(root) or {}
	}
	refreshable, held_back := skills.refresh_candidates(os.join_path(root, 'vroot'), dir)
	assert refreshable.len == 0, refreshable.str()
	assert held_back.len == 0, held_back.str()
}

fn test_refresh_candidates_holds_back_an_install_with_no_record() ! {
	fake_vroot, dir, root := skills_fixture(['alpha'])!
	defer {
		os.rmdir_all(root) or {}
	}
	install(fake_vroot, dir, 'alpha')!
	// An installation from before the record existed: nothing proves the files
	// are the ones that were installed, so it is not refreshable.
	os.rm(os.join_path_single(dir, skills.origin_file))!
	refreshable, held_back := skills.refresh_candidates(fake_vroot, dir)
	assert refreshable.len == 0, refreshable.str()
	assert held_back == ['alpha'], held_back.str()
}

fn test_vup_generates_windows_c_without_handle_type_errors() ! {
	test_root := os.join_path(os.vtmp_dir(), 'vup_windows_handles_${os.getpid()}')
	os.mkdir_all(test_root)!
	defer {
		os.rmdir_all(test_root) or {}
	}
	// A test compiled by the compatibility compiler must still exercise the
	// default compiler, which checks C macros as integers.
	compiler := if os.base(vexe) == 'v1_fallback.exe' {
		os.join_path(vroot, 'v.exe')
	} else if os.base(vexe) == 'v1_fallback' {
		os.join_path(vroot, 'v')
	} else {
		vexe
	}
	source := os.join_path(vroot, 'cmd', 'tools', 'vup.v')
	for cc in ['msvc', 'gcc', 'tcc'] {
		c_file := os.join_path(test_root, 'vup_${cc}.c')
		// Generating C exercises the Windows checker without needing a Windows SDK.
		result := os.exec([compiler, '-new-compiler', '-g', '-gc', 'none', '-nocache', '-os', 'windows',
			'-arch', 'amd64', '-cc', cc, '-o', c_file, source])
		assert result.exit_code == 0, result.output
		assert os.is_file(c_file)
	}
}

fn write_executable(path string, content string) ! {
	os.write_file(path, content)!
	os.chmod(path, 0o755)!
}

fn test_vup_checks_primary_compiler_when_built_by_v1_fallback() ! {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'vup_primary_compiler_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	os.mkdir_all(test_root)!
	defer {
		os.rmdir_all(test_root) or {}
	}

	tool := os.join_path(test_root, 'vup')
	build := os.exec([vexe, '-o', '${tool}', os.join_path(vroot, 'cmd', 'tools', 'vup.v')])
	assert build.exit_code == 0, build.output

	git_refs := os.join_path(test_root, '.git', 'refs', 'heads')
	os.mkdir_all(git_refs)!
	os.write_file(os.join_path(test_root, '.git', 'HEAD'), 'ref: refs/heads/master\n')!
	os.write_file(os.join_path(git_refs, 'master'), 'abcdef0123456789abcdef0123456789abcdef01\n')!

	bin_dir := os.join_path(test_root, 'bin')
	os.mkdir_all(bin_dir)!
	log_file := os.join_path(test_root, 'compiler.log')
	write_executable(os.join_path(test_root, 'v'), '#!/bin/sh\n' + 'printf "%s\\n" "\$*" >> ${os.quoted_path(log_file)}\n' + 'if [ "\$1" = "version" ]; then\n' + '  echo "V 0.5.2 abcdef0"\n' + '  exit 0\n' + 'fi\n' + 'exit 1\n')!
	write_executable(os.join_path(test_root, 'v1_fallback'), '#!/bin/sh\n' + 'printf "fallback %s\\n" "\$*" >> ${os.quoted_path(log_file)}\n' + 'exit 1\n')!
	write_executable(os.join_path(bin_dir, 'git'), '#!/bin/sh\n' + 'if [ "\$1" = "pull" ]; then\n' + '  echo "Already up to date."\n' + 'fi\n' + 'exit 0\n')!
	write_executable(os.join_path(bin_dir, 'make'), '#!/bin/sh\nexit 0\n')!
	write_executable(os.join_path(bin_dir, 'gmake'), '#!/bin/sh\nexit 0\n')!

	path := '${bin_dir}:${os.getenv('PATH')}'
	result := os.exec(['env', 'PATH=' + '${path}',
		'VEXE=' + '${os.join_path(test_root, 'v1_fallback')}', '${tool}'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('V is already updated.'), result.output
	compiler_calls := os.read_file(log_file)!
	assert !compiler_calls.contains('fallback'), compiler_calls
	assert compiler_calls.trim_space().split_into_lines() == ['version', 'version'], compiler_calls
}

fn test_vup_restores_missing_primary_compiler_when_built_by_v1_fallback() ! {
	$if windows {
		return
	}
	test_root := os.join_path(os.vtmp_dir(), 'vup_missing_primary_compiler_${os.getpid()}')
	os.rmdir_all(test_root) or {}
	os.mkdir_all(test_root)!
	defer {
		os.rmdir_all(test_root) or {}
	}

	// Give the test tool a known embedded hash so the fallback and checkout can
	// appear current while the primary compiler is missing.
	vup_source := os.read_file(os.join_path(vroot, 'cmd', 'tools', 'vup.v'))!
	assert vup_source.count('@VCURRENTHASH') == 1
	test_source := os.join_path(test_root, 'vup.v')
	os.write_file(test_source, vup_source.replace('@VCURRENTHASH', "'abcdef0'"))!
	tool := os.join_path(test_root, 'vup')
	build := os.exec([vexe, '-o', '${tool}', test_source])
	assert build.exit_code == 0, build.output

	git_refs := os.join_path(test_root, '.git', 'refs', 'heads')
	os.mkdir_all(git_refs)!
	os.write_file(os.join_path(test_root, '.git', 'HEAD'), 'ref: refs/heads/master\n')!
	os.write_file(os.join_path(git_refs, 'master'), 'abcdef0123456789abcdef0123456789abcdef01\n')!

	bin_dir := os.join_path(test_root, 'bin')
	os.mkdir_all(bin_dir)!
	make_log := os.join_path(test_root, 'make.log')
	fallback_log := os.join_path(test_root, 'fallback.log')
	write_executable(os.join_path(test_root, 'v1_fallback'), '#!/bin/sh\n' + 'printf "%s\\n" "\$*" >> ${os.quoted_path(fallback_log)}\n' + 'if [ "\$1" = "version" ]; then\n' + '  echo "V 0.5.2 abcdef0"\n' + '  exit 0\n' + 'fi\n' + 'exit 1\n')!
	write_executable(os.join_path(bin_dir, 'git'), '#!/bin/sh\n' + 'if [ "\$1" = "pull" ]; then\n' + '  echo "Already up to date."\n' + 'fi\n' + 'exit 0\n')!
	write_executable(os.join_path(bin_dir, 'make'), '#!/bin/sh\n' + 'printf "make:%s\\n" "\$*" >> ${os.quoted_path(make_log)}\n' + 'exit 0\n')!
	write_executable(os.join_path(bin_dir, 'gmake'), '#!/bin/sh\n' + 'printf "gmake:%s\\n" "\$*" >> ${os.quoted_path(make_log)}\n' + 'exit 0\n')!

	path := '${bin_dir}:${os.getenv('PATH')}'
	primary_vexe := os.join_path(os.real_path(test_root), 'v')
	result := os.exec(['env', 'PATH=' + '${path}',
		'VEXE=' + '${os.join_path(test_root, 'v1_fallback')}', '${tool}'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('`${primary_vexe}` is missing, trying `make` to restore it...'), result.output
	make_calls := os.read_file(make_log)!
	tcc_make_call := $if freebsd || openbsd || netbsd || dragonfly || solaris { 'gmake:latest_tcc' } $else { 'make:latest_tcc' }
	assert make_calls.trim_space().split_into_lines() == [tcc_make_call, 'make:'], make_calls
	assert !os.exists(fallback_log), os.read_file(fallback_log) or { '' }
}
