import os
import v.skills

const vexe = @VEXE
const vroot = os.dir(vexe)

// held_back_line and held_back_hint are what `v up` says about the skills it
// leaves alone, whether they were edited or have no install record.
const held_back_line = '> skills: left alone because they were edited locally or have no install record: '
const held_back_hint = '> skills: run `v skills update --global --dry-run` to see why, or `v skills update --global --force` to overwrite them'

// skills_fixture builds a fake V checkout holding `bundled` skill bundles, plus
// an install directory to install them into. It returns the checkout, the
// install directory, and the root holding both, which the caller removes.
fn skills_fixture(bundled []string) !(string, string, string) {
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

// vup_report_fixture builds a checkout `v up` can run against without a network:
// stand-ins for git and make, a compiler stub reporting one fixed revision, and
// an empty home directory for the skills. The checkout holds bundles for
// `alpha`, `beta` and `gamma`, so a test can install them and then decide which
// of them go stale and which are edited in place.
//
// It returns the checkout, the home directory, the built tool, and a PATH
// holding only the stand-ins.
fn vup_report_fixture() !(string, string, string, string) {
	root := os.join_path(os.vtmp_dir(), 'vup_report_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	home := os.join_path(root, 'home')
	os.mkdir_all(os.join_path(home, '.agents', 'skills')) or { panic(err) }
	for name in ['alpha', 'beta', 'gamma'] {
		dir := os.join_path(root, 'vlib', 'v', 'skills', name)
		os.mkdir_all(os.join_path(dir, 'references'))!
		os.write_file(os.join_path_single(dir, skills.entry_file),
			'---\nname: ${name}\ndescription: The ${name} skill.\n---\n\n# ${name}\n')!
		os.write_file(os.join_path(dir, 'references', 'note.md'), 'note\n')!
	}

	// The stub reports the revision the checkout claims to be at, so `v up` takes
	// the "already updated" path and returns without rebuilding anything. That is
	// the path a routine `v up` takes, and the one that reaches the report.
	git_refs := os.join_path(root, '.git', 'refs', 'heads')
	os.mkdir_all(git_refs)!
	os.write_file(os.join_path(root, '.git', 'HEAD'), 'ref: refs/heads/master\n')!
	os.write_file(os.join_path(git_refs, 'master'),
		'abcdef0123456789abcdef0123456789abcdef01\n')!

	bin_dir := os.join_path(root, 'bin')
	os.mkdir_all(bin_dir)!
	// Every stand-in records its call in `calls.log`, so a test can tell that an
	// update never started.
	log_call := 'printf "%s %s\\n" "\${0##*/}" "\$*" >> ' +
		os.quoted_path(os.join_path(root, 'calls.log')) + '\n'
	write_executable(os.join_path(bin_dir, 'git'), '#!/bin/sh\n' + log_call +
		'if [ "\$1" = "pull" ]; then\n  echo "Already up to date."\nfi\nexit 0\n')!
	write_executable(os.join_path(bin_dir, 'make'), '#!/bin/sh\n' + log_call + 'exit 0\n')!
	// The BSDs and Solaris refresh TCC with `gmake`, which has to be found on PATH.
	write_executable(os.join_path(bin_dir, 'gmake'), '#!/bin/sh\n' + log_call + 'exit 0\n')!
	// `v up -skills` reaches the refresh through the compiler, so the stub answers
	// it and records the call. The log is how a swallowed call would show up, and
	// the exit status is non-zero because it held a skill back, which must not make
	// the compiler update fail.
	write_executable(os.join_path(root, 'v'), '#!/bin/sh\n' + log_call +
		'if [ "\$1" = "skills" ]; then\n  printf "skills %s\\n" "\$*" >> ' +
		os.quoted_path(os.join_path(root, 'skills.log')) +
		'\n  echo "alpha: updated 2 file(s)"\n  echo "beta was edited" >&2\n  exit 1\nfi\n' +
		'echo "V 0.5.2 abcdef0"\nexit 0\n')!

	tool := os.join_path(root, 'vup')
	build := os.exec([vexe, '-o', '${tool}', os.join_path(vroot, 'cmd', 'tools', 'vup.v')])
	assert build.exit_code == 0, build.output
	return root, home, tool, bin_dir
}

// run_vup runs the built tool with only the stand-ins on PATH and `home` as the
// home directory the skills live in.
fn run_vup(root string, home string, tool string, bin_dir string, extra ...string) !os.Result {
	mut args := ['env', 'PATH=${bin_dir}', 'HOME=${home}', 'VEXE=' + os.join_path(root, 'v'), tool]
	args << extra
	return os.exec(args)
}

fn test_vup_rebuilds_for_transitive_dependencies_and_honors_rebuild_options() ! {
	$if windows {
		return
	}
	root, home, tool, bin_dir := vup_report_fixture()!
	defer {
		os.rmdir_all(root) or {}
	}
	git := os.find_abs_path_of_executable('git')!
	write_executable(os.join_path(bin_dir, 'cc'), '#!/bin/sh\nexit 0\n')!
	sources := os.join_path(root, 'source comparison')
	os.mkdir_all(sources)!
	paths := ['cmd/v/v.v', 'vlib/crypto/sha256/sha256.v', 'vlib/runtime/runtime.v', 'vlib/sync/sync.v',
		'vlib/json2/encode.v', 'thirdparty/headers/compiler.h', 'v.mod',
		'vlib/crypto/sha256/sha256_test.v', 'doc/docs.md']
	for path in paths {
		file := os.join_path(sources, path)
		os.mkdir_all(os.dir(file))!
		os.write_file(file, 'original\n')!
	}
	for arguments in [['init', '-q'], ['add', '.'], ['-c', 'user.name=V test', '-c',
		'user.email=test@example.invalid', 'commit', '-qm', 'fixture']] {
		mut command := [git, '-C', sources]
		command << arguments
		result := os.exec(command)
		assert result.exit_code == 0, result.output
	}
	revision := os.exec([git, '-C', sources, 'rev-parse', 'HEAD'])
	assert revision.exit_code == 0, revision.output
	hash := revision.output.trim_space()
	// Keep the fixture's checkout revision different from the compiler revision.
	os.write_file(os.join_path(root, '.git', 'refs', 'heads', 'master'),
		'0000000000000000000000000000000000000001\n')!
	log := os.join_path(root, 'calls.log')
	write_executable(os.join_path(root, 'v'), '#!/bin/sh\n' +
		'printf "v %s\\n" "\$*" >> ' + os.quoted_path(log) + '\n' +
		'echo "V 0.5.2 ${hash[..7]}"\nexit 0\n')!
	write_executable(os.join_path(bin_dir, 'git'), '#!/bin/sh\n' +
		'if [ "\$1" = "diff" ]; then\n  exec ' + os.quoted_path(git) + ' -C ' +
		os.quoted_path(sources) + ' "\$@"\nfi\nexit 0\n')!
	for path in paths {
		os.write_file(os.join_path(sources, path), 'changed\n')!
		os.write_file(log, '')!
		result := run_vup(root, home, tool, bin_dir)!
		assert result.exit_code == 0, '${path}: ${result.output}'
		calls := os.read_file(log)!
		should_rebuild := !path.ends_with('_test.v') && !path.ends_with('.md')
		assert calls.contains('v self\n') == should_rebuild, '${path}: ${calls}'
		assert result.output.contains('not recompiling V.') == !should_rebuild, result.output
		os.write_file(os.join_path(sources, path), 'original\n')!
	}
	for checkout_hash in ['0000000000000000000000000000000000000001', hash] {
		os.write_file(os.join_path(root, '.git', 'refs', 'heads', 'master'), checkout_hash + '\n')!
		for option in ['-prod', '-skip_current', '-skip_v_self'] {
			os.write_file(log, '')!
			result := run_vup(root, home, tool, bin_dir, option)!
			assert result.exit_code == 0, result.output
			assert !result.output.contains('not recompiling V.'), result.output
			assert !result.output.contains('V is already updated.'), result.output
			calls := os.read_file(log)!
			if option == '-skip_v_self' {
				assert calls.contains('make \n'), calls
			} else {
				assert calls.contains(if option == '-prod' { 'v -prod self\n' } else { 'v self\n' }), calls
			}
		}
	}
	os.write_file(os.join_path(root, '.git', 'refs', 'heads', 'master'),
		'0000000000000000000000000000000000000001\n')!
	// An unavailable revision must conservatively rebuild, rather than skip.
	os.rm(os.join_path(sources, '.git', 'HEAD'))!
	os.write_file(log, '')!
	result := run_vup(root, home, tool, bin_dir)!
	assert result.exit_code == 0, result.output
	assert os.read_file(log)!.contains('v self\n')
	// The filesystem revision is also needed before the same-revision shortcut.
	os.rm(os.join_path(root, '.git', 'HEAD'))!
	os.write_file(log, '')!
	unknown_checkout := run_vup(root, home, tool, bin_dir)!
	assert unknown_checkout.exit_code == 0, unknown_checkout.output
	assert !unknown_checkout.output.contains('V is already updated.'), unknown_checkout.output
	assert os.read_file(log)!.contains('v self\n')
}

fn test_vup_reports_both_groups_and_names_the_scope_to_use() ! {
	$if windows {
		return
	}
	root, home, tool, bin_dir := vup_report_fixture()!
	defer {
		os.rmdir_all(root) or {}
	}
	dir := os.join_path(home, '.agents', 'skills')
	install(root, dir, 'alpha')!
	install(root, dir, 'beta')!
	// alpha moved on upstream; beta was edited here.
	rebundle(root, 'alpha')!
	os.write_file(os.join_path(os.join_path_single(dir, 'beta'), skills.entry_file), 'edited\n')!

	result := run_vup(root, home, tool, bin_dir)!
	assert result.exit_code == 0, result.output
	assert result.output.contains('can be refreshed: alpha'), result.output
	assert result.output.contains('${held_back_line}beta\n'), result.output
	assert result.output.contains(held_back_hint), result.output
	// The report reads the home directory, so the command it suggests has to say so.
	assert result.output.contains('v skills update --global'), result.output
	// Reporting is not refreshing.
	assert !os.exists(os.join_path(root, 'skills.log')), os.read_file(os.join_path(root, 'skills.log')) or { '' }
}

fn test_vup_with_skills_forwards_the_refresh_and_still_names_what_it_held_back() ! {
	$if windows {
		return
	}
	root, home, tool, bin_dir := vup_report_fixture()!
	defer {
		os.rmdir_all(root) or {}
	}
	dir := os.join_path(home, '.agents', 'skills')
	install(root, dir, 'alpha')!
	install(root, dir, 'beta')!
	rebundle(root, 'alpha')!
	os.write_file(os.join_path(os.join_path_single(dir, 'beta'), skills.entry_file), 'edited\n')!

	result := run_vup(root, home, tool, bin_dir, '-skills')!
	// The stub exits non-zero because it held a skill back; the compiler update
	// still succeeded, so this has to exit 0 too.
	assert result.exit_code == 0, result.output
	log := os.read_file(os.join_path(root, 'skills.log')) or {
		panic('-skills did not reach `v skills`')
	}
	assert log.contains('skills update --global'), log
	// The child's own words reach the user rather than being captured and dropped.
	assert result.output.contains('alpha: updated 2 file(s)'), result.output
	// So do the ones it writes to standard error, which name what it held back.
	assert result.output.contains('beta was edited'), result.output
}

fn test_vup_with_skills_still_names_held_back_skills_when_there_is_nothing_to_refresh() ! {
	$if windows {
		return
	}
	root, home, tool, bin_dir := vup_report_fixture()!
	defer {
		os.rmdir_all(root) or {}
	}
	dir := os.join_path(home, '.agents', 'skills')
	install(root, dir, 'gamma')!
	// gamma was edited here and nothing is stale, so there is nothing to hand to
	// `v skills`: the report has to speak for itself or say nothing at all.
	os.write_file(os.join_path(os.join_path_single(dir, 'gamma'), skills.entry_file), 'edited\n')!

	result := run_vup(root, home, tool, bin_dir, '-skills')!
	assert result.exit_code == 0, result.output
	assert result.output.contains('${held_back_line}gamma\n'), result.output
	assert result.output.contains(held_back_hint), result.output
	// Nothing to refresh, so the compiler was never asked to.
	assert !os.exists(os.join_path(root, 'skills.log')), os.read_file(os.join_path(root, 'skills.log')) or { '' }
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

fn test_vup_help_lists_the_flags_without_touching_anything() ! {
	$if windows {
		return
	}
	root, home, tool, bin_dir := vup_report_fixture()!
	defer {
		os.rmdir_all(root) or {}
	}
	calls_log := os.join_path(root, 'calls.log')
	// `up` is how the launcher passes the command name along with the options.
	for help in [['-h'], ['-help'], ['--help'], ['help'], ['up', '-h']] {
		result := run_vup(root, home, tool, bin_dir, ...help)!
		assert result.exit_code == 0, '${help}: ${result.output}'
		// Every flag the tool reads is named, which is how `-skip_v_self` and
		// `-skip_current` stop being invisible.
		for flag in ['-v', '-prod', '-skills', '-skip_v_self', '-skip_current', '-h, -help, --help'] {
			assert result.output.contains(flag), '${help}: ${flag} missing from:\n${result.output}'
		}
		// Asking how to use the command must not run it.
		assert !os.exists(calls_log), '${help}: ' + (os.read_file(calls_log) or { '' })
	}
}

fn test_vup_help_survives_a_vexe_that_cannot_be_started() ! {
	$if windows {
		return
	}
	root, home, tool, bin_dir := vup_report_fixture()!
	defer {
		os.rmdir_all(root) or {}
	}
	calls_log := os.join_path(root, 'calls.log')
	// `VEXE` may legitimately be set, so a stale one must not turn `-h` into a
	// failure: help has to work even when the compiler a delegated lookup would
	// spawn is not there.
	result := os.exec(['env', 'PATH=${bin_dir}', 'HOME=${home}',
		'VEXE=' + os.join_path(root, 'does-not-exist'), tool, '-h'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('Usage: v up'), result.output
	assert !os.exists(calls_log), os.read_file(calls_log) or { '' }
}

fn test_vup_rejects_unknown_options_before_updating() ! {
	$if windows {
		return
	}
	root, home, tool, bin_dir := vup_report_fixture()!
	defer {
		os.rmdir_all(root) or {}
	}
	calls_log := os.join_path(root, 'calls.log')
	for unknown in ['--hlep', '-x', 'down'] {
		result := run_vup(root, home, tool, bin_dir, 'up', '-v', unknown)!
		assert result.exit_code == 1, '${unknown}: ${result.output}'
		assert result.output.contains('v up: unknown option: ${unknown}'), result.output
		assert result.output.contains('Usage: v up'), result.output
		// A mistyped flag must not pull and rebuild the compiler.
		assert !os.exists(calls_log), '${unknown}: ' + (os.read_file(calls_log) or { '' })
	}
	// The command name and the known options still reach the update, which also
	// shows that the stand-ins would have logged one.
	result := run_vup(root, home, tool, bin_dir, 'up', '-v')!
	assert result.exit_code == 0, result.output
	assert result.output.contains('V is already updated.'), result.output
	assert os.read_file(calls_log)!.contains('git pull'), result.output
}
