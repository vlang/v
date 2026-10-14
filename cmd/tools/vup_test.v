import os
import v.skills
import v.util.cache_refresh

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
	mut root := os.join_path(os.vtmp_dir(), 'vup_report_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	// Match the checkout's physical path when macOS exposes /tmp through /private/tmp.
	root = os.real_path(root)
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

fn test_vup_refreshes_only_tinycc_modes_even_when_already_updated() ! {
	$if windows {
		return
	}
	root, home, tool, bin_dir := vup_report_fixture()!
	previous := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	defer {
		if value := previous { os.setenv('V3CACHE', value, true) } else { os.unsetenv('V3CACHE') }
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'vlib', 'crypto', 'sample', 'sample.v')
	os.mkdir_all(os.dir(source))!
	os.write_file(source, 'module sample\n')!
	for compiler in ['tcc', 'cc', 'clang', 'gcc'] {
		for prod in [false, true] {
			mut flags := ['-cc', compiler]
			if prod { flags << '-prod' }
			dir := os.join_path(cache_refresh.root_dir(root, os.join_path(root, 'v')), '${compiler}_${prod}')
			os.mkdir_all(dir)!
			for is_test in [false, true] {
				cache_refresh.record(dir, root, flags, is_test, {
					'sample': [source]
				}, [], '', false)!
			}
		}
	}
	cache_log := os.join_path(root, 'cache_calls.log')
	for name in ['cc', 'clang', 'gcc', 'tcc'] {
		write_executable(os.join_path(bin_dir, name), '#!/bin/sh\nexit 0\n')!
	}
	write_executable(os.join_path(root, 'v'), '#!/bin/sh\n' +
		'case "\$*" in\n  *vup_cache_refresh_*)\n' +
		'    printf "%s\\n" "\$*" >> ' + os.quoted_path(cache_log) + '\n' +
		'    echo "Caching module crypto.sample..."\n    exit 0;;\nesac\n' +
		'echo "V 0.5.2 abcdef0"\nexit 0\n')!
	// Cache refresh must run on both the early-return and compiler-rebuild paths.
	for options in [[]string{}, ['-skip_current'], ['-prod']] {
		os.write_file(cache_log, '')!
		result := run_vup(root, home, tool, bin_dir, ...options)!
		assert result.exit_code == 0, result.output
		assert result.output.count('Caching module crypto.sample...') == 2, result.output
		assert result.output.count('> Refreshing module cache (') == 2, result.output
		assert !result.output.contains('> Refreshing module cache ['), result.output
		assert result.output.contains('> Module cache refresh complete: 2 build configurations checked.'), result.output
		assert result.output.contains('> Refreshing module cache (normal program; -cc tcc).\nCaching module crypto.sample...'), result.output
		assert result.output.contains('> Refreshing module cache (normal tests; -cc tcc).\nCaching module crypto.sample...'), result.output
		// The production TinyCC records stay on disk from before the rule, and are not refreshed.
		calls := os.read_file(cache_log)!.split_into_lines()
		assert calls.len == 2, calls.str()
		assert calls.all(it.starts_with('-cc tcc ')), calls.str()
		assert calls.any(it.starts_with('-cc tcc -skip-running ')), calls.str()
		assert !calls.any(it.contains('-prod')), calls.str()
		assert !calls.any(it.contains(' run ')), calls.str()
	}
	// Warm caches still get validated on both update paths, without claiming any rebuilds.
	write_executable(os.join_path(root, 'v'), '#!/bin/sh\n' +
		'case "\$*" in\n  *vup_cache_refresh_*)\n' +
		'    printf "%s\\n" "\$*" >> ' + os.quoted_path(cache_log) + '\n' +
		'    exit 0;;\nesac\necho "V 0.5.2 abcdef0"\nexit 0\n')!
	for options in [[]string{}, ['-skip_current'], ['-prod']] {
		os.write_file(cache_log, '')!
		warm := run_vup(root, home, tool, bin_dir, ...options)!
		assert warm.exit_code == 0, warm.output
		assert !warm.output.contains('> Refreshing module cache '), warm.output
		assert !warm.output.contains('> Module cache refresh complete:'), warm.output
		assert os.read_file(cache_log)!.split_into_lines().len == 2
	}
	// Verbose updates retain the indexed validation report even when no modules rebuild.
	os.write_file(cache_log, '')!
	verbose := run_vup(root, home, tool, bin_dir, '-v')!
	assert verbose.exit_code == 0, verbose.output
	assert verbose.output.count('> Refreshing module cache [') == 2, verbose.output
	assert verbose.output.contains('> Refreshing module cache [1/2]'), verbose.output
	assert verbose.output.contains('> Refreshing module cache [2/2]'), verbose.output
	assert verbose.output.contains('> Module cache refresh complete: 2 build configurations checked.'), verbose.output
	assert os.read_file(cache_log)!.split_into_lines().len == 2
	// A failed refresh is visible and fails v up, while other modules still refresh.
	write_executable(os.join_path(root, 'v'), '#!/bin/sh\n' +
		'case "\$*" in\n  *vup_cache_refresh_*)\n' +
		'    printf "%s\\n" "\$*" >> ' + os.quoted_path(cache_log) + '\n' +
		'    exit 1;;\nesac\necho "V 0.5.2 abcdef0"\nexit 0\n')!
	os.write_file(cache_log, '')!
	failure := run_vup(root, home, tool, bin_dir)!
	assert failure.exit_code != 0, failure.output
	assert failure.output.contains('refreshing cached vlib modules failed'), failure.output
	assert !failure.output.contains('> Module cache refresh complete:'), failure.output
	assert os.read_file(cache_log)!.split_into_lines().len == 2
}

fn test_vup_resolves_relative_cache_paths_before_entering_checkout() ! {
	$if windows {
		return
	}
	root, home, tool, bin_dir := vup_report_fixture()!
	previous_cache := os.getenv_opt('V3CACHE')
	previous_tmp := os.getenv_opt('VTMP')
	previous_dir := os.getwd()
	defer {
		if value := previous_cache {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		if value := previous_tmp { os.setenv('VTMP', value, true) } else { os.unsetenv('VTMP') }
		os.chdir(previous_dir) or {}
		os.rmdir_all(root) or {}
	}
	project := os.join_path(root, 'project')
	os.mkdir_all(project)!
	source := os.join_path(root, 'vlib', 'crypto', 'sample', 'sample.v')
	os.mkdir_all(os.dir(source))!
	os.write_file(source, 'module sample\n')!
	log := os.join_path(root, 'cache_paths.log')
	for name in ['cc', 'clang', 'gcc', 'tcc'] {
		write_executable(os.join_path(bin_dir, name), '#!/bin/sh\nexit 0\n')!
	}
	write_executable(os.join_path(root, 'v'), '#!/bin/sh\n' +
		'case "\$*" in\n  *vup_cache_refresh_*)\n' +
		'    printf "%s\\n%s\\n" "\$V3CACHE" "\$VTMP" >> ' + os.quoted_path(log) + '\n' +
		'    echo "Caching module crypto.sample..."\n    exit 0;;\nesac\n' +
		'echo "V 0.5.2 abcdef0"\nexit 0\n')!
	for variable in ['V3CACHE', 'VTMP'] {
		os.chdir(project)!
		os.unsetenv('V3CACHE')
		os.setenv('VTMP', os.join_path(root, 'default_tmp'), true)
		os.setenv(variable, '.cache with spaces', true)
		dir := os.join_path(cache_refresh.root_dir(root, os.join_path(root, 'v')), variable)
		os.mkdir_all(dir)!
		cache_refresh.record(dir, root, ['-cc', 'tcc'], false, {
			'sample': [source]
		}, [], '', false)!
		os.write_file(log, '')!
		result := run_vup(root, home, tool, bin_dir)!
		assert result.exit_code == 0, result.output
		assert result.output.count('Caching module crypto.sample...') == 1, result.output
		paths := os.read_file(log)!.split_into_lines()
		assert paths.len == 2, paths.str()
		assert paths[if variable == 'V3CACHE' { 0 } else { 1 }] == os.join_path(project, '.cache with spaces'), paths.str()
		assert os.is_abs_path(paths[1]), paths.str()
	}
}

fn test_vup_recognizes_tinycc_paths_aliases_and_default_records() ! {
	$if windows {
		return
	}
	root, home, tool, bin_dir := vup_report_fixture()!
	previous := os.getenv_opt('V3CACHE')
	previous_dir := os.getwd()
	os.chdir(root)!
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	defer {
		if value := previous { os.setenv('V3CACHE', value, true) } else { os.unsetenv('V3CACHE') }
		os.chdir(previous_dir) or {}
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'vlib', 'crypto', 'sample', 'sample.v')
	os.mkdir_all(os.dir(source))!
	os.write_file(source, 'module sample\n')!
	bundled := os.join_path(root, 'thirdparty', 'tcc', 'tcc.exe')
	os.mkdir_all(os.dir(bundled))!
	write_executable(bundled, '#!/bin/sh\nexit 0\n')!
	alias := os.join_path(bin_dir, 'compiler-alias')
	os.symlink(bundled, alias)!
	write_executable(os.join_path(bin_dir, 'cc'), '#!/bin/sh\nexit 0\n')!
	renamed := os.join_path(bin_dir, 'my-tcc')
	write_executable(renamed, '#!/bin/sh\nexit 0\n')!
	copied := os.join_path(bin_dir, 'backend-copy')
	write_executable(copied, '#!/bin/sh\necho "Tiny C Compiler 0.9.27"\n')!
	wrapper := os.join_path(bin_dir, 'compiler-wrapper')
	probes := os.join_path(root, 'version_probes')
	write_executable(wrapper, '#!/bin/sh\necho probe >> ' + os.quoted_path(probes) +
		'\necho "tcc version 0.9.27"\n')!
	other := os.join_path(bin_dir, 'other-compiler')
	write_executable(other, '#!/bin/sh\necho "clang version 19.1.7"\n' +
		'echo "InstalledDir: /opt/tinycc-tcc version-compatible-sdk/bin"\n')!
	relative := os.path_rel(os.getwd(), bundled)!
	variants := [['-cc', 'tinyc'], ['-cc', bundled], ['-cc', alias], ['-cc', relative], []string{},
		['-cc', os.join_path(bin_dir, 'cc')], ['-cc'], ['-cc', renamed], ['-cc', copied],
		['-cc', wrapper], ['-cc', wrapper, '-prod'], ['-cc', other]]
	for index, flags in variants {
		dir := os.join_path(cache_refresh.root_dir(root, os.join_path(root, 'v')), 'variant_${index}')
		os.mkdir_all(dir)!
		cache_refresh.record(dir, root, flags, false, {
			'sample': [source]
		}, [], '', false)!
	}
	// A wrapper can select its backend from project-local configuration.
	context_wrapper := os.join_path(bin_dir, 'context-compiler')
	context_probes := os.join_path(root, 'context_probes')
	write_executable(context_wrapper, '#!/bin/sh\npwd >> ' + os.quoted_path(context_probes) +
		'\nif [ -f tinycc.cfg ]; then echo "tcc version 0.9.27"; else echo "clang version 18"; fi\n')!
	for context in ['tinycc_project', 'clang_project'] {
		project := os.join_path(root, context)
		os.mkdir_all(project)!
		if context == 'tinycc_project' { os.write_file(os.join_path(project, 'tinycc.cfg'), '')! }
		os.chdir(project)!
		dir := os.join_path(cache_refresh.root_dir(root, os.join_path(root, 'v')), context)
		os.mkdir_all(dir)!
		cache_refresh.record(dir, root, ['-cc', context_wrapper], false, {
			'sample': [source]
		}, [cache_refresh.FlagGroup{ flags: ['-I.'], identity: 'context' }], '', false)!
	}
	os.chdir(root)!
	log := os.join_path(root, 'cache_calls')
	write_executable(os.join_path(root, 'v'), '#!/bin/sh\n' +
		'case "\$*" in\n  *vup_cache_refresh_*)\n' +
		'    printf "%s\\n" "\$*" >> ' + os.quoted_path(log) + '\n    exit 0;;\nesac\n' +
		'echo "V 0.5.2 abcdef0"\nexit 0\n')!
	result := run_vup(root, home, tool, bin_dir)!
	assert result.exit_code == 0, result.output
	calls := os.read_file(log)!.split_into_lines()
	// The production wrapper resolves to TinyCC, so it is skipped with the other tcc+prod records.
	assert calls.len == 8, calls.str()
	assert calls.any(it.starts_with('-cc tinyc ')), calls.str()
	assert calls.any(it.starts_with('-cc ${bundled} ')), calls.str()
	assert calls.any(it.starts_with('-cc ${alias} ')), calls.str()
	assert calls.any(it.starts_with('-cc ${relative} ')), calls.str()
	assert calls.any(it.starts_with('-cc ${renamed} ')), calls.str()
	assert calls.any(it.starts_with('-cc ${copied} ')), calls.str()
	assert calls.any(it.starts_with('-cc ${wrapper} -skip-running ')), calls.str()
	assert !calls.any(it.starts_with('-cc ${wrapper} -prod ')), calls.str()
	assert calls.filter(it.starts_with('-cc ${context_wrapper} ')).len == 1, calls.str()
	assert !calls.any(it.contains('-cc ${os.join_path(bin_dir, 'cc')} ')), calls.str()
	assert !calls.any(it.contains('-cc ${other} ')), calls.str()
	// vup probes the wrapper once for its cache selection, and the refresh filter probes the
	// production record again before skipping it.
	assert os.read_file(probes)!.split_into_lines() == ['probe', 'probe']
	contexts := os.read_file(context_probes)!.split_into_lines()
	assert contexts.len == 2, contexts.str()
	assert os.join_path(root, 'tinycc_project') in contexts, contexts.str()
	assert os.join_path(root, 'clang_project') in contexts, contexts.str()
	assert !result.output.contains('> Refreshing module cache '), result.output
	assert !result.output.contains('> Module cache refresh complete:'), result.output
}

fn test_vup_rebuilds_changed_tinycc_module_and_reuses_unrelated_module() ! {
	$if windows {
		return
	}
	compiler := os.join_path(vroot, 'v')
	if !os.is_executable(os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')) { return }
	system_cc := os.find_abs_path_of_executable('cc') or { return }
	root, home, tool, bin_dir := vup_report_fixture()!
	name := 'vup_module_change_${os.getpid()}'
	modules := os.join_path(vroot, 'vlib', name)
	defer {
		os.rmdir_all(modules) or {}
		os.rmdir_all(root) or {}
	}
	for module_name in ['changed', 'unchanged'] {
		path := os.join_path(modules, module_name, 'value.v')
		os.mkdir_all(os.dir(path))!
		os.write_file(path, 'module ${module_name}\npub fn value() int { return 42 }\n')!
	}
	entry := os.join_path(root, 'main.v')
	os.write_file(entry, 'import ${name}.changed as _\nimport ${name}.unchanged as _\nfn main() {}\n')!
	cache := os.join_path(root, 'cache')
	environment := ['env', 'VEXE=${compiler}', 'V3CACHE=${cache}', 'V_SKIP_VVMRC=1']
	for cc in ['tcc', 'cc'] {
		seed := os.exec([...environment, compiler, '-cc', cc, '-gc', 'none', '-skip-running', '-o',
			os.join_path(root, 'program'), entry])
		assert seed.exit_code == 0, seed.output
	}
	up := [...environment, 'PATH=${bin_dir}${os.path_delimiter}${os.getenv('PATH')}', 'HOME=${home}',
		tool]
	// Publish current interfaces and objects before changing just one input.
	warm := os.exec(up)
	assert warm.exit_code == 0, warm.output
	os.write_file(os.join_path(modules, 'changed', 'value.v'),
		'module changed\npub fn value() int { return 43 }\n')!
	updated := os.exec(up)
	assert updated.exit_code == 0, updated.output
	assert updated.output.contains('Caching module ${name}.changed...'), updated.output
	assert !updated.output.contains('Caching module ${name}.unchanged...'), updated.output
	assert !updated.output.contains('-cc ${system_cc}'), updated.output
	assert !updated.output.contains('Caching module builtin...'), updated.output
	again := os.exec(up)
	assert again.exit_code == 0, again.output
	assert !again.output.contains('Caching module '), again.output
	assert !again.output.contains('> Refreshing module cache '), again.output
	assert !again.output.contains('> Module cache refresh complete:'), again.output
	// Linking still sees the changed function alongside the reused object.
	os.write_file(entry, 'import ${name}.changed\nimport ${name}.unchanged\n' +
		'fn main() { println(changed.value()); println(unchanged.value()) }\n')!
	result := os.exec([...environment, compiler, '-cc', 'tcc', '-gc', 'none', 'run', entry])
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == '43\n42', result.output
}

fn test_vup_collects_isolated_cache_for_the_compiler_instead_of_the_tool() ! {
	$if windows {
		return
	}
	root, home, tool, bin_dir := vup_report_fixture()!
	previous := os.environ()
	defer {
		for name in ['V3CACHE', 'VTMP', 'V3_TEST_ISOLATE_CACHE'] {
			if name in previous { os.setenv(name, previous[name], true) } else { os.unsetenv(name) }
		}
		os.rmdir_all(root) or {}
	}
	os.unsetenv('V3CACHE')
	os.setenv('VTMP', os.join_path(root, 'tmp'), true)
	os.setenv('V3_TEST_ISOLATE_CACHE', '1', true)
	compiler := os.join_path(root, 'v')
	cache := cache_refresh.root_dir(root, compiler)
	assert cache != cache_refresh.root_dir(root, tool)
	source := os.join_path(root, 'vlib', 'crypto', 'sample', 'sample.v')
	os.mkdir_all(os.dir(source))!
	os.write_file(source, 'module sample\n')!
	dir := os.join_path(cache, 'config')
	os.mkdir_all(dir)!
	cache_refresh.record(dir, root, ['-cc', 'tcc'], false, {
		'sample': [source]
	}, [], '', false)!
	for name in ['cc', 'clang', 'gcc', 'tcc'] {
		write_executable(os.join_path(bin_dir, name), '#!/bin/sh\nexit 0\n')!
	}
	write_executable(compiler, '#!/bin/sh\n' +
		'case "\$*" in\n  *vup_cache_refresh_*)\n' +
		'    echo "Caching module crypto.sample..."\n    exit 0;;\nesac\n' +
		'echo "V 0.5.2 abcdef0"\nexit 0\n')!
	fallback := os.join_path(root, 'v1_fallback')
	write_executable(fallback, '#!/bin/sh\necho fallback must not refresh >&2\nexit 1\n')!
	assert cache != cache_refresh.root_dir(root, fallback)
	for launcher in [compiler, fallback] {
		result := os.exec(['env', 'PATH=${bin_dir}', 'HOME=${home}', 'VEXE=${launcher}', tool])
		assert result.exit_code == 0, result.output
		assert result.output.count('Caching module crypto.sample...') == 1, result.output
	}
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
	for name in ['cc', 'clang', 'gcc', 'tcc'] {
		write_executable(os.join_path(bin_dir, name), '#!/bin/sh\nexit 0\n')!
	}
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
