module vgit

import os

// vgit_test covers parsing/path helpers and offline bootstrap command fixtures.

fn test_line_to_timestamp_and_commit() {
	ts, commit := line_to_timestamp_and_commit('1561805697 5b7a1e84a4d283071d12cb86dc17aeda9b5306a8')
	assert ts == u64(1561805697)
	assert commit == '5b7a1e84a4d283071d12cb86dc17aeda9b5306a8'
}

fn test_line_to_timestamp_and_commit_reads_only_the_first_field() {
	// `git rev-list --timestamp` output is `<ts> <hash>`; a longer line keeps the
	// first field as the commit and drops the rest.
	ts, commit := line_to_timestamp_and_commit('100 a b c')
	assert ts == u64(100)
	assert commit == 'a'
}

// NOTE: a non numeric timestamp is silently coerced to 0 by string.u64(), it is
// not reported as an error.
fn test_line_to_timestamp_and_commit_coerces_unparsable_timestamp() {
	ts, commit := line_to_timestamp_and_commit('notanumber abc')
	assert ts == u64(0)
	assert commit == 'abc'
}

fn test_normalized_workpath_for_commit_names_a_worktree_after_the_commit() {
	assert normalized_workpath_for_commit('/tmp', '5b7a1e8').ends_with('v_at_5b7a1e8')
	assert normalized_workpath_for_commit('/tmp', 'master').ends_with('v_at_master')
}

fn test_normalized_workpath_for_commit_sanitizes_ref_characters() {
	// `^`, `-` and `/` are not valid inside a single path component on Windows.
	name := normalized_workpath_for_commit('/tmp', 'origin/feature-a^b')
	assert name.ends_with('${os.path_separator}v_at_origin_feature_a_b')
}

fn test_normalized_workpath_for_commit_resolves_the_base_dir() {
	workdir := os.join_path(os.vtmp_dir(), 'vgit_workpath_${os.getpid()}')
	os.mkdir_all(workdir)!
	defer {
		os.rmdir_all(workdir) or {}
	}
	normalized := normalized_workpath_for_commit(workdir, 'master')
	assert normalized.starts_with(os.real_path(workdir))
	assert normalized == os.real_path(workdir) + os.path_separator + 'v_at_master'
	assert os.is_dir(workdir)
}

fn test_normalized_workpath_for_commit_resolves_symlinks_before_the_child_exists() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vgit_workpath_symlink_${os.getpid()}')
	workdir := os.join_path(root, 'workdir')
	os.mkdir_all(workdir)!
	defer { os.rmdir_all(root) or {} }
	link := os.join_path(root, 'link')
	os.symlink(workdir, link)!
	child := os.join_path(workdir, 'v_at_master')
	assert !os.exists(child)
	assert normalized_workpath_for_commit(link, 'master') == os.join_path(os.real_path(workdir),
		'v_at_master')
	// Existing worktree symlinks must still resolve to their targets.
	target := os.join_path(root, 'target')
	os.mkdir_all(target)!
	os.symlink(target, child)!
	assert normalized_workpath_for_commit(link, 'master') == os.real_path(target)
}

fn test_check_v_commit_timestamp_before_self_rebuilding_returns_for_both_eras() {
	// The warning branch only prints; neither branch is allowed to exit.
	check_v_commit_timestamp_before_self_rebuilding(1561805697)
	check_v_commit_timestamp_before_self_rebuilding(0)
}

fn test_oldv_refreshes_tcc_with_gnu_make_before_selfbuilding() ! {
	$if windows {
		return
	}
	gnu_make := os.find_abs_path_of_executable('gmake') or {
		os.find_abs_path_of_executable('make') or { return }
	}
	if !os.exec([gnu_make, '--version']).output.contains('GNU Make') {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vgit GNU make ${os.getpid()}')
	bin := os.join_path(root, 'bin')
	checkout := os.join_path(root, 'v')
	vc := os.join_path(root, 'vc')
	for path in [bin, os.join_path(checkout, '.git'), os.join_path(checkout, 'cmd/v'),
		os.join_path(vc, '.git')] {
		os.mkdir_all(path)!
	}
	previous_dir := os.getwd()
	previous_path := os.getenv('PATH')
	previous_vflags := os.getenv_opt('VFLAGS')
	defer {
		os.chdir(previous_dir) or {}
		os.setenv('PATH', previous_path, true)
		if value := previous_vflags {
			os.setenv('VFLAGS', value, true)
		} else {
			os.unsetenv('VFLAGS')
		}
		os.rmdir_all(root) or {}
	}
	// V 0.5.2's portable Makefile has no fresh_tcc target. Native BSD make
	// selects it, whereas GNU make selects GNUmakefile and installs the bundle.
	os.write_file(os.join_path(checkout, 'Makefile'), 'all:\n\t@exit 0\n')!
	os.write_file(os.join_path(checkout, 'GNUmakefile'), "fresh_tcc:\n\t@mkdir -p thirdparty/tcc\n\t@printf '#!/bin/sh\\nexit 0\\n' > thirdparty/tcc/tcc.exe\n\t@chmod +x thirdparty/tcc/tcc.exe\n")!
	os.write_file(os.join_path(vc, 'v.c'), '// snapshot fixture\n')!
	// Keep git operations offline. The C compiler fixture represents a usable
	// snapshot whose self-build needs the TCC selected by the inherited VFLAGS.
	os.write_file(os.join_path(bin, 'git'), '#!/bin/sh\nif [ "$1" = rev-list ]; then printf "1700000000 abcdef1234567890\\n"; fi\n')!
	os.write_file(os.join_path(bin, 'cc'), r'#!/bin/sh
cat > cv <<"SNAPSHOT"
#!/bin/sh
[ "$VFLAGS" = "-cc tcc -no-retry-compilation" ] || exit 38
[ -x thirdparty/tcc/tcc.exe ] || exit 37
cp thirdparty/tcc/tcc.exe v
SNAPSHOT
chmod +x cv
')!
	os.write_file(os.join_path(bin, 'make'), '#!/bin/sh\nexec ${os.quoted_path(gnu_make)} -f Makefile "$@"\n')!
	os.write_file(os.join_path(bin, 'gmake'), '#!/bin/sh\nexec ${os.quoted_path(gnu_make)} "$@"\n')!
	for name in ['git', 'cc', 'make', 'gmake'] {
		os.chmod(os.join_path(bin, name), 0o755)!
	}
	os.setenv('PATH', bin + os.path_delimiter + previous_path, true)
	os.setenv('VFLAGS', '-cc tcc -no-retry-compilation', true)
	os.chdir(checkout)!
	broken := os.exec(['make', 'fresh_tcc'])
	assert broken.exit_code != 0, broken.output
	mut context := VGitContext{
		workdir:        root
		path_v:         checkout
		path_vc:        vc
		make_fresh_tcc: true
	}
	context.compile_oldv_if_needed()
	assert os.is_executable(os.join_path(checkout, 'cv'))
	assert os.is_executable(os.join_path(checkout, 'thirdparty/tcc/tcc.exe'))
	assert os.is_executable(context.vexepath)
}
