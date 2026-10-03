import os

const vexe = os.quoted_path(@VEXE)

const tfolder = os.join_path(os.vtmp_dir(), 'vclean_test')

fn test_clean_refuses_whitespace_basename_and_preserves_unrelated_file() {
	root := prepare_project('whitespace')!
	os.chdir(root)!
	source := os.join_path(root, ' foo.v')
	os.write_file(source, 'module main\nfn main() {}\n')!
	built := os.exec([@VEXE, source])
	assert built.exit_code == 0, built.output
	exe := os.join_path(root, 'foo' + exe_postfix())
	assert os.is_file(exe), built.output
	decoy := os.join_path(root, ' foo' + exe_postfix())
	os.write_file(decoy, 'unrelated')!
	res := os.exec([@VEXE, 'clean', source])
	assert res.exit_code == 1, res.output
	assert res.output.contains('cannot tell which executable'), res.output
	assert os.read_file(decoy)! == 'unrelated'
	assert os.is_file(exe)
	trailing := os.join_path(root, 'foo .v')
	os.write_file(trailing, 'module main\nfn main() {}\n')!
	refused := os.exec([@VEXE, 'clean', '${trailing}'])
	assert refused.exit_code == 1, refused.output
}

// prepare_project writes a one file project into a fresh folder under a name the
// tests can predict the executable of.
fn prepare_project(name string) !string {
	root := os.join_path(tfolder, name)
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	os.write_file(os.join_path(root, 'main.v'), "module main\n\nfn main() {\n\tprintln('hello')\n}\n")!
	return os.real_path(root)
}

// exe_postfix is what the compiler appends to an output name on this platform.
fn exe_postfix() string {
	return if os.user_os() == 'windows' { '.exe' } else { '' }
}

// default_exe is the executable a default build of `root` produces: named after
// the folder, with the platform suffix the compiler appends.
fn default_exe(root string) string {
	return os.join_path_single(root, os.file_name(root) + exe_postfix())
}

// build compiles the project at `root` the way a user would, so that the naming
// the tests assert against is the compiler's own and not a copy of it.
fn build(root string) ! {
	os.chdir(root)!
	res := os.exec([@VEXE, '.'])
	assert res.exit_code == 0, res.output
}

// test_clean_removes_what_the_compiler_wrote pins the naming rule against the
// compiler itself: the tool may only ever remove the executable a real build
// leaves behind, so a drift in either direction fails here.
fn test_clean_removes_what_the_compiler_wrote() {
	root := prepare_project('written')!
	build(root)!
	exe := default_exe(root)
	assert os.is_file(exe), 'the compiler did not produce ${exe}'
	res := os.exec([@VEXE, 'clean', '.'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('removed'), res.output
	assert !os.exists(exe), '${exe} is still there'
	assert os.is_file(os.join_path(root, 'main.v')), 'the source must survive'
}

// test_clean_accepts_a_single_source_file covers the other naming shape: the
// executable is named after the file, not after the folder holding it.
fn test_clean_accepts_a_single_source_file() {
	root := prepare_project('singlefile')!
	build(root)!
	// A second entry point beside the first, so a named file has its own output.
	os.write_file(os.join_path(root, 'tool.v'), "module main\n\nfn main() {\n\tprintln('tool')\n}\n")!
	res := os.exec([@VEXE, 'tool.v'])
	assert res.exit_code == 0, res.output
	exe := os.join_path_single(root, 'tool' + exe_postfix())
	assert os.is_file(exe), 'the compiler did not produce ${exe}'
	out := os.exec([@VEXE, 'clean', 'tool.v'])
	assert out.exit_code == 0, out.output
	assert !os.exists(exe), '${exe} is still there'
}

fn test_clean_dry_run_changes_nothing() {
	root := prepare_project('dryrun')!
	build(root)!
	exe := default_exe(root)
	res := os.exec([@VEXE, 'clean', '-n', '.'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('rm ${exe}'), res.output
	assert os.is_file(exe), '-n must not remove anything'
}

// test_clean_leaves_unrelated_files_alone is the safety property that matters
// most here: only the one predictable name is ever removed.
fn test_clean_leaves_unrelated_files_alone() {
	root := prepare_project('unrelated')!
	build(root)!
	decoy := os.join_path_single(root, 'other' + exe_postfix())
	os.write_file(decoy, 'not a build output')!
	notes := os.join_path(root, 'notes.txt')
	os.write_file(notes, 'notes')!
	res := os.exec([@VEXE, 'clean', '.'])
	assert res.exit_code == 0, res.output
	assert os.is_file(decoy), 'a file that is not the build output must survive'
	assert os.is_file(notes), 'an unrelated file must survive'
}

// test_clean_refuses_an_input_it_cannot_name makes sure an unclear path stops
// that path instead of deleting something guessed.
fn test_clean_refuses_an_input_it_cannot_name() {
	root := prepare_project('refused')!
	os.write_file(os.join_path(root, 'notes.txt'), 'notes')!
	os.chdir(root)!
	res := os.exec([@VEXE, 'clean', 'notes.txt'])
	assert res.exit_code == 1
	assert res.output.contains('cannot tell which executable'), res.output
	assert os.is_file(os.join_path(root, 'notes.txt'))
}

fn test_clean_reports_nothing_to_clean_for_an_unbuilt_project() {
	root := prepare_project('unbuilt')!
	os.chdir(root)!
	res := os.exec([@VEXE, 'clean', '.'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('nothing to clean for `.`'), res.output
}

fn test_clean_help_describes_the_flags() {
	res := os.exec([@VEXE, 'clean', '--help'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('Usage: v clean [options] [PATH...]'), res.output
	assert res.output.contains('dry-run'), res.output
	assert res.output.contains('verbose'), res.output
}

fn test_clean_symlink_source_preserves_the_alias_named_file() {
	$if windows {
		return
	}
	root := prepare_project('symlink')!
	os.chdir(root)!
	os.symlink('main.v', 'alias.v')!
	os.write_file('alias', 'unrelated file')!
	built := os.exec([@VEXE, 'alias.v'])
	assert built.exit_code == 0, built.output
	assert os.is_file('main'), 'the compiler must use the resolved source basename'
	res := os.exec([@VEXE, 'clean', 'alias.v'])
	assert res.exit_code == 0, res.output
	assert !os.exists('main')
	assert os.read_file('alias')! == 'unrelated file'
	assert os.is_link('alias.v')
}

fn test_clean_reports_failed_removal_and_continues_other_inputs() {
	$if windows {
		return
	}
	root := prepare_project('permission_failure')!
	build(root)!
	exe := default_exe(root)
	os.chmod(root, 0o555)!
	defer { os.chmod(root, 0o755) or {} }
	// Root and some filesystem configurations ignore directory mode bits.
	probe := os.join_path(root, 'permission_probe')
	mut permissions_enforced := false
	os.write_file(probe, '') or { permissions_enforced = true }
	if !permissions_enforced {
		os.rm(probe) or {}
		return
	}
	other := prepare_project('permission_other')!
	build(other)!
	res := os.exec([@VEXE, 'clean', root, '${other}'])
	assert res.exit_code != 0, res.output
	assert res.output.contains('cannot remove'), res.output
	assert os.is_file(exe)
	assert !os.exists(default_exe(other))
}
