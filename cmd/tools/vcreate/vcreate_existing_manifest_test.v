// vtest build: !windows
import os

fn test_init_uses_existing_manifest_name_and_preserves_project_files() {
	root := os.join_path(os.vtmp_dir(), 'vcreate_existing_manifest_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	old_wd := os.getwd()
	defer {
		os.chdir(old_wd) or {}
	}
	manifest := "Module { name: 'manifest_name' version: '1.2.3' }\n"
	for library in [false, true] {
		folder := os.join_path(root, if library {
			'different_library_folder'
		} else {
			'different_bin_folder'
		})
		os.mkdir_all(folder) or { panic(err) }
		os.chdir(folder) or { panic(err) }
		os.write_file('v.mod', manifest) or { panic(err) }
		existing_main := 'module main\n\nfn main() { println("keep me") }\n'
		if !library {
			os.write_file('main.v', existing_main) or { panic(err) }
		}
		flag := if library { ' --lib' } else { '' }
		command := '${os.quoted_path(@VEXE)} -new-compiler -no-retry-compilation -cc clang -gc none init${flag} < /dev/null'
		result := os.exec(['sh', '-c', command])
		assert result.exit_code == 0, result.output
		assert result.output.contains('project `manifest_name`'), result.output
		assert os.read_file('v.mod') or { panic(err) } == manifest
		assert (os.read_file('.gitignore') or { panic(err) }).split_into_lines().contains('manifest_name')
		if library {
			assert os.is_file('manifest_name/manifest_name.v')
			assert (os.read_file('manifest_name/manifest_name.v') or { panic(err) }).starts_with('module manifest_name\n')
			assert os.is_file('tests/square_test.v')
			tests := os.exec(['sh', '-c',
				'${os.quoted_path(@VEXE)} -new-compiler -no-retry-compilation -cc clang -gc none test . < /dev/null'])
			assert tests.exit_code == 0, tests.output
		} else {
			assert os.read_file('main.v') or { panic(err) } == existing_main
		}
		files_before := os.walk_ext('.', '.v').map(os.read_file(it) or { panic(err) })
		repeated := os.exec(['sh', '-c', command])
		assert repeated.exit_code == 0, repeated.output
		assert os.read_file('v.mod') or { panic(err) } == manifest
		assert os.walk_ext('.', '.v').map(os.read_file(it) or { panic(err) }) == files_before
	}
}

fn test_init_falls_back_from_empty_or_invalid_manifest_names() {
	root := os.join_path(os.vtmp_dir(), 'vcreate_manifest_fallback_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	old_wd := os.getwd()
	defer {
		os.chdir(old_wd) or {}
	}
	for i, manifest in ["Module { name: '' }\n", 'not a module\n'] {
		folder := os.join_path(root, 'fallback-project-${i}')
		name := 'fallback_project_${i}'
		os.mkdir_all(folder) or { panic(err) }
		os.chdir(folder) or { panic(err) }
		os.write_file('v.mod', manifest) or { panic(err) }
		command := '${os.quoted_path(@VEXE)} -new-compiler -no-retry-compilation -cc clang -gc none init --lib < /dev/null'
		result := os.exec(['sh', '-c', command])
		assert result.exit_code == 0, result.output
		assert result.output.contains('project `${name}`'), result.output
		assert os.read_file('v.mod') or { panic(err) } == manifest
		assert (os.read_file('${name}/${name}.v') or { panic(err) }).starts_with('module ${name}\n')
		assert (os.read_file('.gitignore') or { panic(err) }).split_into_lines().contains(name)
		tests := os.exec(['sh', '-c',
			'${os.quoted_path(@VEXE)} -new-compiler -no-retry-compilation -cc clang -gc none test . < /dev/null'])
		assert tests.exit_code == 0, tests.output
	}
}

fn test_init_normalizes_hyphenated_library_identifier_without_changing_manifest() {
	folder := os.join_path(os.vtmp_dir(), 'vcreate_hyphenated_library_${os.getpid()}')
	os.mkdir_all(folder) or { panic(err) }
	defer {
		os.rmdir_all(folder) or {}
	}
	old_wd := os.getwd()
	os.chdir(folder) or { panic(err) }
	defer {
		os.chdir(old_wd) or {}
	}
	manifest := "Module { name: 'foo-bar' version: '1.2.3' }\n"
	os.write_file('v.mod', manifest) or { panic(err) }
	strict := '${os.quoted_path(@VEXE)} -new-compiler -no-retry-compilation -cc clang -gc none'
	result := os.exec(['sh', '-c', '${strict} init --lib < /dev/null'])
	assert result.exit_code == 0, result.output
	assert result.output.contains('project `foo-bar`'), result.output
	assert os.read_file('v.mod') or { panic(err) } == manifest
	assert (os.read_file('.gitignore') or { panic(err) }).split_into_lines().contains('foo-bar')
	assert !os.exists('foo-bar.v')
	assert (os.read_file('foo_bar/foo_bar.v') or { panic(err) }).starts_with('module foo_bar\n')
	assert (os.read_file('tests/square_test.v') or { panic(err) }).contains('import foo_bar\n')
	// Compile and run the generated import, not just the library declaration.
	tests := os.exec(['sh', '-c', '${strict} test . < /dev/null'])
	assert tests.exit_code == 0, tests.output
	assert os.read_file('v.mod') or { panic(err) } == manifest
}
