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
			assert os.is_file('manifest_name.v')
			assert (os.read_file('manifest_name.v') or { panic(err) }).starts_with('module manifest_name\n')
			assert os.is_file('tests/square_test.v')
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
