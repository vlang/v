module driver

import os
import v.cmdexec

fn manifest_test_root(name string) !string {
	root := os.join_path(os.vtmp_dir(), '${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	return root
}

// test_manifest_is_added_once_to_an_executable_without_one links a program with the bundled
// tcc, which gives it no manifest of its own.
fn test_manifest_is_added_once_to_an_executable_without_one() ! {
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		eprintln('skipped: no bundled tcc at ${tcc}')
		return
	}
	root := manifest_test_root('v3_long_path_manifest')!
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'main.c')
	exe := os.join_path(root, 'main.exe')
	os.write_file(src, 'int main(void) { return 0; }\n')!
	res := cmdexec.run(tcc, ['-o', exe, src])
	assert res.exit_code == 0, res.output
	assert !windows_executable_has_manifest(exe)!
	before := os.read_bytes(exe)!
	assert add_windows_long_path_manifest(exe)!
	assert os.read_bytes(exe)! == before
	assert !windows_executable_has_manifest(exe)!
	assert os.read_file('${exe}.manifest')! == windows_long_path_manifest
	// A second call finds the sidecar and leaves both files alone.
	assert !add_windows_long_path_manifest(exe)!
	assert os.read_bytes(exe)! == before
	assert os.read_file('${exe}.manifest')! == windows_long_path_manifest
	assert cmdexec.run(exe, []).exit_code == 0
}

// test_existing_manifest_is_kept checks a system executable, which carries its own manifest.
fn test_existing_manifest_is_kept() ! {
	root := manifest_test_root('v3_long_path_manifest_kept')!
	defer {
		os.rmdir_all(root) or {}
	}
	exe := os.join_path(root, 'hostname.exe')
	os.cp(os.join_path(os.getenv('SystemRoot'), 'System32', 'hostname.exe'), exe)!
	assert windows_executable_has_manifest(exe)!
	before := os.read_bytes(exe)!
	assert !add_windows_long_path_manifest(exe)!
	assert os.read_bytes(exe)! == before
	assert !os.exists('${exe}.manifest')
}

fn test_msvc_removes_only_the_generated_sidecar() ! {
	root := manifest_test_root('v3_msvc_manifest')!
	defer { os.rmdir_all(root) or {} }
	exe := os.join_path(root, 'main.exe')
	os.write_file('${exe}.manifest', windows_long_path_manifest)!
	ensure_windows_long_path_manifest(exe, true, true)
	assert !os.exists('${exe}.manifest')
	os.write_file('${exe}.manifest', 'user manifest')!
	ensure_windows_long_path_manifest(exe, true, true)
	assert os.read_file('${exe}.manifest')! == 'user manifest'
}

// test_v_builds_get_the_manifest builds a program through the driver with the bundled tcc,
// whose linker adds no manifest, and checks the sidecar at the final output path.
fn test_v_builds_get_the_manifest() ! {
	root := manifest_test_root('v3_long_path_manifest_build')!
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	exe := os.join_path(root, 'hello_tcc.exe')
	for flags in [[]string{}, []string{}, ['-nocache']] {
		os.rm('${exe}.manifest') or {}
		res := cmdexec.run(@VEXE, ['-cc', 'tcc', ...flags, '-o', exe, src])
		assert res.exit_code == 0, res.output
		assert os.read_file('${exe}.manifest')! == windows_long_path_manifest
		assert cmdexec.run(exe, []).output.contains('Hello, World!')
	}
}

fn test_current_drive_rooted_output_gets_the_manifest() ! {
	root := manifest_test_root('v3_drive_rooted_manifest')!
	defer { os.rmdir_all(root) or {} }
	previous := os.getwd()
	os.chdir(root)!
	defer { os.chdir(previous) or {} }
	exe := os.join_path(root, 'rooted.exe').replace('/', '\\')
	assert exe.len > 2 && exe[1] == `:`
	rooted := exe[2..]
	src := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	res := cmdexec.run(@VEXE, ['-cc', 'tcc', '-o', rooted, src])
	assert res.exit_code == 0, res.output
	assert !windows_executable_has_manifest(rooted)!
	assert os.read_file('${exe}.manifest')! == windows_long_path_manifest
	assert cmdexec.run(exe, []).output.contains('Hello, World!')
}

fn test_side_by_side_manifest_at_final_output_is_kept() ! {
	root := manifest_test_root('v3_external_manifest_build')!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	exe := os.join_path(root, 'hello_external.exe')
	manifest := '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
' +
		'<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">' +
		'<dependency><dependentAssembly><assemblyIdentity type="win32" ' +
		'name="Microsoft.Windows.Common-Controls" version="6.0.0.0" ' +
		'processorArchitecture="*" publicKeyToken="6595b64144ccf1df" language="*"/>' +
		'</dependentAssembly></dependency></assembly>'
	os.write_file('${exe}.manifest', manifest)!
	for flags in [[]string{}, ['-nocache']] {
		res := cmdexec.run(@VEXE, ['-cc', 'tcc', ...flags, '-o', exe, src])
		assert res.exit_code == 0, res.output
		assert !windows_executable_has_manifest(exe)!
		assert os.read_file('${exe}.manifest')! == manifest
		assert cmdexec.run(exe, []).output.contains('Hello, World!')
	}
}
