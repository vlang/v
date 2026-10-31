module driver

// The manifest step plan is plain data, so these cases run on every host. The Windows-only
// tests in `long_path_manifest_windows_test.c.v` check that the planned files appear.

fn test_non_msvc_output_without_a_manifest_gets_a_sidecar() {
	assert windows_manifest_step(false, false, false, false) == .write_sidecar
}

fn test_user_sidecar_and_embedded_manifest_are_kept() {
	assert windows_manifest_step(false, true, false, false) == .keep_sidecar
	assert windows_manifest_step(false, true, true, false) == .keep_sidecar
	assert windows_manifest_step(false, false, false, true) == .keep_embedded
}

fn test_msvc_removes_only_the_generated_sidecar() {
	assert windows_manifest_step(true, true, true, false) == .remove_generated_sidecar
	assert windows_manifest_step(true, true, false, false) == .none
	assert windows_manifest_step(true, false, false, false) == .none
}

// The plan never asks to rewrite the executable: the only step that creates a file
// creates the sidecar, and none of the messages names a resource update.
fn test_every_step_that_changes_the_output_directory_is_named() {
	exe := 'out/app.exe'
	assert windows_manifest_step_message(.write_sidecar, exe) == '  > wrote out/app.exe.manifest (longPathAware; out/app.exe is not rewritten)'
	assert windows_manifest_step_message(.remove_generated_sidecar, exe).starts_with('  > removed out/app.exe.manifest')
	assert windows_manifest_step_message(.keep_sidecar, exe) == '  > kept out/app.exe.manifest'
	assert windows_manifest_step_message(.keep_embedded, exe) == '  > kept the manifest embedded in out/app.exe'
	assert windows_manifest_step_message(.none, exe) == ''
}
