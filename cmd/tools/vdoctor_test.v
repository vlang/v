import os

// run_doctor_with_main compiles a copy of the doctor tool, that has `test_main` in place of
// its `main` function, and runs it. `test_main` can check everything that the tool declares.
fn run_doctor_with_main(name string, test_main string) ! {
	root := os.join_path(os.vtmp_dir(), 'vdoctor_${name}_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.read_file(os.join_path(@VMODROOT, 'cmd', 'tools', 'vdoctor.v'))!
	assert source.count('fn main() {') == 1
	path := os.join_path(root, 'doctor_${name}.v')
	os.write_file(path, source.replace('fn main() {', 'fn doctor_main() {') + test_main)!
	tool := os.join_path(root, 'doctor_${name}' + $if windows { '.exe' } $else { '' })
	build := os.exec([@VEXE, '-o', tool, path])
	assert build.exit_code == 0, build.output
	result := os.exec([tool])
	assert result.exit_code == 0, result.output
}

fn test_vdoctor_reports_unavailable_commands_without_hiding_tool_failures() {
	run_doctor_with_main('command_output', '
fn main() {
	if "success" in os.args {
		println("test tool version")
		println("second line")
		return
	}
	if "failure" in os.args {
		println("test tool failure")
		exit(2)
	}
	for code in [2, 3] {
		missing := os.Result{
			exit_code: code
			output: "exec failed (CreateProcess) with code " + code.str() + ": localized missing file or path message"
		}
		assert doctor_command_is_unavailable(missing, "windows")
		assert !doctor_command_is_unavailable(missing, "linux")
		assert !doctor_command_is_unavailable(os.Result{exit_code: code, output: "tool failure"}, "windows")
		assert !doctor_command_is_unavailable(os.Result{exit_code: code, output: "exec failed (CreatePipe): failure"}, "windows")
	}
	assert !doctor_command_is_unavailable(os.Result{exit_code: 5, output: "exec failed (CreateProcess) with code 5: access denied"}, "windows")
	assert doctor_command_is_unavailable(os.Result{exit_code: -1}, "linux")
	assert doctor_command_is_unavailable(os.Result{exit_code: 127}, "linux")
	assert doctor_command_is_unavailable(os.Result{exit_code: 1}, "windows")
	mut app := App{}
	assert app.cmd(command: [os.join_path(os.dir(os.executable()), "missing-doctor-test-tool.exe")]) == "N/A"
	assert app.cmd(command: [os.executable(), "success"]) == "test tool version"
	assert app.cmd(command: [os.executable(), "success"], line: 1) == "second line"
	assert app.cmd(command: [os.executable(), "success"], line: -1).replace("\\r\\n", "\\n") == "test tool version\\nsecond line\\n"
	assert app.cmd(command: [os.executable(), "failure"]).replace("\\r\\n", "\\n") == "Error: test tool failure\\n"
}
')!
}

fn test_vdoctor_reports_a_vlib_that_does_not_match_the_compiler() {
	run_doctor_with_main('vlib_report', '
fn main() {
	// The root that the compiler resolved for this program, holds the modules it is made of.
	assert os.is_dir(os.join_path(compiled_vroot, "vlib", "builtin"))

	assert diagnose_vlib_commit("9e70451", "9e70451") == "OK, value: 9e70451"
	assert diagnose_vlib_commit("9e70451", "cd7a898") == "MISMATCH: V was built from commit 9e70451, but this vlib is at commit cd7a898"
	assert diagnose_vlib_commit("", "cd7a898") == "N/A"
	assert diagnose_vlib_commit("9e70451", "") == "N/A"

	root := os.dir(os.executable())
	checkout := os.join_path(root, "checkout")
	os.mkdir_all(os.join_path(checkout, ".git")) or { panic(err) }
	os.mkdir_all(os.join_path(checkout, "vlib")) or { panic(err) }
	os.write_file(os.join_path(checkout, ".git", "HEAD"), "cd7a89860f0123456789abcdef0123456789abcd") or { panic(err) }
	elsewhere := os.join_path(root, "elsewhere")
	os.mkdir_all(elsewhere) or { panic(err) }
	checkout_vlib := os.join_path(checkout, "vlib")

	assert diagnose_vlib_dir(checkout, checkout) == "OK"
	assert diagnose_vlib_dir(os.join_path(checkout_vlib, ".."), checkout + os.path_separator) == "OK"
	assert diagnose_vlib_dir(checkout, elsewhere) == "NOT in the folder of the V executable"

	// A compiler in its own checkout, built from the commit that the checkout is at.
	// Uncommitted changes there do not make it a different commit.
	os.write_file(os.join_path(checkout_vlib, "uncommitted.v"), "module main") or { panic(err) }
	mut in_sync := App{}
	in_sync.report_vlib("V", checkout, checkout, "cd7a898")
	assert in_sync.report_lines.len == 2
	assert in_sync.report_lines[0].starts_with("|V vlib dir "), in_sync.report_lines[0]
	assert in_sync.report_lines[0].contains("OK"), in_sync.report_lines[0]
	assert in_sync.report_lines[0].contains(checkout_vlib), in_sync.report_lines[0]
	assert in_sync.report_lines[1].starts_with("|V vlib commit "), in_sync.report_lines[1]
	assert in_sync.report_lines[1].contains("OK, value: cd7a898"), in_sync.report_lines[1]

	// The same compiler, after its checkout moved to another commit without a rebuild,
	// or an executable that was copied into another checkout.
	mut other_commit := App{}
	other_commit.report_vlib("V", checkout, checkout, "9e70451")
	assert other_commit.report_lines[0].contains("OK"), other_commit.report_lines[0]
	assert other_commit.report_lines[1].contains("MISMATCH: V was built from commit 9e70451, but this vlib is at commit cd7a898"), other_commit.report_lines[1]

	// An executable that was copied out of its checkout keeps using the vlib there.
	mut copied := App{}
	copied.report_vlib("V", elsewhere, checkout, "cd7a898")
	assert copied.report_lines[0].contains("NOT in the folder of the V executable"), copied.report_lines[0]
	assert copied.report_lines[0].contains(checkout_vlib), copied.report_lines[0]
	assert copied.report_lines[1].contains("OK, value: cd7a898"), copied.report_lines[1]

	// Without a Git checkout, there is no commit to compare with.
	mut no_git := App{}
	no_git.report_vlib("V", elsewhere, elsewhere, "cd7a898")
	assert no_git.report_lines[0].contains("OK"), no_git.report_lines[0]
	assert no_git.report_lines[1].contains("N/A"), no_git.report_lines[1]

	// A working folder inside another V checkout: the sources there use the vlib of it.
	nested := os.join_path(checkout_vlib, "builtin", "linux_bare")
	os.mkdir_all(nested) or { panic(err) }
	assert is_same_dir(vroot_of(nested), checkout)
	assert is_same_dir(vroot_of(checkout), checkout)
	assert !is_same_dir(vroot_of(elsewhere), checkout)
	mut other_checkout := App{}
	other_checkout.report_vlib("cwd", elsewhere, vroot_of(nested), "9e70451")
	assert other_checkout.report_lines[0].starts_with("|cwd vlib dir "), other_checkout.report_lines[0]
	assert other_checkout.report_lines[0].contains("NOT in the folder of the V executable"), other_checkout.report_lines[0]
	assert other_checkout.report_lines[1].starts_with("|cwd vlib commit "), other_checkout.report_lines[1]
	assert other_checkout.report_lines[1].contains("MISMATCH: V was built from commit 9e70451, but this vlib is at commit cd7a898"), other_checkout.report_lines[1]
}
')!
}
