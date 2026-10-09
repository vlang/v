import os

fn test_vdoctor_reports_unavailable_commands_without_hiding_tool_failures() {
	root := os.join_path(os.vtmp_dir(), 'vdoctor_command_output_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.read_file(os.join_path(@VMODROOT, 'cmd', 'tools', 'vdoctor.v'))!
	assert source.count('fn main() {') == 1
	path := os.join_path(root, 'doctor_command_output.v')
	os.write_file(path, source.replace('fn main() {', 'fn doctor_main() {') + '
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
	tool := os.join_path(root, 'doctor_command_output' + $if windows { '.exe' } $else { '' })
	build := os.exec([@VEXE, '-o', tool, path])
	assert build.exit_code == 0, build.output
	result := os.exec([tool])
	assert result.exit_code == 0, result.output
}
