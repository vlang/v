module scripting

fn test_array_execution_helpers() {
	result := exec_args([@VEXE, 'version'])!
	assert result.exit_code == 0, result.output
	assert result.output.contains('V ')
	assert run_args([@VEXE, 'version']).starts_with('V ')
	assert frun_args([@VEXE, 'version'])!.starts_with('V ')
	assert exit_0_status_args([@VEXE, 'version'])
	assert !exit_0_status_args([]string{})
	assert run_args([]string{}) == ''
	exec_args([]string{}) or {
		assert err.msg() == 'exec requires at least one argument'
		return
	}
	assert false
}
