module util

fn test_exec_with_timeout_preserves_literal_arguments() {
	result := exec_with_timeout([@VEXE, 'version'], 60_000) or { panic('timed out') }
	assert result.exit_code == 0, result.output
	assert result.output.contains('V ')
	empty := exec_with_timeout([]string{}, 60_000) or { panic('timed out') }
	assert empty.exit_code == -1
}
