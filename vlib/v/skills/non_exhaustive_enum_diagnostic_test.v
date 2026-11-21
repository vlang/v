import os

// SUMTYPES.md promises the missing-else diagnostic for both complete and
// partial matches on an external non-exhaustive enum.
fn test_non_exhaustive_enum_skill_diagnostic() {
	fixture := os.join_path(@VEXEROOT, 'vlib', 'v', 'checker', 'tests', 'modules',
		'non_exhaustive_enum_external_err', 'main.v')
	result := os.exec([@VEXE, '-check', fixture])
	assert result.exit_code != 0, result.output
	assert result.output.count('error: match on non-exhaustive `Status` from module `amod` must include `else {}`') == 2, result.output
	assert !result.output.contains('error: match must be exhaustive'), result.output
}
