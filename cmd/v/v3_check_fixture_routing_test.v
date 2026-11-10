module main

import os

fn test_check_mode_fixtures_use_the_current_compiler() {
	for name in ['empty_global_const', 'v_import_const', 'v_tictactoe', 'v_tictactoe_fixed_syntax_error'] {
		source := os.join_path(@VEXEROOT, 'vlib', 'v', 'checker', 'tests', 'with_check_option',
			'${name}.vv')
		args := ['-check', source]
		assert v3_exact_output_fixture_args(args), source
		assert !v3_fixture_requires_compatibility_compiler(args), source
	}
}
