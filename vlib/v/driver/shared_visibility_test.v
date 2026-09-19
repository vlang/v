module driver

fn test_shared_flag_plan_hides_symbols_on_linux_and_macos() {
	for target_os in ['linux', 'macos'] {
		for is_prod in [false, true] {
			plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
				target_os:  target_os
				c_compiler: 'cc'
				is_shared:  true
				is_prod:    is_prod
			})
			assert '-shared' in plan.before_inputs
			assert '-fvisibility=hidden' in plan.before_inputs, '${target_os}, prod=${is_prod}'
		}
	}
}

fn test_live_shared_flag_plan_keeps_symbols_visible() {
	for target_os in ['linux', 'macos'] {
		plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
			target_os:     target_os
			c_compiler:    'cc'
			is_shared:     true
			is_liveshared: true
		})
		assert '-shared' in plan.before_inputs
		assert '-fvisibility=hidden' !in plan.before_inputs, target_os
	}
}

fn test_non_shared_flag_plan_does_not_hide_symbols() {
	for target_os in ['linux', 'macos', 'windows'] {
		for is_o in [false, true] {
			plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
				target_os:  target_os
				c_compiler: 'cc'
				is_o:       is_o
			})
			assert '-fvisibility=hidden' !in plan.before_inputs, '${target_os}, object=${is_o}'
		}
	}
}

fn test_windows_shared_flag_plan_does_not_add_unix_visibility() {
	plan := v3_c_compiler_flag_plan(V3CCompilerFlagOptions{
		target_os:  'windows'
		c_compiler: 'gcc'
		is_shared:  true
	})
	assert '-shared' in plan.before_inputs
	assert '-fvisibility=hidden' !in plan.before_inputs
}
