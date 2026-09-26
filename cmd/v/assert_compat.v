module main

import os
import v.pref

const v3_assert_compat_guard_env = 'V3_ASSERT_COMPAT_ROUTED'

// Keep the documented `-assert` modes working while the V3 driver only has
// function-level assert attributes. Route these invocations explicitly to the
// compatibility compiler instead of letting V3 reject `-assert` as unknown.
fn init() {
	if os.getenv(v3_assert_compat_guard_env) == '1' {
		return
	}
	args := merged_v_args()
	if '-old-compiler' in args || '-new-compiler' in args {
		return
	}
	if mode := v1_compat_assert_mode(args) {
		os.setenv(v3_assert_compat_guard_env, '1', true)
		launch_v1(clean_compiler_selection_flags(args),
			'`-assert ${mode}` requires the V ${v_version} compatibility compiler', RetryState{})
	}
}

// v1_compat_assert_mode returns a legacy assert mode only when it appears in
// the compiler-option prefix. Runtime arguments after a source/command must not
// accidentally reroute the compiler.
fn v1_compat_assert_mode(args []string) ?string {
	mut i := 0
	for i < args.len {
		arg := args[i]
		if arg == '-assert' {
			mode := args[i + 1] or { return none }
			if mode in ['aborts', 'backtraces', 'continues'] {
				return mode
			}
			return none
		}
		if arg == '-cf' || pref.option_may_consume_value(arg) {
			i += 2
			continue
		}
		if !arg.starts_with('-') {
			break
		}
		i++
	}
	return none
}
