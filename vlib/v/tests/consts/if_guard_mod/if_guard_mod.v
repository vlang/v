module if_guard_mod

import os

fn opt_int(ok bool) ?int {
	if ok {
		return 11
	}
	return none
}

pub const env_value = if v := os.getenv_opt('V_ISSUE_28944_SURELY_UNSET_ENV_VAR') {
	v
} else {
	'module default'
}

pub const doubled = if v := opt_int(true) { v * 2 } else { 0 }
