module main

import os

@[export: 'v_environment_probe']
fn environment_probe() i32 {
	if os.getenv_opt('FENV_EMPTY') or { return 1 } != '' { return 2 }
	if os.getenv_opt('FENV_UNICODE') or { return 3 } != 'λ🙂' { return 4 }
	initial := os.environ()
	if 'FENV_EMPTY' !in initial || initial['FENV_EQUALS'] != 'left=right' { return 5 }
	if 'FENV_PREFIXQ' in initial || '' in initial || 'NO_EQUALS' in initial { return 6 }
	if os.setenv('FENV_UNICODE', 'ignored', false) != 0 { return 7 }
	if os.getenv('FENV_UNICODE') != 'λ🙂' { return 8 }
	if os.setenv('FENV_UNICODE', 'changed', true) != 0 { return 9 }
	if os.getenv('FENV_UNICODE') != 'changed' || os.environ()['FENV_UNICODE'] != 'changed' {
		return 10
	}
	if os.unsetenv('FENV_EMPTY') != 0 { return 11 }
	if _ := os.getenv_opt('FENV_EMPTY') { return 12 }
	if 'FENV_EMPTY' in os.environ() { return 13 }
	if initial['FENV_UNICODE'] != 'λ🙂' { return 14 }
	os.clearenv()
	if os.environ().len != 0 { return 15 }
	if _ := os.getenv_opt('FENV_EQUALS') { return 16 }
	if os.setenv('FENV_REUSED', 'after-clear', false) != 0 { return 17 }
	if os.getenv('FENV_REUSED') != 'after-clear' || os.environ().len != 1 { return 18 }
	return 0
}
