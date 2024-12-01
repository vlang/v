import os

const vdir = os.getenv_opt('VDIR') or { os.dir(os.getenv_opt('VEXE') or { os.getwd() }) }
const vdir2 = os.getenv_opt('NON_EXISTENT')
const vdir3 = os.getenv_opt('NON_EXISTENT') or { '' }

fn test_main() {
	assert vdir.len > 0
	assert vdir2 == none
	assert vdir3 == ''
}
