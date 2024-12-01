import os

const vdir = os.getenv_opt('VDIR') or { os.dir(os.getenv_opt('VEXE') or { os.getwd() }) }

fn test_main() {
	assert vdir.len > 0
}
