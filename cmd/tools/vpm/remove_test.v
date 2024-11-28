// vtest retry: 3
import os
import rand
import test_utils

const vexe = os.quoted_path(@VEXE)
const test_path = os.join_path(os.vtmp_dir(), 'vpm_remove_test_${rand.ulid()}')

fn testsuite_begin() {
	$if !network ? {
		eprintln('> skipping ${@FILE}, when `-d network` is missing')
		exit(0)
	}
	test_utils.set_test_env(test_path)
}

fn testsuite_end() {
	os.rmdir_all(test_path) or {}
}

fn test_remove() {
	os.execute_or_exit('${vexe} install https://github.com/hungrybluedev/xlsx')
	mod_path := os.join_path(test_path, 'xlsx')
	assert os.is_dir(mod_path)
	res := os.execute('${vexe} remove xlsx')
	assert !os.exists(mod_path)
}
