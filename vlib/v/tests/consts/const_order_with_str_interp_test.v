import os

const exe_path = os.executable()
const exe_old_path = '${exe_path}.old'

fn test_main() {
	assert exe_path == os.executable()
	assert exe_old_path == '${os.executable()}.old'
}
