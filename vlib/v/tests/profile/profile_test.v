import os

const (
	vexe  = os.getenv('VEXE')
	vroot = os.dir(vexe)
)

fn test_vexe_exists() {
	assert vexe.len > 0
	assert os.is_file(vexe)
}

fn test_v_profile_works() {
	res := os.exec('"$vexe" -profile - run vlib/v/tests/profile/calling_http_get.v') or {
		panic(err)
	}
	// eprintln('res: $res')
	assert res.exit_code == 0
	assert res.output.len > 0
	// assert res.output.starts_with('net: socket error')
	assert res.output.contains(' main__main')
	assert res.output.contains(' os__init_os_args')
}
