import os

const vexe = os.getenv('VEXE')
const vroot = os.dir(vexe)

fn test_v_run_simple() {
	echo_os_args := os.join_path(vroot, 'cmd/tools/test_os_args.v')
	res123 := os.execute('${os.quoted_path(vexe)} run ${os.quoted_path(echo_os_args)} 1 2 3')
	println(res123)
	assert res123.exit_code == 0
	assert res123.output.starts_with("['1', '2', '3']")
}

fn test_v_run_quoted_args_with_spaces() {
	echo_os_args := os.join_path(vroot, 'cmd/tools/test_os_args.v')
	res := os.execute('${os.quoted_path(vexe)} run ${os.quoted_path(echo_os_args)} 1 "Learn V" 3')
	println(res)
	assert res.exit_code == 0
	assert res.output.starts_with("['1', 'Learn V', '3']")
}
