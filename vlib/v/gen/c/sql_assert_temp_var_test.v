import os

const sql_assert_temp_var_vexe = @VEXE

const sql_assert_temp_var_vroot = os.real_path(@VMODROOT)

const sql_assert_temp_var_testdata = os.join_path(sql_assert_temp_var_vroot, 'vlib/v/gen/c/testdata/assert_sql_select_sub_array_field.vv')

fn test_assert_with_inline_sql_select_compiles() {
	os.chdir(sql_assert_temp_var_vroot) or {}
	output_path := os.join_path(os.vtmp_dir(), 'assert_sql_select_sub_array_field.exe')
	defer {
		os.rm(output_path) or {}
	}
	cmd := '${os.quoted_path(sql_assert_temp_var_vexe)} -o ${os.quoted_path(output_path)} ${os.quoted_path(sql_assert_temp_var_testdata)}'
	compilation := os.execute(cmd)
	assert compilation.exit_code == 0, '${cmd}\n${compilation.output}'
}
