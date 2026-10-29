import os

// SQLLEN is pointer-wide (8 bytes on 64-bit targets). The C generator copies an `&int`
// out-parameter through a 4-byte C `int`, which is right for SQLINTEGER and wrong for
// SQLLEN: the driver writes 8 bytes into the 4-byte temporary and V keeps only the low half.
fn test_mssql_sqllen_out_parameters_reach_the_driver_by_address() ! {
	root := os.join_path(os.vtmp_dir(), 'mssql_sqllen_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	main_v := os.join_path(root, 'main.v')
	out_c := os.join_path(root, 'out.c')
	os.write_file(main_v, 'import db.mssql\n\nfn main() {\n\t_ = mssql.Connection{}\n}\n')!
	res := os.execute('${os.quoted_path(@VEXE)} -no-skip-unused -o ${os.quoted_path(out_c)} ${os.quoted_path(main_v)}')
	assert res.exit_code == 0, res.output
	c_source := os.read_file(out_c)!
	mut found := 0
	for line in c_source.split_into_lines() {
		for callee in ['SQLRowCount(', 'SQLColAttribute('] {
			if line.contains(callee) && line.contains('hstmt') {
				found++
				assert !line.contains('_cabi_out_val'), line
			}
		}
	}
	assert found == 2, 'expected the SQLRowCount and SQLColAttribute calls, found ${found}'
}
