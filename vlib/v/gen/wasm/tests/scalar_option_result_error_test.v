import os

// A scalar `?T`/`!T` return type used to abort the wasm backend with an internal
// `get_wasm_type: unreachable type ... UnknownTypeInfo` ICE, because the
// option/result guards only ran inside the MultiReturn arm / after the type was
// already lowered. The backend must instead emit the intended, located
// "not implemented" user error.
fn compile_wasm(src string, name string) os.Result {
	vexe := os.quoted_path(@VEXE)
	wrkdir := os.join_path(os.vtmp_dir(), 'wasm_scalar_optres_tests')
	os.mkdir_all(wrkdir) or { panic(err) }
	source_path := os.join_path(wrkdir, '${name}.v')
	output_path := os.join_path(wrkdir, '${name}.wasm')
	os.write_file(source_path, src) or { panic(err) }
	return os.execute('${vexe} -b wasm -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
}

fn test_scalar_option_return_errors_cleanly() {
	res := compile_wasm('fn f() ?int {\n\treturn 3\n}\n\nfn main() {\n\tx := f() or { 0 }\n\t_ = x\n}\n',
		'scalar_option')
	assert res.exit_code != 0, 'expected a compile error, got: ${res.output}'
	assert res.output.contains('option types are not implemented'), res.output
	assert !res.output.contains('get_wasm_type: unreachable'), 'leaked the internal ICE: ${res.output}'
}

fn test_scalar_result_return_errors_cleanly() {
	res := compile_wasm('fn f() !int {\n\treturn 3\n}\n\nfn main() {\n\tx := f() or { 0 }\n\t_ = x\n}\n',
		'scalar_result')
	assert res.exit_code != 0, 'expected a compile error, got: ${res.output}'
	assert res.output.contains('result types are not implemented'), res.output
	assert !res.output.contains('get_wasm_type: unreachable'), 'leaked the internal ICE: ${res.output}'
}
