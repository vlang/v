module parser

import os
import v2.ast
import v2.pref
import v2.token

fn test_c_fn_decl_allows_unnamed_params() {
	tmp_file := os.join_path(os.temp_dir(), 'v2_parser_c_fn_params_${os.getpid()}.v')
	os.write_file(tmp_file, 'module main\nfn C.wyhash(&u8, u64, u64, &u64) u64\nfn main() {}\n') or {
		panic(err)
	}
	defer {
		os.rm(tmp_file) or {}
	}

	p := &pref.Preferences{}
	mut file_set := token.FileSet.new()
	mut par := Parser.new(p)
	files := par.parse_files([tmp_file], mut file_set)

	assert files.len == 1
	assert files[0].stmts.len >= 2
	assert files[0].stmts[1] is ast.FnDecl

	fn_decl := files[0].stmts[1] as ast.FnDecl
	assert fn_decl.typ.params.len == 4
	assert fn_decl.typ.params[0].name == ''
	assert fn_decl.typ.params[1].name == ''
	assert fn_decl.typ.params[2].name == ''
	assert fn_decl.typ.params[3].name == ''
}
