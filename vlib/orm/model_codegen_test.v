import os

const model_codegen_dir = os.join_path(os.vtmp_dir(), 'orm_model_codegen_${os.getpid()}')
const model_codegen_output = os.join_path(model_codegen_dir, 'models.c')

fn testsuite_begin() {
	os.mkdir_all(model_codegen_dir) or { panic(err) }
	fixture := os.join_path(@VMODROOT, 'vlib', 'orm', 'testdata', 'model_codegen.vv')
	res := os.exec([@VEXE, '-nocache', '-o', model_codegen_output, fixture])
	assert res.exit_code == 0, res.output
}

fn testsuite_end() {
	os.rmdir_all(model_codegen_dir) or {}
}

fn test_condition_parser_is_shared_across_models() {
	c := os.read_file(model_codegen_output)!
	// One declaration, one definition, and one scanner call in the shared parser.
	// Each additional model previously generated another scanner call site.
	count := c.count('orm__MyTextScanner__next_tok(')
	assert count > 0 && count <= 3, count.str()
}

fn test_scalar_model_fields_do_not_instantiate_relationship_loading() {
	c := os.read_file(model_codegen_output)!
	assert c.contains('orm__QueryBuilder_ScalarModel__map_row(')
	// At most a shared declaration and definition remain; scalar mappers need no call sites.
	assert c.count('orm__include_child_paths(') <= 2
	assert c.count('orm__relation_should_load(') <= 2
	assert !c.contains('orm__query_relation_optional_array_like_')
}
