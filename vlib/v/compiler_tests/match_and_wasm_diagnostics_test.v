import os
import v.cmdexec
import v.tempname

const diagnostic_vexe = @VEXE
const diagnostic_tests_dir = os.dir(@FILE)
const diagnostic_v3_dir = os.dir(diagnostic_tests_dir)
const diagnostic_v3_src = os.join_path(diagnostic_v3_dir, 'v.v')

fn wasm_diagnostic_compiler(workspace string) string {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3')
	if os.getenv('VFLAGS') == '' && os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(diagnostic_v3_src) {
		return os.real_path(shared)
	}
	v3_bin := os.join_path(workspace, 'compiler') + $if windows { '.exe' } $else { '' }
	build :=
		os.exec([diagnostic_vexe, '-gc', 'none', '-o', v3_bin, diagnostic_v3_src])
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_undefined_sumtype_match_variant_stops_before_codegen() {
	workspace := os.join_path(os.vtmp_dir(), 'v3_match_diagnostic_${tempname.unique_token()}')
	os.mkdir(workspace)!
	defer { os.rmdir_all(workspace) or {} }
	source_path := os.join_path(workspace, 'v3_undefined_sumtype_variant_${os.getpid()}.v')
	output_path := source_path.all_before_last('.')
	os.write_file(source_path,
		"type Value = int | string\n\nfn main() {\n\tvalue := Value(1)\n\tprintln(match value {\n\t\tNil { 'nil' }\n\t\telse { 'other' }\n\t})\n}\n")!
	result := cmdexec.run_in_merged_with_env(diagnostic_vexe, ['-nocache', '-b', 'c', '-o',
		output_path, source_path], '', {
		'VFLAGS': ''
	})
	assert result.exit_code != 0, result.output
	assert result.output.contains('`Value` has no variant `Nil`'), result.output
	assert !result.output.contains('C compilation failed'), result.output
	assert !result.output.contains('redict__Nil_str'), result.output
}

fn test_wasm_unsupported_aggregate_reports_source_error() {
	workspace := os.join_path(os.vtmp_dir(), 'v3_wasm_diagnostic_${tempname.unique_token()}')
	os.mkdir(workspace)!
	defer { os.rmdir_all(workspace) or {} }
	v3_bin := wasm_diagnostic_compiler(workspace)
	source_path := os.join_path(workspace, 'v3_wasm_dynamic_map_${os.getpid()}.v')
	output_path := source_path.all_before_last('.') + '.wasm'
	os.write_file(source_path,
		'fn main() {\n\tmut values := map[string]int{}\n\tvalues["one"] = 1\n}\n')!
	result :=
		os.exec([v3_bin, '-b', 'wasm', '-o', output_path, source_path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('v3_wasm_dynamic_map_${os.getpid()}.v:1:4:'), result.output
	assert result.output.contains('the V wasm backend does not support type `map[string]int` yet'), result.output

	assert !result.output.contains('panic:'), result.output
}
