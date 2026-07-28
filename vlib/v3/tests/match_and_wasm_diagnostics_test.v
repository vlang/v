import os

const diagnostic_vexe = @VEXE
const diagnostic_tests_dir = os.dir(@FILE)
const diagnostic_v3_dir = os.dir(diagnostic_tests_dir)
const diagnostic_v3_src = os.join_path(diagnostic_v3_dir, 'v3.v')
const diagnostic_v3_bin = os.join_path(os.temp_dir(),
	'v3_match_and_wasm_diagnostics_${os.getpid()}')

fn build_diagnostic_v3() string {
	if os.is_executable(diagnostic_v3_bin) {
		return diagnostic_v3_bin
	}
	build :=
		os.execute('${os.quoted_path(diagnostic_vexe)} -gc none -o ${os.quoted_path(diagnostic_v3_bin)} ${os.quoted_path(diagnostic_v3_src)}')
	assert build.exit_code == 0, build.output
	return diagnostic_v3_bin
}

fn test_undefined_sumtype_match_variant_stops_before_codegen() {
	v3_bin := build_diagnostic_v3()
	source_path := os.join_path(os.temp_dir(), 'v3_undefined_sumtype_variant_${os.getpid()}.v')
	output_path := os.join_path(os.temp_dir(), 'v3_undefined_sumtype_variant_${os.getpid()}')
	os.write_file(source_path,
		"type Value = int | string\n\nfn main() {\n\tvalue := Value(1)\n\tprintln(match value {\n\t\tNil { 'nil' }\n\t\telse { 'other' }\n\t})\n}\n")!
	result :=
		os.execute('${os.quoted_path(v3_bin)} -nocache -b c -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('`Value` has no variant `Nil`'), result.output
	assert !result.output.contains('C compilation failed'), result.output
	assert !result.output.contains('redict__Nil_str'), result.output
}

fn test_wasm_unsupported_aggregate_reports_source_error() {
	v3_bin := build_diagnostic_v3()
	source_path := os.join_path(os.temp_dir(), 'v3_wasm_dynamic_map_${os.getpid()}.v')
	output_path := os.join_path(os.temp_dir(), 'v3_wasm_dynamic_map_${os.getpid()}.wasm')
	os.write_file(source_path,
		'fn main() {\n\tmut values := map[string]int{}\n\tvalues["one"] = 1\n}\n')!
	result :=
		os.execute('${os.quoted_path(v3_bin)} -b wasm -o ${os.quoted_path(output_path)} ${os.quoted_path(source_path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('v3_wasm_dynamic_map_${os.getpid()}.v:1:4:'), result.output
	assert result.output.contains('the V3 wasm backend does not support type `map[string]int` yet'), result.output

	assert !result.output.contains('panic:'), result.output
}
