import os

const optional_array_overwrite_vexe = @VEXE
const optional_array_overwrite_tests_dir = os.dir(@FILE)
const optional_array_overwrite_v3_dir = os.dir(optional_array_overwrite_tests_dir)
const optional_array_overwrite_vlib_dir = os.dir(optional_array_overwrite_v3_dir)
const optional_array_overwrite_v3_src = os.join_path(optional_array_overwrite_v3_dir, 'v3.v')

fn test_owned_optional_array_overwrite_wraps_cloned_payload() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_optional_array_overwrite_test')
	build :=
		os.execute('${optional_array_overwrite_vexe} -gc none -d ownership -path "${optional_array_overwrite_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${optional_array_overwrite_v3_src}')
	assert build.exit_code == 0, build.output

	source := os.join_path(os.temp_dir(), 'v3_optional_array_overwrite_input.v')
	os.write_file(source,
		"struct Holder {\nmut:\n\tvalue ?[]u8\n\tseparator_value ?[]u8\n}\n\nfn (mut holder Holder) set(value ?[]u8) {\n\tif bytes := value {\n\t\tholder.value = bytes.clone()\n\t} else {\n\t\tholder.value = none\n\t}\n}\n\nfn (mut holder Holder) separator(separator ?[]u8) {\n\tif sep := separator {\n\t\tholder.separator_value = sep.clone()\n\t} else {\n\t\tholder.separator_value = none\n\t}\n}\n\nfn main() {\n\tmut holder := Holder{}\n\tholder.set([u8(4), 2])\n\tassert holder.value? == [u8(4), 2]\n\tholder.separator([u8(5), 3])\n\tassert holder.separator_value? == [u8(5), 3]\n\tprintln('ok')\n}\n")!
	out := os.execute('${v3_bin} -ownership -d ownership -no-parallel run ${source}')
	assert out.exit_code == 0, out.output
	assert out.output.contains('\nok\n'), out.output
}
