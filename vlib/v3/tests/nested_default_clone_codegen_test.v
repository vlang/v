import os

const nested_clone_vexe = @VEXE
const nested_clone_tests_dir = os.dir(@FILE)
const nested_clone_v3_dir = os.dir(nested_clone_tests_dir)
const nested_clone_vlib_dir = os.dir(nested_clone_v3_dir)
const nested_clone_v3_src = os.join_path(nested_clone_v3_dir, 'v3.v')

fn test_nested_marker_only_iclone_uses_structural_clone() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_nested_default_clone_test')
	build :=
		os.execute('${nested_clone_vexe} -gc none -d ownership -path "${nested_clone_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${nested_clone_v3_src}')
	assert build.exit_code == 0, build.output

	source := os.join_path(os.temp_dir(), 'v3_nested_default_clone_input.v')
	os.write_file(source,
		"interface Drop {\nmut:\n\tdrop()\n}\n\nstruct Inner implements IClone {\n\ttext string\n}\n\nstruct Outer implements IClone {\n\tinner Inner\n}\n\nstruct DropClone implements IClone, Drop {\nmut:\n\ttext string\n}\n\nstruct Token implements IClone {\nmut:\n\tpatterns []Tokens\n}\n\nstruct Tokens implements IClone {\nmut:\n\ttokens []Token\n}\n\nfn (mut value DropClone) drop() {\n\tunsafe { value.text.free() }\n\tvalue.text = ''\n}\n\nfn (value &DropClone) clone() DropClone {\n\treturn DropClone{\n\t\ttext: value.text.clone()\n\t}\n}\n\nfn (mut tokens Tokens) push(token Token) {\n\ttokens.tokens << token\n}\n\nfn clone_in_specialization[T](_ T, original Outer) Outer {\n\treturn original.clone()\n}\n\nfn main() {\n\toriginal := Outer{\n\t\tinner: Inner{\n\t\t\ttext: 'kept'\n\t\t}\n\t}\n\tcloned := clone_in_specialization(1, original)\n\tassert cloned.inner.text == 'kept'\n\towned := DropClone{\n\t\ttext: 'dropped safely'\n\t}\n\towned_clone := owned.clone()\n\tassert owned_clone.text == 'dropped safely'\n\tmut recursive := Tokens{\n\t\ttokens: [Token{\n\t\t\tpatterns: [Tokens{\n\t\t\t\ttokens: [Token{}]\n\t\t\t}]\n\t\t}]\n\t}\n\trecursive_clone := recursive.clone()\n\trecursive.tokens[0].patterns[0].push(Token{})\n\tassert recursive.tokens[0].patterns[0].tokens.len == 2\n\tassert recursive_clone.tokens[0].patterns[0].tokens.len == 1\n\tmut appended := Tokens{}\n\tappended.push(recursive.tokens[0].clone())\n\trecursive.tokens[0].patterns[0].push(Token{})\n\tassert recursive.tokens[0].patterns[0].tokens.len == 3\n\tassert appended.tokens[0].patterns[0].tokens.len == 2\n\tprintln('ok')\n}\n")!
	out := os.execute('${v3_bin} -ownership -d ownership -nocache -no-parallel run ${source}')
	assert out.exit_code == 0, out.output
	assert out.output.contains('\nok\n'), out.output
}
