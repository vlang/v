// vtest ownership
import os
import v.cmdexec
import v.tempname

fn test_nested_marker_only_iclone_uses_structural_clone() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	env := {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	}
	root := os.join_path(os.vtmp_dir(), 'v3_nested_default_clone_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_nested_default_clone_input.v')
	os.write_file(source,
		"interface Drop {\nmut:\n\tdrop()\n}\n\nstruct Inner implements IClone {\n\ttext string\n}\n\nstruct Outer implements IClone {\n\tinner Inner\n}\n\nstruct DropClone implements IClone, Drop {\nmut:\n\ttext string\n}\n\nstruct Token implements IClone {\nmut:\n\tpatterns []Tokens\n}\n\nstruct Tokens implements IClone {\nmut:\n\ttokens []Token\n}\n\nfn (mut value DropClone) drop() {\n\tunsafe { value.text.free() }\n\tvalue.text = ''\n}\n\nfn (value &DropClone) clone() DropClone {\n\treturn DropClone{\n\t\ttext: value.text.clone()\n\t}\n}\n\nfn (mut tokens Tokens) push(token Token) {\n\ttokens.tokens << token\n}\n\nfn clone_in_specialization[T](_ T, original Outer) Outer {\n\treturn original.clone()\n}\n\nfn main() {\n\toriginal := Outer{\n\t\tinner: Inner{\n\t\t\ttext: 'kept'\n\t\t}\n\t}\n\tcloned := clone_in_specialization(1, original)\n\tassert cloned.inner.text == 'kept'\n\towned := DropClone{\n\t\ttext: 'dropped safely'\n\t}\n\towned_clone := owned.clone()\n\tassert owned_clone.text == 'dropped safely'\n\tmut recursive := Tokens{\n\t\ttokens: [Token{\n\t\t\tpatterns: [Tokens{\n\t\t\t\ttokens: [Token{}]\n\t\t\t}]\n\t\t}]\n\t}\n\trecursive_clone := recursive.clone()\n\trecursive.tokens[0].patterns[0].push(Token{})\n\tassert recursive.tokens[0].patterns[0].tokens.len == 2\n\tassert recursive_clone.tokens[0].patterns[0].tokens.len == 1\n\tmut appended := Tokens{}\n\tappended.push(recursive.tokens[0].clone())\n\trecursive.tokens[0].patterns[0].push(Token{})\n\tassert recursive.tokens[0].patterns[0].tokens.len == 3\n\tassert appended.tokens[0].patterns[0].tokens.len == 2\n\tprintln('ok')\n}\n")!
	out := cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-nocache',
		'-no-parallel', 'run', source], '', env)
	assert out.exit_code == 0, out.output
	assert out.output.split_into_lines().contains('ok'), out.output
	// `DropClone.drop()` frees its text and then assigns it; that assignment must not free it again.
	assert !out.output.contains('double string.free() detected'), out.output

	// After an explicit `free()`, reassigning a string or leaving its scope must not free it again.
	explicit_free_source := os.join_path(root, 'v3_nested_default_clone_explicit_free.v')
	os.write_file(explicit_free_source, "struct Holder {\nmut:\n\ttext string\n}\n\nfn release(mut holder Holder) {\n\tunsafe { holder.text.free() }\n\tholder.text = 'released'\n}\n\nfn main() {\n\tmut local := 'local'.repeat(2)\n\tunsafe { local.free() }\n\tlocal = ''\n\tmut holder := Holder{\n\t\ttext: 'field'.repeat(2)\n\t}\n\tunsafe { holder.text.free() }\n\tholder.text = 'replaced'\n\tmut param := Holder{\n\t\ttext: 'param'.repeat(2)\n\t}\n\trelease(mut param)\n\tmut deferred := 'deferred'.repeat(2)\n\tdefer {\n\t\tunsafe { deferred.free() }\n\t}\n\tprintln('\${local}|\${holder.text}|\${param.text}|\${deferred}')\n}\n")!
	explicit_free := cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership',
		'-nocache', '-no-parallel', 'run', explicit_free_source], '', env)
	assert explicit_free.exit_code == 0, explicit_free.output
	assert explicit_free.output.split_into_lines() == ['|replaced|released|deferreddeferred'], explicit_free.output
}
