// vtest ownership
import os
import v.cmdexec
import v.tempname

fn test_compiler_default_clone_rebuilds_sum_payload() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_sum_clone_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_sum_clone_input.v')
	output := os.join_path(root, 'v3_sum_clone_program')
	os.write_file(source, "struct Payload implements IClone {
mut:
\tvalues []string
}

struct Empty implements IClone {}

type Choice = Empty | Payload

struct Owner implements IClone {
mut:
\tchoice Choice
}

struct OptionalOwner implements IClone {
mut:
\tpayload ?Payload
}

struct Recursive implements IClone {
mut:
\tchildren     []Recursive
\talternatives [][]Recursive
}

fn count_last_alternative(node Recursive) int {
\tmut count := 0
\tfor child in node.alternatives.last() {
\t\tcount += 1 + count_last_alternative(child)
\t}
\treturn count
}

fn main() {
\tmut original := Owner{
\t\tchoice: Payload{
\t\t\tvalues: ['first']
\t\t}
\t}
\tcloned := original.clone()
\tif mut original.choice is Payload {
\t\toriginal.choice.values << 'second'
\t}
\tif cloned.choice is Payload {
\t\tassert cloned.choice.values == ['first']
\t} else {
\t\tassert false
\t}
\tmut optional_original := OptionalOwner{
\t\tpayload: Payload{
\t\t\tvalues: ['optional']
\t\t}
\t}
\toptional_cloned := optional_original.clone()
\tif mut payload := optional_original.payload {
\t\tpayload.values << 'changed'
\t}
\tif payload := optional_cloned.payload {
\t\tassert payload.values == ['optional']
\t} else {
\t\tassert false
\t}
\tmut payloads := [Payload{
\t\tvalues: ['only']
\t}]
\tpayloads.last().values << 'next'
\tassert payloads[0].values == ['only', 'next']
\tnested := [[Payload{
\t\tvalues: ['nested']
\t}]]
\ttail := nested.last()
\tassert tail[0].values == ['nested']
\tmut seen := 0
\tfor payload in nested.last() {
\t\tassert payload.values == ['nested']
\t\tseen++
\t}
\tassert seen == 1
\tleaf := Recursive{
\t\talternatives: [[]]
\t}
\troot := Recursive{
\t\talternatives: [[leaf]]
\t}
\tassert count_last_alternative(root) == 1
\tprintln('ok')
}
") or {
		panic(err)
	}
	compile :=
		cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-nocache',
			'-no-parallel', '-o', output, source], '', {
			'VFLAGS':               ''
			'VTOOLS_CACHE_VERBOSE': ''
		})
	assert compile.exit_code == 0, compile.output
	run := os.exec([output])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
