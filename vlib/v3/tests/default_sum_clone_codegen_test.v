import os

const sum_clone_vexe = @VEXE
const sum_clone_tests_dir = os.dir(@FILE)
const sum_clone_v3_dir = os.dir(sum_clone_tests_dir)
const sum_clone_vlib_dir = os.dir(sum_clone_v3_dir)
const sum_clone_v3_src = os.join_path(sum_clone_v3_dir, 'v3.v')

fn test_compiler_default_clone_rebuilds_sum_payload() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_sum_clone_${pid}')
	source := os.join_path(os.temp_dir(), 'v3_sum_clone_input_${pid}.v')
	output := os.join_path(os.temp_dir(), 'v3_sum_clone_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(source) or {}
		os.rm(output) or {}
		os.rm(output + '.c') or {}
	}
	build :=
		os.execute('${sum_clone_vexe} -gc none -d ownership -path "${sum_clone_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${sum_clone_v3_src}')
	assert build.exit_code == 0, build.output
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
		os.execute('${v3_bin} -ownership -d ownership -nocache -no-parallel -o ${output} ${source}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(output)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
