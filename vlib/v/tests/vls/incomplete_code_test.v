// vtest vflags: -w
import os
import json2

// Questions of the mini-VLS protocol about code that is still being written and
// does not parse as it is: `s.` with nothing after the dot, `f(` left open. The
// compiler answers them from the file completed where it is incomplete.
const vexe = @VEXE
const vls_dir = os.join_path(os.real_path(@VMODROOT), 'vlib', 'v', 'tests', 'vls')
const text_file = os.join_path(vls_dir, 'sample_text.vv')
const goto_file = os.join_path(vls_dir, 'goto_def_test_data.vv')
const mod1_file = os.join_path(vls_dir, 'sample_mod1', 'sample.v')

struct Detail {
	kind  int
	label string
}

struct Details {
	details []Detail
}

struct Signature {
	label string
}

struct SignatureHelp {
	signatures []Signature
}

fn ask(file string, question string) string {
	res := os.exec([vexe, '-w', '-check', '-nocolor', '-vls-mode', '-line-info', '${file}:${question}',
		file])
	assert res.exit_code == 0, res.output
	return res.output.trim_space()
}

fn labels(question string) []string {
	answer := ask(text_file, question)
	return (json2.decode[Details](answer) or { panic('${err}: ${answer}') }).details.map(it.label)
}

fn test_a_module_name_before_a_dot_that_ends_a_line_offers_the_module() {
	// `s.` and `sample_mod2.`: functions, types and consts, the types that the
	// file names first.
	mod1 := ['public_fn1', 'PublicStruct1', 'PublicAlias1_1', 'PublicAlias1_2', 'PublicEnum1',
		'PublicInterface1', 'public_const1']
	assert labels('19:3') == mod1
	assert labels('20:13') == ['public_fn2', 'PublicStruct2', 'PublicEnum2', 'PublicInterface2',
		'PublicAlias2', 'public_const2']
	// In the arguments of a call that is left open.
	assert labels('26:28') == mod1
	// `strings.` names no module that the file imports.
	assert ask(text_file, '27:9') == ''
	assert ask(text_file, '28:9') == ''
}

fn test_a_value_before_a_dot_that_ends_a_line_offers_its_members() {
	// `k.`, whose member the next line names.
	assert labels('22:3') == ['a', 'add', 'b']
	// `s.` there is a field of `k`, which has none named `s`.
	assert ask(text_file, '23:3') == ''
}

fn test_a_call_left_open_has_a_signature() {
	answer := ask(text_file, '25:fn^26')
	help := json2.decode[SignatureHelp](answer) or { panic('${err}: ${answer}') }
	assert help.signatures.map(it.label) == ['all_before_last(sub string) string']
}

fn test_the_code_after_the_incomplete_code_has_answers() {
	// The checker checks no more of `main` after its first error: a call of a
	// module's function has a definition still.
	assert ask(text_file, '30:gd^10') == '${mod1_file}:50:7'
	assert ask(text_file, '34:gd^13') == '${mod1_file}:19:10'
	assert ask(text_file, '31:hv^12') == '{"contents":{"kind":"markdown","value":"```v\\nstruct PublicStruct1\\n```"}}'
}

fn test_a_value_of_an_imported_enum_has_a_definition() {
	// `a` of `s.PublicEnum1.a`, with `s` the alias of the module.
	assert ask(goto_file, '129:gd^29') == '${mod1_file}:25:1'
	// The enum's own name stands for the enum.
	assert ask(goto_file, '129:gd^20') == '${mod1_file}:23:9'
}
