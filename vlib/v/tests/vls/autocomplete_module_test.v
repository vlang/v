// vtest vflags: -w
import os
import term
import v.util.diff
import json2

// The compiler under test, not the first `v` on PATH, which may be another checkout.
const vexe = os.quoted_path(@VEXE)

const vroot = os.real_path(@VMODROOT)
const tmp_dir = os.real_path(os.temp_dir())
const text_file = os.join_path(vroot, 'vlib', 'v', 'tests', 'vls', 'sample_text.vv')
const mod1_text_file = os.join_path(vroot, 'vlib', 'v', 'tests', 'vls', 'sample_mod1', 'sample.v')

const autocomplete_info_for_mod_sample_mod1 = '{"details": [
{"kind":3,"label":"public_fn1","detail":"string","declaration":"fn public_fn1(val int) string","documentation":""},
{"kind":22,"label":"PublicStruct1","detail":"","declaration":"","documentation":""},
{"kind":7,"label":"PublicAlias1_1","detail":"","declaration":"","documentation":""},
{"kind":7,"label":"PublicAlias1_2","detail":"","declaration":"","documentation":""},
{"kind":13,"label":"PublicEnum1","detail":"","declaration":"","documentation":""},
{"kind":8,"label":"PublicInterface1","detail":"","declaration":"","documentation":""},
{"kind":21,"label":"public_const1","detail":"","declaration":"","documentation":""}
]}'

const autocomplete_info_for_mod_sample_mod2 = '{"details": [
{"kind":3,"label":"public_fn2","detail":"string","declaration":"fn public_fn2(val int) string","documentation":""},
{"kind":22,"label":"PublicStruct2","detail":"","declaration":"","documentation":""},
{"kind":13,"label":"PublicEnum2","detail":"","declaration":"","documentation":""},
{"kind":8,"label":"PublicInterface2","detail":"","declaration":"","documentation":""},
{"kind":7,"label":"PublicAlias2","detail":"","declaration":"","documentation":""},
{"kind":21,"label":"public_const2","detail":"","declaration":"","documentation":""}
]}'

// The members of a value come by their labels.
const autocomplete_info_for_mod_struct = '{"details": [
{"kind":5,"label":"a","detail":"int","declaration":"","documentation":""},
{"kind":2,"label":"add","detail":"void","declaration":"","documentation":""},
{"kind":5,"label":"b","detail":"string","declaration":"","documentation":""}
]}'

const hover_info_for_public_fn1 = '{"contents":{"kind":"markdown","value":"```v\\nfn public_fn1(val int) string\\n```"}}'

const hover_info_for_public_struct1 = '{"contents":{"kind":"markdown","value":"```v\\nstruct PublicStruct1\\n```"}}'

const fn_signature_info_for_all_before_last = '{
"signatures":[{
	"label":"all_before_last(sub string) string",
	"parameters":[{
		"label":"sub string"
	}]
}],
"activeSignature":0,
"activeParameter":0
}
'

enum Method {
	unknown         @['unknown']
	initialize      @['initialize']
	initialized     @['initialized']
	did_open        @['textDocument/didOpen']
	did_change      @['textDocument/didChange']
	definition      @['textDocument/definition']
	completion      @['textDocument/completion']
	signature_help  @['textDocument/signatureHelp']
	hover           @['textDocument/hover']
	set_trace       @['$/setTrace']
	cancel_request  @['$/cancelRequest']
	shutdown        @['shutdown']
	exit            @['exit']
}

struct TestData {
	method Method
	cmd    string
	output string
}

// A completion and a signature help are compared as JSON, whatever their layout;
// the other answers as text. A check of the whole file reports its diagnostics
// as JSON, one object per line (see check_incomplete_file_diagnostics).
const test_data = [
	TestData{
		method: .completion
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:19:3" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_sample_mod1
	},
	TestData{
		method: .completion
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:20:13" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_sample_mod2
	},
	TestData{
		method: .completion
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:22:3" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_struct
	},
	TestData{
		method: .completion
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:23:3" ${os.quoted_path(text_file)}'
		output: ''
	},
	TestData{
		method: .completion
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:26:28" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_sample_mod1
	},
	TestData{
		method: .signature_help
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:25:fn^26" ${os.quoted_path(text_file)}'
		output: fn_signature_info_for_all_before_last
	},
	TestData{
		method: .completion
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:27:9" ${os.quoted_path(text_file)}'
		output: ''
	},
	TestData{
		method: .completion
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:28:9" ${os.quoted_path(text_file)}'
		output: ''
	},
	TestData{
		method: .hover
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:30:hv^10" ${os.quoted_path(text_file)}'
		output: hover_info_for_public_fn1
	},
	TestData{
		method: .hover
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:31:hv^12" ${os.quoted_path(text_file)}'
		output: hover_info_for_public_struct1
	},
	TestData{
		method: .definition
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:30:gd^10" ${os.quoted_path(text_file)}'
		output: '${mod1_text_file}:50:7'
	},
	TestData{
		method: .definition
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:31:gd^12" ${os.quoted_path(text_file)}'
		output: '${mod1_text_file}:8:11'
	},
	TestData{
		method: .definition
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:32:gd^11" ${os.quoted_path(text_file)}'
		output: '${mod1_text_file}:41:9'
	},
	TestData{
		method: .definition
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:33:gd^15" ${os.quoted_path(text_file)}'
		output: '${mod1_text_file}:44:9'
	},
	TestData{
		method: .definition
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:34:gd^13" ${os.quoted_path(text_file)}'
		output: '${mod1_text_file}:19:10'
	},
	TestData{
		method: .definition
		cmd:    '${vexe} -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:39:gd^13" ${os.quoted_path(text_file)}'
		output: '${mod1_text_file}:50:7'
	},
	TestData{
		// `sample_text.vv` is being written: its diagnostics, not an answer.
		method: .did_change
		cmd:    '${vexe} -w -vls-mode -check -json-errors ${os.quoted_path(text_file)}'
		output: ''
	},
]

// JsonDiagnostic is a diagnostic as `-json-errors` prints it.
struct JsonDiagnostic {
	file     string
	line     int
	col      int
	end_line int
	end_col  int
	severity string
	message  string
}

// copy from `vls`
struct Detail {
	kind               int    // The type of item (e.g., Method, Function, Field)
	label              string // The name of the completion item
	detail             string // Additional info like the function signature or return type
	declaration        string // The full declaration of a function
	documentation      string // The documentation for the item
	insert_text        ?string @[json: 'insertText']
	insert_text_format ?int    @[json: 'insertTextFormat'] // 1 for PlainText, 2 for Snippet
}

struct JsonVarAC {
	details []Detail
}

struct SignatureHelp {
	signatures       []SignatureInformation
	active_signature int @[json: 'activeSignature']
	active_parameter int @[json: 'activeParameter']
}

struct SignatureInformation {
	label      string
	parameters []ParameterInformation
}

struct ParameterInformation {
	label string
}

fn test_main() {
	mut total_errors := 0

	for t in test_data {
		res := os.exec(os.split_args(t.cmd) or { panic(err) })
		if res.exit_code < 0 {
			println('fail execute ${t.cmd}')
			panic(res.output)
		}
		res_output := $if windows { res.output.replace('\r\n', '\n') } $else { res.output }
		if t.method == .did_change {
			check_incomplete_file_diagnostics(res_output) or {
				println('${term.red('FAIL')} ${t.cmd}')
				println(err)
				println(term.header('found:', '-'))
				println(res_output)
				println(term.h_divider('-'))
				total_errors++
				continue
			}
			println('${term.green('OK  ')} ${t.cmd}')
			continue
		}
		if !same_answer(t, res_output) {
			println('${term.red('FAIL')} ${t.cmd}')
			if diff_ := diff.compare_text(t.output, res_output) {
				println(term.header('difference:', '-'))
				println(diff_)
			} else {
				println(term.header('expected:', '-'))
				println(t.output)
				println(term.header('found:', '-'))
				println(res_output)
			}
			println(term.h_divider('-'))
			total_errors++
		} else {
			println('${term.green('OK  ')} ${t.cmd}')
		}

		// Try to decode the response message and verify
		if t.output.trim_space().len > 0 {
			dump(t.output)
			match t.method {
				.definition {
					check_valid_goto_definition(t.output)!
				}
				.completion {
					check_valid_auto_completion(t.output)!
				}
				.signature_help {
					check_valid_fn_signature(t.output)!
				}
				.hover {
					check_valid_hover(t.output)!
				}
				else {}
			}
		}
	}
	assert total_errors == 0
}

fn check_valid_goto_definition(message string) ! {
	// `/home/path/aaa.v:19:10`
	fields := message.split(':')
	if fields.len >= 3 {
		path := fields[..fields.len - 2].join(':')
		line_nr := fields[fields.len - 2].int()
		col := fields[fields.len - 1].int()
		if line_nr <= 0 {
			return error('goto_definition: line_nr should > 0: ${line_nr}')
		}
		if col <= 0 {
			return error('goto_definition: col should > 0: ${col}')
		}
		if path.len == 0 {
			return error('goto_definition: file.len should > 0: ${path}')
		}
	} else {
		return error('goto_definition: goto_definition format error')
	}
}

fn check_valid_auto_completion(message string) ! {
	// {"kind":5,"label":"a","detail":"int","documentation":""},
	result := json2.decode[JsonVarAC](message) or { return error('completion: fail to json decode') }
	for detail in result.details {
		if detail.kind <= 0 || detail.kind > 25 {
			return error('completion: kind should in 1-25 : ${detail.kind}')
		}
	}
}

// same_answer reports whether `found` is the answer that `t` expects: the same
// JSON for a completion and a signature help, else the same text.
fn same_answer(t TestData, found string) bool {
	expected := t.output.trim_space()
	got := found.trim_space()
	if expected == '' || t.method !in [.completion, .signature_help] {
		return expected == got
	}
	if t.method == .completion {
		want := json2.decode[JsonVarAC](expected) or { return false }
		have := json2.decode[JsonVarAC](got) or { return false }
		return want.details.map(detail_text(it)) == have.details.map(detail_text(it))
	}
	want := json2.decode[SignatureHelp](expected) or { return false }
	have := json2.decode[SignatureHelp](got) or { return false }
	return signature_help_text(want) == signature_help_text(have)
}

fn detail_text(d Detail) string {
	return '${d.kind} ${d.label} `${d.detail}` `${d.declaration}` `${d.documentation}`'
}

fn signature_help_text(h SignatureHelp) string {
	mut parts := ['${h.active_signature} ${h.active_parameter}']
	for signature in h.signatures {
		mut params := []string{}
		for param in signature.parameters {
			params << param.label
		}
		parts << '${signature.label}: ${params.join(', ')}'
	}
	return parts.join('; ')
}

// check_incomplete_file_diagnostics checks what `-json-errors` reports for
// `sample_text.vv`, whose code is being written: a JSON object for each
// diagnostic of the file, on a line of its own, the first of them the parse
// error at the `}` that ends `main`, before which the calls of lines 25 and 26
// are not closed.
fn check_incomplete_file_diagnostics(output string) ! {
	mut diagnostics := []JsonDiagnostic{}
	for line in output.split_into_lines() {
		if line.trim_space() == '' {
			continue
		}
		diagnostics << json2.decode[JsonDiagnostic](line) or {
			return error('json_errors: not a JSON diagnostic: ${line}')
		}
	}
	if diagnostics.len == 0 {
		return error('json_errors: no diagnostic')
	}
	for d in diagnostics {
		if !os.to_slash(d.file).ends_with('vlib/v/tests/vls/sample_text.vv') {
			return error('json_errors: a diagnostic of another file: ${d.file}')
		}
		if d.line <= 0 || d.col <= 0 || d.end_line < d.line {
			return error('json_errors: wrong position ${d.line}:${d.col}-${d.end_line}:${d.end_col}')
		}
		if d.severity !in ['error', 'warning', 'notice'] || d.message.len == 0 {
			return error('json_errors: wrong severity or message: ${d}')
		}
	}
	first := diagnostics[0]
	if first.severity != 'error' || first.line != 35 || first.col != 1
		|| !first.message.contains('unexpected token') {
		return error('json_errors: the first diagnostic is not the parse error at 35:1: ${first}')
	}
}

struct HoverContents {
	kind  string
	value string
}

struct HoverResult {
	contents HoverContents
}

fn check_valid_hover(message string) ! {
	result := json2.decode[HoverResult](message) or {
		return error('hover: fail to json decode: ${err}')
	}
	if result.contents.kind != 'markdown' {
		return error('hover: contents.kind should be "markdown": ${result.contents.kind}')
	}
	if result.contents.value.len == 0 {
		return error('hover: contents.value should not be empty')
	}
}

fn check_valid_fn_signature(message string) ! {
	result := json2.decode[SignatureHelp](message) or {
		return error('fn_signature: fail to json decode')
	}
	if result.signatures.len != 1 {
		return error('fn_signature: signatures.len != 1')
	}
	if result.signatures[0].label.len == 0 {
		return error('fn_signature: label.len == 0')
	}
}
