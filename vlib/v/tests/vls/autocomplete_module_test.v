import os
import term
import v.util.diff
import json

const vroot = os.real_path(@VMODROOT)
const tmp_dir = os.real_path(os.temp_dir())
const text_file = os.join_path(vroot, 'vlib', 'v', 'tests', 'vls', 'sample_text.vv')
// note: windows path separator will cause json decode fail
const json_errors_text_file = os.to_slash(text_file)
const mod1_text_file = os.join_path(vroot, 'vlib', 'v', 'tests', 'vls', 'sample_mod1',
	'sample.v')

const autocomplete_info_for_mod_sample_mod1 = '{"details": [
{"kind":3,"label":"public_fn1","detail":"string","documentation":""},
{"kind":22,"label":"PublicStruct1","detail":"","documentation":""},
{"kind":7,"label":"PublicAlias1_1","detail":"","documentation":""},
{"kind":7,"label":"PublicAlias1_2","detail":"","documentation":""},
{"kind":13,"label":"PublicEnum1","detail":"","documentation":""},
{"kind":8,"label":"PublicInterface1","detail":"","documentation":""},
{"kind":21,"label":"public_const1","detail":"","documentation":""}
]}'

const autocomplete_info_for_mod_sample_mod2 = '{"details": [
{"kind":3,"label":"public_fn2","detail":"string","documentation":""},
{"kind":22,"label":"PublicStruct2","detail":"","documentation":""},
{"kind":13,"label":"PublicEnum2","detail":"","documentation":""},
{"kind":8,"label":"PublicInterface2","detail":"","documentation":""},
{"kind":7,"label":"PublicAlias2","detail":"","documentation":""},
{"kind":21,"label":"public_const2","detail":"","documentation":""}
]}'

const autocomplete_info_for_mod_struct = '{"details": [
{"kind":5,"label":"a","detail":"int","documentation":""},
{"kind":5,"label":"b","detail":"string","documentation":""},
{"kind":2,"label":"add","detail":"void","documentation":""}
]}'

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

const test_data = [
	TestData{
		method: .completion
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:19:3" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_sample_mod1
	},
	TestData{
		method: .completion
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:20:13" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_sample_mod2
	},
	TestData{
		method: .completion
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:22:3" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_struct
	},
	TestData{
		method: .completion
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:23:3" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_sample_mod1
	},
	TestData{
		method: .completion
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:26:28" ${os.quoted_path(text_file)}'
		output: autocomplete_info_for_mod_sample_mod1
	},
	TestData{
		method: .signature_help
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:25:fn^26" ${os.quoted_path(text_file)}'
		output: fn_signature_info_for_all_before_last
	},
	TestData{
		method: .completion
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:27:9" ${os.quoted_path(text_file)}'
		output: ''
	},
	TestData{
		method: .completion
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:28:9" ${os.quoted_path(text_file)}'
		output: 'unresolved type, maybe "builtin" was not defined. otherwise this is a bug, should never happen; please report'
	},
	TestData{
		method: .definition
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:30:gd^10" ${os.quoted_path(text_file)}'
		output: '${mod1_text_file}:50:7'
	},
	TestData{
		method: .definition
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:31:gd^12" ${os.quoted_path(text_file)}'
		output: '${mod1_text_file}:8:11'
	},
	TestData{
		method: .definition
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:32:gd^11" ${os.quoted_path(text_file)}'
		output: '${mod1_text_file}:41:9'
	},
	TestData{
		method: .definition
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:33:gd^15" ${os.quoted_path(text_file)}'
		output: '${mod1_text_file}:44:9'
	},
	TestData{
		method: .definition
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:34:gd^13" ${os.quoted_path(text_file)}'
		output: '${mod1_text_file}:19:10'
	},
	TestData{
		method: .definition
		cmd:    'v -w -check -json-errors -nocolor -vls-mode -line-info "${text_file}:39:gd^13" ${os.quoted_path(text_file)}'
		output: '${mod1_text_file}:50:7'
	},
	TestData{
		method: .did_change
		cmd:    'v -w -vls-mode -check -json-errors ${os.quoted_path(text_file)}'
		output: '[
{
"path":"${json_errors_text_file}",
"message":"unexpected token `:=`, expecting `)`",
"line_nr":26,
"col":4,
"len":0
}
,
{
"path":"${json_errors_text_file}",
"message":"unexpected name `strings`, expecting `)`",
"line_nr":27,
"col":2,
"len":0
}
,
{
"path":"${json_errors_text_file}",
"message":"undefined ident: ``",
"line_nr":19,
"col":3,
"len":0
}
,
{
"path":"${json_errors_text_file}",
"message":"undefined ident: ``",
"line_nr":20,
"col":13,
"len":0
}
,
{
"path":"${json_errors_text_file}",
"message":"undefined ident: ``",
"line_nr":23,
"col":3,
"len":0
}
,
{
"path":"${json_errors_text_file}",
"message":"undefined ident: `j`",
"line_nr":26,
"col":2,
"len":0
}
,
{
"path":"${json_errors_text_file}",
"message":"`j` (no value) used as value in argument 1 to `string.all_before_last`",
"line_nr":26,
"col":2,
"len":0
}
,
{
"path":"${json_errors_text_file}",
"message":"undefined ident: ``",
"line_nr":26,
"col":28,
"len":0
}
,
{
"path":"${json_errors_text_file}",
"message":"`` (no value) used as value in argument 1 to `string.all_before_last`",
"line_nr":26,
"col":27,
"len":0
}
,
{
"path":"${json_errors_text_file}",
"message":"`string` has no property ``",
"line_nr":26,
"col":11,
"len":0
}
,
{
"path":"${json_errors_text_file}",
"message":"undefined ident: `builtin`",
"line_nr":28,
"col":2,
"len":0
}
,
{
"path":"${json_errors_text_file}",
"message":"`builtin` does not return a value",
"line_nr":28,
"col":2,
"len":0
}
,
{
"path":"/media/HD/github/kbkpbot/v/vlib/v/tests/vls/sample_text.vv",
"message":"unknown type `main.NotExistStruct`",
"line_nr":38,
"col":11,
"len":0
}
]
'
	},
]

// copy from `vls`
struct JsonError {
	path    string
	message string
	line_nr int
	col     int
	len     int
}

struct Detail {
	kind               int    // The type of item (e.g., Method, Function, Field)
	label              string // The name of the completion item
	detail             string // Additional info like the function signature or return type
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
		res := os.execute(t.cmd)
		if res.exit_code < 0 {
			println('fail execute ${t.cmd}')
			panic(res.output)
		}
		res_output := $if windows {
			res.output.replace('\r\n', '\n')
		} $else {
			res.output
		}
		if t.output.trim_space() != res_output.trim_space() {
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
		// TODO: remove `unresolved type, maybe`
		if t.output.trim_space().len > 0 && !t.output.starts_with('unresolved type, maybe') {
			dump(t.output)
			match t.method {
				.definition {
					check_valid_goto_definition(t.output)!
				}
				.completion {
					check_valid_auto_completion(t.output)!
				}
				.did_change {
					check_valid_json_errors(t.output)!
				}
				.signature_help {
					check_valid_fn_signature(t.output)!
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
	result := json.decode(JsonVarAC, message) or { return error('completion: fail to json decode') }
	for detail in result.details {
		if detail.kind <= 0 || detail.kind > 25 {
			return error('completion: kind should in 1-25 : ${detail.kind}')
		}
	}
}

fn check_valid_json_errors(message string) ! {
	results := json.decode([]JsonError, message) or {
		return error('json_errors: fail to json decode')
	}
	for result in results {
		if result.path.len == 0 {
			return error('json_errors: path.len should > 0')
		}
		if result.message.len == 0 {
			return error('json_errors: message.len should > 0')
		}
		if result.line_nr <= 0 {
			return error('json_errors: line_nr should > 0')
		}
		if result.col <= 0 {
			return error('json_errors: col should > 0')
		}
	}
}

fn check_valid_fn_signature(message string) ! {
	result := json.decode(SignatureHelp, message) or {
		return error('fn_signature: fail to json decode')
	}
	if result.signatures.len != 1 {
		return error('fn_signature: signatures.len != 1')
	}
	if result.signatures[0].label.len == 0 {
		return error('fn_signature: label.len == 0')
	}
}
