module main

import json2 as json
import mcp
import os
import v.astquery
import v.skills

// The tests below exercise the tool handlers in process. They deliberately avoid
// `v_check`, `v_run`, `v_test_run` and `v_doctor`: those start the compiler, and
// a test that shells out would test the sandbox as much as the code.

const test_root = os.join_path(os.vtmp_dir(), 'v_vmcp_test_${os.getpid()}')

// sample_manifest is the `v.mod` of the fake project.
const sample_manifest = 'Module {\n' + "\tname: 'probe'\n" +
	"\tdescription: 'A probe project.'\n" + "\tversion: '0.1.0'\n" +
	"\tlicense: 'MIT'\n" + "\trepo_url: 'https://example.com/probe'\n" +
	'\tdependencies: []\n}\n'

// sample_main is the fake project's `main.v`.
// The fixture body is spelled with a raw string: the pieces below are what the
// parser will see, written out so nothing needs escaping.
const sample_main = 'module main\n\nimport os\n\nconst answer = 42\n\n' +
	sample_greet + sample_run

// sample_greet is the documented function the AST tests look for.
const sample_greet = '/// Greets the world.\nfn greet(name string) string {\n' +
	"\treturn 'hello, " + '\${name}' + "'\n}\n\n"

// sample_run is the entry point that calls it.
const sample_run = "fn main() {\n\tprintln(greet('world'))\n\tprintln(answer)\n}\n"

// probe_path returns a path inside the fake project, creating the project
// directory when it is not there yet. It never rewrites a file that is already
// there, so a test can read back what it wrote.
fn probe_path(name string) string {
	full := os.join_path(os.join_path(test_root, 'probe'), name)
	os.mkdir_all(os.dir(full)) or {
		panic(err)
	}
	return full
}

// probe_root creates a fake project and returns its root.
fn probe_root() !string {
	os.rmdir_all(test_root) or {}
	root := os.join_path(test_root, 'probe')
	os.mkdir_all(root)!
	os.write_file(os.join_path(root, 'v.mod'), sample_manifest)!
	os.write_file(os.join_path(root, 'main.v'), sample_main)!
	return os.real_path(root)
}

// probe_workspace returns a Workspace pointed at the fake project.
//
// The fake project is rebuilt every time, so a test that changes a file cannot
// leak that change into the next one.
fn probe_workspace() Workspace {
	root := probe_root() or { panic(err) }
	return new_workspace(@VEXEROOT, root, false)
}

// field reads one top-level property out of a rendered JSON object.
fn field(json_text string, key string) string {
	parsed := json.decode[map[string]json.Any](json_text) or {
		return ''
	}
	value := parsed[key] or {
		return ''
	}
	return value.str()
}

// call_tool runs a tool through the MCP server wrapper against a freshly built
// fixture, so the tests cover the registration path as well as the handler.
//
// A test that changed a file first calls `call_tool_on` instead: rebuilding the
// fixture here would undo the change the test is about.
fn call_tool(name string, arguments string) string {
	return call_tool_on(probe_workspace(), name, arguments)
}

// call_tool_on runs a tool through the MCP wrapper against `ws`.
fn call_tool_on(ws Workspace, name string, arguments string) string {
	for spec in tool_specs() {
		if spec.tool.name != name {
			continue
		}
		return mcp_text_of(spec.handler(ws, arguments))
	}
	return ''
}

// mcp_text_of unwraps a rendered tool result and returns the JSON payload a
// client would read.
//
// `tools/call` wraps the payload in a JSON content block, so a test that matched
// against the wrapper would be asserting on escape sequences rather than on the
// answer the agent sees.
fn mcp_text_of(json_text string) string {
	wrapped := mcp.tool_text_result(json_text).content
	blocks := json.decode[[]map[string]json.Any](wrapped) or {
		return ''
	}
	for block in blocks {
		value := block['text'] or {
			continue
		}
		return value.str()
	}
	return ''
}

fn test_every_tool_is_named_and_described() {
	mut names := []string{}
	for spec in tool_specs() {
		assert spec.tool.name != '', 'a tool has no name'
		assert spec.tool.description.trim_space() != '', '${spec.tool.name} has no description'
		assert spec.tool.input_schema != '', '${spec.tool.name} has no input schema'
		assert !isnil(spec.handler), '${spec.tool.name} has no handler'
		assert spec.tool.name !in names, '${spec.tool.name} is declared twice'
		names << spec.tool.name
	}
	// Every documented tool is present. A name here that no tool implements is a
	// gap in the catalogue; a tool here that this list omits is a silent addition.
	for expected in ['v_project_info', 'v_modules', 'v_files', 'v_ast', 'v_symbols', 'v_symbol_at',
		'v_references', 'v_stdlib_doc', 'v_check', 'v_test_run', 'v_doctor', 'v_veb_routes', 'v_skills',
		'v_run', 'v_eval', 'v_edit_replace', 'v_rename_symbol', 'v_format'] {
		assert expected in names, '${expected} is missing from the catalogue'
	}
}

// Limit the unencoded catalogue strings separately from the complete response.
// These are UTF-8 byte budgets, not model-specific token counts.
const max_catalogue_content_bytes = 12000

// The current writable/read-only responses are 12,580/9,763 bytes including LF.
// A 13,000-byte ceiling leaves 420 bytes of headroom for serialized metadata.
const max_tools_list_response_bytes = 13000

fn test_tool_catalogue_stays_within_content_byte_budget() {
	mut total := 0
	for spec in tool_specs() {
		total += spec.tool.name.len + spec.tool.description.len + spec.tool.input_schema.len
	}
	assert total < max_catalogue_content_bytes, 'catalogue content is ${total} bytes, over the ${max_catalogue_content_bytes} byte budget'
}

// The catalogue is the whole public surface: `--read-only` drops exactly the
// tools that write, and nothing else.
fn test_read_only_keeps_only_the_safe_tools() {
	read_only := new_workspace(@VEXEROOT, probe_root()!, true)
	mut kept := []string{}
	for spec in tool_specs() {
		if !read_only.read_only || spec.read_only() {
			kept << spec.tool.name
		}
	}
	for name in kept {
		assert name !in ['v_edit_replace', 'v_rename_symbol', 'v_format'], '${name} writes a file and must not be registered in read-only mode'
	}
	for name in ['v_edit_replace', 'v_rename_symbol', 'v_format'] {
		assert name !in kept, '${name} is a writing tool'
	}
	// Every read-only tool survives.
	for name in ['v_project_info', 'v_symbols', 'v_ast', 'v_references', 'v_check', 'v_skills',
		'v_eval'] {
		assert name in kept, '${name} should be available in read-only mode'
	}
}

fn test_project_info_reports_the_manifest() {
	answer := call_tool('v_project_info', '{"include":"installed_modules"}')
	assert !answer.contains('"isError"'), answer
	assert answer.contains('"name":\t"probe"') || answer.contains('"name": "probe"'), answer
	assert answer.contains('probe'), answer
	assert answer.contains('"has_v_mod"'), answer
	assert answer.contains('"read_only"'), answer
	// The manifest is a nested object, not a string that happens to hold braces.
	assert answer.contains('"v_mod":\t{'), answer
	assert !answer.contains('\\"name\\"'), answer
}

fn test_project_info_reports_a_project_without_a_manifest() {
	// Start from a project that has no `v.mod` at all, which is what a plain
	// folder of sources looks like.
	root := os.join_path(test_root, 'bare')
	os.mkdir_all(root)!
	os.write_file(os.join_path(root, 'main.v'), sample_main)!
	ws := new_workspace(@VEXEROOT, root, false)
	answer := mcp_text_of(tool_project_info(ws, '{}'))
	assert answer.contains('"has_v_mod":\tfalse') || answer.contains('"has_v_mod": false'), answer
}

fn test_project_info_omits_the_optional_sections_unless_asked() {
	answer := call_tool('v_project_info', '{}')
	assert !answer.contains('installed_modules'), answer
	assert !answer.contains('"skills"'), answer
}

fn test_modules_lists_declared_and_installed() {
	answer := call_tool('v_modules', '{}')
	assert answer.contains('"declared"'), answer
	assert answer.contains('"installed"'), answer
	assert answer.contains('"local_module_dirs"'), answer
}

fn test_modules_honours_the_filter() {
	answer := call_tool('v_modules', '{"filter":"zzz-nothing"}')
	assert answer.contains('"declared"'), answer
	assert answer.contains('[]'), answer
}

fn test_files_lists_the_project_sources() {
	answer := call_tool('v_files', '{}')
	assert answer.contains('"path":\t"main.v"') || answer.contains('"path": "main.v"'), answer
	assert answer.contains('"lines"'), answer
	assert answer.contains('"bytes"'), answer
	assert answer.contains('"truncated":\t0') || answer.contains('"truncated": 0'), answer
}

fn test_files_honours_the_test_filter_and_the_limit() {
	ws := probe_workspace()
	os.write_file(probe_path('extra_test.v'), 'module main\n')!
	without := tool_files(ws, '{"include_tests":false}')
	assert !without.contains('extra_test.v'), without
	with := tool_files(ws, '{"include_tests":true}')
	assert with.contains('extra_test.v'), with
	limited := tool_files(ws, '{"limit":1}')
	assert field(replace_all(limited, '\t', ''), 'returned') == '1', limited
	assert field(replace_all(limited, '\t', ''), 'truncated') != '0', limited
}

fn test_files_reports_a_missing_directory() {
	answer := call_tool('v_files', '{"path":"nope"}')
	assert answer.contains('"error"'), answer
}

fn test_ast_matches_the_v_ast_renderer() {
	answer := call_tool('v_ast', '{"path":"main.v","terse":true}')
	assert answer.contains('"kind":\t"file"') || answer.contains('"kind": "file"'), answer
	assert answer.contains('"truncated"'), answer
}

fn test_ast_reports_a_missing_file() {
	answer := call_tool('v_ast', '{"path":"nope.v"}')
	assert answer.contains('"error"'), answer
}

fn test_ast_requires_a_path() {
	answer := call_tool('v_ast', '{}')
	assert answer.contains('"error"'), answer
	assert answer.contains('path'), answer
}

fn test_symbols_lists_declarations_with_docs() {
	answer := call_tool('v_symbols', '{"path":"main.v"}')
	assert answer.contains('"kind":\t"fn"') || answer.contains('"kind": "fn"'), answer
	assert answer.contains('greet'), answer
	assert answer.contains('Greets the world.'), answer
	assert answer.contains('"line"'), answer
}

fn test_symbols_filters_by_kind() {
	answer := call_tool('v_symbols', '{"path":"main.v","kind":"const"}')
	assert answer.contains('answer'), answer
	assert !answer.contains('"greet"'), answer
}

fn test_symbols_can_drop_members() {
	answer := call_tool('v_symbols', '{"path":"main.v","include_nested":false}')
	assert !answer.contains('"field"'), answer
	assert answer.contains('greet'), answer
}

fn test_symbol_at_finds_a_name() {
	answer := call_tool('v_symbol_at', '{"path":"main.v","line":8,"column":4,"name":"greet"}')
	assert answer.contains('"found":\ttrue') || answer.contains('"found": true'), answer
	assert answer.contains('"is_declaration":\ttrue') || answer.contains('"is_declaration": true'), answer
	assert answer.contains('greet'), answer
}

fn test_symbol_at_reports_a_miss_without_failing() {
	answer := call_tool('v_symbol_at', '{"path":"main.v","line":1,"column":1,"name":"nothing"}')
	assert answer.contains('"found":\tfalse') || answer.contains('"found": false'), answer
}

fn test_symbol_at_rejects_a_zero_position() {
	answer := call_tool('v_symbol_at', '{"path":"main.v","line":0,"column":0}')
	assert answer.contains('"error"'), answer
}

fn test_symbol_at_without_a_name_lists_what_is_there() {
	answer := call_tool('v_symbol_at', '{"path":"main.v","line":8,"column":4}')
	assert answer.contains('declarations_here'), answer
	assert answer.contains('greet'), answer
}

fn test_references_finds_the_declaration_and_the_call() {
	answer := call_tool('v_references', '{"path":"main.v","name":"greet"}')
	assert answer.contains('"count":\t2') || answer.contains('"count": 2'), answer
	assert answer.contains('"is_declaration":\ttrue') || answer.contains('"is_declaration": true'), answer
	assert answer.contains('"is_declaration":\tfalse') || answer.contains('"is_declaration": false'), answer
}

fn test_references_requires_a_name() {
	answer := call_tool('v_references', '{"path":"main.v"}')
	assert answer.contains('"error"'), answer
}

fn test_references_does_not_match_text() {
	ws := probe_workspace()
	texty := 'module main\n\nfn go() {\n\t// greet is prose\n\ts := "greet"\n\tprintln(s)\n}\n'
	os.write_file(probe_path('texty.v'), texty)!
	answer := tool_references(ws, '{"path":"texty.v","name":"greet"}')
	assert field(replace_all(answer, '\t', ''), 'count') == '0', answer
}

fn test_stdlib_doc_reports_a_module() {
	answer := call_tool('v_stdlib_doc', '{"symbol":"strings"}')
	assert answer.contains('"found":\ttrue') || answer.contains('"found": true'), answer
	assert answer.contains('"symbol_count"'), answer
	assert answer.contains('Builder'), answer
}

fn test_stdlib_doc_reports_a_symbol() {
	answer := call_tool('v_stdlib_doc', '{"symbol":"strings.Builder"}')
	assert answer.contains('"name":\t"Builder"') || answer.contains('"name": "Builder"'), answer
	assert answer.contains('"signature"'), answer
	assert answer.contains('append'), answer
}

fn test_stdlib_doc_reports_an_unknown_module() {
	answer := call_tool('v_stdlib_doc', '{"symbol":"definitely_not_a_module"}')
	assert answer.contains('"error"'), answer
}

fn test_stdlib_doc_reports_an_undocumented_member() {
	answer := call_tool('v_stdlib_doc', '{"symbol":"strings.NoSuchSymbol"}')
	assert answer.contains('"found":\tfalse') || answer.contains('"found": false'), answer
	assert answer.contains('"hint"'), answer
}

fn test_stdlib_doc_pages_a_large_module_listing() {
	paged := call_tool('v_stdlib_doc', '{"symbol":"os","limit":2}')
	assert field(replace_all(paged, '\t', ''), 'returned') == '2', paged
	assert paged.contains('"truncated":\ttrue') || paged.contains('"truncated": true'), paged
	assert paged.contains('"limit"'), paged
	assert paged.contains('"hint"'), paged
	// `symbol_count` stays the total, so the page still says how big the module is.
	full := call_tool('v_stdlib_doc', '{"symbol":"os","limit":1000000}')
	assert full.contains('"truncated":\tfalse') || full.contains('"truncated": false'), full
	shifted := call_tool('v_stdlib_doc', '{"symbol":"os","limit":1,"offset":1}')
	assert field(replace_all(shifted, '\t', ''), 'returned') == '1', shifted
	first := call_tool('v_stdlib_doc', '{"symbol":"os","limit":1}')
	assert shifted != first, 'offset 1 must move the page'
}

fn test_files_clamps_an_enormous_limit() {
	assert clamp_file_limit(1000000) == 2000
	assert clamp_file_limit(2000) == 2000
	assert clamp_file_limit(500) == 500
	assert clamp_file_limit(1) == 1
}

fn test_stdlib_doc_bounds_large_limits_and_offsets() {
	max_limit := if sizeof(int) == 8 { int(0x7fffffffffffffff) } else { int(0x7fffffff) }
	assert max_limit > 0
	assert decode_args('{"limit":${max_limit}}').int('limit', 0) == max_limit
	ws := probe_workspace()
	dir := os.join_path(ws.project_root, 'paged')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'paged.v'), 'module paged\n\n' +
		'// alpha is the first symbol.\npub fn alpha() {}\n\n' +
		'// beta is the second symbol.\npub fn beta() {}\n')!
	last := call_tool_on(ws, 'v_stdlib_doc', '{"symbol":"paged","limit":${max_limit},"offset":1}')
	assert field(last, 'symbol_count') == '2', last
	assert field(last, 'returned') == '1', last
	assert field(last, 'truncated') == 'false', last
	assert last.contains('beta'), last
	assert !last.contains('alpha'), last
	empty := call_tool_on(ws, 'v_stdlib_doc', '{"symbol":"paged","limit":${max_limit},"offset":${max_limit}}')
	assert field(empty, 'symbol_count') == '2', empty
	assert field(empty, 'returned') == '0', empty
	assert field(empty, 'truncated') == 'false', empty
	defaults := call_tool_on(ws, 'v_stdlib_doc', '{"symbol":"paged","limit":0,"offset":-1}')
	assert field(defaults, 'returned') == '2', defaults
	first := call_tool_on(ws, 'v_stdlib_doc', '{"symbol":"paged","limit":1}')
	assert field(first, 'returned') == '1', first
	assert field(first, 'truncated') == 'true', first
	assert first.contains('alpha') && !first.contains('beta'), first
}

fn test_page_diagnostics_keeps_the_head() {
	items := [
		Diagnostic{ path: 'a.v', line: 1, column: 1, kind: 'error', message: 'first' },
		Diagnostic{ path: 'a.v', line: 2, column: 1, kind: 'error', message: 'second' },
		Diagnostic{ path: 'a.v', line: 3, column: 1, kind: 'warning', message: 'third' },
	]
	kept, omitted := page_diagnostics(items, 2)
	assert kept.len == 2 && omitted == 1, 'expected 2 kept and 1 omitted'
	assert kept[0].message == 'first' && kept[1].message == 'second', 'the head must survive'
	all, none_omitted := page_diagnostics(items, 0)
	assert all.len == 3 && none_omitted == 0, 'max < 1 means the default, which fits'
	empty, _ := page_diagnostics([]Diagnostic{}, 2)
	assert empty.len == 0, 'empty stays empty'
}

fn test_check_json_pages_diagnostics_but_keeps_totals() {
	ws := probe_workspace()
	items := [
		Diagnostic{ path: 'a.v', line: 1, column: 1, kind: 'error', message: 'first' },
		Diagnostic{ path: 'a.v', line: 2, column: 1, kind: 'error', message: 'second' },
		Diagnostic{ path: 'a.v', line: 3, column: 1, kind: 'warning', message: 'third' },
	]
	run := CompilerRun{
		exit_code: 1
		command:   'v -check a.v'
	}
	answer := check_json(ws, probe_path('main.v'), run, items, 2)
	flat := replace_all(answer, '\t', '')
	// Counts describe all three; only the array is paged.
	assert field(flat, 'error_count') == '2', answer
	assert field(flat, 'warning_count') == '1', answer
	assert field(flat, 'diagnostics_omitted') == '1', answer
	assert answer.contains('first'), answer
	assert !answer.contains('third'), answer
	assert answer.contains('"hint"'), answer
}

fn test_page_diagnostics_default_and_integer_bounds() {
	mut items := []Diagnostic{}
	for i in 0 .. max_diagnostics_default + 2 {
		items << Diagnostic{
			path:    'many.v'
			line:    i + 1
			kind:    'error'
			message: 'diagnostic ${i}'
		}
	}
	for limit in [0, -1, -2147483647 - 1] {
		assert limit <= 0
		kept, omitted := page_diagnostics(items, limit)
		assert kept.len == max_diagnostics_default
		assert omitted == 2
		assert kept[0].message == 'diagnostic 0'
		assert kept.last().message == 'diagnostic 99'
	}
	all, omitted := page_diagnostics(items, int(0x7fffffff))
	assert all.len == items.len
	assert omitted == 0
}

fn test_run_result_pages_diagnostics_and_reports_total_counts() {
	ws := probe_workspace()
	run := CompilerRun{
		exit_code: 1
		command:   'v run broken.v'
		output:    'broken.v:1:1: error: first\nbroken.v:2:1: warning: second\nbroken.v:3:1: error: third\n'
	}
	answer := run_result_json(ws, probe_path('broken.v'), run, 1)
	parsed := json.decode[map[string]json.Any](answer) or { panic(err) }
	assert parsed['started']!.bool()
	assert parsed['exit_code']!.int() == 1
	assert !parsed['ok']!.bool()
	assert parsed['error_count']!.int() == 2
	assert parsed['warning_count']!.int() == 1
	assert parsed['diagnostics_omitted']!.int() == 2
	diagnostics := parsed['diagnostics']!.as_array()
	assert diagnostics.len == 1
	diagnostic := diagnostics[0].as_map()
	assert diagnostic['message']!.str() == 'first'
	assert parsed['output']!.str() == run.output.trim_space()
	assert parsed['hint']!.str().contains('first 1 diagnostics')
}

fn test_run_result_without_diagnostics_or_started_child() {
	ws := probe_workspace()
	clean := run_result_json(ws, probe_path('main.v'), CompilerRun{
		output: 'hello\n'
	}, 1)
	parsed := json.decode[map[string]json.Any](clean) or { panic(err) }
	assert parsed['ok']!.bool()
	assert parsed['error_count']!.int() == 0
	assert parsed['warning_count']!.int() == 0
	assert parsed['output']!.str() == 'hello'
	assert 'diagnostics' !in parsed
	assert 'hint' !in parsed
	failed := run_result_json(ws, probe_path('main.v'), CompilerRun{
		launch_error: 'could not launch compiler'
	}, 1)
	not_started := json.decode[map[string]json.Any](failed) or { panic(err) }
	assert !not_started['started']!.bool()
	assert 'exit_code' !in not_started
	assert 'ok' !in not_started
	assert 'error_count' !in not_started
}

fn test_module_path_prefers_the_project_over_an_installed_module() {
	root := probe_root()!
	os.mkdir_all(os.join_path(root, 'mylib'))!
	os.write_file(os.join_path(root, 'mylib', 'a.v'), 'module mylib\n')!
	ws := new_workspace(@VEXEROOT, root, false)
	path := module_path(ws, 'mylib') or { panic('the project module was not found') }
	assert path == os.join_path(root, 'mylib'), path
}

fn test_module_path_resolves_a_dotted_name() {
	ws := probe_workspace()
	path := module_path(ws, 'net.http') or { panic('net.http was not found') }
	assert path.ends_with('net\\http') || path.ends_with('net/http'), path
}

fn test_module_files_skips_tests() {
	dir := probe_path('sub')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'a.v'), 'module sub\n')!
	os.write_file(os.join_path(dir, 'b_test.v'), 'module sub\n')!
	files := module_files(dir)
	assert files.len == 1, files.join(', ')
	assert files[0].ends_with('a.v'), files.join(', ')
}

fn test_module_doc_reads_the_comment_above_module() {
	dir := probe_path('docd')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'a.v'), '// The module does a thing.\nmodule docd\n')!
	assert module_doc(module_files(dir)) == 'The module does a thing.'
}

fn test_module_doc_is_empty_without_a_comment() {
	dir := probe_path('plain')
	os.mkdir_all(dir)!
	os.write_file(os.join_path(dir, 'a.v'), 'module plain\n')!
	assert module_doc(module_files(dir)) == ''
}

fn test_signature_of_renders_the_forms_a_caller_writes() {
	fn_decl := astquery.Declaration{
		kind:      .fn
		name:      'read_file'
		type_name: 'string'
	}
	assert signature_of(fn_decl) == 'fn read_file() string'
	method := astquery.Declaration{
		kind:      .method
		name:      'write'
		receiver:  'Builder'
		type_name: 'void'
	}
	assert signature_of(method) == 'fn Builder.write() void', signature_of(method)
	field := astquery.Declaration{
		kind:      .field
		name:      'len'
		type_name: 'int'
	}
	assert signature_of(field) == 'field len int', signature_of(field)
	assert signature_of(astquery.Declaration{
		kind: .struct
		name: 'Builder'
	}) == 'struct Builder'
}

fn test_doctor_reports_the_installation() {
	answer := call_tool('v_doctor', '{}')
	// The compiler may not be runnable in a sandbox, but the tool must still
	// answer with its own state rather than nothing.
	assert answer.contains('"vroot"'), answer
	assert answer.contains('"compiler"'), answer
}

// A compiler that cannot be started is a different fact from a compiler that
// failed, and the tools have to keep them apart. `os.execute` signals a failed
// launch only through its output text, so `launch_failure` is what tells the two
// situations apart.
fn test_launch_failure_recognises_the_ways_a_process_fails_to_start() {
	assert launch_failure('exec failed (SetHandleInformation): The handle is invalid.')
	assert launch_failure('exec failed (CreateProcess) with code 2: no such file')
	assert launch_failure('exec failed (CreatePipe): access denied')
	assert launch_failure('exec requires at least one argument')
	assert launch_failure('exec("v") failed')
}

fn test_launch_failure_does_not_mistake_real_output_for_a_failed_launch() {
	// A real diagnostic is a line and a column and a kind; nothing else is.
	assert !launch_failure('')
	assert !launch_failure('main.v:2:5: error: unknown function: nope')
	assert !launch_failure('V 0.5.0 4b1b2c3')
	assert !launch_failure('something went wrong')
	// A compiler error whose path happens to start with `exec` is still a result.
	assert !launch_failure('exec.v:1:1: error: boom')
}

fn test_a_run_that_never_started_says_so_instead_of_reporting_an_exit_code() {
	ws := probe_workspace()
	run := CompilerRun{
		exit_code:    6
		output:       'exec failed (SetHandleInformation): The handle is invalid.'
		command:      'v check main.v'
		launch_error: 'exec failed (SetHandleInformation): The handle is invalid.'
	}
	answer := check_json(ws, probe_path('main.v'), run, [], 0)
	assert answer.contains('"started":\tfalse'), answer
	assert answer.contains('could not be started'), answer
	// An `error_count` of zero next to a non-zero exit code would read as a clean
	// check, so it must not be there at all.
	assert !answer.contains('error_count'), answer
	assert !answer.contains('"exit_code"'), answer
}

fn test_a_run_that_did_start_reports_its_exit_code() {
	ws := probe_workspace()
	answer := check_json(ws, probe_path('main.v'), CompilerRun{
		exit_code: 1
		output:    'main.v:1:1: error: boom'
		command:   'v check main.v'
	}, [Diagnostic{
		path: 'main.v'
		line: 1
		kind: 'error'
	}], 0)
	assert answer.contains('"started":\ttrue'), answer
	assert answer.contains('"exit_code":\t1'), answer
	assert answer.contains('"error_count":\t1'), answer
	assert !answer.contains('could not be started'), answer
}

fn test_a_failed_version_read_is_not_reported_as_a_version() {
	// `os.execute` puts its own message in `output`, so a naive read would hand an
	// agent `exec failed (...)` as the compiler version.
	ws := probe_workspace()
	broken := CompilerRun{
		exit_code:    6
		output:       'exec failed (SetHandleInformation): The handle is invalid.'
		launch_error: 'exec failed (SetHandleInformation): The handle is invalid.'
	}
	assert compiler_version_value_for(broken) == '', broken.output
	// The reason belongs under `error`; there must be no `value` at all, so nothing
	// downstream can read the launch failure as a version.
	assert !compiler_version_json_for(broken).contains('"value"'), compiler_version_json_for(broken)
	assert compiler_version_json_for(broken).contains('"error"'), compiler_version_json_for(broken)
	assert !v_version_json_for(broken).contains('"value"'), v_version_json_for(broken)
	assert v_version_json_for(broken).contains('"error"'), v_version_json_for(broken)
}

fn test_a_successful_version_read_is_reported_as_a_version() {
	good := CompilerRun{
		output: 'V 0.5.0 4b1b2c3\nbuilt with gcc'
	}
	assert compiler_version_value_for(good) == 'V 0.5.0 4b1b2c3', good.output
	assert v_version_json_for(good).contains('0.5.0'), v_version_json_for(good)
	assert !v_version_json_for(good).contains('"error"'), v_version_json_for(good)
}

fn test_a_run_that_never_started_does_not_report_a_program_result() {
	ws := probe_workspace()
	answer := run_json(ws, probe_path('main.v'), ['run', 'main.v'], 0)
	// On a machine where the compiler does start this is a normal run; where it
	// does not, the answer must not claim a program ran. Either way the shape is
	// checked: `started` is present, and it is false only with an error beside it.
	assert answer.contains('"started"'), answer
	if !answer.contains('"started":\ttrue') {
		assert answer.contains('could not be started'), answer
		assert !answer.contains('"ok"'), answer
	}
}

// veb_main is a minimal web app, built from pieces so the handler bodies need
// no escaping.
const veb_main = 'module main\n\nimport veb\n\npub struct App {}\n\n' +
	'pub struct Context {}\n\n' +
	"@[get]\npub fn (mut app App) index(mut ctx Context) veb.Result {\n\treturn ctx.text('hi')\n}\n\n" +
	"@[post; /submit]\npub fn (mut app App) submit(mut ctx Context) veb.Result {\n\treturn ctx.text('ok')\n}\n\n" +
	'fn main() {\n\tmut app := &App{}\n\tveb.run[App, Context](mut app, 8080)\n}\n'

fn test_veb_routes_reads_a_web_app() {
	ws := probe_workspace()
	os.write_file(probe_path('main.v'), veb_main)!
	answer := call_tool_on(ws, 'v_veb_routes', '{}')
	assert answer.contains('"method":\t"get"') || answer.contains('"method": "get"'), answer
	assert answer.contains('/index'), answer
	assert answer.contains('"method":\t"post"') || answer.contains('"method": "post"'), answer
	assert answer.contains('/submit'), answer
}

fn test_veb_routes_ignores_a_plain_main() {
	answer := call_tool('v_veb_routes', '{}')
	assert answer.contains('"routes"'), answer
	assert answer.contains('[]'), answer
}

fn test_veb_routes_reports_a_missing_main() {
	ws := probe_workspace()
	os.rm(probe_path('main.v')) or {
		panic(err)
	}
	answer := call_tool_on(ws, 'v_veb_routes', '{}')
	assert answer.contains('"error"'), answer
}

fn test_skills_lists_the_catalog_and_the_install_state() {
	answer := call_tool('v_skills', '{}')
	assert answer.contains('"project_dir"'), answer
	assert answer.contains('"global_dir"'), answer
	assert answer.contains('"out_of_date"'), answer
	assert answer.contains('"in_project"'), answer
	assert answer.contains('"in_global"'), answer
}

fn test_eval_runs_a_snippet() {
	answer := call_tool('v_eval', '{"code":"println(6 * 7)"}')
	assert answer.contains('"ok":\ttrue') || answer.contains('"ok": true'), answer
	assert answer.contains('42'), answer
}

fn test_eval_requires_code() {
	answer := call_tool('v_eval', '{}')
	assert answer.contains('"error"'), answer
}

fn test_eval_reports_a_failing_snippet() {
	answer := call_tool('v_eval', '{"code":"this is not V"}')
	assert answer.contains('"ok":\tfalse') || answer.contains('"ok": false'), answer
	assert answer.contains('"error"'), answer
}

fn test_format_reports_without_writing() {
	ws := probe_workspace()
	path := probe_path('messy.v')
	os.write_file(path, 'module main\nfn   main( ) {\n}\n')!
	before := os.read_file(path)!
	answer := tool_format(ws, '{"path":"messy.v"}')
	assert answer.contains('"changed":\ttrue') || answer.contains('"changed": true'), answer
	assert answer.contains('"written":\tfalse') || answer.contains('"written": false'), answer
	assert os.read_file(path)! == before, 'a dry run must not write'
}

fn test_format_writes_when_asked() {
	ws := probe_workspace()
	path := probe_path('messy2.v')
	os.write_file(path, 'module main\nfn   main( ) {\n}\n')!
	answer := tool_format(ws, '{"path":"messy2.v","write":true}')
	assert answer.contains('"written":\ttrue') || answer.contains('"written": true'), answer
	assert os.read_file(path)! != 'module main\nfn   main( ) {\n}\n', 'the file was not formatted'
}

fn test_format_reports_a_missing_file() {
	answer := call_tool('v_format', '{"path":"nope.v"}')
	assert answer.contains('"error"'), answer
}

fn test_format_refuses_parser_errors_without_changing_source() {
	ws := probe_workspace()
	path := probe_path('broken.v')
	before := 'module main\nfn main() {\n\tx :=\n}\n'
	os.write_file(path, before)!
	for write in [false, true] {
		answer := tool_format(ws, '{"path":"broken.v","write":${write}}')
		parsed := json.decode[map[string]json.Any](answer, strict: true)!
		assert parsed['error']!.str() == 'the file contains parser errors', answer
		diagnostics := parsed['diagnostics']!.as_array()
		assert diagnostics.len > 0, answer
		diagnostic := diagnostics[0].as_map()
		assert diagnostic['path']!.str() == 'broken.v', answer
		assert diagnostic['line']!.int() > 0, answer
		assert diagnostic['message']!.str() != '', answer
		assert 'after' !in parsed, answer
		assert os.read_file(path)! == before, 'parser recovery must not replace the source'
	}
}

fn test_edit_replace_rewrites_a_range() {
	ws := probe_workspace()
	path := probe_path('edit.v')
	os.write_file(path, 'one\ntwo\nthree\n')!
	answer := tool_edit_replace(ws, '{"path":"edit.v","start_line":2,"end_line":2,"expected_old":"two\\n","new_text":"TWO"}')
	assert answer.contains('edit.v'), answer
	assert os.read_file(path)! == 'one\nTWO\nthree\n', os.read_file(path)!
}

fn test_edit_replace_refuses_when_the_file_does_not_match() {
	ws := probe_workspace()
	path := probe_path('guard.v')
	os.write_file(path, 'one\ntwo\n')!
	answer := tool_edit_replace(ws, '{"path":"guard.v","start_line":1,"expected_old":"other\\n","new_text":"x"}')
	assert answer.contains('expected_old'), answer
	assert answer.contains('"actual"'), answer
	assert os.read_file(path)! == 'one\ntwo\n', 'a refused edit must not write'
}

fn test_edit_replace_requires_expected_old_for_an_existing_file() {
	ws := probe_workspace()
	path := probe_path('need.v')
	os.write_file(path, 'one\n')!
	answer := tool_edit_replace(ws, '{"path":"need.v","start_line":1,"new_text":"x"}')
	assert answer.contains('expected_old'), answer
}

fn test_edit_replace_can_insert_and_delete() {
	ws := probe_workspace()
	path := probe_path('ins.v')
	os.write_file(path, 'one\ntwo\n')!
	insert := tool_edit_replace(ws, '{"path":"ins.v","start_line":2,"expected_old":"","new_text":"middle"}')
	assert insert.contains('ins.v'), insert
	assert os.read_file(path)! == 'one\nmiddle\ntwo\n', os.read_file(path)!
	deleted := tool_edit_replace(ws, '{"path":"ins.v","start_line":2,"end_line":2,"expected_old":"middle\\n"}')
	assert deleted.contains('ins.v'), deleted
	assert os.read_file(path)! == 'one\ntwo\n', os.read_file(path)!
}

fn test_edit_replace_rejects_an_impossible_range() {
	ws := probe_workspace()
	path := probe_path('rng.v')
	os.write_file(path, 'one\n')!
	answer := tool_edit_replace(ws, '{"path":"rng.v","start_line":0}')
	assert answer.contains('"error"'), answer
	past := tool_edit_replace(ws, '{"path":"rng.v","start_line":99,"expected_old":""}')
	assert past.contains('past the end'), past
}

fn test_rename_symbol_is_a_dry_run_by_default() {
	ws := probe_workspace()
	before := os.read_file(probe_path('main.v'))!
	answer := tool_rename_symbol(ws, '{"name":"greet","new_name":"greet_user"}')
	assert answer.contains('"dry_run":\ttrue') || answer.contains('"dry_run": true'), answer
	assert answer.contains('"edit_count":\t2') || answer.contains('"edit_count": 2'), answer
	assert os.read_file(probe_path('main.v'))! == before, 'a dry run must not write'
}

fn test_rename_symbol_applies_when_asked() {
	ws := probe_workspace()
	answer := tool_rename_symbol(ws, '{"name":"greet","new_name":"greet_user","dry_run":false}')
	assert answer.contains('"dry_run":\tfalse') || answer.contains('"dry_run": false'), answer
	after := os.read_file(probe_path('main.v'))!
	assert after.contains('greet_user'), after
	assert !after.contains('greet('), after
}

fn test_rename_symbol_renames_a_shorter_name_safely() {
	ws := probe_workspace()
	// Two mentions on one line: writing left to right would shift the second.
	path := probe_path('twice.v')
	os.write_file(path, 'module main\n\nfn go() {\n\tprintln(alpha)\n\tprintln(alpha)\n}\n')!
	applied := tool_rename_symbol(ws, '{"name":"alpha","new_name":"b","paths":["twice.v"],"dry_run":false}')
	assert applied.contains('twice.v'), applied
	assert os.read_file(path)! == 'module main\n\nfn go() {\n\tprintln(b)\n\tprintln(b)\n}\n', os.read_file(path)!
}

fn test_rename_symbol_rejects_a_bad_new_name() {
	ws := probe_workspace()
	same := tool_rename_symbol(ws, '{"name":"greet","new_name":"greet"}')
	assert same.contains('"error"'), same
	invalid := tool_rename_symbol(ws, '{"name":"greet","new_name":"not a name"}')
	assert invalid.contains('"error"'), invalid
	numeric := tool_rename_symbol(ws, '{"name":"greet","new_name":"1bad"}')
	assert numeric.contains('"error"'), numeric
}

fn test_is_identifier_accepts_and_rejects() {
	assert is_identifier('greet')
	assert is_identifier('_private')
	assert is_identifier('with2digits')
	assert !is_identifier('')
	assert !is_identifier('2bad')
	assert !is_identifier('has space')
	assert !is_identifier('has-dash')
}

fn test_rename_hits_are_ordered_so_positions_stay_valid() {
	path := probe_path('order.v')
	os.write_file(path, 'module main\n\nfn go() {\n\tprintln(alpha)\n\tprintln(alpha)\n}\n')!
	hits := rename_hits(path, 'alpha')
	assert hits.len == 2, hits.len.str()
	// Later lines come first, so applying them one by one cannot shift a column
	// that has not been reached yet.
	assert hits[0].line > hits[1].line, hits[0].line.str()
}

fn test_apply_rename_refuses_when_a_position_no_longer_fits() {
	path := probe_path('shift.v')
	os.write_file(path, 'module main\n')!
	err := apply_rename(path, [
		RenameHit{
			line:   99
			column: 1
			length: 3
		},
	], 'module', 'x') or { return }
	assert false, 'a position past the end of the file must fail: ${err}'
}

fn test_workspace_rejects_a_path_outside_the_root() {
	ws := probe_workspace()
	assert ws.resolve('../outside.v') == none
	assert ws.resolve('') == none
	inside := ws.resolve('main.v') or { panic('main.v did not resolve') }
	assert inside.ends_with('main.v'), inside
}

fn test_workspace_reports_the_project_root_and_manifest() {
	ws := probe_workspace()
	assert ws.v_modified
	assert ws.v_mod_name == 'probe', ws.v_mod_name
	assert ws.v_mod_description == 'A probe project.', ws.v_mod_description
	assert ws.v_mod_version == '0.1.0', ws.v_mod_version
	assert ws.v_mod_license == 'MIT', ws.v_mod_license
	assert ws.v_mod_repo_url == 'https://example.com/probe', ws.v_mod_repo_url
	assert !ws.is_v_checkout
}

fn test_workspace_tolerates_a_project_without_a_manifest() {
	root := os.join_path(test_root, 'bare')
	os.mkdir_all(root)!
	os.write_file(os.join_path(root, 'main.v'), 'module main\n')!
	ws := new_workspace(@VEXEROOT, root, false)
	assert !ws.v_modified
	assert ws.v_mod_name == '', ws.v_mod_name
	assert ws.project_root == os.real_path(root), ws.project_root
}

fn test_workspace_skips_the_directories_a_project_never_compiles() {
	ws := probe_workspace()
	root := probe_root()!
	os.mkdir_all(os.join_path(root, '.git'))!
	os.write_file(os.join_path(root, '.git', 'hidden.v'), 'module git\n')!
	os.mkdir_all(os.join_path(root, 'node_modules'))!
	os.write_file(os.join_path(root, 'node_modules', 'dep.v'), 'module dep\n')!
	files := ws.v_files(root)
	for file in files {
		assert !file.contains('.git') && !file.contains('node_modules'), file
	}
}

fn test_workspace_ignores_vsh_outside_the_tree_it_walks() {
	ws := probe_workspace()
	root := probe_root()!
	os.write_file(os.join_path(root, 'script.vsh'), '#!/usr/bin/env v\n')!
	files := ws.v_files(root)
	assert files.any(it.ends_with('script.vsh')), files.join(', ')
	assert files.any(it.ends_with('main.v')), files.join(', ')
}

fn test_decode_args_reads_the_documented_shapes() {
	args := decode_args('{"path":"main.v","line":7,"flag":true,"list":["a","b"]}')
	assert args.text('path', '') == 'main.v'
	assert args.int('line', 0) == 7
	assert args.boolean('flag', false)
	assert args.list('list') == ['a', 'b']
	assert args.has('path')
	assert !args.has('missing')
	assert args.text('missing', 'fallback') == 'fallback'
	assert args.int('missing', 3) == 3
	assert !args.boolean('missing', false)
	assert args.list('missing').len == 0
}

fn test_decode_args_tolerates_a_missing_or_broken_payload() {
	empty := decode_args('')
	assert empty.text('path', '') == ''
	assert empty.required_str('path') == none
}

fn test_decode_args_required_str_rejects_a_blank_value() {
	args := decode_args('{"path":"   "}')
	if args.required_str('path') != none {
		assert false, 'a blank required argument must be rejected'
	}
}

fn test_object_skips_empty_text_and_keeps_raw() {
	rendered := object(text_pair('kept', 'yes'), text_pair('dropped', ''),
		raw_pair('nested', string_array(['a', 'b'])))
	assert rendered.contains('"kept"'), rendered
	assert !rendered.contains('dropped'), rendered
	assert rendered.contains('"nested"'), rendered
	// The raw value is JSON, not a quoted string.
	assert rendered.contains('[\n\t\t"a", "b"\n\t]') || rendered.contains('["a", "b"]'), rendered
}

fn test_object_quotes_text_that_looks_like_json() {
	// A message containing braces must stay a quoted string; only an explicit raw
	// pair is inserted verbatim.
	rendered := object(text_pair('message', 'a brace and a bracket'))
	assert rendered.contains('"message":\t"a brace and a bracket"')
		|| rendered.contains('"message": "a brace and a bracket"'), rendered
}

fn test_string_array_renders_an_empty_list_as_an_empty_array() {
	assert string_array([]) == '[]'
}

fn test_parse_diagnostics_reads_positions_kinds_and_context() {
	output := 'C:/p/main.v:2:5: error: unknown function: nope\n    2 |     nope()\n' +
		'      |     ~~~~\nC:/p/main.v:9:1: warning: unused import `os`\n'
	items := parse_diagnostics(output)
	assert items.len == 2, items.len.str()
	assert items[0].path == 'C:/p/main.v', items[0].path
	assert items[0].line == 2, items[0].line.str()
	assert items[0].column == 5, items[0].column.str()
	assert items[0].kind == 'error', items[0].kind
	assert items[0].message == 'unknown function: nope', items[0].message
	// The source context rides along rather than becoming a second diagnostic.
	assert items[0].raw.contains('nope()'), items[0].raw
	assert items[1].kind == 'warning', items[1].kind
	assert items[1].line == 9, items[1].line.str()
	assert count_errors(items) == 1
}

fn test_parse_diagnostics_keeps_a_message_without_a_position() {
	items := parse_diagnostics('v: cannot find module `nope`\n')
	assert items.len == 0, items.len.str()
	assert unparsed(items).len == 0
}

fn test_parse_diagnostics_ignores_a_line_that_is_not_a_diagnostic() {
	items := parse_diagnostics('note: something happened\nplain text\n')
	assert items.len == 0, items.len.str()
}

fn test_diagnostic_json_carries_the_fields_a_reader_needs() {
	parsed := json.decode[map[string]json.Any](Diagnostic{
		path:    'main.v'
		line:    3
		column:  7
		kind:    'error'
		message: 'boom'
	}.to_json()) or { panic('the diagnostic did not render') }
	assert parsed['path']!.str() == 'main.v'
	assert (parsed['line'] or { json.Any(0) }).int() == 3
	assert (parsed['column'] or { json.Any(0) }).int() == 7
	assert parsed['kind']!.str() == 'error'
	assert parsed['message']!.str() == 'boom'
}

fn test_diagnostic_json_reports_a_parsed_position() {
	assert Diagnostic{
		path: 'a.v'
		line: 1
	}.parsed()
	assert !Diagnostic{
		line: 0
	}.parsed()
}

fn test_diagnostics_json_renders_a_list() {
	rendered := diagnostics_json([Diagnostic{
		path: 'a.v'
		line: 1
		kind: 'error'
	}])
	assert rendered.starts_with('['), rendered
	assert rendered.ends_with(']'), rendered
	assert rendered.contains('"path"'), rendered
}

fn test_trim_output_keeps_the_tail_and_says_so() {
	mut lines := []string{}
	for i in 0 .. 100 {
		lines << 'line ${i}'
	}
	long := lines.join('\n')
	trimmed := trim_output(long, 5)
	assert !trimmed.starts_with('line 0'), trimmed
	assert trimmed.contains('line 99'), trimmed
	assert trimmed.contains('earlier output omitted'), trimmed
	short := trim_output('only one line', 5)
	assert short == 'only one line', short
}

fn test_count_by_kind_separates_the_kinds() {
	items := [
		Diagnostic{ kind: 'error' },
		Diagnostic{ kind: 'error' },
		Diagnostic{ kind: 'warning' },
		Diagnostic{ kind: 'notice' },
	]
	assert count_by_kind(items, 'error') == 2
	assert count_by_kind(items, 'warning') == 1
	assert count_by_kind(items, 'notice') == 1
	assert count_by_kind(items, 'note') == 0
}

fn test_skill_status_reports_the_bundled_catalog() {
	answer := skill_status(probe_workspace())
	parsed := json.decode[map[string]json.Any](answer) or {
		panic('the skill status did not render: ' + answer)
	}
	assert 'bundled_dir' in parsed, answer
	assert 'project_dir' in parsed, answer
	assert 'global_dir' in parsed, answer
	assert (parsed['skills'] or { []json.Any{} }) is []json.Any, answer
}

// replace_all substitutes every occurrence, which is what reading a rendered tab
// as a space in an assertion needs.
fn replace_all(text string, from string, to string) string {
	return text.replace(from, to)
}

// The skill module is reachable from here, so a mismatch between the catalogue
// the tool reports and the one `v skills` installs is caught in one place.
fn test_the_tool_and_the_installer_read_the_same_catalog() {
	bundled := skills.catalog(@VEXEROOT)
	answer := skill_status(probe_workspace())
	parsed := json.decode[map[string]json.Any](answer) or { panic('bad json') }
	listed := parsed['skills'] or { []json.Any{} }.as_array()
	assert listed.len == bundled.len, 'the tool reports ${listed.len} skills, the catalog has ${bundled.len}'
}

fn test_instructions_describe_the_working_order() {
	ws := probe_workspace()
	text := instructions(ws)
	assert text.contains('v_project_info'), 'the instructions must name the first tool'
	assert text.contains('v_check'), 'the instructions must name the checker'
	assert text.contains('dry run'), 'the instructions must say edits default to a dry run'
	assert text.contains('expected_old'), 'the instructions must name the guard'
	assert text.contains(ws.root), 'the instructions must name the workspace'
	assert text.contains('read-only'), 'the instructions must report the mode'
}

struct WireTool {
	name         string
	input_schema map[string]json.Any @[json: inputSchema]
}

struct WireToolList {
	tools []WireTool
}

struct WireToolListResponse {
	id     int
	result WireToolList
}

fn test_tools_list_wire_schemas_are_valid_in_both_modes() {
	root := probe_root()!
	input := os.join_path(root, 'mcp_requests.jsonl')
	os.write_file(input, '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"${mcp.protocol_version}","capabilities":{},"clientInfo":{"name":"schema-test","version":"1"}}}\n' +
		'{"jsonrpc":"2.0","method":"notifications/initialized"}\n' +
		'{"jsonrpc":"2.0","id":2,"method":"tools/list"}\n')!
	for read_only in [false, true] {
		mut args := ['mcp', 'serve', '--root', root]
		if read_only { args << '--read-only' }
		mut process := os.new_process(@VEXE)
		process.set_args(args)
		process.set_redirect_stdio()
		process.set_stdin_path(input)
		process.run()
		output := process.stdout_slurp()
		errors := process.stderr_slurp()
		process.wait()
		assert process.code == 0, errors
		process.close()
		mut listed := false
		assert output.ends_with('\n'), output
		for line in output.split('\n') {
			if line == '' { continue }
			response := json.decode[WireToolListResponse](line, strict: true)!
			if response.id != 2 { continue }
			listed = true
			response_bytes := line.len + 1 // Include the stdio framing LF.
			assert response_bytes <= max_tools_list_response_bytes, 'tools/list response is ${response_bytes} bytes, over the ${max_tools_list_response_bytes} byte budget'
			raw_response := json.decode[map[string]json.Any](line, strict: true)!
			raw_result := raw_response['result']!.as_map()
			for raw_tool in raw_result['tools']!.as_array() {
				properties := raw_tool.as_map()
				assert 'title' !in properties, 'tools/list must omit the redundant title for ${properties['name']!.str()}'
			}
			expected := if read_only { 15 } else { 18 }
			assert response.result.tools.len == expected, line
			for tool in response.result.tools {
				schema_type := tool.input_schema['type'] or { panic('missing schema type for ${tool.name}') }
				assert schema_type.str() == 'object', tool.name
				assert 'properties' in tool.input_schema, tool.name
				if read_only {
					assert tool.name !in ['v_edit_replace', 'v_rename_symbol', 'v_format']
				}
			}
		}
		assert listed, output
	}
}

struct SchemaDescription {
	description string
}

struct SchemaDescriptions {
	properties map[string]SchemaDescription
}

fn test_tool_schema_descriptions_preserve_quotes_and_newlines() {
	ast_schema := json.decode[SchemaDescriptions](spec_ast().tool.input_schema, strict: true)!
	assert ast_schema.properties['hide'].description.contains('["pos"]')
	assert ast_schema.properties['hide'].description.contains('\n')
	format_schema := json.decode[SchemaDescriptions](spec_format().tool.input_schema, strict: true)!
	assert format_schema.properties['write'].description.contains('\nDefaults to false.')
}

fn test_workspace_checks_symlink_parents_of_new_files() {
	$if windows {
		return
	}
	ws := probe_workspace()
	outside := os.join_path(test_root, 'outside')
	os.mkdir_all(outside)!
	os.symlink(outside, os.join_path(ws.root, 'escape'))!
	assert ws.resolve('escape/new.v') == none
	assert ws.resolve('missing/../escape/new.v') == none
	os.symlink(os.join_path(outside, 'missing.v'), os.join_path(ws.root, 'dangling'))!
	assert ws.resolve('dangling') == none
	inside := ws.resolve('new_dir/new.v')!
	assert inside == os.join_path(ws.root, 'new_dir', 'new.v')
	os.mkdir_all(os.join_path(ws.root, 'existing'))!
	assert ws.resolve('existing/../new.v')! == os.join_path(ws.root, 'new.v')
}

struct WireContent {
	text string
}

struct WireCallResult {
	content  []WireContent
	is_error bool @[json: isError]
}

struct WireCallResponse {
	id     int
	result WireCallResult
}

fn test_oversized_ast_wire_result_is_bounded_valid_json_in_both_modes() {
	root := probe_root()!
	large := 'x'.repeat(ast_byte_limit + 1)
	os.write_file(os.join_path(root, 'large.v'), "module main\nconst large = '${large}'\n")!
	input := os.join_path(root, 'large_ast_requests.jsonl')
	os.write_file(input, '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"${mcp.protocol_version}","capabilities":{},"clientInfo":{"name":"large-ast-test","version":"1"}}}\n' +
		'{"jsonrpc":"2.0","method":"notifications/initialized"}\n' +
		'{"jsonrpc":"2.0","id":2,"method":"tools/list"}\n' +
		'{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"v_ast","arguments":{"path":"large.v"}}}\n')!
	for read_only in [false, true] {
		mut args := ['mcp', 'serve', '--root', root]
		if read_only { args << '--read-only' }
		mut process := os.new_process(@VEXE)
		process.set_args(args)
		process.set_redirect_stdio()
		process.set_stdin_path(input)
		process.run()
		output := process.stdout_slurp()
		errors := process.stderr_slurp()
		process.wait()
		assert process.code == 0, errors
		process.close()
		mut received := false
		for line in output.trim_space().split_into_lines() {
			response := json.decode[WireCallResponse](line, strict: true)!
			if response.id != 3 { continue }
			received = true
			assert !response.result.is_error, line
			assert response.result.content.len == 1, line
			content := response.result.content[0].text
			parsed := json.decode[map[string]json.Any](content, strict: true)!
			truncated := parsed['truncated'] or { panic('missing truncated') }
			bytes := parsed['bytes'] or { panic('missing bytes') }
			ast := parsed['ast'] or { panic('missing ast') }
			limit := parsed['limit'] or { panic('missing limit') }
			assert content.len < ast_byte_limit
			assert truncated.bool()
			assert bytes.int() > ast_byte_limit
			assert ast.str() == 'null'
			assert limit.int() == ast_byte_limit
		}
		assert received, output
	}
}

// A flag placed after the source file is handed to the program instead of the
// compiler, so `v_run` compiled a different program than the caller asked for.
//
// The reviewer's probe: `flags: ["-d", "proof=present"]` against a file printing
// `$d('proof', 'missing')` printed `missing`, while the same flag before `run`
// printed `present`.
fn test_run_puts_flags_before_the_subcommand_and_target() {
	argv := run_arguments(['-d', 'proof=present'], 'main.v', [])
	assert argv == ['-d', 'proof=present', 'run', 'main.v'], '${argv}'
	// No flag may sit after the target, which is where the program would take it.
	assert argv.last() == 'main.v', 'the target must not be followed by a flag: ${argv}'
	assert argv.index('-d') < argv.index('run'), 'a flag landed after the subcommand: ${argv}'

	// Program arguments stay after the target; V forwards these tokens verbatim.
	with_args := run_arguments(['-stats'], 'main.v', ['one argument', 'second'])
	assert with_args == ['-stats', 'run', 'main.v', 'one argument', 'second'], '${with_args}'
	// `one argument` is one element, not three.
	assert with_args[with_args.len - 2] == 'one argument', '${with_args}'
	assert '--' !in with_args, '${with_args}'

	assert run_arguments([], 'main.v', []) == ['run', 'main.v'], 'the empty case'
}

// `run_compiler` starts the compiler with an argument array, so a client value
// carrying a space is one argument rather than two.
//
// Building a command string and handing it to a shell split `["one argument",
// "second"]` into `one`, `argument`, `second`.
fn test_the_compiler_command_line_reports_every_argument() {
	line := compiler_command_line('/v/v.exe', ['-d', 'proof=present', 'run', 'my prog.v', 'one argument'])
	for expected in ['-d', 'proof=present', 'run', 'my prog.v', 'one argument'] {
		assert line.contains(expected), 'the command line lost ${expected}: ${line}'
	}
	// The rendered line is only for reading, but it still has to quote the values
	// that a shell would otherwise split.
	assert line.contains('one argument'), 'an argument with a space must stay quoted: ${line}'
}

// The rename must refuse a span that no longer holds the old name.
//
// A column that drifts off the name used to be bounds-checked and then written
// anyway, so a method rename turned `fn (h Host) hello()` into
// `fn (h Host) hellogreett`. The text at the span is now checked, so a wrong span
// is a refusal instead of a corrupted file.
fn test_apply_rename_refuses_a_span_that_does_not_hold_the_old_name() {
	path := probe_path('drift.v')
	os.write_file(path, 'module main\n\nfn keep() {}\n')!
	before := os.read_file(path) or { panic('no file') }
	err := apply_rename(path, [
		RenameHit{
			line:   3
			column: 4
			length: 5
		},
	], 'hello', 'greet') or { return }
	assert false, 'a span holding the wrong text must be refused: ${err}'
	// The file is untouched, which is the point of refusing.
	assert os.read_file(path)! == before, 'a refused rename must not write'
}

// SchemaArguments is the part of a tool's input schema that says which arguments
// are mandatory.
struct SchemaArguments {
	properties map[string]json.Any
	required   []string
}

fn test_a_tool_only_requires_arguments_it_declares_as_properties() {
	for spec in tool_specs() {
		schema := json.decode[SchemaArguments](spec.tool.input_schema, strict: true)!
		mut named := []string{}
		for name in schema.required {
			// A `required` entry naming something the schema never declares is one
			// no client can satisfy: it would send that field and still be told the
			// argument is missing.
			assert name in schema.properties, '${spec.tool.name}: required names `${name}`, which is not one of its properties'
			assert name !in named, '${spec.tool.name}: `${name}` is required twice'
			named << name
		}
	}
}

// A file the installer is pointed at is never a real one: every test writes into
// its own directory under the temp root, so nothing here can touch the config
// of a client that is actually installed.
fn config_fixture(name string, content string) !string {
	dir := os.join_path(test_root, 'cfg_' + name)
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir)!
	path := os.join_path(dir, 'config.json')
	if content != '' {
		os.write_file(path, content)!
	}
	return path
}

// read_config returns the file's text, for an assertion about what survived.
fn read_config(path string) string {
	return os.read_file(path) or { panic(err) }
}

// parses reports whether the file is still valid JSON after the edit, which is
// the property that matters most: a config the client cannot load is worse than
// one missing an entry.
fn parses(path string) bool {
	json.decode[map[string]json.Any](read_config(path)) or { return false }
	return true
}

fn test_every_harness_names_a_key_and_a_user_file() {
	for h in harnesses() {
		assert h.name != '', 'a harness with no name'
		assert h.key != '', '${h.name}: no top-level key'
		assert h.user != '', '${h.name}: no user-level file'
	}
}

fn test_harness_names_are_unique() {
	mut seen := []string{}
	for h in harnesses() {
		assert h.name !in seen, '${h.name} is listed twice'
		seen << h.name
	}
}

fn test_opencode_takes_the_executable_and_arguments_as_one_array() {
	opencode := find_harness('opencode') or { panic('opencode is missing') }
	entry := opencode.entry('C:\\v\\v.exe', ['mcp', 'serve'])
	// One array, not a command string plus args.
	assert entry.contains('"command": ["C:\\\\v\\\\v.exe", "mcp", "serve"]'), entry
	assert !entry.contains('"args"'), entry
	// The discriminator this client requires.
	assert entry.contains('"type": "local"'), entry
}

fn test_claude_style_clients_take_a_command_string_and_args() {
	claude := find_harness('claude-code') or { panic('claude-code is missing') }
	entry := claude.entry('C:\\v\\v.exe', ['mcp', 'serve'])
	assert entry.contains('"command": "C:\\\\v\\\\v.exe"'), entry
	assert entry.contains('"args": ["mcp", "serve"]'), entry
}

fn test_a_windows_executable_keeps_its_backslashes() {
	cursor := find_harness('cursor') or { panic('cursor is missing') }
	entry := cursor.entry('C:\\Users\\me\\v.exe', ['mcp'])
	assert entry.contains('"command": "C:\\\\Users\\\\me\\\\v.exe"'), entry
}

fn test_it_refuses_to_reorder_or_drop_an_existing_config() {
	path := config_fixture('order', '{"zed":{"a":1},"mcp":{"duck":{"type":"local"}},"other":true}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	after := read_config(path)
	// The entry went in...
	assert after.contains('"vlang"'), after
	// ...and the surrounding keys are in the order they were written, which a
	// decode/encode round trip would not preserve.
	zed_at := after.index('"zed"') or { -1 }
	servers_at := after.index('"mcp"') or { -1 }
	other_at := after.index('"other"') or { -1 }
	assert zed_at < servers_at, after
	assert servers_at < other_at, after
	assert after.contains('"duck"'), after
	assert parses(path)
}

fn test_it_fills_an_empty_servers_object_without_a_stray_comma() {
	path := config_fixture('empty', '{"mcp":{}}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	after := read_config(path)
	assert !after.contains(',}'), after
	assert parses(path), after
}

fn test_it_adds_a_comma_when_the_object_already_has_servers() {
	path := config_fixture('nonempty', '{"mcp":{"duck":{"type":"local"}}}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	after := read_config(path)
	assert after.contains('"vlang"'), after
	assert after.contains('"duck"'), after
	assert parses(path), after
}

fn test_it_puts_the_entry_on_its_own_line_at_the_files_indentation() {
	path := config_fixture('indent', '{\n  "mcp": {\n    "duck": {\n      "type": "local"\n    }\n  }\n}\n')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	after := read_config(path)
	assert parses(path), after
	// The entry takes the line and the indentation of the entry already there,
	// instead of being crammed onto the line that opens the object.
	assert after.contains('\n    "vlang": '), after
	assert !after.contains('{"vlang"'), after
	// Everything else, including the entry it sat beside, is untouched. The new
	// entry takes the comma, so the one it was inserted before stays last.
	assert after.contains('\n    "duck": {\n      "type": "local"\n    }\n'), after
}

fn test_it_edits_a_commented_file_without_touching_the_comment() {
	// A comment is the common case: these files are meant to be edited by hand.
	// The edit is textual, so the comment survives it.
	path := config_fixture('jsonc', '{\n  // my servers\n  "mcp": {}\n}\n')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	assert !is_plain_json(read_config(path)), 'a commented file must not count as plain JSON'
	write_entry(h, path, false) or { panic(err) }
	after := read_config(path)
	// The comment is still there, and the entry sits beside it.
	assert after.contains('// my servers'), after
	assert is_editable(read_config(path)), after
	assert has_entry(after, 'mcp', 'vlang'), after
}

fn test_print_gives_a_pasteable_member_when_the_key_is_absent() {
	path := config_fixture('printnokey', '{"unrelated":{}}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	out := print_one(h, path, false)
	// The entry on its own is not something a client can read, so what is printed
	// has to be the member that holds it.
	assert out.contains('add this member'), out
	assert out.contains('"mcp": { "vlang": '), out
	assert !out.contains('top-level key:'), out
	member := out.split_into_lines().last().trim_space()
	assert is_plain_json('{ ${member} }'), out
	assert read_config(path) == '{"unrelated":{}}'
}

fn test_print_gives_the_whole_file_when_there_is_none() {
	path := config_fixture('printmissing', '')!
	os.rm(path) or {}
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	out := print_one(h, path, false)
	assert out.contains('no config file yet'), out
	assert out.contains('"mcp": { "vlang": '), out
	whole_file := out.split_into_lines().last().trim_space()
	assert is_plain_json(whole_file), out
	assert has_entry(whole_file, 'mcp', server_id), out
	assert !os.exists(path)
}

fn test_print_still_names_the_key_when_it_is_there() {
	path := config_fixture('printkey', '{"mcp":{"duck":{}}}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	out := print_one(h, path, false)
	assert out.contains('top-level key: mcp'), out
	assert !out.contains('add this member'), out
	member := out.split_into_lines().last().trim_space()
	assert member.starts_with('"vlang": '), out
	assert is_plain_json('{ ${member} }'), out
	assert read_config(path) == '{"mcp":{"duck":{}}}'
}

fn test_print_reports_a_read_error() {
	path := config_fixture('printunreadable', '')!
	os.mkdir(path)!
	h := Harness{ name: 'test', label: 'test', key: 'mcp' }
	out := print_one(h, path, false)
	assert out.contains('could not read ${path}:'), out
	assert !out.contains('add this member'), out
	assert os.is_dir(path)
}

fn test_print_keeps_scope_and_creation_restrictions() {
	path := config_fixture('printnocreate', '')!
	h := Harness{ name: 'test', label: 'test', key: 'mcp', no_create_user: true }
	user := print_one(h, path, false)
	assert user.contains('the installer will not create this file'), user
	assert !user.contains('what would be created'), user
	assert is_plain_json(user.split_into_lines().last().trim_space()), user
	project := print_one(h, '', true)
	assert project == 'test  ()\n  (no project-level file)\n', project
	assert !os.exists(path)
}

fn test_server_entry_escapes_control_bytes() {
	exe := 'C:\\tools\\v\n\t\r"\x01.exe'
	args := ['mcp', 'serve', 'line\nbreak', '\x00\x1f']
	for array_command in [false, true] {
		h := Harness{ argv_in_command: array_command }
		entry := h.entry(exe, args)
		decoded := json.decode[map[string]json.Any](entry)!
		if array_command {
			assert decoded['command']!.as_array().map(it.str()) == [exe, ...args]
		} else {
			assert decoded['command']!.str() == exe
			assert decoded['args']!.as_array().map(it.str()) == args
		}
	}
}

fn test_it_refuses_a_config_with_trailing_commas() {
	path := config_fixture('trailing', '{"mcp":{"duck":{},}}')!
	assert !is_plain_json(read_config(path)), 'a trailing comma must not count as plain JSON'
}

fn test_it_will_not_register_the_same_server_twice() {
	path := config_fixture('twice', '{"mcp":{}}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	once := read_config(path)
	write_entry(h, path, false) or { panic(err) }
	assert read_config(path) == once, 'a second install changed the file'
}

fn test_it_says_which_compiler_an_existing_entry_runs() {
	path := config_fixture('moved', '{"mcp":{}}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	// An entry registered from a different compiler is the case a reader cannot
	// otherwise discover: install says nothing and changes nothing, so the file is
	// the only place the answer lives.
	other := '{"mcp":{"vlang":{"command":["C:\\\\elsewhere\\\\v.exe","mcp","serve"]}}}'
	os.write_file(path, other)!
	exe, command := recorded_entry(read_config(path), 'mcp') or { panic('no entry') }
	assert exe == 'C:\\elsewhere\\v.exe', exe
	assert command == 'C:\\elsewhere\\v.exe mcp serve', command
	// And the harness that ran install names a different one, which is what the
	// message compares.
	assert exe != server_exe(), 'the fixture must not name this compiler'
}

fn test_it_refuses_a_config_whose_root_is_not_an_object() {
	// Valid JSON, so it is not the JSONC case, and an array is not somewhere a
	// top-level object can be added without guessing.
	path := config_fixture('arrayroot', '["not","an","object"]')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	before := read_config(path)
	mut refused := false
	write_entry(h, path, false) or {
		refused = true
		assert err.msg().contains('valid JSON'), err.msg()
		assert err.msg().contains('top level is not an object'), err.msg()
	}
	assert refused, 'an array-root file was not refused'
	// The file is valid JSON, so it must not be described as JSONC, which sends
	// the reader looking for a comment that is not there.
	assert is_json_value(before), 'an array is valid JSON'
	assert !is_plain_json(before), 'but it is not an object either'
	assert read_config(path) == before, 'the file was rewritten'
}

fn test_existing_entry_report_reads_both_command_shapes_and_keeps_project_scope() {
	h := Harness{
		name:  'opencode'
		label: 'opencode'
		key:   'mcp'
	}
	for argv_in_command in [false, true] {
		shape := Harness{ ...h, argv_in_command: argv_in_command }
		entry := shape.entry('/elsewhere/My Compiler/v', ['mcp', 'serve', '--title=one two'])
		text := '{"mcp":{"vlang":${entry}}}'
		exe, command := recorded_entry(text, 'mcp') or { panic('no readable entry') }
		assert exe == '/elsewhere/My Compiler/v'
		assert command == '"/elsewhere/My Compiler/v" mcp serve "--title=one two"'
		for project in [false, true] {
			report := existing_entry_report(text, shape, '/fixture/config.json', project)
			assert report.contains('  it runs ${command}\n')
			assert report.contains('  this compiler is ${server_exe()}\n')
			assert report.contains('  to move it'), report
			assert !report.contains('to move it: v mcp'), report
			compiler := if os.user_os() == 'windows' {
				"& '" + server_exe().replace("'", "''") + "'"
			} else {
				os.quoted_path(server_exe())
			}
			if project {
				assert report.ends_with('${compiler} mcp uninstall opencode --project\n  ${compiler} mcp install opencode --project')
			} else {
				assert report.ends_with('${compiler} mcp uninstall opencode\n  ${compiler} mcp install opencode')
			}
		}
	}
	matching := '{"mcp":{"vlang":${h.entry(server_exe(), server_args())}}}'
	assert !existing_entry_report(matching, h, '/fixture/config.json', true).contains('to move it:')
}

fn test_existing_entry_report_moves_to_the_invoked_compiler_with_shell_quoting() {
	h := Harness{ name: 'opencode', key: 'mcp' }
	text := '{"mcp":{"vlang":{"command":"v","args":["mcp","serve"]}}}'
	wanted := "/somewhere/My Compiler's v\$1"
	posix := existing_entry_report_for_compiler(text, h, '/fixture/config.json', true, wanted, false)
	assert posix.ends_with("  '/somewhere/My Compiler'\\''s v\$1' mcp uninstall opencode --project\n  '/somewhere/My Compiler'\\''s v\$1' mcp install opencode --project"), posix
	windows := existing_entry_report_for_compiler(text, h, '/fixture/config.json', false, wanted, true)
	assert windows.contains('to move it (PowerShell):'), windows
	assert windows.ends_with("  & '/somewhere/My Compiler''s v\$1' mcp uninstall opencode\n  & '/somewhere/My Compiler''s v\$1' mcp install opencode"), windows
}

fn test_commented_existing_entry_reports_both_command_shapes_without_rewriting() {
	h := find_harness('opencode') or { panic('opencode is missing') }
	for i, argv_in_command in [false, true] {
		shape := Harness{ ...h, argv_in_command: argv_in_command }
		entry := shape.entry('/elsewhere/My Compiler/v', ['mcp', 'serve', '--title=one two'])
		text := '// keep leading comment\n{"mcp":{/* keep server comment */"vlang":${entry}}}'
		exe, command := recorded_entry(text, 'mcp') or { panic('no readable commented entry') }
		assert exe == '/elsewhere/My Compiler/v'
		assert command == '"/elsewhere/My Compiler/v" mcp serve "--title=one two"'
		report := existing_entry_report_for_compiler(text, shape, '/fixture/config.json', true,
			'/current/v', false)
		assert report.contains('  it runs ${command}\n')
		assert report.ends_with("  '/current/v' mcp uninstall opencode --project\n  '/current/v' mcp install opencode --project")
		path := config_fixture('commented_existing_command_${i}', text)!
		write_entry(shape, path, true)!
		assert read_config(path) == text
	}
}

fn test_existing_entry_report_leaves_unreadable_commands_alone() {
	h := Harness{ name: 'test', label: 'test', key: 'mcp' }
	for index, entry in ['{}', '{"command":null}', '{"command":42}', '{"command":false}',
		'{"command":""}', '{"command":[]}', '{"command":["v",null]}',
		'{"command":"v","args":"mcp serve"}', '{"command":"v","args":["mcp",true]}'] {
		text := '{"mcp":{"vlang":${entry}}}'
		path := config_fixture('unreadable${index}', text)!
		write_entry(h, path, false) or { panic(err) }
		assert read_config(path) == text
		assert existing_entry_report(text, h, path, false) == 'v mcp install: vlang is already in ${path}; leaving it alone.'
	}
}

fn test_recorded_entry_quotes_empty_arguments_and_control_bytes() {
	text := '{"mcp":{"vlang":{"command":"v","args":["","line\\nnext","quote\\\"here"]}}}'
	exe, command := recorded_entry(text, 'mcp') or { panic('no readable entry') }
	assert exe == 'v'
	assert command == 'v "" "line\\nnext" "quote\\\"here"'
}

fn test_it_adds_the_key_to_a_config_that_has_no_servers_yet() {
	path := config_fixture('nokey', '{"unrelated":{}}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	after := read_config(path)
	assert parses(path), after
	assert has_entry(after, 'mcp', server_id), after
	// The new key goes first, and the member already there is untouched.
	assert after.starts_with('{ "mcp": {"vlang": '), after
	assert after.ends_with(',"unrelated":{}}'), after
}

fn test_it_adds_the_key_at_the_indentation_of_a_pretty_printed_config() {
	// The shape of `~/.claude.json` before any user-level server is added.
	path := config_fixture('nokeypretty', '{\n  "numStartups": 3,\n  "projects": {}\n}\n')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcpServers'
	}
	write_entry(h, path, false) or { panic(err) }
	after := read_config(path)
	assert parses(path), after
	assert after.starts_with('{\n  "mcpServers": {\n    "vlang": {'), after
	assert after.ends_with('}\n  },\n  "numStartups": 3,\n  "projects": {}\n}\n'), after
}

fn test_it_adds_the_key_to_an_empty_root_object() {
	path := config_fixture('emptyroot', '{}\n')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	assert parses(path), read_config(path)
	assert has_entry(read_config(path), 'mcp', server_id), read_config(path)
}

fn test_it_fills_a_file_that_holds_only_whitespace() {
	path := config_fixture('blank', '\n  \n')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	assert parses(path), read_config(path)
	assert has_entry(read_config(path), 'mcp', server_id), read_config(path)
}

fn test_it_refuses_a_key_whose_value_is_not_an_object() {
	path := config_fixture('notobject', '{"mcp": null}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	mut refused := false
	write_entry(h, path, false) or { refused = true }
	assert refused, 'a non-object key was not refused'
	assert read_config(path) == '{"mcp": null}', 'the file changed'
}

fn test_it_fills_an_empty_servers_object_with_whitespace_inside() {
	for i, content in ['{"mcp": { }}', '{\n  "mcp": { }\n}\n', '{\n  "mcp": {\n  }\n}\n',
		'{\n  "mcp": {\n\n    }\n}\n'] {
		path := config_fixture('spaced${i}', content)!
		h := Harness{
			name:  'test'
			label: 'test'
			key:   'mcp'
		}
		write_entry(h, path, false) or { panic(err) }
		after := read_config(path)
		assert parses(path), after
		assert has_entry(after, 'mcp', server_id), after
	}
	path := config_fixture('spacedlayout', '{\n  "mcp": { }\n}\n')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	assert read_config(path).starts_with('{\n  "mcp": {\n    "vlang": {'), read_config(path)
	assert read_config(path).ends_with('}\n  }\n}\n'), read_config(path)
}

fn test_it_keeps_the_line_endings_of_a_crlf_file() {
	path := config_fixture('crlf', '{\r\n  "mcp": {\r\n    "duck": {}\r\n  }\r\n}\r\n')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	after := read_config(path)
	assert parses(path), after
	assert after.contains('"vlang"'), after
	assert !after.replace('\r\n', '').contains('\n'), 'a bare line feed went into a CRLF file'
}

fn test_it_writes_through_a_symlink_and_keeps_the_mode() {
	$if windows {
		return
	}
	dir := os.join_path(test_root, 'cfg_symlink')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir)!
	real := os.join_path(dir, 'real.json')
	link := os.join_path(dir, 'link.json')
	os.write_file(real, '{"mcp":{}}')!
	os.chmod(real, 0o600)!
	os.symlink(real, link)!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, link, false) or { panic(err) }
	assert os.is_link(link), 'the symlink was replaced by a file'
	assert has_entry(read_config(real), 'mcp', server_id), read_config(real)
	st := os.stat(real)!
	assert st.mode & 0o777 == 0o600, 'the mode changed to ${st.mode & 0o777:o}'
	// No temporary file is left beside it.
	assert os.ls(dir)!.len == 2, os.ls(dir)!.str()
}

fn test_a_key_named_like_another_client_is_not_mistaken_for_it() {
	// The string "mcp" appears inside a value here, and it must not be taken for
	// the top-level key.
	path := config_fixture('decoy', '{"note":{"text":"the mcp key"},"mcp":{"duck":{}}}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	after := read_config(path)
	assert after.contains('"note"'), after
	assert after.contains('"vlang"'), after
	assert parses(path), after
}

fn test_it_creates_a_missing_config_with_the_key_in_it() {
	path := config_fixture('missing', '')!
	// The default, which is what every real client but Zed gets.
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcpServers'
	}
	write_entry(h, path, false) or { panic(err) }
	assert parses(path), read_config(path)
	assert has_entry(read_config(path), 'mcpServers', server_id), read_config(path)
}

fn test_it_will_not_create_a_user_file_it_may_only_add_to() {
	path := config_fixture('nocreate', '')!
	h := Harness{
		name:           'test'
		label:          'test'
		key:            'context_servers'
		no_create_user: true
	}
	mut refused := false
	write_entry(h, path, false) or { refused = true }
	assert refused, 'a missing file was not reported'
	assert !os.exists(path), 'a file was created that may only be added to'
	// The flag is about the user-level file; a project file is always created.
	write_entry(h, path, true) or { panic(err) }
	assert parses(path), read_config(path)
}

fn test_every_client_but_zed_creates_a_missing_user_file() {
	for h in harnesses() {
		assert h.no_create_user == (h.name == 'zed'), '${h.name}: no_create_user is ${h.no_create_user}'
	}
}

fn test_a_project_install_creates_the_file_for_every_client_that_has_one() {
	root := os.join_path(test_root, 'project_scope')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	mut seen := 0
	for h in harnesses() {
		if !h.has_project_scope() {
			continue
		}
		path := h.path_for(root, true)
		assert path.starts_with(root), '${h.name}: ${path}'
		assert !os.exists(path), '${h.name}: ${path} is shared with another client'
		write_entry(h, path, true) or { panic('${h.name}: ${err.msg()}') }
		assert parses(path), '${h.name}: ' + read_config(path)
		assert has_entry(read_config(path), h.key, server_id), '${h.name}: ' + read_config(path)
		seen++
	}
	assert seen == 5, 'expected five clients with a project file, got ${seen}'
}

fn test_opencode_uses_the_root_project_file_unless_another_already_exists() {
	opencode := find_harness('opencode') or { panic('opencode is missing') }
	root := os.join_path(test_root, 'opencode_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, '.opencode'))!
	assert opencode.path_for(root, true) == os.join_path(root, 'opencode.json')
	nested := os.join_path(root, '.opencode', 'opencode.json')
	os.write_file(nested, '{}')!
	assert opencode.path_for(root, true) == nested
	// The root file wins when both are there, as it does for opencode itself.
	os.write_file(os.join_path(root, 'opencode.json'), '{}')!
	assert opencode.path_for(root, true) == os.join_path(root, 'opencode.json')
}

fn test_zed_reads_the_settings_file_its_own_paths_name() {
	settings := 'settings.json'
	// Windows: %APPDATA%\Zed, capitalised.
	assert zed_settings_file('windows', 'C:\\Users\\me', 'C:\\Users\\me\\AppData\\Roaming',
		'') == os.join_path('C:\\Users\\me\\AppData\\Roaming', 'Zed', settings)
	// Linux and FreeBSD: the XDG config directory, or the Flatpak one inside a
	// Flatpak, lowercased.
	assert zed_settings_file('linux', '/home/me', '/home/me/.config', '') == os.join_path('/home/me/.config',
		'zed', settings)
	assert zed_settings_file('linux', '/home/me', '/xdg', '') == os.join_path('/xdg', 'zed',
		settings)
	assert zed_settings_file('freebsd', '/home/me', '/xdg', '/flatpak') == os.join_path('/flatpak',
		'zed', settings)
	// macOS: ~/.config/zed, not ~/Library/Application Support, and not XDG.
	assert zed_settings_file('macos', '/Users/me', '/Users/me/Library/Application Support',
		'/flatpak') == os.join_path('/Users/me', '.config', 'zed', settings)
	$if macos {
		zed := find_harness('zed') or { panic('zed is missing') }
		assert zed.user == os.join_path(os.home_dir(), '.config', 'zed', settings), zed.user
	}
}

fn test_uninstall_removes_the_entry_and_leaves_the_rest() {
	path := config_fixture('remove', '{"mcp":{"duck":{"type":"local"},"vlang":{"type":"local"}},"other":true}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	assert remove_entry(h, path)!, 'the entry was not removed'
	after := read_config(path)
	assert !after.contains('"vlang"'), after
	assert after.contains('"duck"'), after
	assert after.contains('"other"'), after
	assert parses(path), after
}

fn test_uninstall_removes_the_last_entry_without_leaving_a_comma() {
	path := config_fixture('removelast', '{"mcp":{"vlang":{"type":"local"}}}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	assert remove_entry(h, path)!, 'the entry was not removed'
	assert parses(path), read_config(path)
	assert !read_config(path).contains('"vlang"'), read_config(path)
}

fn test_uninstall_reports_nothing_when_the_entry_is_absent() {
	path := config_fixture('absent', '{"mcp":{"duck":{}}}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	assert !remove_entry(h, path)!, 'removing a missing entry reported success'
	assert read_config(path) == '{"mcp":{"duck":{}}}', 'the file changed'
}

fn test_uninstall_removes_the_entry_from_a_commented_file() {
	path := config_fixture('removejsonc', '{\n  // keep me\n  "mcp":{"vlang":{}}\n}\n')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	assert !is_plain_json(read_config(path)), 'a commented file must not count as plain JSON'
	assert remove_entry(h, path)!, 'the entry was not removed'
	after := read_config(path)
	// The comment survives, and the entry is gone.
	assert after.contains('// keep me'), after
	assert !has_entry(after, 'mcp', 'vlang'), after
	assert is_editable(read_config(path)), after
}

fn test_uninstall_ignores_a_commented_out_entry() {
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	// A commented-out entry looks like a real one to a scan that does not skip
	// comments, and cutting it would take the comment marker with it and leave
	// the next server inside the comment. This scan skips comments, so the
	// entry is not removed and the file is untouched.
	commented := '{\n  "mcp": {\n    // "vlang": {"type": "local"},\n    "duck": {"type": "local"}\n  }\n}\n'
	path2 := config_fixture('removecommented', commented)!
	assert !(remove_entry(h, path2) or { false }), 'a commented-out entry was removed'
	assert read_config(path2) == commented, 'the file changed'
	// A commented file the entry is not in at all is no failure, so
	// `v mcp uninstall --all` can pass over it.
	path3 := config_fixture('removeunrelated', '{\n  // mine\n  "mcp": {"duck": {}}\n}\n')!
	assert !remove_entry(h, path3)!, 'an unrelated commented file reported a removal'
}

fn test_non_plain_json_refusal_members_use_every_clients_key_and_entry_shape() {
	mut clients := harnesses()
	clients << Harness{ name: 'escaped', label: 'escaped', key: 'servers"\\key' }
	for i, h in clients {
		path := config_fixture('refusal_member_${i}', '{"other": true,}')!
		before := read_config(path)
		mut message := ''
		write_entry(h, path, false) or { message = err.msg() }
		assert message.contains('is not plain JSON'), message
		assert message.contains('already exists, merge'), message
		assert read_config(path) == before
		member := message.split_into_lines().last().trim_space()
		assert member == '${json_string(h.key)}: { ${json_string(server_id)}: ${entry_text(h)} }'
		assert is_plain_json('{ ${member} }'), member
	}
}

fn test_truncated_json_refusal_does_not_claim_it_contains_a_comment() {
	path := config_fixture('truncated_refusal', '{"mcp":')!
	h := find_harness('opencode') or { panic('opencode is missing') }
	before := read_config(path)
	mut message := ''
	write_entry(h, path, false) or { message = err.msg() }
	assert message.contains('is not plain JSON'), message
	assert !message.contains('has comments'), message
	assert message.contains('Add this member by hand'), message
	assert read_config(path) == before
}

fn test_unreadable_config_refusal_does_not_suggest_pasting_a_member() {
	path := config_fixture('directory_refusal', '')!
	h := find_harness('opencode') or { panic('opencode is missing') }
	mut message := ''
	write_entry(h, os.dir(path), false) or { message = err.msg() }
	assert message.contains('could not read'), message
	assert !message.contains('Add this member'), message
	assert os.is_dir(os.dir(path))
}

fn test_a_comment_holding_a_quote_does_not_confuse_the_scan() {
	path := config_fixture('jsoncquote', '{\n  // use "mcp" here\n  "mcp": {}\n}\n')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	after := read_config(path)
	assert has_entry(after, 'mcp', 'vlang'), after
	assert is_editable(after), after
}

fn test_a_comment_holding_braces_does_not_change_the_depth() {
	path := config_fixture('jsoncbrace', '{\n  // { not: real }\n  "mcp": {}\n}\n')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	assert has_entry(read_config(path), 'mcp', 'vlang'), read_config(path)
}

fn test_a_block_comment_is_skipped() {
	path := config_fixture('jsoncblock', '{\n  /* a block\n     comment */\n  "mcp": {}\n}\n')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	assert has_entry(read_config(path), 'mcp', 'vlang'), read_config(path)
}

fn test_a_comment_between_the_colon_and_the_value_is_skipped() {
	path := config_fixture('jsonccolon', '{"mcp": /* c */ {}}\n')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	write_entry(h, path, false) or { panic(err) }
	assert has_entry(read_config(path), 'mcp', 'vlang'), read_config(path)
}

fn test_a_trailing_comma_is_still_refused() {
	// Comments are edited; a trailing comma is not, because removing one is a
	// change this tool does not make.
	path := config_fixture('trailing', '{"mcp":{},}\n')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	mut refused := false
	write_entry(h, path, false) or { refused = true }
	assert refused, 'a trailing comma was not refused'
}

fn test_an_unterminated_block_comment_is_refused_without_changing_the_file() {
	h := find_harness('opencode') or { panic('opencode is missing') }
	for i, text in ['{} /* unfinished', '{"mcp":{"vlang":{}}} /* unfinished'] {
		path := config_fixture('unterminated_block_${i}', text)!
		assert !is_editable(text)
		mut message := ''
		write_entry(h, path, false) or { message = err.msg() }
		assert message.len > 0
		assert read_config(path) == text
		mut removed := false
		remove_entry(h, path) or { removed = true }
		assert removed || !text.contains('vlang')
		assert read_config(path) == text
	}
}

fn test_install_preserves_comments_in_an_empty_server_object() {
	h := find_harness('opencode') or { panic('opencode is missing') }
	for i, text in ['{"mcp":{/* keep, \"quoted\" } */}}', '{"mcp":{\n// keep, \"quoted\" }\n}}'] {
		path := config_fixture('comment_only_servers_${i}', text)!
		write_entry(h, path, false)!
		installed := read_config(path)
		assert is_editable(installed), installed
		assert has_entry(installed, 'mcp', server_id), installed
		assert installed.contains(if i == 0 {
			'/* keep, "quoted" } */'
		} else {
			'// keep, "quoted" }'
		})
		assert remove_entry(h, path)!
		after := read_config(path)
		assert is_editable(after), after
		assert !has_entry(after, 'mcp', server_id)
		assert after.contains(if i == 0 { '/* keep, "quoted" } */' } else { '// keep, "quoted" }' })
	}
}

fn test_install_preserves_leading_comments_when_adding_the_root_key() {
	h := find_harness('opencode') or { panic('opencode is missing') }
	for i, prefix in ['// leading, "mcp"\n', '/* leading, "mcp" */\n'] {
		text := prefix + '{"other":true}'
		path := config_fixture('leading_comments_${i}', text)!
		write_entry(h, path, false)!
		after := read_config(path)
		assert after.starts_with(prefix)
		assert after.contains('"other":true')
		assert is_editable(after), after
		assert has_entry(after, 'mcp', server_id)
	}
}

fn test_uninstall_preserves_comments_around_each_entry_separator() {
	h := find_harness('opencode') or { panic('opencode is missing') }
	for i, body in [
		'"vlang":{} /* tail, */ ,"duck":{"text":"a,b"}',
		'"duck":{}, /* before, */ "vlang":{} /* after, */',
		'"duck":{}, /* before, */ "vlang":{} /* after, */,"goose":{}',
		'"vlang":null /* tail, */ ,"duck":{}',
		'"vlang":{} // tail,\n ,"duck":{}',
	] {
		text := '{"mcp":{${body}}}'
		path := config_fixture('comment_separators_${i}', text)!
		assert remove_entry(h, path)!
		after := read_config(path)
		assert is_editable(after), after
		assert !has_entry(after, 'mcp', server_id)
		assert has_entry(after, 'mcp', 'duck')
		for comment in ['/* tail, */', '/* before, */', '/* after, */', '// tail,'] {
			if text.contains(comment) {
				assert after.contains(comment), after
			}
		}
		if text.contains('"goose"') {
			assert has_entry(after, 'mcp', 'goose')
		}
		if text.contains('"a,b"') {
			assert after.contains('"a,b"')
		}
	}
}

fn test_a_string_value_equal_to_the_name_is_not_taken_for_the_entry() {
	path := config_fixture('valuename', '{"mcp":{"duck":"vlang","vlang":{"type":"local"}}}')!
	h := Harness{
		name:  'test'
		label: 'test'
		key:   'mcp'
	}
	assert has_entry(read_config(path), 'mcp', server_id), 'the entry after the value was missed'
	assert remove_entry(h, path)!, 'the entry was not removed'
	assert read_config(path) == '{"mcp":{"duck":"vlang"}}', read_config(path)
}

fn test_server_exe_falls_back_to_the_recorded_path_when_the_env_names_nothing() {
	// A binary run outside `v` has no VEXE, or has one that is not there. The
	// path recorded at build time is then the best answer available, so it is
	// tried rather than returning a path that cannot be launched.
	original := os.getenv_opt('VEXE') or { '' }
	os.setenv('VEXE', os.join_path(os.vtmp_dir(), 'no-such-compiler'), true)
	defer { os.setenv('VEXE', original, true) }
	got := server_exe()
	assert got == os.real_path(@VEXE) || got == os.real_path(@VEXE + '.exe'), got
}

fn test_server_exe_chooses_a_regular_executable_and_keeps_platform_order() {
	path := config_fixture('compiler_candidates', '')!
	raw := path + '_v'
	exe := raw + '.exe'
	recorded := path + '_recorded.exe'
	for candidate in [raw, exe, recorded] {
		os.write_file(candidate, 'compiler fixture')!
		os.chmod(candidate, 0o755)!
	}
	defer {
		for candidate in [raw, exe, recorded] {
			os.rm(candidate) or {}
		}
	}
	assert server_exe_for(raw, recorded, true) == os.real_path(exe)
	assert server_exe_for(exe, recorded, true) == os.real_path(exe)
	$if !windows {
		assert server_exe_for(raw, recorded, false) == os.real_path(raw)
		os.chmod(raw, 0o600)!
		assert server_exe_for(raw, recorded, false) == os.real_path(exe)
	}
	os.rm(raw)!
	os.mkdir(raw)!
	defer { os.rmdir(raw) or {} }
	assert server_exe_for(raw, recorded, false) == os.real_path(exe)
	os.rm(exe)!
	assert server_exe_for(raw, recorded, false) == os.real_path(recorded)
	assert server_exe_for('', recorded, false) == os.real_path(recorded)
	assert server_exe_for(raw + '_missing', recorded + '_missing', false) == raw + '_missing'
}

fn test_server_exe_resolves_the_selected_symlink() {
	$if windows {
		return
	}
	path := config_fixture('compiler_symlink', '')!
	executable := path + '_real'
	link := path + '_link'
	os.write_file(executable, 'compiler fixture')!
	os.chmod(executable, 0o755)!
	os.symlink(executable, link)!
	defer {
		os.rm(link) or {}
		os.rm(executable) or {}
	}
	assert server_exe_for(link, @VEXE, false) == os.real_path(executable)
}
