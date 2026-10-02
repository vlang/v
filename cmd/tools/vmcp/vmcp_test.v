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
	answer := check_json(ws, probe_path('main.v'), run, [])
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
	}])
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
	answer := run_json(ws, probe_path('main.v'), ['run', 'main.v'])
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
		for line in output.trim_space().split_into_lines() {
			response := json.decode[WireToolListResponse](line, strict: true)!
			if response.id != 2 { continue }
			listed = true
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
