// Extra read-only tools ported from gil-silva/v-mcp-server.
//
// `v_build` does a full codegen build (unlike `v_check` which is `-check`
// only), `v_fmt_check`/`v_vet` answer directory-level hygiene in one call,
// `v_repo_brief` orients in one call instead of N file reads, and `v_affected`
// narrows `v test` to the `*_test.v` files a change can reach.
module main

import os
import v.astjson

// brief_char_limit caps `v_repo_brief` output, same token-economy idea as the
// 100 kB cap in gil-silva's server.
const brief_char_limit = 40000

fn spec_build() ToolSpec {
	return read_only_spec('v_build',
		'Compile a file or directory with full codegen and return compiler diagnostics. Unlike v_check (-check only), this also catches backend/linker failures. Writes the binary to a temp dir, never into the project.',
		input_schema(['path'], {
			'path':            SchemaProperty{
				kind:        'string'
				description: 'The .v file or directory to build.'
			}
			'flags':           SchemaProperty{
				kind:        'array'
				description: 'Extra compiler flags, for example `["-g"]`.'
			}
			'max_diagnostics': SchemaProperty{
				kind:        'integer'
				description: 'Limit (100); counts stay totals.'
			}
		}), tool_build)
}

fn tool_build(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	if !os.exists(path) {
		return error_json('`${path}` does not exist')
	}
	tmp := os.join_path_single(os.vtmp_dir(), 'v_mcp_build_${os.getpid()}.exe')
	mut compiler_args := args.list('flags')
	compiler_args << ['-o', tmp, path]
	run := run_compiler(ws, compiler_args)
	os.rm(tmp) or {}
	os.rm(tmp.trim_string_right('.exe')) or {}
	items := parse_diagnostics(run.output)
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('path')
	w.string(ws.relative(path))
	w.key('command')
	w.string(run.command)
	if !run.started() {
		w.key('started')
		w.boolean(false)
		w.key('error')
		w.string('the compiler could not be started: ${run.launch_error}')
		w.end_object()
		return w.str()
	}
	w.key('started')
	w.boolean(true)
	w.key('exit_code')
	w.number(run.exit_code)
	w.key('ok')
	w.boolean(run.exit_code == 0)
	w.key('error_count')
	w.number(count_errors(items))
	w.key('warning_count')
	w.number(count_by_kind(items, 'warning'))
	maxd := args.int('max_diagnostics', max_diagnostics_default)
	kept, omitted := page_diagnostics(items, maxd)
	w.key_raw('diagnostics', diagnostics_json(kept))
	w.key('diagnostics_omitted')
	w.number(omitted)
	if items.len == 0 && run.exit_code != 0 {
		w.key('output')
		w.string(trim_output(run.output, 60))
	}
	w.end_object()
	return w.str()
}

fn spec_fmt_check() ToolSpec {
	return read_only_spec('v_fmt_check',
		'Check whether files are formatted (`v fmt -c`). Check-only, never touches files. Point at a file or a directory.',
		input_schema(['path'], {
			'path': SchemaProperty{
				kind:        'string'
				description: 'The .v file or directory to check.'
			}
		}), tool_fmt_check)
}

fn tool_fmt_check(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	if !os.exists(path) {
		return error_json('`${path}` does not exist')
	}
	run := run_compiler(ws, ['fmt', '-c', path])
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('path')
	w.string(ws.relative(path))
	w.key('command')
	w.string(run.command)
	if !run.started() {
		w.key('started')
		w.boolean(false)
		w.key('error')
		w.string('the compiler could not be started: ${run.launch_error}')
		w.end_object()
		return w.str()
	}
	w.key('started')
	w.boolean(true)
	w.key('exit_code')
	w.number(run.exit_code)
	w.key('formatted')
	w.boolean(run.exit_code == 0)
	w.key('output')
	w.string(trim_output(run.output, 60))
	w.end_object()
	return w.str()
}

fn spec_vet() ToolSpec {
	return read_only_spec('v_vet',
		'Run `v vet` and return its warnings. Read-only, changes nothing.',
		input_schema(['path'], {
			'path':            SchemaProperty{
				kind:        'string'
				description: 'The .v file or directory to vet.'
			}
			'max_diagnostics': SchemaProperty{
				kind:        'integer'
				description: 'Limit (100); counts stay totals.'
			}
		}), tool_vet)
}

fn tool_vet(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	path := ws.resolve_arg(args, 'path') or { return error_json(err.msg()) }
	if !os.exists(path) {
		return error_json('`${path}` does not exist')
	}
	run := run_compiler(ws, ['vet', path])
	items := parse_diagnostics(run.output)
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('path')
	w.string(ws.relative(path))
	w.key('command')
	w.string(run.command)
	if !run.started() {
		w.key('started')
		w.boolean(false)
		w.key('error')
		w.string('the compiler could not be started: ${run.launch_error}')
		w.end_object()
		return w.str()
	}
	w.key('started')
	w.boolean(true)
	w.key('exit_code')
	w.number(run.exit_code)
	w.key('warning_count')
	w.number(items.len)
	maxd := args.int('max_diagnostics', max_diagnostics_default)
	kept, omitted := page_diagnostics(items, maxd)
	w.key_raw('diagnostics', diagnostics_json(kept))
	w.key('diagnostics_omitted')
	w.number(omitted)
	if items.len == 0 && run.output.trim_space() != '' {
		w.key('output')
		w.string(trim_output(run.output, 60))
	}
	w.end_object()
	return w.str()
}

fn spec_repo_brief() ToolSpec {
	return read_only_spec('v_repo_brief',
		'Compact repo map in one call: module identity, entry points, tree with LOC, per-file byte/line counts. Capped so one call orients instead of N file reads.',
		input_schema([], {
			'path': SchemaProperty{
				kind:        'string'
				description: 'Directory to map, relative to the project root. Defaults to the whole project.'
			}
		}), tool_repo_brief)
}

fn tool_repo_brief(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	requested := args.text('path', '.')
	dir := ws.resolve_or_root(requested) or { return error_json(err.msg()) }
	if !os.is_dir(dir) {
		return error_json('`${requested}` is not a directory')
	}
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('root')
	w.string(ws.relative(ws.project_root))
	w.key('directory')
	w.string(ws.relative(dir))
	w.key_raw('v_mod', manifest_json(ws))
	w.key_raw('dependencies', string_array(ws.dependencies))
	mut entries := []string{}
	mut total_bytes := 0
	mut total_lines := 0
	for file in ws.v_files(dir) {
		contents := os.read_file(file) or { continue }
		lines := contents.split_into_lines().len
		total_bytes += contents.len
		total_lines += lines
		is_test := file.ends_with('_test.v') || file.ends_with('_test.vsh')
		mut ew := astjson.Writer{}
		ew.begin_object()
		ew.key('path')
		ew.string(ws.relative(file))
		ew.key('bytes')
		ew.number(contents.len)
		ew.key('lines')
		ew.number(lines)
		ew.key('is_test')
		ew.boolean(is_test)
		ew.end_object()
		entries << ew.str()
		if entries.join(',').len + w.str().len > brief_char_limit {
			break
		}
	}
	w.key('file_count')
	w.number(entries.len)
	w.key('total_bytes')
	w.number(total_bytes)
	w.key('total_lines')
	w.number(total_lines)
	mut joined := entries.join(',')
	if joined.len > brief_char_limit {
		joined = joined[..brief_char_limit]
	}
	w.key_raw('files', '[' + joined + ']')
	w.key('truncated')
	w.boolean(joined.len >= brief_char_limit)
	w.end_object()
	return w.str()
}

fn spec_affected() ToolSpec {
	return read_only_spec('v_affected',
		'List the *_test.v files a change can affect: git diff vs base propagated through the static import graph. Run this before v_test_run to avoid the full suite.',
		input_schema([], {
			'path': SchemaProperty{
				kind:        'string'
				description: 'File or directory that changed. Defaults to the git diff vs base.'
			}
			'base': SchemaProperty{
				kind:        'string'
				description: 'Git base to diff against. Defaults to HEAD.'
			}
		}), tool_affected)
}

fn tool_affected(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	base := args.text('base', 'HEAD')
	changed := affected_changed_files(ws, base)
	mut seed := map[string]bool{}
	if args.has('path') {
		p := ws.resolve(args.text('path', '')) or { return error_json(err.msg()) }
		seed[os.real_path(p)] = true
	} else {
		for f in changed {
			seed[f] = true
		}
	}
	graph := import_graph(ws)
	reached := propagate(graph, seed)
	mut tests := []string{}
	for file in ws.v_files(ws.project_root) {
		if !(file.ends_with('_test.v') || file.ends_with('_test.vsh')) {
			continue
		}
		if os.real_path(file) in reached || file_is_imported_by_changed(file, reached, graph) {
			tests << ws.relative(file)
		}
	}
	// A changed test file itself is always affected.
	for s in seed.keys() {
		if s.ends_with('_test.v') || s.ends_with('_test.vsh') {
			rel := ws.relative(s)
			if rel !in tests {
				tests << rel
			}
		}
	}
	tests.sort()
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('base')
	w.string(base)
	w.key_raw('changed', string_array(changed.map(ws.relative(it))))
	w.key_raw('affected_tests', string_array(tests))
	w.key('count')
	w.number(tests.len)
	w.end_object()
	return w.str()
}

fn affected_changed_files(ws &Workspace, base string) []string {
	res := os.exec(['git', '-C', ws.project_root, 'diff', '--name-only', base])
	if res.exit_code != 0 {
		return []string{}
	}
	mut out := []string{}
	for line in res.output.split_into_lines() {
		t := line.trim_space()
		if t == '' {
			continue
		}
		full := os.join_path_single(ws.project_root, t)
		if os.is_file(full) && (full.ends_with('.v') || full.ends_with('.vsh')) {
			out << os.real_path(full)
		}
	}
	return out
}

// import_graph maps a file to the project files it imports (by module dir match).
fn import_graph(ws &Workspace) map[string][]string {
	mut files := ws.v_files(ws.project_root)
	mut by_dir := map[string]string{}
	for f in files {
		by_dir[os.dir(os.real_path(f))] = os.real_path(f)
	}
	mut graph := map[string][]string{}
	for f in files {
		real := os.real_path(f)
		graph[real] = imported_project_files(ws, f, files)
	}
	return graph
}

fn imported_project_files(ws &Workspace, file string, all []string) []string {
	src := os.read_file(file) or { return []string{} }
	mut mods := []string{}
	for line in src.split_into_lines() {
		t := line.trim_space()
		if t.starts_with('import ') {
			rest := t[7..].trim_space()
			for part in rest.split(',') {
				name := part.trim_space().split(' ')[0].trim_space()
				if name != '' {
					mods << name.all_before(' as ').trim_space()
				}
			}
		}
	}
	mut out := []string{}
	for m in mods {
		// Local module: a directory under project root with the module name.
		cand := os.real_path(os.join_path_single(ws.project_root, m.split('.').join('/')))
		if os.is_dir(cand) {
			for f in all {
				if os.real_path(f).starts_with(cand) {
					out << os.real_path(f)
				}
			}
		}
	}
	return out
}

fn propagate(graph map[string][]string, seed map[string]bool) map[string]bool {
	mut reached := map[string]bool{}
	for k in seed.keys() {
		reached[k] = true
	}
	// Reverse graph: dependency -> importers.
	mut rev := map[string][]string{}
	for importer, deps in graph {
		for d in deps {
			rev[d] << importer
		}
	}
	mut stack := seed.keys()
	for stack.len > 0 {
		cur := stack.pop()
		for importer in rev[cur] {
			if importer !in reached {
				reached[importer] = true
				stack << importer
			}
		}
	}
	return reached
}

fn file_is_imported_by_changed(file string, reached map[string]bool, graph map[string][]string) bool {
	real := os.real_path(file)
	if real in reached {
		return true
	}
	for dep in graph[real] {
		if dep in reached {
			return true
		}
	}
	return false
}
