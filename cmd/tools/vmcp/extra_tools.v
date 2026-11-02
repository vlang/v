// Extra read-only tools ported from gil-silva/v-mcp-server.
//
// `v_build` does a full codegen build (unlike `v_check` which is `-check`
// only), `v_fmt_check`/`v_vet` answer directory-level hygiene in one call,
// `v_repo_brief` orients in one call instead of N file reads, and `v_affected`
// narrows `v test` to the test files a change can reach.
module main

import os
import v.astjson

// brief_char_limit caps `v_repo_brief` output, same token-economy idea as the
// 100 kB cap in gil-silva's server.
const brief_char_limit = 40000

fn spec_build() ToolSpec {
	return read_only_spec('v_build',
		'Full-codegen build of a file or directory. Catches backend and linker failures that v_check misses. The binary goes to a temp dir, never the project.',
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
	suffix := $if windows { '.exe' } $else { '' }
	tmp := os.join_path_single(os.vtmp_dir(), 'v_mcp_build_${os.getpid()}${suffix}')
	mut compiler_args := args.list('flags')
	compiler_args << ['-o', tmp, path]
	run := run_compiler(ws, compiler_args)
	os.rm(tmp) or {}
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
		'Check formatting (v fmt -c) for a file or directory. Never writes anything.',
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
	return read_only_spec('v_vet', 'Run v vet on a file or directory. Read-only.',
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
		'One-call repo map: manifest, dependencies, file list with sizes. Output is capped.',
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
	mut entries_len := 0
	mut total_bytes := 0
	mut total_lines := 0
	mut truncated := false
	for file in ws.v_files(dir) {
		contents := os.read_file(file) or { continue }
		lines := contents.split_into_lines().len
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
		entry := ew.str()
		// Cap the files array on its own; the header above is small next
		// to the limit. Note: never call w.str() before the response is
		// complete, it drains what was written so far.
		if entries_len + entry.len + 1 > brief_char_limit {
			truncated = true
			break
		}
		entries << entry
		entries_len += entry.len + 1
		total_bytes += contents.len
		total_lines += lines
	}
	w.key('file_count')
	w.number(entries.len)
	w.key('total_bytes')
	w.number(total_bytes)
	w.key('total_lines')
	w.number(total_lines)
	// `totals_partial` tells the reader whether the totals above cover the
	// whole directory or only the files listed below.
	w.key('totals_partial')
	w.boolean(truncated)
	w.key_raw('files', '[' + entries.join(',') + ']')
	w.key('truncated')
	w.boolean(truncated)
	w.end_object()
	return w.str()
}

fn spec_affected() ToolSpec {
	return read_only_spec('v_affected',
		'List the test files a change can reach through the local import graph. Pass a file or directory, or omit it to use the git worktree (staged, unstaged and untracked files). Same-module tests count via their directory. Only modules under the project root are followed.',
		input_schema([], {
			'path': SchemaProperty{
				kind:        'string'
				description: 'File or directory that changed. Defaults to the git worktree.'
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
	mut seed := map[string]bool{}
	mut changed := []string{}
	if args.has('path') {
		p := ws.resolve(args.text('path', '')) or { return error_json(err.msg()) }
		if os.is_dir(p) {
			for f in ws.v_files(p) {
				seed[os.real_path(f)] = true
			}
		} else if os.is_file(p) {
			seed[os.real_path(p)] = true
		} else {
			return error_json('`${args.text('path', '')}` is not a file or directory')
		}
	} else {
		if !is_git_repo(ws) {
			return error_json('`${ws.relative(ws.project_root)}` is not a git checkout: pass `path` explicitly')
		}
		changed = affected_changed_files(ws, base)
		for f in changed {
			seed[f] = true
		}
	}
	graph := import_graph(ws)
	reached := propagate(graph, seed)
	mut seed_dirs := map[string]bool{}
	for s in seed.keys() {
		seed_dirs[os.dir(s)] = true
	}
	mut tests := []string{}
	for file in ws.v_files(ws.project_root) {
		if !(file.ends_with('_test.v') || file.ends_with('_test.vsh')) {
			continue
		}
		real := os.real_path(file)
		// Same-module tests share a directory with the changed file, so they
		// count even though they import nothing.
		if real in reached || os.dir(real) in seed_dirs
			|| file_imported_by_changed(real, reached, graph) {
			tests << ws.relative(file)
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

// is_git_repo reports whether the project root is inside a git worktree.
fn is_git_repo(ws &Workspace) bool {
	res := os.exec(['git', '-C', ws.project_root, 'rev-parse', '--is-inside-work-tree'])
	return res.exit_code == 0
}

fn affected_changed_files(ws &Workspace, base string) []string {
	// `status --porcelain` covers staged, unstaged and untracked files at once,
	// which is what `diff --name-only` misses. A non-default base still goes
	// through `diff`, which only sees tracked files.
	res := if base == 'HEAD' {
		os.exec(['git', '-C', ws.project_root, 'status', '--porcelain'])
	} else {
		os.exec(['git', '-C', ws.project_root, 'diff', '--name-only', base])
	}
	if res.exit_code != 0 {
		return []string{}
	}
	mut out := []string{}
	for line in res.output.split_into_lines() {
		p := if base == 'HEAD' { changed_path_from_status(line) } else { line.trim_space() }
		if p == '' {
			continue
		}
		full := os.join_path_single(ws.project_root, p)
		if os.is_file(full) && (full.ends_with('.v') || full.ends_with('.vsh')) {
			out << os.real_path(full)
		}
	}
	return out
}

// changed_path_from_status reads the path out of one `git status --porcelain`
// line, following renames to the new name and unquoting quoted paths.
fn changed_path_from_status(line string) string {
	if line.len < 4 {
		return ''
	}
	mut p := line[3..].trim_space()
	if p.contains(' -> ') {
		p = p.all_after(' -> ').trim_space()
	}
	return p.trim('"')
}

// import_graph maps a file to the project files it imports (by module dir match).
fn import_graph(ws &Workspace) map[string][]string {
	files := ws.v_files(ws.project_root)
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
	mut in_block := false
	for line in src.split_into_lines() {
		t := line.trim_space()
		if in_block {
			mut body := t
			if body.contains(')') {
				body = body.all_before(')').trim_space()
				in_block = false
			}
			if body != '' {
				mods << import_names(body)
			}
			continue
		}
		if t.starts_with('import')
			&& (t.len == 6 || t[6] == ` ` || t[6] == `\t` || t[6] == `(`) {
			mut rest := t[6..].trim_space()
			if rest.starts_with('(') {
				rest = rest[1..].trim_space()
				if rest.contains(')') {
					rest = rest.all_before(')').trim_space()
				} else {
					in_block = true
				}
			} else {
				rest = rest.all_before('//')
			}
			if rest != '' {
				mods << import_names(rest)
			}
			continue
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

// import_names splits one import list (`os`, `a, b`, `mymod as m`) into names.
fn import_names(text string) []string {
	mut out := []string{}
	for part in text.split(',') {
		mut name := part.trim_space().split(' ')[0].trim_space()
		name = name.all_before(' as ').trim_space()
		if name != '' && name != ')' && !name.starts_with('//') {
			out << name
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

// file_imported_by_changed reports whether the file at `real` imports anything
// the change reached, which makes a test in another directory affected.
fn file_imported_by_changed(real string, reached map[string]bool, graph map[string][]string) bool {
	for dep in graph[real] {
		if dep in reached {
			return true
		}
	}
	return false
}
