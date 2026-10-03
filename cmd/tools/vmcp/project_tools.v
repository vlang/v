// Tools that describe the project: its manifest, its modules, its files.
module main

import os
import v.astjson
import v.skills

// spec_project_info declares `v_project_info`.
fn spec_project_info() ToolSpec {
	return read_only_spec('v_project_info',
		'Describe the V project this server is pointed at: the v.mod manifest, the
compiler in use, whether the tree is the V checkout itself, and which skills are
installed for it. Start here, before reading any file.',
		one_string('include', 'Comma separated extra sections to include. One of:
`installed_modules`, `skill_status`.'), tool_project_info)
}

// tool_project_info answers `v_project_info`.
fn tool_project_info(ws &Workspace, arguments string) string {
	extra := decode_args(arguments).text('include', '')
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('root')
	w.string(ws.root)
	w.key('project_root')
	w.string(ws.project_root)
	w.key('has_v_mod')
	w.boolean(ws.v_modified)
	w.key('v_mod_path')
	w.string(ws.relative(ws.v_mod_file))
	w.key('is_v_checkout')
	w.boolean(ws.is_v_checkout)
	w.key('read_only')
	w.boolean(ws.read_only)
	w.key_raw('v_mod', manifest_json(ws))
	w.key('compiler')
	w.string(ws.compiler)
	w.key_raw('compiler_version', compiler_version_json(ws))
	w.key_raw('v_version', v_version_json(ws))
	w.key('bundled_skills_dir')
	w.string(skills.bundled_root(ws.vroot))
	if extra.contains('installed_modules') {
		w.key_raw('installed_modules', installed_modules())
	}
	if extra.contains('skill_status') {
		w.key_raw('skills', skill_status(ws))
	}
	w.end_object()
	return w.str()
}

// manifest_json renders the project's `v.mod` fields.
fn manifest_json(ws &Workspace) string {
	if !ws.v_modified {
		return '{}'
	}
	return object(
		text_pair('name', ws.v_mod_name),
		text_pair('description', ws.v_mod_description),
		text_pair('version', ws.v_mod_version),
		text_pair('license', ws.v_mod_license),
		text_pair('repo_url', ws.v_mod_repo_url),
		raw_pair('dependencies', string_array(ws.dependencies)),
	)
}

// compiler_version_json renders what `v version` printed, or why it could not be
// read.
//
// The two cases are kept apart on purpose. An empty string here means "the
// compiler did not answer", which is a different fact from a version, and an
// agent that cannot tell them apart will report the second as the first.
fn compiler_version_json(ws &Workspace) string {
	return compiler_version_json_for(run_compiler(ws, ['version']))
}

// compiler_version_json_for renders one already finished run.
fn compiler_version_json_for(run CompilerRun) string {
	if !run.started() {
		return object(text_pair('error', run.launch_error))
	}
	lines := run.output.trim_space().split_into_lines()
	first := if lines.len > 0 { lines[0].trim_space() } else { '' }
	return object(text_pair('value', first), text_pair('full', first))
}

// compiler_version_value returns what `v version` printed, or an empty string when
// the compiler could not be started.
//
// `v_doctor` reports this next to fields it reads from its own process, so the
// failure to start is reported there as its own `error` key and this stays a
// plain version string.
fn compiler_version_value(ws &Workspace) string {
	return compiler_version_value_for(run_compiler(ws, ['version']))
}

// compiler_version_value_for reads the version out of an already finished run, so
// both the started and the failed case can be exercised without starting anything.
fn compiler_version_value_for(run CompilerRun) string {
	if !run.started() {
		return ''
	}
	lines := run.output.trim_space().split_into_lines()
	return if lines.len > 0 { lines[0].trim_space() } else { '' }
}

// v_version_json renders the bare version, the part after `V `.
fn v_version_json(ws &Workspace) string {
	return v_version_json_for(run_compiler(ws, ['version']))
}

// v_version_json_for renders one already finished run.
fn v_version_json_for(run CompilerRun) string {
	if !run.started() {
		return object(text_pair('error', run.launch_error))
	}
	lines := run.output.trim_space().split_into_lines()
	full := if lines.len > 0 { lines[0].trim_space() } else { '' }
	return object(text_pair('value', full.trim_left('V').trim_space()))
}

// installed_modules returns the modules installed for this user, which is where a
// `v.mod` dependency is resolved from.
fn installed_modules() string {
	mut names := []string{}
	dir := os.vmodules_dir()
	if os.is_dir(dir) {
		mut entries := os.ls(dir) or {
			return string_array([])
		}
		entries.sort()
		for entry in entries {
			if os.is_dir(os.join_path_single(dir, entry)) {
				names << entry
			}
		}
	}
	return string_array(names)
}

// spec_modules declares `v_modules`.
fn spec_modules() ToolSpec {
	return read_only_spec('v_modules',
		'List the modules this project can import: the declared `v.mod`
dependencies, the directories that hold local modules, and the modules already
installed for the current user.',
		one_string('filter', 'Optional substring; only modules whose name contains it
are returned.'), tool_modules)
}

// tool_modules answers `v_modules`.
fn tool_modules(ws &Workspace, arguments string) string {
	filter := decode_args(arguments).text('filter', '')
	mut w := astjson.Writer{}
	w.begin_object()
	w.key_raw('declared', filtered(ws.dependencies, filter))
	w.key('declared_path')
	w.string(ws.relative(ws.v_mod_file))
	w.key_raw('installed', filtered(installed_module_names(), filter))
	w.key('installed_path')
	w.string(os.vmodules_dir())
	w.key_raw('local_module_dirs', string_array(local_module_dirs(ws)))
	w.end_object()
	return w.str()
}

// installed_module_names returns the module names installed for this user.
fn installed_module_names() []string {
	mut names := []string{}
	dir := os.vmodules_dir()
	if !os.is_dir(dir) {
		return names
	}
	entries := os.ls(dir) or {
		return names
	}
	for entry in entries {
		if os.is_dir(os.join_path_single(dir, entry)) {
			names << entry
		}
	}
	names.sort()
	return names
}

// local_module_dirs returns the subdirectories of the project root that look like
// local modules, which is where a sibling module is found without installing it.
fn local_module_dirs(ws &Workspace) []string {
	mut dirs := []string{}
	entries := os.ls(ws.project_root) or {
		return dirs
	}
	for entry in entries {
		full := os.join_path_single(ws.project_root, entry)
		if !os.is_dir(full) {
			continue
		}
		// A directory is a module when it carries its own `v.mod`, or when it
		// holds `.v` files directly, which is how a plain folder module is written.
		if os.is_file(os.join_path_single(full, 'v.mod')) || os.is_file(os.join_path_single(full,
			'${entry}.v'))
		{
			dirs << ws.relative(full)
		}
	}
	dirs.sort()
	return dirs
}

// filtered keeps the names containing `needle`, or all of them when it is empty.
fn filtered(names []string, needle string) string {
	if needle == '' {
		return string_array(names)
	}
	return string_array(names.filter(it.contains(needle)))
}

// spec_files declares `v_files`.
fn spec_files() ToolSpec {
	return read_only_spec('v_files',
		'List the V source files of the project, with their size and line count.
Use it to find a file before reading or editing it, or to see the shape of a
directory.',
		input_schema([], {
			'path':          SchemaProperty{
				kind:        'string'
				description: 'Directory to list, relative to the\nproject root. Defaults to the whole project.'
			}
			'include_tests': SchemaProperty{
				kind:        'boolean'
				description: 'Include `_test.v` files.\nDefaults to true.'
			}
			'limit':         SchemaProperty{
				kind:        'integer'
				description: 'Maximum number of files to return.\nDefaults to 500.'
			}
		}), tool_files)
}

// file_limit is how many files `v_files` returns before it truncates.
const file_limit = 500

// tool_files answers `v_files`.
fn tool_files(ws &Workspace, arguments string) string {
	args := decode_args(arguments)
	requested := args.text('path', '.')
	dir := ws.resolve_or_root(requested) or { return error_json(err.msg()) }
	if !os.is_dir(dir) {
		return error_json('`${requested}` is not a directory')
	}
	include_tests := args.boolean('include_tests', true)
	limit := args.int('limit', file_limit)
	mut out := []string{}
	mut truncated := 0
	for file in ws.v_files(dir) {
		is_test := file.ends_with('_test.v') || file.ends_with('_test.vsh')
		if is_test && !include_tests {
			continue
		}
		if out.len >= limit {
			truncated++
			continue
		}
		out << file_json(ws, file, is_test)
	}
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('directory')
	w.string(ws.relative(dir))
	w.key('returned')
	w.number(out.len)
	w.key('truncated')
	w.number(truncated)
	w.key('files')
	w.begin_array()
	for entry in out {
		w.array_raw(entry)
	}
	w.end_array()
	w.end_object()
	return w.str()
}

// file_json renders one project file.
fn file_json(ws &Workspace, file string, is_test bool) string {
	contents := os.read_file(file) or { return object(text_pair('path', ws.relative(file))) }
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('path')
	w.string(ws.relative(file))
	w.key('bytes')
	w.number(contents.len)
	w.key('lines')
	w.number(contents.split_into_lines().len)
	w.key('is_test')
	w.boolean(is_test)
	w.end_object()
	return w.str()
}

// error_json renders a tool failure the agent can read.
fn error_json(message string) string {
	return object(text_pair('error', message))
}

// resolve_or_root resolves `path` inside the workspace, falling back to the root
// when the path is empty.
pub fn (ws &Workspace) resolve_or_root(path string) !string {
	return ws.resolve(if path == '' { '.' } else { path })
}

// resolve_arg reads a required string argument and resolves it inside the
// workspace.
//
// A tool handler cannot both unwrap a required argument and handle the failure,
// so keeping the two steps in one place lets a caller report the message.
pub fn (ws &Workspace) resolve_arg(args &Args, key string) !string {
	name := args.required_str(key)!
	return ws.resolve(name)
}
