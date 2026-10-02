// Tools that describe the environment and the project beyond plain V code: what
// the compiler installation looks like, what a web app exposes, and which agent
// skills are installed.
module main

import os
import v.astjson
import v.skills

// spec_doctor declares `v_doctor`.
fn spec_doctor() ToolSpec {
	return read_only_spec('v_doctor',
		'Report the state of the V installation: version, C compiler, third-party
directories, module and cache locations. Run it before diagnosing a build
failure, so the answer names the actual cause instead of guessing.',
		no_args, tool_doctor)
}

// tool_doctor answers `v_doctor`.
fn tool_doctor(ws &Workspace, _ string) string {
	run := run_compiler(ws, ['doctor'])
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('command')
	w.string(run.command)
	if !run.started() {
		// `v doctor` could not run, so there is no report to relay. The fields
		// below are read from this process rather than from the compiler, so they
		// still describe the installation usefully.
		w.key('started')
		w.boolean(false)
		w.key('error')
		w.string('the compiler could not be started: ${run.launch_error}')
	} else {
		w.key('started')
		w.boolean(true)
		w.key('exit_code')
		w.number(run.exit_code)
		w.key('report')
		w.string(trim_output(run.output, 200))
	}
	w.key('vroot')
	w.string(ws.vroot)
	w.key('compiler')
	w.string(ws.compiler)
	w.key('compiler_version')
	w.string(compiler_version_value(ws))
	w.key('v_modules_dir')
	w.string(os.vmodules_dir())
	w.key('v_modules_dir_exists')
	w.boolean(os.is_dir(os.vmodules_dir()))
	w.end_object()
	return w.str()
}

// spec_veb_routes declares `v_veb_routes`.
fn spec_veb_routes() ToolSpec {
	return read_only_spec('v_veb_routes',
		'List the routes a veb web application registers: the HTTP methods, the
paths, and the handler each one maps to. Use it to understand a web app without
reading every handler.',
		input_schema([], {
			'path': SchemaProperty{
				kind:        'string'
				description: "The .v file declaring the veb routes.\nDefaults to the project's `main.v`."
			}
		}), tool_veb_routes)
}

// tool_veb_routes answers `v_veb_routes`.
fn tool_veb_routes(ws &Workspace, arguments string) string {
	path := ws.resolve_veb_target(decode_args(arguments).text('path', '')) or {
		return error_json(err.msg())
	}
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('path')
	w.string(ws.relative(path))
	w.key_raw('routes', routes_json(veb_routes(path)))
	w.end_object()
	return w.str()
}

// resolve_veb_target picks the file to look for routes in, defaulting to the
// `main.v` of the project root.
fn (ws &Workspace) resolve_veb_target(requested string) !string {
	if requested != '' {
		return ws.resolve(requested)
	}
	entry_point := os.join_path(ws.project_root, 'main.v')
	if os.is_file(entry_point) {
		return entry_point
	}
	return error('no `main.v` in `${ws.project_root}`; pass `path` to point at the file that registers the routes')
}

// spec_skills declares `v_skills`.
fn spec_skills() ToolSpec {
	return read_only_spec('v_skills',
		'List the agent skills bundled with this compiler and which of them are
installed for this project or for the current user, flagging any whose installed
copy has fallen behind. Add one with `v skills add <name>`.',
		no_args, tool_skills)
}

// tool_skills answers `v_skills`.
fn tool_skills(ws &Workspace, _ string) string {
	return skill_status(ws)
}

// skill_status renders the bundled catalog next to what is installed where.
fn skill_status(ws &Workspace) string {
	project_dir := skills.target_dir(.project_root, ws.project_root)
	global_dir := skills.target_dir(.home_dir, '')
	stale := skills.out_of_date(ws.vroot, project_dir)
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('bundled_dir')
	w.string(skills.bundled_root(ws.vroot))
	w.key('project_dir')
	w.string(project_dir)
	w.key('project_dir_exists')
	w.boolean(os.is_dir(project_dir))
	w.key('global_dir')
	w.string(global_dir)
	w.key('global_dir_exists')
	w.boolean(os.is_dir(global_dir))
	w.key_raw('out_of_date', string_array(stale))
	w.key('skills')
	w.begin_array()
	for skill in skills.catalog(ws.vroot) {
		w.array_raw(skill_summary_json(skill, project_dir, global_dir, stale))
	}
	w.end_array()
	w.end_object()
	return w.str()
}

// skill_summary_json renders one bundled skill and where it is installed.
fn skill_summary_json(skill skills.Skill, project_dir string, global_dir string,
	stale []string) string {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('name')
	w.string(skill.name)
	w.key('description')
	w.string(skill.description)
	w.key_raw('files', string_array(skill.files))
	w.key('in_project')
	w.boolean(os.is_dir(os.join_path_single(project_dir, skill.name)))
	w.key('in_global')
	w.boolean(os.is_dir(os.join_path_single(global_dir, skill.name)))
	w.key('out_of_date')
	w.boolean(skill.name in stale)
	w.end_object()
	return w.str()
}
