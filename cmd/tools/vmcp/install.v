module main

import json2 as json
import os

// Insertion is where a new entry goes inside a config file, and what has to
// surround it there.
struct Insertion {
	// pos is the byte offset the entry is spliced in at.
	pos int
	// prefix is the entry's own indentation, copied from the entry already in
	// the object so a hand-arranged file stays hand-arranged.
	prefix string
	// suffix is the comma and line break that follow it.
	suffix string
}

// install registers the V MCP server with a coding agent, or prints how to.
fn install(args []string) {
	mut name := ''
	mut project := false
	mut print_only := false
	for arg in args {
		match arg {
			'--project' {
				project = true
			}
			'--print' {
				print_only = true
			}
			'-h', '--help' {
				println(install_usage)
				return
			}
			else {
				if arg.starts_with('-') {
					eprintln('v mcp install: unknown option `${arg}`')
					exit(1)
				}
				if name != '' {
					eprintln('v mcp install: name one client at a time')
					exit(1)
				}
				name = arg
			}
		}
	}
	if name == '' {
		print_registrations(project)
		return
	}
	harness := find_harness(name) or {
		eprintln('v mcp install: no client called `${name}`; known: ${known_names()}')
		exit(1)
	}
	if project && !harness.has_project_scope() {
		eprintln('v mcp install: ${harness.label} has no documented project-level file;')
		eprintln('  use without --project, or register it by hand.')
		exit(1)
	}
	path := harness.path_for(os.getwd(), project)
	if print_only {
		print_one(harness, path, project)
		return
	}
	write_entry(harness, path) or {
		eprintln('v mcp install: could not write ${path}')
		exit(1)
	}
}

// uninstall removes what `v mcp install` wrote.
fn uninstall(args []string) {
	mut name := ''
	mut project := false
	mut all := false
	for arg in args {
		match arg {
			'--project' {
				project = true
			}
			'--all' {
				all = true
			}
			'-h', '--help' {
				println(uninstall_usage)
				return
			}
			else {
				if arg.starts_with('-') {
					eprintln('v mcp uninstall: unknown option `${arg}`')
					exit(1)
				}
				name = arg
			}
		}
	}
	if name == '' && !all {
		eprintln('v mcp uninstall: name a client, or pass --all')
		exit(1)
	}
	mut targets := []Harness{}
	if all {
		targets = harnesses()
	} else {
		targets = [find_harness(name) or {
			eprintln('v mcp uninstall: no client called `${name}`; known: ${known_names()}')
			exit(1)
		}]
	}
	mut removed := 0
	for h in targets {
		if project && !h.has_project_scope() {
			continue
		}
		if remove_entry(h, h.path_for(os.getwd(), project)) {
			removed++
		}
	}
	if removed == 0 {
		println('v mcp uninstall: nothing to remove')
	}
}

const install_usage = 'Usage: v mcp install [client] [options]\n' +
	'\n' +
	'Options:\n' +
	'  --project  the project-level file rather than the user-level one\n' +
	'  --print    print the entry instead of writing it\n' +
	'  -h, --help show this help and exit\n'

const uninstall_usage = 'Usage: v mcp uninstall [client] [options]\n' +
	'\n' +
	'Options:\n' +
	'  --project  the project-level file rather than the user-level one\n' +
	'  --all      remove the entry from every client that has it\n' +
	'  -h, --help show this help and exit\n'

fn known_names() string {
	return harnesses().map(it.name).join(', ')
}

// print_registrations prints the entry for every client whose config file
// already exists, which is how the user finds out which ones they have.
fn print_registrations(project bool) {
	mut any := false
	for h in harnesses() {
		path := h.path_for(os.getwd(), project)
		if path == '' || !os.exists(path) {
			continue
		}
		if any {
			println('')
		}
		print_one(h, path, project)
		any = true
	}
	if !any {
		println('No MCP configuration file was found for a known client.')
		println('Name one to have it written: v mcp install ${known_names()}')
	}
}

fn print_one(h Harness, path string, project bool) {
	println('${h.label}  (${path})')
	println('  top-level key: ${h.key}')
	println('  ${server_id}: ${h.entry(server_exe(), server_args())}')
	if project && !h.has_project_scope() {
		println('  (no project-level file)')
	}
}

// server_exe is the compiler this tool is running as, which is the one the
// registered command has to keep working after a PATH change.
fn server_exe() string {
	raw := os.getenv_opt('VEXE') or { @VEXE }
	// The compiler is routinely named without its extension, which nothing on
	// Windows can launch, so the executable form wins whenever it is there.
	// The real path is preferred so the entry does not carry forward slashes
	// that only work by accident.
	if os.exists(raw) {
		return os.real_path(raw)
	}
	if os.exists(raw + '.exe') {
		return os.real_path(raw + '.exe')
	}
	return raw
}

fn server_args() []string {
	return ['mcp', 'serve']
}

// write_entry puts the entry into the client's file, or explains why not.
fn write_entry(h Harness, path string) ! {
	// The key goes through the JSON writer like any other, so it cannot end up
	// as a bare word in the file.
	entry := json_string(server_id) + ': ' + h.entry(server_exe(), server_args())

	if !os.exists(path) {
		if !h.create_user {
			eprintln('v mcp install: ${path} does not exist, and this client\'s path is not confirmed')
			eprintln('  on this platform, so it was not created. Add this by hand:')
			println('  ${server_id}: ${entry_text(h)}')
			return
		}
		os.mkdir_all(os.dir(path))!
		// The key is quoted rather than pasted, so a client whose name needs it
		// gets it quoted. `entry` already carries the quoted server name.
		skeleton := '{\n  ' + json_string(h.key) + ': {\n    ' + entry + '\n  }\n}\n'
		os.write_file(path, skeleton)!
		println('${h.label}: created ${path}')
		return
	}

	text := os.read_file(path) or { panic(err) }
	// A file with comments or trailing commas is not something to rewrite: json2
	// cannot parse it back, and a round trip would drop what it cannot model.
	if !is_plain_json(text) {
		eprintln('v mcp install: ${path} is not plain JSON (comments or trailing commas).')
		eprintln('  Not rewriting it. Add this by hand:')
		println('  ${server_id}: ${entry_text(h)}')
		return
	}
	point := insertion_point(text, h.key) or {
		eprintln('v mcp install: ${path} has no top-level "${h.key}" object.')
		eprintln('  Not guessing where the servers belong. Add this by hand:')
		println('  ${server_id}: ${entry_text(h)}')
		return
	}
	if has_entry(text, h.key, server_id) {
		eprintln('v mcp install: ${server_id} is already in ${path}; leaving it alone.')
		return
	}
	// The comma, the line break and the indentation all come from
	// `insertion_point`, which read them off the entries already in the object.
	os.write_file(path, text[..point.pos] + point.prefix + entry + point.suffix +
		text[point.pos..])!
	println('${h.label}: added ${server_id} to ${path}')
}

fn entry_text(h Harness) string {
	return h.entry(server_exe(), server_args())
}

// is_plain_json reports whether the text is strict JSON, which is what decides
// whether the file can be edited without losing something.
fn is_plain_json(text string) bool {
	if text.trim_space() == '' {
		return true
	}
	json.decode[map[string]json.Any](text) or { return false }
	return true
}