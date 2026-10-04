module main

import json2 as json
import os

// Insertion is where a new entry goes inside a config file, and what has to
// surround it there.
struct Insertion {
	// pos is the byte offset the entry is spliced in at.
	pos int
	// end is the offset the original text resumes at. It is past `pos` only when
	// the whitespace inside an empty object is replaced.
	end int
	// prefix is the entry's own indentation, copied from the entry already in
	// the object so a hand-arranged file stays hand-arranged.
	prefix string
	// suffix is the comma and line break that follow it.
	suffix string
	// inline is set when the entry shares the line of the brace that opens the
	// object, so anything it holds stays on that line too.
	inline bool
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
		eprintln('v mcp install: only the user-level file is handled for ${harness.label};')
		eprintln('  use without --project, or register it by hand.')
		exit(1)
	}
	path := harness.path_for(os.getwd(), project)
	if print_only {
		print_one(harness, path, project)
		return
	}
	write_entry(harness, path, project) or {
		eprintln('v mcp install: ${err.msg()}')
		eprintln('  Add this to the top-level ${json_string(harness.key)} object by hand:')
		println('  ${json_string(server_id)}: ${entry_text(harness)}')
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
	mut failed := 0
	for h in targets {
		if project && !h.has_project_scope() {
			continue
		}
		was_there := remove_entry(h, h.path_for(os.getwd(), project)) or {
			eprintln('v mcp uninstall: ${err.msg()}')
			failed++
			continue
		}
		if was_there {
			removed++
		}
	}
	if failed > 0 {
		exit(1)
	}
	if removed == 0 {
		println('v mcp uninstall: nothing to remove')
	}
}

const install_usage = 'Usage: v mcp install [client] [options]\n' +
	'\n' +
	'Options:\n' +
	'  --project   the project-level file rather than the user-level one\n' +
	'  --print     print the entry instead of writing it\n' +
	'  -h, --help  show this help and exit\n'

const uninstall_usage = 'Usage: v mcp uninstall [client] [options]\n' +
	'\n' +
	'Options:\n' +
	'  --project   the project-level file rather than the user-level one\n' +
	'  --all       remove the entry from every client that has it\n' +
	'  -h, --help  show this help and exit\n'

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

// write_entry puts the entry into the client's file. An error says why it was
// not written, and the file is then exactly as it was. An entry that is already
// there is not an error.
fn write_entry(h Harness, path string, project bool) ! {
	// The key goes through the JSON writer like any other, so it cannot end up
	// as a bare word in the file.
	entry := json_string(server_id) + ': ' + entry_text(h)
	// The key is quoted rather than pasted, so a client whose name needs it gets
	// it quoted. `entry` already carries the quoted server name.
	skeleton := '{\n  ' + json_string(h.key) + ': {\n    ' + entry + '\n  }\n}\n'

	if !os.exists(path) {
		if !project && h.no_create_user {
			return error('${path} does not exist, and ${h.label} keeps its servers in a file that is only added to, never created.')
		}
		os.mkdir_all(os.dir(path)) or {
			return error('could not create ${os.dir(path)}: ${err.msg()}')
		}
		write_atomically(path, skeleton) or {
			return error('could not write ${path}: ${err.msg()}')
		}
		println('${h.label}: created ${path}')
		return
	}

	text := os.read_file(path) or { return error('could not read ${path}: ${err.msg()}') }
	if text.trim_space() == '' {
		// An empty file holds nothing to keep, so it gets the same body as a
		// missing one.
		write_atomically(path, skeleton) or {
			return error('could not write ${path}: ${err.msg()}')
		}
		println('${h.label}: added ${server_id} to ${path}')
		return
	}
	// A file with comments or trailing commas is not something to rewrite: json2
	// cannot parse it back, and a round trip would drop what it cannot model.
	if !is_plain_json(text) {
		return error('${path} is not plain JSON (comments or trailing commas); not rewriting it.')
	}
	mut point := Insertion{}
	mut addition := entry
	if found := insertion_point(text, h.key) {
		if has_entry(text, h.key, server_id) {
			eprintln('v mcp install: ${server_id} is already in ${path}; leaving it alone.')
			return
		}
		point = found
	} else {
		if has_top_level_key(text, h.key) {
			return error('${path} has a top-level ${json_string(h.key)} that is not an object; not guessing.')
		}
		// No servers yet, which is how a client's own file starts out: the
		// object goes in as the first member of the root, laid out like the
		// member already there.
		point = root_insertion(text) or {
			return error('${path} does not hold a JSON object; not guessing.')
		}
		addition = json_string(h.key) + ': ' + wrap_entry(entry, point)
	}
	// The comma, the line break and the indentation all come from
	// `insertion_point`, which read them off the entries already in the object.
	mut piece := point.prefix + addition + point.suffix
	if text.contains('\r\n') {
		piece = piece.replace('\n', '\r\n')
	}
	write_atomically(path, text[..point.pos] + piece + text[point.end..]) or {
		return error('could not write ${path}: ${err.msg()}')
	}
	println('${h.label}: added ${server_id} to ${path}')
}

// wrap_entry is the object a new top-level key holds, with the entry one step
// deeper than the key itself.
fn wrap_entry(entry string, point Insertion) string {
	if point.inline {
		return '{' + entry + '}'
	}
	indent := point.prefix.all_after_last('\n')
	return '{\n' + indent + '  ' + entry + '\n' + indent + '}'
}

// has_top_level_key reports whether the root object has `key` at all, whatever
// its value.
fn has_top_level_key(text string, key string) bool {
	root := json.decode[map[string]json.Any](text) or { return false }
	return key in root
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
