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
		println(print_one(harness, path, project))
		return
	}
	write_entry(harness, path, project) or {
		// The message already says what to do, including the member to paste when
		// the file was refused rather than written.
		eprintln('v mcp install: ${err.msg()}')
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
		println(print_one(h, path, project))
		any = true
	}
	if !any {
		println('No MCP configuration file was found for a known client.')
		println('Name one to have it written: v mcp install ${known_names()}')
	}
}

// print_one is what `v mcp install <client> --print` reports for one client. It
// returns the text rather than printing it, so the wording can be asserted.
fn print_one(h Harness, path string, project bool) string {
	mut out := '${h.label}  (${path})\n'
	if project && !h.has_project_scope() {
		return out + '  (no project-level file)\n'
	}
	if !os.exists(path) {
		if h.no_create_user && !project {
			out += '  no config file yet; the installer will not create this file.\n'
			out += '  If preparing the configuration by hand, use this JSON:\n'
		} else {
			out += '  no config file yet; this is what would be created:\n'
		}
		out += '  { ${json_string(h.key)}: { ${json_string(server_id)}: ${entry_text(h)} } }\n'
		return out
	}
	if !os.is_file(path) {
		return out + '  could not read ${path}: not a regular file\n'
	}
	text := os.read_file(path) or { return out + '  could not read ${path}: ${err.msg()}\n' }
	if has_top_level_key(text, h.key) {
		out += '  top-level key: ${h.key}\n'
		out += '  ${json_string(server_id)}: ${entry_text(h)}\n'
	} else {
		// The entry alone is not something a client can read: it has to sit
		// inside the client's own key. Print the member, which is what a reader
		// pastes, rather than the entry, which is not.
		out += '  no top-level ${json_string(h.key)} yet; add this member:\n'
		out += '  ${json_string(h.key)}: { ${json_string(server_id)}: ${entry_text(h)} }\n'
	}
	return out
}

// server_exe selects a launchable compiler path, trying the build-time path when
// VEXE is stale. A resolved path keeps the registration independent of PATH.
fn server_exe() string {
	return server_exe_for(os.getenv_opt('VEXE') or { @VEXE }, @VEXE, os.user_os() == 'windows')
}

fn server_exe_for(raw string, recorded string, windows bool) string {
	mut candidates := []string{}
	for path in [raw, recorded] {
		if windows && path.to_lower().all_after_last('.') !in ['exe', 'com', 'bat', 'cmd'] {
			candidates << path + '.exe'
			candidates << path
		} else {
			candidates << path
			candidates << path + '.exe'
		}
	}
	for candidate in candidates {
		if os.is_file(candidate) && os.is_executable(candidate) {
			return os.real_path(candidate)
		}
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
	// A file this tool cannot parse is left alone rather than guessed at. A
	// commented file is different: the comments are the reader's, and the edit is
	// textual, so they survive. What decides it is whether the file parses once
	// the comments are gone.
	if !is_editable(text) {
		// Parsing as some other JSON value means the file is fine and its shape is
		// wrong, which is a different problem. Saying "comments or trailing commas"
		// about it sends the reader hunting for a comment the file does not have.
		if is_json_value(strip_comments(text)) {
			return error('${path} is valid JSON, but its top level is not an object; not guessing where the servers belong.')
		}
		// The file is left exactly as it was. An entry on its own is not something
		// a client can read, so the whole member is what a reader pastes, and
		// printing it here saves them assembling that by hand.
		return error('${path} is not plain JSON (for example, comments or trailing commas); it was not rewritten.\n' +
			'  Add this member by hand. If ${json_string(h.key)} already exists, merge ${json_string(server_id)} into it instead of adding a second key:\n' +
			'  ${json_string(h.key)}: { ${json_string(server_id)}: ${entry_text(h)} }')
	}
	mut point := Insertion{}
	mut addition := entry
	if found := insertion_point(text, h.key) {
		if has_entry(text, h.key, server_id) {
			eprintln(existing_entry_report(text, h, path, project))
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

// is_json_value reports whether the text parses as JSON at all, whatever its
// top level is. `is_plain_json` cannot answer that, because it decodes into a
// map and so rejects an array, a string and a number for the same reason it
// rejects a comment.
fn is_json_value(text string) bool {
	json.decode[json.Any](text) or { return false }
	return true
}

// is_editable reports whether the file can be edited without losing something.
// A commented file qualifies, because the edit is textual and the comments are
// not touched; what is checked is that the file parses once they are gone. A
// trailing comma still does not qualify, since removing one is a change this
// tool does not make.
fn is_editable(text string) bool {
	if is_plain_json(text) {
		return true
	}
	return is_plain_json(strip_comments(text))
}

// existing_entry_report says which command the entry that is already there runs.
//
// Without it, the only way to learn that the registered compiler is not the one
// you just invoked is to read the file, and the only way to move the entry is to
// discover that `install` will not do it.
fn existing_entry_report(text string, h Harness, path string, project bool) string {
	return existing_entry_report_for_compiler(text, h, path, project, server_exe(), os.user_os() == 'windows')
}

fn existing_entry_report_for_compiler(text string, h Harness, path string, project bool, wanted string, windows bool) string {
	mut lines := ['v mcp install: ${server_id} is already in ${path}; leaving it alone.']
	exe, command := recorded_entry(text, h.key) or { return lines.join('\n') }
	lines << '  it runs ${command}'
	if exe != wanted {
		lines << '  this compiler is ${wanted}'
		scope := if project { ' --project' } else { '' }
		// PowerShell needs its call operator for a quoted executable. POSIX shells
		// accept the quoted path directly. Both commands name this compiler.
		compiler := if windows {
			"& '" + wanted.replace("'", "''") + "'"
		} else {
			"'" + wanted.replace("'", "'\\''") + "'"
		}
		lines << if windows { '  to move it (PowerShell):' } else { '  to move it:' }
		lines << '  ${compiler} mcp uninstall ${h.name}${scope}'
		lines << '  ${compiler} mcp install ${h.name}${scope}'
	}
	return lines.join('\n')
}

// recorded_entry returns the executable the entry for `server_id` runs, and the
// command and arguments rendered for reading, or none when the entry holds no
// readable command. The two are returned apart because only the executable can
// be compared with the compiler running this tool.
fn recorded_entry(text string, key string) ?(string, string) {
	root := json.decode[map[string]json.Any](strip_comments(text)) or { return none }
	group := root[key] or { return none }
	servers := group.as_map()
	entry := servers[server_id] or { return none }
	fields := entry.as_map()
	command := fields['command'] or { return none }
	mut parts := []string{}
	if command is []json.Any {
		parts = command_strings(command) or { return none }
	} else if command is string {
		parts << command
		if args := fields['args'] {
			argument_parts := command_strings(args) or { return none }
			parts << argument_parts
		}
	} else {
		return none
	}
	if parts.len == 0 || parts[0] == '' {
		return none
	}
	mut displayed := []string{cap: parts.len}
	for part in parts {
		displayed << display_argument(part)
	}
	return parts[0], displayed.join(' ')
}

// command_strings reads an argument array without treating non-strings as commands.
fn command_strings(value json.Any) ?[]string {
	if value !is []json.Any {
		return none
	}
	mut parts := []string{}
	for part in value as []json.Any {
		if part is string {
			parts << part
		} else {
			return none
		}
	}
	return parts
}

// display_argument quotes whitespace and control bytes to preserve argument boundaries.
fn display_argument(value string) string {
	if value == '' {
		return json.encode(value)
	}
	for byte in value {
		if byte <= ` ` || byte == `"` {
			return json.encode(value)
		}
	}
	return value
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
// its value. The comments are stripped first, because a commented file is one
// this tool edits and the parser cannot read it as it stands.
fn has_top_level_key(text string, key string) bool {
	root := json.decode[map[string]json.Any](strip_comments(text)) or { return false }
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
