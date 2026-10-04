module main

import os

// server_id is the name a harness shows for this server. It is the same across
// every client, so `v mcp uninstall` can find what `v mcp install` wrote no
// matter which client wrote it.
const server_id = 'vlang'

// Field is one fixed key/value pair in a rendered entry.
struct Field {
	key   string
	value string
}

// Harness describes one coding agent's MCP configuration: where the file lives,
// which top-level object holds the servers, and how that client spells a local
// server.
//
// Everything `install` and `uninstall` need is here, so a client V has never
// heard of is one more entry here rather than one more branch in the code.
struct Harness {
	// name is what the user types.
	name string
	// label is what the user reads.
	label string
	// user is the config file that applies to every project.
	user string
	// project is the config file inside a project, empty when the client has no
	// documented project-level file.
	project string
	// key is the top-level object the servers live in.
	key string
	// argv_in_command is set for clients that take the executable and its
	// arguments as one `command` array instead of `command` plus `args`.
	argv_in_command bool
	// fields are the fixed pairs this client needs beyond the command, such as
	// opencode's `type` discriminator.
	fields []Field
	// create_user is false when the path is not confirmed for this platform. A
	// missing file there is reported rather than created, because writing a
	// config to a place the client never reads is the worst possible outcome:
	// it looks installed and is not.
	create_user bool
}

// path_for is the file this harness reads for the requested scope.
fn (h Harness) path_for(project_root string, project bool) string {
	if !project {
		return h.user
	}
	if h.project == '' {
		return ''
	}
	return os.join_path(project_root, h.project)
}

// has_project_scope reports whether `--project` means anything for this client.
fn (h Harness) has_project_scope() bool {
	return h.project != ''
}

// entry renders one server entry as JSON text, in the shape this client expects.
fn (h Harness) entry(exe string, args []string) string {
	mut parts := []string{}
	if h.argv_in_command {
		mut argv := [exe]
		argv << args
		parts << '"command": ' + json_string_array(argv)
	} else {
		parts << '"command": ' + json_string(exe)
		parts << '"args": ' + json_string_array(args)
	}
	for f in h.fields {
		parts << '"${f.key}": ${f.value}'
	}
	return '{${parts.join(', ')}}'
}

// json_string quotes one value through the JSON writer, so a Windows path keeps
// its backslashes instead of turning them into escapes.
fn json_string(value string) string {
	return '"' + value.replace('\\', '\\\\').replace('"', '\\"') + '"'
}

fn json_string_array(values []string) string {
	if values.len == 0 {
		return '[]'
	}
	mut quoted := []string{}
	for v in values {
		quoted << json_string(v)
	}
	return '[' + quoted.join(', ') + ']'
}

// harnesses returns every client V knows how to register with.
//
// Each path here was read out of that client's own documentation or its live
// configuration file, not from memory. A client whose project-level file is not
// documented has an empty `project`, and says so rather than guessing.
fn harnesses() []Harness {
	home := os.home_dir()
	// VS Code and Zed keep their configuration under the platform config
	// directory, which is `%APPDATA%` on Windows and `~/.config` elsewhere.
	config := os.config_dir() or { os.join_path(home, '.config') }
	return [
		Harness{
			name: 'opencode'
			label: 'opencode'
			// opencode reads `~/.config/opencode/` on every platform, including
			// Windows, so this must not follow the platform config directory.
			user:            os.join_path(home, '.config', 'opencode', 'opencode.json')
			project:         os.join_path('.opencode', 'opencode.json')
			key:             'mcp'
			argv_in_command: true
			fields:          [Field{ key: 'type', value: '"local"' }, Field{
				key:   'enabled'
				value: 'true'
			}]
		},
		Harness{
			name:    'claude-code'
			label:   'Claude Code'
			user:    os.join_path(home, '.claude.json')
			project: '.mcp.json'
			key:     'mcpServers'
		},
		Harness{
			name:    'cursor'
			label:   'Cursor'
			user:    os.join_path(home, '.cursor', 'mcp.json')
			project: os.join_path('.cursor', 'mcp.json')
			key:     'mcpServers'
		},
		Harness{
			name:    'vscode'
			label:   'VS Code'
			user:    os.join_path(config, 'Code', 'User', 'mcp.json')
			project: os.join_path('.vscode', 'mcp.json')
			key:     'servers'
			// `.vscode/mcp.json` is deprecated in favour of the portable
			// `.mcp.json`, which VS Code also reads and which is Claude Code's own
			// project file. The path here stays the VS Code one, because naming
			// one client should not edit another client's file.
		},
		Harness{
			name:   'zed'
			label:  'Zed'
			user:   os.join_path(config, 'Zed', 'settings.json')
			key:    'context_servers'
			// Zed documents its servers inside its settings file and says
			// nothing about a project-level file, so none is invented here.
			// The Windows path is also unconfirmed, so a missing file is reported
			// rather than created.
			create_user: false
		},
		Harness{
			name:    'gemini'
			label:   'Gemini CLI'
			user:    os.join_path(home, '.gemini', 'settings.json')
			project: os.join_path('.gemini', 'settings.json')
			key:     'mcpServers'
		},
	]
}

// find_harness looks up a client by the name the user typed.
fn find_harness(name string) ?Harness {
	for h in harnesses() {
		if h.name == name {
			return h
		}
	}
	return none
}