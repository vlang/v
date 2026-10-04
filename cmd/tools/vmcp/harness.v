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
	// project is the config file inside a project, empty when only the
	// user-level file is handled for the client.
	project string
	// project_also are the other project files the client reads in the same
	// role. The first of `project` and these that already exists is the one
	// edited; `project` is created when none of them is there.
	project_also []string
	// key is the top-level object the servers live in.
	key string
	// argv_in_command is set for clients that take the executable and its
	// arguments as one `command` array instead of `command` plus `args`.
	argv_in_command bool
	// fields are the fixed pairs this client needs beyond the command, such as
	// opencode's `type` discriminator.
	fields []Field
	// no_create_user is set when a missing user-level file is reported rather
	// than created. It never applies to a project file, which sits at the path
	// the client documents and is always safe to create.
	no_create_user bool
}

// path_for is the file this harness reads for the requested scope.
fn (h Harness) path_for(project_root string, project bool) string {
	if !project {
		return h.user
	}
	if h.project == '' {
		return ''
	}
	primary := os.join_path(project_root, h.project)
	if os.exists(primary) {
		return primary
	}
	for rel in h.project_also {
		path := os.join_path(project_root, rel)
		if os.exists(path) {
			return path
		}
	}
	return primary
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
// Each path here follows where that client itself reads its configuration. A
// client with no project-level file here has an empty `project`, and says so
// rather than guessing.
fn harnesses() []Harness {
	home := os.home_dir()
	// VS Code keeps its configuration under the platform config directory:
	// `%APPDATA%` on Windows, `~/Library/Application Support` on macOS, and
	// `$XDG_CONFIG_HOME` (default `~/.config`) elsewhere.
	config := os.config_dir() or { os.join_path(home, '.config') }
	return [
		Harness{
			name:            'opencode'
			label:           'opencode'
			// opencode reads `~/.config/opencode/` on every platform, including
			// Windows, so this must not follow the platform config directory.
			user:            os.join_path(home, '.config', 'opencode', 'opencode.json')
			// The project file is `opencode.json` at the root. An existing one of
			// the other files opencode reads there is edited instead, in the order
			// its own `opencode mcp add` looks for them.
			project:         'opencode.json'
			project_also:    ['opencode.jsonc', os.join_path('.opencode', 'opencode.json'),
				os.join_path('.opencode', 'opencode.jsonc')]
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
			// VS Code spells the object `servers`, not `mcpServers`.
			key:     'servers'
		},
		Harness{
			name:           'zed'
			label:          'Zed'
			user:           zed_settings_file(os.user_os(), home, config, os.getenv('FLATPAK_XDG_CONFIG_HOME'))
			key:            'context_servers'
			// The servers live in Zed's settings file, which holds every other
			// Zed setting too, so it is only ever added to, never started from
			// nothing. Only the user-level file is handled.
			no_create_user: true
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

// zed_settings_file is the user settings file Zed reads, following Zed's own
// `paths::config_dir`: `%APPDATA%\Zed` on Windows; on Linux and FreeBSD
// `$FLATPAK_XDG_CONFIG_HOME/zed` inside a Flatpak, otherwise
// `$XDG_CONFIG_HOME/zed` (default `~/.config/zed`); and `~/.config/zed`
// everywhere else, macOS included, whatever XDG says. `config` is the platform
// config directory, as `os.config_dir` reports it.
fn zed_settings_file(os_name string, home string, config string, flatpak_config string) string {
	dir := match os_name {
		'windows' {
			os.join_path(config, 'Zed')
		}
		'linux', 'freebsd' {
			os.join_path(if flatpak_config != '' { flatpak_config } else { config }, 'zed')
		}
		else {
			os.join_path(home, '.config', 'zed')
		}
	}
	return os.join_path(dir, 'settings.json')
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
