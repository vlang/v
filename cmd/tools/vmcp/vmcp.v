// `v mcp` is the Model Context Protocol server for the V compiler.
//
// It gives a coding agent the compiler's own view of a V project: what the code
// declares, what does not compile, what a symbol refers to, and the bundled
// skills that describe the language. Everything that can be answered from the
// AST is answered in process, so no compiler run is needed for a question about
// one file; only the tools that genuinely compile or run code start the
// compiler.
//
// Usage:
//   v mcp serve                     serve over stdio (what an MCP client launches)
//   v mcp serve --http 127.0.0.1:0  serve over Streamable HTTP
//   v mcp serve --root DIR          resolve relative paths against DIR
//   v mcp serve --read-only         register no tool that writes a file
//   v mcp serve --instructions      print the model instructions and exit
//   v mcp tools                     list the tools this server exposes
module main

// Winsock, for the `--http` transport.
//
// `vlib/net` asks for this itself through `#flag -lws2_32` in
// `net_windows.c.v`, but a `#flag` that comes from an imported module is emitted
// on the link line *before* the object files, and GNU ld only pulls symbols out of
// an archive for the objects it has already seen. Winsock's import library is
// therefore never searched for the objects that actually call into it, and the
// link fails with `undefined reference to __imp_getsockopt`.
//
// A `#flag` in the main module is emitted after the objects instead, which is what
// GNU ld needs. Repeating it here is therefore what makes this tool linkable with
// `-cc gcc` on Windows, rather than requiring every caller to pass
// `-cflags -lws2_32`.
// Upstream report: https://github.com/vlang/v/issues/29293
#flag windows -lws2_32

import mcp
import os

const usage = 'Usage: v mcp serve [options]\n' +
	'       v mcp tools\n' +
	'       v mcp install [client] [--project] [--print]\n' +
	'       v mcp uninstall [client|--all] [--project]\n' +
	'\n' +
	'Options:\n' +
	'  --http <addr>    serve over Streamable HTTP instead of stdio\n' +
	'  --root <dir>     resolve relative paths against <dir> (default: cwd)\n' +
	'  --read-only      register no tool that writes a file\n' +
	'  --instructions   print the model instructions and exit\n' +
	'  -h, --help       show this help and exit\n'

fn main() {
	// `v mcp ...` reaches this program as `argv = ['mcp', 'serve', ...]`, the same
	// shape every other `cmd/tools` program sees. A leading `--` is dropped so the
	// binary also works when run directly.
	passed := os.args[1..].filter(it != '--')
	args := if passed.len > 0 && passed[0] == 'mcp' { passed[1..] } else { passed }
	if args.len == 0 || args[0] in ['-h', '--help', 'help'] {
		print(usage)
		exit(if args.len == 0 { 1 } else { 0 })
	}
	match args[0] {
		'serve' {
			serve(args[1..])
		}
		'tools' {
			list_tools()
		}
		'install' {
			install(args[1..])
		}
		'uninstall' {
			uninstall(args[1..])
		}
		else {
			eprintln('v mcp: unknown subcommand `${args[0]}`')
			eprint(usage)
			exit(1)
		}
	}
}

// serve runs the server with the options in `args`.
fn serve(args []string) {
	mut http_addr := ''
	mut root := os.getwd()
	mut read_only := false
	mut show_instructions := false
	mut i := 0
	for i < args.len {
		match args[i] {
			'--http' {
				i++
				if i >= args.len {
					fail('--http needs an address, for example 127.0.0.1:8080')
				}
				http_addr = args[i]
			}
			'--root' {
				i++
				if i >= args.len {
					fail('--root needs a directory')
				}
				root = args[i]
			}
			'--read-only' {
				read_only = true
			}
			'--instructions' {
				show_instructions = true
			}
			else {
				fail('unknown option `${args[i]}`')
			}
		}
		i++
	}
	ws := new_workspace(find_vroot(os.executable()) or { @VEXEROOT }, root, read_only)
	if show_instructions {
		print(instructions(ws))
		return
	}
	mut server := mcp.new_server(server_config(ws))
	register_all(mut server, ws) or { fail(err.msg()) }
	register_resources(mut server, ws) or { fail(err.msg()) }
	if http_addr != '' {
		eprintln('v mcp serving on http://${http_addr}/mcp')
		server.serve_http(http_addr) or { fail(err.msg()) }
	} else {
		server.serve_stdio() or { fail(err.msg()) }
	}
}

// server_config describes this server in the MCP handshake.
//
// The instructions are what a client shows its model, so they are the same text
// `v mcp serve --instructions` prints.
fn server_config(ws &Workspace) mcp.ServerConfig {
	return mcp.ServerConfig{
		name:           server_name
		version:        server_version
		title:          'V language server'
		description:    'The V compiler, exposed as tools: the AST, the declarations, the
diagnostics, the standard library documentation and the bundled agent skills.'
		website_url:    'https://vlang.io'
		instructions:   instructions(ws)
		enable_logging: false
	}
}

// list_tools prints the registered tools, for documentation and for a human
// checking what an agent will be able to do.
fn list_tools() {
	for spec in tool_specs() {
		println('${spec.tool.name}\t${spec.read_only()}\t${spec.tool.description.replace('\n', ' ')}')
	}
}

// find_vroot walks up from an executable to the V source tree it belongs to.
fn find_vroot(exe_path string) ?string {
	mut dir := os.dir(os.real_path(exe_path))
	for dir.len > 0 {
		if os.is_file(os.join_path_single(dir, 'v.mod')) && os.is_dir(os.join_path(dir,
			'vlib', 'v'))
		{
			return dir
		}
		dir = os.parent_dir(dir)
	}
	return none
}

// fail reports a command line problem and exits.
@[noreturn]
fn fail(message string) {
	eprintln('v mcp: ${message}')
	eprint(usage)
	exit(1)
}
