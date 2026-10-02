// The resources this server publishes.
//
// Resources are the read side of the same ground the tools cover: the compiler's
// own help topics and language documentation, served without an agent having to
// shell out to `v help` or guess a path into the source tree.
module main

import mcp
import os

// help_root is where `v help <topic>` finds its topics.
fn help_root(ws &Workspace) string {
	return os.join_path(ws.vroot, 'vlib', 'v', 'help')
}

// docs_root is where the language reference lives.
fn docs_root(ws &Workspace) string {
	return os.join_path(ws.vroot, 'doc')
}

// version_text is what `v://version` serves.
fn version_text() string {
	return os.getenv_opt('V_VERSION') or { 'V ' + server_version }
}

// register_text_resource publishes a resource whose body is one file on disk.
//
// The URI and the path are bound when the handler is built, so a caller cannot
// reach another file by sending a different URI.
fn register_text_resource(mut server mcp.Server, uri string, name string, title string,
	description string, path string) ! {
	handler := text_resource_handler(uri, path)
	server.add_resource(mcp.Resource{
		uri:         uri
		name:        name
		title:       title
		description: description
		mime_type:   mime_of(path)
	}, handler)!
}

// register_resources publishes the read-only documents this server can serve.
fn register_resources(mut server mcp.Server, ws &Workspace) ! {
	server.add_resource(mcp.Resource{
		uri:         'v://version'
		name:        'compiler_version'
		title:       'V compiler version'
		description: 'The version and commit of the compiler this server runs.'
		mime_type:   'text/plain'
	}, version_resource_handler())!

	mut help_paths := os.walk_ext(help_root(ws), '.txt')
	help_paths.sort()
	for path in help_paths {
		topic := os.file_name(path).all_before_last('.txt')
		register_text_resource(mut server, 'v://help/${topic}', 'v_help_${topic}',
			if topic == 'default' { 'v help' } else { 'v help ${topic}' },
			if topic == 'default' {
				'The top level V command line help.'
			} else {
				'The V command line help for `${topic}`.'
			}, path)!
	}
	register_text_resource(mut server, 'v://docs/language', 'v_language_reference',
		'V language reference', 'The V language specification, the document the language rules come from.',
		os.join_path(docs_root(ws), 'docs.md'))!

	server.add_resource_template(mcp.ResourceTemplate{
		uri_template: 'v://help/{topic}'
		name:         'v_help_topic'
		title:        'v help topic'
		description:  'An installed help topic from `resources/list`, for example `v://help/test`.'
		mime_type:    'text/plain'
	})!
}

// version_resource_handler serves the compiler version.
fn version_resource_handler() mcp.ResourceHandler {
	return fn (_ mcp.Context, _ string) !mcp.ReadResourceResult {
		return read_resource_result('v://version', 'text/plain', version_text())
	}
}

// text_resource_handler serves one file from disk under a fixed URI.
fn text_resource_handler(uri string, path string) mcp.ResourceHandler {
	return fn [uri, path] (_ mcp.Context, _ string) !mcp.ReadResourceResult {
		contents := os.read_file(path) or {
			return error('`${path}` could not be read: ${err.msg()}')
		}
		return read_resource_result(uri, mime_of(path), contents)
	}
}

// read_resource_result wraps one text document as an MCP resource result.
fn read_resource_result(uri string, mime_type string, text string) mcp.ReadResourceResult {
	return mcp.ReadResourceResult{
		contents: [
			mcp.ResourceContents{
				uri:       uri
				mime_type: mime_type
				text:      text
			},
		]
	}
}

// mime_of picks the content type a path is served as.
fn mime_of(path string) string {
	return if path.ends_with('.md') { 'text/markdown' } else { 'text/plain' }
}
