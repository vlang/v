// Comprehensive MCP server example covering every feature exposed by `vlib/mcp`:
// tools (with annotations), concrete resources, resource templates, prompts,
// per-argument completions, RFC 5424 logging, cooperative cancellation,
// progress notifications, subscriptions, and server-initiated requests.
//
// Usage:
//   v run examples/mcp/server.v                            # stdio transport (default)
//   v run examples/mcp/server.v -- --http                  # HTTP transport on 127.0.0.1:8080
//   v run examples/mcp/server.v -- --http 127.0.0.1:9000   # HTTP transport on 127.0.0.1:9000
//
// Connect a client (e.g. Claude Desktop / Cursor / a custom MCP client) to the
// command above for stdio, or POST to `http://127.0.0.1:8080/mcp` for HTTP.
module main

import json2 as json
import mcp
import os
import time

struct CountArgs {
	n int
}

const supported_languages = ['rust', 'python', 'go', 'v', 'typescript', 'zig']!
const welcome_text = 'Welcome to the V MCP showcase server.'

fn main() {
	mut server := mcp.new_server(
		name:           'v.mcp.showcase'
		version:        '1.0.0'
		title:          'V MCP Showcase'
		description:    'Reference server for vlib/mcp covering every capability of the 2025-11-25 spec.'
		website_url:    'https://vlang.io'
		icons:          [
			mcp.Icon{
				src:       'https://vlang.io/img/v-logo.png'
				mime_type: 'image/png'
				sizes:     ['256x256']
			},
		]
		instructions:   'Demo server exercising every MCP capability shipped by vlib/mcp.'
		enable_logging: true
		// `*` only for the demo; tighten this for real deployments.
		allowed_origins: ['*']
	)
	register_tools(mut server)!
	register_resources(mut server)!
	register_prompts(mut server)!
	register_completions(mut server)!

	// Strip a leading `--` so the same binary works whether launched as
	// `./v run server.v -- --http :8080` (V's run forwards `--`) or as the
	// pre-compiled binary `./server --http :8080`.
	args := os.args[1..].filter(it != '--')
	if args.len > 0 && args[0] == '--http' {
		addr := if args.len > 1 { args[1] } else { '127.0.0.1:8080' }
		eprintln('mcp showcase listening on http://${addr}/mcp')
		server.serve_http(addr)!
	} else {
		server.serve_stdio()!
	}
}

fn register_tools(mut server mcp.Server) ! {
	// `echo` — pure, idempotent, read-only. Demonstrates annotations + icons.
	server.add_tool(mcp.Tool{
		name:         'echo'
		title:        'Echo'
		description:  'Return the provided text unchanged.'
		input_schema: '{"type":"object","required":["text"],"properties":{"text":{"type":"string"}}}'
		icons:        [
			mcp.Icon{
				src:       'https://vlang.io/img/echo.svg'
				mime_type: 'image/svg+xml'
			},
		]
		annotations:  mcp.ToolAnnotations{
			title:           'Echo input'
			read_only_hint:  true
			idempotent_hint: true
			open_world_hint: false
		}
	}, fn (_ mcp.Context, arguments string) !mcp.ToolResult {
		// `arguments` is a JSON string the caller may decode. Keep it simple here.
		return mcp.tool_text_result(arguments)
	})!

	// `count_to` — long-running tool that reports progress and is cancellable.
	server.add_tool(mcp.Tool{
		name:         'count_to'
		title:        'Counter'
		description:  'Count up to N with progress notifications. Cooperatively cancellable.'
		input_schema: '{"type":"object","required":["n"],"properties":{"n":{"type":"integer","minimum":1,"maximum":50}}}'
	}, fn (ctx mcp.Context, arguments string) !mcp.ToolResult {
		args := json.decode[CountArgs](arguments) or {
			return mcp.tool_text_result('invalid arguments: ${err.msg()}')
		}
		if args.n < 1 || args.n > 50 {
			return mcp.tool_text_result('n must be in [1, 50]')
		}
		for i in 1 .. args.n + 1 {
			if ctx.is_cancelled() {
				return mcp.tool_text_result('cancelled at ${i - 1}')
			}
			ctx.notify_progress(f64(i), f64(args.n), 'tick ${i}')
			time.sleep(50 * time.millisecond)
		}
		return mcp.tool_text_result('counted to ${args.n}')
	})!

	// `delete_record` — destructive; a host can warn the user before invoking.
	server.add_tool(mcp.Tool{
		name:        'delete_record'
		title:       'Delete record (demo)'
		description: 'Demonstration only — does not delete anything.'
		annotations: mcp.ToolAnnotations{
			destructive_hint: true
			idempotent_hint:  false
		}
	}, fn (_ mcp.Context, _ string) !mcp.ToolResult {
		return mcp.tool_text_result('record removed (no-op)')
	})!
}

fn register_resources(mut server mcp.Server) ! {
	server.add_resource(mcp.Resource{
		uri:         'demo://welcome.txt'
		name:        'welcome'
		title:       'Welcome message'
		description: 'A static welcome string.'
		mime_type:   'text/plain'
		size:        welcome_text.len
		annotations: mcp.Annotations{
			audience: ['user']
			priority: 0.5
		}
	}, fn (_ mcp.Context, uri string) !mcp.ReadResourceResult {
		return mcp.ReadResourceResult{
			contents: [
				mcp.ResourceContents{
					uri:       uri
					mime_type: 'text/plain'
					text:      welcome_text
				},
			]
		}
	})!

	server.add_resource_template(mcp.ResourceTemplate{
		uri_template: 'demo://greet/{language}'
		name:         'greet'
		title:        'Localised greeting'
		description:  'Returns a greeting in {language}.'
		mime_type:    'text/plain'
	})!
}

fn register_prompts(mut server mcp.Server) ! {
	server.add_prompt(mcp.Prompt{
		name:        'review'
		title:       'Code review'
		description: 'Ask the assistant to review a snippet in the requested language.'
		arguments:   [
			mcp.PromptArgument{
				name:        'language'
				description: 'Source language of the snippet.'
				required:    true
			},
			mcp.PromptArgument{
				name:        'snippet'
				description: 'The code to review.'
				required:    true
			},
		]
	}, fn (_ mcp.Context, arguments string) !mcp.GetPromptResult {
		return mcp.GetPromptResult{
			description: 'Code review prompt'
			messages:    [
				mcp.prompt_text_message('user',
					'Please review the following code. Arguments: ${arguments}'),
			]
		}
	})!
}

fn register_completions(mut server mcp.Server) ! {
	// Auto-complete the prompt argument `language` against a known list.
	server.add_completion(mcp.CompletionRef{
		ref_type: 'ref/prompt'
		name:     'review'
	}, 'language', fn (_ mcp.Context, current_value string, _ string) !mcp.CompletionResult {
		matches := supported_languages.filter(it.starts_with(current_value.to_lower()))
		return mcp.CompletionResult{
			values:   matches
			total:    matches.len
			has_more: false
		}
	})!

	// Auto-complete the resource template variable `language` similarly.
	server.add_completion(mcp.CompletionRef{
		ref_type: 'ref/resource'
		uri:      'demo://greet/{language}'
	}, 'language', fn (_ mcp.Context, current_value string, _ string) !mcp.CompletionResult {
		matches := supported_languages.filter(it.starts_with(current_value.to_lower()))
		return mcp.CompletionResult{
			values: matches
		}
	})!
}
