module mcp

import net.http
import time

fn test_server_routes_initialize_and_registered_features() {
	mut server := new_server(
		name:         'test-server'
		version:      '1.2.3'
		instructions: 'Be precise.'
	)
	server.add_tool(Tool{
		name:        'say_hello'
		description: 'Returns a greeting'
	}, fn (ctx Context, arguments string) !ToolResult {
		assert ctx.session_id == stdio_session_id
		assert ctx.transport == .stdio
		assert arguments == '{"name":"V"}'
		return tool_text_result('Hello, V!')
	})!
	server.add_resource(Resource{
		uri:       'resource://guide'
		name:      'guide'
		mime_type: 'text/plain'
	}, fn (_ Context, uri string) !ReadResourceResult {
		return ReadResourceResult{
			contents: [
				ResourceContents{
					uri:       uri
					mime_type: 'text/plain'
					text:      'guide contents'
				},
			]
		}
	})!
	server.add_resource_template(ResourceTemplate{
		uri_template: 'resource://docs/{slug}'
		name:         'docs'
	})!
	server.add_prompt(Prompt{
		name:        'review'
		description: 'Review some code'
		arguments:   [
			PromptArgument{
				name:     'code'
				required: true
			},
		]
	}, fn (_ Context, arguments string) !GetPromptResult {
		assert arguments == '{"code":"fn main() {}"}'
		return GetPromptResult{
			description: 'Review prompt'
			messages:    [
				prompt_text_message('user', 'Review this code'),
			]
		}
	})!

	init_request := Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{"roots":{}}'
			client_info:      Implementation{
				name:    'test-client'
				version: '0.1.0'
			}
		})
	}
	init_dispatch := server.dispatch_message(init_request.encode(), stdio_session_id, .stdio)!
	assert init_dispatch.has_response
	init_response := decode_response(init_dispatch.response)!
	init_result := init_response.decode_result[InitializeResult]()!
	assert init_result.server_info.name == 'test-server'
	assert init_result.instructions == 'Be precise.'
	assert init_result.capabilities == '{"tools":{},"resources":{},"prompts":{}}'

	blocked_dispatch := server.dispatch_message(new_request(2, 'tools/list', empty).encode(),
		stdio_session_id, .stdio)!
	blocked_response := decode_response(blocked_dispatch.response)!
	assert blocked_response.error.code == server_not_initialized.code

	initialized := server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!
	assert !initialized.has_response

	tools_list := server.dispatch_message(new_request(3, 'tools/list', empty).encode(),
		stdio_session_id, .stdio)!
	tools_result := decode_response(tools_list.response)!.decode_result[ListToolsResult]()!
	assert tools_result.tools.len == 1
	assert tools_result.tools[0].name == 'say_hello'

	tool_call := server.dispatch_message(Request{
		id:     encode_id(4)
		method: 'tools/call'
		params: '{"name":"say_hello","arguments":{"name":"V"}}'
	}.encode(), stdio_session_id, .stdio)!
	tool_result := decode_response(tool_call.response)!.decode_result[ToolResult]()!
	assert tool_result.content.contains('Hello, V!')
	assert !tool_result.is_error

	resource_list := server.dispatch_message(new_request(5, 'resources/list', empty).encode(),
		stdio_session_id, .stdio)!
	resource_list_result :=
		decode_response(resource_list.response)!.decode_result[ListResourcesResult]()!
	assert resource_list_result.resources.len == 1
	assert resource_list_result.resources[0].uri == 'resource://guide'

	resource_templates := server.dispatch_message(new_request(6, 'resources/templates/list', empty).encode(),
		stdio_session_id, .stdio)!
	resource_template_result :=
		decode_response(resource_templates.response)!.decode_result[ListResourceTemplatesResult]()!
	assert resource_template_result.resource_templates.len == 1
	assert resource_template_result.resource_templates[0].uri_template == 'resource://docs/{slug}'

	resource_read := server.dispatch_message(new_request(7, 'resources/read', ReadResourceParams{
		uri: 'resource://guide'
	}).encode(), stdio_session_id, .stdio)!
	resource_read_result :=
		decode_response(resource_read.response)!.decode_result[ReadResourceResult]()!
	assert resource_read_result.contents.len == 1
	assert resource_read_result.contents[0].text == 'guide contents'

	prompts_list := server.dispatch_message(new_request(8, 'prompts/list', empty).encode(),
		stdio_session_id, .stdio)!
	prompts_list_result :=
		decode_response(prompts_list.response)!.decode_result[ListPromptsResult]()!
	assert prompts_list_result.prompts.len == 1
	assert prompts_list_result.prompts[0].name == 'review'

	prompt_get := server.dispatch_message(Request{
		id:     encode_id(9)
		method: 'prompts/get'
		params: '{"name":"review","arguments":{"code":"fn main() {}"}}'
	}.encode(), stdio_session_id, .stdio)!
	prompt_result := decode_response(prompt_get.response)!.decode_result[GetPromptResult]()!
	assert prompt_result.messages.len == 1
	assert prompt_result.messages[0].role == 'user'

	ping := server.dispatch_message(new_request(10, 'ping', empty).encode(), stdio_session_id,
		.stdio)!
	ping_result := decode_response(ping.response)!.decode_result[EmptyObject]()!
	assert ping_result == empty_object
}

fn test_server_http_sessions_and_delete() {
	mut server_value := new_server(
		name:    'http-server'
		version: '0.0.1'
	)
	mut server := &server_value
	server.add_tool(Tool{
		name: 'ping_tool'
	}, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('pong')
	})!

	server_thread := spawn server.serve_http('127.0.0.1:0')
	server.wait_till_running(max_retries: 200, retry_period_ms: 10)!
	time.sleep(20 * time.millisecond)
	addr := server.http_server.addr
	url := 'http://${addr}/mcp'

	mut init_header := http.new_header(http.HeaderConfig{
		key:   .content_type
		value: 'application/json'
	}, http.HeaderConfig{
		key:   .accept
		value: 'application/json'
	})
	init_response := http.fetch(
		method: .post
		url:    url
		data:   Request{
			id:     encode_id(1)
			method: 'initialize'
			params: encode_initialize_params(InitializeParams{
				protocol_version: protocol_version
				capabilities:     '{}'
				client_info:      Implementation{
					name:    'http-client'
					version: '0.1.0'
				}
			})
		}.encode()
		header: init_header
	)!
	assert init_response.status_code == 200
	session_id := init_response.header.get_custom(mcp_session_id_header) or {
		assert false
		return
	}
	assert session_id != ''

	mut notification_header := init_header
	notification_header.set_custom(mcp_session_id_header, session_id)!
	notification_response := http.fetch(
		method: .post
		url:    url
		data:   new_notification('notifications/initialized', empty).encode()
		header: notification_header
	)!
	assert notification_response.status_code == 202

	mut list_header := notification_header
	list_header.set(.accept, 'text/event-stream')
	list_response := http.fetch(
		method: .post
		url:    url
		data:   new_request(2, 'tools/list', empty).encode()
		header: list_header
	)!
	assert list_response.status_code == 200
	assert list_response.header.get(.content_type)!.starts_with(event_stream_content_type)
	list_messages := parse_sse_messages(list_response.body)!
	assert list_messages.len == 1
	list_result := decode_response(list_messages[0])!.decode_result[ListToolsResult]()!
	assert list_result.tools.len == 1
	assert list_result.tools[0].name == 'ping_tool'

	mut delete_header := http.new_header()
	delete_header.set_custom(mcp_session_id_header, session_id)!
	delete_response := http.fetch(
		method: .delete
		url:    url
		header: delete_header
	)!
	assert delete_response.status_code == 200

	mut stale_header := init_header
	stale_header.set_custom(mcp_session_id_header, session_id)!
	stale_response := http.fetch(
		method: .post
		url:    url
		data:   new_request(3, 'tools/list', empty).encode()
		header: stale_header
	)!
	assert stale_response.status_code == 404
	server.close()
	server_thread.wait() or {}
}
