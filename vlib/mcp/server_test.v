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
	assert init_result.capabilities == '{"tools":{"listChanged":true},"resources":{"listChanged":true,"subscribe":true},"prompts":{"listChanged":true}}'

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
	assert list_response.header.get(.content_type)?.starts_with(event_stream_content_type)
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

fn http_initialize(url string) !(string, http.Header) {
	mut header := http.new_header(http.HeaderConfig{
		key:   .content_type
		value: 'application/json'
	}, http.HeaderConfig{
		key:   .accept
		value: 'application/json, text/event-stream'
	})
	response := http.fetch(
		method: .post
		url:    url
		data:   Request{
			id:     encode_id(1)
			method: 'initialize'
			params: encode_initialize_params(InitializeParams{
				protocol_version: protocol_version
				capabilities:     '{}'
				client_info:      Implementation{
					name:    'spec-test'
					version: '0.0.1'
				}
			})
		}.encode()
		header: header
	)!
	if response.status_code != 200 {
		return error('initialize failed: ${response.status_code}')
	}
	session_id := response.header.get_custom(mcp_session_id_header) or {
		return error('missing session id')
	}
	return session_id, header
}

fn test_http_rejects_disallowed_origin() {
	mut server_value := new_server(
		name:            'origin-server'
		version:         '0.0.1'
		allowed_origins: ['https://allowed.example']
	)
	mut server := &server_value
	server_thread := spawn server.serve_http('127.0.0.1:0')
	server.wait_till_running(max_retries: 200, retry_period_ms: 10)!
	time.sleep(20 * time.millisecond)
	url := 'http://${server.http_server.addr}/mcp'

	mut header := http.new_header(http.HeaderConfig{
		key:   .content_type
		value: 'application/json'
	}, http.HeaderConfig{
		key:   .accept
		value: 'application/json, text/event-stream'
	})
	header.set_custom('Origin', 'https://attacker.example')!
	response := http.fetch(
		method: .post
		url:    url
		data:   Request{
			id:     encode_id(1)
			method: 'initialize'
			params: encode_initialize_params(InitializeParams{
				protocol_version: protocol_version
				capabilities:     '{}'
				client_info:      Implementation{
					name:    'attacker'
					version: '0.0.1'
				}
			})
		}.encode()
		header: header
	)!
	assert response.status_code == 403

	server.close()
	server_thread.wait() or {}
}

fn test_http_rejects_unsupported_protocol_version_header() {
	mut server_value := new_server(
		name:    'version-server'
		version: '0.0.1'
	)
	mut server := &server_value
	server.add_tool(Tool{ name: 'noop' }, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!

	server_thread := spawn server.serve_http('127.0.0.1:0')
	server.wait_till_running(max_retries: 200, retry_period_ms: 10)!
	time.sleep(20 * time.millisecond)
	url := 'http://${server.http_server.addr}/mcp'

	session_id, mut header := http_initialize(url)!
	header.set_custom(mcp_session_id_header, session_id)!
	header.set_custom(mcp_protocol_version_header, '1999-01-01')!
	response := http.fetch(
		method: .post
		url:    url
		data:   new_notification('notifications/initialized', empty).encode()
		header: header
	)!
	assert response.status_code == 400

	server.close()
	server_thread.wait() or {}
}

fn test_http_rejects_unacceptable_accept_header() {
	mut server_value := new_server(
		name:    'accept-server'
		version: '0.0.1'
	)
	mut server := &server_value
	server_thread := spawn server.serve_http('127.0.0.1:0')
	server.wait_till_running(max_retries: 200, retry_period_ms: 10)!
	time.sleep(20 * time.millisecond)
	url := 'http://${server.http_server.addr}/mcp'

	mut header := http.new_header(http.HeaderConfig{
		key:   .content_type
		value: 'application/json'
	}, http.HeaderConfig{
		key:   .accept
		value: 'image/png'
	})
	response := http.fetch(
		method: .post
		url:    url
		data:   new_request(1, 'ping', empty).encode()
		header: header
	)!
	assert response.status_code == 406

	server.close()
	server_thread.wait() or {}
}

fn test_tool_annotations_are_serialized() {
	mut server := new_server(name: 'annot', version: '0')
	server.add_tool(Tool{
		name:        'annotated'
		description: 'demo'
		annotations: ToolAnnotations{
			title:           'Read-only fetcher'
			read_only_hint:  true
			open_world_hint: false
		}
	}, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!

	encoded := encode_tool(server.tools['annotated'].tool)
	assert encoded.contains('"annotations":{"title":"Read-only fetcher","readOnlyHint":true,"openWorldHint":false}')
}

fn test_list_changed_notifications_are_queued_after_initialize() {
	mut server := new_server(name: 'listchanged', version: '0')
	init_request := Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'c'
				version: '0'
			}
		})
	}
	server.dispatch_message(init_request.encode(), stdio_session_id, .stdio)!
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!

	server.add_tool(Tool{ name: 'late' }, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!
	server.add_resource(Resource{ uri: 'res://x', name: 'x' }, fn (_ Context, uri string) !ReadResourceResult {
		return ReadResourceResult{
			contents: [ResourceContents{
				uri:  uri
				text: 'x'
			}]
		}
	})!
	server.add_prompt(Prompt{ name: 'late_prompt' }, fn (_ Context, _ string) !GetPromptResult {
		return GetPromptResult{}
	})!

	queue := server.drain_session_notifications(stdio_session_id)
	assert queue.len == 3
	assert decode_notification(queue[0])!.method == 'notifications/tools/list_changed'
	assert decode_notification(queue[1])!.method == 'notifications/resources/list_changed'
	assert decode_notification(queue[2])!.method == 'notifications/prompts/list_changed'
}

fn test_resources_subscribe_then_updated_notifies_only_subscribers() {
	mut server := new_server(name: 'sub', version: '0')
	server.add_resource(Resource{ uri: 'res://x', name: 'x' }, fn (_ Context, uri string) !ReadResourceResult {
		return ReadResourceResult{
			contents: [ResourceContents{
				uri:  uri
				text: 'x'
			}]
		}
	})!
	init_request := Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'c'
				version: '0'
			}
		})
	}
	server.dispatch_message(init_request.encode(), stdio_session_id, .stdio)!
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!

	subscribe := server.dispatch_message(new_request(2, 'resources/subscribe', SubscribeParams{
		uri: 'res://x'
	}).encode(), stdio_session_id, .stdio)!
	assert decode_response(subscribe.response)!.error.code == 0

	server.notify_resource_updated('res://x')
	server.notify_resource_updated('res://other')

	queue := server.drain_session_notifications(stdio_session_id)
	assert queue.len == 1
	notif := decode_notification(queue[0])!
	assert notif.method == 'notifications/resources/updated'
	assert notif.params.contains('"uri":"res://x"')

	unsubscribe := server.dispatch_message(new_request(3, 'resources/unsubscribe', SubscribeParams{
		uri: 'res://x'
	}).encode(), stdio_session_id, .stdio)!
	assert decode_response(unsubscribe.response)!.error.code == 0
	server.notify_resource_updated('res://x')
	assert server.drain_session_notifications(stdio_session_id).len == 0
}

fn test_logging_set_level_filters_messages_below_threshold() {
	mut server := new_server(name: 'log', version: '0', enable_logging: true)
	init_request := Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'c'
				version: '0'
			}
		})
	}
	init_dispatch := server.dispatch_message(init_request.encode(), stdio_session_id, .stdio)!
	init_response := decode_response(init_dispatch.response)!
	init_result := init_response.decode_result[InitializeResult]()!
	assert init_result.capabilities.contains('"logging":{}')
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!

	set_level := server.dispatch_message(new_request(2, 'logging/setLevel', SetLevelParams{
		level: 'warning'
	}).encode(), stdio_session_id, .stdio)!
	assert decode_response(set_level.response)!.error.code == 0

	server.notify_log(.debug, 'svc', '"low"')
	server.notify_log(.warning, 'svc', '"hi"')
	server.notify_log(.error, 'svc', '{"k":1}')

	queue := server.drain_session_notifications(stdio_session_id)
	assert queue.len == 2
	first := decode_notification(queue[0])!
	assert first.method == 'notifications/message'
	assert first.params.contains('"level":"warning"')
	assert first.params.contains('"logger":"svc"')
	second := decode_notification(queue[1])!
	assert second.params.contains('"level":"error"')
	assert second.params.contains('"data":{"k":1}')
}

fn test_logging_set_level_unknown_returns_invalid_params() {
	mut server := new_server(name: 'log', version: '0', enable_logging: true)
	server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'c'
				version: '0'
			}
		})
	}.encode(), stdio_session_id, .stdio)!
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!

	dispatch := server.dispatch_message(new_request(2, 'logging/setLevel', SetLevelParams{
		level: 'fatal'
	}).encode(), stdio_session_id, .stdio)!
	assert decode_response(dispatch.response)!.error.code == invalid_params.code
}

fn test_logging_disabled_rejects_set_level() {
	mut server := new_server(name: 'log', version: '0')
	server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'c'
				version: '0'
			}
		})
	}.encode(), stdio_session_id, .stdio)!
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!

	dispatch := server.dispatch_message(new_request(2, 'logging/setLevel', SetLevelParams{
		level: 'info'
	}).encode(), stdio_session_id, .stdio)!
	assert decode_response(dispatch.response)!.error.code == method_not_found.code
}

fn test_progress_token_is_extracted_and_notification_is_sent() {
	mut server := new_server(name: 'p', version: '0')
	server.add_tool(Tool{ name: 'work' }, fn (ctx Context, _ string) !ToolResult {
		ctx.notify_progress(0.25, 1.0, 'starting')
		ctx.notify_progress(1.0, 1.0, 'done')
		return tool_text_result('done')
	})!

	server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'c'
				version: '0'
			}
		})
	}.encode(), stdio_session_id, .stdio)!
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!

	server.dispatch_message('{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"work","arguments":{},"_meta":{"progressToken":"abc"}}}',
		stdio_session_id, .stdio)!

	queue := server.drain_session_notifications(stdio_session_id)
	assert queue.len == 2
	first := decode_notification(queue[0])!
	assert first.method == 'notifications/progress'
	assert first.params.contains('"progressToken":"abc"')
	assert first.params.contains('"progress":0.25')
	assert first.params.contains('"total":1')
	assert first.params.contains('"message":"starting"')
}

fn test_cancellation_marks_request_until_cleared() {
	mut server := new_server(name: 'c', version: '0')
	server.add_tool(Tool{ name: 'check' }, fn (ctx Context, _ string) !ToolResult {
		assert ctx.is_cancelled()
		return tool_text_result('seen')
	})!

	server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'c'
				version: '0'
			}
		})
	}.encode(), stdio_session_id, .stdio)!
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!

	// Request ids carry their JSON form unchanged (string vs number) so the
	// cancellation must use the exact same form per spec.
	server.dispatch_message('{"jsonrpc":"2.0","method":"notifications/cancelled","params":{"requestId":7,"reason":"user"}}',
		stdio_session_id, .stdio)!
	assert server.is_request_cancelled(stdio_session_id, '7')

	server.dispatch_message('{"jsonrpc":"2.0","id":7,"method":"tools/call","params":{"name":"check","arguments":{}}}',
		stdio_session_id, .stdio)!
	assert !server.is_request_cancelled(stdio_session_id, '7')
}

fn test_completion_complete_routes_to_registered_handler() {
	mut server := new_server(name: 'cplt', version: '0')
	server.add_prompt(Prompt{
		name:      'review'
		arguments: [PromptArgument{
			name: 'lang'
		}]
	}, fn (_ Context, _ string) !GetPromptResult {
		return GetPromptResult{}
	})!
	server.add_completion(CompletionRef{ ref_type: 'ref/prompt', name: 'review' }, 'lang', fn (_ Context, current_value string, _ string) !CompletionResult {
		candidates := ['rust', 'python', 'go', 'v']
		matches := candidates.filter(it.starts_with(current_value))
		return CompletionResult{
			values:   matches
			total:    matches.len
			has_more: false
		}
	})!

	server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'c'
				version: '0'
			}
		})
	}.encode(), stdio_session_id, .stdio)!
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!

	dispatch := server.dispatch_message('{"jsonrpc":"2.0","id":2,"method":"completion/complete","params":{"ref":{"type":"ref/prompt","name":"review"},"argument":{"name":"lang","value":"r"}}}',
		stdio_session_id, .stdio)!
	body := decode_response(dispatch.response)!.result
	assert body.contains('"values":["rust"]')
	assert body.contains('"total":1')
	assert body.contains('"hasMore":false')
}

fn test_completion_unknown_handler_returns_empty_values() {
	mut server := new_server(name: 'cplt', version: '0')
	server.add_completion(CompletionRef{ ref_type: 'ref/resource', uri: 'res://a' }, 'k', fn (_ Context, _ string, _ string) !CompletionResult {
		return CompletionResult{
			values: ['x']
		}
	})!

	server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'c'
				version: '0'
			}
		})
	}.encode(), stdio_session_id, .stdio)!
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!

	dispatch := server.dispatch_message('{"jsonrpc":"2.0","id":2,"method":"completion/complete","params":{"ref":{"type":"ref/resource","uri":"res://other"},"argument":{"name":"k","value":""}}}',
		stdio_session_id, .stdio)!
	body := decode_response(dispatch.response)!.result
	assert body == '{"completion":{"values":[]}}'
}

fn test_http_get_streams_queued_notifications_with_event_ids() {
	mut server_value := new_server(name: 'sse', version: '0', enable_logging: true)
	mut server := &server_value
	server.add_resource(Resource{ uri: 'res://x', name: 'x' }, fn (_ Context, uri string) !ReadResourceResult {
		return ReadResourceResult{
			contents: [ResourceContents{
				uri:  uri
				text: 'x'
			}]
		}
	})!

	server_thread := spawn server.serve_http('127.0.0.1:0')
	server.wait_till_running(max_retries: 200, retry_period_ms: 10)!
	time.sleep(20 * time.millisecond)
	url := 'http://${server.http_server.addr}/mcp'

	session_id, mut header := http_initialize(url)!
	header.set_custom(mcp_session_id_header, session_id)!
	notification_response := http.fetch(
		method: .post
		url:    url
		data:   new_notification('notifications/initialized', empty).encode()
		header: header
	)!
	assert notification_response.status_code == 202

	server.notify_log(.info, 'svc', '"hello"')
	server.notify_log(.warning, 'svc', '"world"')

	mut get_header := http.new_header()
	get_header.set(.accept, 'text/event-stream')
	get_header.set_custom(mcp_session_id_header, session_id)!
	stream := http.fetch(
		method: .get
		url:    url
		header: get_header
	)!
	assert stream.status_code == 200
	assert stream.header.get(.content_type)?.starts_with(event_stream_content_type)
	assert stream.body.contains('id: 1')
	assert stream.body.contains('id: 2')
	first_messages := parse_sse_messages(stream.body)!
	assert first_messages.len == 2
	assert decode_notification(first_messages[0])!.method == 'notifications/message'

	mut resume_header := get_header
	resume_header.set_custom(last_event_id_header, '1')!
	resume := http.fetch(
		method: .get
		url:    url
		header: resume_header
	)!
	assert resume.body.contains('id: 2')
	assert !resume.body.contains('id: 1\n')

	server.close()
	server_thread.wait() or {}
}

fn test_server_initiated_list_roots_round_trip() {
	mut server := new_server(name: 'roots', version: '0')
	server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'c'
				version: '0'
			}
		})
	}.encode(), stdio_session_id, .stdio)!
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!

	mut request_thread := spawn fn [mut server] () !ListRootsResult {
		return server.list_roots(stdio_session_id, 1 * time.second)
	}()

	// Drain the queued request and respond as a client would.
	mut server_request_id := ''
	deadline := time.now().add(1 * time.second)
	for time.now() < deadline {
		queued := server.drain_session_notifications(stdio_session_id)
		if queued.len != 0 {
			req := decode_request(queued[0])!
			assert req.method == 'roots/list'
			server_request_id = req.id
			break
		}
		time.sleep(2 * time.millisecond)
	}
	assert server_request_id != ''

	server.dispatch_message('{"jsonrpc":"2.0","id":${server_request_id},"result":{"roots":[{"uri":"file:///tmp","name":"tmp"}]}}',
		stdio_session_id, .stdio)!
	roots := request_thread.wait()!
	assert roots.roots.len == 1
	assert roots.roots[0].uri == 'file:///tmp'
}

fn test_server_initiated_request_returns_on_timeout() {
	mut server := new_server(name: 'roots', version: '0')
	server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'c'
				version: '0'
			}
		})
	}.encode(), stdio_session_id, .stdio)!
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!

	started := time.now()
	server.list_roots(stdio_session_id, 50 * time.millisecond) or {
		// `timed_wait` should return promptly after the deadline; allow up to
		// 10x the timeout to account for CI scheduler jitter.
		elapsed := time.now() - started
		assert elapsed < 500 * time.millisecond
		assert err.msg().contains('timeout')
		return
	}
	assert false, 'list_roots should have timed out'
}

fn test_late_response_after_timeout_does_not_leak_pending() {
	mut server := new_server(name: 'late', version: '0')
	server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'c'
				version: '0'
			}
		})
	}.encode(), stdio_session_id, .stdio)!
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!

	server.list_roots(stdio_session_id, 30 * time.millisecond) or {
		assert err.msg().contains('timeout')
	}

	// The waiter has cleaned up its semaphore; a late reply must be dropped
	// rather than parked in `pending_responses` forever.
	mut server_request_id := ''
	rlock server.state {
		session := server.state.sessions[stdio_session_id]
		server_request_id = '"server-${session.next_request_seq - 1}"'
	}
	server.dispatch_message('{"jsonrpc":"2.0","id":${server_request_id},"result":{"roots":[]}}',
		stdio_session_id, .stdio)!

	rlock server.state {
		session := server.state.sessions[stdio_session_id]
		assert session.pending_responses.len == 0
	}
}

fn test_http_get_resume_drains_queue_before_replay() {
	mut server_value := new_server(name: 'resume', version: '0', enable_logging: true)
	mut server := &server_value
	server_thread := spawn server.serve_http('127.0.0.1:0')
	server.wait_till_running(max_retries: 200, retry_period_ms: 10)!
	time.sleep(20 * time.millisecond)
	url := 'http://${server.http_server.addr}/mcp'

	session_id, mut header := http_initialize(url)!
	header.set_custom(mcp_session_id_header, session_id)!
	notification_response := http.fetch(
		method: .post
		url:    url
		data:   new_notification('notifications/initialized', empty).encode()
		header: header
	)!
	assert notification_response.status_code == 202

	// First batch: drained on the initial GET, assigns ids 1..2.
	server.notify_log(.info, 'svc', '"first"')
	server.notify_log(.info, 'svc', '"second"')

	mut get_header := http.new_header()
	get_header.set(.accept, 'text/event-stream')
	get_header.set_custom(mcp_session_id_header, session_id)!
	first := http.fetch(method: .get, url: url, header: get_header)!
	assert first.status_code == 200
	assert first.body.contains('id: 1')
	assert first.body.contains('id: 2')

	// Second batch arrives while the client is between GETs — it stays in the
	// notification queue and would be missed if the resume only replayed the
	// event log without draining first.
	server.notify_log(.info, 'svc', '"third"')
	server.notify_log(.info, 'svc', '"fourth"')

	mut resume_header := get_header
	resume_header.set_custom(last_event_id_header, '2')!
	resume := http.fetch(method: .get, url: url, header: resume_header)!
	assert resume.status_code == 200
	assert resume.body.contains('id: 3')
	assert resume.body.contains('id: 4')
	assert resume.body.contains('"third"')
	assert resume.body.contains('"fourth"')

	server.close()
	server_thread.wait() or {}
}

fn test_http_returns_json_when_accept_lists_both() {
	mut server_value := new_server(
		name:    'json-default-server'
		version: '0.0.1'
	)
	mut server := &server_value
	server_thread := spawn server.serve_http('127.0.0.1:0')
	server.wait_till_running(max_retries: 200, retry_period_ms: 10)!
	time.sleep(20 * time.millisecond)
	url := 'http://${server.http_server.addr}/mcp'

	session_id, mut header := http_initialize(url)!
	header.set_custom(mcp_session_id_header, session_id)!
	notification_response := http.fetch(
		method: .post
		url:    url
		data:   new_notification('notifications/initialized', empty).encode()
		header: header
	)!
	assert notification_response.status_code == 202

	ping_response := http.fetch(
		method: .post
		url:    url
		data:   new_request(2, 'ping', empty).encode()
		header: header
	)!
	assert ping_response.status_code == 200
	assert ping_response.header.get(.content_type)?.starts_with(default_content_type)

	server.close()
	server_thread.wait() or {}
}
