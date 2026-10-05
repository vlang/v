module mcp

import json2 as json
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

fn test_initialize_negotiates_supported_protocol_version() {
	mut server := new_server(
		name:         'negotiation-server'
		version:      '1.0.0'
		instructions: 'Ask first.'
	)
	init_dispatch := server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version_2026_07_28
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'future-client'
				version: '2.0.0'
			}
		})
	}.encode(), stdio_session_id, .stdio)!
	init_response := decode_response(init_dispatch.response)!
	assert init_response.error.code == 0
	init_result := init_response.decode_result[InitializeResult]()!
	// The initialize result still announces the version the server prefers.
	assert init_result.protocol_version == protocol_version

	// The session itself runs on the revision the client asked for.
	rlock server.state {
		assert server.state.sessions[stdio_session_id].protocol_version ==
			protocol_version_2026_07_28
	}
}

fn test_initialize_unknown_version_announces_server_preferred() {
	mut server := new_server(
		name:    'fallback-server'
		version: '1.0.0'
	)
	init_dispatch := server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: '1999-01-01'
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'ancient-client'
				version: '0.0.1'
			}
		})
	}.encode(), stdio_session_id, .stdio)!
	init_result := decode_response(init_dispatch.response)!.decode_result[InitializeResult]()!
	assert init_result.protocol_version == protocol_version_2025_11_25

	rlock server.state {
		assert server.state.sessions[stdio_session_id].protocol_version ==
			protocol_version_2025_11_25
	}
}

fn test_server_discover_works_without_initialize_over_stdio() {
	mut server := new_server(
		name:    'discover-server'
		version: '1.0.0'
	)
	server.add_tool(Tool{ name: 'noop' }, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!

	discover := server.dispatch_message(new_request(1, 'server/discover', empty).encode(),
		stdio_session_id, .stdio)!
	assert discover.has_response
	discover_response := decode_response(discover.response)!
	assert discover_response.error.code == 0
	result := discover_response.result
	assert result.contains('"supportedVersions":["2025-11-25","2026-07-28"]')
	assert result.contains('"capabilities":{"tools":{"listChanged":true}}')
	// No instructions were configured, so the key must be absent.
	assert !result.contains('"instructions"')
	// The CacheableResult fields only exist on 2026-07-28 sessions.
	assert !result.contains('"resultType"')
	assert !result.contains('"ttlMs"')
	assert !result.contains('"cacheScope"')
}

fn test_server_discover_on_2026_session_includes_cacheable_fields() {
	mut server := new_server(
		name:         'discover-2026-server'
		version:      '1.0.0'
		instructions: 'Be brief.'
		cache_ttl_ms: 60_000
		cache_scope:  'public'
	)
	server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version_2026_07_28
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'future-client'
				version: '2.0.0'
			}
		})
	}.encode(), stdio_session_id, .stdio)!

	discover := server.dispatch_message(new_request(2, 'server/discover', empty).encode(),
		stdio_session_id, .stdio)!
	result := decode_response(discover.response)!.result
	assert result.contains('"supportedVersions":["2025-11-25","2026-07-28"]')
	assert result.contains('"instructions":"Be brief."')
	assert result.contains('"resultType":"complete"')
	assert result.contains('"ttlMs":60000')
	assert result.contains('"cacheScope":"public"')
}

fn test_server_discover_defaults_to_server_preference_without_session() {
	mut server := new_server(
		name:             'discover-default-server'
		version:          '1.0.0'
		protocol_version: protocol_version_2026_07_28
	)
	assert server.supported_versions == default_supported_versions

	discover := server.dispatch_message(new_request(1, 'server/discover', empty).encode(),
		stdio_session_id, .stdio)!
	result := decode_response(discover.response)!.result
	// With no session the server preference applies, so the 2026-only
	// CacheableResult fields are present.
	assert result.contains('"resultType":"complete"')
	assert result.contains('"ttlMs":300000')
	assert result.contains('"cacheScope":"private"')
}

// stateless_params builds 2026-07-28 request params carrying the reserved
// `_meta` keys, with an optional progress token and the given `Mcp-Name` target
// spliced into the body.
fn stateless_params(method string, name string, log_level string, client_name string) string {
	mut meta := []string{}
	meta << '"${meta_protocol_version_key}":"${protocol_version_2026_07_28}"'
	meta << '"${meta_client_capabilities_key}":{}'
	if log_level != '' {
		meta << '"${meta_log_level_key}":"${log_level}"'
	}
	if client_name != '' {
		meta << '"${meta_client_info_key}":{"name":"${client_name}","version":"9.9.9"}'
	}
	meta_fields := meta.join(',')
	mut fields := ['"_meta":{${meta_fields}}']
	match method {
		'tools/call' {
			fields << '"name":"${name}"'
			fields << '"arguments":{}'
		}
		'resources/read' {
			fields << '"uri":"${name}"'
		}
		'prompts/get' {
			fields << '"name":"${name}"'
		}
		else {}
	}
	return '{${fields.join(',')}}'
}

fn stateless_request(id int, method string, name string, log_level string, client_name string) string {
	return Request{
		id:     encode_id(id)
		method: method
		params: stateless_params(method, name, log_level, client_name)
	}.encode()
}

// build_stateless_header assembles the request headers of a 2026-07-28
// Streamable HTTP POST. An empty `version`, `method` or `name` omits that
// header, which is how the mismatch cases are built.
fn build_stateless_header(version string, method string, name string) http.Header {
	mut header := http.new_header(http.HeaderConfig{
		key:   .content_type
		value: 'application/json'
	}, http.HeaderConfig{
		key:   .accept
		value: 'application/json, text/event-stream'
	})
	if version != '' {
		header.set_custom(mcp_protocol_version_header, version) or {}
	}
	if method != '' {
		header.set_custom(mcp_method_header, method) or {}
	}
	if name != '' {
		header.set_custom(mcp_name_header, name) or {}
	}
	return header
}

fn stateless_http_header(method string, name string) http.Header {
	return build_stateless_header(protocol_version_2026_07_28, method, name)
}

fn spawn_stateless_server() !(&Server, string) {
	mut server_value := new_server(
		name:           'stateless-server'
		version:        '2.0.0'
		enable_logging: true
	)
	mut server := &server_value
	server.add_tool(Tool{ name: 'shout' }, fn (mut ctx Context, arguments string) !ToolResult {
		ctx.server.notify_log(.debug, 'shout', '{"volume":11}')
		return tool_text_result('shouting from ${ctx.protocol_version} for ${ctx.client_info.name}')
	})!
	server.add_resource(Resource{
		uri:  'resource://note'
		name: 'note'
	}, fn (_ Context, uri string) !ReadResourceResult {
		return ReadResourceResult{
			contents: [
				ResourceContents{
					uri:  uri
					text: 'a note'
				},
			]
		}
	})!
	server_thread := spawn server.serve_http('127.0.0.1:0')
	server.wait_till_running(max_retries: 200, retry_period_ms: 10)!
	time.sleep(20 * time.millisecond)
	return server, 'http://${server.http_server.addr}/mcp'
}

fn test_http_stateless_tools_list_needs_no_session() {
	mut server, url := spawn_stateless_server()!

	response := http.fetch(
		method: .post
		url:    url
		data:   stateless_request(1, 'tools/list', '', '', '')
		header: stateless_http_header('tools/list', '')
	)!
	assert response.status_code == 200
	assert response.body.contains('"resultType":"complete"')
	assert response.body.contains('"ttlMs":300000')
	assert response.body.contains('"cacheScope":"private"')
	assert response.body.contains('shout')
	// 2026-07-28 is sessionless, so no MCP-Session-Id may be handed out.
	assert response.header.get_custom(mcp_session_id_header) or { '' } == ''

	// The ephemeral session is dropped with the request.
	rlock server.state {
		assert server.state.sessions.len == 0
	}

	server.close()
}

fn test_http_stateless_rejects_version_mismatch() {
	mut server, url := spawn_stateless_server()!

	// `_meta` says 2026-07-28, the header says 2025-11-25.
	mut response := http.fetch(
		method: .post
		url:    url
		data:   stateless_request(1, 'tools/list', '', '', '')
		header: build_stateless_header(protocol_version_2025_11_25, 'tools/list', '')
	)!
	assert response.status_code == 400
	assert decode_response(response.body)!.error.code == header_mismatch.code

	// A missing header is a mismatch too.
	response = http.fetch(
		method: .post
		url:    url
		data:   stateless_request(2, 'tools/list', '', '', '')
		header: build_stateless_header('', 'tools/list', '')
	)!
	assert response.status_code == 400
	assert decode_response(response.body)!.error.code == header_mismatch.code

	server.close()
}

fn test_http_stateless_rejects_unsupported_version() {
	mut server, url := spawn_stateless_server()!

	mut header := stateless_http_header('tools/list', '')
	header.set_custom(mcp_protocol_version_header, '1999-01-01')!
	body := Request{
		id:     encode_id(1)
		method: 'tools/list'
		params: '{"_meta":{"${meta_protocol_version_key}":"1999-01-01"}}'
	}.encode()
	response := http.fetch(method: .post, url: url, data: body, header: header)!
	assert response.status_code == 400
	err := decode_response(response.body)!.error
	assert err.code == unsupported_protocol_version.code
	assert err.data.contains('"supported":["2025-11-25","2026-07-28"]')
	assert err.data.contains('"requested":"1999-01-01"')

	server.close()
}

fn test_http_stateless_requires_matching_mcp_headers() {
	mut server, url := spawn_stateless_server()!

	// Missing `Mcp-Method`.
	mut response := http.fetch(
		method: .post
		url:    url
		data:   stateless_request(1, 'tools/list', '', '', '')
		header: build_stateless_header(protocol_version_2026_07_28, '', '')
	)!
	assert response.status_code == 400
	assert decode_response(response.body)!.error.code == header_mismatch.code

	// `Mcp-Method` contradicting the body.
	response = http.fetch(
		method: .post
		url:    url
		data:   stateless_request(2, 'tools/list', '', '', '')
		header: stateless_http_header('resources/list', '')
	)!
	assert response.status_code == 400
	assert decode_response(response.body)!.error.code == header_mismatch.code

	// `Mcp-Name` contradicting the tool named in the body.
	response = http.fetch(
		method: .post
		url:    url
		data:   stateless_request(3, 'tools/call', 'shout', '', '')
		header: stateless_http_header('tools/call', 'whisper')
	)!
	assert response.status_code == 400
	assert decode_response(response.body)!.error.code == header_mismatch.code

	// `Mcp-Name` missing for a method that requires it.
	response = http.fetch(
		method: .post
		url:    url
		data:   stateless_request(4, 'tools/call', 'shout', '', '')
		header: stateless_http_header('tools/call', '')
	)!
	assert response.status_code == 400
	assert decode_response(response.body)!.error.code == header_mismatch.code

	server.close()
}

fn test_http_stateless_rejects_removed_methods() {
	mut server, url := spawn_stateless_server()!

	for method in ['ping', 'resources/subscribe'] {
		response := http.fetch(
			method: .post
			url:    url
			data:   stateless_request(1, method, '', '', '')
			header: stateless_http_header(method, '')
		)!
		assert response.status_code == 200
		assert decode_response(response.body)!.error.code == method_not_found.code
	}

	server.close()
}

fn test_http_stateless_tools_call_sees_request_state() {
	mut server, url := spawn_stateless_server()!

	response := http.fetch(
		method: .post
		url:    url
		data:   stateless_request(1, 'tools/call', 'shout', '', 'wire-client')
		header: stateless_http_header('tools/call', 'shout')
	)!
	assert response.status_code == 200
	assert response.body.contains('"resultType":"complete"')
	// The handler sees the request's own version and `_meta` clientInfo.
	assert response.body.contains('shouting from ${protocol_version_2026_07_28} for wire-client')
	// A non-cacheable method carries no cache hints.
	assert !response.body.contains('"ttlMs"')
	assert !response.body.contains('"cacheScope"')

	server.close()
}

fn test_http_stateless_resources_read_is_cacheable() {
	mut server, url := spawn_stateless_server()!

	response := http.fetch(
		method: .post
		url:    url
		data:   stateless_request(1, 'resources/read', 'resource://note', '', '')
		header: stateless_http_header('resources/read', 'resource://note')
	)!
	assert response.status_code == 200
	assert response.body.contains('"resultType":"complete"')
	assert response.body.contains('"ttlMs":300000')
	assert response.body.contains('"cacheScope":"private"')

	server.close()
}

fn test_http_stateless_logs_need_a_requested_log_level() {
	mut server, url := spawn_stateless_server()!

	// Without `_meta` logLevel the handler's log must not be delivered, not
	// even on a stream that would otherwise carry it.
	mut header := stateless_http_header('tools/call', 'shout')
	header.set(.accept, 'text/event-stream')
	silent := http.fetch(
		method: .post
		url:    url
		data:   stateless_request(1, 'tools/call', 'shout', '', '')
		header: header
	)!
	assert silent.status_code == 200
	assert !silent.body.contains('notifications/message')

	// With one, the log rides the SSE stream of the very same request.
	logged := http.fetch(
		method: .post
		url:    url
		data:   stateless_request(2, 'tools/call', 'shout', 'debug', '')
		header: header
	)!
	assert logged.status_code == 200
	assert logged.body.contains('notifications/message')
	assert logged.body.contains('"volume":11')

	server.close()
}

fn test_stdio_stateless_logs_need_a_requested_log_level() {
	mut server := new_server(
		name:           'stateless-log-server'
		version:        '2.0.0'
		enable_logging: true
	)
	server.add_tool(Tool{ name: 'shout' }, fn (mut ctx Context, _ string) !ToolResult {
		ctx.server.notify_log(.debug, 'shout', '{"volume":11}')
		return tool_text_result('ok')
	})!

	silent := server.dispatch_message(stateless_request(1, 'tools/call', 'shout', '', ''),
		stdio_session_id, .stdio)!
	assert decode_response(silent.response)!.error.code == 0
	// stdio has no response stream, so the request-scoped notification is
	// simply dropped, and no session is left behind to buffer it.
	assert !server.session_exists(stdio_session_id)

	logged := server.dispatch_message(stateless_request(2, 'tools/call', 'shout', 'debug', ''),
		stdio_session_id, .stdio)!
	assert decode_response(logged.response)!.error.code == 0
	assert !server.session_exists(stdio_session_id)
}

fn test_stdio_stateless_tools_list_needs_no_initialize() {
	mut server := new_server(
		name:    'stateless-stdio-server'
		version: '2.0.0'
	)
	server.add_tool(Tool{ name: 'shout' }, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!

	dispatch := server.dispatch_message(stateless_request(1, 'tools/list', '', '', ''),
		stdio_session_id, .stdio)!
	assert dispatch.has_response
	response := decode_response(dispatch.response)!
	assert response.error.code == 0
	assert response.result.contains('"resultType":"complete"')
	assert response.result.contains('"ttlMs":300000')
	assert response.result.contains('"cacheScope":"private"')
	// No session was created by the stateless request.
	assert !server.session_exists(stdio_session_id)

	// The 2025-11-25 path is untouched: it still needs the handshake.
	legacy := server.dispatch_message(new_request(2, 'tools/list', empty).encode(),
		stdio_session_id, .stdio)!
	assert decode_response(legacy.response)!.error.code == server_not_initialized.code
}

fn test_stdio_stateless_2025_declaration_keeps_2025_wire_behaviour() {
	mut server := new_server(
		name:    'stateless-stdio-2025-server'
		version: '2.0.0'
	)
	server.add_tool(Tool{ name: 'shout' }, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!

	// A request that declares 2025-11-25 through `_meta` is dispatched
	// sessionlessly, but must keep the 2025 wire behaviour: no `resultType`,
	// no cache hints, and the session-bound methods stay available.
	meta_params := '{"_meta":{"${meta_protocol_version_key}":"${protocol_version_2025_11_25}"}}'
	dispatch := server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'tools/list'
		params: meta_params
	}.encode(), stdio_session_id, .stdio)!
	assert dispatch.has_response
	response := decode_response(dispatch.response)!
	assert response.error.code == 0
	assert !response.result.contains('"resultType"')
	assert !response.result.contains('"ttlMs"')
	assert !response.result.contains('"cacheScope"')
	assert !server.session_exists(stdio_session_id)

	ping := server.dispatch_message(Request{
		id:     encode_id(2)
		method: 'ping'
		params: meta_params
	}.encode(), stdio_session_id, .stdio)!
	assert decode_response(ping.response)!.error.code == 0
}

// listen_request builds a 2026-07-28 `subscriptions/listen` request with the
// given opt-in filter.
fn listen_request(id int, tools_changed bool, prompts_changed bool, resources_changed bool, uris []string) string {
	mut notifications := []string{}
	if tools_changed {
		notifications << '"toolsListChanged":true'
	}
	if prompts_changed {
		notifications << '"promptsListChanged":true'
	}
	if resources_changed {
		notifications << '"resourcesListChanged":true'
	}
	if uris.len != 0 {
		notifications << '"resourceSubscriptions":${json.encode(uris)}'
	}
	return Request{
		id:     encode_id(id)
		method: 'subscriptions/listen'
		params: '{"notifications":{${notifications.join(',')}},"_meta":{"${meta_protocol_version_key}":"${protocol_version_2026_07_28}","${meta_client_capabilities_key}":{}}}'
	}.encode()
}

fn subscription_id_json(params string) !string {
	return json.decode[SubscriptionIdEnvelope](params)!.meta.subscription_id
}

fn noop_tool_handler(_ Context, _ string) !ToolResult {
	return tool_text_result('ok')
}

fn noop_prompt_handler(_ Context, _ string) !GetPromptResult {
	return GetPromptResult{}
}

fn noop_resource_handler(_ Context, uri string) !ReadResourceResult {
	return ReadResourceResult{
		contents: [
			ResourceContents{
				uri:  uri
				text: 'hi'
			},
		]
	}
}

fn test_stdio_listen_acknowledges_then_pushes_tagged_notifications() {
	mut server := new_server(
		name:    'listen-server'
		version: '1.0.0'
	)
	// A tool and a resource exist before the listen, so both kinds are honored.
	server.add_tool(Tool{ name: 'shout' }, noop_tool_handler)!
	server.add_resource(Resource{
		uri:  'resource://note'
		name: 'note'
	}, noop_resource_handler)!
	dispatch := server.dispatch_message(listen_request(7, true, false, true, ['resource://note']),
		stdio_session_id, .stdio)!
	// The stream stays open, so the listen request itself gets no response.
	assert !dispatch.has_response

	out := server.drain_session_notifications(stdio_session_id)
	assert out.len == 1
	ack := decode_notification(out[0])!
	assert ack.method == 'notifications/subscriptions/acknowledged'
	assert ack.params.contains('"toolsListChanged":true')
	assert ack.params.contains('"resourcesListChanged":true')
	assert ack.params.contains('"resourceSubscriptions":["resource://note"]')
	// A type that was not opted into is never acknowledged.
	assert !ack.params.contains('"promptsListChanged"')
	assert subscription_id_json(ack.params)! == '7'

	// An opted-in change arrives, tagged with the subscription id.
	server.add_tool(Tool{ name: 'whisper' }, noop_tool_handler)!
	mut events := server.drain_session_notifications(stdio_session_id)
	assert events.len == 1
	changed := decode_notification(events[0])!
	assert changed.method == 'notifications/tools/list_changed'
	assert subscription_id_json(changed.params)! == '7'

	// A type the subscription did not opt into never reaches its stream.
	server.add_prompt(Prompt{ name: 'review' }, noop_prompt_handler)!
	assert server.drain_session_notifications(stdio_session_id).len == 0

	// `resources/updated` only flows for a uri the subscription named.
	server.notify_resource_updated('resource://other')
	assert server.drain_session_notifications(stdio_session_id).len == 0
	server.notify_resource_updated('resource://note')
	events = server.drain_session_notifications(stdio_session_id)
	assert events.len == 1
	updated := decode_notification(events[0])!
	assert updated.method == 'notifications/resources/updated'
	assert updated.params.contains('"uri":"resource://note"')
	assert subscription_id_json(updated.params)! == '7'
}

fn test_stdio_listen_preserves_distinct_request_id_types() {
	mut server := new_server(name: 'typed-listen-server', version: '1.0.0')
	server.add_tool(Tool{ name: 'shout' }, noop_tool_handler)!
	request := decode_request(listen_request(7, true, false, false, []))!
	raw_ids := ['7', '"7"', '9007199254740993', '""', r'"quote\" slash\\ snowman\u2603"']
	for raw_id in raw_ids {
		dispatch := server.dispatch_message(Request{
			id:     raw_id
			method: request.method
			params: request.params
		}.encode(), stdio_session_id, .stdio)!
		assert !dispatch.has_response
		acknowledgements := server.drain_session_notifications(stdio_session_id)
		assert acknowledgements.len == 1
		ack := decode_notification(acknowledgements[0])!
		assert ack.method == listen_acknowledged_method
		assert subscription_id_json(ack.params)! == raw_id
	}

	server.add_tool(Tool{ name: 'whisper' }, noop_tool_handler)!
	events := server.drain_session_notifications(stdio_session_id)
	assert events.len == raw_ids.len
	mut event_ids := []string{}
	for event in events {
		notification := decode_notification(event)!
		assert notification.method == 'notifications/tools/list_changed'
		event_ids << subscription_id_json(notification.params)!
	}
	for raw_id in raw_ids {
		assert event_ids.filter(it == raw_id).len == 1
	}

	cancellations := server.terminate_listen_subscriptions()
	assert cancellations.len == raw_ids.len
	mut cancelled_ids := []string{}
	for cancellation in cancellations {
		notification := decode_notification(cancellation)!
		assert notification.method == 'notifications/cancelled'
		cancelled_ids << notification.decode_params[CancelledParams]()!.request_id
	}
	for raw_id in raw_ids {
		assert cancelled_ids.filter(it == raw_id).len == 1
	}
}

fn test_stdio_listen_terminates_with_notifications_cancelled() {
	mut server := new_server(name: 'teardown-server', version: '1.0.0')
	server.dispatch_message(listen_request(3, true, false, false, []), stdio_session_id, .stdio)!
	assert server.drain_session_notifications(stdio_session_id).len == 1

	cancellations := server.terminate_listen_subscriptions()
	assert cancellations.len == 1
	cancelled := decode_notification(cancellations[0])!
	assert cancelled.method == 'notifications/cancelled'
	assert cancelled.params.contains('"requestId":3')
	// The registrations are gone, so later changes reach nobody.
	server.add_tool(Tool{ name: 'shout' }, noop_tool_handler)!
	assert server.drain_session_notifications(stdio_session_id).len == 0
}

fn test_stdio_listen_is_not_a_2025_method() {
	mut server := new_server(name: 'legacy-server', version: '1.0.0')
	legacy := server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'subscriptions/listen'
		params: '{"notifications":{"toolsListChanged":true}}'
	}.encode(), stdio_session_id, .stdio)!
	assert decode_response(legacy.response)!.error.code == server_not_initialized.code
}

fn test_http_listen_returns_a_finite_sse_stream() {
	mut server, url := spawn_stateless_server()!

	mut header := stateless_http_header('subscriptions/listen', '')
	header.set(.accept, 'text/event-stream')
	request := decode_request(listen_request(11, true, false, false, []))!
	for raw_id in ['11', '"11"', '9007199254740993', '""', r'"quote\" slash\\ snowman\u2603"'] {
		response := http.fetch(
			method: .post
			url:    url
			data:   Request{
				id:     raw_id
				method: request.method
				params: request.params
			}.encode()
			header: header
		)!
		assert response.status_code == 200
		assert response.header.get(.content_type)?.starts_with(event_stream_content_type)
		// The acknowledgement comes first, then the closing result.
		messages := parse_sse_messages(response.body)!
		assert messages.len == 2
		ack := decode_notification(messages[0])!
		assert ack.method == listen_acknowledged_method
		assert ack.params.contains('"toolsListChanged":true')
		assert subscription_id_json(ack.params)! == raw_id
		closing := decode_response(messages[1])!
		assert closing.id == raw_id
		assert closing.error.code == 0
		assert closing.decode_result[MetaMergeProbe]()!.result_type == 'complete'
		assert subscription_id_json(closing.result)! == raw_id
		// A finite subscription is not kept after the response.
		assert response.header.get_custom(mcp_session_id_header) or { '' } == ''
	}

	server.close()
}

fn test_http_listen_requires_sse_acceptance() {
	mut server, url := spawn_stateless_server()!
	response := http.fetch(
		method: .post
		url:    url
		data:   listen_request(1, true, false, false, [])
		header: stateless_http_header('subscriptions/listen', '')
	)!
	assert response.status_code == 406
	server.close()
}

struct MetaMergeProbe {
	result_type string @[json: resultType]
	meta        string @[json: '_meta'; raw]
}

fn test_with_result_meta_member_merges_into_an_existing_meta_object() {
	merged := with_result_meta_member('{"resultType":"complete","_meta":{"${meta_subscription_id_key}":"7"}}',
		meta_server_info_key, '{"name":"srv","version":"1.0"}')
	probe := json.decode[MetaMergeProbe](merged)!
	assert probe.result_type == 'complete'
	assert probe.meta.contains('"${meta_subscription_id_key}":"7"')
	assert probe.meta.contains('"${meta_server_info_key}":{"name":"srv","version":"1.0"}')

	created := with_result_meta_member('{"resultType":"complete"}', meta_server_info_key,
		'{"name":"srv","version":"1.0"}')
	created_probe := json.decode[MetaMergeProbe](created)!
	assert created_probe.result_type == 'complete'
	assert created_probe.meta.contains('"${meta_server_info_key}"')
}

fn test_http_listen_client_server_round_trip() {
	mut server, url := spawn_stateless_server()!
	mut client := connect_2026(url, ClientConfig{
		client_info: Implementation{
			name:    'listen-round-trip'
			version: '0.1.0'
		}
	})!

	filter := client.listen(SubscriptionListenParams{
		notifications: SubscriptionFilter{
			tools_list_changed:   true
			prompts_list_changed: true
		}
	})!

	// The server honors only what it can produce: a tool is registered, but
	// no prompts are.
	assert filter.tools_list_changed
	assert !filter.prompts_list_changed

	client.close()
	server.close()
}

// mrtr_tool asks for an elicitation the first time and uses the answer on the
// retry, which is the take-or-require pattern.
fn mrtr_tool(ctx Context, _ string) !ToolResult {
	answer := ctx.take_elicit_result('ok') or {
		return ctx.require_elicit('ok', ElicitParams{
			message:          'May I proceed?'
			requested_schema: ElicitSchema{
				properties: '{"proceed":{"type":"boolean"}}'
				required:   ['proceed']
			}
		})
	}
	return tool_text_result('client said ${answer.action}')
}

fn mrtr_params(with_answer bool) string {
	meta := '{"${meta_protocol_version_key}":"${protocol_version_2026_07_28}","${meta_client_capabilities_key}":{}}'
	if !with_answer {
		return '{"name":"confirm","arguments":{},"_meta":${meta}}'
	}
	answer := '{"ok":{"action":"accept","content":{}}}'
	return '{"name":"confirm","arguments":{},"_meta":${meta},"inputResponses":${answer},"requestState":"rs-1"}'
}

fn test_stateless_input_required_asks_then_completes() {
	mut server := new_server(name: 'mrtr-server', version: '1.0.0')
	server.add_tool(Tool{ name: 'confirm' }, fn (ctx Context, _ string) !ToolResult {
		answer := ctx.take_elicit_result('ok') or {
			return ctx.require_elicit('ok', ElicitParams{
				message:          'May I proceed?'
				requested_schema: ElicitSchema{
					properties: '{"proceed":{"type":"boolean"}}'
					required:   ['proceed']
				}
			})
		}
		return tool_text_result('client said ${answer.action} state=${ctx.request_state}')
	})!

	first := server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'tools/call'
		params: mrtr_params(false)
	}.encode(), stdio_session_id, .stdio)!
	first_response := decode_response(first.response)!
	// The ask is a success, not an error.
	assert first_response.error.code == 0
	assert first_response.result.contains('"resultType":"input_required"')
	// `apply_stateless_result_fields` must not stamp a second resultType.
	assert first_response.result.count('"resultType"') == 1
	assert first_response.result.contains('"inputRequests"')
	assert first_response.result.contains('"elicitation/create"')
	assert first_response.result.contains('"proceed"')
	// `requestState` is optional and the client sent none to echo.
	assert !first_response.result.contains('"requestState"')

	second := server.dispatch_message(Request{
		id:     encode_id(2)
		method: 'tools/call'
		params: mrtr_params(true)
	}.encode(), stdio_session_id, .stdio)!
	second_response := decode_response(second.response)!
	assert second_response.error.code == 0
	assert second_response.result.contains('"resultType":"complete"')
	assert second_response.result.contains('client said accept')
	assert !second_response.result.contains('input_required')
	// The handler is re-invoked from the start and sees the echoed state.
	assert second_response.result.contains('state=rs-1')
}

fn test_input_required_stays_an_error_on_2025() {
	mut server := new_server(name: 'mrtr-2025-server', version: '1.0.0')
	server.add_tool(Tool{ name: 'confirm' }, mrtr_tool)!

	legacy := server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'tools/call'
		params: '{"name":"confirm","arguments":{}}'
	}.encode(), stdio_session_id, .stdio)!
	// Without a handshake the request is refused before the handler runs.
	assert decode_response(legacy.response)!.error.code == server_not_initialized.code

	server.dispatch_message(Request{
		id:     encode_id(2)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{}'
			client_info:      Implementation{
				name:    'legacy-client'
				version: '0'
			}
		})
	}.encode(), stdio_session_id, .stdio)!
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio)!

	stateful := server.dispatch_message(Request{
		id:     encode_id(3)
		method: 'tools/call'
		params: '{"name":"confirm","arguments":{}}'
	}.encode(), stdio_session_id, .stdio)!
	err := decode_response(stateful.response)!.error
	// 2025-11-25 has no way to ask through the result, so it stays an error.
	assert err.code == internal_error.code
}

fn test_stdio_listen_acknowledgement_omits_kinds_the_server_cannot_produce() {
	mut server := new_server(name: 'partial-server', version: '1.0.0')
	// Only tools exist, so the acknowledgement must drop the other kinds even
	// though the client asked for all of them.
	server.add_tool(Tool{ name: 'shout' }, noop_tool_handler)!

	dispatch := server.dispatch_message(listen_request(4, true, true, true, []),
		stdio_session_id, .stdio)!
	assert !dispatch.has_response
	out := server.drain_session_notifications(stdio_session_id)
	assert out.len == 1
	ack := decode_notification(out[0])!
	assert ack.params.contains('"toolsListChanged":true')
	assert !ack.params.contains('"promptsListChanged"')
	assert !ack.params.contains('"resourcesListChanged"')

	// A kind the server dropped is never pushed either.
	server.add_prompt(Prompt{ name: 'review' }, noop_prompt_handler)!
	assert server.drain_session_notifications(stdio_session_id).len == 0
	// The honored kind still flows.
	server.add_tool(Tool{ name: 'whisper' }, noop_tool_handler)!
	events := server.drain_session_notifications(stdio_session_id)
	assert events.len == 1
	assert decode_notification(events[0])!.method == 'notifications/tools/list_changed'
}

fn test_stateless_request_requires_client_capabilities_in_meta() {
	mut server := new_server(name: 'strict-server', version: '1.0.0')
	server.add_tool(Tool{ name: 'shout' }, noop_tool_handler)!

	// A 2026-07-28 request must name both the revision and the capabilities.
	missing := '{"_meta":{"${meta_protocol_version_key}":"${protocol_version_2026_07_28}"}}'
	dispatch := server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'tools/list'
		params: missing
	}.encode(), stdio_session_id, .stdio)!
	response := decode_response(dispatch.response)!
	assert response.error.code == invalid_request.code
	assert response.error.code == -32600
	// The message names the key that is missing.
	assert response.error.message.contains(meta_client_capabilities_key)

	// With the key present the same request is served normally.
	ok_params := '{"_meta":{"${meta_protocol_version_key}":"${protocol_version_2026_07_28}",' +
		'"${meta_client_capabilities_key}":{}}'
	ok := server.dispatch_message(Request{
		id:     encode_id(2)
		method: 'tools/list'
		params: ok_params
	}.encode(), stdio_session_id, .stdio)!
	assert decode_response(ok.response)!.error.code == 0
}

fn test_result_meta_merge_preserves_other_members() {
	for input in [
		'{"_meta":{"subscriptionId":"2"},"resultType":"complete","count":1e2}',
		'{"resultType":"complete", "_meta" : {"subscriptionId":"2"}, "count":1e2}',
		'{"resultType":"complete","count":1e2,"_meta":{"subscriptionId":"2"}}',
		'{"_meta":{"subscriptionId":"2"}}',
		'{}',
		'{"text":"_meta", "nested":{"_meta":{"ignored":true}}}',
	] {
		merged := with_result_meta_member(input, 'serverInfo', '{"name":"server","version":"1"}')
		object := json.decode[map[string]json.Any](merged)!
		meta := object['_meta'] or { panic('missing metadata') }
		info := meta.as_map()['serverInfo'] or { panic('missing server info') }
		assert (info.as_map()['name'] or { panic('missing server name') }).str() == 'server'
		if input.contains('subscriptionId') {
			assert (meta.as_map()['subscriptionId'] or { panic('missing subscription id') }).str() == '2'
		}
		if input.contains('resultType') {
			assert (object['resultType'] or { panic('missing result type') }).str() == 'complete'
			assert merged.contains('"count":1e2')
		}
	}
}

fn test_http_client_and_server_listen_round_trip() {
	mut server, url := spawn_stateless_server()!
	defer { server.close() }
	mut client := connect_2026(url, ClientConfig{})!
	defer { client.close() }
	assert client.initialize()!.protocol_version == protocol_version_2026_07_28
	filter := client.listen(SubscriptionListenParams{
		notifications: SubscriptionFilter{ tools_list_changed: true }
	})!
	assert filter.tools_list_changed
	notifications := client.take_notifications()
	assert notifications.len == 1
	assert subscription_id_of(notifications[0]) or { '' } == '2'
	assert client.request_message('tools/list', empty_object)!.error.code == 0
}

// MountedHostHandler is a host server that delegates `/mcp` to an MCP handler
// and answers everything else itself.
struct MountedHostHandler {
mut:
	mcp http.Handler
}

fn (mut h MountedHostHandler) handle(req http.Request) http.Response {
	if req.url.all_before('?') == '/mcp' {
		return h.mcp.handle(req)
	}
	mut response := http.Response{}
	response.set_status(.not_found)
	return response
}

fn test_http_handler_mounts_on_existing_server() {
	mut server := new_server(
		name:    'mounted-server'
		version: '0.0.1'
	)
	server.add_tool(Tool{
		name: 'ping_tool'
	}, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('pong')
	})!

	// The per-request entry point is directly callable by a host route.
	not_found := server.handle_http_request(http.Request{
		method: .post
		url:    '/not-mcp'
	})
	assert not_found.status_code == 404

	mut host := &http.Server{
		addr:                 '127.0.0.1:0'
		handler:              MountedHostHandler{
			mcp: server.http_handler()
		}
		accept_timeout:       100 * time.millisecond
		show_startup_message: false
	}
	host_thread := spawn host.listen_and_serve()
	host.wait_till_running(max_retries: 200, retry_period_ms: 10)!
	url := 'http://${host.addr}/mcp'

	session_id, mut header := http_initialize(url)!
	header.set_custom(mcp_session_id_header, session_id)!
	notification_response := http.fetch(
		method: .post
		url:    url
		data:   new_notification('notifications/initialized', empty).encode()
		header: header
	)!
	assert notification_response.status_code == 202

	list_response := http.fetch(
		method: .post
		url:    url
		data:   new_request(2, 'tools/list', empty).encode()
		header: header
	)!
	assert list_response.status_code == 200
	list_result := decode_response(list_response.body)!.decode_result[ListToolsResult]()!
	assert list_result.tools.len == 1
	assert list_result.tools[0].name == 'ping_tool'

	missing := http.fetch(
		method: .post
		url:    'http://${host.addr}/other'
		data:   '{}'
	)!
	assert missing.status_code == 404

	host.close()
	host_thread.wait()
}
