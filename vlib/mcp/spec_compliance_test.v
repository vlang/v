// Spec-compliance tests for MCP 2025-11-25 wire shapes.
// These tests intentionally check the JSON shape of responses and notifications
// produced by the server against the published schema:
//   https://github.com/modelcontextprotocol/modelcontextprotocol/blob/main/schema/2025-11-25/schema.json
// Add new cases here whenever a schema field is touched.
module mcp

import json2 as json

fn build_initialized_session(mut server Server) {
	server.dispatch_message(Request{
		id:     encode_id(1)
		method: 'initialize'
		params: encode_initialize_params(InitializeParams{
			protocol_version: protocol_version
			capabilities:     '{"roots":{"listChanged":true}}'
			client_info:      Implementation{
				name:    'compliance'
				version: '0'
			}
		})
	}.encode(), stdio_session_id, .stdio) or { panic(err) }
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio) or { panic(err) }
}

fn test_initialize_result_shape_matches_spec() {
	mut server := new_server(name: 's', version: '0', enable_logging: true)
	server.add_tool(Tool{ name: 't' }, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!
	dispatch := server.dispatch_message(Request{
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
	response := decode_response(dispatch.response)!
	assert response.error.code == 0
	result := response.decode_result[InitializeResult]()!
	assert result.protocol_version == protocol_version
	assert result.server_info.name == 's'
	assert result.server_info.version == '0'
	assert result.capabilities.contains('"tools":{"listChanged":true}')
	assert result.capabilities.contains('"logging":{}')
}

fn test_jsonrpc_envelope_includes_required_fields() {
	mut server := new_server(name: 's', version: '0')
	build_initialized_session(mut server)

	dispatch := server.dispatch_message(new_request(2, 'ping', empty).encode(), stdio_session_id,
		.stdio)!
	envelope := json.decode[MessageEnvelope](dispatch.response)!
	assert envelope.jsonrpc == '2.0'
	assert envelope.id == '2'
	assert envelope.result == '{}'
	assert envelope.method == ''
	assert envelope.error.code == 0
}

fn test_tools_list_response_uses_input_schema_field() {
	mut server := new_server(name: 's', version: '0')
	server.add_tool(Tool{
		name:         'test'
		description:  'd'
		input_schema: '{"type":"object","properties":{"x":{"type":"string"}}}'
	}, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!
	build_initialized_session(mut server)

	dispatch := server.dispatch_message(new_request(2, 'tools/list', empty).encode(),
		stdio_session_id, .stdio)!
	body := decode_response(dispatch.response)!.result
	assert body.contains('"inputSchema":{"type":"object","properties":{"x":{"type":"string"}}}')
	assert body.contains('"name":"test"')
}

fn test_tool_call_result_has_is_error_and_content_array() {
	mut server := new_server(name: 's', version: '0')
	server.add_tool(Tool{ name: 'fails' }, fn (_ Context, _ string) !ToolResult {
		return error('boom')
	})!
	build_initialized_session(mut server)

	dispatch := server.dispatch_message('{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"fails","arguments":{}}}',
		stdio_session_id, .stdio)!
	body := decode_response(dispatch.response)!.result
	assert body.contains('"isError":true')
	assert body.contains('"content":[')
	assert body.contains('"type":"text"')
}

fn test_resources_list_uses_camel_case_mime_type() {
	mut server := new_server(name: 's', version: '0')
	server.add_resource(Resource{
		uri:       'res://a'
		name:      'a'
		mime_type: 'text/plain'
	}, fn (_ Context, uri string) !ReadResourceResult {
		return ReadResourceResult{
			contents: [
				ResourceContents{
					uri:       uri
					mime_type: 'text/plain'
					text:      'hi'
				},
			]
		}
	})!
	build_initialized_session(mut server)

	list_dispatch := server.dispatch_message(new_request(2, 'resources/list', empty).encode(),
		stdio_session_id, .stdio)!
	list_body := decode_response(list_dispatch.response)!.result
	assert list_body.contains('"mimeType":"text/plain"')
	assert !list_body.contains('"mime_type"')

	read_dispatch := server.dispatch_message(new_request(3, 'resources/read', ReadResourceParams{
		uri: 'res://a'
	}).encode(), stdio_session_id, .stdio)!
	read_body := decode_response(read_dispatch.response)!.result
	assert read_body.contains('"mimeType":"text/plain"')
}

fn test_resource_templates_list_uses_camel_case_uri_template() {
	mut server := new_server(name: 's', version: '0')
	server.add_resource_template(ResourceTemplate{
		uri_template: 'res://a/{x}'
		name:         'a'
	})!
	build_initialized_session(mut server)

	dispatch := server.dispatch_message(new_request(2, 'resources/templates/list', empty).encode(),
		stdio_session_id, .stdio)!
	body := decode_response(dispatch.response)!.result
	assert body.contains('"resourceTemplates"')
	assert body.contains('"uriTemplate":"res://a/{x}"')
}

fn test_progress_notification_uses_camel_case_progress_token() {
	mut server := new_server(name: 's', version: '0')
	server.add_tool(Tool{ name: 'work' }, fn (ctx Context, _ string) !ToolResult {
		ctx.notify_progress(0.5, 1.0, 'half')
		return tool_text_result('done')
	})!
	build_initialized_session(mut server)

	server.dispatch_message('{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"work","arguments":{},"_meta":{"progressToken":"abc"}}}',
		stdio_session_id, .stdio)!
	queue := server.drain_session_notifications(stdio_session_id)
	assert queue.len == 1
	notif := decode_notification(queue[0])!
	assert notif.method == 'notifications/progress'
	assert notif.params.contains('"progressToken":"abc"')
}

fn test_logging_message_uses_rfc5424_levels() {
	levels := [
		LogLevel.debug,
		LogLevel.info,
		LogLevel.notice,
		LogLevel.warning,
		LogLevel.error,
		LogLevel.critical,
		LogLevel.alert,
		LogLevel.emergency,
	]
	expected := ['debug', 'info', 'notice', 'warning', 'error', 'critical', 'alert', 'emergency']
	for i, level in levels {
		assert level.str() == expected[i]
		parsed := parse_log_level(expected[i]) or { panic('failed to parse ${expected[i]}') }
		assert parsed == level
	}
}

fn test_error_codes_match_jsonrpc_specification() {
	assert parse_error.code == -32700
	assert invalid_request.code == -32600
	assert method_not_found.code == -32601
	assert invalid_params.code == -32602
	assert internal_error.code == -32603
	assert server_not_initialized.code == -32002
}

fn test_pagination_emits_next_cursor_when_more_pages_exist() {
	mut server := new_server(name: 's', version: '0')
	for i in 0 .. default_list_page_size + 5 {
		server.add_tool(Tool{ name: 'tool_${i}' }, fn (_ Context, _ string) !ToolResult {
			return tool_text_result('ok')
		})!
	}
	build_initialized_session(mut server)

	first_dispatch := server.dispatch_message(new_request(2, 'tools/list', empty).encode(),
		stdio_session_id, .stdio)!
	first := decode_response(first_dispatch.response)!.decode_result[ListToolsResult]()!
	assert first.tools.len == default_list_page_size
	assert first.next_cursor == default_list_page_size.str()

	second_dispatch := server.dispatch_message(Request{
		id:     encode_id(3)
		method: 'tools/list'
		params: '{"cursor":"${first.next_cursor}"}'
	}.encode(), stdio_session_id, .stdio)!
	second := decode_response(second_dispatch.response)!.decode_result[ListToolsResult]()!
	assert second.tools.len == 5
	assert second.next_cursor == ''
}

fn test_completion_result_matches_spec_shape() {
	mut server := new_server(name: 's', version: '0')
	server.add_completion(CompletionRef{ ref_type: 'ref/prompt', name: 'p' }, 'arg', fn (_ Context, _ string, _ string) !CompletionResult {
		return CompletionResult{
			values:   ['a', 'b']
			total:    2
			has_more: false
		}
	})!
	build_initialized_session(mut server)

	dispatch := server.dispatch_message('{"jsonrpc":"2.0","id":2,"method":"completion/complete","params":{"ref":{"type":"ref/prompt","name":"p"},"argument":{"name":"arg","value":""}}}',
		stdio_session_id, .stdio)!
	body := decode_response(dispatch.response)!.result
	// The result MUST be wrapped in a `completion` object per spec.
	assert body.starts_with('{"completion":{')
	assert body.contains('"values":["a","b"]')
	assert body.contains('"total":2')
	assert body.contains('"hasMore":false')
}

fn test_initialize_blocks_other_methods_until_notifications_initialized_received() {
	mut server := new_server(name: 's', version: '0')
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
	dispatch := server.dispatch_message(new_request(2, 'tools/list', empty).encode(),
		stdio_session_id, .stdio)!
	response := decode_response(dispatch.response)!
	assert response.error.code == server_not_initialized.code
}

fn test_implementation_carries_2025_11_25_metadata_extensions() {
	icon := Icon{
		src:       'https://example.com/icon.svg'
		mime_type: 'image/svg+xml'
		sizes:     ['48x48', '96x96']
		theme:     'light'
	}
	mut server := new_server(
		name:        's'
		version:     '0'
		title:       'Server Title'
		description: 'Showcase'
		website_url: 'https://example.com'
		icons:       [icon]
	)
	build_initialized_session(mut server)

	dispatch := server.dispatch_message(new_request(2, 'ping', empty).encode(), stdio_session_id,
		.stdio)!
	// The server's Implementation is folded into the cached InitializeResult; round-trip
	// it through json.encode to verify the wire shape.
	encoded := json.encode(server.server_info)
	assert encoded.contains('"name":"s"')
	assert encoded.contains('"version":"0"')
	assert encoded.contains('"title":"Server Title"')
	assert encoded.contains('"description":"Showcase"')
	assert encoded.contains('"websiteUrl":"https://example.com"')
	assert encoded.contains('"icons":[')
	assert encoded.contains('"src":"https://example.com/icon.svg"')
	assert encoded.contains('"mimeType":"image/svg+xml"')
	assert encoded.contains('"sizes":["48x48","96x96"]')
	assert encoded.contains('"theme":"light"')
	assert dispatch.response.contains('"jsonrpc":"2.0"')
}

fn test_implementation_omits_optional_metadata_when_empty() {
	encoded := json.encode(Implementation{ name: 'c', version: '0' })
	assert encoded == '{"name":"c","version":"0"}'
}

fn test_tool_emits_icons_and_execution_task_support() {
	mut server := new_server(name: 's', version: '0')
	server.add_tool(Tool{
		name:      'launch'
		icons:     [
			Icon{
				src:       'https://example.com/launch.png'
				mime_type: 'image/png'
			},
		]
		execution: ToolExecution{
			task_support: 'optional'
		}
	}, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!
	build_initialized_session(mut server)

	dispatch := server.dispatch_message(new_request(2, 'tools/list', empty).encode(),
		stdio_session_id, .stdio)!
	body := decode_response(dispatch.response)!.result
	assert body.contains('"icons":[{"src":"https://example.com/launch.png","mimeType":"image/png"}]')
	assert body.contains('"execution":{"taskSupport":"optional"}')
}

fn test_tool_omits_icons_and_execution_when_unset() {
	mut server := new_server(name: 's', version: '0')
	server.add_tool(Tool{ name: 'plain' }, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!
	build_initialized_session(mut server)

	dispatch := server.dispatch_message(new_request(2, 'tools/list', empty).encode(),
		stdio_session_id, .stdio)!
	body := decode_response(dispatch.response)!.result
	assert !body.contains('"icons"')
	assert !body.contains('"execution"')
}

fn test_resource_emits_icons_size_and_annotations() {
	mut server := new_server(name: 's', version: '0')
	server.add_resource(Resource{
		uri:         'res://big'
		name:        'big'
		size:        4096
		icons:       [Icon{
			src: 'https://example.com/file.svg'
		}]
		annotations: Annotations{
			audience:      ['user']
			priority:      0.8
			last_modified: '2026-04-29T00:00:00Z'
		}
	}, fn (_ Context, uri string) !ReadResourceResult {
		return ReadResourceResult{
			contents: [ResourceContents{
				uri:  uri
				text: ''
			}]
		}
	})!
	build_initialized_session(mut server)

	dispatch := server.dispatch_message(new_request(2, 'resources/list', empty).encode(),
		stdio_session_id, .stdio)!
	body := decode_response(dispatch.response)!.result
	assert body.contains('"size":4096')
	assert body.contains('"icons":[{"src":"https://example.com/file.svg"}]')
	assert body.contains('"audience":["user"]')
	assert body.contains('"priority":0.8')
	assert body.contains('"lastModified":"2026-04-29T00:00:00Z"')
}

fn test_resource_template_emits_icons_and_annotations() {
	mut server := new_server(name: 's', version: '0')
	server.add_resource_template(ResourceTemplate{
		uri_template: 'res://greet/{name}'
		name:         'greet'
		icons:        [Icon{
			src: 'https://example.com/wave.svg'
		}]
		annotations:  Annotations{
			audience: ['assistant']
		}
	})!
	build_initialized_session(mut server)

	dispatch := server.dispatch_message(new_request(2, 'resources/templates/list', empty).encode(),
		stdio_session_id, .stdio)!
	body := decode_response(dispatch.response)!.result
	assert body.contains('"icons":[{"src":"https://example.com/wave.svg"}]')
	assert body.contains('"audience":["assistant"]')
}

fn test_prompt_emits_icons() {
	mut server := new_server(name: 's', version: '0')
	server.add_prompt(Prompt{
		name:  'review'
		icons: [Icon{
			src: 'https://example.com/review.svg'
		}]
	}, fn (_ Context, _ string) !GetPromptResult {
		return GetPromptResult{
			messages: [prompt_text_message('user', 'hi')]
		}
	})!
	build_initialized_session(mut server)

	dispatch := server.dispatch_message(new_request(2, 'prompts/list', empty).encode(),
		stdio_session_id, .stdio)!
	body := decode_response(dispatch.response)!.result
	assert body.contains('"icons":[{"src":"https://example.com/review.svg"}]')
}

fn test_content_helpers_match_spec_shapes() {
	assert text_content('hi') == '{"type":"text","text":"hi"}'
	assert image_content('AAA=', 'image/png') == '{"type":"image","data":"AAA=","mimeType":"image/png"}'
	assert audio_content('BBB=', 'audio/wav') == '{"type":"audio","data":"BBB=","mimeType":"audio/wav"}'
	assert embedded_text_resource('res://a', 'text/plain', 'hi') == '{"type":"resource","resource":{"uri":"res://a","mimeType":"text/plain","text":"hi"}}'
	assert embedded_blob_resource('res://b', 'image/png', 'AAA=') == '{"type":"resource","resource":{"uri":"res://b","mimeType":"image/png","blob":"AAA="}}'
	link := resource_link_content(Resource{ uri: 'res://l', name: 'l' })
	assert link.starts_with('{"type":"resource_link",')
	assert link.contains('"uri":"res://l"')
	assert link.contains('"name":"l"')
}

fn test_content_helpers_attach_annotations() {
	annot := Annotations{
		audience:      ['user', 'assistant']
		priority:      0.7
		last_modified: '2026-04-29T00:00:00Z'
	}
	text := text_content_with_annotations('hi', annot)
	assert text.contains('"type":"text"')
	assert text.contains('"text":"hi"')
	assert text.contains('"annotations":{')
	assert text.contains('"audience":["user","assistant"]')
	assert text.contains('"priority":0.7')
	assert text.contains('"lastModified":"2026-04-29T00:00:00Z"')

	image := image_content_with_annotations('AAA=', 'image/png', annot)
	assert image.contains('"type":"image"')
	assert image.contains('"data":"AAA="')
	assert image.contains('"mimeType":"image/png"')
	assert image.contains('"annotations":{')

	audio := audio_content_with_annotations('BBB=', 'audio/wav', annot)
	assert audio.contains('"type":"audio"')
	assert audio.contains('"annotations":{')

	embedded := embedded_text_resource_with_annotations('res://x', 'text/plain', 'hi', annot)
	assert embedded.starts_with('{"type":"resource"')
	assert embedded.contains('"resource":{"uri":"res://x"')
	assert embedded.contains('"annotations":{')
}

fn test_progress_notifications_must_strictly_increase() {
	mut server := new_server(name: 's', version: '0')
	server.add_tool(Tool{ name: 'work' }, fn (ctx Context, _ string) !ToolResult {
		ctx.notify_progress(0.3, 1.0, 'a')
		ctx.notify_progress(0.3, 1.0, 'b') // should be dropped (equal)
		ctx.notify_progress(0.1, 1.0, 'c') // should be dropped (lower)
		ctx.notify_progress(0.6, 1.0, 'd') // accepted
		return tool_text_result('done')
	})!
	build_initialized_session(mut server)

	server.dispatch_message('{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"work","arguments":{},"_meta":{"progressToken":"p1"}}}',
		stdio_session_id, .stdio)!
	queue := server.drain_session_notifications(stdio_session_id)
	progress_notifications := queue.filter(it.contains('notifications/progress'))
	assert progress_notifications.len == 2
	assert progress_notifications[0].contains('"progress":0.3')
	assert progress_notifications[1].contains('"progress":0.6')
}

fn test_completion_clamps_values_to_one_hundred() {
	mut server := new_server(name: 's', version: '0')
	server.add_completion(CompletionRef{ ref_type: 'ref/prompt', name: 'p' }, 'arg', fn (_ Context, _ string, _ string) !CompletionResult {
		mut values := []string{cap: 150}
		for i in 0 .. 150 {
			values << 'item_${i}'
		}
		return CompletionResult{
			values: values
		}
	})!
	build_initialized_session(mut server)

	dispatch := server.dispatch_message('{"jsonrpc":"2.0","id":2,"method":"completion/complete","params":{"ref":{"type":"ref/prompt","name":"p"},"argument":{"name":"arg","value":""}}}',
		stdio_session_id, .stdio)!
	body := decode_response(dispatch.response)!.result
	assert body.contains('"hasMore":true')
	// 100 commas inside the values array (one between every consecutive pair).
	values_section := body.all_after('"values":[').all_before(']')
	assert values_section.split(',').len == 100
	assert values_section.contains('"item_99"')
	assert !values_section.contains('"item_100"')
}

fn test_url_elicitation_emits_mode_url_and_elicitation_id() {
	encoded := encode_elicit_params(ElicitParams{
		mode:           'url'
		message:        'Connect your GitHub account'
		url:            'https://example.com/connect/abc'
		elicitation_id: '550e8400-e29b-41d4-a716-446655440000'
	})
	assert encoded.contains('"mode":"url"')
	assert encoded.contains('"message":"Connect your GitHub account"')
	assert encoded.contains('"url":"https://example.com/connect/abc"')
	assert encoded.contains('"elicitationId":"550e8400-e29b-41d4-a716-446655440000"')
	assert !encoded.contains('"requestedSchema"')
}

fn test_form_elicitation_omits_url_fields() {
	encoded := encode_elicit_params(ElicitParams{
		message:          'Pick a color'
		requested_schema: ElicitSchema{
			properties: '{"color":{"type":"string"}}'
			required:   ['color']
		}
	})
	assert encoded.contains('"message":"Pick a color"')
	assert encoded.contains('"requestedSchema":{"type":"object","properties":{"color":{"type":"string"}},"required":["color"]}')
	assert !encoded.contains('"mode"')
	assert !encoded.contains('"url"')
	assert !encoded.contains('"elicitationId"')
}

fn test_resources_read_returns_resource_not_found_with_uri() {
	mut server := new_server(name: 's', version: '0')
	build_initialized_session(mut server)

	dispatch := server.dispatch_message(new_request(2, 'resources/read', ReadResourceParams{
		uri: 'res://missing'
	}).encode(), stdio_session_id, .stdio)!
	assert dispatch.response.contains('"code":-32002')
	assert dispatch.response.contains('"data":{"uri":"res://missing"}')
}

fn test_elicitation_completion_notification_has_id() {
	mut server := new_server(name: 's', version: '0')
	build_initialized_session(mut server)

	server.notify_elicitation_complete(stdio_session_id, '550e8400-e29b-41d4-a716-446655440000')
	queue := server.drain_session_notifications(stdio_session_id)
	assert queue.len == 1
	notif := decode_notification(queue[0])!
	assert notif.method == 'notifications/elicitation/complete'
	assert notif.params.contains('"elicitationId":"550e8400-e29b-41d4-a716-446655440000"')
}

fn test_url_elicitation_required_error_code_matches_spec() {
	assert url_elicitation_required.code == -32042
}

fn test_sampling_create_message_carries_tools_and_tool_choice() {
	encoded := json.encode(CreateMessageParams{
		messages:        [
			SamplingMessage{
				role:    'user'
				content: text_content('hello')
			},
		]
		max_tokens:      100
		tools:           [
			Tool{
				name:         'lookup'
				input_schema: '{"type":"object","properties":{"q":{"type":"string"}}}'
			},
		]
		tool_choice:     ToolChoice{
			mode: 'auto'
		}
		include_context: 'thisServer'
	})
	assert encoded.contains('"tools":[')
	assert encoded.contains('"name":"lookup"')
	assert encoded.contains('"toolChoice":{"mode":"auto"}')
	assert encoded.contains('"includeContext":"thisServer"')
}
