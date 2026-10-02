module mcp

struct MockTransport {
mut:
	incoming []string
	sent     []string
	closed   bool
}

fn (mut transport MockTransport) send(message string) ! {
	transport.sent << message
}

fn (mut transport MockTransport) receive() !string {
	if transport.incoming.len == 0 {
		return error('no messages queued in MockTransport')
	}
	message := transport.incoming[0]
	transport.incoming = if transport.incoming.len == 1 {
		[]string{}
	} else {
		transport.incoming[1..].clone()
	}
	return message
}

fn (mut transport MockTransport) close() {
	transport.closed = true
}

fn test_initialize_sends_the_mcp_handshake() {
	mut transport := &MockTransport{
		incoming: [
			new_response(1, InitializeResult{
				protocol_version: protocol_version
				capabilities:     '{"tools":{}}'
				server_info:      Implementation{
					name:    'mock-server'
					version: '1.0.0'
				}
			}, ResponseError{}).encode(),
		]
	}
	mut client := new_client(transport, ClientConfig{
		client_info:  Implementation{
			name:    'mcp-test-client'
			version: '0.1.0'
		}
		capabilities: '{"roots":{"listChanged":true}}'
	})

	result := client.initialize()!

	assert result.server_info.name == 'mock-server'
	assert transport.sent.len == 2

	request := decode_request(transport.sent[0])!
	params := request.decode_params[InitializeParams]()!
	assert request.method == 'initialize'
	assert params.protocol_version == protocol_version
	assert params.client_info.name == 'mcp-test-client'
	assert params.capabilities == '{"roots":{"listChanged":true}}'

	notification := decode_notification(transport.sent[1])!
	assert notification.method == 'notifications/initialized'
	assert notification.params == ''
}

fn test_request_buffers_server_messages_after_initialize() {
	mut transport := &MockTransport{
		incoming: [
			new_response(1, InitializeResult{
				protocol_version: protocol_version
				capabilities:     '{"tools":{}}'
				server_info:      Implementation{
					name:    'mock-server'
					version: '1.0.0'
				}
			}, ResponseError{}).encode(),
			new_notification('notifications/tools/list_changed', empty).encode(),
			new_request('server-1', 'roots/list', empty).encode(),
			new_response(2, true, ResponseError{}).encode(),
		]
	}
	mut client := new_client(transport, ClientConfig{})
	client.initialize()!

	response := client.request_message('ping', empty)!

	assert response.result == 'true'
	assert transport.sent.len == 3
	assert decode_request(transport.sent[2])!.method == 'ping'

	notifications := client.take_notifications()
	assert notifications.len == 1
	assert notifications[0].method == 'notifications/tools/list_changed'

	requests := client.take_requests()
	assert requests.len == 1
	assert requests[0].method == 'roots/list'
	assert requests[0].id == '"server-1"'
}

fn test_parse_sse_messages_reads_json_rpc_events() {
	body := 'event: message\r\n' +
		'data: {"jsonrpc":"2.0","method":"notifications/progress","params":{"progress":0.5}}\r\n' +
		'\r\n' + 'event: message\r\n' + 'data: {"jsonrpc":"2.0","id":1,"result":true}\r\n' + '\r\n'

	messages := parse_sse_messages(body)!

	assert messages.len == 2
	assert decode_notification(messages[0])!.method == 'notifications/progress'
	assert decode_response(messages[1])!.result == 'true'
}

fn test_stdio_messages_handle_partial_reads() {
	payload := new_notification('notifications/initialized', empty).encode()
	frame := encode_stdio_message(payload)
	assert frame.ends_with('\n')
	mut buffer := frame[..frame.len - 1]

	try_extract_stdio_message(buffer) or { assert err.msg() == NoFrameError{}.msg() }

	buffer += frame[frame.len - 1..]
	extracted := try_extract_stdio_message(buffer)!
	buffer = extracted.remaining
	message := extracted.message

	assert buffer == ''
	assert decode_notification(message)!.method == 'notifications/initialized'
}

fn test_stdio_messages_strip_embedded_newlines() {
	payload := '{"jsonrpc":"2.0",\n"method":"ping"}'
	frame := encode_stdio_message(payload)
	assert frame == '{"jsonrpc":"2.0","method":"ping"}\n'
}

fn test_stdio_messages_skip_blank_lines() {
	first := encode_stdio_message(new_notification('a', empty).encode())
	second := encode_stdio_message(new_notification('b', empty).encode())
	buffer := first + '\n\n' + second

	frame_a := try_extract_stdio_message(buffer)!
	assert decode_notification(frame_a.message)!.method == 'a'
	frame_b := try_extract_stdio_message(frame_a.remaining)!
	assert decode_notification(frame_b.message)!.method == 'b'
}

fn test_close_delegates_to_the_transport() {
	mut transport := &MockTransport{}
	mut client := new_client(transport, ClientConfig{})

	client.close()

	assert transport.closed
}

fn discover_response_json() string {
	return new_response(1, DiscoverResult{
		supported_versions: [protocol_version_2025_11_25, protocol_version_2026_07_28]
		capabilities:       '{"tools":{}}'
	}, ResponseError{}).encode()
}

fn test_client_2026_skips_the_handshake_and_injects_meta() {
	mut transport := &MockTransport{
		incoming: [
			discover_response_json(),
			new_response(2, map[string]string{
				'ok': 'yes'
			}, ResponseError{}).encode(),
		]
	}
	mut client := new_client(transport, ClientConfig{
		stateless_2026:   true
		protocol_version: protocol_version_2026_07_28
		log_level:        'debug'
		client_info:      Implementation{
			name:    'c'
			version: '1'
		}
	})

	result := client.initialize()!
	assert result.protocol_version == protocol_version_2026_07_28
	// The only message on the wire is `server/discover`: no initialize, and no
	// `notifications/initialized`.
	assert transport.sent.len == 1
	discover := decode_request(transport.sent[0])!
	assert discover.method == 'server/discover'
	assert discover.params.contains('io.modelcontextprotocol/protocolVersion')
	assert discover.params.contains('io.modelcontextprotocol/clientInfo')
	assert discover.params.contains('io.modelcontextprotocol/logLevel')

	reply := client.request_message('tools/list', empty)!
	assert reply.error.code == 0
	list := decode_request(transport.sent[1])!
	assert list.method == 'tools/list'
	// Every request re-declares the revision, as the spec requires.
	assert list.params.contains('io.modelcontextprotocol/protocolVersion')
}

fn test_client_2026_answers_input_required_and_retries() {
	for result_json in [
		'{"resultType":"input_required","inputRequests":{"ask":{"method":"elicitation/create","params":{"message":"May I?"}}},"requestState":"rs-1"}',
		'{"inputRequests":{"ask":{"method":"elicitation/create","params":{"message":"May I?"}}},"requestState":"rs-1","resultType":"input_required"}',
		' {\n  "inputRequests": {"ask": {"method": "elicitation/create", "params": {"message": "May I?"}}},\n  "requestState": "rs-1",\n  "resultType": "input_required"\n } ',
	] {
		assert_client_2026_input_retry(result_json)!
	}
}

fn assert_client_2026_input_retry(result_json string) ! {
	input_required := Response{
		id:     '1'
		result: result_json
	}.encode()
	complete := Response{
		id:     '1'
		result: '{"content":"done","isError":false,"resultType":"complete"}'
	}.encode()
	mut transport := &MockTransport{
		incoming: [
			// `request_message` allocates the caller's id first, so the lazy
			// discover probe is the one that answers with id 2.
			Response{
				id:     '2'
				result: '{"supportedVersions":["2026-07-28"],"capabilities":"{}"}'
			}.encode(),
			input_required,
			complete,
		]
	}
	mut client := new_client(transport, ClientConfig{
		stateless_2026:      true
		protocol_version:    protocol_version_2026_07_28
		elicitation_handler: fn (_ string) string {
			return '{"action":"accept","content":{}}'
		}
	})

	reply := client.request_message('tools/call', ToolCallParams{
		name:      'confirm'
		arguments: '{}'
	})!
	assert reply.error.code == 0
	assert reply.result.contains('"resultType":"complete"')
	// discover, the first attempt, then the automatic retry.
	assert transport.sent.len == 3
	retry := decode_request(transport.sent[2])!
	assert retry.method == 'tools/call'
	assert retry.params.contains('"inputResponses"')
	assert retry.params.contains('"ask"')
	assert retry.params.contains('"action":"accept"')
	// The opaque state the server sent is echoed back.
	assert retry.params.contains('"requestState":"rs-1"')
	// The original request fields survive the retry.
	assert retry.params.contains('"name":"confirm"')
}

fn test_result_type_defaults_to_complete_without_top_level_member() {
	for result_json in ['{}', '{"content":"done"}', '{"content":{"resultType":"input_required"}}'] {
		assert result_type_of(result_json) == result_type_complete
	}
}

fn test_client_2026_downgrades_when_the_version_is_unsupported() {
	unsupported := Response{
		id:    '1'
		error: ResponseError{
			code:    unsupported_protocol_version.code
			message: unsupported_protocol_version.message
			data:    '{"supported":["2025-11-25"],"requested":"2026-07-28"}'
		}
	}.encode()
	mut transport := &MockTransport{
		incoming: [
			unsupported,
			new_response(2, InitializeResult{
				protocol_version: protocol_version
				capabilities:     '{}'
				server_info:      Implementation{
					name:    'legacy-server'
					version: '1'
				}
			}, ResponseError{}).encode(),
			new_response(3, map[string]string{
				'ok': 'yes'
			}, ResponseError{}).encode(),
		]
	}
	mut client := new_client(transport, ClientConfig{
		stateless_2026:   true
		protocol_version: protocol_version_2026_07_28
	})

	result := client.initialize()!
	// The client fell back to the revision the server named and ran the
	// 2025-11-25 handshake instead.
	assert result.protocol_version == protocol_version
	assert result.server_info.name == 'legacy-server'
	discover := decode_request(transport.sent[0])!
	assert discover.method == 'server/discover'
	handshake := decode_request(transport.sent[1])!
	assert handshake.method == 'initialize'
	assert handshake.decode_params[InitializeParams]()!.protocol_version == protocol_version
}

fn test_client_listen_returns_the_acknowledged_subset() {
	acknowledged := build_notification_message('notifications/subscriptions/acknowledged',
		'{"notifications":{"toolsListChanged":true},"_meta":{"io.modelcontextprotocol/subscriptionId":"7"}}')
	mut transport := &MockTransport{
		incoming: [
			discover_response_json(),
			acknowledged,
			Response{
				id:     '2'
				result: '{"_meta":{"io.modelcontextprotocol/subscriptionId":"7"}}'
			}.encode(),
		]
	}
	mut client := new_client(transport, ClientConfig{
		stateless_2026:   true
		protocol_version: protocol_version_2026_07_28
	})

	filter := client.listen(SubscriptionListenParams{
		notifications: SubscriptionFilter{
			tools_list_changed:     true
			prompts_list_changed:   true
			resources_list_changed: true
		}
	})!
	// The server acknowledged only what it honors.
	assert filter.tools_list_changed
	assert !filter.prompts_list_changed
	assert !filter.resources_list_changed

	// The notification is delivered, tagged with the subscription id.
	notifications := client.take_notifications()
	assert notifications.len == 1
	assert subscription_id_of(notifications[0]) or { '' } == '7'
}

fn test_client_listen_requires_the_2026_protocol() {
	mut transport := &MockTransport{}
	mut client := new_client(transport, ClientConfig{})
	client.listen(SubscriptionFilter{}) or {
		assert err.msg().contains('2026-07-28')
		return
	}
	assert false
}
