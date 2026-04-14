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

fn test_framed_messages_handle_partial_reads() {
	payload := new_notification('notifications/initialized', empty).encode()
	frame := encode_framed_message(payload)
	mut buffer := frame[..12]

	try_extract_framed_message(buffer) or { assert err.msg() == NoFrameError{}.msg() }

	buffer += frame[12..]
	extracted := try_extract_framed_message(buffer)!
	buffer = extracted.remaining
	message := extracted.message

	assert buffer == ''
	assert decode_notification(message)!.method == 'notifications/initialized'
}

fn test_close_delegates_to_the_transport() {
	mut transport := &MockTransport{}
	mut client := new_client(transport, ClientConfig{})

	client.close()

	assert transport.closed
}
