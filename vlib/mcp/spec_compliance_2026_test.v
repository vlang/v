// Spec-compliance tests for MCP 2026-07-28 wire shapes.
// These tests intentionally check the JSON shape of results, errors and
// notifications produced by the server against the published schema:
//   https://github.com/modelcontextprotocol/modelcontextprotocol/blob/main/schema/2026-07-28/schema.json
// 2026-07-28 is sessionless: there is no `initialize` handshake, so a request
// declares its revision in `params._meta` and carries its own client state.
// Add new cases here whenever a schema field is touched.
module mcp

import net.http
import time

// meta_params builds a 2026-07-28 `_meta` object. The schema requires
// `io.modelcontextprotocol/protocolVersion` and
// `io.modelcontextprotocol/clientCapabilities` on every request.
fn meta_params() string {
	return '{"io.modelcontextprotocol/protocolVersion":"${protocol_version_2026_07_28}",' +
		'"io.modelcontextprotocol/clientCapabilities":{}}'
}

// request_2026 builds a 2026-07-28 request whose params carry the reserved
// `_meta` alongside the method's own fields.
fn request_2026(id int, method string, fields string) string {
	mut members := []string{}
	if fields != '' {
		members << fields
	}
	members << '"_meta":${meta_params()}'
	return Request{
		id:     encode_id(id)
		method: method
		params: '{${members.join(',')}}'
	}.encode()
}

// post_2026_header builds the request headers a 2026-07-28 Streamable HTTP POST
// must carry.
fn post_2026_header(method string) http.Header {
	mut header := http.new_header(http.HeaderConfig{
		key:   .content_type
		value: 'application/json'
	}, http.HeaderConfig{
		key:   .accept
		value: 'application/json, text/event-stream'
	})
	header.set_custom(mcp_protocol_version_header, protocol_version_2026_07_28) or {}
	header.set_custom(mcp_method_header, method) or {}
	return header
}

fn dispatch_2026(mut server Server, id int, method string, fields string) Response {
	dispatch := server.dispatch_message(request_2026(id, method, fields), stdio_session_id,
		.stdio) or { panic(err) }
	return decode_response(dispatch.response) or { panic(err) }
}

fn result_2026(mut server Server, id int, method string, fields string) string {
	return dispatch_2026(mut server, id, method, fields).result
}

// build_2026_server returns a server offering one of every cacheable kind.
fn build_2026_server() !Server {
	mut server := new_server(name: 's', version: '0', enable_logging: true)
	server.add_tool(Tool{
		name:         'test'
		description:  'd'
		input_schema: '{"type":"object"}'
	}, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!
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
	server.add_resource_template(ResourceTemplate{
		uri_template: 'res://t/{slug}'
		name:         't'
	})!
	server.add_prompt(Prompt{ name: 'p' }, fn (_ Context, _ string) !GetPromptResult {
		return GetPromptResult{
			messages: [
				prompt_text_message('user', 'hi'),
			]
		}
	})!
	return server
}

fn test_2026_tools_list_result_is_complete_and_cacheable() {
	mut server := build_2026_server()!

	response := dispatch_2026(mut server, 1, 'tools/list', '')
	assert response.error.code == 0
	result := response.result
	// `ListToolsResult` requires tools, resultType, ttlMs and cacheScope.
	assert result.contains('"tools":[')
	assert result.contains('"resultType":"complete"')
	assert result.contains('"ttlMs":300000')
	assert result.contains('"cacheScope":"private"')
	// `ResultMetaObject` carries the server identity on every response.
	assert result.contains('"${meta_server_info_key}":{"name":"s","version":"0"}')
	// Exactly one `_meta` object, even though the result already had members.
	assert result.count('"_meta"') == 1
}

fn test_2026_cacheable_results_all_carry_ttl_and_scope() {
	mut server := build_2026_server()!

	tools := result_2026(mut server, 1, 'tools/list', '')
	prompts := result_2026(mut server, 2, 'prompts/list', '')
	resources := result_2026(mut server, 3, 'resources/list', '')
	templates := result_2026(mut server, 4, 'resources/templates/list', '')
	read_result := result_2026(mut server, 5, 'resources/read', '"uri":"res://a"')

	for result in [tools, prompts, resources, templates, read_result] {
		assert result.contains('"resultType":"complete"')
		assert result.contains('"ttlMs":300000')
		assert result.contains('"cacheScope":"private"')
	}
	// The payloads themselves keep their spec field names.
	assert prompts.contains('"prompts":[')
	assert resources.contains('"resources":[')
	assert templates.contains('"resourceTemplates":[')
	assert read_result.contains('"contents":[')
}

fn test_2026_non_cacheable_result_omits_cache_hints() {
	mut server := build_2026_server()!

	// `CallToolResult` requires content and resultType, but no cache hints.
	result := result_2026(mut server, 1, 'tools/call', '"name":"test","arguments":{}')
	assert result.contains('"resultType":"complete"')
	assert result.contains('"content":[')
	assert !result.contains('"ttlMs"')
	assert !result.contains('"cacheScope"')
}

fn test_2026_discover_result_shape() {
	mut server := build_2026_server()!

	response := dispatch_2026(mut server, 1, 'server/discover', '')
	assert response.error.code == 0
	result := response.result
	assert result.contains('"supportedVersions":["2025-11-25","2026-07-28"]')
	assert result.contains('"capabilities":{')
	assert result.contains('"resultType":"complete"')
	assert result.contains('"ttlMs":300000')
	assert result.contains('"cacheScope":"private"')
	// The key spelling is part of the contract, not just its contents.
	assert result.contains('"supportedVersions"')
	// With no handshake, the result `_meta` is the only place a client can
	// learn which server answered.
	assert result.contains('"_meta"')
	assert result.contains('"${meta_server_info_key}"')
	assert result.contains('"name":"s"')
	assert result.contains('"version":"0"')
}

fn test_2026_unsupported_version_error_shape() {
	mut server := build_2026_server()!

	// A request that declares a revision the server does not speak is
	// rejected with UnsupportedProtocolVersionError.
	request := Request{
		id:     encode_id(1)
		method: 'tools/list'
		params: '{"_meta":{"io.modelcontextprotocol/protocolVersion":"1999-01-01"}}'
	}.encode()
	dispatch := server.dispatch_message(request, stdio_session_id, .stdio)!
	response := decode_response(dispatch.response)!
	assert response.error.code == unsupported_protocol_version.code
	assert response.error.code == -32022
	// `data` requires both `supported` and `requested`.
	assert response.error.data.contains('"supported":[')
	assert response.error.data.contains('"2025-11-25"')
	assert response.error.data.contains('"2026-07-28"')
	assert response.error.data.contains('"requested":"1999-01-01"')
}

fn test_2026_header_mismatch_error_shape() {
	mut server_value := new_server(name: 's', version: '0')
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
	// The header names one revision while `_meta` names another.
	header.set_custom(mcp_protocol_version_header, protocol_version_2025_11_25)!
	header.set_custom(mcp_method_header, 'tools/list')!
	response := http.fetch(
		method: .post
		url:    url
		data:   request_2026(1, 'tools/list', '')
		header: header
	)!
	assert response.status_code == 400
	body := decode_response(response.body)!
	assert body.error.code == header_mismatch.code
	assert body.error.code == -32020
	assert body.jsonrpc == '2.0'

	server.close()
	server_thread.wait() or {}
}

fn test_2026_missing_client_capabilities_is_invalid_request() {
	mut server_value := new_server(name: 's', version: '0')
	mut server := &server_value
	server_thread := spawn server.serve_http('127.0.0.1:0')
	server.wait_till_running(max_retries: 200, retry_period_ms: 10)!
	time.sleep(20 * time.millisecond)
	url := 'http://${server.http_server.addr}/mcp'

	// `RequestMetaObject` requires the protocol version AND the client
	// capabilities; a request naming only the version is malformed.
	body := Request{
		id:     encode_id(1)
		method: 'tools/list'
		params: '{"_meta":{"io.modelcontextprotocol/protocolVersion":"2026-07-28"}}'
	}.encode()
	response := http.fetch(
		method: .post
		url:    url
		data:   body
		header: post_2026_header('tools/list')
	)!
	assert response.status_code == 400
	// A malformed request, not a header/body contradiction.
	assert decode_response(response.body)!.error.code == -32600

	server.close()
	server_thread.wait() or {}
}

fn test_2026_listen_acknowledgement_is_tagged_with_subscription_id() {
	mut server := new_server(name: 's', version: '0')
	server.add_tool(Tool{ name: 'test' }, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!

	request := Request{
		id:     encode_id(7)
		method: 'subscriptions/listen'
		params: '{"notifications":{"toolsListChanged":true,"promptsListChanged":true},"_meta":${meta_params()}}'
	}.encode()
	dispatch := server.dispatch_message(request, stdio_session_id, .stdio) or { panic(err) }
	// The stream stays open, so the listen request itself is not answered.
	assert !dispatch.has_response

	out := server.drain_session_notifications(stdio_session_id)
	assert out.len == 1
	ack := decode_notification(out[0]) or { panic(err) }
	assert ack.method == 'notifications/subscriptions/acknowledged'
	// `notifications` is required and names the honored subset. This server has
	// a tool but no prompts, so the prompts kind is omitted from the ack even
	// though the client asked for it.
	assert ack.params.contains('"notifications":{')
	assert ack.params.contains('"toolsListChanged":true')
	assert !ack.params.contains('"promptsListChanged"')
	// `_meta` binds the stream to the listen request id.
	assert ack.params.contains('"${meta_subscription_id_key}":7')
	assert subscription_id_of(ack) or { '' } == '7'

	// A fanned-out change rides the same stream with the same subscription id.
	server.add_tool(Tool{ name: 'other' }, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!
	events := server.drain_session_notifications(stdio_session_id)
	assert events.len == 1
	changed := decode_notification(events[0]) or { panic(err) }
	assert changed.method == 'notifications/tools/list_changed'
	assert changed.params.contains('"${meta_subscription_id_key}":7')
}

fn test_2026_listen_result_shape() {
	// `SubscriptionsListenResult` requires `_meta` carrying the subscription id.
	body := encode_listen_result('7')
	// The schema requires both members on a SubscriptionsListenResult.
	assert body.contains('"resultType":"complete"')
	assert body.contains('"_meta"')
	assert body.contains('"${meta_subscription_id_key}":7')
}

fn test_2026_input_required_result_shape() {
	mut server := new_server(name: 's', version: '0')
	server.add_tool(Tool{ name: 'confirm' }, fn (ctx Context, _ string) !ToolResult {
		answer := ctx.take_elicit_result('ok') or {
			return ctx.require_elicit('ok', ElicitParams{
				message: 'May I?'
			})
		}
		return tool_text_result('said ${answer.action}')
	})!

	ask := result_2026(mut server, 1, 'tools/call', '"name":"confirm","arguments":{}')
	// The ask is a successful result, not an error.
	assert ask.contains('"resultType":"input_required"')
	// Exactly one resultType: it is not also stamped "complete".
	assert ask.count('"resultType"') == 1
	// `inputRequests.<key>` holds a full request object.
	assert ask.contains('"inputRequests":{')
	assert ask.contains('"ok":{')
	assert ask.contains('"method":"elicitation/create"')
	assert ask.contains('"params":{')
	assert ask.contains('"${meta_server_info_key}"')

	retry_params := '"name":"confirm","arguments":{},"inputResponses":{"ok":{"action":"accept",' +
		'"content":{}}},"requestState":"rs-1"'
	done := result_2026(mut server, 2, 'tools/call', retry_params)
	assert done.contains('"resultType":"complete"')
	assert done.count('"resultType"') == 1
	assert done.contains('said accept')
}

fn test_2025_tools_list_result_has_no_2026_fields() {
	mut server := new_server(name: 's', version: '0')
	server.add_tool(Tool{ name: 'test' }, fn (_ Context, _ string) !ToolResult {
		return tool_text_result('ok')
	})!
	// The 2025-11-25 handshake still owns the session.
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
	}.encode(), stdio_session_id, .stdio) or { panic(err) }
	server.dispatch_message(new_notification('notifications/initialized', empty).encode(),
		stdio_session_id, .stdio) or { panic(err) }

	dispatch := server.dispatch_message(new_request(2, 'tools/list', empty).encode(),
		stdio_session_id, .stdio) or { panic(err) }
	result := decode_response(dispatch.response) or { panic(err) }.result
	assert result.contains('"tools":[')
	// Neither revision's 2026-only fields may leak into the 2025 wire shape.
	assert !result.contains('resultType')
	assert !result.contains('ttlMs')
	assert !result.contains('cacheScope')
}
