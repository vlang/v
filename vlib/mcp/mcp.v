module mcp

import json
import net.http
import os
import time

pub const jsonrpc_version = '2.0'
pub const protocol_version = '2025-11-25'
pub const parse_error = ResponseError{
	code:    -32700
	message: 'Invalid JSON.'
}
pub const invalid_request = ResponseError{
	code:    -32600
	message: 'Invalid request.'
}
pub const method_not_found = ResponseError{
	code:    -32601
	message: 'Method not found.'
}
pub const invalid_params = ResponseError{
	code:    -32602
	message: 'Invalid params.'
}
pub const internal_error = ResponseError{
	code:    -32603
	message: 'Internal error.'
}
pub const server_not_initialized = ResponseError{
	code:    -32002
	message: 'Server not initialized.'
}

const default_content_type = 'application/json'
const streamable_http_accept = 'application/json, text/event-stream'
const content_length_header = 'Content-Length'
const mcp_session_id_header = 'Mcp-Session-Id'
const process_poll_interval = 5 * time.millisecond
const default_client_name = 'v.mcp'
const default_client_version = 'dev'

// ResponseError is the JSON-RPC error payload used by MCP responses.
pub struct ResponseError {
pub:
	code    int
	message string
	data    string @[raw]
}

// code returns the JSON-RPC error code.
pub fn (err ResponseError) code() int {
	return err.code
}

// msg returns the JSON-RPC error message.
pub fn (err ResponseError) msg() string {
	return err.message
}

// err casts the response error to `IError`.
pub fn (err ResponseError) err() IError {
	return IError(err)
}

// Null represents the JSON `null` literal.
pub struct Null {}

// str returns the JSON `null` literal.
pub fn (n Null) str() string {
	return 'null'
}

pub const null = Null{}

// Empty omits a JSON-RPC field when used with MCP helpers.
pub struct Empty {}

// str returns the empty string.
pub fn (e Empty) str() string {
	return ''
}

pub const empty = Empty{}

// EmptyObject encodes to an empty JSON object.
pub struct EmptyObject {}

// str returns the JSON empty object literal.
pub fn (e EmptyObject) str() string {
	return '{}'
}

pub const empty_object = EmptyObject{}

// Implementation identifies an MCP client or server implementation.
pub struct Implementation {
pub:
	name    string
	version string
}

// InitializeParams is the typed payload for the `initialize` request.
pub struct InitializeParams {
pub:
	protocol_version string         @[json: protocolVersion]
	capabilities     string         @[raw]
	client_info      Implementation @[json: clientInfo]
}

// InitializeResult is the typed result returned by an MCP server after initialization.
pub struct InitializeResult {
pub:
	protocol_version string         @[json: protocolVersion]
	capabilities     string         @[raw]
	server_info      Implementation @[json: serverInfo]
	instructions     string
}

// Request is a JSON-RPC request message encoded for MCP.
pub struct Request {
pub:
	jsonrpc string = jsonrpc_version
	id      string @[raw]
	method  string
	params  string @[omitempty; raw]
}

// new_request constructs an MCP request with a typed id and params payload.
pub fn new_request[I, P](id I, method string, params P) Request {
	return Request{
		id:     encode_id(id)
		method: method
		params: encode_value(params)
	}
}

// encode serializes the request to JSON.
pub fn (req Request) encode() string {
	params_payload := if req.params.len == 0 { '' } else { ',"params":${req.params}' }
	id_payload := if req.id.len == 0 { null.str() } else { req.id }
	return '{"jsonrpc":"${jsonrpc_version}","id":${id_payload},"method":${json.encode(req.method)}${params_payload}}'
}

// decode_params decodes the raw request params into `T`.
pub fn (req Request) decode_params[T]() !T {
	return decode_value[T](req.params)
}

// Notification is a JSON-RPC notification encoded for MCP.
pub struct Notification {
pub:
	jsonrpc string = jsonrpc_version
	method  string
	params  string @[omitempty; raw]
}

// new_notification constructs an MCP notification with a typed params payload.
pub fn new_notification[P](method string, params P) Notification {
	return Notification{
		method: method
		params: encode_value(params)
	}
}

// encode serializes the notification to JSON.
pub fn (notification Notification) encode() string {
	params_payload := if notification.params.len == 0 {
		''
	} else {
		',"params":${notification.params}'
	}
	return '{"jsonrpc":"${jsonrpc_version}","method":${json.encode(notification.method)}${params_payload}}'
}

// decode_params decodes the raw notification params into `T`.
pub fn (notification Notification) decode_params[T]() !T {
	return decode_value[T](notification.params)
}

// Response is a JSON-RPC response message encoded for MCP.
pub struct Response {
pub:
	jsonrpc string = jsonrpc_version
	id      string @[raw]
	result  string @[raw]
	error   ResponseError
}

// new_response constructs an MCP response with a typed id and result payload.
pub fn new_response[I, R](id I, result R, err ResponseError) Response {
	return Response{
		id:     encode_id(id)
		result: if err.code != 0 { '' } else { encode_value(result) }
		error:  err
	}
}

// encode serializes the response to JSON.
pub fn (resp Response) encode() string {
	mut payload := '{"jsonrpc":"${jsonrpc_version}"'
	if resp.error.code != 0 {
		payload += ',"error":' + json.encode(resp.error)
	} else {
		result_payload := if resp.result.len == 0 { null.str() } else { resp.result }
		payload += ',"result":' + result_payload
	}
	id_payload := if resp.id.len == 0 { null.str() } else { resp.id }
	return payload + ',"id":${id_payload}}'
}

// decode_result decodes the response result into `T`.
pub fn (resp Response) decode_result[T]() !T {
	if resp.error.code != 0 {
		return resp.error.err()
	}
	return decode_value[T](resp.result)
}

// decode_request decodes a JSON payload into an MCP request.
pub fn decode_request(raw string) !Request {
	return json.decode(Request, raw) or { return err }
}

// decode_notification decodes a JSON payload into an MCP notification.
pub fn decode_notification(raw string) !Notification {
	return json.decode(Notification, raw) or { return err }
}

// decode_response decodes a JSON payload into an MCP response.
pub fn decode_response(raw string) !Response {
	return json.decode(Response, raw) or { return err }
}

struct MessageEnvelope {
	jsonrpc string
	id      string @[raw]
	method  string
	params  string @[raw]
	result  string @[raw]
	error   ResponseError
}

fn (env MessageEnvelope) encode() string {
	if env.method.len != 0 {
		if env.id.len == 0 || env.id == null.str() {
			return Notification{
				method: env.method
				params: env.params
			}.encode()
		}
		return Request{
			id:     env.id
			method: env.method
			params: env.params
		}.encode()
	}
	return Response{
		id:     env.id
		result: env.result
		error:  env.error
	}.encode()
}

fn decode_envelope(raw string) !MessageEnvelope {
	return json.decode(MessageEnvelope, raw) or { return err }
}

// Transport is the boundary between MCP messages and the wire format.
pub interface Transport {
mut:
	send(message string) !
	receive() !string
	close()
}

@[params]
pub struct ClientConfig {
pub mut:
	protocol_version string         = protocol_version
	client_info      Implementation = Implementation{
		name:    default_client_name
		version: default_client_version
	}
	capabilities     string = '{}'
	headers          map[string]string
}

pub struct Client {
mut:
	transport         Transport
	config            ClientConfig
	next_id           int = 1
	initialized       bool
	init_result       InitializeResult
	pending_responses map[string]Response
	notifications     []Notification
	server_requests   []Request
}

// new_client constructs an MCP client on top of a custom transport.
pub fn new_client(transport Transport, config ClientConfig) Client {
	return Client{
		transport:         transport
		config:            config
		pending_responses: map[string]Response{}
	}
}

// connect creates an MCP client for a streamable HTTP endpoint.
pub fn connect(url string) !Client {
	return connect_http(url, ClientConfig{})
}

// connect_http creates an MCP client for a streamable HTTP endpoint.
pub fn connect_http(url string, config ClientConfig) !Client {
	transport := new_http_transport(url, config)!
	return new_client(transport, config)
}

// connect_stdio creates an MCP client that talks to a local stdio server process.
pub fn connect_stdio(command string, args []string, config ClientConfig) !Client {
	transport := new_process_transport(command, args)!
	return new_client(transport, config)
}

// initialize starts the MCP initialization handshake using the client's config.
pub fn (mut c Client) initialize() !InitializeResult {
	return c.initialize_with_raw(c.config.capabilities, c.config.client_info)
}

// initialize_with starts the MCP initialization handshake using typed capabilities.
pub fn (mut c Client) initialize_with[X](capabilities X, client_info Implementation) !InitializeResult {
	return c.initialize_with_raw(encode_value(capabilities), client_info)
}

// send_request sends a typed request and waits for its response.
pub fn (mut c Client) send_request(request Request) !Response {
	if request.method == 'initialize' {
		return error('mcp.Client.initialize must be used for the MCP handshake')
	}
	c.ensure_initialized()!
	c.transport.send(request.encode())!
	return c.wait_for_response(request.id)
}

// request_message sends a method call and returns the raw MCP response.
pub fn (mut c Client) request_message[P](method string, params P) !Response {
	request := new_request(c.next_request_id(), method, params)
	return c.send_request(request)
}

// request sends a method call and decodes its result into `Result`.
pub fn (mut c Client) request[P, R](method string, params P) !R {
	response := c.request_message(method, params)!
	result := response.decode_result[R]()!
	return result
}

// send_notification sends a typed notification message.
pub fn (mut c Client) send_notification(notification Notification) ! {
	if notification.method == 'notifications/initialized' {
		return error('notifications/initialized is sent automatically after initialization')
	}
	c.ensure_initialized()!
	c.transport.send(notification.encode())!
}

// notify sends a method notification with a typed params payload.
pub fn (mut c Client) notify[P](method string, params P) ! {
	c.send_notification(new_notification(method, params))!
}

// take_notifications drains notifications queued while waiting for responses.
pub fn (mut c Client) take_notifications() []Notification {
	if c.notifications.len == 0 {
		return []Notification{}
	}
	drained := c.notifications.clone()
	c.notifications = []Notification{}
	return drained
}

// take_requests drains server initiated requests queued while waiting for responses.
pub fn (mut c Client) take_requests() []Request {
	if c.server_requests.len == 0 {
		return []Request{}
	}
	drained := c.server_requests.clone()
	c.server_requests = []Request{}
	return drained
}

// close releases the underlying transport.
pub fn (mut c Client) close() {
	c.transport.close()
}

fn (mut c Client) initialize_with_raw(capabilities string, client_info Implementation) !InitializeResult {
	if c.initialized {
		return c.init_result
	}
	c.config.capabilities = normalize_capabilities(capabilities)
	c.config.client_info = normalize_client_info(client_info)
	params := InitializeParams{
		protocol_version: normalize_protocol_version(c.config.protocol_version)
		capabilities:     c.config.capabilities
		client_info:      c.config.client_info
	}
	request := Request{
		id:     encode_id(c.next_request_id())
		method: 'initialize'
		params: encode_initialize_params(params)
	}
	c.transport.send(request.encode())!
	response := c.wait_for_response(request.id)!
	result := response.decode_result[InitializeResult]()!
	c.transport.send(new_notification('notifications/initialized', empty).encode())!
	c.initialized = true
	c.init_result = result
	return result
}

fn (mut c Client) ensure_initialized() ! {
	if !c.initialized {
		c.initialize()!
	}
}

fn (mut c Client) next_request_id() int {
	request_id := c.next_id
	c.next_id++
	return request_id
}

fn (mut c Client) wait_for_response(expected_id string) !Response {
	if expected_id in c.pending_responses {
		response := c.pending_responses[expected_id]
		c.pending_responses.delete(expected_id)
		return response
	}
	for {
		raw_message := c.transport.receive()!
		envelope := decode_envelope(raw_message)!
		if envelope.method.len != 0 {
			if envelope.id.len == 0 || envelope.id == null.str() {
				c.notifications << Notification{
					method: envelope.method
					params: envelope.params
				}
			} else {
				c.server_requests << Request{
					id:     envelope.id
					method: envelope.method
					params: envelope.params
				}
			}
			continue
		}
		response := Response{
			id:     envelope.id
			result: envelope.result
			error:  envelope.error
		}
		if response.id == expected_id {
			return response
		}
		c.pending_responses[response.id] = response
	}
	return error('mcp: response loop exited unexpectedly')
}

fn encode_initialize_params(params InitializeParams) string {
	return '{"protocolVersion":${json.encode(params.protocol_version)},"capabilities":${normalize_capabilities(params.capabilities)},"clientInfo":${json.encode(params.client_info)}}'
}

struct NoFrameError {}

fn (err NoFrameError) msg() string {
	return 'no complete frame available'
}

fn (err NoFrameError) code() int {
	return 0
}

struct FrameExtraction {
	message   string
	remaining string
}

struct HttpTransport {
mut:
	url        string
	header     http.Header
	session_id string
	pending    []string
}

fn new_http_transport(url string, config ClientConfig) !HttpTransport {
	if url == '' {
		return error('mcp.connect_http: empty url')
	}
	if !url.starts_with('http://') && !url.starts_with('https://') {
		return error('mcp.connect_http: expected an http:// or https:// MCP endpoint')
	}
	mut header := http.new_header()
	header.set(.user_agent, default_client_name)
	if config.headers.len != 0 {
		header.add_custom_map(config.headers)!
	}
	return HttpTransport{
		url:    url
		header: header
	}
}

fn (mut transport HttpTransport) send(message string) ! {
	mut header := transport.header
	header.set(.content_type, default_content_type)
	header.set(.accept, streamable_http_accept)
	if transport.session_id != '' {
		header.set_custom(mcp_session_id_header, transport.session_id)!
	}
	response := http.fetch(
		method: .post
		url:    transport.url
		data:   message
		header: header
	)!
	if session_id := response.header.get_custom(mcp_session_id_header) {
		transport.session_id = session_id
	}
	messages := parse_http_response_messages(response)!
	if messages.len != 0 {
		transport.pending << messages
		return
	}
	if response.status_code >= 400 {
		return error('mcp.http: server returned HTTP ${response.status_code} without an MCP payload')
	}
}

fn (mut transport HttpTransport) receive() !string {
	if transport.pending.len == 0 {
		return error('mcp.http: no pending messages are available')
	}
	message := transport.pending[0]
	transport.pending = if transport.pending.len == 1 {
		[]string{}
	} else {
		transport.pending[1..].clone()
	}
	return message
}

fn (mut transport HttpTransport) close() {
	if transport.session_id == '' {
		return
	}
	mut header := transport.header
	header.set_custom(mcp_session_id_header, transport.session_id) or { return }
	http.fetch(
		method: .delete
		url:    transport.url
		header: header
	) or {}
	transport.session_id = ''
}

struct ProcessTransport {
mut:
	process &os.Process
	buffer  string
}

fn new_process_transport(command string, args []string) !ProcessTransport {
	if command == '' {
		return error('mcp.connect_stdio: empty command')
	}
	mut process := os.new_process(command)
	process.set_args(args)
	process.set_redirect_stdio()
	process.run()
	return ProcessTransport{
		process: process
	}
}

fn (mut transport ProcessTransport) send(message string) ! {
	transport.process.stdin_write(encode_framed_message(message))
}

fn (mut transport ProcessTransport) receive() !string {
	for {
		frame := try_extract_framed_message(transport.buffer) or {
			if err.msg() != NoFrameError{}.msg() {
				return err
			}
			FrameExtraction{}
		}
		if frame.message.len != 0 {
			transport.buffer = frame.remaining
			return frame.message
		}
		if transport.process.is_pending(.stdout) {
			chunk := transport.process.stdout_read()
			if chunk.len != 0 {
				transport.buffer += chunk
				continue
			}
		}
		if !transport.process.is_alive() {
			transport.buffer += transport.process.stdout_slurp()
			frame_after_exit := try_extract_framed_message(transport.buffer) or {
				if err.msg() != NoFrameError{}.msg() {
					return err
				}
				FrameExtraction{}
			}
			if frame_after_exit.message.len != 0 {
				transport.buffer = frame_after_exit.remaining
				return frame_after_exit.message
			}
			stderr_output := transport.process.stderr_slurp().trim_space()
			if stderr_output.len != 0 {
				return error('mcp.stdio: process exited before a full MCP message was received: ${stderr_output}')
			}
			return error('mcp.stdio: process exited before a full MCP message was received')
		}
		time.sleep(process_poll_interval)
	}
	return error('mcp.stdio: receive loop exited unexpectedly')
}

fn (mut transport ProcessTransport) close() {
	if transport.process.is_alive() {
		transport.process.signal_term()
		for _ in 0 .. 20 {
			if !transport.process.is_alive() {
				break
			}
			time.sleep(10 * time.millisecond)
		}
		if transport.process.is_alive() {
			transport.process.signal_kill()
		} else if transport.process.status in [.running, .stopped] {
			transport.process.wait()
		}
	}
	transport.process.close()
}

fn parse_http_response_messages(response http.Response) ![]string {
	content_type := response.header.get(.content_type) or { '' }
	body := response.body.trim_space()
	if body.len == 0 {
		return []string{}
	}
	content_type_lower := content_type.to_lower()
	if content_type_lower.starts_with('application/json')
		|| (content_type == '' && is_json_payload(body)) {
		return split_json_payloads(body)
	}
	if content_type_lower.starts_with('text/event-stream') {
		return parse_sse_messages(body)
	}
	return error('mcp.http: unsupported content type `${content_type}`')
}

fn split_json_payloads(body string) ![]string {
	trimmed := body.trim_space()
	if trimmed.len == 0 {
		return []string{}
	}
	if trimmed[0] != `[` {
		return [trimmed]
	}
	envelopes := json.decode([]MessageEnvelope, trimmed) or { return err }
	mut messages := []string{cap: envelopes.len}
	for envelope in envelopes {
		messages << envelope.encode()
	}
	return messages
}

fn parse_sse_messages(body string) ![]string {
	normalized := body.replace('\r\n', '\n').replace('\r', '\n')
	mut data_lines := []string{}
	mut messages := []string{}
	for line in normalized.split('\n') {
		if line.len == 0 {
			if data_lines.len != 0 {
				append_sse_payload(mut messages, data_lines.join('\n'))!
				data_lines = []string{}
			}
			continue
		}
		if line.starts_with(':') {
			continue
		}
		if line.starts_with('data:') {
			mut payload := line[5..]
			if payload.len != 0 && payload[0] == ` ` {
				payload = payload[1..]
			}
			data_lines << payload
		}
	}
	if data_lines.len != 0 {
		append_sse_payload(mut messages, data_lines.join('\n'))!
	}
	return messages
}

fn append_sse_payload(mut messages []string, payload string) ! {
	trimmed := payload.trim_space()
	if !is_json_payload(trimmed) {
		return
	}
	payloads := split_json_payloads(trimmed)!
	for item in payloads {
		messages << item
	}
}

fn is_json_payload(payload string) bool {
	trimmed := payload.trim_space()
	if trimmed.len == 0 {
		return false
	}
	return trimmed[0] == `{` || trimmed[0] == `[`
}

fn encode_framed_message(message string) string {
	return '${content_length_header}: ${message.len}\r\n\r\n${message}'
}

fn try_extract_framed_message(buffer string) !FrameExtraction {
	header_end := buffer.index('\r\n\r\n') or { return NoFrameError{} }
	header_text := buffer[..header_end]
	mut content_length := -1
	for line in header_text.split('\r\n') {
		if line.len == 0 {
			continue
		}
		parts := line.split_nth(':', 2)
		if parts.len != 2 {
			continue
		}
		if parts[0].trim_space().to_lower() != content_length_header.to_lower() {
			continue
		}
		length_text := parts[1].trim_space()
		parsed_length := length_text.int()
		if parsed_length == 0 && length_text != '0' {
			return error('mcp.stdio: invalid Content-Length header `${length_text}`')
		}
		content_length = parsed_length
		break
	}
	if content_length < 0 {
		return error('mcp.stdio: missing Content-Length header')
	}
	body_start := header_end + 4
	body_end := body_start + content_length
	if buffer.len < body_end {
		return NoFrameError{}
	}
	message := buffer[body_start..body_end]
	remaining := if body_end >= buffer.len { '' } else { buffer[body_end..] }
	return FrameExtraction{
		message:   message
		remaining: remaining
	}
}

fn encode_id[I](id I) string {
	return $if I is string {
		json.encode(id)
	} $else $if I is int {
		id.str()
	} $else {
		json.encode(id)
	}
}

fn encode_value[T](value T) string {
	return $if T is Empty {
		value.str()
	} $else $if T is EmptyObject {
		value.str()
	} $else $if T is Null {
		value.str()
	} $else $if T is string {
		json.encode(value)
	} $else {
		json.encode(value)
	}
}

fn decode_value[T](value string) !T {
	$if T is Empty {
		if value.len == 0 || value == null.str() {
			return Empty{}
		}
		return error('mcp: expected an empty payload, got `${value}`')
	} $else $if T is EmptyObject {
		if value == '{}' {
			return EmptyObject{}
		}
		return error('mcp: expected an empty object payload, got `${value}`')
	} $else $if T is Null {
		if value == null.str() {
			return null
		}
		return error('mcp: expected null, got `${value}`')
	} $else $if T is string {
		if value.len >= 2 && value[0] == `"` && value[value.len - 1] == `"` {
			return json.decode(string, value) or { return err }
		}
		return error('mcp: could not decode `${value}` into string')
	} $else $if T is bool {
		if value == 'true' {
			return true
		}
		if value == 'false' {
			return false
		}
		return error('mcp: could not decode `${value}` into bool')
	} $else {
		return json.decode(T, value) or { return err }
	}
}

fn normalize_client_info(client_info Implementation) Implementation {
	return if client_info.name == '' {
		Implementation{
			name:    default_client_name
			version: if client_info.version == '' {
				default_client_version
			} else {
				client_info.version
			}
		}
	} else if client_info.version == '' {
		Implementation{
			name:    client_info.name
			version: default_client_version
		}
	} else {
		client_info
	}
}

fn normalize_capabilities(capabilities string) string {
	trimmed := capabilities.trim_space()
	return if trimmed.len == 0 { '{}' } else { trimmed }
}

fn normalize_protocol_version(version string) string {
	trimmed := version.trim_space()
	return if trimmed.len == 0 { protocol_version } else { trimmed }
}
