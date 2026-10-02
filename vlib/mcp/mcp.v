module mcp

import json2 as json
import net.http
import os
import time

pub const jsonrpc_version = '2.0'
// protocol_version is the revision a server speaks by default. It stays on
// 2025-11-25 so clients that never negotiate keep the current wire behaviour.
pub const protocol_version = '2025-11-25'
// protocol_version_2025_11_25 and protocol_version_2026_07_28 name the
// revisions a server can speak; latest_protocol_version is the newest one.
// The 2025-11-25 constant aliases protocol_version so the default and the
// named revision can never drift apart.
pub const protocol_version_2025_11_25 = protocol_version
pub const protocol_version_2026_07_28 = '2026-07-28'
pub const latest_protocol_version = protocol_version_2026_07_28
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
pub const resource_not_found = ResponseError{
	code:    -32002
	message: 'Resource not found.'
}
pub const url_elicitation_required = ResponseError{
	code:    -32042
	message: 'URL mode elicitation required.'
}
pub const header_mismatch = ResponseError{
	code:    -32020
	message: 'Header mismatch.'
}
pub const missing_required_client_capability = ResponseError{
	code:    -32021
	message: 'Missing required client capability.'
}
pub const unsupported_protocol_version = ResponseError{
	code:    -32022
	message: 'Unsupported protocol version.'
}

const default_content_type = 'application/json'
const event_stream_content_type = 'text/event-stream'
const streamable_http_accept = '${default_content_type}, ${event_stream_content_type}'
const mcp_session_id_header = 'MCP-Session-Id'
const mcp_protocol_version_header = 'MCP-Protocol-Version'
const last_event_id_header = 'Last-Event-ID'
const default_protocol_version = '2025-03-26'
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

// Icon describes a UI icon advertised by an MCP implementation, tool,
// resource, resource template or prompt. `src` is required; `mime_type`,
// `sizes` and `theme` are optional metadata mirroring the spec's `Icon` shape.
pub struct Icon {
pub:
	src       string
	mime_type string   @[json: mimeType; omitempty]
	sizes     []string @[omitempty]
	theme     string   @[omitempty]
}

// Implementation identifies an MCP client or server implementation. `name`
// and `version` are required; `title`, `description`, `website_url` and
// `icons` are optional 2025-11-25 metadata extensions (BaseMetadata + Icons).
pub struct Implementation {
pub:
	name        string
	version     string
	title       string @[omitempty]
	description string @[omitempty]
	website_url string @[json: websiteUrl; omitempty]
	icons       []Icon @[omitempty]
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
		payload += ',"error":' + encode_response_error(resp.error)
	} else {
		result_payload := if resp.result.len == 0 { null.str() } else { resp.result }
		payload += ',"result":' + result_payload
	}
	id_payload := if resp.id.len == 0 { null.str() } else { resp.id }
	return payload + ',"id":${id_payload}}'
}

// encode_response_error renders a ResponseError as JSON, preserving the
// `data` payload as raw JSON. V's `json.encode` ignores the `@[raw]` tag on
// encode, so a hand-rolled writer is required to keep the wire shape spec
// compliant (the `data` field MAY be any JSON value per JSON-RPC 2.0).
fn encode_response_error(err ResponseError) string {
	mut fields := ['"code":${err.code}', '"message":${json.encode(err.message)}']
	if err.data.trim_space() != '' {
		fields << '"data":${err.data}'
	}
	return '{${fields.join(',')}}'
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
	return json.decode[Request](raw) or { return err }
}

// decode_notification decodes a JSON payload into an MCP notification.
pub fn decode_notification(raw string) !Notification {
	return json.decode[Notification](raw) or { return err }
}

// decode_response decodes a JSON payload into an MCP response.
pub fn decode_response(raw string) !Response {
	return json.decode[Response](raw) or { return err }
}

// MessageEnvelope is the shared wire representation used while decoding MCP messages.
pub struct MessageEnvelope {
pub:
	jsonrpc string
	id      string @[raw]
	method  string
	params  string @[raw]
	result  string @[raw]
	error   ResponseError
}

// is_notification_id reports whether a JSON-RPC `id` field encodes the
// "absent or null" form, which per spec marks the envelope as a notification.
fn is_notification_id(id string) bool {
	return id == '' || id == null.str()
}

fn (env MessageEnvelope) encode() string {
	if env.method.len != 0 {
		if is_notification_id(env.id) {
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
	return json.decode[MessageEnvelope](raw) or { return err }
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
	// stateless_2026 selects the sessionless 2026-07-28 client: no initialize
	// handshake, every request carries its version in `_meta`, and the HTTP
	// transport adds the `MCP-Protocol-Version`, `Mcp-Method` and `Mcp-Name`
	// headers. It is also implied by asking for the 2026-07-28 revision.
	stateless_2026 bool
	// supported_versions is the fallback order used when a 2026-07-28 server
	// rejects the requested revision with UnsupportedProtocolVersionError.
	supported_versions []string = [protocol_version_2025_11_25, protocol_version_2026_07_28]
	// log_level, when set, is sent as the reserved `_meta` logLevel key of
	// every 2026-07-28 request, which is what opts that request into log
	// notifications.
	log_level string
	// roots_handler answers a `roots/list` request embedded in a 2026-07-28
	// MRTR result. It returns the raw JSON of a ListRootsResult.
	roots_handler ?fn () string
	// sampling_handler answers a `sampling/createMessage` request embedded in a
	// 2026-07-28 MRTR result. It takes the raw params and returns the raw JSON
	// of a CreateMessageResult.
	sampling_handler ?fn (string) string
	// elicitation_handler answers an `elicitation/create` request embedded in a
	// 2026-07-28 MRTR result. It takes the raw params and returns the raw JSON
	// of an ElicitResult.
	elicitation_handler ?fn (string) string
}

// max_mrtr_rounds bounds the automatic MRTR retry loop of a 2026-07-28 client.
const max_mrtr_rounds = 8

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

// connect_2026 creates a sessionless 2026-07-28 client for a streamable HTTP
// endpoint. It never runs the initialize handshake.
pub fn connect_2026(url string, config ClientConfig) !Client {
	mut config_2026 := config
	config_2026.stateless_2026 = true
	config_2026.protocol_version = protocol_version_2026_07_28
	return connect_http(url, config_2026)
}

// is_stateless_2026 reports whether this client speaks the sessionless
// 2026-07-28 protocol rather than the 2025-11-25 handshake.
pub fn (c &Client) is_stateless_2026() bool {
	if c.config.stateless_2026 {
		return true
	}
	return normalize_protocol_version(c.config.protocol_version) == protocol_version_2026_07_28
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
	mut current := request
	mut round := 0
	mut downgraded := false
	for {
		if c.is_stateless_2026() {
			current = c.inject_stateless_meta(current)
		}
		c.transport.send(current.encode())!
		response := c.wait_for_response(current.id)!
		if !c.is_stateless_2026() {
			return response
		}
		// A server that does not speak the requested revision names the ones it
		// does; fall back to the first and retry the request exactly once.
		if response.error.code == -32022 && !downgraded {
			if c.downgrade_to_supported(response) {
				downgraded = true
				continue
			}
		}
		retry := c.build_input_retry(request, response)!
		if retry.method.len == 0 {
			return response
		}
		current = retry
		if round >= max_mrtr_rounds {
			return error('mcp.Client: gave up after ${max_mrtr_rounds} input rounds')
		}
		round++
	}
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
	if c.is_stateless_2026() {
		// 2026-07-28 has no handshake: `server/discover` is how a client learns
		// what the server speaks, and the InitializeResult is synthesized from
		// it so callers of `initialize()` keep working.
		c.config.protocol_version = protocol_version_2026_07_28
		return c.discover_as_initialize()
	}
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

// discover_as_initialize asks `server/discover` and maps its answer onto an
// InitializeResult, which is all a 2026-07-28 client can know before its first
// real request.
fn (mut c Client) discover_as_initialize() !InitializeResult {
	request := c.inject_stateless_meta(new_request(c.next_request_id(), 'server/discover', empty))
	c.transport.send(request.encode())!
	response := c.wait_for_response(request.id)!
	if response.error.code == unsupported_protocol_version.code && c.downgrade_to_supported(response) {
		// The server does not speak 2026-07-28. Fall back to the revision it
		// named, which means going through the real handshake.
		return c.initialize_with_raw(c.config.capabilities, c.config.client_info)
	}
	discovery := response.decode_result[DiscoverResult]()!
	c.initialized = true
	c.init_result = InitializeResult{
		protocol_version: protocol_version_2026_07_28
		capabilities:     discovery.capabilities
		server_info:      Implementation{
			name:    'mcp-server'
			version: latest_protocol_version
		}
		instructions:     discovery.instructions
	}
	return c.init_result
}

fn (mut c Client) ensure_initialized() ! {
	if !c.initialized {
		c.initialize()!
	}
}

// inject_stateless_meta stamps a 2026-07-28 request with the reserved `_meta`
// keys. Every such request MUST declare its protocol version this way; the
// client info and the optional log level travel with it.
fn (mut c Client) inject_stateless_meta(request Request) Request {
	version := json.encode(normalize_protocol_version(c.config.protocol_version))
	info := json.encode(c.config.client_info)
	capabilities := normalize_capabilities(c.config.capabilities)
	mut fields := [
		'"io.modelcontextprotocol/protocolVersion":${version}',
		'"io.modelcontextprotocol/clientInfo":${info}',
		'"io.modelcontextprotocol/clientCapabilities":${capabilities}',
	]
	if c.config.log_level.trim_space() != '' {
		fields << '"io.modelcontextprotocol/logLevel":${json.encode(c.config.log_level.trim_space())}'
	}
	return Request{
		jsonrpc: request.jsonrpc
		id:      request.id
		method:  request.method
		params:  merge_params(request.params, '"_meta":{${fields.join(',')}}')
	}
}

// merge_params splices a set of `"key":value` members into a request's params
// object. The extras are members, never a nested object, so a caller can merge
// several at once without producing invalid JSON.
fn merge_params(params_json string, extra_members string) string {
	trimmed := params_json.trim_space()
	mut parts := []string{}
	if trimmed.len >= 2 && trimmed[0] == `{` && trimmed[trimmed.len - 1] == `}` {
		inner := trimmed[1..trimmed.len - 1].trim_space()
		if inner.len != 0 {
			parts << inner
		}
	}
	if extra_members.trim_space().len != 0 {
		parts << extra_members.trim_space()
	}
	if parts.len == 0 {
		return '{}'
	}
	return '{${parts.join(',')}}'
}

// ResultTypeInputRequired marks a 2026-07-28 result that needs client input.
const result_type_input_required = 'input_required'

// ResultTypeComplete marks a finished 2026-07-28 result. A result without a
// `resultType` is treated as complete, as the spec requires.
const result_type_complete = 'complete'

// result_type_of reads the top-level `resultType` of a result object.
fn result_type_of(result_json string) string {
	trimmed := result_json.trim_space()
	wrapper := json.decode[ResultTypeEnvelope](trimmed) or { return result_type_complete }
	return wrapper.result_type
}

struct ResultTypeEnvelope {
	result_type string = result_type_complete @[json: resultType]
}

// InputRequestsEnvelope is the MRTR part of a 2026-07-28 result. The embedded
// requests are arbitrary JSON objects, so they are captured dynamically and
// re-encoded verbatim.
struct InputRequestsEnvelope {
	input_requests map[string]json.Any @[json: inputRequests]
	request_state  string              @[json: requestState]
}

// build_input_retry answers a MRTR result by running the registered handlers
// for every embedded request and rebuilding the original request with
// `params.inputResponses`. An empty method means the result needs no input, so
// the caller stops the retry loop.
fn (mut c Client) build_input_retry(original Request, response Response) !Request {
	if response.error.code != 0 || response.result.len == 0 {
		return Request{}
	}
	if result_type_of(response.result) != result_type_input_required {
		return Request{}
	}
	wrapper := json.decode[InputRequestsEnvelope](response.result.trim_space()) or {
		return Request{}
	}
	mut responses := []string{}
	for key, raw in wrapper.input_requests {
		answer := c.answer_input_request(json.encode(raw)) or {
			return error('mcp.Client: no handler registered for the embedded request `${key}`')
		}
		responses << '${json.encode(key)}:${answer}'
	}
	if responses.len == 0 {
		return error('mcp.Client: input_required result without inputRequests')
	}
	mut extra_fields := ['"inputResponses":{${responses.join(',')}}']
	if wrapper.request_state != '' {
		extra_fields << '"requestState":${json.encode(wrapper.request_state)}'
	}
	return Request{
		id:     original.id
		method: original.method
		params: merge_params(original.params, extra_fields.join(','))
	}
}

// answer_input_request dispatches one embedded MRTR request object to the
// handler registered for its method.
fn (c &Client) answer_input_request(request_json string) ?string {
	trimmed := request_json.trim_space()
	envelope := json.decode[EmbeddedRequest](trimmed) or { return none }
	return match envelope.method {
		'roots/list' {
			handler := c.config.roots_handler or { return none }
			handler()
		}
		'sampling/createMessage' {
			handler := c.config.sampling_handler or { return none }
			handler(envelope.params)
		}
		'elicitation/create' {
			handler := c.config.elicitation_handler or { return none }
			handler(envelope.params)
		}
		else { none }
	}
}

// EmbeddedRequest is one request object embedded in an MRTR result. It keeps
// the 2025 method names and shapes but is never sent as a JSON-RPC request.
struct EmbeddedRequest {
	method string
	params string @[raw]
}

// downgrade_to_supported switches the client to the first revision the server
// named in an UnsupportedProtocolVersionError payload.
fn (mut c Client) downgrade_to_supported(response Response) bool {
	payload := json.decode[UnsupportedVersionEnvelope](response.error.data.trim_space()) or {
		return false
	}
	for candidate in payload.supported {
		trimmed := candidate.trim_space()
		if trimmed != '' && trimmed != normalize_protocol_version(c.config.protocol_version) {
			c.config.protocol_version = trimmed
			c.config.stateless_2026 = trimmed == protocol_version_2026_07_28
			// Falling back to a revision with a handshake means starting over.
			c.initialized = false
			return true
		}
	}
	return false
}

// UnsupportedVersionEnvelope is the data payload of
// UnsupportedProtocolVersionError.
struct UnsupportedVersionEnvelope {
	supported []string
	requested string
}

// listen subscribes to notifications and returns the subset the server
// acknowledged. A 2026-07-28 client only: the notifications themselves are
// queued for `take_notifications`, tagged with the subscription id. It returns
// after the matching acknowledgment without waiting for a live stdio subscription to end.
pub fn (mut c Client) listen[P](filter P) !SubscriptionFilter {
	if !c.is_stateless_2026() {
		return error('mcp.Client.listen: subscriptions/listen requires the 2026-07-28 protocol')
	}
	c.ensure_initialized()!
	if !c.is_stateless_2026() {
		return error('mcp.Client.listen: subscriptions/listen requires the 2026-07-28 protocol')
	}
	request := c.inject_stateless_meta(new_request(c.next_request_id(), 'subscriptions/listen',
		filter))
	c.transport.send(request.encode())!
	return c.wait_for_listen_ack(request.id)
}

// wait_for_listen_ack receives until the server acknowledges a
// `subscriptions/listen` request. A live stdio subscription stays open and
// never answers the request itself, so the acknowledgment notification is the
// success signal there; over HTTP the stream is finite, and the closing
// SubscriptionsListenResult is consumed before returning.
fn (mut c Client) wait_for_listen_ack(request_id string) !SubscriptionFilter {
	wait_for_result := c.transport is HttpTransport
	expected_subscription_id := if request_id.starts_with('"') {
		json.encode(json.decode[string](request_id)!)
	} else {
		request_id
	}
	mut acknowledged := SubscriptionFilter{}
	mut got_ack := false
	for {
		raw_message := c.transport.receive() or {
			if got_ack {
				// A finite HTTP stream may legitimately end right after the
				// acknowledgment.
				return acknowledged
			}
			return err
		}
		envelope := decode_envelope(raw_message)!
		if envelope.method.len != 0 {
			just_acknowledged := is_notification_id(envelope.id)
				&& envelope.method == listen_acknowledged_method && !got_ack
				&& (subscription_request_id_of(Notification{
					method: envelope.method
					params: envelope.params
				}) or { '' }) == expected_subscription_id
			if just_acknowledged {
				ack := json.decode[ListenAcknowledgement](envelope.params.trim_space()) or {
					return error('mcp.Client.listen: malformed subscription acknowledgment')
				}
				acknowledged = ack.notifications
				got_ack = true
			}
			if is_notification_id(envelope.id) {
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
			if just_acknowledged && !wait_for_result {
				return acknowledged
			}
			continue
		}
		response := Response{
			id:     envelope.id
			result: envelope.result
			error:  envelope.error
		}
		if response.id == request_id {
			if response.error.code != 0 {
				return response.error.err()
			}
			if got_ack {
				return acknowledged
			}
			// The closing result arrived before the acknowledgment; keep
			// waiting for the acknowledgment.
			continue
		}
		if !got_ack && is_notification_id(response.id) && response.error.code != 0 {
			// A transport level rejection, such as an HTTP 406, is reported
			// under a null id.
			return response.error.err()
		}
		c.pending_responses[response.id] = response
	}
	return error('mcp.Client.listen: response loop exited unexpectedly')
}

// ListenAcknowledgement is the first message of a 2026-07-28 subscription
// stream: the notification types the server honors.
struct ListenAcknowledgement {
	notifications SubscriptionFilter
}

// subscription_id_of returns the reserved subscriptionId `_meta` value of a
// notification that arrived on a listen stream. String IDs are decoded, and
// numeric IDs are returned as text for display.
pub fn subscription_id_of(notification Notification) ?string {
	request_id := subscription_request_id_of(notification) or { return none }
	if request_id.starts_with('"') {
		return json.decode[string](request_id) or { return none }
	}
	return request_id
}

// subscription_request_id_of preserves the JSON type for acknowledgment
// matching, normalizing string escapes while keeping numeric tokens intact.
fn subscription_request_id_of(notification Notification) ?string {
	wrapper := json.decode[SubscriptionIdEnvelope](notification.params.trim_space()) or {
		return none
	}
	raw_id := wrapper.meta.subscription_id.trim_space()
	if raw_id.len == 0 {
		return none
	}
	id := json.decode[json.Any](raw_id) or { return none }
	return match id {
		string { json.encode(id) }
		i64, u64, f64 { raw_id }
		else { none }
	}
}

struct SubscriptionIdEnvelope {
	meta SubscriptionIdMeta @[json: '_meta']
}

struct SubscriptionIdMeta {
	subscription_id string @[json: 'io.modelcontextprotocol/subscriptionId'; raw]
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
			if is_notification_id(envelope.id) {
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

// FrameExtraction holds a single stdio message and the unconsumed buffer remainder.
struct FrameExtraction {
	message   string
	remaining string
}

// DiscoverResult is what `server/discover` answers with: the revisions the
// server speaks and the capabilities it offers.
pub struct DiscoverResult {
pub:
	supported_versions []string @[json: supportedVersions]
	capabilities       string   @[raw]
	instructions       string
	ttl_ms             int    @[json: ttlMs]
	cache_scope        string @[json: cacheScope]
	result_type        string @[json: resultType]
}

struct HttpTransport {
mut:
	url              string
	header           http.Header
	session_id       string
	protocol_version string
	// stateless_2026 switches the transport to the sessionless 2026-07-28
	// request shape: the per-request `Mcp-Method` / `Mcp-Name` headers and no
	// MCP-Session-Id.
	stateless_2026 bool
	pending        []string
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
	mut stateless_2026 := config.stateless_2026
	if normalize_protocol_version(config.protocol_version) == protocol_version_2026_07_28 {
		stateless_2026 = true
	}
	return HttpTransport{
		url:            url
		header:         header
		stateless_2026: stateless_2026
	}
}

fn (mut transport HttpTransport) send(message string) ! {
	mut header := transport.header
	header.set(.content_type, default_content_type)
	method := transport.request_method(message)
	if method == 'subscriptions/listen' {
		// An HTTP listen stream is only served as a finite SSE response; the
		// stateless server gate rejects a JSON capable Accept for it.
		header.set(.accept, event_stream_content_type)
	} else {
		header.set(.accept, streamable_http_accept)
	}
	if transport.stateless_2026 {
		// The 2026-07-28 headers mirror the body, so a mismatch is impossible
		// by construction. The version comes from the `_meta` the client just
		// injected rather than from a field that could drift.
		header.set_custom(mcp_protocol_version_header, transport.request_version(message)) or {}
		header.set_custom(mcp_method_header, method) or {}
		name := transport.request_target(message)
		if name != '' {
			header.set_custom(mcp_name_header, name) or {}
		}
	} else {
		if transport.session_id != '' {
			header.set_custom(mcp_session_id_header, transport.session_id)!
		}
		if transport.protocol_version != '' {
			header.set_custom(mcp_protocol_version_header, transport.protocol_version)!
		}
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
	if transport.protocol_version == '' && transport.session_id != '' {
		// First handshake response: capture the negotiated version so all
		// subsequent requests carry MCP-Protocol-Version per spec §Transports.
		transport.protocol_version = read_negotiated_version(response.body)
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

// request_version reads the 2026-07-28 protocol version a message declares in
// its `_meta`.
fn (transport &HttpTransport) request_version(message string) string {
	envelope := decode_envelope(message) or { return '' }
	return extract_stateless_meta(envelope.params).protocol_version
}

// request_method reads the JSON-RPC method a message carries.
fn (transport &HttpTransport) request_method(message string) string {
	envelope := decode_envelope(message) or { return '' }
	return envelope.method
}

// request_target reads the tool, resource or prompt a 2026-07-28 message
// addresses, or an empty string when the method addresses none.
fn (transport &HttpTransport) request_target(message string) string {
	envelope := decode_envelope(message) or { return '' }
	field := method_target_field(envelope.method)
	if field == '' {
		return ''
	}
	target := decode_optional_params[NamedTargetParams](envelope.params) or {
		NamedTargetParams{}
	}
	return if field == 'uri' { target.uri } else { target.name }
}

fn read_negotiated_version(body string) string {
	envelope := decode_envelope(body) or { return '' }
	result := json.decode[InitializeResult](envelope.result) or { return '' }
	return result.protocol_version
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
	transport.process.stdin_write(encode_stdio_message(message))
}

fn (mut transport ProcessTransport) receive() !string {
	for {
		frame := try_extract_stdio_message(transport.buffer) or {
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
			frame_after_exit := try_extract_stdio_message(transport.buffer) or {
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
	envelopes := json.decode[[]MessageEnvelope](trimmed) or { return err }
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

// encode_stdio_message produces a newline-delimited stdio frame per MCP spec.
// The MCP spec mandates that messages are delimited by newlines and MUST NOT
// contain embedded newlines. Compact JSON encoding satisfies the latter; we
// strip stray CR/LF defensively to keep the on-wire contract.
fn encode_stdio_message(message string) string {
	return message.replace('\r', '').replace('\n', '') + '\n'
}

// try_extract_stdio_message consumes the next newline-delimited frame from
// `buffer`, returning the message body without its trailing newline.
fn try_extract_stdio_message(buffer string) !FrameExtraction {
	newline := buffer.index('\n') or { return NoFrameError{} }
	mut end := newline
	if end > 0 && buffer[end - 1] == `\r` {
		end--
	}
	message := buffer[..end].trim_space()
	remaining := if newline + 1 >= buffer.len { '' } else { buffer[newline + 1..] }
	if message.len == 0 {
		return try_extract_stdio_message(remaining) or {
			if err.msg() == NoFrameError{}.msg() {
				return NoFrameError{}
			}
			err
		}
	}
	return FrameExtraction{
		message:   message
		remaining: remaining
	}
}

fn encode_id[I](id I) string {
	return $if I is int {
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
	} $else {
		json.encode(value)
	}
}

fn decode_value[T](value string) !T {
	$if T is Empty {
		if value == '' || value == null.str() {
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
			return json.decode[string](value) or { return err }
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
		return json.decode[T](value) or { return err }
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
