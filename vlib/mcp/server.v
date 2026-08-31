module mcp

import json2 as json
import io
import net.http
import os
import rand
import sync
import time

const default_server_name = 'v.mcp.server'
const default_server_version = 'dev'
const default_http_path = '/mcp'
const default_list_page_size = 50
const completion_values_limit = 100
const stdio_session_id = 'stdio'
const event_log_capacity = 1024

// SessionTransport identifies how an MCP session is connected.
pub enum SessionTransport {
	stdio
	http
}

// LogLevel is the RFC 5424 severity used by `notifications/message`.
pub enum LogLevel {
	debug
	info
	notice
	warning
	error
	critical
	alert
	emergency
}

// str returns the wire string for a log level (lowercase per spec).
pub fn (l LogLevel) str() string {
	return match l {
		.debug { 'debug' }
		.info { 'info' }
		.notice { 'notice' }
		.warning { 'warning' }
		.error { 'error' }
		.critical { 'critical' }
		.alert { 'alert' }
		.emergency { 'emergency' }
	}
}

// parse_log_level decodes the wire string for a log level.
pub fn parse_log_level(value string) ?LogLevel {
	return match value {
		'debug' { LogLevel.debug }
		'info' { LogLevel.info }
		'notice' { LogLevel.notice }
		'warning' { LogLevel.warning }
		'error' { LogLevel.error }
		'critical' { LogLevel.critical }
		'alert' { LogLevel.alert }
		'emergency' { LogLevel.emergency }
		else { none }
	}
}

// Context provides request-scoped server metadata to handlers.
pub struct Context {
pub:
	session_id          string @[json: sessionId]
	request_id          string @[json: requestId]
	method              string
	transport           SessionTransport
	protocol_version    string         @[json: protocolVersion]
	client_info         Implementation @[json: clientInfo]
	client_capabilities string         @[json: clientCapabilities; raw]
	progress_token      string         @[json: progressToken; raw]
mut:
	server &Server = unsafe { nil } @[skip]
}

// is_cancelled reports whether the client has sent `notifications/cancelled`
// for this request. Handlers should poll this for cooperative cancellation.
pub fn (ctx Context) is_cancelled() bool {
	if isnil(ctx.server) {
		return false
	}
	return ctx.server.is_request_cancelled(ctx.session_id, ctx.request_id)
}

// notify_progress sends `notifications/progress` for this request when the
// client included a `_meta.progressToken`. `total` and `message` are optional
// (pass 0 / '' to omit).
pub fn (ctx Context) notify_progress(progress f64, total f64, message string) {
	if isnil(ctx.server) || ctx.progress_token.trim_space() == '' {
		return
	}
	ctx.server.notify_progress_for(ctx.session_id, ctx.progress_token, progress, total, message)
}

// ToolAnnotations exposes the optional behavioural hints described by the
// MCP spec (`tools/list` Annotations object).
pub struct ToolAnnotations {
pub:
	title            string @[omitempty]
	read_only_hint   ?bool  @[json: readOnlyHint]
	destructive_hint ?bool  @[json: destructiveHint]
	idempotent_hint  ?bool  @[json: idempotentHint]
	open_world_hint  ?bool  @[json: openWorldHint]
}

// ToolExecution carries optional execution metadata for a tool. Set
// `task_support` to one of `'forbidden'`, `'optional'` or `'required'` to
// advertise tasks/* support; the default (empty) omits the field on the wire.
pub struct ToolExecution {
pub:
	task_support string @[json: taskSupport; omitempty]
}

// Annotations are the optional rendering hints attached to resources, resource
// templates and content blocks. `audience` is a list of MCP `Role` values
// (e.g. `'user'`, `'assistant'`); `priority` is in the `[0.0, 1.0]` range with
// higher meaning more important; `last_modified` is an ISO 8601 timestamp.
pub struct Annotations {
pub:
	audience      []string @[omitempty]
	priority      ?f64
	last_modified string @[json: lastModified; omitempty]
}

// Tool describes an MCP tool exposed by the server.
pub struct Tool {
pub:
	name          string
	title         string @[omitempty]
	description   string @[omitempty]
	input_schema  string = default_tool_input_schema @[json: inputSchema; raw]
	output_schema string @[json: outputSchema; omitempty; raw]
	annotations   ToolAnnotations
	icons         []Icon @[omitempty]
	execution     ToolExecution
}

// Resource describes a concrete MCP resource exposed by the server.
pub struct Resource {
pub:
	uri         string
	name        string
	title       string @[omitempty]
	description string @[omitempty]
	mime_type   string @[json: mimeType; omitempty]
	size        ?int
	icons       []Icon @[omitempty]
	annotations Annotations
}

// ResourceTemplate describes a parameterized MCP resource URI template.
pub struct ResourceTemplate {
pub:
	uri_template string @[json: uriTemplate]
	name         string
	title        string @[omitempty]
	description  string @[omitempty]
	mime_type    string @[json: mimeType; omitempty]
	icons        []Icon @[omitempty]
	annotations  Annotations
}

// ResourceContents contains the result of `resources/read`.
pub struct ResourceContents {
pub:
	uri       string
	mime_type string @[json: mimeType; omitempty]
	text      string @[omitempty]
	blob      string @[omitempty]
}

// ReadResourceResult is returned by `resources/read`.
pub struct ReadResourceResult {
pub:
	contents []ResourceContents
}

// PromptArgument describes one prompt argument.
pub struct PromptArgument {
pub:
	name        string
	description string @[omitempty]
	required    bool
}

// Prompt describes an MCP prompt exposed by the server.
pub struct Prompt {
pub:
	name        string
	title       string @[omitempty]
	description string @[omitempty]
	arguments   []PromptArgument
	icons       []Icon @[omitempty]
}

// PromptMessage is one message returned by `prompts/get`.
pub struct PromptMessage {
pub:
	role    string
	content string @[raw]
}

// GetPromptResult is returned by `prompts/get`.
pub struct GetPromptResult {
pub:
	description string @[omitempty]
	messages    []PromptMessage
}

// ToolResult is returned by `tools/call`.
pub struct ToolResult {
pub:
	content            string @[omitempty; raw]
	structured_content string @[json: structuredContent; omitempty; raw]
	is_error           bool   @[json: isError]
}

// ToolHandler handles `tools/call` for a registered tool.
pub type ToolHandler = fn (ctx Context, arguments string) !ToolResult

// ResourceHandler handles `resources/read` for a registered resource URI.
pub type ResourceHandler = fn (ctx Context, uri string) !ReadResourceResult

// PromptHandler handles `prompts/get` for a registered prompt.
pub type PromptHandler = fn (ctx Context, arguments string) !GetPromptResult

// CompletionRef identifies the prompt or resource template a completion
// targets. Prompt refs set `name`; resource refs set `uri`.
pub struct CompletionRef {
pub:
	ref_type string @[json: type] // 'ref/prompt' or 'ref/resource'
	name     string @[omitempty]
	uri      string @[omitempty]
}

// CompletionResult is the payload returned by `completion/complete`.
pub struct CompletionResult {
pub:
	values   []string
	total    ?int
	has_more ?bool
}

// CompletionHandler returns candidate values for a partial argument value.
// `arguments_json` is the JSON object of already-supplied sibling arguments
// (the spec's `context.arguments`).
pub type CompletionHandler = fn (ctx Context, current_value string, arguments_json string) !CompletionResult

// ServerConfig configures an MCP server instance.
@[params]
pub struct ServerConfig {
pub:
	name             string
	version          string
	title            string
	description      string
	website_url      string
	icons            []Icon
	protocol_version string = protocol_version
	capabilities     string
	instructions     string
	http_path        string = default_http_path
	// enable_logging declares the `logging` capability and lets clients call
	// `logging/setLevel`. Disabled by default — turn on when the server emits
	// `notifications/message` payloads.
	enable_logging bool
	// allowed_origins lists the Origin header values accepted on Streamable
	// HTTP. Use `*` to accept any origin (NOT recommended). When empty the
	// server only accepts requests without an Origin header or from the
	// loopback addresses (`http://localhost[:port]`, `http://127.0.0.1[:port]`,
	// `http://[::1][:port]`, or the literal string `null`).
	allowed_origins []string
}

struct RegisteredTool {
	tool    Tool
	handler ToolHandler = unsafe { nil }
}

struct RegisteredResource {
	resource Resource
	handler  ResourceHandler = unsafe { nil }
}

struct RegisteredPrompt {
	prompt  Prompt
	handler PromptHandler = unsafe { nil }
}

struct RegisteredCompletion {
	ref      CompletionRef
	argument string
	handler  CompletionHandler = unsafe { nil }
}

// Root identifies a filesystem-or-URI boundary advertised by the client in
// response to `roots/list`.
pub struct Root {
pub:
	uri  string
	name string @[omitempty]
}

// ListRootsResult is the typed payload returned by the client to a server
// `roots/list` request.
pub struct ListRootsResult {
pub:
	roots []Root
}

// SamplingMessage is one message of a sampling/createMessage exchange.
pub struct SamplingMessage {
pub:
	role    string
	content string @[raw]
}

// ModelHint is a name hint for sampling/createMessage model selection.
pub struct ModelHint {
pub:
	name string
}

// ModelPreferences expresses sampling/createMessage routing weights.
pub struct ModelPreferences {
pub:
	hints                 []ModelHint
	cost_priority         f64 @[json: costPriority; omitempty]
	speed_priority        f64 @[json: speedPriority; omitempty]
	intelligence_priority f64 @[json: intelligencePriority; omitempty]
}

// ToolChoice configures `sampling/createMessage` tool-use behaviour. `mode`
// is one of `'auto'` (default), `'required'`, or `'none'`.
pub struct ToolChoice {
pub:
	mode string
}

// CreateMessageParams are the typed parameters for sampling/createMessage.
pub struct CreateMessageParams {
pub:
	messages          []SamplingMessage
	model_preferences ModelPreferences @[json: modelPreferences]
	system_prompt     string           @[json: systemPrompt; omitempty]
	max_tokens        int              @[json: maxTokens]
	temperature       f64              @[omitempty]
	stop_sequences    []string         @[json: stopSequences; omitempty]
	metadata          string           @[omitempty; raw]
	tools             []Tool           @[omitempty]
	tool_choice       ToolChoice       @[json: toolChoice; omitempty]
	include_context   string           @[json: includeContext; omitempty]
}

// CreateMessageResult is the typed payload returned by the client.
pub struct CreateMessageResult {
pub:
	role        string
	content     string @[raw]
	model       string
	stop_reason string @[json: stopReason]
}

// ElicitSchema is the requested object schema sent to the client for elicitation.
pub struct ElicitSchema {
pub:
	type_      string = 'object'   @[json: type]
	properties string   @[raw]
	required   []string @[omitempty]
}

// ElicitParams are the typed parameters for elicitation/create. Set `mode`
// to `'url'` and supply `url` + `elicitation_id` to send a URL-mode request;
// otherwise the call defaults to form mode and `requested_schema` is
// expected to describe the form fields.
pub struct ElicitParams {
pub:
	mode             string @[omitempty]
	message          string
	requested_schema ElicitSchema @[json: requestedSchema; omitempty]
	url              string       @[omitempty]
	elicitation_id   string       @[json: elicitationId; omitempty]
}

// ElicitResult is the typed payload returned by the client.
pub struct ElicitResult {
pub:
	action  string
	content string @[omitempty; raw]
}

struct LoggedEvent {
	id   int
	body string
}

struct Session {
mut:
	id                  string
	transport           SessionTransport
	protocol_version    string
	client_info         Implementation
	client_capabilities string
	initialize_complete bool
	initialized         bool
	notification_queue  []string
	subscribed_uris     []string
	log_level           LogLevel = .debug
	log_level_set       bool
	cancelled_requests  []string
	event_log           []LoggedEvent
	next_event_id       int = 1
	next_request_seq    int = 1
	pending_responses   map[string]Response
	// progress_seen tracks the last progress value emitted for a given
	// `progressToken`. The MCP spec requires `progress` values to be strictly
	// monotonically increasing, so we silently drop any non-increasing call
	// rather than send an out-of-order notification on the wire.
	progress_seen map[string]f64
}

struct ServerState {
mut:
	sessions map[string]Session
	// response_signals carries one semaphore per in-flight server-initiated
	// request, keyed by `pending_signal_key(session_id, request_id)`.
	// `wait_for_response` blocks on it (with a deadline) and
	// `deliver_response` / `delete_session` post it so we never burn CPU
	// polling for a response that may take seconds to arrive. Kept outside
	// `Session` to avoid V's pointer-in-shared-map access warnings.
	response_signals map[string]&sync.Semaphore
}

fn pending_signal_key(session_id string, request_id string) string {
	return '${session_id}\x00${request_id}'
}

struct DispatchResult {
	has_response bool
	response     string
	session_id   string
}

struct HandledRequest {
	response   Response
	session_id string
}

struct ProtocolError {
	response_error ResponseError
	request_id     string
}

fn (err ProtocolError) msg() string {
	return err.response_error.message
}

fn (err ProtocolError) code() int {
	return err.response_error.code
}

struct CursorParams {
	cursor string
}

struct ToolCallParams {
	name      string
	arguments string @[raw]
}

struct ReadResourceParams {
	uri string
}

struct SubscribeParams {
	uri string
}

struct GetPromptParams {
	name      string
	arguments string @[raw]
}

struct ListToolsResult {
	tools       []Tool
	next_cursor string @[json: nextCursor; omitempty]
}

struct ListResourcesResult {
	resources   []Resource
	next_cursor string @[json: nextCursor; omitempty]
}

struct ListResourceTemplatesResult {
	resource_templates []ResourceTemplate @[json: resourceTemplates]
	next_cursor        string             @[json: nextCursor; omitempty]
}

struct ListPromptsResult {
	prompts     []Prompt
	next_cursor string @[json: nextCursor; omitempty]
}

const default_tool_input_schema = '{"type":"object","additionalProperties":false}'

// Server handles MCP protocol requests for stdio and HTTP transports.
@[heap]
pub struct Server {
mut:
	server_info           Implementation
	protocol_version      string
	capabilities_override string
	instructions          string
	http_path             string
	allowed_origins       []string
	enable_logging        bool
	http_server           &http.Server = unsafe { nil }
	tools                 map[string]RegisteredTool
	tool_names            []string
	resources             map[string]RegisteredResource
	resource_uris         []string
	resource_templates    map[string]ResourceTemplate
	resource_template_ids []string
	prompts               map[string]RegisteredPrompt
	prompt_names          []string
	completions           map[string]RegisteredCompletion
	state                 shared ServerState
}

// new_server constructs a new MCP server.
pub fn new_server(config ServerConfig) Server {
	return Server{
		server_info:           normalize_server_info(config)
		protocol_version:      normalize_protocol_version(config.protocol_version)
		capabilities_override: config.capabilities.trim_space()
		instructions:          config.instructions
		http_path:             normalize_http_path(config.http_path)
		allowed_origins:       config.allowed_origins.clone()
		enable_logging:        config.enable_logging
		tools:                 map[string]RegisteredTool{}
		resources:             map[string]RegisteredResource{}
		resource_templates:    map[string]ResourceTemplate{}
		prompts:               map[string]RegisteredPrompt{}
		completions:           map[string]RegisteredCompletion{}
		state:                 ServerState{}
	}
}

// add_tool registers a tool and its handler.
pub fn (mut s Server) add_tool(tool Tool, handler ToolHandler) ! {
	validate_tool_name(tool.name)!
	if tool.name in s.tools {
		return error('mcp.Server.add_tool: duplicate tool `${tool.name}`')
	}
	normalized := Tool{
		name:          tool.name
		title:         tool.title
		description:   tool.description
		input_schema:  normalize_tool_input_schema(tool.input_schema)
		output_schema: tool.output_schema.trim_space()
		annotations:   tool.annotations
		icons:         tool.icons
		execution:     tool.execution
	}
	s.tools[tool.name] = RegisteredTool{
		tool:    normalized
		handler: handler
	}
	s.tool_names << tool.name
	s.broadcast_notification('notifications/tools/list_changed', empty_object.str())
}

// add_resource registers a concrete resource and its read handler.
pub fn (mut s Server) add_resource(resource Resource, handler ResourceHandler) ! {
	if resource.uri.trim_space() == '' {
		return error('mcp.Server.add_resource: empty uri')
	}
	if resource.uri in s.resources {
		return error('mcp.Server.add_resource: duplicate resource `${resource.uri}`')
	}
	s.resources[resource.uri] = RegisteredResource{
		resource: resource
		handler:  handler
	}
	s.resource_uris << resource.uri
	s.broadcast_notification('notifications/resources/list_changed', empty_object.str())
}

// add_resource_template registers a resource template exposed by `resources/templates/list`.
pub fn (mut s Server) add_resource_template(template ResourceTemplate) ! {
	if template.uri_template.trim_space() == '' {
		return error('mcp.Server.add_resource_template: empty uri template')
	}
	if template.uri_template in s.resource_templates {
		return error('mcp.Server.add_resource_template: duplicate resource template `${template.uri_template}`')
	}
	s.resource_templates[template.uri_template] = template
	s.resource_template_ids << template.uri_template
	s.broadcast_notification('notifications/resources/list_changed', empty_object.str())
}

// add_prompt registers a prompt and its handler.
pub fn (mut s Server) add_prompt(prompt Prompt, handler PromptHandler) ! {
	if prompt.name.trim_space() == '' {
		return error('mcp.Server.add_prompt: empty prompt name')
	}
	if prompt.name in s.prompts {
		return error('mcp.Server.add_prompt: duplicate prompt `${prompt.name}`')
	}
	s.prompts[prompt.name] = RegisteredPrompt{
		prompt:  prompt
		handler: handler
	}
	s.prompt_names << prompt.name
	s.broadcast_notification('notifications/prompts/list_changed', empty_object.str())
}

// add_completion registers a completion handler for one argument of a prompt
// or resource template. `argument` is the argument name being completed.
pub fn (mut s Server) add_completion(ref CompletionRef, argument string, handler CompletionHandler) ! {
	if argument.trim_space() == '' {
		return error('mcp.Server.add_completion: empty argument name')
	}
	key := completion_key(ref, argument) or {
		return error('mcp.Server.add_completion: ${err.msg()}')
	}
	if key in s.completions {
		return error('mcp.Server.add_completion: duplicate completion `${key}`')
	}
	s.completions[key] = RegisteredCompletion{
		ref:      ref
		argument: argument
		handler:  handler
	}
}

fn completion_key(ref CompletionRef, argument string) !string {
	match ref.ref_type {
		'ref/prompt' {
			if ref.name.trim_space() == '' {
				return error('ref/prompt requires a `name`')
			}
			return 'prompt|${ref.name}|${argument}'
		}
		'ref/resource' {
			if ref.uri.trim_space() == '' {
				return error('ref/resource requires a `uri`')
			}
			return 'resource|${ref.uri}|${argument}'
		}
		else {
			return error('unsupported ref type `${ref.ref_type}`')
		}
	}
}

// notify_tools_list_changed broadcasts the tool catalog change notification.
pub fn (mut s Server) notify_tools_list_changed() {
	s.broadcast_notification('notifications/tools/list_changed', empty_object.str())
}

// notify_resources_list_changed broadcasts the resource catalog change notification.
pub fn (mut s Server) notify_resources_list_changed() {
	s.broadcast_notification('notifications/resources/list_changed', empty_object.str())
}

// notify_prompts_list_changed broadcasts the prompt catalog change notification.
pub fn (mut s Server) notify_prompts_list_changed() {
	s.broadcast_notification('notifications/prompts/list_changed', empty_object.str())
}

// notify_log emits a `notifications/message` payload at `level` filtered per
// session by the most recent `logging/setLevel` call. `data_json` MUST be a
// valid JSON value (object preferred per spec). `logger` is optional.
pub fn (mut s Server) notify_log(level LogLevel, logger string, data_json string) {
	if !s.enable_logging {
		return
	}
	mut fields := ['"level":${json.encode(level.str())}']
	if logger != '' {
		fields << '"logger":${json.encode(logger)}'
	}
	payload := if data_json.trim_space() == '' { 'null' } else { data_json.trim_space() }
	fields << '"data":${payload}'
	params := '{${fields.join(',')}}'
	message := build_notification_message('notifications/message', params)
	lock s.state {
		for id in s.state.sessions.keys() {
			mut session := s.state.sessions[id]
			if !session.initialized {
				continue
			}
			if session.log_level_set && int(level) < int(session.log_level) {
				continue
			}
			session.notification_queue << message
			s.state.sessions[id] = session
		}
	}
}

// list_roots issues `roots/list` to the session and waits for the response.
pub fn (mut s Server) list_roots(session_id string, timeout time.Duration) !ListRootsResult {
	response := s.send_server_request(session_id, 'roots/list', empty_object.str(), timeout)!
	return response.decode_result[ListRootsResult]()
}

// sample issues `sampling/createMessage` and waits for the LLM result.
pub fn (mut s Server) sample(session_id string, params CreateMessageParams, timeout time.Duration) !CreateMessageResult {
	encoded := json.encode(params)
	response := s.send_server_request(session_id, 'sampling/createMessage', encoded, timeout)!
	return response.decode_result[CreateMessageResult]()
}

// elicit issues `elicitation/create` and waits for the user-supplied content.
pub fn (mut s Server) elicit(session_id string, params ElicitParams, timeout time.Duration) !ElicitResult {
	encoded := encode_elicit_params(params)
	response := s.send_server_request(session_id, 'elicitation/create', encoded, timeout)!
	return response.decode_result[ElicitResult]()
}

fn encode_elicit_params(params ElicitParams) string {
	mut fields := []string{}
	if params.mode != '' {
		fields << '"mode":${json.encode(params.mode)}'
	}
	fields << '"message":${json.encode(params.message)}'
	if params.url != '' {
		fields << '"url":${json.encode(params.url)}'
	}
	if params.elicitation_id != '' {
		fields << '"elicitationId":${json.encode(params.elicitation_id)}'
	}
	if params.requested_schema.properties.trim_space() != ''
		|| params.requested_schema.required.len > 0 {
		fields << '"requestedSchema":${encode_elicit_schema(params.requested_schema)}'
	}
	return '{${fields.join(',')}}'
}

fn encode_elicit_schema(schema ElicitSchema) string {
	type_value := if schema.type_.trim_space() == '' { 'object' } else { schema.type_ }
	mut fields := ['"type":${json.encode(type_value)}']
	if schema.properties.trim_space() != '' {
		fields << '"properties":${schema.properties.trim_space()}'
	}
	if schema.required.len > 0 {
		fields << '"required":[${schema.required.map(json.encode(it)).join(',')}]'
	}
	return '{${fields.join(',')}}'
}

fn (mut s Server) send_server_request(session_id string, method string, params_json string, timeout time.Duration) !Response {
	request_id_quoted := s.allocate_server_request_id(session_id) or {
		return error('mcp.Server.send_server_request: unknown session `${session_id}`')
	}
	encoded := Request{
		id:     request_id_quoted
		method: method
		params: params_json
	}.encode()
	lock s.state {
		if session_id in s.state.sessions {
			mut session := s.state.sessions[session_id]
			session.notification_queue << encoded
			s.state.sessions[session_id] = session
		}
	}
	return s.wait_for_response(session_id, request_id_quoted, timeout)
}

fn (mut s Server) allocate_server_request_id(session_id string) ?string {
	mut request_id := ''
	lock s.state {
		if session_id in s.state.sessions {
			mut session := s.state.sessions[session_id]
			request_id = '"server-${session.next_request_seq}"'
			session.next_request_seq++
			s.state.sessions[session_id] = session
			s.state.response_signals[pending_signal_key(session_id, request_id)] =
				sync.new_semaphore()
		}
	}
	if request_id == '' {
		return none
	}
	return request_id
}

fn (mut s Server) deliver_response(session_id string, response Response) {
	key := pending_signal_key(session_id, response.id)
	mut signal := unsafe { &sync.Semaphore(nil) }
	lock s.state {
		// Only stash the reply if a waiter is still listening on this id.
		// `wait_for_response` removes its semaphore entry on timeout, so a
		// missing signal means the request has been abandoned and any late
		// reply must be dropped instead of accumulating in `pending_responses`.
		signal = s.state.response_signals[key] or { unsafe { nil } }
		if !isnil(signal) && session_id in s.state.sessions {
			mut session := s.state.sessions[session_id]
			session.pending_responses[response.id] = response
			s.state.sessions[session_id] = session
		}
	}
	if !isnil(signal) {
		signal.post()
	}
}

fn (mut s Server) wait_for_response(session_id string, request_id string, timeout time.Duration) !Response {
	key := pending_signal_key(session_id, request_id)
	mut signal := unsafe { &sync.Semaphore(nil) }
	rlock s.state {
		signal = s.state.response_signals[key] or { unsafe { nil } }
	}
	if isnil(signal) {
		return error('mcp.Server.wait_for_response: no pending request `${request_id}`')
	}
	signaled := signal.timed_wait(timeout)
	mut response := Response{}
	mut found := false
	lock s.state {
		if session_id in s.state.sessions {
			mut session := s.state.sessions[session_id]
			if request_id in session.pending_responses {
				response = session.pending_responses[request_id]
				session.pending_responses.delete(request_id)
				found = true
			}
			s.state.sessions[session_id] = session
		}
		s.state.response_signals.delete(key)
	}
	if found {
		return response
	}
	if !signaled {
		return error('mcp.Server.wait_for_response: timeout waiting for ${request_id}')
	}
	return error('mcp.Server.wait_for_response: session `${session_id}` closed while waiting for ${request_id}')
}

// notify_elicitation_complete emits a `notifications/elicitation/complete`
// notification for the session that initiated a URL-mode elicitation. Pass
// the same `elicitation_id` that was supplied to `elicit()` so the client
// can correlate the out-of-band interaction with its pending request.
pub fn (mut s Server) notify_elicitation_complete(session_id string, elicitation_id string) {
	params := '{"elicitationId":${json.encode(elicitation_id)}}'
	message := build_notification_message('notifications/elicitation/complete', params)
	lock s.state {
		if session_id in s.state.sessions {
			mut session := s.state.sessions[session_id]
			session.notification_queue << message
			s.state.sessions[session_id] = session
		}
	}
}

// notify_resource_updated emits notifications/resources/updated to every
// session that has subscribed to `uri`.
pub fn (mut s Server) notify_resource_updated(uri string) {
	params := '{"uri":${json.encode(uri)}}'
	message := build_notification_message('notifications/resources/updated', params)
	lock s.state {
		for id in s.state.sessions.keys() {
			mut session := s.state.sessions[id]
			if !session.initialized {
				continue
			}
			if uri !in session.subscribed_uris {
				continue
			}
			session.notification_queue << message
			s.state.sessions[id] = session
		}
	}
}

fn (mut s Server) subscribe(session_id string, uri string) {
	lock s.state {
		if session_id in s.state.sessions {
			mut session := s.state.sessions[session_id]
			if uri !in session.subscribed_uris {
				session.subscribed_uris << uri
				s.state.sessions[session_id] = session
			}
		}
	}
}

fn (mut s Server) unsubscribe(session_id string, uri string) {
	lock s.state {
		if session_id in s.state.sessions {
			mut session := s.state.sessions[session_id]
			session.subscribed_uris = session.subscribed_uris.filter(it != uri)
			s.state.sessions[session_id] = session
		}
	}
}

fn build_notification_message(method string, params_json string) string {
	return Notification{
		method: method
		params: params_json
	}.encode()
}

fn (mut s Server) broadcast_notification(method string, params_json string) {
	message := build_notification_message(method, params_json)
	lock s.state {
		for id in s.state.sessions.keys() {
			mut session := s.state.sessions[id]
			if !session.initialized {
				continue
			}
			session.notification_queue << message
			s.state.sessions[id] = session
		}
	}
}

fn (mut s Server) drain_session_notifications(session_id string) []string {
	mut messages := []string{}
	lock s.state {
		if session_id in s.state.sessions {
			mut session := s.state.sessions[session_id]
			messages = session.notification_queue.clone()
			session.notification_queue = []string{}
			s.state.sessions[session_id] = session
		}
	}
	return messages
}

// drain_to_event_log moves queued notifications into the session's bounded
// event log and returns the assigned (id, body) pairs ready for SSE framing.
fn (mut s Server) drain_to_event_log(session_id string) []LoggedEvent {
	mut events := []LoggedEvent{}
	lock s.state {
		if session_id in s.state.sessions {
			mut session := s.state.sessions[session_id]
			for body in session.notification_queue {
				event := LoggedEvent{
					id:   session.next_event_id
					body: body
				}
				session.next_event_id++
				session.event_log << event
				events << event
			}
			session.notification_queue = []string{}
			if session.event_log.len > event_log_capacity {
				session.event_log =
					session.event_log[session.event_log.len - event_log_capacity..].clone()
			}
			s.state.sessions[session_id] = session
		}
	}
	return events
}

fn (s &Server) replay_events_after(session_id string, last_event_id int) []LoggedEvent {
	mut events := []LoggedEvent{}
	rlock s.state {
		if session_id in s.state.sessions {
			session := s.state.sessions[session_id]
			for event in session.event_log {
				if event.id > last_event_id {
					events << event
				}
			}
		}
	}
	return events
}

// serve_stdio starts serving MCP messages over stdio using newline framing.
// MCP 2025-11-25 mandates that stdio messages are delimited by newlines and
// MUST NOT contain embedded newlines. We bypass libc stdio buffering on the
// way in (raw `read()` on fd 0) and flush stdout after every frame on the way
// out, so peers behind a pipe see responses immediately and the server
// reacts to each line as soon as it arrives.
pub fn (mut s Server) serve_stdio() ! {
	mut stdout := os.stdout()
	mut source := StdinReader{}
	mut sink := StdioWriter{
		file: &stdout
	}
	s.serve_stdio_transport(mut source, mut sink, stdio_session_id, .stdio)!
}

struct StdinReader {}

fn (mut r StdinReader) read(mut buf []u8) !int {
	if buf.len == 0 {
		return io.Eof{}
	}
	n := unsafe { C.read(0, buf.data, buf.len) }
	if n == 0 {
		return io.Eof{}
	}
	if n < 0 {
		return error('mcp.stdio: read(0) failed')
	}
	return int(n)
}

struct StdioWriter {
mut:
	file &os.File = unsafe { nil }
}

fn (mut w StdioWriter) write(buf []u8) !int {
	n := w.file.write(buf)!
	w.file.flush()
	return n
}

// serve_http starts serving MCP over a single HTTP endpoint.
pub fn (mut s Server) serve_http(addr string) ! {
	if addr.trim_space() == '' {
		return error('mcp.Server.serve_http: empty address')
	}
	mut handler := HttpHandler{
		server: s
	}
	mut http_server := &http.Server{
		addr:                 addr
		handler:              handler
		accept_timeout:       100 * time.millisecond
		show_startup_message: false
	}
	s.http_server = http_server
	http_server.listen_and_serve()
}

// close stops the HTTP server if it is running.
pub fn (mut s Server) close() {
	if !isnil(s.http_server) {
		s.http_server.close()
	}
}

// wait_till_running waits until the HTTP server transitions to the running state.
pub fn (mut s Server) wait_till_running(params http.WaitTillRunningParams) !int {
	mut retries := 0
	for isnil(s.http_server) && retries < params.max_retries {
		time.sleep(params.retry_period_ms * time.millisecond)
		retries++
	}
	if isnil(s.http_server) {
		return error('mcp.Server.wait_till_running: HTTP server is not running')
	}
	remaining_retries := if params.max_retries > retries { params.max_retries - retries } else { 0 }
	if remaining_retries == 0 {
		return error('mcp.Server.wait_till_running: HTTP server is not running')
	}
	retry_count := s.http_server.wait_till_running(
		max_retries:     remaining_retries
		retry_period_ms: params.retry_period_ms
	)!
	return retries + retry_count
}

// text_content creates a raw MCP text content item.
pub fn text_content(text string) string {
	return text_content_with_annotations(text, Annotations{})
}

// text_content_with_annotations behaves like `text_content` but attaches the
// provided `annotations` (audience, priority, lastModified) to the block.
pub fn text_content_with_annotations(text string, annotations Annotations) string {
	mut fields := ['"type":"text"', '"text":${json.encode(text)}']
	if encoded := encode_annotations(annotations) {
		fields << '"annotations":${encoded}'
	}
	return '{${fields.join(',')}}'
}

// image_content creates a raw MCP image content block. `data` must be the
// base64-encoded payload and `mime_type` the IANA media type (e.g. `image/png`).
pub fn image_content(data string, mime_type string) string {
	return image_content_with_annotations(data, mime_type, Annotations{})
}

// image_content_with_annotations behaves like `image_content` but attaches
// the provided `annotations` to the block.
pub fn image_content_with_annotations(data string, mime_type string, annotations Annotations) string {
	mut fields := ['"type":"image"', '"data":${json.encode(data)}',
		'"mimeType":${json.encode(mime_type)}']
	if encoded := encode_annotations(annotations) {
		fields << '"annotations":${encoded}'
	}
	return '{${fields.join(',')}}'
}

// audio_content creates a raw MCP audio content block. `data` must be the
// base64-encoded payload and `mime_type` the IANA media type (e.g. `audio/wav`).
pub fn audio_content(data string, mime_type string) string {
	return audio_content_with_annotations(data, mime_type, Annotations{})
}

// audio_content_with_annotations behaves like `audio_content` but attaches
// the provided `annotations` to the block.
pub fn audio_content_with_annotations(data string, mime_type string, annotations Annotations) string {
	mut fields := ['"type":"audio"', '"data":${json.encode(data)}',
		'"mimeType":${json.encode(mime_type)}']
	if encoded := encode_annotations(annotations) {
		fields << '"annotations":${encoded}'
	}
	return '{${fields.join(',')}}'
}

// resource_link_content creates a raw MCP `resource_link` content block from
// a `Resource` description. The block carries the resource's metadata —
// including any `annotations` set on the resource — so the host can render
// or read it later.
pub fn resource_link_content(resource Resource) string {
	encoded := json.encode(resource)
	if encoded.len <= 2 {
		return '{"type":"resource_link"}'
	}
	return '{"type":"resource_link",${encoded[1..]}'
}

// embedded_text_resource creates an embedded MCP text resource content block.
pub fn embedded_text_resource(uri string, mime_type string, text string) string {
	return embedded_text_resource_with_annotations(uri, mime_type, text, Annotations{})
}

// embedded_text_resource_with_annotations behaves like `embedded_text_resource`
// but attaches the provided `annotations` to the outer `EmbeddedResource`.
pub fn embedded_text_resource_with_annotations(uri string, mime_type string, text string, annotations Annotations) string {
	inner := encode_resource_text_payload(uri, mime_type, text)
	return wrap_embedded_resource(inner, annotations)
}

// embedded_blob_resource creates an embedded MCP binary resource content
// block. `blob` must be the base64-encoded payload.
pub fn embedded_blob_resource(uri string, mime_type string, blob string) string {
	return embedded_blob_resource_with_annotations(uri, mime_type, blob, Annotations{})
}

// embedded_blob_resource_with_annotations behaves like `embedded_blob_resource`
// but attaches the provided `annotations` to the outer `EmbeddedResource`.
pub fn embedded_blob_resource_with_annotations(uri string, mime_type string, blob string, annotations Annotations) string {
	inner := encode_resource_blob_payload(uri, mime_type, blob)
	return wrap_embedded_resource(inner, annotations)
}

fn encode_resource_text_payload(uri string, mime_type string, text string) string {
	encoded_uri := json.encode(uri)
	encoded_text := json.encode(text)
	if mime_type == '' {
		return '{"uri":${encoded_uri},"text":${encoded_text}}'
	}
	encoded_mime := json.encode(mime_type)
	return '{"uri":${encoded_uri},"mimeType":${encoded_mime},"text":${encoded_text}}'
}

fn encode_resource_blob_payload(uri string, mime_type string, blob string) string {
	encoded_uri := json.encode(uri)
	encoded_blob := json.encode(blob)
	if mime_type == '' {
		return '{"uri":${encoded_uri},"blob":${encoded_blob}}'
	}
	encoded_mime := json.encode(mime_type)
	return '{"uri":${encoded_uri},"mimeType":${encoded_mime},"blob":${encoded_blob}}'
}

fn wrap_embedded_resource(inner string, annotations Annotations) string {
	mut fields := ['"type":"resource"', '"resource":${inner}']
	if encoded := encode_annotations(annotations) {
		fields << '"annotations":${encoded}'
	}
	return '{${fields.join(',')}}'
}

// encode_annotations renders an `Annotations` value as JSON, omitting the
// whole object when no field is populated (returns `none`).
fn encode_annotations(annotations Annotations) ?string {
	mut fields := []string{}
	if annotations.audience.len > 0 {
		fields << '"audience":[${annotations.audience.map(json.encode(it)).join(',')}]'
	}
	if priority := annotations.priority {
		fields << '"priority":${format_number(priority)}'
	}
	if annotations.last_modified != '' {
		fields << '"lastModified":${json.encode(annotations.last_modified)}'
	}
	if fields.len == 0 {
		return none
	}
	return '{${fields.join(',')}}'
}

// prompt_text_message creates a prompt message with text content.
pub fn prompt_text_message(role string, text string) PromptMessage {
	return PromptMessage{
		role:    role
		content: text_content(text)
	}
}

// tool_text_result wraps plain text in an MCP tool result.
pub fn tool_text_result(text string) ToolResult {
	return ToolResult{
		content: '[${text_content(text)}]'
	}
}

fn tool_error_result(text string) ToolResult {
	return ToolResult{
		content:  '[${text_content(text)}]'
		is_error: true
	}
}

fn response_with_json(request_id string, result_json string) Response {
	return Response{
		id:     request_id
		result: result_json
	}
}

fn encode_initialize_result(result InitializeResult) string {
	mut fields := [
		'"protocolVersion":${json.encode(result.protocol_version)}',
		'"capabilities":${normalize_capabilities(result.capabilities)}',
		'"serverInfo":${json.encode(result.server_info)}',
	]
	if result.instructions != '' {
		fields << '"instructions":${json.encode(result.instructions)}'
	}
	return '{${fields.join(',')}}'
}

fn encode_tools_list_result(result ListToolsResult) string {
	mut fields := ['"tools":[${result.tools.map(encode_tool).join(',')}]']
	if result.next_cursor != '' {
		fields << '"nextCursor":${json.encode(result.next_cursor)}'
	}
	return '{${fields.join(',')}}'
}

fn encode_tool(tool Tool) string {
	mut fields := ['"name":${json.encode(tool.name)}',
		'"inputSchema":${normalize_tool_input_schema(tool.input_schema)}']
	if tool.title != '' {
		fields << '"title":${json.encode(tool.title)}'
	}
	if tool.description != '' {
		fields << '"description":${json.encode(tool.description)}'
	}
	if tool.output_schema.trim_space() != '' {
		fields << '"outputSchema":${tool.output_schema.trim_space()}'
	}
	if encoded_annotations := encode_tool_annotations(tool.annotations) {
		fields << '"annotations":${encoded_annotations}'
	}
	if tool.icons.len > 0 {
		fields << '"icons":${json.encode(tool.icons)}'
	}
	if tool.execution.task_support != '' {
		fields << '"execution":{"taskSupport":${json.encode(tool.execution.task_support)}}'
	}
	return '{${fields.join(',')}}'
}

fn encode_tool_annotations(annotations ToolAnnotations) ?string {
	mut fields := []string{}
	if annotations.title != '' {
		fields << '"title":${json.encode(annotations.title)}'
	}
	if hint := annotations.read_only_hint {
		fields << '"readOnlyHint":${hint.str()}'
	}
	if hint := annotations.destructive_hint {
		fields << '"destructiveHint":${hint.str()}'
	}
	if hint := annotations.idempotent_hint {
		fields << '"idempotentHint":${hint.str()}'
	}
	if hint := annotations.open_world_hint {
		fields << '"openWorldHint":${hint.str()}'
	}
	if fields.len == 0 {
		return none
	}
	return '{${fields.join(',')}}'
}

fn encode_tool_result(result ToolResult) string {
	// `content` is REQUIRED by the 2025-11-25 schema (CallToolResult.content),
	// so always emit at least an empty array; clients reading the result MUST
	// be able to find `content` whether or not the tool produced output.
	content_payload := if result.content.trim_space() == '' {
		'[]'
	} else {
		result.content.trim_space()
	}
	mut fields := ['"content":${content_payload}', '"isError":${result.is_error.str()}']
	if result.structured_content.trim_space() != '' {
		fields << '"structuredContent":${result.structured_content.trim_space()}'
	}
	return '{${fields.join(',')}}'
}

fn encode_prompt_result(result GetPromptResult) string {
	mut fields := [
		'"messages":[${result.messages.map(encode_prompt_message).join(',')}]',
	]
	if result.description != '' {
		fields << '"description":${json.encode(result.description)}'
	}
	return '{${fields.join(',')}}'
}

fn encode_prompt_message(message PromptMessage) string {
	return '{"role":${json.encode(message.role)},"content":${message.content}}'
}

fn (mut s Server) serve_stdio_transport(mut reader io.Reader, mut writer io.Writer, session_id string, transport SessionTransport) ! {
	mut buffer := ''
	for {
		frame := try_extract_stdio_message(buffer) or {
			if err.msg() != NoFrameError{}.msg() {
				error_response := Response{
					error: normalize_response_error(err)
				}.encode()
				writer.write(encode_stdio_message(error_response).bytes())!
				return err
			}
			FrameExtraction{}
		}
		if frame.message.len != 0 {
			buffer = frame.remaining
			dispatch_result := s.dispatch_message(frame.message, session_id, transport) or {
				error_response := Response{
					error: normalize_response_error(err)
				}.encode()
				writer.write(encode_stdio_message(error_response).bytes())!
				continue
			}
			if dispatch_result.has_response {
				writer.write(encode_stdio_message(dispatch_result.response).bytes())!
			}
			for notification in s.drain_session_notifications(session_id) {
				writer.write(encode_stdio_message(notification).bytes())!
			}
			continue
		}
		mut chunk := []u8{len: 4096}
		bytes_read := reader.read(mut chunk) or {
			if err is os.Eof {
				return
			}
			if err is io.Eof {
				return
			}
			return err
		}
		if bytes_read == 0 {
			return
		}
		buffer += chunk[..bytes_read].bytestr()
	}
}

fn (mut s Server) dispatch_message(raw string, session_id string, transport SessionTransport) !DispatchResult {
	trimmed := raw.trim_space()
	if trimmed.len == 0 || trimmed[0] == `[` {
		return ProtocolError{
			response_error: invalid_request
		}
	}
	envelope := decode_envelope(trimmed) or {
		return ProtocolError{
			response_error: parse_error
		}
	}
	return s.dispatch_envelope(envelope, session_id, transport)
}

fn (mut s Server) dispatch_envelope(envelope MessageEnvelope, session_id string, transport SessionTransport) !DispatchResult {
	if envelope.method.len == 0 {
		if !is_notification_id(envelope.id) {
			s.deliver_response(session_id, Response{
				id:     envelope.id
				result: envelope.result
				error:  envelope.error
			})
		}
		return DispatchResult{}
	}
	if is_notification_id(envelope.id) {
		s.handle_notification(Notification{
			method: envelope.method
			params: envelope.params
		}, session_id, transport)!
		return DispatchResult{}
	}
	req := Request{
		id:     envelope.id
		method: envelope.method
		params: envelope.params
	}
	handled := s.handle_request(req, session_id, transport)
	return DispatchResult{
		has_response: true
		response:     handled.response.encode()
		session_id:   handled.session_id
	}
}

fn (mut s Server) handle_request(req Request, session_id string, transport SessionTransport) HandledRequest {
	defer {
		s.clear_cancelled(session_id, req.id)
	}
	return s.handle_request_impl(req, session_id, transport) or {
		return HandledRequest{
			response: error_response_for(req.id, err)
		}
	}
}

fn (mut s Server) handle_request_impl(req Request, session_id string, transport SessionTransport) !HandledRequest {
	if req.method == 'ping' {
		return HandledRequest{
			response: response_with_json(req.id, empty_object.str())
		}
	}
	if req.method == 'initialize' {
		return s.handle_initialize(req, session_id, transport)
	}
	session := s.session_for_request(session_id, transport) or {
		return ProtocolError{
			response_error: server_not_initialized
			request_id:     req.id
		}
	}
	if !session.initialize_complete || !session.initialized {
		return ProtocolError{
			response_error: server_not_initialized
			request_id:     req.id
		}
	}
	ctx := s.context_from_session(req, session)
	match req.method {
		'tools/list' {
			return s.handle_tools_list(req)
		}
		'tools/call' {
			return s.handle_tools_call(req, ctx)
		}
		'resources/list' {
			return s.handle_resources_list(req)
		}
		'resources/read' {
			return s.handle_resources_read(req, ctx)
		}
		'resources/templates/list' {
			return s.handle_resource_templates_list(req)
		}
		'resources/subscribe' {
			return s.handle_resources_subscribe(req, session_id)
		}
		'resources/unsubscribe' {
			return s.handle_resources_unsubscribe(req, session_id)
		}
		'logging/setLevel' {
			return s.handle_logging_set_level(req, session_id)
		}
		'completion/complete' {
			return s.handle_completion_complete(req, ctx)
		}
		'prompts/list' {
			return s.handle_prompts_list(req)
		}
		'prompts/get' {
			return s.handle_prompts_get(req, ctx)
		}
		else {
			return ProtocolError{
				response_error: method_not_found
				request_id:     req.id
			}
		}
	}
}

fn (mut s Server) handle_initialize(req Request, session_id string, transport SessionTransport) !HandledRequest {
	params := req.decode_params[InitializeParams]() or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	mut session := s.ensure_session_for_initialize(session_id, transport)
	if session.initialize_complete {
		return ProtocolError{
			response_error: invalid_request
			request_id:     req.id
		}
	}
	// Per MCP lifecycle: server replies with the protocol version it supports;
	// the client decides whether to proceed or disconnect on a mismatch.
	session.protocol_version = s.protocol_version
	session.client_info = normalize_client_info(params.client_info)
	session.client_capabilities = normalize_capabilities(params.capabilities)
	session.initialize_complete = true
	session.initialized = false
	s.store_session(session)
	result := InitializeResult{
		protocol_version: session.protocol_version
		capabilities:     s.capabilities_json()
		server_info:      s.server_info
		instructions:     s.instructions
	}
	return HandledRequest{
		response:   response_with_json(req.id, encode_initialize_result(result))
		session_id: if transport == .http { session.id } else { '' }
	}
}

fn (mut s Server) handle_tools_list(req Request) !HandledRequest {
	params := decode_optional_params[CursorParams](req.params) or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	start, end, next_cursor := paginate_bounds(s.tool_names.len, params.cursor)!
	mut tools := []Tool{cap: end - start}
	for name in s.tool_names[start..end] {
		tools << s.tools[name].tool
	}
	return HandledRequest{
		response: response_with_json(req.id, encode_tools_list_result(ListToolsResult{
			tools:       tools
			next_cursor: next_cursor
		}))
	}
}

fn (mut s Server) handle_tools_call(req Request, ctx Context) !HandledRequest {
	params := decode_optional_params[ToolCallParams](req.params) or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	if params.name !in s.tools {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	entry := s.tools[params.name]
	result := entry.handler(ctx, json_object_or_empty(params.arguments)) or {
		if err is ProtocolError {
			return err
		}
		if err is ResponseError {
			return err
		}
		return HandledRequest{
			response: response_with_json(req.id, encode_tool_result(tool_error_result(err.msg())))
		}
	}
	return HandledRequest{
		response: response_with_json(req.id, encode_tool_result(result))
	}
}

fn (mut s Server) handle_resources_list(req Request) !HandledRequest {
	params := decode_optional_params[CursorParams](req.params) or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	start, end, next_cursor := paginate_bounds(s.resource_uris.len, params.cursor)!
	mut resources := []Resource{cap: end - start}
	for uri in s.resource_uris[start..end] {
		resources << s.resources[uri].resource
	}
	return HandledRequest{
		response: response_with_json(req.id, encode_value(ListResourcesResult{
			resources:   resources
			next_cursor: next_cursor
		}))
	}
}

fn (mut s Server) handle_resources_read(req Request, ctx Context) !HandledRequest {
	params := req.decode_params[ReadResourceParams]() or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	if params.uri !in s.resources {
		return ProtocolError{
			response_error: ResponseError{
				code:    resource_not_found.code
				message: 'Resource not found.'
				data:    '{"uri":${json.encode(params.uri)}}'
			}
			request_id:     req.id
		}
	}
	entry := s.resources[params.uri]
	result := entry.handler(ctx, params.uri) or { return err }
	return HandledRequest{
		response: response_with_json(req.id, encode_value(result))
	}
}

struct SetLevelParams {
	level string
}

struct CompletionRequestArgument {
	name  string
	value string
}

struct CompletionRequestContext {
	arguments string @[raw]
}

struct CompletionRequestParams {
	ref      CompletionRef
	argument CompletionRequestArgument
	context  CompletionRequestContext
}

fn (mut s Server) handle_completion_complete(req Request, ctx Context) !HandledRequest {
	if s.completions.len == 0 {
		return ProtocolError{
			response_error: method_not_found
			request_id:     req.id
		}
	}
	params := req.decode_params[CompletionRequestParams]() or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	key := completion_key(params.ref, params.argument.name) or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	entry := s.completions[key] or {
		return HandledRequest{
			response: response_with_json(req.id, encode_completion_result(CompletionResult{}))
		}
	}
	arguments_json := json_object_or_empty(params.context.arguments)
	result := entry.handler(ctx, params.argument.value, arguments_json) or {
		if err is ProtocolError {
			return err
		}
		if err is ResponseError {
			return err
		}
		return ProtocolError{
			response_error: ResponseError{
				code:    internal_error.code
				message: err.msg()
			}
			request_id:     req.id
		}
	}
	return HandledRequest{
		response: response_with_json(req.id, encode_completion_result(result))
	}
}

fn encode_completion_result(result CompletionResult) string {
	// MCP 2025-11-25 caps completion responses at 100 values. We clamp here
	// and surface the truncation through `hasMore: true` so clients keep
	// requesting refinements instead of silently losing options.
	clamped := if result.values.len > completion_values_limit {
		result.values[..completion_values_limit]
	} else {
		result.values
	}
	mut completion_fields := [
		'"values":[${clamped.map(json.encode(it)).join(',')}]',
	]
	if total := result.total {
		completion_fields << '"total":${total}'
	}
	mut has_more_value := result.values.len > completion_values_limit
	if explicit := result.has_more {
		has_more_value = explicit || has_more_value
	}
	if result.has_more != none || has_more_value {
		completion_fields << '"hasMore":${has_more_value.str()}'
	}
	return '{"completion":{${completion_fields.join(',')}}}'
}

fn (mut s Server) handle_logging_set_level(req Request, session_id string) !HandledRequest {
	if !s.enable_logging {
		return ProtocolError{
			response_error: method_not_found
			request_id:     req.id
		}
	}
	params := req.decode_params[SetLevelParams]() or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	level := parse_log_level(params.level) or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	s.set_session_log_level(session_id, level)
	return HandledRequest{
		response: response_with_json(req.id, empty_object.str())
	}
}

fn (mut s Server) set_session_log_level(session_id string, level LogLevel) {
	lock s.state {
		if session_id in s.state.sessions {
			mut session := s.state.sessions[session_id]
			session.log_level = level
			session.log_level_set = true
			s.state.sessions[session_id] = session
		}
	}
}

fn (mut s Server) handle_resources_subscribe(req Request, session_id string) !HandledRequest {
	params := req.decode_params[SubscribeParams]() or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	if params.uri.trim_space() == '' {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	s.subscribe(session_id, params.uri)
	return HandledRequest{
		response: response_with_json(req.id, empty_object.str())
	}
}

fn (mut s Server) handle_resources_unsubscribe(req Request, session_id string) !HandledRequest {
	params := req.decode_params[SubscribeParams]() or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	s.unsubscribe(session_id, params.uri)
	return HandledRequest{
		response: response_with_json(req.id, empty_object.str())
	}
}

fn (mut s Server) handle_resource_templates_list(req Request) !HandledRequest {
	params := decode_optional_params[CursorParams](req.params) or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	start, end, next_cursor := paginate_bounds(s.resource_template_ids.len, params.cursor)!
	mut templates := []ResourceTemplate{cap: end - start}
	for uri_template in s.resource_template_ids[start..end] {
		templates << s.resource_templates[uri_template]
	}
	return HandledRequest{
		response: response_with_json(req.id, encode_value(ListResourceTemplatesResult{
			resource_templates: templates
			next_cursor:        next_cursor
		}))
	}
}

fn (mut s Server) handle_prompts_list(req Request) !HandledRequest {
	params := decode_optional_params[CursorParams](req.params) or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	start, end, next_cursor := paginate_bounds(s.prompt_names.len, params.cursor)!
	mut prompts := []Prompt{cap: end - start}
	for name in s.prompt_names[start..end] {
		prompts << s.prompts[name].prompt
	}
	return HandledRequest{
		response: response_with_json(req.id, encode_value(ListPromptsResult{
			prompts:     prompts
			next_cursor: next_cursor
		}))
	}
}

fn (mut s Server) handle_prompts_get(req Request, ctx Context) !HandledRequest {
	params := decode_optional_params[GetPromptParams](req.params) or {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	if params.name !in s.prompts {
		return ProtocolError{
			response_error: invalid_params
			request_id:     req.id
		}
	}
	entry := s.prompts[params.name]
	result := entry.handler(ctx, json_object_or_empty(params.arguments)) or { return err }
	return HandledRequest{
		response: response_with_json(req.id, encode_prompt_result(result))
	}
}

fn (mut s Server) handle_notification(notification Notification, session_id string, transport SessionTransport) ! {
	match notification.method {
		'notifications/initialized' {
			mut session := s.session_for_request(session_id, transport) or {
				return ProtocolError{
					response_error: server_not_initialized
				}
			}
			if !session.initialize_complete {
				return ProtocolError{
					response_error: server_not_initialized
				}
			}
			session.initialized = true
			s.store_session(session)
		}
		'notifications/cancelled' {
			params := notification.decode_params[CancelledParams]() or { return }
			s.mark_cancelled(session_id, params.request_id)
		}
		else {}
	}
}

struct CancelledParams {
	request_id string @[json: requestId; raw]
	reason     string @[omitempty]
}

fn (mut s Server) mark_cancelled(session_id string, request_id string) {
	id := request_id.trim_space()
	if id.len == 0 {
		return
	}
	lock s.state {
		if session_id in s.state.sessions {
			mut session := s.state.sessions[session_id]
			if id !in session.cancelled_requests {
				session.cancelled_requests << id
				s.state.sessions[session_id] = session
			}
		}
	}
}

fn (s &Server) is_request_cancelled(session_id string, request_id string) bool {
	mut cancelled := false
	rlock s.state {
		if session_id in s.state.sessions {
			cancelled = request_id in s.state.sessions[session_id].cancelled_requests
		}
	}
	return cancelled
}

fn (mut s Server) clear_cancelled(session_id string, request_id string) {
	lock s.state {
		if session_id in s.state.sessions {
			mut session := s.state.sessions[session_id]
			session.cancelled_requests = session.cancelled_requests.filter(it != request_id)
			s.state.sessions[session_id] = session
		}
	}
}

fn (s &Server) notify_progress_for(session_id string, progress_token string, progress f64, total f64, message string) {
	mut fields := [
		'"progressToken":${progress_token}',
		'"progress":${format_number(progress)}',
	]
	if total != 0 {
		fields << '"total":${format_number(total)}'
	}
	if message != '' {
		fields << '"message":${json.encode(message)}'
	}
	params := '{${fields.join(',')}}'
	notification := build_notification_message('notifications/progress', params)
	lock s.state {
		if session_id !in s.state.sessions {
			return
		}
		mut session := s.state.sessions[session_id]
		// MCP 2025-11-25 mandates `progress` strictly increases per token.
		if last := session.progress_seen[progress_token] {
			if progress <= last {
				return
			}
		}
		session.progress_seen[progress_token] = progress
		session.notification_queue << notification
		s.state.sessions[session_id] = session
	}
}

fn format_number(value f64) string {
	if value == f64(i64(value)) {
		return i64(value).str()
	}
	return value.str()
}

fn (s &Server) capabilities_json() string {
	if s.capabilities_override != '' {
		return normalize_capabilities(s.capabilities_override)
	}
	mut parts := []string{}
	if s.tool_names.len != 0 {
		parts << '"tools":{"listChanged":true}'
	}
	if s.resource_uris.len != 0 || s.resource_template_ids.len != 0 {
		parts << '"resources":{"listChanged":true,"subscribe":true}'
	}
	if s.prompt_names.len != 0 {
		parts << '"prompts":{"listChanged":true}'
	}
	if s.enable_logging {
		parts << '"logging":{}'
	}
	if s.completions.len != 0 {
		parts << '"completions":{}'
	}
	return '{${parts.join(',')}}'
}

fn (mut s Server) ensure_session_for_initialize(session_id string, transport SessionTransport) Session {
	if transport == .stdio {
		return s.ensure_session(stdio_session_id, .stdio)
	}
	if session_id != '' {
		return s.ensure_session(session_id, .http)
	}
	return s.create_http_session()
}

fn (mut s Server) ensure_session(session_id string, transport SessionTransport) Session {
	if session := s.get_session(session_id) {
		return session
	}
	session := Session{
		id:               session_id
		transport:        transport
		protocol_version: s.protocol_version
	}
	s.store_session(session)
	return session
}

fn (mut s Server) create_http_session() Session {
	for {
		session_id := rand.uuid_v7()
		if !s.session_exists(session_id) {
			session := Session{
				id:               session_id
				transport:        .http
				protocol_version: s.protocol_version
			}
			s.store_session(session)
			return session
		}
	}
	return Session{}
}

fn (s &Server) session_for_request(session_id string, transport SessionTransport) ?Session {
	if transport == .stdio {
		return s.get_session(stdio_session_id)
	}
	if session_id == '' {
		return none
	}
	return s.get_session(session_id)
}

fn (mut s Server) store_session(session Session) {
	lock s.state {
		s.state.sessions[session.id] = session
	}
}

fn (s &Server) get_session(session_id string) ?Session {
	mut session := Session{}
	mut found := false
	rlock s.state {
		if session_id in s.state.sessions {
			session = s.state.sessions[session_id]
			found = true
		}
	}
	if !found {
		return none
	}
	return session
}

fn (s &Server) session_exists(session_id string) bool {
	mut found := false
	rlock s.state {
		found = session_id in s.state.sessions
	}
	return found
}

fn (mut s Server) delete_session(session_id string) bool {
	mut deleted := false
	mut orphaned_signals := []&sync.Semaphore{}
	prefix := pending_signal_key(session_id, '')
	lock s.state {
		if session_id in s.state.sessions {
			s.state.sessions.delete(session_id)
			deleted = true
		}
		for key, signal in s.state.response_signals {
			if key.starts_with(prefix) {
				orphaned_signals << signal
			}
		}
	}
	for mut signal in orphaned_signals {
		signal.post()
	}
	return deleted
}

fn (s &Server) context_from_session(req Request, session Session) Context {
	return Context{
		session_id:          session.id
		request_id:          req.id
		method:              req.method
		transport:           session.transport
		protocol_version:    session.protocol_version
		client_info:         session.client_info
		client_capabilities: session.client_capabilities
		progress_token:      extract_progress_token(req.params)
		server:              unsafe { s }
	}
}

// MetaParams wraps the optional MCP `_meta` request field for JSON decoding.
pub struct MetaParams {
pub:
	meta MetaPayload @[json: '_meta']
}

// MetaPayload contains metadata extracted from an MCP request.
pub struct MetaPayload {
pub:
	progress_token string @[json: progressToken; raw]
}

fn extract_progress_token(params string) string {
	trimmed := params.trim_space()
	if trimmed.len == 0 || trimmed == null.str() {
		return ''
	}
	wrapper := json.decode[MetaParams](trimmed) or { return '' }
	return wrapper.meta.progress_token.trim_space()
}

fn decode_optional_params[T](raw string) !T {
	trimmed := raw.trim_space()
	if trimmed.len == 0 || trimmed == null.str() {
		return T{}
	}
	return decode_value[T](trimmed)
}

fn json_object_or_empty(raw string) string {
	trimmed := raw.trim_space()
	if trimmed.len == 0 || trimmed == null.str() {
		return '{}'
	}
	return trimmed
}

fn paginate_bounds(total int, cursor string) !(int, int, string) {
	mut start := 0
	if cursor.trim_space() != '' {
		start = cursor.int()
		if start < 0 || start > total || (start == 0 && cursor != '0') {
			return ProtocolError{
				response_error: invalid_params
			}
		}
	}
	mut end := start + default_list_page_size
	if end > total {
		end = total
	}
	next_cursor := if end < total { end.str() } else { '' }
	return start, end, next_cursor
}

fn validate_tool_name(name string) ! {
	trimmed := name.trim_space()
	if trimmed.len == 0 || trimmed.len > 128 {
		return error('mcp.Server.add_tool: invalid tool name `${name}`')
	}
	for ch in trimmed {
		if ch.is_letter() || ch.is_digit() || ch in [`_`, `-`, `.`] {
			continue
		}
		return error('mcp.Server.add_tool: invalid tool name `${name}`')
	}
}

fn normalize_tool_input_schema(input_schema string) string {
	trimmed := input_schema.trim_space()
	return if trimmed.len == 0 { default_tool_input_schema } else { trimmed }
}

fn normalize_server_info(config ServerConfig) Implementation {
	return Implementation{
		name:        if config.name.trim_space() == '' {
			default_server_name
		} else {
			config.name.trim_space()
		}
		version:     if config.version.trim_space() == '' {
			default_server_version
		} else {
			config.version.trim_space()
		}
		title:       config.title.trim_space()
		description: config.description.trim_space()
		website_url: config.website_url.trim_space()
		icons:       config.icons.clone()
	}
}

fn normalize_http_path(path string) string {
	trimmed := path.trim_space()
	if trimmed == '' {
		return default_http_path
	}
	return if trimmed.starts_with('/') { trimmed } else { '/' + trimmed }
}

fn error_response_for(request_id string, err IError) Response {
	mut response_id := request_id
	response_error := normalize_response_error(err)
	if err is ProtocolError && err.request_id != '' {
		response_id = err.request_id
	}
	return Response{
		id:    response_id
		error: response_error
	}
}

fn normalize_response_error(err IError) ResponseError {
	if err is ProtocolError {
		return err.response_error
	}
	if err is ResponseError {
		return err
	}
	return ResponseError{
		code:    internal_error.code
		message: err.msg()
	}
}

fn build_sse_response(message string) string {
	mut lines := []string{}
	for line in message.split('\n') {
		lines << 'data: ${line}'
	}
	return lines.join('\n') + '\n\n'
}

fn build_sse_event(event LoggedEvent) string {
	mut lines := ['id: ${event.id}']
	for line in event.body.split('\n') {
		lines << 'data: ${line}'
	}
	return lines.join('\n') + '\n\n'
}

fn build_sse_stream(events []LoggedEvent) string {
	mut chunks := []string{cap: events.len}
	for event in events {
		chunks << build_sse_event(event)
	}
	return chunks.join('')
}

// accept_modes describes which response Content-Types the client tolerates.
struct AcceptModes {
	json bool
	sse  bool
}

// parse_accept extracts JSON/SSE acceptance from a comma-separated Accept header.
// `*/*` and `application/*` / `text/*` wildcards count as accepting the matching type.
fn parse_accept(header http.Header) AcceptModes {
	accept := header.get(.accept) or { '' }
	if accept.trim_space() == '' {
		return AcceptModes{
			json: true
			sse:  true
		}
	}
	mut modes := AcceptModes{}
	for raw in accept.split(',') {
		entry := raw.split(';')[0].trim_space().to_lower()
		match entry {
			'*/*', '' { modes = AcceptModes{true, true} }
			'application/json', 'application/*' { modes = AcceptModes{true, modes.sse} }
			'text/event-stream', 'text/*' { modes = AcceptModes{modes.json, true} }
			else {}
		}
	}
	return modes
}

// origin_is_allowed enforces the spec MUST: validate Origin to prevent DNS rebinding.
fn (s &Server) origin_is_allowed(origin string) bool {
	if origin == '' {
		return true
	}
	for allowed in s.allowed_origins {
		if allowed == '*' || allowed == origin {
			return true
		}
	}
	if s.allowed_origins.len != 0 {
		return false
	}
	return is_loopback_origin(origin)
}

fn is_loopback_origin(origin string) bool {
	if origin == 'null' {
		return true
	}
	mut without_scheme := origin
	for prefix in ['http://', 'https://'] {
		if origin.starts_with(prefix) {
			without_scheme = origin[prefix.len..]
			break
		}
	}
	host := without_scheme.all_before('/').all_before(':').to_lower()
	bracketed := without_scheme.all_before('/')
	return host in ['localhost', '127.0.0.1', '::1'] || bracketed.starts_with('[::1]')
}

// supports_protocol_version applies spec rules: missing header defaults to
// 2025-03-26 (back-compat), otherwise the value MUST match the negotiated one.
fn (s &Server) supports_protocol_version(value string, has_session bool) bool {
	if value == '' {
		return !has_session || s.protocol_version == default_protocol_version
			|| s.protocol_version == protocol_version
	}
	return value == s.protocol_version
}

fn json_http_response(status http.Status, body string, content_type string) http.Response {
	mut header := http.new_header()
	if content_type != '' {
		header.set(.content_type, content_type)
	}
	mut response := http.Response{
		body:   body
		header: header
	}
	response.set_status(status)
	return response
}

fn error_http_response(status http.Status, err ResponseError) http.Response {
	body := Response{
		error: err
	}.encode()
	return json_http_response(status, body, default_content_type)
}

struct HttpHandler {
mut:
	server &Server = unsafe { nil }
}

fn (mut h HttpHandler) handle(req http.Request) http.Response {
	return h.server.handle_http_request(req)
}

fn (mut s Server) handle_http_get(req http.Request, session_id string) http.Response {
	if session_id == '' {
		return error_http_response(.bad_request, ResponseError{
			code:    invalid_request.code
			message: 'GET requires an active MCP-Session-Id.'
		})
	}
	if !s.session_exists(session_id) {
		return json_http_response(.not_found, '', '')
	}
	accept := parse_accept(req.header)
	if !accept.sse {
		return error_http_response(.not_acceptable, ResponseError{
			code:    invalid_request.code
			message: 'GET requires Accept: text/event-stream.'
		})
	}
	last_event_id_text := req.header.get_custom(last_event_id_header) or { '' }
	last_event_id := last_event_id_text.int()
	mut events := []LoggedEvent{}
	if last_event_id_text != '' {
		// Flush anything generated while the client was disconnected into the
		// event log first, so the replay covers the whole gap. Without this
		// step the resume only sees what was already drained before the drop,
		// which defeats the point of `Last-Event-ID`.
		s.drain_to_event_log(session_id)
		events = s.replay_events_after(session_id, last_event_id)
	} else {
		events = s.drain_to_event_log(session_id)
	}
	body := build_sse_stream(events)
	mut response := json_http_response(.ok, body, event_stream_content_type)
	response.header.set_custom(mcp_session_id_header, session_id) or {}
	return response
}

fn (mut s Server) handle_http_request(req http.Request) http.Response {
	if req.url.all_before('?') != s.http_path {
		return json_http_response(.not_found, '', '')
	}
	origin := req.header.get_custom('Origin') or { '' }
	if !s.origin_is_allowed(origin) {
		return error_http_response(.forbidden, ResponseError{
			code:    invalid_request.code
			message: 'Origin not allowed.'
		})
	}
	session_id := req.header.get_custom(mcp_session_id_header) or { '' }
	match req.method {
		.delete {
			if session_id == '' {
				return json_http_response(.bad_request, '', '')
			}
			if !s.delete_session(session_id) {
				return json_http_response(.not_found, '', '')
			}
			return json_http_response(.ok, '', '')
		}
		.get {
			return s.handle_http_get(req, session_id)
		}
		.post {}
		else {
			return json_http_response(.method_not_allowed, '', '')
		}
	}

	protocol_value := req.header.get_custom(mcp_protocol_version_header) or { '' }
	if !s.supports_protocol_version(protocol_value, session_id != '') {
		return error_http_response(.bad_request, ResponseError{
			code:    invalid_request.code
			message: 'Unsupported MCP-Protocol-Version `${protocol_value}`.'
		})
	}

	accept := parse_accept(req.header)
	if !accept.json && !accept.sse {
		return error_http_response(.not_acceptable, ResponseError{
			code:    invalid_request.code
			message: 'Accept must include application/json or text/event-stream.'
		})
	}

	trimmed := req.data.trim_space()
	if trimmed.len == 0 || trimmed[0] == `[` {
		return error_http_response(.bad_request, invalid_request)
	}
	envelope := decode_envelope(trimmed) or {
		return error_http_response(.bad_request, parse_error)
	}
	if session_id != '' && !s.session_exists(session_id) {
		return json_http_response(.not_found, '', '')
	}
	if envelope.method != 'initialize' && envelope.method.len != 0 && session_id == '' {
		return error_http_response(.bad_request, server_not_initialized)
	}
	dispatch_result := s.dispatch_envelope(envelope, session_id, .http) or {
		return error_http_response(.bad_request, normalize_response_error(err))
	}
	if !dispatch_result.has_response {
		return json_http_response(.accepted, '', '')
	}
	use_sse := accept.sse && !accept.json
	mut body := ''
	mut content_type := default_content_type
	if use_sse {
		notifications := s.drain_to_event_log(session_for_drain(session_id,
			dispatch_result.session_id))
		body = build_sse_stream(notifications) + build_sse_response(dispatch_result.response)
		content_type = event_stream_content_type
	} else {
		body = dispatch_result.response
	}
	mut response := json_http_response(.ok, body, content_type)
	if dispatch_result.session_id != '' {
		response.header.set_custom(mcp_session_id_header, dispatch_result.session_id) or {}
	}
	return response
}

fn session_for_drain(request_session string, dispatch_session string) string {
	if request_session != '' {
		return request_session
	}
	return dispatch_session
}
