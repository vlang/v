module mcp

import json
import io
import net.http
import os
import rand
import time

const default_server_name = 'v.mcp.server'
const default_server_version = 'dev'
const default_http_path = '/mcp'
const default_list_page_size = 50
const stdio_session_id = 'stdio'
const event_stream_content_type = 'text/event-stream'

// SessionTransport identifies how an MCP session is connected.
pub enum SessionTransport {
	stdio
	http
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
}

// Tool describes an MCP tool exposed by the server.
pub struct Tool {
pub:
	name          string
	title         string @[omitempty]
	description   string @[omitempty]
	input_schema  string = default_tool_input_schema @[json: inputSchema; raw]
	output_schema string @[json: outputSchema; omitempty; raw]
}

// Resource describes a concrete MCP resource exposed by the server.
pub struct Resource {
pub:
	uri         string
	name        string
	title       string @[omitempty]
	description string @[omitempty]
	mime_type   string @[json: mimeType; omitempty]
}

// ResourceTemplate describes a parameterized MCP resource URI template.
pub struct ResourceTemplate {
pub:
	uri_template string @[json: uriTemplate]
	name         string
	title        string @[omitempty]
	description  string @[omitempty]
	mime_type    string @[json: mimeType; omitempty]
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

// ServerConfig configures an MCP server instance.
@[params]
pub struct ServerConfig {
pub:
	name             string
	version          string
	protocol_version string = protocol_version
	capabilities     string
	instructions     string
	http_path        string = default_http_path
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

struct Session {
mut:
	id                  string
	transport           SessionTransport
	protocol_version    string
	client_info         Implementation
	client_capabilities string
	initialize_complete bool
	initialized         bool
}

struct ServerState {
mut:
	sessions map[string]Session
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
	http_server           &http.Server = unsafe { nil }
	tools                 map[string]RegisteredTool
	tool_names            []string
	resources             map[string]RegisteredResource
	resource_uris         []string
	resource_templates    map[string]ResourceTemplate
	resource_template_ids []string
	prompts               map[string]RegisteredPrompt
	prompt_names          []string
	state                 shared ServerState
}

// new_server constructs a new MCP server.
pub fn new_server(config ServerConfig) Server {
	return Server{
		server_info:           normalize_server_info(config.name, config.version)
		protocol_version:      normalize_protocol_version(config.protocol_version)
		capabilities_override: config.capabilities.trim_space()
		instructions:          config.instructions
		http_path:             normalize_http_path(config.http_path)
		tools:                 map[string]RegisteredTool{}
		resources:             map[string]RegisteredResource{}
		resource_templates:    map[string]ResourceTemplate{}
		prompts:               map[string]RegisteredPrompt{}
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
	}
	s.tools[tool.name] = RegisteredTool{
		tool:    normalized
		handler: handler
	}
	s.tool_names << tool.name
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
}

// serve_stdio starts serving MCP messages over stdio using Content-Length framing.
pub fn (mut s Server) serve_stdio() ! {
	mut stdin := os.stdin()
	mut stdout := os.stdout()
	s.serve_framed_transport(mut stdin, mut stdout, stdio_session_id, .stdio)!
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
	if isnil(s.http_server) {
		return error('mcp.Server.wait_till_running: HTTP server is not running')
	}
	return s.http_server.wait_till_running(params)
}

// text_content creates a raw MCP text content item.
pub fn text_content(text string) string {
	return '{"type":"text","text":${json.encode(text)}}'
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
	return '{${fields.join(',')}}'
}

fn encode_tool_result(result ToolResult) string {
	mut fields := ['"isError":${result.is_error.str()}']
	if result.content.trim_space() != '' {
		fields << '"content":${result.content.trim_space()}'
	}
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

fn (mut s Server) serve_framed_transport(mut reader io.Reader, mut writer io.Writer, session_id string, transport SessionTransport) ! {
	mut buffer := ''
	for {
		frame := try_extract_framed_message(buffer) or {
			if err.msg() != NoFrameError{}.msg() {
				error_response := Response{
					error: normalize_response_error(err)
				}.encode()
				writer.write(encode_framed_message(error_response).bytes())!
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
				writer.write(encode_framed_message(error_response).bytes())!
				continue
			}
			if dispatch_result.has_response {
				writer.write(encode_framed_message(dispatch_result.response).bytes())!
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
		return DispatchResult{}
	}
	if envelope.id.len == 0 || envelope.id == null.str() {
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

	return ProtocolError{
		response_error: method_not_found
		request_id:     req.id
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
	session.protocol_version = if params.protocol_version == s.protocol_version {
		s.protocol_version
	} else {
		s.protocol_version
	}
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
			response_error: invalid_params
			request_id:     req.id
		}
	}
	entry := s.resources[params.uri]
	result := entry.handler(ctx, params.uri) or { return err }
	return HandledRequest{
		response: response_with_json(req.id, encode_value(result))
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
		'notifications/cancelled' {}
		else {}
	}
}

fn (s &Server) capabilities_json() string {
	if s.capabilities_override != '' {
		return normalize_capabilities(s.capabilities_override)
	}
	mut parts := []string{}
	if s.tool_names.len != 0 {
		parts << '"tools":{}'
	}
	if s.resource_uris.len != 0 || s.resource_template_ids.len != 0 {
		parts << '"resources":{}'
	}
	if s.prompt_names.len != 0 {
		parts << '"prompts":{}'
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
	lock s.state {
		if session_id in s.state.sessions {
			s.state.sessions.delete(session_id)
			deleted = true
		}
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
	}
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

fn normalize_server_info(name string, version string) Implementation {
	return Implementation{
		name:    if name.trim_space() == '' { default_server_name } else { name.trim_space() }
		version: if version.trim_space() == '' {
			default_server_version
		} else {
			version.trim_space()
		}
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

fn accepts_event_stream_only(header http.Header) bool {
	accept := header.get(.accept) or { '' }
	if accept == '' {
		return false
	}
	accept_lower := accept.to_lower()
	return accept_lower.contains(event_stream_content_type)
		&& !accept_lower.contains(default_content_type)
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

struct HttpHandler {
mut:
	server &Server = unsafe { nil }
}

fn (mut h HttpHandler) handle(req http.Request) http.Response {
	return h.server.handle_http_request(req)
}

fn (mut s Server) handle_http_request(req http.Request) http.Response {
	if req.url.all_before('?') != s.http_path {
		return json_http_response(.not_found, '', '')
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
			return json_http_response(.method_not_allowed, '', '')
		}
		.post {}
		else {
			return json_http_response(.method_not_allowed, '', '')
		}
	}

	trimmed := req.data.trim_space()
	if trimmed.len == 0 || trimmed[0] == `[` {
		return json_http_response(.bad_request, Response{
			error: invalid_request
		}.encode(), default_content_type)
	}
	envelope := decode_envelope(trimmed) or {
		return json_http_response(.bad_request, Response{
			error: parse_error
		}.encode(), default_content_type)
	}
	if session_id != '' && !s.session_exists(session_id) {
		return json_http_response(.not_found, '', '')
	}
	if envelope.method != 'initialize' && envelope.method.len != 0 && session_id == '' {
		return json_http_response(.bad_request, Response{
			error: server_not_initialized
		}.encode(), default_content_type)
	}
	dispatch_result := s.dispatch_envelope(envelope, session_id, .http) or {
		return json_http_response(.bad_request, Response{
			error: normalize_response_error(err)
		}.encode(), default_content_type)
	}
	if !dispatch_result.has_response {
		return json_http_response(.accepted, '', '')
	}
	body := if accepts_event_stream_only(req.header) {
		build_sse_response(dispatch_result.response)
	} else {
		dispatch_result.response
	}
	content_type := if accepts_event_stream_only(req.header) {
		event_stream_content_type
	} else {
		default_content_type
	}
	mut response := json_http_response(.ok, body, content_type)
	if dispatch_result.session_id != '' {
		response.header.set_custom(mcp_session_id_header, dispatch_result.session_id) or {}
	}
	return response
}
