// The catalogue of tools this server exposes.
//
// One list, declared once, drives three things: what `tools/list` advertises,
// what `v mcp tools` prints, and which tools are skipped in `--read-only` mode.
// A tool that is not in this list is not registered, so the catalogue is the
// whole public surface.
module main

import mcp
import v.astjson

// ToolSpec is one tool's declaration, with everything needed to register it.
//
// The handler is stored in a function pointer that takes the workspace, so the
// catalogue stays a plain data structure that can be walked, printed and tested
// without an MCP server.
pub struct ToolSpec {
pub:
	tool mcp.Tool
	// handler is always set by `read_only_spec` and `writing_spec`; the nil
	// default exists only so the struct can be built field by field.
	handler ToolHandler = unsafe { nil }
}

// ToolHandler is a tool implementation: it receives the shared workspace, the
// tool name for error messages, and the raw JSON arguments.
pub type ToolHandler = fn (ws &Workspace, arguments string) string

// read_only says a tool cannot write a file or change the project, so
// `--read-only` may keep it.
pub fn (spec ToolSpec) read_only() bool {
	return spec.tool.annotations.read_only_hint or { false }
}

// tool_specs returns every tool, in the order they are advertised.
//
// Read-only tools come first and the tools that change files come last, so the
// listing an agent reads starts with the safe ones.
pub fn tool_specs() []ToolSpec {
	mut specs := []ToolSpec{}
	// Project shape.
	specs << spec_project_info()
	specs << spec_modules()
	specs << spec_files()
	// Code shape.
	specs << spec_ast()
	specs << spec_symbols()
	specs << spec_symbol_at()
	specs << spec_references()
	specs << spec_stdlib_doc()
	// Does it build.
	specs << spec_check()
	specs << spec_test_run()
	// Environment and skills.
	specs << spec_doctor()
	specs << spec_veb_routes()
	specs << spec_skills()
	// Running code.
	specs << spec_run()
	specs << spec_eval()
	// Changing files. These are skipped in `--read-only` mode.
	specs << spec_edit_replace()
	specs << spec_rename_symbol()
	specs << spec_format()
	return specs
}

// register_all registers every tool the workspace permits.
pub fn register_all(mut server mcp.Server, ws &Workspace) ! {
	mut registered := 0
	for spec in tool_specs() {
		if ws.read_only && !spec.read_only() {
			continue
		}
		handler := spec.handler
		name := spec.tool.name
		server.add_tool(spec.tool, fn [ws, handler, name] (ctx mcp.Context,
			arguments string) !mcp.ToolResult {
			if ctx.is_cancelled() {
				return mcp.tool_text_result('${name} was cancelled before it started')
			}
			return mcp.tool_text_result(handler(ws, arguments))
		})!
		registered++
	}
	if registered == 0 {
		return error('v mcp: no tools registered')
	}
}

// spec builds a read-only tool declaration.
fn read_only_spec(name string, description string, schema string,
	handler ToolHandler) ToolSpec {
	return ToolSpec{
		tool:    mcp.Tool{
			name:         name
			title:        name
			description:  description
			input_schema: schema
			annotations:  mcp.ToolAnnotations{
				read_only_hint:  true
				idempotent_hint: true
				open_world_hint: false
			}
		}
		handler: handler
	}
}

// spec builds a tool that changes the project. `--read-only` never registers it,
// and the annotation lets a client warn before it runs.
fn writing_spec(name string, description string, schema string,
	handler ToolHandler) ToolSpec {
	return ToolSpec{
		tool:    mcp.Tool{
			name:         name
			title:        name
			description:  description
			input_schema: schema
			annotations:  mcp.ToolAnnotations{
				read_only_hint:   false
				destructive_hint: true
				idempotent_hint:  false
				open_world_hint:  false
			}
		}
		handler: handler
	}
}

// no_args is the schema of a tool that takes nothing.
const no_args = '{"type":"object","additionalProperties":false,"properties":{}}'

// one_string is the schema of a tool that takes exactly one string property.
fn one_string(name string, description string) string {
	return input_schema([name], {
		name: SchemaProperty{
			kind:        'string'
			description: description
		}
	})
}

// SchemaProperty describes one tool argument. Array arguments contain strings.
struct SchemaProperty {
	kind        string
	description string
}

// input_schema renders tool argument schemas through the JSON writer so string
// values, including quotes and multiline descriptions, cannot break tools/list.
fn input_schema(required []string, properties map[string]SchemaProperty) string {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('type')
	w.string('object')
	w.key('additionalProperties')
	w.boolean(false)
	w.key('required')
	w.begin_array()
	for name in required {
		w.array_item()
		w.string(name)
	}
	w.end_array()
	w.key('properties')
	w.begin_object()
	for name, property in properties {
		w.key(name)
		w.begin_object()
		w.key('type')
		w.string(property.kind)
		w.key('description')
		w.string(property.description)
		if property.kind == 'array' {
			w.key('items')
			w.begin_object()
			w.key('type')
			w.string('string')
			w.end_object()
		}
		w.end_object()
	}
	w.end_object()
	w.end_object()
	return w.str()
}
