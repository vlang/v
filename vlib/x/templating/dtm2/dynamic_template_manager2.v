module dtm2

import os
import json
import strings

// dtm2 is the modern runtime renderer for Dynamic Template Manager.
// It keeps DTM dynamic, meaning templates can still be edited on disk without
// recompiling the application, while moving the hot path to a parsed-template
// cache instead of a rendered-output cache. File extensions are configurable,
// but every template ultimately renders in either HTML mode or text mode.
//
// The intended runtime model is one long-lived Manager per application or
// rendering context. Tests and benchmarks may create short-lived managers, but
// normal applications should reuse a manager so parsed templates and resolved
// paths stay hot.
const message_signature = '[Dynamic Template Manager 2]'
const internal_server_error = 'Internal Server Error'
const include_html_key_suffix = '_#includehtml'
const max_include_depth = 32
const max_extension_config_size = 64 * 1024
const default_extension_config_filename = 'dtm2_extensions.json'
const segment_instruction_size = 9
const segment_kind_text = u8(0)
const segment_kind_placeholder = u8(1)

// These tags are the compatibility allow-list for the historical `_#includehtml`
// placeholder suffix. Any other tag is still escaped.
const allowed_html_tags = ['<div>', '</div>', '<h1>', '</h1>', '<h2>', '</h2>', '<h3>', '</h3>',
	'<h4>', '</h4>', '<h5>', '</h5>', '<h6>', '</h6>', '<p>', '</p>', '<br>', '<hr>', '<span>',
	'</span>', '<ul>', '</ul>', '<ol>', '</ol>', '<li>', '</li>', '<dl>', '</dl>', '<dt>', '</dt>',
	'<dd>', '</dd>', '<menu>', '</menu>', '<table>', '</table>', '<caption>', '</caption>', '<th>',
	'</th>', '<tr>', '</tr>', '<td>', '</td>', '<thead>', '</thead>', '<thread>', '</thread>',
	'<tbody>', '</tbody>', '<tfoot>', '</tfoot>', '<col>', '</col>', '<colgroup>', '</colgroup>',
	'<header>', '</header>', '<footer>', '</footer>', '<main>', '</main>', '<section>', '</section>',
	'<article>', '</article>', '<aside>', '</aside>', '<details>', '</details>', '<dialog>',
	'</dialog>', '<data>', '</data>', '<summary>', '</summary>']!

pub enum TemplateType {
	html
	text
}

// ExtensionConfig is the JSON representation accepted by
// `ManagerParams.extension_config_file`.
//
// Example:
//
// ```json
// {
//   "html": [".html", ".htm", ".xml", ".view"],
//   "text": [".txt", ".mail"]
// }
// ```
pub struct ExtensionConfig {
pub:
	html []string
	text []string
}

// TemplateDependency records the file metadata that decides whether a parsed
// template is still fresh. Includes are tracked as dependencies of the parent
// template, so editing a partial can invalidate the compiled parent tree.
struct TemplateDependency {
	path        string
	modified_at i64
	size        u64
}

// CompiledTemplate is the parsed representation stored by Manager.
//
// It intentionally stores a compact instruction string instead of slices of
// segment structs. The instructions contain offsets into the owned `content`
// string, which keeps the cache compact and avoids keeping fragile string-slice
// graphs alive across many -prod renders.
@[heap]
struct CompiledTemplate {
	// Rendering mode inferred from the source file extension.
	template_type TemplateType
	// Full template content after static @include expansion.
	content string
	// Compact metadata used to decide whether this compiled tree is stale.
	dependency_signature string
	// Binary instruction stream: one fixed-size record per text/placeholder segment.
	instructions string
	// Number of text and placeholder records in `instructions`.
	segment_count int
	// Static-text size hint used to preallocate the render buffer.
	estimated_size int
}

// Manager owns the runtime state of a dtm2 renderer.
//
// It caches resolved template paths and parsed template trees, but never caches
// rendered HTML responses. This keeps the new engine deterministic and leaves
// legacy rendered-cache compatibility in `x.templating.dtm`.
@[heap]
pub struct Manager {
mut:
	// Base directory for relative template paths.
	template_dir string
	// Enables the lightweight deterministic HTML whitespace compressor.
	compress_html bool
	// When true, source and include files are stat-checked before cache reuse.
	reload_modified_templates bool
	// Maps caller-provided template paths to canonical source paths.
	resolved_template_paths map[string]string
	// Parsed-template cache keyed by canonical source path.
	compiled_templates map[string]&CompiledTemplate
	// Extension-to-render-mode table. Users can extend or override it through
	// ManagerParams without changing the parser or renderer.
	template_extensions map[string]TemplateType
}

// ManagerParams configure a dtm2 Manager.
@[params]
pub struct ManagerParams {
pub:
	// Root directory used when `expand()` receives a relative template path.
	// If empty, `<executable directory>/templates` is used.
	template_dir string
	// Compresses HTML output by removing newlines/tabs and redundant spaces.
	compress_html bool = true
	// Re-check source files and includes before reusing a parsed template tree.
	// Disable it for maximum hot-path throughput when templates are immutable.
	reload_modified_templates bool = true
	// Additional or overriding extension mappings.
	// Default mappings are: `.html`, `.htm`, `.xml` => HTML mode and
	// `.txt`, `.text` => text mode.
	template_extensions map[string]TemplateType
	// Optional JSON file containing extension mappings. It is merged after the
	// default mappings and before `template_extensions`, so explicit code
	// configuration wins over the file. If empty, DTM2 tries to load
	// `<template_dir>/dtm2_extensions.json` when that file exists.
	extension_config_file string
}

// RenderParams configure one render call.
@[params]
pub struct RenderParams {
pub:
	// Placeholder values keyed by their template name without the `@` prefix.
	// Values are escaped by default.
	placeholders &map[string]string = &map[string]string{}
	// Prefix written back when a placeholder is missing. The default preserves
	// the original `@placeholder` text.
	missing_placeholder_prefix string = '@'
}

// initialize creates a dynamic template manager rooted at `params.template_dir`.
//
// The returned manager should normally be kept and reused. Reusing it is what
// gives dtm2 its parsed-template and path-resolution cache benefits.
pub fn initialize(params ManagerParams) &Manager {
	raw_template_dir := if params.template_dir == '' {
		os.join_path(os.dir(os.executable()), 'templates')
	} else {
		params.template_dir
	}
	template_dir := canonical_template_dir(raw_template_dir)
	template_extensions := build_template_extensions(params, template_dir)
	return &Manager{
		template_dir:              template_dir.clone()
		compress_html:             params.compress_html
		reload_modified_templates: params.reload_modified_templates
		resolved_template_paths:   map[string]string{}
		compiled_templates:        map[string]&CompiledTemplate{}
		template_extensions:       template_extensions
	}
}

fn canonical_template_dir(template_dir string) string {
	return os.real_path(template_dir)
}

fn build_template_extensions(params ManagerParams, template_dir string) map[string]TemplateType {
	mut extensions := default_template_extensions()
	config_path := extension_config_path(params, template_dir)
	if config_path != '' {
		file_extensions := read_extension_config_file(config_path) or {
			eprintln('${message_signature} ${err.msg()}')
			map[string]TemplateType{}
		}
		merge_template_extensions(mut extensions, file_extensions)
	}
	merge_template_extensions(mut extensions, params.template_extensions)
	return extensions
}

fn extension_config_path(params ManagerParams, template_dir string) string {
	if params.extension_config_file != '' {
		return params.extension_config_file
	}
	default_path := os.join_path(template_dir, default_extension_config_filename)
	if os.exists(default_path) {
		return default_path
	}
	return ''
}

fn default_template_extensions() map[string]TemplateType {
	return {
		'.html': .html
		'.htm':  .html
		'.xml':  .html
		'.txt':  .text
		'.text': .text
	}
}

fn read_extension_config_file(config_path string) !map[string]TemplateType {
	if !os.exists(config_path) {
		return error('extension config file "${config_path}" not found')
	}
	if os.is_dir(config_path) {
		return error('extension config path "${config_path}" is a directory')
	}
	config_stat := os.stat(config_path)!
	if config_stat.size > u64(max_extension_config_size) {
		return error('extension config file "${config_path}" is larger than ${max_extension_config_size} bytes')
	}
	raw_config := os.read_file(config_path)!
	config := json.decode(ExtensionConfig, raw_config)!
	mut extensions := map[string]TemplateType{}
	merge_template_extension_list(mut extensions, config.html, .html)
	merge_template_extension_list(mut extensions, config.text, .text)
	return extensions
}

fn merge_template_extensions(mut target map[string]TemplateType, source map[string]TemplateType) {
	for ext, template_type in source {
		normalized := validate_template_extension(ext) or {
			eprintln('${message_signature} ${err.msg()}')
			continue
		}
		target[normalized] = template_type
	}
}

fn merge_template_extension_list(mut target map[string]TemplateType, extensions []string, template_type TemplateType) {
	for ext in extensions {
		normalized := validate_template_extension(ext) or {
			eprintln('${message_signature} ${err.msg()}')
			continue
		}
		target[normalized] = template_type
	}
}

fn normalize_template_extension(ext string) string {
	trimmed := ext.trim_space().to_lower()
	if trimmed == '' {
		return ''
	}
	if trimmed.starts_with('.') {
		return trimmed
	}
	return '.${trimmed}'
}

fn validate_template_extension(ext string) !string {
	normalized := normalize_template_extension(ext)
	if normalized.len < 2 {
		return error('ignoring invalid template extension "${ext}"')
	}
	for i := 1; i < normalized.len; i++ {
		c := normalized[i]
		if (c >= `a` && c <= `z`) || (c >= `0` && c <= `9`) || c == `_` || c == `-` || c == `.` {
			continue
		}
		return error('ignoring invalid template extension "${ext}"')
	}
	return normalized
}

// compiled_template_count is intentionally exposed for tests and diagnostics. In
// dtm2 it counts parsed template trees currently held by the manager.
pub fn (m &Manager) compiled_template_count() int {
	return m.compiled_templates.len
}

// expand renders `template_path` with the provided placeholders.
//
// `template_path` can be absolute or relative to the manager's template
// directory. Supported extensions come from the manager extension table.
// Errors are reported to stderr and return the legacy-compatible
// `Internal Server Error` string.
pub fn (mut m Manager) expand(template_path string, params RenderParams) string {
	source_path := m.cached_template_path(template_path) or {
		eprintln('${message_signature} ${err.msg()}')
		return internal_server_error
	}
	compiled := m.compiled_template_for_path(source_path) or {
		eprintln('${message_signature} ${err.msg()}')
		return internal_server_error
	}
	if m.compress_html && compiled.template_type == .html {
		rendered := render_compiled_template(compiled, params)
		compressed := compress_html_output(rendered)
		return compressed.clone()
	}
	rendered := render_compiled_template(compiled, params)
	return rendered.clone()
}

// cached_template_path resolves caller paths once and then reuses the canonical
// filesystem path. This removes repeated path normalization from the hot render
// loop while still keeping template content reload decisions separate.
fn (mut m Manager) cached_template_path(template_path string) !string {
	if source_path := m.resolved_template_paths[template_path] {
		return source_path
	}
	source_path := m.resolve_template_path(template_path)!
	m.resolved_template_paths[template_path] = source_path
	return source_path
}

// compiled_template_for_path returns the parsed tree for a canonical source
// path. When reload checks are enabled, dependency metadata decides whether the
// tree can be reused or must be rebuilt from disk.
fn (mut m Manager) compiled_template_for_path(source_path string) !&CompiledTemplate {
	if compiled := m.compiled_templates[source_path] {
		if !m.reload_modified_templates || compiled.dependencies_are_fresh() {
			return compiled
		}
	}
	compiled := compile_template_from_file(source_path, m.template_dir, m.template_extensions)!
	m.compiled_templates[source_path] = compiled
	return compiled
}

// compile_template_from_file reads a template, expands static includes, parses
// placeholders, and stores dependency metadata for future invalidation.
fn compile_template_from_file(source_path string, template_root string, template_extensions map[string]TemplateType) !&CompiledTemplate {
	template_type := template_type_from_path(source_path, template_extensions)!
	raw_content, dependencies := read_template_with_includes(source_path, template_root, 0)!
	instructions, estimated_size, segment_count := parse_segments(raw_content)
	dependency_signature := encode_dependencies(dependencies)
	return &CompiledTemplate{
		template_type:        template_type
		content:              copy_string(raw_content)
		dependency_signature: copy_string(dependency_signature)
		instructions:         copy_string(instructions)
		segment_count:        segment_count
		estimated_size:       estimated_size
	}
}

// copy_string forces cached strings to own their memory. Cached templates live
// beyond the stack frame that built them, so the cache should not retain
// accidental views into temporary buffers.
fn copy_string(value string) string {
	mut bytes := []u8{len: value.len}
	for i := 0; i < value.len; i++ {
		bytes[i] = value[i]
	}
	copied := bytes.bytestr()
	return copied.clone()
}

// dependencies_are_fresh checks root and included files. Both modification time
// and file size are used so same-second rewrites still invalidate reliably.
fn (compiled &CompiledTemplate) dependencies_are_fresh() bool {
	signature := compiled.dependency_signature
	mut offset := 0
	for offset < signature.len {
		first_sep := index_byte_from(signature, `|`, offset) or { return false }
		second_sep := index_byte_from(signature, `|`, first_sep + 1) or { return false }
		line_end := index_byte_from(signature, `\n`, second_sep + 1) or { signature.len }
		size := signature[first_sep + 1..second_sep].i64()
		if size < 0 {
			return false
		}
		path := signature[second_sep + 1..line_end]
		stat := os.stat(path) or { return false }
		if stat.mtime != signature[offset..first_sep].i64() || stat.size != u64(size) {
			return false
		}
		offset = line_end + 1
	}
	return true
}

fn index_byte_from(value string, needle u8, start int) ?int {
	for i := start; i < value.len; i++ {
		if value[i] == needle {
			return i
		}
	}
	return none
}

fn encode_dependencies(dependencies []TemplateDependency) string {
	mut out := strings.new_builder(dependencies.len * 64)
	for dependency in dependencies {
		out.writeln('${dependency.modified_at}|${dependency.size}|${dependency.path}')
	}
	encoded := out.str()
	return encoded.clone()
}

// resolve_template_path converts absolute or manager-relative template names to
// canonical paths. The canonical path is used as the compiled-template cache key.
fn (m &Manager) resolve_template_path(template_path string) !string {
	source_path := if os.is_abs_path(template_path) {
		template_path
	} else {
		os.join_path(m.template_dir, template_path)
	}
	if !os.exists(source_path) {
		return error('template "${source_path}" not found')
	}
	canonical_path := os.real_path(source_path)
	ensure_path_inside_template_dir(canonical_path, m.template_dir)!
	return canonical_path
}

fn ensure_path_inside_template_dir(path string, template_root string) ! {
	root := path_without_trailing_separator(template_root)
	candidate := path_without_trailing_separator(path)
	if candidate == root || candidate.starts_with(root + os.path_separator) {
		return
	}
	return error('template "${path}" is outside template directory "${template_root}"')
}

fn path_without_trailing_separator(path string) string {
	if path.len > os.path_separator.len && path.ends_with(os.path_separator) {
		return path[..path.len - os.path_separator.len]
	}
	return path
}

// template_type_from_path keeps rendering rules extension-driven and explicit.
fn template_type_from_path(source_path string, template_extensions map[string]TemplateType) !TemplateType {
	ext := normalize_template_extension(os.file_ext(source_path))
	if template_type := template_extensions[ext] {
		return template_type
	}
	return error('template "${source_path}" uses unsupported extension "${ext}"')
}

// read_template_with_includes expands simple line-level `@include "path"`
// directives before parsing placeholders. Includes are part of the parsed tree
// and are tracked as dependencies for reload checks.
fn read_template_with_includes(source_path string, template_root string, depth int) !(string, []TemplateDependency) {
	if depth > max_include_depth {
		return error('maximum @include depth exceeded while reading "${source_path}"')
	}
	content := os.read_file(source_path)!
	base_dir := os.dir(source_path)
	mut out := strings.new_builder(content.len)
	mut dependencies := []TemplateDependency{cap: 4}
	dependencies << template_dependency(source_path)!
	lines := content.split_into_lines()
	for line in lines {
		expanded_line, line_dependencies := expand_include_directives(line, base_dir,
			template_root, depth)!
		out.write_string(expanded_line)
		out.write_u8(`\n`)
		dependencies << line_dependencies
	}
	rendered := out.str()
	return rendered.clone(), dependencies
}

fn template_dependency(source_path string) !TemplateDependency {
	stat := os.stat(source_path)!
	return TemplateDependency{
		path:        source_path.clone()
		modified_at: stat.mtime
		size:        stat.size
	}
}

fn expand_include_directives(line string, base_dir string, template_root string, depth int) !(string, []TemplateDependency) {
	mut out := strings.new_builder(line.len)
	mut dependencies := []TemplateDependency{}
	mut offset := 0
	for {
		rel_pos := line[offset..].index('@include ') or {
			out.write_string(line[offset..])
			break
		}
		pos := offset + rel_pos
		target, end_pos := include_target_from_line_at(line, pos) or {
			out.write_string(line[offset..pos + '@include '.len])
			offset = pos + '@include '.len
			continue
		}
		out.write_string(line[offset..pos])
		include_path := resolve_include_path(base_dir, target, template_root)!
		next_depth := depth + 1
		included, include_dependencies := read_template_with_includes(include_path, template_root,
			next_depth)!
		out.write_string(included.trim_right('\n'))
		dependencies << include_dependencies
		offset = end_pos
	}
	expanded := out.str()
	return expanded.clone(), dependencies
}

fn include_target_from_line_at(line string, pos int) ?(string, int) {
	mut cursor := pos + '@include '.len
	for cursor < line.len && line[cursor].is_space() {
		cursor++
	}
	if cursor >= line.len {
		return none
	}
	quote := line[cursor]
	if quote != `'` && quote != `"` {
		return none
	}
	start := cursor + 1
	for cursor = start; cursor < line.len; cursor++ {
		if line[cursor] == quote {
			return line[start..cursor], cursor + 1
		}
	}
	return none
}

fn resolve_include_path(base_dir string, include_target string, template_root string) !string {
	mut target := include_target
	if os.file_ext(target) == '' {
		target += '.html'
	}
	source_path := if os.is_abs_path(target) {
		target
	} else {
		os.join_path(base_dir, target)
	}
	if !os.exists(source_path) {
		return error('included template "${source_path}" not found')
	}
	include_path := os.real_path(source_path)
	ensure_path_inside_template_dir(include_path, template_root)!
	return include_path
}

// parse_segments converts template content to a compact instruction stream.
// Each instruction stores a segment kind plus offset/length into `content`.
// Rendering can then walk the stream without reparsing placeholder syntax.
fn parse_segments(content string) (string, int, int) {
	mut instructions := []u8{cap: segment_instruction_size * 16}
	mut text_start := 0
	mut estimated_size := 0
	mut segment_count := 0
	mut i := 0
	for i < content.len {
		if content[i] != `@` || i + 1 >= content.len || !is_placeholder_char(content[i + 1]) {
			i++
			continue
		}
		if i > text_start {
			append_segment_instruction(mut instructions, segment_kind_text, text_start,
				i - text_start)
			segment_count++
			estimated_size += i - text_start
		}
		mut end := i + 1
		for end < content.len && is_placeholder_char(content[end]) {
			end++
		}
		append_segment_instruction(mut instructions, segment_kind_placeholder, i + 1, end - i - 1)
		segment_count++
		i = end
		text_start = end
	}
	if text_start < content.len {
		append_segment_instruction(mut instructions, segment_kind_text, text_start,
			content.len - text_start)
		segment_count++
		estimated_size += content.len - text_start
	}
	encoded := instructions.bytestr()
	return encoded.clone(), estimated_size, segment_count
}

fn append_segment_instruction(mut instructions []u8, kind u8, start int, len int) {
	instructions << kind
	append_u32(mut instructions, start)
	append_u32(mut instructions, len)
}

fn append_u32(mut data []u8, value int) {
	encoded := u32(value)
	data << u8(encoded & 0xff)
	data << u8((encoded >> 8) & 0xff)
	data << u8((encoded >> 16) & 0xff)
	data << u8((encoded >> 24) & 0xff)
}

fn read_u32(data string, offset int) int {
	b0 := u32(data[offset])
	b1 := u32(data[offset + 1]) << 8
	b2 := u32(data[offset + 2]) << 16
	b3 := u32(data[offset + 3]) << 24
	value := b0 | b1 | b2 | b3
	return int(value)
}

fn is_placeholder_char(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_`
}

// render_compiled_template walks the parsed instruction stream and writes the
// final output. Placeholder values are rendered only for this call; the manager
// never stores caller data after `expand()` returns.
fn render_compiled_template(compiled &CompiledTemplate, params RenderParams) string {
	mut out := strings.new_builder(estimate_render_size(compiled))
	mut offset := 0
	for offset + segment_instruction_size <= compiled.instructions.len {
		kind := compiled.instructions[offset]
		start := read_u32(compiled.instructions, offset + 1)
		len := read_u32(compiled.instructions, offset + 5)
		offset += segment_instruction_size
		if kind == segment_kind_text {
			text := compiled.segment_text(start, len)
			out.write_string(text)
			continue
		}
		if kind == segment_kind_placeholder {
			name := compiled.segment_text(start, len)
			if write_placeholder_value(mut out, name, params.placeholders, compiled.template_type) {
				continue
			}
			out.write_string(params.missing_placeholder_prefix)
			out.write_string(name)
		}
	}
	rendered := out.str()
	return rendered.clone()
}

fn (compiled &CompiledTemplate) segment_text(start int, len int) string {
	end := start + len
	if start < 0 || len < 0 || end > compiled.content.len {
		return ''
	}
	return compiled.content[start..end]
}

// write_placeholder_value resolves the normal placeholder name first, then the
// historical `_#includehtml` alias. The alias is accepted for compatibility but
// still passes through the allow-list based HTML escaping path.
fn write_placeholder_value(mut out strings.Builder, name string, placeholders &map[string]string, template_type TemplateType) bool {
	unsafe {
		if raw_value := placeholders[name] {
			rendered_value := render_value(raw_value, name.ends_with(include_html_key_suffix),
				template_type)
			out.write_string(rendered_value)
			return true
		}
		include_html_name := name + include_html_key_suffix
		if raw_html := placeholders[include_html_name] {
			rendered_html := render_value(raw_html, true, template_type)
			out.write_string(rendered_html)
			return true
		}
	}
	return false
}

// estimate_render_size gives strings.Builder enough capacity for common
// placeholder expansion without making a second pre-render pass.
fn estimate_render_size(compiled &CompiledTemplate) int {
	return compiled.estimated_size + 1024 + (compiled.segment_count * 64)
}

// render_value applies DTM's safety default: values are escaped unless the
// caller explicitly uses the include-html convention in an HTML template.
fn render_value(raw string, allow_html bool, template_type TemplateType) string {
	if allow_html && template_type == .html {
		return escape_with_allowed_html(raw)
	}
	return escape_html(raw)
}

// escape_with_allowed_html escapes the full value first, then restores only the
// supported tags. This preserves the legacy opt-in behavior without allowing
// arbitrary raw HTML through.
fn escape_with_allowed_html(value string) string {
	mut escaped := escape_html(value)
	for tag in allowed_html_tags {
		escaped = escaped.replace(escape_html(tag), tag)
	}
	return escaped
}

// escape_html is a local deterministic HTML escape helper. It avoids depending
// on heavier generic replacement paths in the hot render loop.
fn escape_html(value string) string {
	mut escaped := strings.new_builder(value.len)
	for i := 0; i < value.len; i++ {
		match value[i] {
			`&` {
				escaped.write_string('&amp;')
			}
			`<` {
				escaped.write_string('&lt;')
			}
			`>` {
				escaped.write_string('&gt;')
			}
			`"` {
				escaped.write_string('&#34;')
			}
			`'` {
				escaped.write_string('&#39;')
			}
			else {
				escaped.write_u8(value[i])
			}
		}
	}
	escaped_value := escaped.str()
	return escaped_value.clone()
}

// compress_html_output performs a small, predictable whitespace compression
// pass for HTML templates. It intentionally avoids regex work in the hot path.
fn compress_html_output(html string) string {
	mut result := strings.new_builder(html.len)
	mut pending_space := false
	mut last_written := u8(0)
	for i := 0; i < html.len; i++ {
		c := html[i]
		if c == `\n` || c == `\t` {
			continue
		}
		if c == ` ` {
			pending_space = true
			continue
		}
		if pending_space {
			if !(last_written == `>` && c == `<`) {
				result.write_u8(` `)
				last_written = ` `
			}
			pending_space = false
		}
		result.write_u8(c)
		last_written = c
	}
	compressed := result.str()
	return compressed.clone()
}
