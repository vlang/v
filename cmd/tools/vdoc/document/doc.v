module document

import os
import time
import v.flat
import v.parser
import v.pref
import v.token

// SymbolKind categorizes the symbols it documents.
pub enum SymbolKind {
	none
	const_group
	constant
	variable
	function
	method
	interface
	typedef
	enum
	enum_field
	struct
	struct_field
}

pub enum Platform {
	auto
	ios
	macos
	linux
	windows
	freebsd
	openbsd
	netbsd
	dragonfly
	js
	android
	termux
	solaris
	serenity
	plan9
	vinix
	haiku
	raw
	cross
}

// Position is the source position retained by vdoc.
pub struct Position {
pub:
	line_nr   int
	last_line int
	offset    int
}

// platform_from_string converts a command-line platform name.
pub fn platform_from_string(platform_str string) !Platform {
	return match platform_str {
		'all', 'cross' { .cross }
		'linux', 'nix' { .linux }
		'windows' { .windows }
		'ios' { .ios }
		'macos' { .macos }
		'freebsd' { .freebsd }
		'openbsd' { .openbsd }
		'netbsd' { .netbsd }
		'dragonfly' { .dragonfly }
		'js' { .js }
		'solaris' { .solaris }
		'serenity' { .serenity }
		'plan9' { .plan9 }
		'vinix' { .vinix }
		'android' { .android }
		'termux' { .termux }
		'haiku' { .haiku }
		'raw' { .raw }
		'' { .auto }
		else { return error('vdoc: invalid platform `${platform_str}`') }
	}
}

// platform_from_filename returns the platform suffix of a V source file.
pub fn platform_from_filename(filename string) Platform {
	stem := filename.all_before_last('.v').all_before_last('.c')
	suffix := stem.all_after_last('_')
	mut platform := platform_from_string(suffix) or { Platform.cross }
	if platform == .auto {
		platform = .cross
	}
	return platform
}

// str returns the display label of a symbol kind.
pub fn (sk SymbolKind) str() string {
	return match sk {
		.const_group { 'Constants' }
		.function, .method { 'fn' }
		.interface { 'interface' }
		.typedef { 'type' }
		.enum { 'enum' }
		.struct { 'struct' }
		else { '' }
	}
}

@[minify]
pub struct Doc {
pub mut:
	base_path           string
	filename            string
	pos                 int
	pub_only            bool = true
	with_comments       bool = true
	with_pos            bool
	with_head           bool = true
	is_vlib             bool
	time_generated      time.Time
	head                DocNode
	contents            map[string]DocNode
	scoped_contents     map[string]DocNode
	parent_mod_name     string
	orig_mod_name       string
	extract_vars        bool
	filter_symbol_names []string
	common_symbols      []string
	platform            Platform
}

@[minify]
pub struct DocNode {
pub mut:
	name        string
	content     string
	comments    []DocComment
	pos         Position
	file_path   string
	kind        SymbolKind
	tags        []string
	parent_name string
	return_type string
	children    []DocNode
	attrs       map[string]string @[json: attributes]
	from_scope  bool
	is_pub      bool @[json: public]
	platform    Platform
	is_readme   bool
	frontmatter map[string]string
}

// new_vdoc_preferences creates permissive parser preferences for vdoc.
pub fn new_vdoc_preferences() &pref.Preferences {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	prefs.is_fmt = true
	return prefs
}

// new creates a documentation collector rooted at input_path.
pub fn new(input_path string) Doc {
	return Doc{
		base_path:       os.real_path(input_path)
		head:            DocNode{}
		contents:        map[string]DocNode{}
		scoped_contents: map[string]DocNode{}
		time_generated:  time.now()
	}
}

fn (mut d Doc) add_node(mut node DocNode) {
	if node.parent_name != '' {
		parent_name := node.parent_name
		if parent_name !in d.contents {
			d.contents[parent_name] = DocNode{
				name: parent_name
				kind: if parent_name == 'Constants' { .const_group } else { .typedef }
			}
		}
		if parent_name == 'Constants' {
			node.parent_name = ''
		}
		d.contents[parent_name].children << node
		return
	}
	if node.name !in d.contents {
		d.contents[node.name] = node
		return
	}
	if d.contents[node.name].kind == .typedef && node.kind !in [.typedef, .none] {
		children := d.contents[node.name].children.clone()
		d.contents[node.name] = node
		d.contents[node.name].children = children
	}
}

fn (mut d Doc) parse_file(path string) ! {
	source := os.read_file(path)!
	mut assigned_comments := map[int]bool{}
	mut parser_ := parser.Parser.new(new_vdoc_preferences())
	a := parser_.parse_file(path)
	mut file_node := &flat.Node(unsafe { nil })
	for raw_id in a.file_node_ids {
		candidate := a.node(flat.NodeId(raw_id))
		if candidate.kind == .file && candidate.value == path && candidate.children_count > 0 {
			file_node = candidate
			break
		}
	}
	if isnil(file_node) {
		return
	}
	mut module_name := 'main'
	mut first_declaration_line := int(1 << 30)
	for id in a.children_of(file_node) {
		node := a.node(id)
		if node.kind == .module_decl {
			module_name = node.value
			continue
		}
		if !is_documentable(node.kind) {
			continue
		}
		anchor_line := declaration_anchor_line(a, source, node)
		if anchor_line < first_declaration_line {
			first_declaration_line = anchor_line
		}
		mut doc_node := d.node_from_flat(a, source, id, path) or { continue }
		if d.with_comments {
			doc_node.comments = comments_before(a, source, anchor_line, mut assigned_comments)
		}
		d.add_node(mut doc_node)
		if node.kind == .enum_decl && 'flag' in doc_node.attrs {
			d.add_flag_enum_helpers(doc_node)
		}
	}
	if d.orig_mod_name == '' {
		d.orig_mod_name = module_name
		d.parent_mod_name = module_parent_for_docs(d.base_path, module_name)
		qualified := if d.parent_mod_name != '' {
			'${d.parent_mod_name}.${module_name}'
		} else {
			module_name
		}
		d.orig_mod_name = qualified
		if d.with_head {
			d.head = DocNode{
				name:      qualified
				content:   'module ${qualified}'
				file_path: path
			}
		}
	}
	if d.with_comments && d.head.comments.len == 0 {
		d.head.comments = module_comments(a, source, first_declaration_line, mut assigned_comments)
	}
}

fn is_documentable(kind flat.NodeKind) bool {
	return kind in [.const_decl, .enum_decl, .interface_decl, .struct_decl, .type_decl, .fn_decl]
}

fn (d &Doc) node_from_flat(a &flat.FlatAst, source string, id flat.NodeId, path string) !DocNode {
	node := a.node(id)
	is_pub := node.op == .arrow
	if d.pub_only && !is_pub {
		return error('symbol not public')
	}
	mut name := node.value
	mut parent_name := ''
	mut kind := SymbolKind.none
	match node.kind {
		.const_decl {
			name = ''
			parent_name = 'Constants'
			kind = .const_group
		}
		.enum_decl { kind = .enum }
		.interface_decl { kind = .interface }
		.struct_decl { kind = .struct }
		.type_decl { kind = .typedef }
		.fn_decl {
			kind = .function
			if receiver, method := flat.decode_static_type_method_name(name) {
				name = '${receiver}.${method}'
				kind = .method
			} else if name.contains('.') {
				parent_name = name.all_before_last('.')
				name = name.all_after_last('.')
				kind = .method
			}
		}
		else { return error('invalid node') }
	}
	included := name in d.filter_symbol_names || parent_name in d.filter_symbol_names
	if d.filter_symbol_names.len > 0 && !included {
		return error('filtered')
	}
	attributes := declaration_attributes(a, source, node)
	mut attrs := map[string]string{}
	mut tags := []string{}
	for attribute in attributes {
		key := attribute.trim_space().trim_string_left('@[').trim_string_right(']').all_before(':').all_before(';')
		attrs[key] = attribute
		tags << attribute
	}
	if function_signature(a, source, node).contains('unsafe fn ') {
		tags << 'unsafe'
	}
	return DocNode{
		name:        name
		content:     declaration_content(a, source, id, attributes)
		pos:         doc_position(a, node.pos)
		file_path:   path
		kind:        kind
		tags:        tags
		parent_name: parent_name
		return_type: if node.kind == .fn_decl { node.typ } else { '' }
		attrs:       attrs
		is_pub:      is_pub
		platform:    platform_from_filename(path)
	}
}

fn (mut d Doc) add_flag_enum_helpers(enum_node DocNode) {
	for name in ['all', 'has', 'is_empty'] {
		d.contents[enum_node.name].children << DocNode{
			name:        name
			kind:        .method
			parent_name: enum_node.name
			file_path:   enum_node.file_path
			is_pub:      true
		}
	}
	for name in ['from', 'zero'] {
		full_name := '${enum_node.name}.${name}'
		d.contents[full_name] = DocNode{
			name:      full_name
			kind:      .method
			content:   'fn ${full_name}() ${enum_node.name}'
			file_path: enum_node.file_path
			is_pub:    true
		}
	}
}

fn declaration_content(a &flat.FlatAst, source string, id flat.NodeId, attributes []string) string {
	node := a.node(id)
	mut content := if node.kind == .fn_decl {
		function_signature(a, source, node)
	} else {
		declaration_source(a, source, id)
	}
	if node.op == .arrow && !content.starts_with('pub ') {
		content = 'pub ' + content
	}
	if attributes.len > 0 {
		content = attributes.join('\n') + '\n' + content
	}
	return content
}

fn declaration_source(a &flat.FlatAst, source string, id flat.NodeId) string {
	node := a.node(id)
	position := a.source_position(node.pos) or { return source_span(source, node.pos).trim_space() }
	file := a.source_files[node.pos.id] or { return source_span(source, node.pos).trim_space() }
	mut start := file.line_start(position.line)
	for start < node.pos.offset && source[start].is_space() {
		start++
	}
	end := a.formatter_node_ends[int(id)] or { int(node.pos.end) }
	if end <= start || end > source.len {
		return source_span(source, node.pos).trim_space()
	}
	return source[start..end].trim_space()
}

fn function_signature(a &flat.FlatAst, source string, node &flat.Node) string {
	position := a.source_position(node.pos) or { return '' }
	file := a.source_files[node.pos.id] or { return '' }
	mut start := file.line_start(position.line)
	for start < node.pos.offset && source[start].is_space() {
		start++
	}
	mut paren_depth := 0
	mut bracket_depth := 0
	mut quote := u8(0)
	mut escaped := false
	mut i := start
	for i < source.len {
		ch := source[i]
		if quote != 0 {
			if escaped {
				escaped = false
			} else if ch == `\\` {
				escaped = true
			} else if ch == quote {
				quote = 0
			}
			i++
			continue
		}
		if ch in [`'`, `"`] || ch == 96 {
			quote = ch
		} else if ch == `(` {
			paren_depth++
		} else if ch == `)` {
			paren_depth--
		} else if ch == `[` {
			bracket_depth++
		} else if ch == `]` {
			bracket_depth--
		} else if ch == `{` && paren_depth == 0 && bracket_depth == 0 {
			return source[start..i].trim_space()
		} else if ch == `=` && i + 1 < source.len && source[i + 1] == `>`
			&& paren_depth == 0 && bracket_depth == 0 {
			return source[start..i].trim_space()
		}
		i++
	}
	return source[start..int(node.pos.end)].trim_space()
}

fn declaration_attributes(a &flat.FlatAst, source string, node &flat.Node) []string {
	position := a.source_position(node.pos) or { return [] }
	file := a.source_files[node.pos.id] or { return [] }
	mut attributes := []string{}
	for line := position.line - 1; line > 0; line-- {
		start := file.line_start(line)
		end := if line < file.line_count() { file.line_start(line + 1) } else { source.len }
		text := source[start..end].trim_space()
		if text.starts_with('@[') && text.ends_with(']') {
			attributes << text
			continue
		}
		break
	}
	return attributes.reverse()
}

fn declaration_anchor_line(a &flat.FlatAst, source string, node &flat.Node) int {
	line := source_line(a, node.pos)
	return line - declaration_attributes(a, source, node).len
}

fn source_line(a &flat.FlatAst, pos token.Pos) int {
	position := a.source_position(pos) or { return 1 }
	return position.line
}

fn doc_position(a &flat.FlatAst, pos token.Pos) Position {
	file := a.source_files[pos.id] or { return Position{} }
	return Position{
		line_nr:   file.position_at(pos.offset).line - 1
		last_line: file.position_at(pos.end).line - 1
		offset:    pos.offset
	}
}

fn source_span(source string, pos token.Pos) string {
	if pos.offset < 0 || pos.end < pos.offset || pos.end > source.len {
		return ''
	}
	return source[pos.offset..pos.end]
}

fn module_parent_for_docs(base_path string, module_name string) string {
	if module_name == 'main' || os.file_name(base_path) != module_name {
		return ''
	}
	normalized := os.real_path(base_path)
	vlib_marker := os.path_separator + 'vlib' + os.path_separator
	if normalized.contains(vlib_marker) {
		parts := normalized.all_after(vlib_marker).split(os.path_separator)
		return if parts.len > 1 { parts[..parts.len - 1].join('.') } else { '' }
	}
	mut boundary := normalized
	for {
		parent := os.dir(boundary)
		if parent == boundary {
			break
		}
		for entry in os.ls(parent) or { []string{} } {
			if !entry.ends_with('.v') || entry.ends_with('_test.v') {
				continue
			}
			source := os.read_file(os.join_path(parent, entry)) or { continue }
			if module_name_from_source(source) == 'main' {
				relative := normalized.trim_string_left(parent).trim(os.path_separator)
				parts := relative.split(os.path_separator)
				return if parts.len > 1 { parts[..parts.len - 1].join('.') } else { '' }
			}
		}
		if os.is_file(os.join_path(parent, 'v.mod')) {
			return ''
		}
		boundary = parent
	}
	return ''
}

fn comments_before(a &flat.FlatAst, source string, anchor_line int, mut assigned map[int]bool) []DocComment {
	mut result := []DocComment{}
	mut expected_line := anchor_line - 1
	for i := a.comments.len - 1; i >= 0; i-- {
		comment := a.comments[i]
		position := doc_position(a, comment.pos)
		end_line := position.last_line + 1
		if end_line > expected_line || i in assigned {
			continue
		}
		if end_line < expected_line {
			break
		}
		text := source_span(source, comment.pos)
		if !text.starts_with('//') {
			break
		}
		result << DocComment{
			text:     '\x01' + text[2..]
			pos:      position
			is_multi: false
		}
		assigned[i] = true
		expected_line = position.line_nr
	}
	return result.reverse()
}

fn module_comments(a &flat.FlatAst, source string, first_decl_line int, mut assigned map[int]bool) []DocComment {
	mut result := []DocComment{}
	for i, comment in a.comments {
		if i in assigned {
			continue
		}
		position := doc_position(a, comment.pos)
		line := position.line_nr + 1
		if line >= first_decl_line {
			continue
		}
		text := source_span(source, comment.pos)
		if !text.starts_with('//') {
			continue
		}
		result << DocComment{
			text:     '\x01' + text[2..]
			pos:      position
			is_multi: false
		}
		assigned[i] = true
	}
	return result
}

fn file_matches_platform(name string, platform Platform) bool {
	if name.ends_with('.js.v') || name.ends_with('.native.v') || name.ends_with('.wasm.v') {
		return false
	}
	file_platform := platform_from_filename(name)
	if file_platform == .cross {
		return true
	}
	if platform == .cross {
		return true
	}
	if platform != .auto {
		return file_platform == platform
	}
	host := $if windows { Platform.windows } $else $if macos { Platform.macos } $else $if linux { Platform.linux } $else $if freebsd { Platform.freebsd } $else $if openbsd { Platform.openbsd } $else $if netbsd { Platform.netbsd } $else { Platform.cross }
	return file_platform == host
}

fn source_files_for_platform(dir string, entries []string, platform Platform) []string {
	if platform == .cross || platform in [.plan9, .raw] {
		mut files := entries.filter((it.ends_with('.v') || it.ends_with('.vsh'))
			&& !it.ends_with('_test.v') && file_matches_platform(it, platform))
		files.sort()
		return files
	}
	target_os := if platform == .auto { pref.host_os_name() } else { '${platform}' }
	target := pref.target_from(target_os, pref.host_arch()) or { pref.host_target() }
	return pref.get_v_files_from_dir_for_target(dir, [], target).map(os.file_name(it)).filter(!it.ends_with('.native.v')
		&& !it.ends_with('.wasm.v'))
}

// generate populates this Doc from its input directory.
pub fn (mut d Doc) generate() ! {
	d.base_path = if os.is_dir(d.base_path) {
		d.base_path
	} else {
		os.real_path(os.dir(d.base_path))
	}
	d.is_vlib = d.base_path.contains('vlib')
	entries := os.ls(d.base_path)!
	files := source_files_for_platform(d.base_path, entries, d.platform)
	if files.len == 0 {
		eprintln('vdoc: No valid V files were found. Skipping folder: ${d.base_path}.')
		return
	}
	for filename in files {
		d.parse_file(os.join_path(d.base_path, filename))!
	}
	if d.filter_symbol_names.len > 0 && d.contents.len > 0 {
		for filter_name in d.filter_symbol_names {
			if filter_name !in d.contents {
				return error('vdoc: `${filter_name}` symbol in module `${d.orig_mod_name}` not found')
			}
		}
	}
	d.time_generated = time.now()
}

// generate documents a file or directory.
pub fn generate(input_path string, pub_only bool, with_comments bool, platform Platform, filter_symbol_names ...string) !Doc {
	if platform == .js {
		return error('vdoc: Platform `${platform}` is not supported.')
	}
	mut d := new(input_path)
	d.pub_only = pub_only
	d.with_comments = with_comments
	d.platform = platform
	d.filter_symbol_names = filter_symbol_names.filter(it.len != 0)
	d.generate()!
	return d
}

// generate_with_pos generates documentation and retains the requested source position.
pub fn generate_with_pos(input_path string, filename string, pos int) !Doc {
	mut d := new(input_path)
	d.pub_only = false
	d.with_comments = true
	d.with_pos = true
	d.filename = filename
	d.pos = pos
	d.generate()!
	return d
}
