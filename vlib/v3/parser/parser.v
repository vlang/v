module parser

import os
import strings
import v3.flat
import v3.pref
import v3.scanner
import v3.token

// C.open declares the C open symbol used by parser.
fn C.open(charptr, int, int) int

// C.read declares the C read symbol used by parser.
fn C.read(int, voidptr, int) int

// C.close declares the C close symbol used by parser.
fn C.close(int) int

// C.malloc declares the C malloc symbol used by parser.
fn C.malloc(int) &u8

// C.realloc declares the C realloc symbol used by parser.
fn C.realloc(voidptr, int) &u8

// C.free declares the C free symbol used by parser.
fn C.free(voidptr)

const max_source_file_size = 8388608
const read_source_chunk_size = 65536

// Parser represents parser data used by parser.
pub struct Parser {
	prefs &pref.Preferences
mut:
	s                       scanner.Scanner
	tok                     token.Token
	lit                     string
	tok_pos                 int
	peek_tok                token.Token = .eof
	peek_lit                string
	peek_pos                int
	has_peek                bool
	cur_file                string
	cur_module              string
	cur_fn                  string
	cur_struct              string   // receiver type name of the current method, for `@STRUCT`
	cur_method_is_static    bool     // distinguishes `Type.method()` from `(x Type) method()` for `@LOCATION`
	comptime_for_vars       []string // active `$for` loop variables; a `$if` that reads one is deferred to unroll time
	comptime_method_var     string   // innermost active `$for method in Type.methods` loop variable
	comptime_const_values   map[string]string
	comptime_local_values   map[string]string
	comptime_value_undos    []ComptimeValueUndo
	comptime_value_scopes   []int
	pending_flag            bool
	pending_json_as_number  bool // `@[json_as_number]` seen before the next enum decl
	pending_params          bool
	pending_export          string
	pending_noreturn        bool
	pending_soa             bool
	skip_next_decl          bool
	disable_fn_body         bool
	pending_fn_pub          bool
	pending_decl_attrs      []string
	pending_decl_attr_kinds []int
	in_for_container        bool
	local_type_names        map[string]string
	local_type_scopes       []string
	anonymous_struct_types  map[string][]string
	anonymous_struct_count  int
	export_records          []ExportRecord
pub mut:
	a              &flat.FlatAst = unsafe { nil }
	parsed_v_files int
}

// ExportRecord captures one accepted `@[export: name]` registration in file
// order. The parallel-parse merge replays worker exports through these records
// so they can be revalidated against disabled fns from earlier chunks (see
// merge_parsed_worker), reproducing the serial parse's order-dependent check.
struct ExportRecord {
	name  string
	qname string
	value string
}

struct ComptimeValueUndo {
	name      string
	old_value string
	had_value bool
}

struct ParsedFieldAttrs {
	attrs []string
	kinds []int
}

// new creates a Parser value for parser.
pub fn Parser.new(prefs &pref.Preferences) &Parser {
	return &Parser{
		prefs:                  unsafe { prefs }
		s:                      scanner.new_scanner(prefs, .normal)
		local_type_names:       map[string]string{}
		anonymous_struct_types: map[string][]string{}
		comptime_const_values:  map[string]string{}
		comptime_local_values:  map[string]string{}
		a:                      &flat.FlatAst{
			nodes:                []flat.Node{cap: 256}
			children:             []flat.NodeId{cap: 512}
			disabled_fns:         map[string]bool{}
			export_fn_names:      map[string]string{}
			specialized_fn_nodes: map[int]bool{}
		}
	}
}

// parse_file reads parse file input for parser.
pub fn (mut p Parser) parse_file(path string) &flat.FlatAst {
	p.parse_into(path)
	return p.a
}

// parse_files reads parse files input for parser.
pub fn (mut p Parser) parse_files(paths []string) &flat.FlatAst {
	for path in paths {
		p.parse_into(path)
	}
	return p.a
}

// parse_files_with_starts parses paths in order like parse_files, recording the
// first node id of each file's region. Import resolution uses the starts to
// bound per-file post-processing (module-name canonicalization) when files are
// parsed in batches instead of one at a time.
pub fn (mut p Parser) parse_files_with_starts(paths []string) []int {
	mut starts := []int{cap: paths.len}
	for path in paths {
		starts << p.a.nodes.len
		p.parse_into(path)
	}
	return starts
}

// parse_into reads parse into input for parser.
pub fn (mut p Parser) parse_into(path string) {
	p.cur_file = path
	p.cur_module = ''
	p.cur_fn = ''
	p.has_peek = false
	p.pending_flag = false
	p.pending_params = false
	p.pending_export = ''
	p.pending_noreturn = false
	p.pending_soa = false
	p.skip_next_decl = false
	p.disable_fn_body = false
	p.pending_fn_pub = false
	p.pending_decl_attrs.clear()
	p.pending_decl_attr_kinds.clear()
	p.comptime_local_values.clear()
	p.comptime_value_undos.clear()
	p.comptime_value_scopes.clear()
	p.in_for_container = false
	p.local_type_scopes = []string{}
	p.anonymous_struct_types = map[string][]string{}
	p.anonymous_struct_count = 0
	// File marker before content so import resolver can track source files
	p.a.add_node(flat.Node{
		kind:  .file
		value: path
	})
	src := read_source_file_raw(path)
	if src.len == 0 {
		// An empty but existing `.v` file is valid (declares nothing); only a
		// genuine read failure (missing file) is an error worth reporting.
		if !os.exists(path) {
			eprintln('error reading ${path}')
		}
		return
	}
	if path.ends_with('.v') {
		p.parsed_v_files++
	}
	p.reserve_for_source(src.len)
	mut file_set := token.FileSet.new()
	file := file_set.add_file(path, -1, src.len)
	p.s.init(file, src)
	p.next()

	mut ids := []flat.NodeId{}
	for p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		if p.tok == .key_module {
			p.next()
			p.cur_module = p.lit
			mod_id := p.a.add_node(flat.Node{
				kind:  .module_decl
				value: p.lit
			})
			ids << mod_id
			p.next()
			if p.tok == .semicolon || p.lit == '' {
				p.next()
			}
			continue
		}
		id := p.top_level_stmt()
		if int(id) >= 0 {
			ids << id
		}
	}
	start := p.add_children(ids)
	p.a.add_node(flat.Node{
		kind:           .file
		value:          path
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) reserve_for_source(src_len int) {
	if src_len <= 0 {
		return
	}
	node_cap := p.a.nodes.len + src_len / 6 + 64
	if node_cap > p.a.nodes.cap {
		p.a.nodes.ensure_cap(node_cap)
	}
	child_cap := p.a.children.len + src_len / 10 + 64
	if child_cap > p.a.children.cap {
		p.a.children.ensure_cap(child_cap)
	}
}

// read_source_file_raw reads read source file raw input for parser.
fn read_source_file_raw(path string) string {
	cpath := cstring_from_vstring(path)
	fd := C.open(cpath, 0, 0)
	if fd < 0 {
		return ''
	}
	mut cap := read_source_chunk_size
	mut buf := C.malloc(cap + 1)
	if isnil(buf) {
		C.close(fd)
		return ''
	}
	mut total := 0
	for total < max_source_file_size {
		if total == cap {
			mut new_cap := cap * 2
			if new_cap > max_source_file_size {
				new_cap = max_source_file_size
			}
			mut new_buf := C.realloc(buf, new_cap + 1)
			if isnil(new_buf) {
				C.close(fd)
				unsafe { C.free(buf) }
				return ''
			}
			buf = new_buf
			cap = new_cap
		}
		remaining := cap - total
		nread := C.read(fd, unsafe { buf + total }, remaining)
		if nread <= 0 {
			break
		}
		total += nread
	}
	C.close(fd)
	if total <= 0 {
		unsafe { C.free(buf) }
		return ''
	}
	unsafe {
		if cap > total {
			new_buf := C.realloc(buf, total + 1)
			if !isnil(new_buf) {
				buf = new_buf
			}
		}
		buf[total] = 0
		return tos(buf, total)
	}
}

// cstring_from_vstring converts cstring from vstring data for parser.
fn cstring_from_vstring(s string) &u8 {
	buf := C.malloc(s.len + 1)
	unsafe {
		for i in 0 .. s.len {
			buf[i] = s[i]
		}
		buf[s.len] = 0
	}
	return buf
}

// vmod_root_for_file supports vmod root for file handling for parser.
fn vmod_root_for_file(path string) string {
	mut dir := if path.len > 0 { os.dir(path) } else { os.getwd() }
	if dir.len == 0 {
		dir = os.getwd()
	}
	original_dir := dir
	for {
		if os.exists(os.join_path(dir, 'v.mod')) {
			return dir
		}
		parent := os.dir(dir)
		if parent == dir || parent.len == 0 {
			return original_dir
		}
		dir = parent
	}
	return dir
}

fn (p &Parser) source_line_number(pos int) int {
	mut line := 1
	end := if pos < 0 {
		0
	} else if pos < p.s.src.len {
		pos
	} else {
		p.s.src.len
	}
	for i in 0 .. end {
		if p.s.src[i] == `\n` {
			line++
		}
	}
	return line
}

// next supports next handling for Parser.
@[inline]
fn (mut p Parser) next() {
	if p.has_peek {
		p.tok = p.peek_tok
		p.lit = p.peek_lit
		p.tok_pos = p.peek_pos
		p.has_peek = false
		return
	}
	p.tok = p.s.scan()
	p.lit = p.s.lit
	p.tok_pos = p.s.pos
}

// line_nr_for_pos returns the 1-based line number of a byte offset in the
// current source (the scanner tracks byte positions only, so `@LINE` and
// `@LOCATION` count newlines on demand).
@[direct_array_access]
fn (p &Parser) line_nr_for_pos(pos int) int {
	end := if pos < p.s.src.len { pos } else { p.s.src.len }
	mut line := 1
	for i in 0 .. end {
		if p.s.src[i] == `\n` {
			line++
		}
	}
	return line
}

fn (p &Parser) column_for_pos(pos int) int {
	mut column := 0
	mut i := if pos < p.s.src.len { pos } else { p.s.src.len }
	for i > 0 && p.s.src[i - 1] != `\n` {
		i--
		column++
	}
	return column
}

// peek supports peek handling for Parser.
@[inline]
fn (mut p Parser) peek() token.Token {
	if !p.has_peek {
		p.peek_tok = p.s.scan()
		p.peek_lit = p.s.lit
		p.peek_pos = p.s.pos
		p.has_peek = true
	}
	return p.peek_tok
}

// peek_is supports peek is handling for Parser.
@[inline]
fn (mut p Parser) peek_is(tok token.Token) bool {
	return p.peek() == tok
}

@[inline]
fn (mut p Parser) check(expected token.Token) {
	if p.tok == expected {
		p.next()
	}
}

fn (mut p Parser) expect(expected token.Token) string {
	lit := p.lit
	if p.tok != expected {
		eprintln('expected ${expected}, got ${p.tok} "${p.lit}"')
	}
	p.next()
	return lit
}

fn (mut p Parser) expect_name() string {
	name := p.lit
	if p.tok != .name && p.lit.len == 0 {
		eprintln('expected name, got ${p.tok} "${p.lit}"')
	}
	p.next()
	return name
}

fn (mut p Parser) expect_name_or_keyword() string {
	name := p.lit
	p.next()
	return name
}

@[inline]
fn token_id_is_infix(tv int) bool {
	return tv == 0 || tv == 1 || tv == 3 || tv == 13 || tv == 20 || tv == 21 || tv == 22 || tv == 44
		|| tv == 46 || tv == 74 || tv == 75 || tv == 77 || tv == 80 || tv == 81 || tv == 83
		|| tv == 85 || tv == 88 || tv == 90 || tv == 91 || tv == 94 || tv == 95 || tv == 99
		|| tv == 101 || tv == 109
}

@[inline]
fn token_is_infix(tok token.Token) bool {
	return token_id_is_infix(int(tok))
}

@[inline]
fn token_id_is_postfix(tv int) bool {
	return tv == 11 || tv == 24
}

@[inline]
fn token_is_postfix(tok token.Token) bool {
	return token_id_is_postfix(int(tok))
}

@[inline]
fn token_id_is_assignment(tv int) bool {
	return tv == 2 || tv == 4 || tv == 12 || tv == 14 || tv == 76 || tv == 82 || tv == 84
		|| tv == 86 || tv == 93 || tv == 96 || tv == 100 || tv == 102 || tv == 110
}

@[inline]
fn token_is_assignment(tok token.Token) bool {
	return token_id_is_assignment(int(tok))
}

@[inline]
fn token_id_left_binding_power(tv int) token.BindingPower {
	if tv == 77 {
		return token.BindingPower.logical_or
	}
	if tv == 1 {
		return token.BindingPower.logical_and
	}
	if tv == 20 || tv == 88 || tv == 80 || tv == 74 || tv == 22 || tv == 21 || tv == 44 || tv == 90
		|| tv == 46 || tv == 91 {
		return token.BindingPower.compare
	}
	if tv == 94 {
		return token.BindingPower.bit_or
	}
	if tv == 109 {
		return token.BindingPower.bit_xor
	}
	if tv == 75 || tv == 99 || tv == 101 {
		return token.BindingPower.shift
	}
	if tv == 95 || tv == 81 {
		return token.BindingPower.add
	}
	if tv == 85 || tv == 13 || tv == 83 || tv == 0 {
		return token.BindingPower.product
	}
	return token.BindingPower.lowest
}

@[inline]
fn token_left_binding_power(tok token.Token) token.BindingPower {
	return token_id_left_binding_power(int(tok))
}

@[inline]
fn token_id_right_binding_power(tv int) token.BindingPower {
	bp := token_id_left_binding_power(tv)
	if bp == .logical_or {
		return token.BindingPower.logical_and
	}
	if bp == .logical_and {
		return token.BindingPower.compare
	}
	if bp == .compare {
		return token.BindingPower.bit_or
	}
	if bp == .bit_or {
		return token.BindingPower.bit_xor
	}
	if bp == .bit_xor {
		return token.BindingPower.shift
	}
	if bp == .shift {
		return token.BindingPower.add
	}
	if bp == .add {
		return token.BindingPower.product
	}
	if bp == .product {
		return token.BindingPower.highest
	}
	return token.BindingPower.lowest
}

@[inline]
fn token_right_binding_power(tok token.Token) token.BindingPower {
	return token_id_right_binding_power(int(tok))
}

@[inline]
fn token_id_to_op(tv int) flat.Op {
	if tv == 95 {
		return flat.Op.plus
	}
	if tv == 81 {
		return flat.Op.minus
	}
	if tv == 85 {
		return flat.Op.mul
	}
	if tv == 13 {
		return flat.Op.div
	}
	if tv == 83 {
		return flat.Op.mod
	}
	if tv == 20 {
		return flat.Op.eq
	}
	if tv == 88 {
		return flat.Op.ne
	}
	if tv == 80 {
		return flat.Op.lt
	}
	if tv == 22 {
		return flat.Op.gt
	}
	if tv == 74 {
		return flat.Op.le
	}
	if tv == 21 {
		return flat.Op.ge
	}
	if tv == 0 {
		return flat.Op.amp
	}
	if tv == 94 {
		return flat.Op.pipe
	}
	if tv == 109 {
		return flat.Op.xor
	}
	if tv == 75 {
		return flat.Op.left_shift
	}
	if tv == 99 {
		return flat.Op.right_shift
	}
	if tv == 101 {
		return flat.Op.right_shift_unsigned
	}
	if tv == 1 {
		return flat.Op.logical_and
	}
	if tv == 77 {
		return flat.Op.logical_or
	}
	if tv == 89 {
		return flat.Op.not
	}
	if tv == 6 {
		return flat.Op.bit_not
	}
	if tv == 4 {
		return flat.Op.assign
	}
	if tv == 96 {
		return flat.Op.plus_assign
	}
	if tv == 82 {
		return flat.Op.minus_assign
	}
	if tv == 86 {
		return flat.Op.mul_assign
	}
	if tv == 14 {
		return flat.Op.div_assign
	}
	if tv == 84 {
		return flat.Op.mod_assign
	}
	if tv == 2 {
		return flat.Op.amp_assign
	}
	if tv == 93 {
		return flat.Op.pipe_assign
	}
	if tv == 110 {
		return flat.Op.xor_assign
	}
	if tv == 76 {
		return flat.Op.left_shift_assign
	}
	if tv == 100 {
		return flat.Op.right_shift_assign
	}
	if tv == 102 {
		return flat.Op.right_shift_unsigned_assign
	}
	if tv == 24 {
		return flat.Op.inc
	}
	if tv == 11 {
		return flat.Op.dec
	}
	if tv == 16 {
		return flat.Op.dot
	}
	if tv == 3 {
		return flat.Op.arrow
	}
	return flat.Op.none
}

fn (p &Parser) tok_can_be_decl_name() bool {
	return p.tok == .name || (int(p.tok) >= int(token.Token.key_as)
		&& int(p.tok) <= int(token.Token.key_unsafe))
}

fn (mut p Parser) add_children(ids []flat.NodeId) int {
	start := p.a.children.len
	for id in ids {
		p.a.children << id
	}
	return start
}

@[inline]
fn (mut p Parser) add_child(id flat.NodeId) int {
	start := p.a.children.len
	p.a.children << id
	return start
}

@[inline]
fn (mut p Parser) add_children2(a flat.NodeId, b flat.NodeId) int {
	start := p.a.children.len
	p.a.children << a
	p.a.children << b
	return start
}

// ==================== top-level ====================

fn (mut p Parser) top_level_stmt() flat.NodeId {
	match p.tok {
		.key_fn {
			return p.fn_decl()
		}
		.key_pub {
			if p.peek() == .key_fn {
				p.pending_fn_pub = true
			}
			p.next()
			return p.top_level_stmt()
		}
		.key_struct, .key_union {
			return p.struct_decl()
		}
		.key_global {
			return p.global_decl()
		}
		.key_const {
			return p.const_decl()
		}
		.key_enum {
			return p.enum_decl()
		}
		.key_type {
			return p.type_decl()
		}
		.key_interface {
			return p.interface_decl()
		}
		.key_import {
			return p.import_stmt()
		}
		.key_module {
			return p.module_stmt()
		}
		.attribute {
			p.parse_pending_decl_attrs()
			return p.parse_decl_after_attrs()
		}
		.lsbr {
			p.parse_pending_decl_attrs()
			return p.parse_decl_after_attrs()
		}
		.dollar {
			return p.parse_top_level_comptime_if()
		}
		.hash {
			return p.directive()
		}
		.key_assert {
			return p.assert_stmt()
		}
		.semicolon {
			p.next()
			return flat.empty_node
		}
		else {
			return p.stmt()
		}
	}
}

// parse_decl_after_attrs parses the declaration that follows an attribute group.
// A newline after `@[...]` yields an auto-inserted semicolon, which is consumed
// here so the declaration itself is parsed next. When the attribute group is a
// disabled `@[if flag ?]` (skip_next_decl set), functions are emitted as no-op
// stubs (empty body): the signature must remain so call sites still type-check
// and link, but the body is elided. Other disabled declarations are skipped.
fn (mut p Parser) parse_decl_after_attrs() flat.NodeId {
	p.consume_decl_prefix_after_attrs()
	if p.skip_next_decl {
		p.pending_params = false
		// A skipped enum (e.g. disabled by a preceding `@[if false]`) never reaches
		// enum_decl to consume this flag, so clear it here to avoid tagging the next
		// enum as `@[json_as_number]`.
		p.pending_json_as_number = false
		p.skip_next_decl = false
		p.pending_decl_attrs.clear()
		p.pending_decl_attr_kinds.clear()
		if p.cur_decl_is_fn() {
			p.disable_fn_body = true
			res := p.top_level_stmt()
			p.disable_fn_body = false
			p.skip_next_decl = false
			p.pending_soa = false
			return res
		}
		p.skip_top_level_stmt()
		p.skip_next_decl = false
		p.pending_export = ''
		p.pending_noreturn = false
		p.pending_soa = false
		return flat.empty_node
	}
	res := p.top_level_stmt()
	p.apply_decl_attrs(res)
	p.pending_params = false
	p.pending_export = ''
	p.pending_noreturn = false
	p.pending_json_as_number = false
	p.pending_soa = false
	return res
}

fn (mut p Parser) consume_decl_prefix_after_attrs() {
	for p.tok == .semicolon || p.tok == .attribute || p.tok == .lsbr {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		p.parse_pending_decl_attrs()
	}
}

fn (mut p Parser) parse_pending_decl_attrs() {
	parsed := p.parse_field_attrs_with_kinds()
	p.apply_decl_attr_flags(parsed.attrs)
	p.pending_decl_attrs << parsed.attrs
	p.pending_decl_attr_kinds << parsed.kinds
}

fn (mut p Parser) apply_decl_attrs(id flat.NodeId) {
	if p.pending_decl_attrs.len == 0 || int(id) < 0 || int(id) >= p.a.nodes.len {
		p.pending_decl_attrs.clear()
		p.pending_decl_attr_kinds.clear()
		return
	}
	p.a.add_node(flat.Node{
		kind:           .directive
		value:          '@attributes:${int(id)}'
		typ:            p.pending_decl_attr_kinds.map(it.str()).join(',')
		generic_params: p.pending_decl_attrs.clone()
	})
	p.pending_decl_attrs.clear()
	p.pending_decl_attr_kinds.clear()
}

fn (mut p Parser) apply_decl_attr_flags(attrs []string) {
	for attr in attrs {
		name := attr.all_before(':').trim_space()
		match name {
			'flag' {
				p.pending_flag = true
			}
			'json_as_number' {
				p.pending_json_as_number = true
			}
			'params' {
				p.pending_params = true
			}
			'noreturn' {
				p.pending_noreturn = true
			}
			'soa' {
				p.pending_soa = true
			}
			'export' {
				p.pending_export = attr_unquote(attr.all_after(':').trim_space())
			}
			else {}
		}
	}
}

fn (mut p Parser) cur_decl_is_fn() bool {
	if p.tok == .key_fn {
		return true
	}
	return p.tok == .key_pub && p.peek() == .key_fn
}

fn (mut p Parser) fn_decl() flat.NodeId {
	p.check(.key_fn)
	mut name := ''
	mut name_pos := p.tok_pos
	mut receiver_name := ''
	mut receiver_type := ''
	mut receiver_is_mut := false
	mut is_method := false

	// method receiver: fn (mut r Type) name()
	if p.tok == .lpar {
		is_method = true
		p.next()
		mut is_mut := false
		mut is_shared := false
		if p.tok == .key_mut || p.tok == .key_shared {
			is_mut = p.tok == .key_mut
			is_shared = p.tok == .key_shared
			receiver_is_mut = is_mut
			p.next()
		}
		receiver_name = p.expect_name()
		receiver_type = p.parse_type_name()
		if is_mut {
			receiver_type = '&' + receiver_type
		}
		if is_shared {
			receiver_type = 'shared ' + receiver_type
		}
		p.check(.rpar)

		// operator overload: fn (r Type) + (other Type) RetType { }
		if p.tok != .name && p.tok != .eof {
			if p.tok.is_overloadable() {
				name_pos = p.tok_pos
				op_name := overload_token_name(p.tok)
				p.next()
				return p.fn_operator_overload(receiver_name, receiver_type, receiver_is_mut,
					op_name, name_pos)
			}
		}
	}

	// function name
	if p.tok_can_be_decl_name() {
		name_pos = p.tok_pos
		name = p.expect_name_or_keyword()
		if p.tok == .dot {
			p.next()
			if name == 'C' || name == 'JS' {
				// C.func or JS.func
				name_pos = p.tok_pos
				name = p.expect_name_or_keyword()
				for p.tok == .dot {
					p.next()
					name += '.' + p.expect_name_or_keyword()
				}
				if is_method {
					clean_type := method_receiver_type_name(receiver_type)
					name = '${clean_type}.${name}'
				}
				return p.fn_decl_body(name, receiver_name, receiver_type, receiver_is_mut,
					is_method, true, name_pos)
			}
			// module.func or Type.static_method
			name_pos = p.tok_pos
			second := p.expect_name_or_keyword()
			if is_method {
				// This shouldn't happen for methods
				name = name + '.' + second
			} else {
				// Static method: Type.name
				receiver_type = name
				name = second
				is_method = true
			}
		}
	}

	if is_method && receiver_type.len > 0 {
		clean_type := method_receiver_type_name(receiver_type)
		name = '${clean_type}.${name}'
	}

	return p.fn_decl_body(name, receiver_name, receiver_type, receiver_is_mut, is_method, false,
		name_pos)
}

fn (mut p Parser) fn_operator_overload(receiver_name string, receiver_type string, receiver_is_mut bool, op_name string, name_pos int) flat.NodeId {
	is_pub := p.pending_fn_pub
	p.pending_fn_pub = false
	disable_body := p.disable_fn_body
	p.disable_fn_body = false
	// parse parameter
	p.check(.lpar)
	mut param_ids := []flat.NodeId{}
	// receiver
	param_ids << p.a.add_node(flat.Node{
		kind:   .param
		value:  receiver_name
		typ:    receiver_type
		is_mut: receiver_is_mut
		op:     .dot // receiver marker; ordinary declared parameters use `.none`
	})
	// operator param
	if p.tok == .key_mut {
		p.next()
	}
	for p.tok != .rpar && p.tok != .eof {
		start_offset := p.s.offset
		param_ids << p.parse_param_group(false)
		if p.s.offset == start_offset && p.tok != .rpar && p.tok != .eof {
			p.next()
		}
	}
	p.check(.rpar)

	mut ret_type := 'void'
	if p.tok != .lcbr && p.tok != .semicolon && p.tok != .eof {
		ret_type = p.parse_type_name()
	}

	clean_type := method_receiver_type_name(receiver_type)
	name := '${clean_type}.${op_name}'

	mut body_ids := []flat.NodeId{}
	if p.tok == .lcbr {
		prev_fn := p.cur_fn
		prev_struct := p.cur_struct
		prev_method_is_static := p.cur_method_is_static
		p.cur_fn = name
		p.cur_struct = method_receiver_type_name(receiver_type).all_after_last('.')
		p.cur_method_is_static = false
		p.push_local_type_scope(name)
		p.begin_comptime_value_scope()
		if disable_body {
			p.mark_disabled_fn(name)
			p.skip_block()
		} else {
			body_start := p.tok_pos
			p.check(.lcbr)
			p.predeclare_local_type_names_in_block(body_start)
			for p.tok != .rcbr && p.tok != .eof {
				id := p.stmt()
				if int(id) >= 0 {
					body_ids << id
				}
			}
			p.check(.rcbr)
		}
		p.end_comptime_value_scope()
		p.pop_local_type_scope()
		p.cur_fn = prev_fn
		p.cur_struct = prev_struct
		p.cur_method_is_static = prev_method_is_static
	}

	mut all_ids := []flat.NodeId{cap: param_ids.len + body_ids.len}
	for id in param_ids {
		all_ids << id
	}
	for id in body_ids {
		all_ids << id
	}
	start := p.add_children(all_ids)
	id := p.a.add_node(flat.Node{
		kind:           .fn_decl
		op:             if is_pub { .arrow } else { .none }
		value:          name
		typ:            ret_type
		pos:            source_token_pos(name_pos)
		children_start: start
		children_count: flat.child_count(all_ids.len)
	})
	p.register_pending_export(name)
	return id
}

fn (mut p Parser) fn_decl_body(name string, receiver_name string, receiver_type string, receiver_is_mut bool, is_method bool, is_c_decl bool, name_pos int) flat.NodeId {
	is_pub := p.pending_fn_pub
	p.pending_fn_pub = false
	// Capture & clear here so it applies only to this function (not nested closures
	// or a following declaration), and is cleared even on the extern/no-body path.
	disable_body := p.disable_fn_body
	p.disable_fn_body = false
	// generic params — skip
	mut generic_params := []string{}
	if p.tok == .lsbr {
		generic_params = p.parse_generic_param_names()
	}

	// params
	p.check(.lpar)
	mut param_ids := []flat.NodeId{}
	if is_method && receiver_name.len > 0 {
		param_ids << p.a.add_node(flat.Node{
			kind:   .param
			value:  receiver_name
			typ:    receiver_type
			is_mut: receiver_is_mut
			op:     .dot // receiver marker; static methods do not inject this parameter
		})
	}
	for p.tok != .rpar && p.tok != .eof {
		start_offset := p.s.offset
		param_ids << p.parse_param_group(is_c_decl)
		if p.s.offset == start_offset && p.tok != .rpar && p.tok != .eof {
			p.next()
		}
	}
	p.check(.rpar)

	// return type
	mut ret_type := 'void'
	if p.tok == .name || p.tok == .amp || p.tok == .question || p.tok == .not || p.tok == .lsbr
		|| p.tok == .lpar || p.tok == .key_fn || p.tok == .ellipsis {
		ret_type = p.parse_type_name()
	}
	// no body — extern/C declaration
	if p.tok != .lcbr {
		for p.tok == .semicolon {
			p.next()
		}
		start := p.add_children(param_ids)
		is_v_header_decl := !is_c_decl && p.cur_file.ends_with('.vh')
		if disable_body && is_v_header_decl {
			p.mark_disabled_fn(name)
		}
		id := p.a.add_node(flat.Node{
			kind:           if is_v_header_decl { .fn_decl } else { .c_fn_decl }
			op:             if is_pub { .arrow } else { .none }
			value:          name
			typ:            ret_type
			pos:            source_token_pos(name_pos)
			generic_params: generic_params
			children_start: start
			children_count: flat.child_count(param_ids.len)
			// Function nodes do not otherwise use is_mut. On a .vh declaration it
			// records that the body lives in a cached object and must not be emitted.
			is_mut: is_v_header_decl
		})
		p.pending_export = ''
		p.register_pending_noreturn(name)
		return id
	}

	// body
	mut body_ids := []flat.NodeId{}
	prev_fn := p.cur_fn
	prev_struct := p.cur_struct
	prev_method_is_static := p.cur_method_is_static
	p.cur_fn = name
	// `@STRUCT` inside a method expands to the receiver's (dereferenced) type name.
	p.cur_struct = if is_method {
		method_receiver_type_name(receiver_type).all_after_last('.')
	} else {
		''
	}
	p.cur_method_is_static = is_method && receiver_name.len == 0
	p.push_local_type_scope(name)
	p.begin_comptime_value_scope()
	// A disabled `@[if flag ?]` function keeps its signature but gets an empty body
	// (a no-op stub), so callers still resolve while the body is compiled out.
	if disable_body {
		p.mark_disabled_fn(name)
		p.skip_block()
	} else {
		body_start := p.tok_pos
		p.check(.lcbr)
		p.predeclare_local_type_names_in_block(body_start)
		for p.tok != .rcbr && p.tok != .eof {
			id := p.stmt()
			if int(id) >= 0 {
				body_ids << id
			}
		}
		p.check(.rcbr)
	}
	p.end_comptime_value_scope()
	p.pop_local_type_scope()
	p.cur_fn = prev_fn
	p.cur_struct = prev_struct
	p.cur_method_is_static = prev_method_is_static

	mut all_ids := []flat.NodeId{cap: param_ids.len + body_ids.len}
	for id in param_ids {
		all_ids << id
	}
	for id in body_ids {
		all_ids << id
	}
	start := p.add_children(all_ids)
	id := p.a.add_node(flat.Node{
		kind:           .fn_decl
		op:             if is_pub { .arrow } else { .none }
		value:          name
		typ:            ret_type
		pos:            source_token_pos(name_pos)
		generic_params: generic_params
		children_start: start
		children_count: flat.child_count(all_ids.len)
	})
	p.register_pending_export(name)
	p.register_pending_noreturn(name)
	return id
}

// source_token_pos stores a deterministic source-local byte offset. Each input file has its own
// token file set, so the offset is encoded with the same one-byte base used by token.File.pos.
fn source_token_pos(offset int) token.Pos {
	return token.Pos{
		offset: offset + 1
		id:     offset + 1
	}
}

fn method_receiver_type_name(receiver_type string) string {
	mut clean := receiver_type.trim_space()
	for {
		if clean.starts_with('&') {
			clean = clean[1..].trim_space()
			continue
		}
		if clean.starts_with('shared ') {
			clean = clean[7..].trim_space()
			continue
		}
		break
	}
	return clean
}

fn (mut p Parser) mark_disabled_fn(name string) {
	if name.len == 0 {
		return
	}
	if p.cur_module.len > 0 && p.cur_module != 'main' && p.cur_module != 'builtin' {
		qname := if name.starts_with('${p.cur_module}.') { name } else { '${p.cur_module}.${name}' }
		p.a.disabled_fns[qname] = true
	}
	p.a.disabled_fns[name] = true
}

// register_pending_noreturn records a `@[noreturn]` function so the checker's
// missing-return analysis treats calls to it as terminating.
fn (mut p Parser) register_pending_noreturn(name string) {
	if !p.pending_noreturn || name.len == 0 {
		p.pending_noreturn = false
		return
	}
	if p.cur_module.len > 0 && p.cur_module != 'main' && p.cur_module != 'builtin'
		&& !name.starts_with('${p.cur_module}.') {
		p.a.noreturn_fns['${p.cur_module}.${name}'] = true
	}
	p.a.noreturn_fns[name] = true
	p.pending_noreturn = false
}

fn (mut p Parser) register_pending_export(name string) {
	if p.pending_export.len == 0 || name.len == 0 {
		p.pending_export = ''
		return
	}
	qname := if p.cur_module.len > 0 && p.cur_module != 'main' && p.cur_module != 'builtin'
		&& !name.starts_with('${p.cur_module}.') {
		'${p.cur_module}.${name}'
	} else {
		name
	}
	if name in p.a.disabled_fns || qname in p.a.disabled_fns {
		p.pending_export = ''
		return
	}
	p.a.export_fn_names[qname] = p.pending_export
	p.export_records << ExportRecord{
		name:  name
		qname: qname
		value: p.pending_export
	}
	p.pending_export = ''
}

fn (mut p Parser) parse_param_group(is_c_decl bool) []flat.NodeId {
	mut ids := []flat.NodeId{}
	mut names := []string{}
	mut is_mut := false
	mut is_shared := false
	if p.tok == .key_mut {
		is_mut = true
		p.next()
	}
	if p.tok == .key_shared {
		is_shared = true
		p.next()
	}
	// variadic ...Type (no param name)
	if p.tok == .ellipsis {
		typ := p.parse_type_name()
		ids << p.a.add_node(flat.Node{
			kind:   .param
			value:  ''
			typ:    typ
			is_mut: is_mut
		})
		if p.tok == .comma {
			p.next()
		}
		return ids
	}
	if is_c_decl && p.c_anon_param_starts_type() {
		mut typ := p.parse_type_name()
		if is_mut && !typ.starts_with('&') {
			typ = '&' + typ
		}
		if is_shared {
			typ = 'shared ' + typ
		}
		ids << p.a.add_node(flat.Node{
			kind:   .param
			value:  ''
			typ:    typ
			is_mut: is_mut
		})
		if p.tok == .comma {
			p.next()
		}
		return ids
	}
	names << p.lit
	p.next()
	for p.tok == .comma {
		p.next()
		if p.tok == .key_mut {
			p.next()
		}
		if p.tok == .name {
			names << p.lit
			p.next()
		}
	}
	mut typ := p.parse_type_name()
	if is_mut && !typ.starts_with('&') {
		typ = '&' + typ
	}
	if is_shared {
		typ = 'shared ' + typ
	}
	for name in names {
		ids << p.a.add_node(flat.Node{
			kind:   .param
			value:  name
			typ:    typ
			is_mut: is_mut
		})
	}
	if p.tok == .comma {
		p.next()
	}
	return ids
}

fn (mut p Parser) c_anon_param_starts_type() bool {
	if p.tok in [.amp, .and, .question, .not, .lsbr, .lpar, .key_fn, .key_atomic, .key_struct] {
		return true
	}
	if p.tok == .name {
		if p.lit == '&' {
			return true
		}
		pk := p.peek()
		if pk == .dot || pk == .rpar || pk == .eof {
			return true
		}
		if pk == .comma && (is_builtin_type(p.lit) || is_c_anon_simple_type_name(p.lit)) {
			return true
		}
	}
	return false
}

fn (mut p Parser) struct_decl() flat.NodeId {
	is_union := p.tok == .key_union
	is_params := p.pending_params
	is_soa := p.pending_soa
	p.pending_params = false
	p.pending_soa = false
	p.next() // skip 'struct' or 'union'
	mut name := p.expect(.name)
	if (name == 'C' || name == 'JS') && p.tok == .dot {
		for p.tok == .dot {
			p.next()
			name += '.' + p.expect_name_or_keyword()
		}
	}
	local_scope := p.current_local_type_scope()
	if local_scope.len > 0 && !name.contains('.') {
		name = p.declare_local_type_name(name, local_scope)
	}
	// generic params — skip
	mut is_generic := false
	mut generic_params := []string{}
	if p.tok == .lsbr {
		generic_params = p.parse_generic_param_names()
		is_generic = generic_params.len > 0
	}
	// implements clause
	mut implements_types := []string{}
	if p.tok == .name && p.lit == 'implements' {
		p.next()
		implements_types << p.parse_type_name()
		for p.tok == .comma {
			p.next()
			implements_types << p.parse_type_name()
		}
	}
	// no body (C struct forward decl)
	if p.tok != .lcbr {
		if p.tok == .semicolon {
			p.next()
		}
		return p.a.add_node(flat.Node{
			kind:           .struct_decl
			value:          name
			typ:            struct_decl_typ(is_union, is_generic, is_params, is_soa,
				implements_types)
			generic_params: generic_params
		})
	}
	p.check(.lcbr)
	mut ids := []flat.NodeId{}
	// Track the current `pub:`/`mut:` section and any leading attributes so each field's real
	// mutability/visibility/attrs are recorded on its `field_decl` node for `$for` reflection.
	mut sect_is_pub := false
	mut sect_is_mut := false
	mut pending_attrs := []string{}
	for p.tok != .rcbr && p.tok != .eof {
		// access modifiers
		if p.tok == .key_pub {
			peek_tok := p.peek()
			peek_lit := p.peek_lit
			if peek_tok == .colon || peek_tok == .key_mut
				|| (peek_tok == .name && peek_lit == 'module_mut') {
				p.next()
				mut section_mut := false
				if p.tok == .key_mut {
					section_mut = true
					p.next()
				}
				if p.tok == .name && p.lit == 'module_mut' {
					section_mut = true
					p.next()
				}
				if p.tok == .colon {
					p.next()
				}
				sect_is_pub = true
				sect_is_mut = section_mut
				continue
			}
		}
		if p.tok == .key_mut {
			if p.peek() == .colon {
				p.next()
				p.next()
				sect_is_pub = false
				sect_is_mut = true
				continue
			}
		}
		if p.tok == .key_global {
			if p.peek() == .colon {
				p.next()
				p.next()
				sect_is_pub = true
				sect_is_mut = true
				continue
			}
		}
		if p.tok == .semicolon {
			p.next()
			continue
		}
		// comptime $if in struct fields
		if p.tok == .dollar {
			id := p.parse_comptime_if()
			if int(id) >= 0 {
				ids << id
			}
			continue
		}
		// leading attributes apply to the next field
		if p.tok == .attribute {
			pending_attrs << p.parse_field_attrs()
			continue
		}
		// field: name type [= default] [@[attrs]]
		if p.tok == .name || p.tok.is_keyword() {
			field_name := p.expect_name_or_keyword()
			if p.tok == .dot {
				p.next()
				second := p.expect_name_or_keyword()
				full_type := '${field_name}.${second}'
				field_type := full_type + p.parse_type_generic_suffix()
				fid := p.a.add_node(flat.Node{
					kind:  .field_decl
					value: full_type
					typ:   field_type
				})
				p.apply_field_meta(fid, sect_is_mut, sect_is_pub, pending_attrs)
				pending_attrs = []string{}
				ids << fid
				if p.tok == .semicolon {
					p.next()
				}
				continue
			}
			// embedded struct (type on its own line, followed by semicolon)
			if p.tok == .semicolon || p.tok == .rcbr {
				embedded_type := p.resolve_local_type_name(field_name)
				fid := p.a.add_node(flat.Node{
					kind:  .field_decl
					value: embedded_type
					typ:   embedded_type
				})
				p.apply_field_meta(fid, sect_is_mut, sect_is_pub, pending_attrs)
				pending_attrs = []string{}
				ids << fid
				if p.tok == .semicolon {
					p.next()
				}
				continue
			}
			// grouped fields: x, y int
			if p.tok == .comma {
				mut names := []string{}
				names << field_name
				for p.tok == .comma {
					p.next()
					names << p.expect_name_or_keyword()
				}
				field_type := p.parse_type_name()
				mut group_attrs := pending_attrs.clone()
				pending_attrs = []string{}
				if p.tok == .attribute || p.tok == .lsbr {
					group_attrs << p.parse_field_attrs()
				}
				for n in names {
					fid := p.a.add_node(flat.Node{
						kind:  .field_decl
						value: n
						typ:   field_type
					})
					p.apply_field_meta(fid, sect_is_mut, sect_is_pub, group_attrs)
					ids << fid
				}
				if p.tok == .semicolon {
					p.next()
				}
				continue
			}
			// For embedded structs followed by access modifier or another field,
			// check if the next token could be a type
			field_type := p.parse_type_name()
			mut default_id := flat.empty_node
			// default value
			if p.tok == .assign {
				p.next()
				default_id = p.expr(.lowest)
			}
			mut children_start := 0
			mut children_count := 0
			if int(default_id) >= 0 {
				children_start = p.add_child(default_id)
				children_count = 1
			}
			// trailing field attributes
			mut fattrs := pending_attrs.clone()
			pending_attrs = []string{}
			if p.tok == .attribute || p.tok == .lsbr {
				fattrs << p.parse_field_attrs()
			}
			fid := p.a.add_node(flat.Node{
				kind:           .field_decl
				value:          field_name
				typ:            field_type
				children_start: children_start
				children_count: flat.child_count(children_count)
			})
			p.apply_field_meta(fid, sect_is_mut, sect_is_pub, fattrs)
			ids << fid
			if p.tok == .semicolon {
				p.next()
			}
		} else {
			p.next()
		}
	}
	p.check(.rcbr)
	p.pending_params = false
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .struct_decl
		value:          name
		typ:            struct_decl_typ(is_union, is_generic, is_params, is_soa, implements_types)
		generic_params: generic_params
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn struct_decl_typ(is_union bool, is_generic bool, is_params bool, is_soa bool, implements_types []string) string {
	mut parts := []string{}
	if is_union {
		parts << 'union'
	}
	if is_generic {
		parts << 'generic'
	}
	if is_params {
		parts << 'params'
	}
	if is_soa {
		parts << 'soa'
	}
	$if ownership ? {
		if implements_types.len > 0 {
			parts << 'implements=' + implements_types.join('|')
		}
	}
	return parts.join(',')
}

fn (mut p Parser) global_decl() flat.NodeId {
	p.next() // skip '__global'
	is_grouped := p.tok == .lpar
	if is_grouped {
		p.next()
	}
	mut ids := []flat.NodeId{}
	for {
		if p.tok == .semicolon {
			p.next()
			if !is_grouped {
				break
			}
			if p.tok == .rpar {
				p.next()
				if p.tok == .semicolon {
					p.next()
				}
				break
			}
			continue
		}
		if p.tok == .key_pub {
			p.next()
		}
		if p.tok == .key_mut {
			p.next()
		}
		if p.tok == .name || p.tok.is_keyword() {
			gname := p.expect_name_or_keyword()
			// handle qualified names: C.errno
			mut full_name := gname
			for p.tok == .dot {
				p.next()
				full_name += '.' + p.expect_name_or_keyword()
			}
			mut gtype := ''
			mut val_id := flat.empty_node
			if p.tok != .assign {
				gtype = p.parse_type_name()
			}
			if p.tok == .assign {
				// global with initializer: __global name = expr, or __global name Type = expr
				p.next()
				val_id = p.expr(.lowest)
			}
			if int(val_id) >= 0 {
				vstart := p.add_child(val_id)
				ids << p.a.add_node(flat.Node{
					kind:           .field_decl
					value:          full_name
					typ:            gtype
					children_start: vstart
					children_count: 1
				})
			} else {
				ids << p.a.add_node(flat.Node{
					kind:  .field_decl
					value: full_name
					typ:   gtype
				})
			}
			if p.tok == .semicolon {
				p.next()
			}
			if !is_grouped {
				break
			}
			if is_grouped && p.tok == .rpar {
				p.next()
				if p.tok == .semicolon {
					p.next()
				}
				break
			}
		} else if p.tok == .rpar {
			p.next()
			if p.tok == .semicolon {
				p.next()
			}
			break
		} else {
			p.next()
			if !is_grouped {
				break
			}
		}
	}
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .global_decl
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) const_decl() flat.NodeId {
	p.next() // skip 'const'
	is_grouped := p.tok == .lpar
	if is_grouped {
		p.next()
	}
	mut ids := []flat.NodeId{}
	for {
		if p.tok == .semicolon {
			p.next()
			if !is_grouped {
				break
			}
			if p.tok == .rpar {
				p.next()
				if p.tok == .semicolon {
					p.next()
				}
				break
			}
			continue
		}
		if p.tok == .name {
			cname := p.expect_name()
			mut full_name := cname
			// C.NAME
			if full_name == 'C' && p.tok == .dot {
				p.next()
				full_name = 'C.' + p.expect_name_or_keyword()
			}
			if p.tok == .assign {
				p.next()
				val_id := p.expr(.lowest)
				if value := p.comptime_node_value(val_id) {
					p.comptime_const_values[comptime_const_value_key(p.cur_module, full_name)] = value
				}
				vstart := p.add_child(val_id)
				ids << p.a.add_node(flat.Node{
					kind:           .const_field
					value:          full_name
					children_start: vstart
					children_count: 1
				})
			} else {
				// const with type only (header files)
				ctype := p.parse_type_name()
				ids << p.a.add_node(flat.Node{
					kind:  .const_field
					value: full_name
					typ:   ctype
				})
			}
			if p.tok == .semicolon {
				p.next()
			}
			if !is_grouped {
				break
			}
			if is_grouped && p.tok == .rpar {
				p.next()
				if p.tok == .semicolon {
					p.next()
				}
				break
			}
		} else if p.tok == .rpar {
			p.next()
			if p.tok == .semicolon {
				p.next()
			}
			break
		} else {
			p.next()
			if !is_grouped {
				break
			}
		}
	}
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .const_decl
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) enum_decl() flat.NodeId {
	p.next() // skip 'enum'
	name := p.expect(.name)
	mut enum_base_type := ''
	// `as` type
	if p.tok == .key_as {
		p.next()
		enum_base_type = p.parse_type_name()
	}
	p.check(.lcbr)
	mut ids := []flat.NodeId{}
	mut pending_attrs := []string{}
	for p.tok != .rcbr && p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		// Leading attributes apply to the next enum field.
		if p.tok == .attribute || p.tok == .lsbr {
			pending_attrs << p.parse_field_attrs()
			continue
		}
		field_name := p.expect_name_or_keyword()
		mut attrs := pending_attrs.clone()
		pending_attrs = []string{}
		mut children_start := 0
		mut children_count := flat.child_count(0)
		if p.tok == .assign {
			p.next()
			val_id := p.expr(.lowest)
			children_start = p.add_child(val_id)
			children_count = 1
		}
		if p.tok == .attribute || p.tok == .lsbr {
			attrs << p.parse_field_attrs()
		}
		mut field := flat.Node{
			kind:           .enum_field
			value:          field_name
			children_start: children_start
			children_count: children_count
		}
		if attrs.len > 0 {
			field.generic_params = attrs
		}
		ids << p.a.add_node(field)
		if p.tok == .semicolon {
			p.next()
		}
	}
	p.check(.rcbr)
	start := p.add_children(ids)
	mut typ := ''
	if p.pending_flag {
		typ = 'flag'
		p.pending_flag = false
	}
	mut enum_node := flat.Node{
		kind:           .enum_decl
		value:          name
		typ:            typ
		children_start: start
		children_count: flat.child_count(ids.len)
	}
	// generic_params[0] is the backing type (empty when unspecified); a trailing
	// `json_as_number` marker records the `@[json_as_number]` enum attribute so the
	// json fast path can encode members as their numeric value.
	if p.pending_json_as_number {
		enum_node.generic_params = [enum_base_type, 'json_as_number']
		p.pending_json_as_number = false
	} else if enum_base_type.len > 0 {
		enum_node.generic_params = [enum_base_type]
	}
	return p.a.add_node(enum_node)
}

fn (mut p Parser) type_decl() flat.NodeId {
	p.next() // skip 'type'
	// C. or JS. prefix
	if p.tok == .name && (p.lit == 'C' || p.lit == 'JS') {
		p.next()
		if p.tok == .dot {
			p.next()
		}
	}
	name := p.expect_name()
	// generic params
	mut generic_params := []string{}
	if p.tok == .lsbr {
		generic_params = p.parse_generic_param_names()
	}
	p.expect(.assign)
	first_type := p.parse_type_name()
	// check for sum type: type T = A | B | C
	// skip auto-semicolon before pipe
	if p.tok == .pipe || (p.tok == .semicolon && p.peek_is(token.Token.pipe)) {
		mut variants := []flat.NodeId{}
		variants << p.a.add_val(.ident, first_type)
		for p.tok == .pipe || (p.tok == .semicolon && p.peek_is(token.Token.pipe)) {
			if p.tok == .semicolon {
				p.next()
			}
			p.next() // skip |
			variant_type := p.parse_type_name()
			variants << p.a.add_val(.ident, variant_type)
		}
		if p.tok == .semicolon {
			p.next()
		}
		start := p.add_children(variants)
		return p.a.add_node(flat.Node{
			kind:           .type_decl
			value:          name
			generic_params: generic_params
			children_start: start
			children_count: flat.child_count(variants.len)
		})
	}
	if p.tok == .semicolon {
		p.next()
	}
	// type alias
	return p.a.add_node(flat.Node{
		kind:           .type_decl
		value:          name
		typ:            first_type
		generic_params: generic_params
	})
}

fn (mut p Parser) interface_decl() flat.NodeId {
	p.next() // skip 'interface'
	mut name := p.expect(.name)
	for p.tok == .dot {
		p.next()
		name += '.' + p.expect(.name)
	}
	// generic params
	mut generic_params := []string{}
	if p.tok == .lsbr {
		generic_params = p.parse_generic_param_names()
	}
	p.check(.lcbr)
	mut ids := []flat.NodeId{}
	for p.tok != .rcbr && p.tok != .eof {
		if p.tok == .key_mut {
			p.next()
			if p.tok == .colon {
				p.next()
			}
			continue
		}
		if p.tok == .semicolon {
			p.next()
			continue
		}
		mut field_name := p.expect_name_or_keyword()
		for p.tok == .dot {
			p.next()
			field_name += '.' + p.expect_name_or_keyword()
		}
		if p.tok == .lsbr && p.peek() == .xor {
			// Lifetime param list on an interface method: `name[^a](...)`. v3 erases
			// lifetimes, so consume and drop the `[^a]` list; without this the following
			// `(` is not seen as the method parameter list and the name is mis-parsed as a
			// `[^a]`-typed (fixed-array) interface field, which then fails in cgen.
			p.parse_generic_param_names()
		}
		if p.tok == .lpar {
			// method: name(params) ret_type
			p.next() // skip (
			mut params := []flat.NodeId{}
			for p.tok != .rpar && p.tok != .eof {
				mut param_is_mut := false
				if p.tok == .key_mut {
					param_is_mut = true
					p.next()
				}
				// Interface method params may be named (e.g. `seed_data []u32`,
				// `node &ast.Node`) or type-only (`[]u32`). When the current token
				// is a plain identifier and the next token starts a type, that
				// identifier is the param name; consume it first so the array/pointer
				// prefix of the real type is not mis-parsed onto the name
				// (e.g. `seed_data[]`). `map`/`chan` are type heads, not names.
				if p.tok == .name && p.lit != 'map' && p.lit != 'chan' {
					nt := p.peek()
					if nt == .name || nt == .amp || nt == .question || nt == .not
						|| nt == .key_fn || nt == .ellipsis
						|| (nt == .lsbr && p.peek_lbr_starts_array_type()) {
						p.next()
					}
				}
				mut ptype := p.parse_type_name()
				// `mut` params are references, exactly like fn decls record them
				// (parse_param_group), so implementation signatures compare equal.
				if param_is_mut && !ptype.starts_with('&') {
					ptype = '&' + ptype
				}
				params << p.a.add_node(flat.Node{
					kind: .param
					typ:  ptype
					op:   if param_is_mut { .amp } else { .none }
				})
				if p.tok == .comma {
					p.next()
				}
			}
			p.check(.rpar) // skip )
			mut ret_type := ''
			if p.tok != .semicolon && p.tok != .rcbr && p.tok != .eof {
				ret_type = p.parse_type_name()
			}
			start := p.add_children(params)
			ids << p.a.add_node(flat.Node{
				kind:           .interface_field
				op:             .dot
				value:          field_name
				typ:            ret_type
				children_start: start
				children_count: flat.child_count(params.len)
			})
		} else if p.tok == .semicolon || p.tok == .rcbr {
			// embedded type or field without explicit type
			ids << p.a.add_node(flat.Node{
				kind:  .interface_field
				value: field_name
			})
		} else {
			// field: name type
			ftype := p.parse_type_name()
			ids << p.a.add_node(flat.Node{
				kind:  .interface_field
				value: field_name
				typ:   ftype
			})
		}
		if p.tok == .semicolon {
			p.next()
		}
	}
	p.check(.rcbr)
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .interface_decl
		value:          name
		generic_params: generic_params
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) import_stmt() flat.NodeId {
	p.next() // skip 'import'
	mut name := p.expect_name()
	mut alias := name
	mut selective_ids := []flat.NodeId{}
	for p.tok == .dot {
		p.next()
		alias = p.expect_name()
		name += '.' + alias
	}
	if p.tok == .key_as {
		p.next()
		alias = p.expect_name()
	}
	// selective import: import mod { sym1, sym2 }
	if p.tok == .lcbr {
		p.next()
		for p.tok != .rcbr && p.tok != .eof {
			sym := p.expect_name_or_keyword()
			selective_ids << p.a.add_node(flat.Node{
				kind:  .ident
				value: sym
			})
			if p.tok == .comma {
				p.next()
			}
		}
		p.check(.rcbr)
	}
	if p.tok == .semicolon {
		p.next()
	}
	return p.a.add_node(flat.Node{
		kind:           .import_decl
		value:          name
		typ:            alias
		children_start: p.add_children(selective_ids)
		children_count: flat.child_count(selective_ids.len)
	})
}

fn (mut p Parser) module_stmt() flat.NodeId {
	p.next() // skip 'module'
	name := p.expect_name()
	if p.tok == .semicolon {
		p.next()
	}
	return p.a.add_node(flat.Node{
		kind:  .module_decl
		value: name
	})
}

fn (mut p Parser) directive() flat.NodeId {
	mut full := p.lit
	p.next() // skip '#' (lit already contains the full line)
	if full.starts_with('#') {
		full = full[1..]
	}
	mut name := full
	mut value := ''
	space_idx := full.index_u8(` `)
	if space_idx > 0 {
		name = full[..space_idx]
		value = full[space_idx + 1..].trim_space()
	}
	if p.tok == .semicolon {
		p.next()
	}
	return p.a.add_node(flat.Node{
		kind:  .directive
		value: name
		typ:   value
	})
}

fn (mut p Parser) skip_attrs() {
	if p.tok == .attribute {
		p.next()
		if p.tok == .key_if {
			p.next()
			if !eval_comptime_cond(p.prefs, p.parse_attribute_comptime_cond()) {
				p.skip_next_decl = true
			}
		}
		for p.tok != .rsbr && p.tok != .eof {
			if p.tok == .name {
				if p.lit == 'flag' {
					p.pending_flag = true
				} else if p.lit == 'json_as_number' {
					p.pending_json_as_number = true
				} else if p.lit == 'params' {
					p.pending_params = true
				} else if p.lit == 'noreturn' {
					p.pending_noreturn = true
				} else if p.lit == 'soa' {
					p.pending_soa = true
				} else if p.lit == 'export' {
					p.try_parse_export_attr()
					continue
				}
			}
			p.next()
		}
		p.check(.rsbr)
		return
	}
	if p.tok == .lsbr {
		mut depth := 1
		p.next()
		for depth > 0 && p.tok != .eof {
			if depth == 1 && p.tok == .name {
				if p.lit == 'flag' {
					p.pending_flag = true
				} else if p.lit == 'json_as_number' {
					p.pending_json_as_number = true
				} else if p.lit == 'params' {
					p.pending_params = true
				} else if p.lit == 'noreturn' {
					p.pending_noreturn = true
				} else if p.lit == 'soa' {
					p.pending_soa = true
				} else if p.lit == 'export' {
					p.try_parse_export_attr()
					continue
				}
			}
			if p.tok == .lsbr {
				depth++
			} else if p.tok == .rsbr {
				depth--
			}
			p.next()
		}
	}
}

fn (mut p Parser) parse_attribute_comptime_cond() string {
	mut cond := strings.new_builder(32)
	for p.tok != .rsbr && p.tok != .eof {
		raw_tok_str := p.comptime_cond_token_text()
		tok_str := if raw_tok_str.starts_with('@') {
			p.resolve_comptime_at_values_at(raw_tok_str, p.tok_pos)
		} else {
			raw_tok_str
		}
		if cond.len > 0 && tok_str != '?' {
			cond.write_string(' ')
		}
		cond.write_string(tok_str)
		p.next()
	}
	return cond.str()
}

// apply_field_meta records a struct field's mutability, visibility, and attributes on its
// `field_decl` node so `$for field in T.fields` reflection can expose `field.is_mut`,
// `field.is_pub`, and `field.attrs`. Everything is packed into the otherwise-unused
// `generic_params` (element 0 is a flag string: `m` = mut, `p` = pub; the rest are the
// attributes) - the node's own `kind_id`/`is_mut` are load-bearing (kind dispatch) and must not
// be repurposed. A default field (private, immutable, no attrs) is left untouched so it costs no
// allocation and reads back as the default.
fn (mut p Parser) apply_field_meta(id flat.NodeId, is_mut bool, is_pub bool, attrs []string) {
	if int(id) < 0 || int(id) >= p.a.nodes.len {
		return
	}
	if !is_mut && !is_pub && attrs.len == 0 {
		return
	}
	mut flags := ''
	if is_mut {
		flags += 'm'
	}
	if is_pub {
		flags += 'p'
	}
	mut gp := []string{cap: attrs.len + 1}
	gp << flags
	gp << attrs
	p.a.nodes[int(id)].generic_params = gp
}

// attr_unquote strips a single pair of surrounding quotes from an attribute key (unlike
// strip_quotes, it does not treat a leading `r` as a raw-string prefix).
fn attr_unquote(s string) string {
	if s.len >= 2 && (s[0] == `'` || s[0] == `"`) && s[s.len - 1] == s[0] {
		return s[1..s.len - 1]
	}
	return s
}

fn parsed_attribute_kind(tok token.Token) int {
	return match tok {
		.string { 1 }
		.number { 2 }
		.key_true, .key_false { 3 }
		else { 0 }
	}
}

// parse_field_attrs captures the attributes on a struct field (`@[json: 'x']`, `@[required]`,
// `@[sql: serial]`) as their `key` / `key: value` string forms, mirroring V's `FieldData.attrs`.
fn (mut p Parser) parse_field_attrs() []string {
	return p.parse_field_attrs_with_kinds().attrs
}

// parse_field_attrs_with_kinds also retains the structured VAttribute kind. Declaration
// reflection needs this sidecar because the legacy string form intentionally unquotes `@['x']`.
// A comptime `@[if cond]` guard keeps its existing evaluation semantics and is not exposed as an
// attribute. Regardless of how the content splits, the loop always consumes through the closing
// `]`, so parsing stays correct even for attribute forms it does not fully model.
fn (mut p Parser) parse_field_attrs_with_kinds() ParsedFieldAttrs {
	mut attrs := []string{}
	mut kinds := []int{}
	for p.tok == .attribute || p.tok == .lsbr {
		p.next() // consume `@[` / `[`
		if p.tok == .key_if {
			p.next()
			if !eval_comptime_cond(p.prefs, p.parse_attribute_comptime_cond()) {
				p.skip_next_decl = true
			}
			p.check(.rsbr)
			continue
		}
		for p.tok != .rsbr && p.tok != .eof {
			if p.tok == .comma || p.tok == .semicolon {
				p.next()
				continue
			}
			piece_kind := parsed_attribute_kind(p.tok)
			mut piece := attr_unquote(p.lit)
			p.next()
			if p.tok == .lpar {
				attr_name := piece
				mut has_base_arg := false
				p.next()
				for p.tok != .rpar && p.tok != .eof {
					if p.tok == .comma {
						p.next()
						continue
					}
					mut arg_name := ''
					if p.tok == .name && p.peek() == .colon {
						arg_name = p.lit
						p.next()
						p.check(.colon)
					}
					arg_kind := parsed_attribute_kind(p.tok)
					arg := p.lit.trim_space()
					p.next()
					if arg_name.len == 0 || arg_name == 'msg' {
						attrs << '${attr_name}: ${arg}'
						kinds << arg_kind
						has_base_arg = true
					} else {
						attrs << '${attr_name}_${arg_name}: ${arg}'
						kinds << arg_kind
					}
				}
				p.check(.rpar)
				if !has_base_arg {
					attrs << attr_name
					kinds << piece_kind
				}
				continue
			}
			mut kind := piece_kind
			if p.tok == .colon {
				p.next()
				kind = parsed_attribute_kind(p.tok)
				piece += ': ' + p.lit.trim_space()
				p.next()
			}
			piece = piece.trim_space()
			if piece.len > 0 {
				attrs << piece
				kinds << kind
			}
		}
		p.check(.rsbr)
	}
	return ParsedFieldAttrs{
		attrs: attrs
		kinds: kinds
	}
}

fn (mut p Parser) try_parse_export_attr() {
	p.next()
	if p.tok != .colon {
		return
	}
	p.next()
	if int(p.tok) != int(token.Token.string) {
		return
	}
	p.pending_export = strip_quotes(p.lit)
	p.next()
}

fn (mut p Parser) skip_top_level_stmt() {
	// A newline after the attribute group (`@[if flag ?]`) yields an auto-inserted
	// semicolon before the declaration; skip it (and any further attributes) so the
	// declaration itself — not just the empty statement — is skipped.
	for p.tok == .attribute || p.tok == .lsbr || p.tok == .semicolon {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		p.skip_attrs()
	}
	if p.tok == .key_pub {
		p.next()
	}
	match p.tok {
		.key_fn {
			p.next()
			for p.tok != .lcbr && p.tok != .semicolon && p.tok != .eof {
				p.next()
			}
			if p.tok == .lcbr {
				p.skip_block()
			} else if p.tok == .semicolon {
				p.next()
			}
		}
		.key_struct, .key_union, .key_enum, .key_interface, .key_const, .key_global {
			for p.tok != .lcbr && p.tok != .semicolon && p.tok != .eof {
				p.next()
			}
			if p.tok == .lcbr {
				p.skip_block()
			} else if p.tok == .semicolon {
				p.next()
			}
		}
		else {
			for p.tok != .semicolon && p.tok != .eof {
				p.next()
			}
			if p.tok == .semicolon {
				p.next()
			}
		}
	}
}

fn (mut p Parser) parse_comptime_if() flat.NodeId {
	p.next() // skip $
	if p.tok != .key_if {
		if p.tok == .key_for {
			return p.parse_comptime_for()
		}
		if p.tok == .key_match {
			return p.parse_comptime_match(false)
		}
		if p.tok == .name && p.lit == 'compile_error' {
			return p.parse_compile_error_stmt()
		}
		// other comptime — skip
		for p.tok != .semicolon && p.tok != .eof {
			p.next()
		}
		if p.tok == .semicolon {
			p.next()
		}
		return flat.empty_node
	}
	p.next() // skip 'if'
	mut cond :=
		p.resolve_comptime_const_values(p.resolve_comptime_at_values(p.parse_comptime_cond()))
	// Only defer conditions that need information unavailable at parse time: a `$for` loop var
	// (`field.typ`, `field.indirections`, `value.value`), known once the loop is unrolled, or a
	// type test (`T is int`), known after monomorphization. Ordinary platform/custom flags
	// (`$if linux`) must still be evaluated here — the transformer only folds loop-var/type
	// conditions and the C backend drops any `.comptime_if` that survives.
	if p.comptime_cond_needs_loop_var(cond) || comptime_cond_has_type_test(cond)
		|| comptime_cond_has_type_metadata(cond) || comptime_cond_has_builtin_threads(cond) {
		cond = p.simplify_deferred_comptime_cond(cond)
		if p.comptime_cond_needs_loop_var(cond) || comptime_cond_has_type_test(cond)
			|| comptime_cond_has_type_metadata(cond) || comptime_cond_has_builtin_threads(cond) {
			then_block := p.block_stmt()
			else_block := p.parse_comptime_else()
			return p.comptime_if_node(cond, then_block, else_block)
		}
	}
	taken := eval_comptime_cond(p.prefs, cond)
	if taken {
		result := p.block_stmt()
		p.skip_comptime_else()
		return result
	} else {
		p.skip_block()
		return p.parse_comptime_else()
	}
}

fn (mut p Parser) parse_compile_error_stmt() flat.NodeId {
	call := p.parse_compile_error_call()
	return p.a.add_node(flat.Node{
		kind:           .expr_stmt
		children_start: p.add_child(call)
		children_count: 1
	})
}

fn (mut p Parser) parse_compile_error_call() flat.NodeId {
	p.next() // skip `compile_error`
	p.check(.lpar)
	message := if p.tok == .rpar {
		p.a.add_val_id(5, 'compile-time error')
	} else {
		p.expr(.lowest)
	}
	for p.tok != .rpar && p.tok != .eof {
		p.next()
	}
	p.check(.rpar)
	if p.tok == .semicolon {
		p.next()
	}
	return p.make_compile_error_call(message)
}

fn (mut p Parser) make_compile_error_call(message flat.NodeId) flat.NodeId {
	// Keep this as a compiler-only sentinel call. The checker reports it immediately for a
	// selected concrete branch, while a deferred generic branch keeps it until specialization;
	// if that branch is selected, generic validation rejects the sentinel as well.
	callee := p.a.add_val(.ident, '__v_compile_error')
	start := p.add_children2(callee, message)
	return p.a.add_node(flat.Node{
		kind:           .call
		value:          '__v_compile_error'
		children_start: start
		children_count: 2
	})
}

fn (mut p Parser) parse_top_level_compile_error() flat.NodeId {
	call := p.parse_compile_error_call()
	return p.make_top_level_compile_error(call)
}

fn (mut p Parser) make_top_level_compile_error(call flat.NodeId) flat.NodeId {
	stmt := p.a.add_node(flat.Node{
		kind:           .expr_stmt
		children_start: p.add_child(call)
		children_count: 1
	})
	start := p.add_child(stmt)
	return p.a.add_node(flat.Node{
		kind:           .fn_decl
		value:          '__v_top_level_compile_error_${p.a.nodes.len}'
		typ:            'void'
		children_start: start
		children_count: 1
	})
}

// parse_comptime_for parses `$for <var> in <Type>.<kind> { body }` into a comptime_for node.
// The loop is unrolled later (once <Type> is concrete) by the transformer. `value` holds
// `<var>|<kind>` (e.g. `field|fields`); `typ` holds the base type so generic substitution can
// turn `T` into the concrete type during monomorphization.
fn (mut p Parser) parse_comptime_for() flat.NodeId {
	p.next() // skip 'for'
	val_var := p.expect_name()
	if p.tok == .key_in {
		p.next()
	} else if p.tok == .name && p.lit == 'in' {
		p.next()
	}
	mut segs := [p.expect_name()]
	for p.tok == .dot {
		p.next()
		segs << p.expect_name()
	}
	kind := if segs.len > 1 { segs.last() } else { 'fields' }
	base := if segs.len > 1 { segs[..segs.len - 1].join('.') } else { segs.last() }
	p.comptime_for_vars << val_var
	previous_method_var := p.comptime_method_var
	if kind == 'methods' {
		p.comptime_method_var = val_var
	}
	body := p.block_stmt()
	p.comptime_method_var = previous_method_var
	p.comptime_for_vars.pop()
	start := p.add_children([body])
	return p.a.add_node(flat.Node{
		kind:           .comptime_for
		value:          '${val_var}|${kind}'
		typ:            base
		children_start: start
		children_count: 1
	})
}

fn (mut p Parser) parse_top_level_comptime_if() flat.NodeId {
	p.next() // skip $
	if p.tok != .key_if {
		if p.tok == .key_match {
			return p.parse_comptime_match(true)
		}
		if p.tok == .name && p.lit == 'compile_error' {
			return p.parse_top_level_compile_error()
		}
		// $for or other comptime - skip
		if p.tok == .key_for {
			p.next()
			for p.tok != .lcbr && p.tok != .eof {
				p.next()
			}
			p.skip_block()
		} else {
			for p.tok != .semicolon && p.tok != .eof {
				p.next()
			}
			if p.tok == .semicolon {
				p.next()
			}
		}
		return flat.empty_node
	}
	p.next() // skip 'if'
	mut cond :=
		p.resolve_comptime_const_values(p.resolve_comptime_at_values(p.parse_comptime_cond()))
	if comptime_cond_has_type_test(cond) || comptime_cond_has_type_metadata(cond)
		|| comptime_cond_has_builtin_threads(cond) {
		cond = p.simplify_deferred_comptime_cond(cond)
		if comptime_cond_has_type_test(cond) || comptime_cond_has_type_metadata(cond)
			|| comptime_cond_has_builtin_threads(cond) {
			then_block := p.top_level_block_stmt()
			else_block := p.parse_top_level_comptime_else()
			return p.comptime_if_node(cond, then_block, else_block)
		}
	}
	taken := eval_comptime_cond(p.prefs, cond)
	if taken {
		result := p.top_level_block_stmt()
		p.skip_comptime_else()
		return result
	}
	p.skip_block()
	return p.parse_top_level_comptime_else()
}

// parse_comptime_match desugars `$match subj { pat1 { ... } pat2, pat3 { ... } $else { ... } }`
// into the equivalent `$if subj is pat1 { ... } $else $if ... $else { ... }` chain, reusing
// the comptime-if machinery (deferral, monomorph-time folding).
fn (mut p Parser) parse_comptime_match(is_top_level bool) flat.NodeId {
	p.next() // skip 'match'
	subject, subject_is_literal := p.parse_comptime_match_subject()
	for p.tok == .semicolon {
		p.next()
	}
	p.check(.lcbr)
	if subject_is_literal {
		return p.parse_known_comptime_match_value(subject, is_top_level)
	}
	if subject.starts_with('@') {
		return p.parse_known_comptime_match_value(p.resolve_comptime_at_values(subject),
			is_top_level)
	}
	if value := p.comptime_value(subject) {
		return p.parse_known_comptime_match_value(value, is_top_level)
	}
	mut conds := []string{}
	mut blocks := []flat.NodeId{}
	mut else_block := flat.empty_node
	for p.tok != .rcbr && p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		if (p.tok == .dollar && p.peek() == .key_else) || p.tok == .key_else {
			if p.tok == .dollar {
				p.next()
			}
			p.next() // skip 'else'
			else_block = if is_top_level { p.top_level_block_stmt() } else { p.block_stmt() }
			continue
		}
		mut pats := [p.parse_comptime_match_pattern()]
		for p.tok == .comma {
			p.next()
			pats << p.parse_comptime_match_pattern()
		}
		mut cond_parts := []string{cap: pats.len}
		for pat in pats {
			cond_parts << '${subject} is ${pat}'
		}
		conds << cond_parts.join(' || ')
		blocks << if is_top_level { p.top_level_block_stmt() } else { p.block_stmt() }
	}
	p.check(.rcbr)
	mut result := else_block
	for i := conds.len - 1; i >= 0; i-- {
		result = p.comptime_if_node(conds[i], blocks[i], result)
	}
	if int(result) < 0 {
		return flat.empty_node
	}
	return result
}

fn (mut p Parser) parse_comptime_match_subject() (string, bool) {
	if p.tok == .dollar {
		p.next()
		return '$' + p.expect_name_or_keyword(), false
	}
	if p.tok == .name && p.lit.starts_with('@') {
		name_pos := p.tok_pos
		name := p.expect_name()
		return p.resolve_comptime_at_values_at(name, name_pos), true
	}
	if p.tok == .string || p.tok == .char {
		value := p.comptime_cond_token_text()
		p.next()
		return value, true
	}
	if p.tok == .number {
		value := p.lit
		p.next()
		return value, true
	}
	if p.tok == .key_true || p.tok == .key_false {
		value := if p.tok == .key_true { 'true' } else { 'false' }
		p.next()
		return value, true
	}
	mut segs := [p.expect_name()]
	for p.tok == .dot {
		p.next()
		segs << p.expect_name()
	}
	return segs.join('.'), false
}

fn (mut p Parser) parse_known_comptime_match_value(value string, is_top_level bool) flat.NodeId {
	mut result := flat.empty_node
	mut matched := false
	for p.tok != .rcbr && p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		if (p.tok == .dollar && p.peek() == .key_else) || p.tok == .key_else {
			if p.tok == .dollar {
				p.next()
			}
			p.next()
			if matched {
				p.skip_block()
			} else {
				result = if is_top_level { p.top_level_block_stmt() } else { p.block_stmt() }
				matched = true
			}
			continue
		}
		mut pattern_matches := false
		for {
			pattern := p.parse_comptime_match_pattern()
			mut pattern_value := pattern
			if known_value := p.comptime_value(pattern) {
				pattern_value = known_value
			} else if pattern.starts_with('@') {
				pattern_value = p.resolve_comptime_at_values(pattern)
			}
			if comptime_cond_value(pattern_value) == comptime_cond_value(value) {
				pattern_matches = true
			}
			if p.tok != .comma {
				break
			}
			p.next()
		}
		if !matched && pattern_matches {
			result = if is_top_level { p.top_level_block_stmt() } else { p.block_stmt() }
			matched = true
		} else {
			p.skip_block()
		}
	}
	p.check(.rcbr)
	return result
}

fn (mut p Parser) parse_comptime_match_pattern() string {
	if p.tok == .dollar {
		p.next()
		return '$' + p.expect_name_or_keyword()
	}
	if p.tok == .name && p.lit.starts_with('@') {
		name_pos := p.tok_pos
		name := p.expect_name()
		return p.resolve_comptime_at_values_at(name, name_pos)
	}
	if p.tok == .string || p.tok == .char {
		value := p.comptime_cond_token_text()
		p.next()
		return value
	}
	if p.tok == .number {
		value := p.lit
		p.next()
		return value
	}
	if p.tok == .key_true || p.tok == .key_false {
		value := if p.tok == .key_true { 'true' } else { 'false' }
		p.next()
		return value
	}
	return p.parse_type_name()
}

fn (mut p Parser) comptime_if_node(cond string, then_block flat.NodeId, else_block flat.NodeId) flat.NodeId {
	mut ids := []flat.NodeId{cap: 2}
	if int(then_block) >= 0 {
		ids << then_block
	}
	if int(else_block) >= 0 {
		ids << else_block
	}
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .comptime_if
		value:          cond
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) top_level_block_stmt() flat.NodeId {
	ids := p.parse_top_level_block_body()
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .block
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) parse_top_level_block_body() []flat.NodeId {
	p.check(.lcbr)
	mut ids := []flat.NodeId{}
	for p.tok != .rcbr && p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		id := p.top_level_stmt()
		if int(id) >= 0 {
			ids << id
		}
	}
	p.check(.rcbr)
	return ids
}

fn (mut p Parser) parse_comptime_cond() string {
	mut cond := strings.new_builder(64)
	mut prev_tok_str := ''
	for p.tok != .lcbr && p.tok != .eof {
		raw_tok_str := p.comptime_cond_token_text()
		tok_str := if raw_tok_str.starts_with('@') {
			p.resolve_comptime_at_values_at(raw_tok_str, p.tok_pos)
		} else {
			raw_tok_str
		}
		if cond.len > 0 && comptime_cond_needs_space(prev_tok_str, tok_str) {
			cond.write_string(' ')
		}
		cond.write_string(tok_str)
		prev_tok_str = tok_str
		p.next()
	}
	return cond.str()
}

fn (mut p Parser) resolve_comptime_at_values(cond string) string {
	return p.resolve_comptime_at_values_at(cond, p.tok_pos)
}

fn (mut p Parser) resolve_comptime_at_values_at(cond string, pseudo_pos int) string {
	module_name := if p.cur_module.len > 0 { p.cur_module } else { 'main' }
	fn_name := p.cur_fn.all_after_last('.')
	method_name := if p.cur_struct.len > 0 { '${p.cur_struct}.${fn_name}' } else { fn_name }
	mut out := strings.new_builder(cond.len)
	mut i := 0
	mut quote := u8(0)
	for i < cond.len {
		c := cond[i]
		if quote != 0 {
			out.write_u8(c)
			if c == `\\` && i + 1 < cond.len {
				i++
				out.write_u8(cond[i])
			} else if c == quote {
				quote = 0
			}
			i++
			continue
		}
		if c == `'` || c == `"` {
			quote = c
			out.write_u8(c)
			i++
			continue
		}
		if c == `@` {
			start := i
			i++
			for i < cond.len && (cond[i].is_letter() || cond[i].is_digit() || cond[i] == `_`) {
				i++
			}
			name := cond[start..i]
			match name {
				'@FILE' {
					write_comptime_cond_string(mut out, os.real_path(p.cur_file))
				}
				'@DIR' {
					write_comptime_cond_string(mut out, os.real_path(os.dir(p.cur_file)))
				}
				'@VMODROOT' {
					write_comptime_cond_string(mut out,
						os.real_path(vmod_root_for_file(p.cur_file)))
				}
				'@VMOD_FILE' {
					vmod_file := os.join_path_single(vmod_root_for_file(p.cur_file), 'v.mod')
					content := os.read_file(vmod_file) or {
						message := p.a.add_val_id(5,
							'@VMOD_FILE can only be used in projects that have a v.mod file')
						call := p.make_compile_error_call(message)
						_ = p.make_top_level_compile_error(call)
						''
					}
					write_comptime_cond_string(mut out, content.replace('\r\n', '\n'))
				}
				'@VEXEROOT' {
					write_comptime_cond_string(mut out, p.prefs.vroot)
				}
				'@VEXE' {
					vexe := if p.prefs.vexe.len > 0 { p.prefs.vexe } else { p.prefs.vroot + '/v' }
					write_comptime_cond_string(mut out, vexe)
				}
				'@LINE' {
					write_comptime_cond_string(mut out, p.line_nr_for_pos(pseudo_pos).str())
				}
				'@FILE_LINE' {
					write_comptime_cond_string(mut out,
						'${os.real_path(p.cur_file)}:${p.line_nr_for_pos(pseudo_pos)}')
				}
				'@METHOD' {
					write_comptime_cond_string(mut out, method_name)
				}
				'@STRUCT' {
					write_comptime_cond_string(mut out, p.cur_struct)
				}
				'@MOD' {
					write_comptime_cond_string(mut out, module_name)
				}
				'@FN' {
					write_comptime_cond_string(mut out, fn_name)
				}
				'@LOCATION' {
					mut location_method := '${module_name}.${fn_name}'
					if p.cur_struct.len > 0 {
						if p.cur_method_is_static {
							location_method = '${module_name}.${p.cur_struct}.${fn_name} (static)'
						} else {
							location_method = '${module_name}.${p.cur_struct}{}.${fn_name}'
						}
					}
					write_comptime_cond_string(mut out,
						'${p.cur_file}:${p.line_nr_for_pos(pseudo_pos)}, ${location_method}')
				}
				'@BUILD_DATE' {
					write_comptime_cond_string(mut out, p.prefs.build_date)
				}
				'@BUILD_TIME' {
					write_comptime_cond_string(mut out, p.prefs.build_time)
				}
				'@BUILD_TIMESTAMP' {
					write_comptime_cond_string(mut out, p.prefs.build_timestamp)
				}
				'@OS' {
					write_comptime_cond_string(mut out, p.prefs.normalized_target_os())
				}
				'@CCOMPILER' {
					write_comptime_cond_string(mut out, @CCOMPILER)
				}
				'@BACKEND' {
					write_comptime_cond_string(mut out, p.prefs.backend)
				}
				'@PLATFORM' {
					write_comptime_cond_string(mut out, @PLATFORM)
				}
				'@VCURRENTHASH', '@VHASH' {
					write_comptime_cond_string(mut out, '')
				}
				else {
					out.write_string(name)
				}
			}

			continue
		}
		out.write_u8(c)
		i++
	}
	return out.str()
}

fn write_comptime_cond_string(mut out strings.Builder, value string) {
	out.write_u8(`'`)
	for i := 0; i < value.len; i++ {
		if value[i] == `\\` || value[i] == `'` {
			out.write_u8(`\\`)
		}
		out.write_u8(value[i])
	}
	out.write_u8(`'`)
}

// comptime_char_is_name_cont reports whether the byte at `pos` continues an identifier. Used
// to keep the word operators `!is`/`!in` from matching identifiers such as `!isset`/`!inner`.
fn comptime_char_is_name_cont(src string, pos int) bool {
	if pos < 0 || pos >= src.len {
		return false
	}
	c := src[pos]
	return c.is_letter() || c.is_digit() || c == `_`
}

fn (p &Parser) comptime_cond_token_text() string {
	if p.s.pos >= 0 && p.s.pos < p.s.src.len {
		c := p.s.src[p.s.pos]
		if c == `&` && p.s.pos + 1 < p.s.src.len && p.s.src[p.s.pos + 1] == `&` {
			return '&&'
		}
		if c == `|` && p.s.pos + 1 < p.s.src.len && p.s.src[p.s.pos + 1] == `|` {
			return '||'
		}
		if c == `!` && p.s.pos + 2 < p.s.src.len && p.s.src[p.s.pos + 1] == `i`
			&& p.s.src[p.s.pos + 2] == `s` && !comptime_char_is_name_cont(p.s.src, p.s.pos + 3) {
			return '!is'
		}
		if c == `!` && p.s.pos + 2 < p.s.src.len && p.s.src[p.s.pos + 1] == `i`
			&& p.s.src[p.s.pos + 2] == `n` && !comptime_char_is_name_cont(p.s.src, p.s.pos + 3) {
			return '!in'
		}
		if c == `!` && p.s.pos + 1 < p.s.src.len && p.s.src[p.s.pos + 1] == `=` {
			return '!='
		}
		if c == `!` {
			return '!'
		}
		if c == `?` {
			return '?'
		}
	}
	tok := p.tok
	if tok == .char {
		return comptime_cond_string_token_text(p.lit)
	}
	if tok == .string {
		return comptime_cond_string_token_text(p.lit)
	}
	if tok == .and {
		return '&&'
	}
	if tok == .logical_or {
		return '||'
	}
	if tok == .not {
		return '!'
	}
	if tok == .lpar {
		return '('
	}
	if tok == .rpar {
		return ')'
	}
	if tok == .key_is {
		return 'is'
	}
	if tok == .not_is {
		return '!is'
	}
	if tok == .eq {
		return '=='
	}
	if tok == .ne {
		return '!='
	}
	if tok == .lt {
		return '<'
	}
	if tok == .gt {
		return '>'
	}
	if tok == .le {
		return '<='
	}
	if tok == .ge {
		return '>='
	}
	if tok == .key_in {
		return 'in'
	}
	if tok == .not_in {
		return '!in'
	}
	if tok == .dollar {
		return '$'
	}
	if tok == .lsbr {
		return '['
	}
	if tok == .rsbr {
		return ']'
	}
	if tok == .dot {
		return '.'
	}
	if tok == .comma {
		return ','
	}
	if tok == .question {
		return '?'
	}
	if tok == .key_true {
		return 'true'
	}
	if tok == .key_false {
		return 'false'
	}
	if p.lit.len > 0 {
		return p.lit
	}
	return ''
}

fn comptime_cond_string_token_text(lit string) string {
	if lit.len >= 2 && lit[0] in [`'`, `"`] && lit[lit.len - 1] == lit[0] {
		return lit
	}
	if lit.len >= 3 && lit[0] == `r` && lit[1] in [`'`, `"`] && lit[lit.len - 1] == lit[1] {
		return comptime_cond_quoted_string(lit[2..lit.len - 1])
	}
	return comptime_cond_quoted_string(lit)
}

fn comptime_cond_quoted_string(value string) string {
	mut out := strings.new_builder(value.len + 2)
	write_comptime_cond_string(mut out, value)
	return out.str()
}

fn comptime_cond_needs_space(prev string, cur string) bool {
	if prev == 'is' || prev == '!is' {
		return true
	}
	if prev == '?' || prev == '!' {
		return false
	}
	if cur == '?' || cur == ']' || cur == '[' || cur == '.' || cur == ',' {
		return false
	}
	if prev == '$' || prev == '[' || prev == ']' || prev == '.' {
		return false
	}
	return true
}

fn comptime_cond_has_type_test(cond string) bool {
	return cond.contains(' is ') || cond.contains(' !is ') || cond.contains(' in[')
		|| cond.contains(' in [') || cond.contains(' !in[') || cond.contains(' !in [')
}

fn comptime_cond_has_type_metadata(cond string) bool {
	return cond.contains('.indirections')
}

// comptime_cond_has_builtin_threads reports whether a condition contains the bare builtin
// `threads` flag. Optional `threads?` flags and `$d('threads', ...)` defines are evaluated
// normally at parse time.
fn comptime_cond_has_builtin_threads(cond string) bool {
	c := comptime_cond_strip_outer_parens(cond.trim_space())
	left_or, right_or, has_or := comptime_cond_split_top_level(c, '||')
	if has_or {
		return comptime_cond_has_builtin_threads(left_or)
			|| comptime_cond_has_builtin_threads(right_or)
	}
	left_and, right_and, has_and := comptime_cond_split_top_level(c, '&&')
	if has_and {
		return comptime_cond_has_builtin_threads(left_and)
			|| comptime_cond_has_builtin_threads(right_and)
	}
	if c.starts_with('!') {
		return comptime_cond_has_builtin_threads(c[1..])
	}
	return c == 'threads'
}

// comptime_cond_needs_loop_var reports whether a `$if` condition reads any currently active
// `$for` loop variable, meaning it can only be evaluated once the loop is unrolled.
fn (p &Parser) comptime_cond_needs_loop_var(cond string) bool {
	for var_name in p.comptime_for_vars {
		if comptime_cond_references_var(cond, var_name) {
			return true
		}
	}
	return false
}

fn (p &Parser) simplify_deferred_comptime_cond(cond string) string {
	c := comptime_cond_strip_outer_parens(cond.trim_space())
	if !p.comptime_cond_needs_loop_var(c) && !comptime_cond_has_type_test(c)
		&& !comptime_cond_has_type_metadata(c) && !comptime_cond_has_builtin_threads(c) {
		return if eval_comptime_cond(p.prefs, c) { 'true' } else { 'false' }
	}
	left_or, right_or, has_or := comptime_cond_split_top_level(c, '||')
	if has_or {
		left := p.simplify_deferred_comptime_cond(left_or)
		if left == 'true' {
			return 'true'
		}
		right := p.simplify_deferred_comptime_cond(right_or)
		if right == 'true' {
			return 'true'
		}
		if left == 'false' {
			return right
		}
		if right == 'false' {
			return left
		}
		return '(${left}) || (${right})'
	}
	left_and, right_and, has_and := comptime_cond_split_top_level(c, '&&')
	if has_and {
		left := p.simplify_deferred_comptime_cond(left_and)
		if left == 'false' {
			return 'false'
		}
		right := p.simplify_deferred_comptime_cond(right_and)
		if right == 'false' {
			return 'false'
		}
		if left == 'true' {
			return right
		}
		if right == 'true' {
			return left
		}
		return '(${left}) && (${right})'
	}
	if c.starts_with('!') {
		inner := p.simplify_deferred_comptime_cond(c[1..])
		if inner == 'true' {
			return 'false'
		}
		if inner == 'false' {
			return 'true'
		}
		return '!(${inner})'
	}
	return c
}

// comptime_cond_references_var reports whether `cond` reads `var_name` as a whole identifier
// (bare `var_name` or the base of a `var_name.member` selector), so `field` is not matched
// inside `fields_enabled` or `myfield`.
fn comptime_cond_references_var(cond string, var_name string) bool {
	if var_name.len == 0 {
		return false
	}
	mut i := 0
	for i < cond.len {
		c := cond[i]
		if c.is_letter() || c == `_` {
			start := i
			for i < cond.len && (cond[i].is_letter() || cond[i].is_digit() || cond[i] == `_`) {
				i++
			}
			if cond[start..i] == var_name {
				return true
			}
		} else {
			i++
		}
	}
	return false
}

fn (mut p Parser) skip_comptime_else() {
	for {
		if p.tok == .semicolon {
			p.next()
		}
		if p.tok != .dollar || p.peek() != .key_else {
			return
		}
		p.next() // skip $
		p.next() // skip else
		if p.tok == .semicolon {
			p.next()
		}
		if p.tok != .dollar || p.peek() != .key_if {
			p.skip_block()
			return
		}
		p.next() // skip $
		p.next() // skip if
		for p.tok != .lcbr && p.tok != .eof {
			p.next()
		}
		p.skip_block()
	}
}

fn (mut p Parser) parse_comptime_else() flat.NodeId {
	// Skip auto-semicolons before $else
	if p.tok == .semicolon && p.peek() == .dollar {
		p.next()
	}
	if p.tok != .dollar {
		return flat.empty_node
	}
	if p.peek() != .key_else {
		return flat.empty_node
	}
	p.next() // skip $
	p.next() // skip else
	// $else $if — recurse
	if p.tok == .dollar || (p.tok == .semicolon && p.peek() == .dollar) {
		if p.tok == .semicolon {
			p.next()
		}
		return p.parse_comptime_if()
	}
	return p.block_stmt()
}

fn (mut p Parser) parse_top_level_comptime_else() flat.NodeId {
	// Skip auto-semicolons before $else
	if p.tok == .semicolon && p.peek() == .dollar {
		p.next()
	}
	if p.tok != .dollar {
		return flat.empty_node
	}
	if p.peek() != .key_else {
		return flat.empty_node
	}
	p.next() // skip $
	p.next() // skip else
	// $else $if - recurse
	if p.tok == .dollar || (p.tok == .semicolon && p.peek() == .dollar) {
		if p.tok == .semicolon {
			p.next()
		}
		return p.parse_top_level_comptime_if()
	}
	return p.top_level_block_stmt()
}

fn eval_comptime_cond(prefs &pref.Preferences, cond string) bool {
	c := comptime_cond_strip_outer_parens(cond.trim_space())
	left_or, right_or, has_or := comptime_cond_split_top_level(c, '||')
	if has_or {
		return eval_comptime_cond(prefs, left_or) || eval_comptime_cond(prefs, right_or)
	}
	left_and, right_and, has_and := comptime_cond_split_top_level(c, '&&')
	if has_and {
		return eval_comptime_cond(prefs, left_and) && eval_comptime_cond(prefs, right_and)
	}
	left_ne, right_ne, has_ne := comptime_cond_split_top_level(c, '!=')
	if has_ne {
		return comptime_cond_value(left_ne) != comptime_cond_value(right_ne)
	}
	left_eq, right_eq, has_eq := comptime_cond_split_top_level(c, '==')
	if has_eq {
		return comptime_cond_value(left_eq) == comptime_cond_value(right_eq)
	}
	if c.starts_with('!') {
		return !eval_comptime_cond(prefs, c[1..])
	}
	if c == 'true' {
		return true
	}
	if c == 'false' {
		return false
	}
	if c.starts_with('pkgconfig') {
		return eval_pkgconfig_cond(c)
	}
	if value := eval_comptime_define_cond(prefs, c) {
		return value
	}
	if c.ends_with('?') {
		flag := c[..c.len - 1].trim_space()
		return pref.comptime_optional_flag_value(prefs, flag)
	}
	return pref.comptime_flag_value(prefs, c.trim_space())
}

fn (p &Parser) resolve_comptime_const_values(cond string) string {
	return p.resolve_comptime_cached_values(cond, true)
}

fn (p &Parser) resolve_comptime_cached_values(cond string, preserve_flags bool) string {
	mut out := strings.new_builder(cond.len)
	mut i := 0
	mut quote := u8(0)
	for i < cond.len {
		c := cond[i]
		if quote != 0 {
			out.write_u8(c)
			if c == `\\` && i + 1 < cond.len {
				i++
				out.write_u8(cond[i])
			} else if c == quote {
				quote = 0
			}
			i++
			continue
		}
		if c == `'` || c == `"` {
			quote = c
			out.write_u8(c)
			i++
			continue
		}
		if c.is_letter() || c == `_` {
			start := i
			for i < cond.len && (cond[i].is_letter() || cond[i].is_digit() || cond[i] == `_`) {
				i++
			}
			name := cond[start..i]
			mut prev := start
			for prev > 0 && cond[prev - 1].is_space() {
				prev--
			}
			is_protected_name := prev > 0 && (cond[prev - 1] == `.` || cond[prev - 1] == `$`)
			is_comptime_flag := preserve_flags && p.comptime_cond_name_is_flag(cond, name, i)
			if !is_protected_name && !is_comptime_flag && name !in p.comptime_for_vars {
				if value := p.comptime_value(name) {
					out.write_string(value)
					continue
				}
			}
			out.write_string(name)
			continue
		}
		out.write_u8(c)
		i++
	}
	return out.str()
}

fn (p &Parser) comptime_cond_name_is_flag(cond string, name string, end int) bool {
	mut next := end
	for next < cond.len && cond[next].is_space() {
		next++
	}
	if next < cond.len && cond[next] == `?` {
		return true
	}
	match name {
		'macos', 'darwin', 'mac', 'linux', 'windows', 'freebsd', 'openbsd', 'netbsd', 'dragonfly',
		'android', 'posix', 'unix', 'bsd', 'x64', 'x32', 'amd64', 'arm64', 'aarch64',
		'little_endian', 'big_endian', 'debug', 'test', 'native',
		'builtin_write_buf_to_fd_should_use_c_write', 'tinyc', 'no_backtrace', 'gcboehm',
		'gcboehm_opt', 'prealloc', 'autofree', 'no_bounds_checking', 'freestanding', 'nofloat',
		'threads' {
			return true
		}
		else {}
	}

	for define in p.prefs.user_defines {
		if define == name || define.starts_with('${name}=') {
			return true
		}
	}
	return false
}

fn (p &Parser) comptime_value(name string) ?string {
	if value := p.comptime_local_values[name] {
		return value
	}
	if value := p.comptime_const_values[comptime_const_value_key(p.cur_module, name)] {
		return value
	}
	if p.cur_module != 'builtin' {
		if value := p.comptime_const_values[comptime_const_value_key('builtin', name)] {
			return value
		}
	}
	return none
}

fn comptime_const_value_key(module_name string, name string) string {
	return '${module_name}\n${name}'
}

fn (p &Parser) comptime_node_value(id flat.NodeId) ?string {
	if int(id) < 0 || int(id) >= p.a.nodes.len {
		return none
	}
	node := p.a.nodes[int(id)]
	return match node.kind {
		.int_literal, .float_literal, .bool_literal {
			node.value
		}
		.char_literal {
			'`${node.value}`'
		}
		.string_literal {
			comptime_cond_quoted_string(node.value)
		}
		.ident {
			p.comptime_value(node.value)
		}
		.paren {
			if node.children_count == 1 {
				p.comptime_node_value(p.a.children[int(node.children_start)])
			} else {
				none
			}
		}
		else {
			none
		}
	}
}

fn (mut p Parser) begin_comptime_value_scope() {
	p.comptime_value_scopes << p.comptime_value_undos.len
}

fn (mut p Parser) end_comptime_value_scope() {
	if p.comptime_value_scopes.len == 0 {
		return
	}
	start := p.comptime_value_scopes.pop()
	for i := p.comptime_value_undos.len - 1; i >= start; i-- {
		undo := p.comptime_value_undos[i]
		if undo.had_value {
			p.comptime_local_values[undo.name] = undo.old_value
		} else {
			p.comptime_local_values.delete(undo.name)
		}
	}
	p.comptime_value_undos.trim(start)
}

fn (mut p Parser) set_comptime_local_value(name string, value string) {
	if name.len == 0 || p.comptime_value_scopes.len == 0 {
		return
	}
	had_value := name in p.comptime_local_values
	old_value := p.comptime_local_values[name]
	p.comptime_value_undos << ComptimeValueUndo{
		name:      name
		old_value: old_value
		had_value: had_value
	}
	p.comptime_local_values[name] = value
}

fn (mut p Parser) forget_comptime_local_value(name string) {
	if name.len == 0 || p.comptime_value_scopes.len == 0 {
		return
	}
	had_value := name in p.comptime_local_values
	old_value := p.comptime_local_values[name]
	if !had_value {
		return
	}
	p.comptime_value_undos << ComptimeValueUndo{
		name:      name
		old_value: old_value
		had_value: true
	}
	p.comptime_local_values.delete(name)
}

fn (mut p Parser) remember_comptime_decl_value(lhs flat.NodeId, rhs flat.NodeId) {
	if int(lhs) < 0 || int(lhs) >= p.a.nodes.len {
		return
	}
	lhs_node := p.a.nodes[int(lhs)]
	if lhs_node.kind != .ident {
		return
	}
	if lhs_node.is_mut {
		p.forget_comptime_local_value(lhs_node.value)
		return
	}
	if value := p.comptime_node_value(rhs) {
		p.set_comptime_local_value(lhs_node.value, value)
	} else {
		p.forget_comptime_local_value(lhs_node.value)
	}
}

fn (mut p Parser) forget_comptime_lhs_value(lhs flat.NodeId) {
	if int(lhs) < 0 || int(lhs) >= p.a.nodes.len {
		return
	}
	lhs_node := p.a.nodes[int(lhs)]
	if lhs_node.kind == .ident {
		p.forget_comptime_local_value(lhs_node.value)
	}
}

fn comptime_cond_value(value string) string {
	clean := value.trim_space()
	if clean.len >= 2 && ((clean[0] == `'` && clean[clean.len - 1] == `'`)
		|| (clean[0] == `"` && clean[clean.len - 1] == `"`)) {
		return unescape_string(clean[1..clean.len - 1])
	}
	if clean.len >= 2 && clean[0] == `\`` && clean[clean.len - 1] == `\`` {
		return unescape_string(clean[1..clean.len - 1])
	}
	return clean
}

// eval_comptime_define_cond evaluates the boolean form of `$d('name', default)`. A builtin
// comptime flag (notably `test`) is intentionally not a `-d` define, so an absent user define
// still uses the supplied default.
fn eval_comptime_define_cond(prefs &pref.Preferences, cond string) ?bool {
	clean := cond.replace(' ', '')
	if !clean.starts_with('$d(') || !clean.ends_with(')') {
		return none
	}
	inner := clean[3..clean.len - 1]
	comma := inner.index_u8(`,`)
	if comma <= 0 || comma + 1 >= inner.len {
		return none
	}
	name := inner[..comma].trim('\'"')
	for define in prefs.user_defines {
		if define == name || define.starts_with('${name}=') {
			if define.contains('=') {
				value := define.all_after_first('=').to_lower()
				return value !in ['', '0', 'false']
			}
			return true
		}
	}
	default_value := inner[comma + 1..].to_lower()
	if default_value == 'true' {
		return true
	}
	if default_value == 'false' {
		return false
	}
	return none
}

fn comptime_cond_strip_outer_parens(cond string) string {
	mut c := cond
	for c.len >= 2 && c[0] == `(` && c[c.len - 1] == `)` {
		mut depth := 0
		mut quote := u8(0)
		mut escaped := false
		mut wraps := true
		for i in 0 .. c.len {
			ch := c[i]
			if quote != 0 {
				if escaped {
					escaped = false
					continue
				}
				if ch == `\\` {
					escaped = true
					continue
				}
				if ch == quote {
					quote = 0
				}
				continue
			}
			if ch == `'` || ch == `"` {
				quote = ch
				continue
			}
			if ch == `(` {
				depth++
			} else if ch == `)` {
				depth--
				if depth == 0 && i != c.len - 1 {
					wraps = false
					break
				}
			}
		}
		if !wraps {
			break
		}
		c = c[1..c.len - 1].trim_space()
	}
	return c
}

fn comptime_cond_split_top_level(cond string, op string) (string, string, bool) {
	mut depth := 0
	mut quote := u8(0)
	mut i := 0
	for i < cond.len {
		ch := cond[i]
		if quote != 0 {
			if ch == `\\` && i + 1 < cond.len {
				i += 2
				continue
			}
			if ch == quote {
				quote = 0
			}
			i++
			continue
		}
		if ch == `'` || ch == `"` {
			quote = ch
			i++
			continue
		}
		if ch == `(` {
			depth++
		} else if ch == `)` && depth > 0 {
			depth--
		}
		if depth == 0 && i + op.len <= cond.len && cond[i..i + op.len] == op {
			return cond[..i], cond[i + op.len..], true
		}
		i++
	}
	return '', '', false
}

fn eval_pkgconfig_cond(cond string) bool {
	name := pkgconfig_name_from_cond(cond)
	if !is_safe_pkgconfig_name(name) {
		return false
	}
	return os.system('pkg-config --exists ${name}') == 0
}

fn is_safe_pkgconfig_name(name string) bool {
	if name.len == 0 || !pkgconfig_name_char_is_alnum(name[0]) {
		return false
	}
	for ch in name {
		if pkgconfig_name_char_is_alnum(ch) || ch == `_` || ch == `-` || ch == `.` || ch == `+` {
			continue
		}
		return false
	}
	return true
}

fn pkgconfig_name_char_is_alnum(ch u8) bool {
	return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || (ch >= `0` && ch <= `9`)
}

fn pkgconfig_name_from_cond(cond string) string {
	quote1 := cond.index_u8(`'`)
	if quote1 >= 0 {
		rest := cond[quote1 + 1..]
		end := rest.index_u8(`'`)
		if end >= 0 {
			return rest[..end]
		}
	}
	quote2 := cond.index_u8(`"`)
	if quote2 >= 0 {
		rest := cond[quote2 + 1..]
		end := rest.index_u8(`"`)
		if end >= 0 {
			return rest[..end]
		}
	}
	open := cond.index_u8(`(`)
	close := cond.last_index_u8(`)`)
	if open < 0 || close < 0 {
		return ''
	}
	if close <= open {
		return ''
	}
	return cond[open + 1..close].trim_space().trim('\'"')
}

fn (mut p Parser) skip_block() {
	if p.tok != .lcbr {
		return
	}
	mut depth := 1
	p.next()
	for depth > 0 && p.tok != .eof {
		if p.tok == .lcbr {
			depth++
		} else if p.tok == .rcbr {
			depth--
		}
		p.next()
	}
}

fn (mut p Parser) skip_brackets() {
	if p.tok != .lsbr {
		return
	}
	mut depth := 1
	p.next()
	for depth > 0 && p.tok != .eof {
		if p.tok == .lsbr {
			depth++
		} else if p.tok == .rsbr {
			depth--
		}
		p.next()
	}
}

fn (mut p Parser) parse_generic_param_names() []string {
	mut names := []string{}
	if p.tok != .lsbr {
		return names
	}
	mut depth := 1
	mut expect_name := true
	p.next()
	for depth > 0 && p.tok != .eof {
		if p.tok == .lsbr {
			depth++
			expect_name = false
		} else if p.tok == .rsbr {
			depth--
			if depth == 0 {
				p.next()
				break
			}
		} else if depth == 1 {
			if p.tok == .comma {
				expect_name = true
			} else if expect_name && p.tok == .name {
				names << p.lit
				expect_name = false
			} else {
				expect_name = false
			}
		}
		p.next()
	}
	return names
}

fn (mut p Parser) skip_parens() {
	if p.tok != .lpar {
		return
	}
	mut depth := 1
	p.next()
	for depth > 0 && p.tok != .eof {
		if p.tok == .lpar {
			depth++
		} else if p.tok == .rpar {
			depth--
		}
		p.next()
	}
}

fn (mut p Parser) parse_comptime_expr() flat.NodeId {
	if p.peek() == .key_for {
		return p.parse_comptime_if()
	}
	p.next() // skip $
	if p.tok == .key_if || (p.tok == .name && p.lit == 'if') {
		return p.parse_comptime_if_expr_after_if()
	}
	if p.tok == .name && p.lit == 'd' {
		p.next()
		if p.tok != .lpar {
			return flat.empty_node
		}
		p.next()
		// First argument is the compile-time define name. v3 currently only
		// uses the default expression, so consume the name expression.
		if p.tok != .comma && p.tok != .rpar && p.tok != .eof {
			p.expr(.lowest)
		}
		if p.tok == .comma {
			p.next()
			default_expr := p.expr(.lowest)
			for p.tok != .rpar && p.tok != .eof {
				p.next()
			}
			p.check(.rpar)
			return default_expr
		}
		p.check(.rpar)
		return flat.empty_node
	}
	if p.tok == .name && p.lit == 'res' {
		p.next()
		if p.tok == .lpar {
			p.skip_parens()
		}
		return p.a.add_val_id(3, 'false')
	}
	if p.tok == .name && p.lit == 'env' {
		// $env('NAME') evaluates an environment variable at compile time and
		// becomes a plain string literal (empty when the variable is unset).
		p.next() // skip 'env'
		mut env_val := ''
		if p.tok == .lpar {
			p.next() // skip (
			if p.tok == .string {
				env_val = os.getenv(strip_quotes(p.lit))
				p.next()
			}
			for p.tok != .rpar && p.tok != .eof {
				p.next()
			}
			p.check(.rpar)
		}
		return p.a.add_val_id(5, env_val)
	}
	if p.tok == .name && p.lit == 'embed_file' {
		return p.parse_embed_file_expr()
	}
	if p.tok == .name && p.lit in ['zero', 'new'] {
		name := p.lit
		p.next()
		if p.tok != .lpar {
			return flat.empty_node
		}
		p.next()
		type_expr := p.expr(.lowest)
		p.check(.rpar)
		return p.a.add_node(flat.Node{
			kind:           .string_literal
			value:          '__v3_comptime_${name}'
			typ:            'string'
			children_start: p.add_child(type_expr)
			children_count: 1
		})
	}
	for p.tok != .semicolon && p.tok != .eof {
		p.next()
	}
	// Unknown comptime expression: return an empty string literal rather than
	// `empty_node`, so consumers (e.g. const initializers) never store an
	// invalid (-1) child node.
	return p.a.add_val_id(5, '')
}

fn (mut p Parser) parse_embed_file_expr() flat.NodeId {
	p.next() // skip embed_file
	if p.tok != .lpar {
		return flat.empty_node
	}
	p.next()
	mut rel_path := ''
	if p.tok == .string {
		rel_path = strip_quotes(p.lit)
		p.next()
	} else if p.tok == .name && p.lit == '@FILE' {
		rel_path = if os.is_abs_path(p.cur_file) { p.cur_file } else { os.real_path(p.cur_file) }
		p.next()
	} else {
		// V3 does not evaluate arbitrary comptime expressions yet. Keep parsing
		// valid and let the runtime helper fail clearly if the path is unknown.
		p.expr(.lowest)
	}
	for p.tok != .rpar && p.tok != .eof {
		p.next()
	}
	p.check(.rpar)
	apath := p.embed_file_abs_path(rel_path)
	len := if apath.len > 0 && os.is_file(apath) { int(os.file_size(apath)) } else { 0 }
	mut field_ids := [
		p.embed_file_field('path', p.a.add_val_id(5, rel_path)),
		p.embed_file_field('apath', p.a.add_val_id(5, apath)),
		p.embed_file_field('len', p.a.add_val_id(1, len.str())),
	]
	if uncompressed := p.embed_file_uncompressed_data(apath) {
		field_ids << p.embed_file_field('uncompressed', uncompressed)
	}
	return p.a.add_node(flat.Node{
		kind:           .struct_init
		value:          'embed_file.EmbedFileData'
		typ:            'embed_file.EmbedFileData'
		children_start: p.add_children(field_ids)
		children_count: flat.child_count(field_ids.len)
	})
}

fn (mut p Parser) embed_file_uncompressed_data(apath string) ?flat.NodeId {
	if !p.prefs.is_prod || apath.len == 0 || !os.is_file(apath) {
		return none
	}
	bytes := os.read_bytes(apath) or { return none }
	data := p.a.add_val_id(5, bytes.bytestr().clone())
	return p.a.add_node(flat.Node{
		kind:           .cast_expr
		value:          '&u8'
		typ:            '&u8'
		children_start: p.add_child(data)
		children_count: 1
	})
}

fn (mut p Parser) embed_file_field(name string, val flat.NodeId) flat.NodeId {
	return p.a.add_node(flat.Node{
		kind:           .field_init
		value:          name
		children_start: p.add_child(val)
		children_count: 1
	})
}

fn (p &Parser) embed_file_abs_path(path string) string {
	if path.len == 0 {
		return ''
	}
	mut full_path := path
	if full_path.starts_with('@VEXEROOT/') {
		full_path = os.join_path_single(p.prefs.vroot, full_path['@VEXEROOT/'.len..])
	} else if full_path == '@VEXEROOT' {
		full_path = p.prefs.vroot
	} else if full_path.starts_with('@VMODROOT/') {
		full_path = os.join_path_single(vmod_root_for_file(p.cur_file),
			full_path['@VMODROOT/'.len..])
	} else if full_path == '@VMODROOT' {
		full_path = vmod_root_for_file(p.cur_file)
	} else if !os.is_abs_path(full_path) {
		full_path = os.join_path_single(os.dir(p.cur_file), full_path)
	}
	if os.exists(full_path) {
		return os.real_path(full_path)
	}
	return full_path
}

fn (mut p Parser) parse_comptime_if_expr() flat.NodeId {
	p.next() // skip $
	if p.tok != .key_if && !(p.tok == .name && p.lit == 'if') {
		return flat.empty_node
	}
	return p.parse_comptime_if_expr_after_if()
}

fn (mut p Parser) parse_comptime_if_expr_after_if() flat.NodeId {
	p.next() // skip if
	mut cond :=
		p.resolve_comptime_const_values(p.resolve_comptime_at_values(p.parse_comptime_cond()))
	// Whether `threads` is enabled depends on spawn expressions in the completed AST,
	// so expression branches must be retained for the checker/transformer to select.
	if comptime_cond_has_type_test(cond) || comptime_cond_has_type_metadata(cond)
		|| comptime_cond_has_builtin_threads(cond) {
		cond = p.simplify_deferred_comptime_cond(cond)
		if comptime_cond_has_type_test(cond) || comptime_cond_has_type_metadata(cond)
			|| comptime_cond_has_builtin_threads(cond) {
			then_expr := p.parse_comptime_expr_block()
			else_expr := p.parse_comptime_else_expr()
			return p.comptime_if_node(cond, then_expr, else_expr)
		}
	}
	taken := eval_comptime_cond(p.prefs, cond)
	if taken {
		result := p.parse_comptime_expr_block()
		p.skip_comptime_else()
		return result
	}
	p.skip_block()
	return p.parse_comptime_else_expr()
}

fn (mut p Parser) parse_comptime_else_expr() flat.NodeId {
	if p.tok == .semicolon && p.peek() == .dollar {
		p.next()
	}
	if p.tok != .dollar || p.peek() != .key_else {
		return flat.empty_node
	}
	p.next() // skip $
	p.next() // skip else
	if p.tok == .semicolon && p.peek() == .dollar {
		p.next()
	}
	if p.tok == .dollar {
		return p.parse_comptime_if_expr()
	}
	return p.parse_comptime_expr_block()
}

fn (mut p Parser) parse_comptime_expr_block() flat.NodeId {
	if p.tok != .lcbr {
		return flat.empty_node
	}
	ids := p.parse_block_body()
	if ids.len == 0 {
		return flat.empty_node
	}
	if ids.len > 1 {
		start := p.add_children(ids)
		return p.a.add_node(flat.Node{
			kind:           .block
			children_start: start
			children_count: flat.child_count(ids.len)
		})
	}
	last_id := ids.last()
	last := p.a.nodes[int(last_id)]
	if last.kind == .expr_stmt && last.children_count == 1 {
		return p.a.children[int(last.children_start)]
	}
	return last_id
}

// ==================== statements ====================

fn (mut p Parser) stmt() flat.NodeId {
	match p.tok {
		.key_return {
			return p.return_stmt()
		}
		.key_if {
			return p.if_stmt()
		}
		.key_for {
			return p.for_stmt()
		}
		.key_fn {
			return p.fn_decl()
		}
		.key_struct, .key_union {
			return p.struct_decl()
		}
		.key_match {
			return p.match_stmt()
		}
		.key_break {
			p.next()
			mut label := ''
			if p.tok == .name {
				label = p.lit
				p.next()
			}
			if p.tok == .semicolon {
				p.next()
			}
			return p.a.add_val(.break_stmt, label)
		}
		.key_continue {
			p.next()
			mut label := ''
			if p.tok == .name {
				label = p.lit
				p.next()
			}
			if p.tok == .semicolon {
				p.next()
			}
			return p.a.add_val(.continue_stmt, label)
		}
		.key_mut {
			p.next()
			if p.tok == .key_static {
				return p.static_decl_stmt()
			}
			stmt_id := p.assign_or_expr_stmt()
			p.mark_node_mut(stmt_id)
			return stmt_id
		}
		.key_shared {
			p.next()
			stmt_id := p.assign_or_expr_stmt()
			p.mark_node_shared(stmt_id)
			return stmt_id
		}
		.key_static {
			return p.static_decl_stmt()
		}
		.key_pub {
			p.next()
			if p.tok == .key_fn {
				return p.fn_decl()
			}
			if p.tok == .key_struct || p.tok == .key_union {
				return p.struct_decl()
			}
			return p.assign_or_expr_stmt()
		}
		.attribute {
			p.skip_attrs()
			return p.stmt()
		}
		.key_unsafe {
			p.next()
			return p.block_stmt()
		}
		.key_defer {
			return p.defer_stmt()
		}
		.key_assert {
			return p.assert_stmt()
		}
		.key_goto {
			return p.goto_stmt()
		}
		.key_go, .key_spawn {
			p.next()
			inner := p.expr(.lowest)
			if p.tok == .semicolon {
				p.next()
			}
			spawn_start := p.add_child(inner)
			spawn_expr := p.a.add_node(flat.Node{
				kind:           .spawn_expr
				children_start: spawn_start
				children_count: 1
			})
			sstart := p.add_child(spawn_expr)
			return p.a.add_node(flat.Node{
				kind:           .expr_stmt
				children_start: sstart
				children_count: 1
			})
		}
		.key_asm {
			return p.asm_stmt()
		}
		.dollar {
			pk := p.peek()
			if pk == .key_if || pk == .key_for || pk == .key_match
				|| (pk == .name && p.peek_lit in ['if', 'for', 'match', 'compile_error']) {
				return p.parse_comptime_if()
			}
			return p.assign_or_expr_stmt()
		}
		.hash {
			return p.directive()
		}
		.lcbr {
			return p.block_stmt()
		}
		.semicolon {
			p.next()
			return flat.empty_node
		}
		.name {
			// label: name followed by ':'
			if p.peek() == .colon {
				label_name := p.lit
				p.next() // skip name
				p.next() // skip :
				if p.tok == .semicolon {
					p.next()
				}
				return p.a.add_val(.label_stmt, label_name)
			}
			return p.assign_or_expr_stmt()
		}
		else {
			return p.assign_or_expr_stmt()
		}
	}
}

fn (mut p Parser) mark_node_mut(id flat.NodeId) {
	p.a.set_node_is_mut(id, true)
	if int(id) < 0 || int(id) >= p.a.nodes.len {
		return
	}
	node := p.a.nodes[int(id)]
	if node.kind == .decl_assign {
		p.forget_comptime_decl_lhs_values(node)
	}
}

fn (mut p Parser) forget_comptime_decl_lhs_values(node flat.Node) {
	lhs_count_value := node.value.int()
	lhs_count := if lhs_count_value > 0 { lhs_count_value } else { 1 }
	rhs_count := int(node.children_count) - lhs_count
	mut child_offset := 0
	for i in 0 .. lhs_count {
		if child_offset >= int(node.children_count) {
			break
		}
		p.forget_comptime_lhs_value(p.a.children[int(node.children_start) + child_offset])
		child_offset++
		if i < rhs_count {
			child_offset++
		}
	}
}

fn (mut p Parser) mark_node_shared(id flat.NodeId) {
	if int(id) < 0 || int(id) >= p.a.nodes.len {
		return
	}
	node := p.a.nodes[int(id)]
	if node.kind != .decl_assign {
		return
	}
	p.forget_comptime_decl_lhs_values(node)
	unsafe {
		mut node_ptr := &p.a.nodes[int(id)]
		node_ptr.value = if node_ptr.value.len == 0 { 'shared' } else { 'shared:${node_ptr.value}' }
	}
}

fn (mut p Parser) static_decl_stmt() flat.NodeId {
	p.next() // skip `static`
	mut is_mut := false
	if p.tok == .key_mut {
		is_mut = true
		p.next()
	}
	lhs := p.expr(.lowest)
	if p.tok == .decl_assign {
		p.next()
		rhs := p.expr(.lowest)
		if p.tok == .semicolon {
			p.next()
		}
		istart := p.add_children2(lhs, rhs)
		return p.a.add_node(flat.Node{
			kind:           .decl_assign
			op:             .assign
			value:          'static'
			is_mut:         is_mut
			children_start: istart
			children_count: 2
		})
	}
	if p.tok == .semicolon {
		p.next()
	}
	estart := p.add_child(lhs)
	return p.a.add_node(flat.Node{
		kind:           .expr_stmt
		children_start: estart
		children_count: 1
	})
}

fn (mut p Parser) return_stmt() flat.NodeId {
	return_pos := p.tok_pos
	p.next() // skip 'return'
	mut ids := []flat.NodeId{}
	// vfmt wraps long return expressions after the keyword. The following token
	// remains part of the return only when it is indented beyond `return` itself;
	// an ordinary next statement stays at the same block indentation. Only a
	// newline inserted by the scanner can continue the expression; `return;`
	// always ends the statement.
	if p.tok == .semicolon && p.tok_pos >= 0 && p.tok_pos < p.s.src.len
		&& p.s.src[p.tok_pos] == `\n` {
		_ = p.peek()
		if p.peek_tok !in [.eof, .rcbr]
			&& p.column_for_pos(p.peek_pos) > p.column_for_pos(return_pos) {
			p.next()
		}
	}
	if p.tok != .semicolon && p.tok != .rcbr && p.tok != .eof {
		ids << p.expr(.lowest)
		for p.tok == .comma {
			p.next()
			ids << p.expr(.lowest)
		}
	}
	if p.tok == .semicolon {
		p.next()
	}
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .return_stmt
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) if_stmt() flat.NodeId {
	p.next() // skip 'if'
	cond := p.expr(.lowest)

	// if-guard: if a, b := expr { ... } or if val := expr { ... }
	mut guard_cond := cond
	if p.tok == .comma || p.tok == .decl_assign {
		// Simple if-guard; treat as regular condition for flat AST
		if p.tok == .decl_assign {
			p.next()
			rhs := p.expr(.lowest)
			istart := p.add_children2(guard_cond, rhs)
			guard_cond = p.a.add_node(flat.Node{
				kind:           .decl_assign
				op:             .assign
				children_start: istart
				children_count: 2
			})
		} else {
			// comma case: if a, b := expr
			mut lhs_ids := []flat.NodeId{}
			lhs_ids << guard_cond
			for p.tok == .comma {
				p.next()
				lhs_ids << p.expr(.lowest)
			}
			if p.tok == .decl_assign {
				p.next()
				rhs := p.expr(.lowest)
				mut all_ids := []flat.NodeId{cap: lhs_ids.len + 1}
				all_ids << lhs_ids[0]
				all_ids << rhs
				for i in 1 .. lhs_ids.len {
					all_ids << lhs_ids[i]
				}
				istart := p.add_children(all_ids)
				guard_cond = p.a.add_node(flat.Node{
					kind:           .decl_assign
					op:             .assign
					value:          '${lhs_ids.len}'
					children_start: istart
					children_count: flat.child_count(all_ids.len)
				})
			}
		}
	}

	// skip auto-semicolon before {
	if p.tok == .semicolon && p.peek() == .lcbr {
		p.next()
	}
	body := p.block_stmt()
	mut ids := []flat.NodeId{}
	ids << guard_cond
	ids << body

	// skip auto-semicolon between } and else
	if p.tok == .semicolon && p.peek() == .key_else {
		p.next()
	}
	if p.tok == .key_else {
		p.next()
		if p.tok == .key_if {
			ids << p.if_stmt()
		} else {
			ids << p.block_stmt()
		}
	}
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) for_stmt() flat.NodeId {
	p.next() // skip 'for'
	if p.tok == .lcbr {
		// infinite loop: for { ... }
		body_ids := p.parse_block_body()
		empty1 := p.a.add(flat.NodeKind.empty)
		empty2 := p.a.add(flat.NodeKind.empty)
		empty3 := p.a.add(flat.NodeKind.empty)
		mut ids := []flat.NodeId{}
		ids << empty1
		ids << empty2
		ids << empty3
		for id in body_ids {
			ids << id
		}
		start := p.add_children(ids)
		return p.a.add_node(flat.Node{
			kind:           .for_stmt
			children_start: start
			children_count: flat.child_count(ids.len)
		})
	}

	// Check for for-in: `for x in ...` or `for mut x in ...`.
	// A comma after the first name is ambiguous with C-style multi-init
	// (`for h, t := ...`), so handle it after parsing the first expression.
	if p.tok == .name && p.peek() == .key_in {
		first_expr := p.expr(.bit_or)
		return p.for_in(first_expr, false)
	}
	if p.tok == .key_mut {
		p.next()
		mut first_expr := flat.empty_node
		if p.tok == .name {
			ident := p.a.add_val(.ident, p.expect_name())
			if p.tok == .key_in || p.tok == .comma {
				first_expr = ident
			} else {
				first_expr = p.expr_with_lhs(ident, .lowest)
			}
		} else {
			first_expr = p.expr(.lowest)
		}
		if p.tok == .key_in {
			return p.for_in(first_expr, true)
		}
		if p.tok == .comma {
			return p.for_comma_header(first_expr, true)
		}
		if p.tok == .lcbr {
			body_ids := p.parse_block_body()
			init_empty := p.a.add(flat.NodeKind.empty)
			post_empty := p.a.add(flat.NodeKind.empty)
			mut ids := []flat.NodeId{}
			ids << init_empty
			ids << first_expr
			ids << post_empty
			for id in body_ids {
				ids << id
			}
			start := p.add_children(ids)
			return p.a.add_node(flat.Node{
				kind:           .for_stmt
				children_start: start
				children_count: flat.child_count(ids.len)
			})
		}
	}

	if p.tok == .semicolon {
		p.next()
		cond := if p.tok == .semicolon {
			p.a.add(flat.NodeKind.empty)
		} else {
			p.expr(.lowest)
		}
		if p.tok == .semicolon {
			p.next()
		}
		post := if p.tok != .lcbr && p.tok != .eof {
			p.assign_or_expr_inline()
		} else {
			p.a.add(flat.NodeKind.empty)
		}
		body_ids := p.parse_block_body()
		mut ids := []flat.NodeId{}
		ids << p.a.add(flat.NodeKind.empty)
		ids << cond
		ids << post
		for id in body_ids {
			ids << id
		}
		start := p.add_children(ids)
		return p.a.add_node(flat.Node{
			kind:           .for_stmt
			children_start: start
			children_count: flat.child_count(ids.len)
		})
	}

	first_expr := p.expr(.lowest)

	// for-in: `for x in expr`
	if p.tok == .key_in {
		return p.for_in(first_expr, false)
	}
	if p.tok == .comma {
		return p.for_comma_header(first_expr, false)
	}

	// C-style: `for i := 0; ...`
	if p.tok == .decl_assign || token_is_assignment(p.tok) {
		return p.for_c_style(first_expr)
	}

	if p.tok == .semicolon {
		// could be C-style with expression init, or just a separator
		// peek to check
		pk := p.peek()
		if pk == .semicolon || pk == .lcbr {
			// condition-only: `for cond { }`
			// fall through
		} else if pk != .rcbr && pk != .eof {
			// might be C-style: `for expr; cond; post`
			p.next() // skip ;
			cond := p.expr(.lowest)
			if p.tok == .semicolon {
				p.next()
			}
			post := if p.tok != .lcbr && p.tok != .eof {
				p.assign_or_expr_inline()
			} else {
				p.a.add(flat.NodeKind.empty)
			}
			body_ids := p.parse_block_body()
			// first_expr becomes init as expr_stmt
			init_start := p.add_child(first_expr)
			init_id := p.a.add_node(flat.Node{
				kind:           .expr_stmt
				children_start: init_start
				children_count: 1
			})
			mut ids := []flat.NodeId{}
			ids << init_id
			ids << cond
			ids << post
			for id in body_ids {
				ids << id
			}
			start := p.add_children(ids)
			return p.a.add_node(flat.Node{
				kind:           .for_stmt
				children_start: start
				children_count: flat.child_count(ids.len)
			})
		}
	}

	// condition-only: `for cond { ... }`
	if p.tok == .lcbr {
		body_ids := p.parse_block_body()
		init_empty := p.a.add(flat.NodeKind.empty)
		post_empty := p.a.add(flat.NodeKind.empty)
		mut ids := []flat.NodeId{}
		ids << init_empty
		ids << first_expr
		ids << post_empty
		for id in body_ids {
			ids << id
		}
		start := p.add_children(ids)
		return p.a.add_node(flat.Node{
			kind:           .for_stmt
			children_start: start
			children_count: flat.child_count(ids.len)
		})
	}

	return flat.empty_node
}

fn (mut p Parser) for_comma_header(first_expr flat.NodeId, first_is_mut bool) flat.NodeId {
	mut lhs_ids := []flat.NodeId{}
	lhs_ids << first_expr
	mut val_id := flat.empty_node
	mut value_is_mut := first_is_mut
	for p.tok == .comma {
		p.next()
		if p.tok == .key_mut {
			p.next()
			value_is_mut = true
		}
		next_lhs := p.expr(.bit_or)
		lhs_ids << next_lhs
		if lhs_ids.len == 2 {
			val_id = next_lhs
		}
	}
	if p.tok == .key_in {
		if lhs_ids.len != 2 {
			return flat.empty_node
		}
		if !p.for_in_var_is_ident(first_expr) || !p.for_in_var_is_ident(val_id) {
			for_id := p.for_in_parts(first_expr, val_id, value_is_mut)
			return p.invalid_for_in_header(for_id)
		}
		return p.for_in_parts(first_expr, val_id, value_is_mut)
	}
	if p.tok == .decl_assign || token_is_assignment(p.tok) {
		return p.for_c_style_multi(lhs_ids)
	}
	return flat.empty_node
}

fn (mut p Parser) for_in_var_is_ident(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	return p.a.nodes[int(id)].kind == .ident
}

fn (mut p Parser) invalid_for_in_header(for_id flat.NodeId) flat.NodeId {
	lhs := p.a.add(flat.NodeKind.empty)
	rhs := p.a.add(flat.NodeKind.empty)
	marker_start := p.add_children2(lhs, rhs)
	marker_id := p.a.add_node(flat.Node{
		kind:           .assign
		value:          'for init assignment mismatch: invalid for-in header: expected identifiers before `in`'
		op:             .assign
		children_start: marker_start
		children_count: 2
	})
	block_start := p.add_children2(marker_id, for_id)
	return p.a.add_node(flat.Node{
		kind:           .block
		children_start: block_start
		children_count: 2
	})
}

fn (mut p Parser) for_c_style_multi(lhs_ids []flat.NodeId) flat.NodeId {
	is_decl := p.tok == .decl_assign
	op_id := int(p.tok)
	p.next()
	mut rhs_ids := []flat.NodeId{}
	rhs_ids << p.expr(.lowest)
	for p.tok == .comma {
		p.next()
		rhs_ids << p.expr(.lowest)
	}
	mut all_ids := []flat.NodeId{cap: lhs_ids.len * 2}
	for i in 0 .. lhs_ids.len {
		all_ids << lhs_ids[i]
		if i < rhs_ids.len {
			all_ids << rhs_ids[i]
		}
	}
	arity_msg := if lhs_ids.len == rhs_ids.len {
		''
	} else {
		'for init assignment mismatch: ${lhs_ids.len} variables but ${rhs_ids.len} values'
	}
	init_start := p.add_children(all_ids)
	init_id := p.a.add_node(flat.Node{
		kind:           if is_decl { flat.NodeKind.decl_assign } else { flat.NodeKind.assign }
		value:          arity_msg
		op:             if is_decl { .assign } else { token_id_to_op(op_id) }
		children_start: init_start
		children_count: flat.child_count(all_ids.len)
	})
	if p.tok == .semicolon {
		p.next()
	}
	cond := if p.tok == .semicolon {
		p.a.add(flat.NodeKind.empty)
	} else {
		p.expr(.lowest)
	}
	if p.tok == .semicolon {
		p.next()
	}
	post := if p.tok != .lcbr && p.tok != .eof {
		p.assign_or_expr_inline()
	} else {
		p.a.add(flat.NodeKind.empty)
	}
	body_ids := p.parse_block_body()
	mut for_ids := []flat.NodeId{}
	for_ids << p.a.add(flat.NodeKind.empty)
	for_ids << cond
	for_ids << post
	for id in body_ids {
		for_ids << id
	}
	for_start := p.add_children(for_ids)
	for_id := p.a.add_node(flat.Node{
		kind:           .for_stmt
		children_start: for_start
		children_count: flat.child_count(for_ids.len)
	})
	block_start := p.add_children2(init_id, for_id)
	return p.a.add_node(flat.Node{
		kind:           .block
		value:          'for_c_style_multi'
		children_start: block_start
		children_count: 2
	})
}

fn (mut p Parser) for_c_style(lhs_expr flat.NodeId) flat.NodeId {
	op_id := int(p.tok)
	p.next()
	rhs := p.expr(.lowest)

	mut init_id := flat.empty_node
	if op_id == 12 {
		istart := p.add_children2(lhs_expr, rhs)
		init_id = p.a.add_node(flat.Node{
			kind:           .decl_assign
			op:             .assign
			children_start: istart
			children_count: 2
		})
	} else {
		istart := p.add_children2(lhs_expr, rhs)
		init_id = p.a.add_node(flat.Node{
			kind:           .assign
			op:             token_id_to_op(op_id)
			children_start: istart
			children_count: 2
		})
	}

	if p.tok == .semicolon {
		p.next()
	}

	cond := p.expr(.lowest)
	if p.tok == .semicolon {
		p.next()
	}

	post := if p.tok != .lcbr && p.tok != .eof {
		p.assign_or_expr_inline()
	} else {
		p.a.add(flat.NodeKind.empty)
	}

	body_ids := p.parse_block_body()

	mut ids := []flat.NodeId{}
	ids << init_id
	ids << cond
	ids << post
	for id in body_ids {
		ids << id
	}
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .for_stmt
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) for_in(first_expr flat.NodeId, first_is_mut bool) flat.NodeId {
	// first_expr is either the key var or the only var
	mut key_id := first_expr
	mut val_id := flat.empty_node
	mut value_is_mut := first_is_mut

	if p.tok == .comma {
		p.next()
		// second variable
		if p.tok == .key_mut {
			p.next()
			value_is_mut = true
		}
		val_id = p.a.add_val(.ident, p.expect_name())
	}

	return p.for_in_parts(key_id, val_id, value_is_mut)
}

fn (mut p Parser) for_in_parts(key_id flat.NodeId, val_id flat.NodeId, value_is_mut bool) flat.NodeId {
	p.check(.key_in)
	was_in_for_container := p.in_for_container
	p.in_for_container = true
	container := p.expr(.lowest)
	p.in_for_container = was_in_for_container

	// optional range: `for i in 0 .. n`
	mut range_end := flat.empty_node
	if p.tok == .dotdot {
		p.next()
		range_end = p.expr(.lowest)
	}

	body_ids := p.parse_block_body()

	mut ids := []flat.NodeId{cap: 4 + body_ids.len}
	ids << key_id
	ids << val_id
	ids << container
	if int(range_end) >= 0 {
		ids << range_end
	}
	for id in body_ids {
		ids << id
	}
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .for_in_stmt
		children_start: start
		children_count: flat.child_count(ids.len)
		// value field stores the count of header elements (key, val, container, [range_end])
		// so gen knows where body starts
		value: if int(range_end) >= 0 { '4' } else { '3' }
		op:    if value_is_mut { .amp } else { .none }
	})
}

fn (mut p Parser) match_stmt() flat.NodeId {
	p.next() // skip 'match'
	match_expr := p.expr(.lowest)
	p.check(.lcbr)

	mut ids := []flat.NodeId{cap: 8}
	ids << match_expr

	for p.tok != .rcbr && p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		ids << p.match_branch()
	}
	p.check(.rcbr)

	start := p.add_children(ids)
	match_id := p.a.add_node(flat.Node{
		kind:           .match_stmt
		children_start: start
		children_count: flat.child_count(ids.len)
	})
	if p.tok == .key_or {
		p.next()
		or_body := p.block_stmt()
		ostart := p.add_children2(match_id, or_body)
		return p.a.add_node(flat.Node{
			kind:           .or_expr
			children_start: ostart
			children_count: 2
		})
	}
	return match_id
}

fn (mut p Parser) match_branch_cond() flat.NodeId {
	if p.tok == .lsbr && p.peek() == .rsbr {
		typ := p.parse_type_name()
		elem_type := if typ.starts_with('[]') { typ[2..] } else { typ }
		return p.a.add_node(flat.Node{
			kind:  .array_init
			value: elem_type
			typ:   typ
		})
	}
	if p.tok == .name && p.lit == 'map' && p.peek() == .lsbr {
		typ := p.parse_type_name()
		return p.a.add_val(.ident, typ)
	}
	if p.tok == .name && p.peek() == .dot {
		mod_name := p.lit
		p.next()
		p.next()
		if p.tok == .name && p.lit.len > 0 && p.lit[0] >= `A` && p.lit[0] <= `Z` {
			type_name := mod_name + '.' + p.parse_type_name()
			return p.match_type_pattern_node(type_name)
		}
		field_name := p.expect_name_or_keyword()
		if field_name.len > 0 && field_name[0] >= `A` && field_name[0] <= `Z` {
			mod_id := p.a.add_val(.ident, mod_name)
			start := p.add_child(mod_id)
			return p.a.add_node(flat.Node{
				kind:           .selector
				value:          field_name
				children_start: start
				children_count: 1
			})
		}
		base_id := p.a.add_val(.ident, mod_name)
		start := p.add_child(base_id)
		sel := p.a.add_node(flat.Node{
			kind:           .selector
			value:          field_name
			children_start: start
			children_count: 1
		})
		if p.tok != .lcbr && p.tok != .comma {
			return p.expr_with_lhs(sel, .lowest)
		}
		return sel
	}
	if p.tok == .name && p.lit.len > 0 && p.lit[0] >= `A` && p.lit[0] <= `Z` {
		name := p.parse_type_name()
		return p.match_type_pattern_node(name)
	}
	if p.tok == .name && is_builtin_type(p.lit) && p.peek() == .lcbr {
		name := p.lit
		p.next()
		return p.a.add_val(.ident, name)
	}
	cond := p.expr(.lowest)
	if p.tok == .ellipsis {
		p.next()
		rhs := p.expr(.lowest)
		rstart := p.add_children2(cond, rhs)
		return p.a.add_node(flat.Node{
			kind:           .range
			children_start: rstart
			children_count: 2
		})
	}
	return cond
}

fn (mut p Parser) match_type_pattern_node(type_name string) flat.NodeId {
	if dot := top_level_dot_index(type_name) {
		base := type_name[..dot]
		name := type_name[dot + 1..]
		base_id := p.a.add_val(.ident, base)
		start := p.add_child(base_id)
		return p.a.add_node(flat.Node{
			kind:           .selector
			value:          name
			children_start: start
			children_count: 1
		})
	}
	return p.a.add_val(.ident, type_name)
}

fn top_level_dot_index(s string) ?int {
	mut depth := 0
	for i := 0; i < s.len; i++ {
		match s[i] {
			`[`, `(` {
				depth++
			}
			`]`, `)` {
				if depth > 0 {
					depth--
				}
			}
			`.` {
				if depth == 0 {
					return i
				}
			}
			else {}
		}
	}
	return none
}

fn (mut p Parser) match_branch() flat.NodeId {
	mut branch_ids := []flat.NodeId{}
	mut is_else := false
	mut n_conds := 0

	if p.tok == .key_else {
		is_else = true
		p.next()
	} else {
		branch_ids << p.match_branch_cond()
		n_conds = 1
		for p.tok == .comma {
			p.next()
			branch_ids << p.match_branch_cond()
			n_conds++
		}
	}

	branch_block_start := p.tok_pos
	p.check(.lcbr)
	block_scope := p.block_local_type_scope(branch_block_start)
	p.push_local_type_scope(block_scope)
	p.begin_comptime_value_scope()
	p.predeclare_local_type_names_in_block(branch_block_start)
	for p.tok != .rcbr && p.tok != .eof {
		if p.looks_like_match_branch_start() {
			break
		}
		id := p.stmt()
		if int(id) >= 0 {
			branch_ids << id
		}
	}
	p.check(.rcbr)
	p.end_comptime_value_scope()
	if block_scope.len > 0 {
		p.pop_local_type_scope()
	}

	bstart := p.add_children(branch_ids)
	return p.a.add_node(flat.Node{
		kind:           .match_branch
		value:          if is_else { 'else' } else { '${n_conds}' }
		children_start: bstart
		children_count: flat.child_count(branch_ids.len)
	})
}

fn (p &Parser) looks_like_match_branch_start() bool {
	if p.tok == .key_else {
		return true
	}
	return false
}

fn (mut p Parser) block_stmt() flat.NodeId {
	ids := p.parse_block_body()
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .block
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) parse_block_body() []flat.NodeId {
	block_start := p.tok_pos
	p.check(.lcbr)
	block_scope := p.block_local_type_scope(block_start)
	p.push_local_type_scope(block_scope)
	p.begin_comptime_value_scope()
	p.predeclare_local_type_names_in_block(block_start)
	mut ids := []flat.NodeId{}
	for p.tok != .rcbr && p.tok != .eof {
		id := p.stmt()
		if int(id) >= 0 {
			ids << id
		}
	}
	p.check(.rcbr)
	p.end_comptime_value_scope()
	if block_scope.len > 0 {
		p.pop_local_type_scope()
	}
	return ids
}

fn (mut p Parser) assign_or_expr_stmt() flat.NodeId {
	lhs := p.expr(.lowest)

	// multi-assign: a, b := expr1, expr2
	if p.tok == .comma {
		mut lhs_ids := []flat.NodeId{}
		lhs_ids << lhs
		for p.tok == .comma {
			p.next()
			mut lhs_is_mut := false
			if p.tok == .key_mut {
				lhs_is_mut = true
				p.next()
			}
			lhs_id := p.expr(.lowest)
			if lhs_is_mut {
				p.mark_node_mut(lhs_id)
			}
			lhs_ids << lhs_id
		}
		if p.tok == .decl_assign || token_is_assignment(p.tok) {
			op_id := int(p.tok)
			p.next()
			mut rhs_ids := []flat.NodeId{}
			rhs_ids << p.expr(.lowest)
			for p.tok == .comma {
				p.next()
				rhs_ids << p.expr(.lowest)
			}
			if p.tok == .semicolon {
				p.next()
			}
			mut all_ids := []flat.NodeId{cap: lhs_ids.len * 2}
			for i in 0 .. lhs_ids.len {
				all_ids << lhs_ids[i]
				if i < rhs_ids.len {
					all_ids << rhs_ids[i]
					if op_id == 12 {
						p.remember_comptime_decl_value(lhs_ids[i], rhs_ids[i])
					} else {
						p.forget_comptime_lhs_value(lhs_ids[i])
					}
				} else {
					p.forget_comptime_lhs_value(lhs_ids[i])
				}
			}
			istart := p.add_children(all_ids)
			return p.a.add_node(flat.Node{
				kind:           if op_id == 12 {
					flat.NodeKind.decl_assign
				} else {
					flat.NodeKind.assign
				}
				op:             token_id_to_op(op_id)
				value:          '${lhs_ids.len}'
				children_start: istart
				children_count: flat.child_count(all_ids.len)
			})
		}
		if p.tok == .semicolon {
			p.next()
		}
		mut stmt_ids := []flat.NodeId{cap: lhs_ids.len}
		for expr_id in lhs_ids {
			estart := p.add_child(expr_id)
			stmt_ids << p.a.add_node(flat.Node{
				kind:           .expr_stmt
				children_start: estart
				children_count: 1
			})
		}
		bstart := p.add_children(stmt_ids)
		// Preserve comma grouping so match tails do not absorb preceding statements.
		return p.a.add_node(flat.Node{
			kind:           .block
			value:          'comma_exprs'
			children_start: bstart
			children_count: flat.child_count(stmt_ids.len)
		})
	}

	if p.tok == .decl_assign {
		p.next()
		rhs := p.expr(.lowest)
		p.remember_comptime_decl_value(lhs, rhs)
		if p.tok == .semicolon {
			p.next()
		}
		istart := p.add_children2(lhs, rhs)
		return p.a.add_node(flat.Node{
			kind:           .decl_assign
			op:             .assign
			children_start: istart
			children_count: 2
		})
	}

	if token_is_assignment(p.tok) {
		op_id := int(p.tok)
		p.next()
		rhs := p.expr(.lowest)
		p.forget_comptime_lhs_value(lhs)
		if p.tok == .semicolon {
			p.next()
		}
		lhs_node := p.a.nodes[int(lhs)]
		kind := if lhs_node.kind == .selector {
			flat.NodeKind.selector_assign
		} else if lhs_node.kind == .index {
			flat.NodeKind.index_assign
		} else {
			flat.NodeKind.assign
		}
		istart := p.add_children2(lhs, rhs)
		return p.a.add_node(flat.Node{
			kind:           kind
			op:             token_id_to_op(op_id)
			children_start: istart
			children_count: 2
		})
	}

	if p.tok == .semicolon {
		p.next()
	}

	estart := p.add_child(lhs)
	return p.a.add_node(flat.Node{
		kind:           .expr_stmt
		children_start: estart
		children_count: 1
	})
}

fn (mut p Parser) assign_or_expr_inline() flat.NodeId {
	lhs := p.expr(.lowest)

	if token_is_assignment(p.tok) {
		op_id := int(p.tok)
		p.next()
		rhs := p.expr(.lowest)
		lhs_node := p.a.nodes[int(lhs)]
		kind := if lhs_node.kind == .selector {
			flat.NodeKind.selector_assign
		} else if lhs_node.kind == .index {
			flat.NodeKind.index_assign
		} else {
			flat.NodeKind.assign
		}
		istart := p.add_children2(lhs, rhs)
		return p.a.add_node(flat.Node{
			kind:           kind
			op:             token_id_to_op(op_id)
			children_start: istart
			children_count: 2
		})
	}

	estart := p.add_child(lhs)
	return p.a.add_node(flat.Node{
		kind:           .expr_stmt
		children_start: estart
		children_count: 1
	})
}

fn (mut p Parser) defer_stmt() flat.NodeId {
	p.next() // skip 'defer'
	mut mode := ''
	if p.tok == .lpar {
		p.next()
		if p.tok == .key_fn {
			mode = 'function'
			p.next()
		}
		p.check(.rpar)
	}
	body := p.block_stmt()
	dstart := p.add_child(body)
	return p.a.add_node(flat.Node{
		kind:           .defer_stmt
		children_start: dstart
		children_count: 1
		value:          mode
	})
}

fn (mut p Parser) assert_stmt() flat.NodeId {
	p.next() // skip 'assert'
	cond := p.expr(.lowest)
	mut ids := []flat.NodeId{}
	ids << cond
	// optional message: assert cond, 'message'
	if p.tok == .comma {
		p.next()
		ids << p.expr(.lowest)
	}
	if p.tok == .semicolon {
		p.next()
	}
	astart := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .assert_stmt
		children_start: astart
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) goto_stmt() flat.NodeId {
	p.next() // skip 'goto'
	label := p.expect_name()
	if p.tok == .semicolon {
		p.next()
	}
	return p.a.add_val(.goto_stmt, label)
}

fn (mut p Parser) asm_stmt() flat.NodeId {
	p.next() // skip 'asm'
	// consume optional volatile keyword
	if p.tok == .name && p.lit == 'volatile' {
		p.next()
	}
	// consume the asm block
	if p.tok == .lcbr {
		p.skip_block()
	}
	if p.tok == .semicolon {
		p.next()
	}
	return p.a.add(flat.NodeKind.asm_stmt)
}

// ==================== expressions (Pratt parser) ====================

fn (mut p Parser) expr(min_bp token.BindingPower) flat.NodeId {
	lhs := p.prefix_expr()
	return p.expr_with_lhs(lhs, min_bp)
}

fn (mut p Parser) expr_with_lhs(first flat.NodeId, min_bp token.BindingPower) flat.NodeId {
	mut lhs := first
	for {
		// A newline (scanned as `;`) directly followed by `.` continues the
		// expression: `expr or { ... }` / `expr` on one line, `.method()` on
		// the next. Statements can never start with `.`, so this is unambiguous.
		if p.tok == .semicolon && p.peek() == .dot {
			p.next()
			continue
		}
		// selector / method call
		if p.tok == .dot {
			lhs = p.selector_or_method(lhs)
			continue
		}
		// module-qualified struct init: module.Type{} or module.Type{field: val, ...}
		if p.tok == .lcbr {
			lhs_node := p.a.nodes[int(lhs)]
			if lhs_node.kind == .index {
				if full_name := p.generic_struct_init_type_name(lhs) {
					lhs = p.struct_init(full_name)
					continue
				}
			}
			if lhs_node.kind == .selector && lhs_node.value.len > 0 {
				base := p.a.child_node(&lhs_node, 0)
				is_c_struct := base.kind == .ident && base.value == 'C'
					&& !is_all_upper_ident(lhs_node.value)
					&& (p.peek() == .rcbr || p.peek() == .name
					|| p.peek() == .ellipsis)
				is_v_struct := base.kind == .ident && base.value != 'C' && lhs_node.value[0] >= `A`
					&& lhs_node.value[0] <= `Z`
				if is_c_struct || is_v_struct {
					full_name := '${base.value}.${lhs_node.value}'
					lhs = p.struct_init(full_name)
					continue
				}
			}
		}
		// function call
		if p.tok == .lpar {
			lhs_node := p.a.nodes[int(lhs)]
			if lhs_node.kind == .selector && lhs_node.children_count > 0 && lhs_node.value.len > 0
				&& lhs_node.value[0] >= `A` && lhs_node.value[0] <= `Z` {
				base := p.a.child_node(&lhs_node, 0)
				if base.kind == .ident && base.value != 'C' {
					full_name := '${base.value}.${lhs_node.value}'
					p.next() // skip (
					inner := p.expr(.lowest)
					p.check(.rpar)
					cstart := p.add_child(inner)
					lhs = p.a.add_node(flat.Node{
						kind:           .cast_expr
						value:          full_name
						children_start: cstart
						children_count: 1
					})
					continue
				}
			}
			lhs = p.call_args(lhs)
			continue
		}
		// index / generic
		if p.tok == .lsbr {
			base_type_name := p.resolve_local_type_name(p.type_expr_name(lhs))
			if type_name_can_init(base_type_name)
				&& p.current_generic_struct_init_suffix_followed_by_lcbr() {
				before_suffix_offset := p.s.offset
				suffix := p.parse_type_generic_suffix()
				if p.s.offset != before_suffix_offset && p.tok == .lcbr {
					lhs = p.struct_init(p.resolve_local_type_name(base_type_name + suffix))
					continue
				}
			}
			lhs = p.index_expr(lhs)
			continue
		}
		// postfix: ++ -- ? !
		if token_is_postfix(p.tok) {
			op_id := int(p.tok)
			p.next()
			pstart := p.add_child(lhs)
			lhs = p.a.add_node(flat.Node{
				kind:           .postfix
				op:             token_id_to_op(op_id)
				children_start: pstart
				children_count: 1
			})
			continue
		}
		// postfix `!` error propagation: expr!
		if p.tok == .not {
			p.next()
			ostart := p.add_children2(lhs, p.a.add(flat.NodeKind.empty))
			lhs = p.a.add_node(flat.Node{
				kind:           .or_expr
				value:          '!'
				children_start: ostart
				children_count: 2
			})
			continue
		}
		// postfix `?` optional propagation: expr?
		if p.tok == .question {
			p.next()
			ostart := p.add_children2(lhs, p.a.add(flat.NodeKind.empty))
			lhs = p.a.add_node(flat.Node{
				kind:           .or_expr
				value:          '?'
				children_start: ostart
				children_count: 2
			})
			continue
		}
		// `as` cast: expr as Type
		if p.tok == .key_as {
			p.next()
			type_name := p.parse_type_name()
			astart := p.add_child(lhs)
			lhs = p.a.add_node(flat.Node{
				kind:           .as_expr
				value:          type_name
				children_start: astart
				children_count: 1
			})
			continue
		}
		// `or` block: expr or { ... }
		if p.tok == .key_or {
			p.next()
			or_body := p.block_stmt()
			ostart := p.add_children2(lhs, or_body)
			lhs = p.a.add_node(flat.Node{
				kind:           .or_expr
				children_start: ostart
				children_count: 2
			})
			continue
		}
		// range: expr .. expr
		if p.tok == .dotdot {
			if int(min_bp) > int(token.BindingPower.lowest) {
				break
			}
			p.next()
			rhs := p.expr(.lowest)
			rstart := p.add_children2(lhs, rhs)
			lhs = p.a.add_node(flat.Node{
				kind:           .range
				children_start: rstart
				children_count: 2
			})
			continue
		}
		// `is` / `!is` / `not_is` type check
		if p.tok == .key_is || p.tok == .not_is {
			bp := token.BindingPower.compare
			if int(bp) < int(min_bp) {
				break
			}
			is_negated := p.tok == .not_is
			p.next()
			type_name := p.parse_type_name()
			istart := p.add_child(lhs)
			is_node := p.a.add_node(flat.Node{
				kind:           .is_expr
				value:          type_name
				children_start: istart
				children_count: 1
			})
			if is_negated {
				nstart := p.add_child(is_node)
				lhs = p.a.add_node(flat.Node{
					kind:           .prefix
					op:             .not
					children_start: nstart
					children_count: 1
				})
			} else {
				lhs = is_node
			}
			continue
		}
		if p.tok == .not && p.peek() == .key_is {
			bp := token.BindingPower.compare
			if int(bp) < int(min_bp) {
				break
			}
			p.next() // skip !
			p.next() // skip is
			type_name := p.parse_type_name()
			istart := p.add_child(lhs)
			is_node := p.a.add_node(flat.Node{
				kind:           .is_expr
				value:          type_name
				children_start: istart
				children_count: 1
			})
			nstart := p.add_child(is_node)
			lhs = p.a.add_node(flat.Node{
				kind:           .prefix
				op:             .not
				children_start: nstart
				children_count: 1
			})
			continue
		}
		// `in` / `!in` / `not_in`
		if p.tok == .key_in || p.tok == .not_in {
			is_negated := p.tok == .not_in
			bp := token.BindingPower.compare
			if int(bp) < int(min_bp) {
				break
			}
			p.next()
			mut rhs := p.expr(token.BindingPower.bit_or)
			if p.tok == .dotdot {
				p.next()
				range_rhs := p.expr(.lowest)
				rstart := p.add_children2(rhs, range_rhs)
				rhs = p.a.add_node(flat.Node{
					kind:           .range
					children_start: rstart
					children_count: 2
				})
			}
			istart := p.add_children2(lhs, rhs)
			in_node := p.a.add_node(flat.Node{
				kind:           .in_expr
				children_start: istart
				children_count: 2
			})
			if is_negated {
				nstart := p.add_child(in_node)
				lhs = p.a.add_node(flat.Node{
					kind:           .prefix
					op:             .not
					children_start: nstart
					children_count: 1
				})
			} else {
				lhs = in_node
			}
			continue
		}
		if p.tok == .not && p.peek() == .key_in {
			bp := token.BindingPower.compare
			if int(bp) < int(min_bp) {
				break
			}
			p.next() // skip !
			p.next() // skip in
			mut rhs := p.expr(token.BindingPower.bit_or)
			if p.tok == .dotdot {
				p.next()
				range_rhs := p.expr(.lowest)
				rstart := p.add_children2(rhs, range_rhs)
				rhs = p.a.add_node(flat.Node{
					kind:           .range
					children_start: rstart
					children_count: 2
				})
			}
			istart := p.add_children2(lhs, rhs)
			in_node := p.a.add_node(flat.Node{
				kind:           .in_expr
				children_start: istart
				children_count: 2
			})
			nstart := p.add_child(in_node)
			lhs = p.a.add_node(flat.Node{
				kind:           .prefix
				op:             .not
				children_start: nstart
				children_count: 1
			})
			continue
		}
		// skip auto-semicolons before infix operators (multi-line expressions)
		if p.tok == .semicolon {
			peek_tok := p.peek()
			if (token_is_infix(peek_tok) || peek_tok == .key_as) && int(peek_tok) != 85
				&& int(peek_tok) != 0 {
				p.next()
				continue
			}
		}
		if token_is_assignment(p.tok) {
			break
		}
		// infix operators
		if !token_is_infix(p.tok) {
			break
		}
		op_id := int(p.tok)
		bp := token_id_left_binding_power(op_id)
		if int(bp) < int(min_bp) {
			break
		}
		p.next()
		rhs := p.expr(token_id_right_binding_power(op_id))
		istart := p.add_children2(lhs, rhs)
		lhs = p.a.add_node(flat.Node{
			kind:           .infix
			op:             token_id_to_op(op_id)
			children_start: istart
			children_count: 2
		})
	}

	return lhs
}

fn (mut p Parser) sql_expr() flat.NodeId {
	db_expr := if p.tok != .lcbr && p.tok != .eof {
		p.expr(.lowest)
	} else {
		flat.empty_node
	}
	tokens := p.sql_block_tokens()
	typ := sql_result_type(tokens)
	mut children_start := 0
	mut children_count := 0
	if int(db_expr) >= 0 {
		children_start = p.add_child(db_expr)
		children_count = 1
	}
	return p.a.add_node(flat.Node{
		kind:           .sql_expr
		value:          tokens.join(' ')
		typ:            typ
		children_start: children_start
		children_count: flat.child_count(children_count)
	})
}

fn (mut p Parser) starts_sql_expr() bool {
	if p.tok != .name {
		return false
	}
	saved_s := p.s
	saved_tok := p.tok
	saved_lit := p.lit
	saved_tok_pos := p.tok_pos
	saved_peek_tok := p.peek_tok
	saved_peek_lit := p.peek_lit
	saved_peek_pos := p.peek_pos
	saved_has_peek := p.has_peek
	p.next()
	for p.tok == .dot {
		p.next()
		if p.tok != .name && !p.tok.is_keyword() {
			break
		}
		p.next()
	}
	ok := p.tok == .lcbr
	p.s = saved_s
	p.tok = saved_tok
	p.lit = saved_lit
	p.tok_pos = saved_tok_pos
	p.peek_tok = saved_peek_tok
	p.peek_lit = saved_peek_lit
	p.peek_pos = saved_peek_pos
	p.has_peek = saved_has_peek
	return ok
}

fn (mut p Parser) sql_block_tokens() []string {
	mut tokens := []string{}
	if p.tok != .lcbr {
		return tokens
	}
	mut depth := 1
	p.next()
	for depth > 0 && p.tok != .eof {
		if p.tok == .lcbr {
			depth++
		} else if p.tok == .rcbr {
			depth--
			if depth == 0 {
				p.next()
				break
			}
		}
		if depth > 0 {
			text := p.sql_token_text()
			if text.len > 0 {
				tokens << text
			}
			p.next()
		}
	}
	return tokens
}

fn (p &Parser) sql_token_text() string {
	if p.lit.len > 0 {
		return p.lit
	}
	return match p.tok {
		.key_select { 'select' }
		.key_in { 'in' }
		.key_or { 'or' }
		.key_as { 'as' }
		.key_true { 'true' }
		.key_false { 'false' }
		else { '' }
	}
}

fn sql_result_type(tokens []string) string {
	if tokens.len == 0 {
		return '!void'
	}
	if tokens[0] != 'select' {
		return '!void'
	}
	if tokens.len > 1 && tokens[1] == 'count' {
		return '!int'
	}
	for i, tok in tokens {
		if tok == 'from' && i + 1 < tokens.len {
			table := sql_type_name(tokens[i + 1])
			if table.len > 0 {
				return '![]${table}'
			}
		}
	}
	return '![]int'
}

fn sql_type_name(raw string) string {
	mut out := strings.new_builder(raw.len)
	for ch in raw {
		if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
			|| (ch >= `0` && ch <= `9`) || ch == `_` || ch == `.` {
			out.write_u8(ch)
		} else {
			break
		}
	}
	return out.str()
}

fn is_float_number_literal(val string) bool {
	if val.len > 2 && val[0] == `0` {
		second := val[1]
		if second == `x` || second == `X` || second == `b` || second == `B` || second == `o`
			|| second == `O` {
			return false
		}
	}
	return val.contains('.') || val.contains('e') || val.contains('E')
}

fn (mut p Parser) prefix_expr() flat.NodeId {
	tok_id := int(p.tok)
	if tok_id == 92 {
		val := p.lit
		p.next()
		kind_id := if is_float_number_literal(val) {
			2
		} else {
			1
		}
		return p.a.add_val_id(kind_id, val)
	}
	if tok_id == 107 {
		return p.string_literal()
	}
	if tok_id == 7 {
		val := p.lit
		p.next()
		return p.a.add_val_id(4, val)
	}
	if tok_id == 66 {
		p.next()
		return p.a.add_val_id(3, 'true')
	}
	if tok_id == 36 {
		p.next()
		return p.a.add_val_id(3, 'false')
	}
	if tok_id == 53 {
		p.next()
		return p.a.add_id(28)
	}
	if tok_id == 54 {
		p.next()
		return p.a.add_id(29)
	}
	if tok_id == 3 {
		p.next()
		inner := p.expr(.highest)
		return p.a.add_node(flat.Node{
			kind:           .prefix
			op:             .arrow
			children_start: p.add_child(inner)
			children_count: 1
		})
	}
	if tok_id == 6 || tok_id == 81 || tok_id == 85 || tok_id == 89 {
		p.next()
		operand := p.expr(.highest)
		pstart := p.add_child(operand)
		return p.a.add_node(flat.Node{
			kind:           .prefix
			op:             token_id_to_op(tok_id)
			children_start: pstart
			children_count: 1
		})
	}
	match p.tok {
		.number {
			val := p.lit
			p.next()
			kind_id := if is_float_number_literal(val) {
				2
			} else {
				1
			}
			return p.a.add_val_id(kind_id, val)
		}
		.string {
			return p.string_literal()
		}
		.char {
			val := p.lit
			p.next()
			return p.a.add_val_id(4, val)
		}
		.key_true {
			p.next()
			return p.a.add_val_id(3, 'true')
		}
		.key_false {
			p.next()
			return p.a.add_val_id(3, 'false')
		}
		.key_nil {
			p.next()
			return p.a.add_id(28)
		}
		.key_none {
			p.next()
			return p.a.add_id(29)
		}
		.arrow {
			p.next()
			inner := p.expr(.highest)
			return p.a.add_node(flat.Node{
				kind:           .prefix
				op:             .arrow
				children_start: p.add_child(inner)
				children_count: 1
			})
		}
		.key_mut, .key_shared {
			p.next()
			return p.prefix_expr()
		}
		.name, .key_module {
			name_pos := p.tok_pos
			name := p.lit
			p.next()
			if name == 'sql' && p.starts_sql_expr() {
				return p.sql_expr()
			}
			if name == '@FILE' {
				return p.a.add_val_id(5, os.real_path(p.cur_file))
			}
			if name == '@DIR' {
				return p.a.add_val_id(5, os.real_path(os.dir(p.cur_file)))
			}
			if name == '@VMODROOT' {
				return p.a.add_val_id(5, os.real_path(vmod_root_for_file(p.cur_file)))
			}
			if name == '@VMOD_FILE' {
				vmod_file := os.join_path_single(vmod_root_for_file(p.cur_file), 'v.mod')
				content := os.read_file(vmod_file) or {
					message := p.a.add_val_id(5,
						'@VMOD_FILE can only be used in projects that have a v.mod file')
					return p.make_compile_error_call(message)
				}
				return p.a.add_val_id(5, content.replace('\r\n', '\n'))
			}
			if name == '@VEXEROOT' {
				return p.a.add_val_id(5, p.prefs.vroot)
			}
			if name == '@VEXE' {
				if p.prefs.vexe.len > 0 {
					return p.a.add_val_id(5, p.prefs.vexe)
				}
				return p.a.add_val_id(5, p.prefs.vroot + '/v')
			}
			if name == '@LINE' {
				// like V1, `@LINE` is a string literal holding the 1-based line number
				return p.a.add_val_id(5, p.line_nr_for_pos(name_pos).str())
			}
			if name == '@FILE_LINE' {
				return p.a.add_val_id(5,
					'${os.real_path(p.cur_file)}:${p.line_nr_for_pos(name_pos)}')
			}
			if name == '@MOD' {
				if p.cur_module.len == 0 {
					return p.a.add_val_id(5, 'main')
				}
				return p.a.add_val_id(5, p.cur_module)
			}
			if name == '@FN' {
				return p.a.add_val_id(5, p.cur_fn.all_after_last('.'))
			}
			if name == '@METHOD' {
				return p.a.add_val_id(5, if p.cur_struct.len > 0 {
					'${p.cur_struct}.${p.cur_fn.all_after_last('.')}'
				} else {
					p.cur_fn.all_after_last('.')
				})
			}
			if name == '@STRUCT' {
				return p.a.add_val_id(5, p.cur_struct)
			}
			if name == '@LOCATION' {
				module_name := if p.cur_module.len > 0 { p.cur_module } else { 'main' }
				fn_name := p.cur_fn.all_after_last('.')
				mut method_name := '${module_name}.${fn_name}'
				if p.cur_struct.len > 0 {
					if p.cur_method_is_static {
						method_name = '${module_name}.${p.cur_struct}.${fn_name} (static)'
					} else {
						method_name = '${module_name}.${p.cur_struct}{}.${fn_name}'
					}
				}
				return p.a.add_val_id(5,
					'${p.cur_file}:${p.line_nr_for_pos(name_pos)}, ${method_name}')
			}
			if name == '@BUILD_DATE' {
				return p.a.add_val_id(5, p.prefs.build_date)
			}
			if name == '@BUILD_TIME' {
				return p.a.add_val_id(5, p.prefs.build_time)
			}
			if name == '@BUILD_TIMESTAMP' {
				return p.a.add_val_id(5, p.prefs.build_timestamp)
			}
			if name == '@OS' {
				return p.a.add_val_id(5, p.prefs.normalized_target_os())
			}
			if name == '@CCOMPILER' {
				return p.a.add_val_id(5, @CCOMPILER)
			}
			if name == '@BACKEND' {
				return p.a.add_val_id(5, p.prefs.backend)
			}
			if name == '@PLATFORM' {
				return p.a.add_val_id(5, @PLATFORM)
			}
			if name == '@VCURRENTHASH' || name == '@VHASH' {
				return p.a.add_val_id(5, '')
			}
			if name == 'chan' && p.can_start_type_name() {
				elem_type := p.parse_type_name()
				chan_type := 'chan ${elem_type}'
				if p.tok == .lcbr {
					return p.struct_init(chan_type)
				}
				return p.a.add_val(.ident, chan_type)
			}
			// map init: map[K]V{} or map[K]V{k1: v1, ...}
			if name == 'map' && p.tok == .lsbr {
				p.next() // skip [
				key_type := p.parse_type_name()
				p.check(.rsbr)
				val_type := p.parse_type_name()
				map_type := 'map[${key_type}]${val_type}'
				if p.tok == .lcbr {
					p.next() // skip {
					mut ids := []flat.NodeId{}
					for p.tok != .rcbr && p.tok != .eof {
						if p.tok == .semicolon {
							p.next()
							continue
						}
						k := p.expr(.lowest)
						p.check(.colon)
						v := p.expr(.lowest)
						ids << k
						ids << v
						if p.tok == .comma || p.tok == .semicolon {
							p.next()
						}
					}
					p.check(.rcbr)
					if ids.len > 0 {
						istart := p.add_children(ids)
						return p.a.add_node(flat.Node{
							kind:           .map_init
							value:          map_type
							children_start: istart
							children_count: flat.child_count(ids.len)
						})
					}
				}
				return p.a.add_val(.map_init, map_type)
			}
			local_type_name := p.resolve_local_type_name(name)
			// struct init: Name{...}; vlib/builtin also uses concrete lowercase
			// runtime structs like array{} and string{}.
			if !p.in_for_container && p.tok == .lcbr && local_type_name.len > 0
				&& ((local_type_name[0] >= `A` && local_type_name[0] <= `Z`)
				|| name in ['array', 'string', 'map', 'mapnode', '_result', '_option']) {
				return p.struct_init(local_type_name)
			}
			// type cast: TypeName(expr) or builtin_type(expr)
			if p.tok == .lpar && local_type_name.len > 0
				&& ((local_type_name[0] >= `A` && local_type_name[0] <= `Z`)
				|| is_builtin_type(name)) {
				p.next() // skip (
				inner := p.expr(.lowest)
				p.check(.rpar)
				cstart := p.add_child(inner)
				return p.a.add_node(flat.Node{
					kind:           .cast_expr
					value:          local_type_name
					children_start: cstart
					children_count: 1
				})
			}
			return p.a.add_val(.ident, name)
		}
		.lpar {
			p.next()
			inner := p.expr(.lowest)
			p.check(.rpar)
			pstart := p.add_child(inner)
			return p.a.add_node(flat.Node{
				kind:           .paren
				children_start: pstart
				children_count: 1
			})
		}
		.lcbr {
			p.next()
			mut ids := []flat.NodeId{}
			for p.tok != .rcbr && p.tok != .eof {
				if p.tok == .semicolon || p.tok == .comma {
					p.next()
					continue
				}
				k := p.expr(.lowest)
				p.check(.colon)
				v := p.expr(.lowest)
				ids << k
				ids << v
				if p.tok == .comma || p.tok == .semicolon {
					p.next()
				}
			}
			p.check(.rcbr)
			start := p.add_children(ids)
			return p.a.add_node(flat.Node{
				kind:           .map_init
				children_start: start
				children_count: flat.child_count(ids.len)
			})
		}
		.amp {
			p.next()
			if p.tok == .amp {
				p.next()
				if p.tok == .name && is_builtin_type(p.lit) {
					type_name := '&&${p.lit}'
					p.next()
					if p.tok == .lpar {
						p.next()
						inner := p.expr(.lowest)
						p.check(.rpar)
						cstart := p.add_child(inner)
						return p.a.add_node(flat.Node{
							kind:           .cast_expr
							value:          type_name
							children_start: cstart
							children_count: 1
						})
					}
					id := p.a.add_val(.ident, type_name[2..])
					first := p.a.add_node(flat.Node{
						kind:           .prefix
						op:             .amp
						children_start: p.add_child(id)
						children_count: 1
					})
					return p.a.add_node(flat.Node{
						kind:           .prefix
						op:             .amp
						children_start: p.add_child(first)
						children_count: 1
					})
				}
			} else if p.tok == .name && is_builtin_type(p.lit) && !(p.lit == 'map'
				&& p.peek() == .lsbr) {
				name := p.lit
				type_name := '&${name}'
				p.next()
				if p.tok == .lpar {
					p.next()
					inner := p.expr(.lowest)
					p.check(.rpar)
					cstart := p.add_child(inner)
					return p.a.add_node(flat.Node{
						kind:           .cast_expr
						value:          type_name
						children_start: cstart
						children_count: 1
					})
				}
				if p.tok == .lcbr {
					inner := p.struct_init(name)
					return p.a.add_node(flat.Node{
						kind:           .prefix
						op:             .amp
						children_start: p.add_child(inner)
						children_count: 1
					})
				}
				id := p.a.add_val(.ident, name)
				return p.a.add_node(flat.Node{
					kind:           .prefix
					op:             .amp
					children_start: p.add_child(id)
					children_count: 1
				})
			} else if p.tok == .name && p.lit == 'C' && p.peek() == .dot {
				if cast := p.pointer_cast_expr_from_current_amp() {
					return cast
				}
			} else if p.tok == .lsbr && p.peek() == .rsbr {
				type_name := '&' + p.parse_type_name()
				if p.tok == .lpar {
					p.next()
					inner := p.expr(.lowest)
					p.check(.rpar)
					cstart := p.add_child(inner)
					return p.a.add_node(flat.Node{
						kind:           .cast_expr
						value:          type_name
						children_start: cstart
						children_count: 1
					})
				}
				if p.tok == .lcbr && type_name.starts_with('&[]') {
					inner := p.array_init_after_element_type(type_name[3..])
					return p.a.add_node(flat.Node{
						kind:           .prefix
						op:             .amp
						typ:            type_name
						children_start: p.add_child(inner)
						children_count: 1
					})
				}
			}
			operand := p.expr(.highest)
			pstart := p.add_child(operand)
			return p.a.add_node(flat.Node{
				kind:           .prefix
				op:             .amp
				children_start: pstart
				children_count: 1
			})
		}
		.and {
			p.next()
			if p.tok == .name && is_builtin_type(p.lit) {
				type_name := '&&${p.lit}'
				p.next()
				if p.tok == .lpar {
					p.next()
					inner := p.expr(.lowest)
					p.check(.rpar)
					cstart := p.add_child(inner)
					return p.a.add_node(flat.Node{
						kind:           .cast_expr
						value:          type_name
						children_start: cstart
						children_count: 1
					})
				}
			}
			return p.a.add(.empty)
		}
		.minus, .not, .bit_not, .mul {
			op_id := int(p.tok)
			p.next()
			operand := p.expr(.highest)
			pstart := p.add_child(operand)
			return p.a.add_node(flat.Node{
				kind:           .prefix
				op:             token_id_to_op(op_id)
				children_start: pstart
				children_count: 1
			})
		}
		.question {
			p.next()
			inner_type := p.parse_type_name()
			type_name := if inner_type.len > 0 { '?${inner_type}' } else { '?' }
			if p.tok == .lpar {
				p.next()
				inner := p.expr(.lowest)
				p.check(.rpar)
				cstart := p.add_child(inner)
				return p.a.add_node(flat.Node{
					kind:           .cast_expr
					value:          type_name
					children_start: cstart
					children_count: 1
				})
			}
			if p.tok == .lcbr {
				return p.struct_init(type_name)
			}
			return p.a.add(flat.NodeKind.empty)
		}
		.key_if {
			return p.if_stmt()
		}
		.key_match {
			return p.match_stmt()
		}
		.key_fn {
			return p.fn_literal()
		}
		.key_struct {
			p.next()
			init_id := p.struct_init('struct')
			init := p.a.nodes[int(init_id)]
			if name := p.anonymous_struct_type_for_literal(init) {
				p.a.nodes[int(init_id)].value = name
			}
			return init_id
		}
		.key_go, .key_spawn {
			p.next()
			inner := p.expr(.lowest)
			sstart := p.add_child(inner)
			return p.a.add_node(flat.Node{
				kind:           .spawn_expr
				children_start: sstart
				children_count: 1
			})
		}
		.key_lock, .key_rlock {
			return p.lock_expr()
		}
		.key_select {
			if p.peek() == .lpar {
				p.next()
				return p.a.add_val(.ident, 'select')
			}
			return p.select_expr()
		}
		.key_sizeof {
			return p.sizeof_expr()
		}
		.key_typeof {
			return p.typeof_expr()
		}
		.key_offsetof {
			return p.offsetof_expr()
		}
		.key_dump {
			return p.dump_expr()
		}
		.key_likely, .key_unlikely {
			p.next()
			p.check(.lpar)
			inner := p.expr(.lowest)
			p.check(.rpar)
			return inner
		}
		.key_isreftype {
			p.next()
			p.check(.lpar)
			p.parse_type_name()
			p.check(.rpar)
			return p.a.add_val_id(3, 'false')
		}
		.dot {
			// enum value: .member
			p.next()
			member := p.expect_name_or_keyword()
			return p.a.add_val(.enum_val, member)
		}
		.lsbr {
			return p.array_literal()
		}
		.key_unsafe {
			p.next()
			return p.block_stmt()
		}
		.ellipsis {
			// spread: ...expr
			p.next()
			inner := p.expr(.lowest)
			pstart := p.add_child(inner)
			return p.a.add_node(flat.Node{
				kind:           .prefix
				op:             .none
				value:          '...'
				children_start: pstart
				children_count: 1
			})
		}
		.dollar {
			return p.parse_comptime_expr()
		}
		else {
			p.next()
			return p.a.add(flat.NodeKind.empty)
		}
	}
}

fn (mut p Parser) pointer_cast_expr_from_current_amp() ?flat.NodeId {
	saved_s := p.s
	saved_tok := p.tok
	saved_lit := p.lit
	saved_tok_pos := p.tok_pos
	saved_peek_tok := p.peek_tok
	saved_peek_lit := p.peek_lit
	saved_peek_pos := p.peek_pos
	saved_has_peek := p.has_peek
	type_name := '&' + p.parse_type_name()
	if type_name.len > 1 && p.tok == .lpar {
		p.next()
		inner := p.expr(.lowest)
		p.check(.rpar)
		cstart := p.add_child(inner)
		return p.a.add_node(flat.Node{
			kind:           .cast_expr
			value:          type_name
			children_start: cstart
			children_count: 1
		})
	}
	p.s = saved_s
	p.tok = saved_tok
	p.lit = saved_lit
	p.tok_pos = saved_tok_pos
	p.peek_tok = saved_peek_tok
	p.peek_lit = saved_peek_lit
	p.peek_pos = saved_peek_pos
	p.has_peek = saved_has_peek
	return none
}

fn (mut p Parser) selector_or_method(lhs flat.NodeId) flat.NodeId {
	p.next() // skip '.'
	if p.tok == .dollar {
		// Compile-time field selector `receiver.$(field.name)`: the field name is resolved when
		// the enclosing `$for` loop is unrolled. `receiver.$method()` binds its identifier to the
		// innermost methods loop. Valid selectors use marker `$`; unresolved shorthand keeps its
		// spelling in the marker so the checker reports it instead of deferring it to cgen.
		p.next() // skip $
		mut selector_value := '$'
		inner := if p.tok == .lpar {
			p.next()
			name_expr := p.expr(.lowest)
			p.check(.rpar)
			name_expr
		} else {
			name := p.expect_name_or_keyword()
			valid_method_name := p.comptime_method_var.len > 0
				&& name in ['method', p.comptime_method_var]
			if !valid_method_name {
				selector_value = '$${name}'
			}
			resolved_name := if name == 'method' && valid_method_name {
				p.comptime_method_var
			} else {
				name
			}
			p.a.add_val(.ident, resolved_name)
		}
		sel_start := p.add_children2(lhs, inner)
		sel := p.a.add_node(flat.Node{
			kind:           .selector
			value:          selector_value
			children_start: sel_start
			children_count: 2
		})
		if p.tok == .lpar {
			return p.call_args(sel)
		}
		return sel
	}
	field_name := p.expect_name_or_keyword()
	sel_start := p.add_child(lhs)
	sel := p.a.add_node(flat.Node{
		kind:           .selector
		value:          field_name
		children_start: sel_start
		children_count: 1
	})
	if p.tok == .lpar {
		lhs_node := p.a.nodes[int(lhs)]
		if lhs_node.kind == .ident && lhs_node.value != 'C' && field_name.len > 0
			&& field_name[0] >= `A` && field_name[0] <= `Z` {
			full_name := '${lhs_node.value}.${field_name}'
			p.next() // skip (
			inner := p.expr(.lowest)
			p.check(.rpar)
			cstart := p.add_child(inner)
			return p.a.add_node(flat.Node{
				kind:           .cast_expr
				value:          full_name
				children_start: cstart
				children_count: 1
			})
		}
		return p.call_args(sel)
	}
	return sel
}

fn (mut p Parser) call_args(fn_expr flat.NodeId) flat.NodeId {
	p.check(.lpar)
	mut ids := []flat.NodeId{}
	ids << fn_expr
	for p.tok != .rpar && p.tok != .eof {
		prev_offset := p.s.offset
		prev_tok := p.tok
		mut arg_is_shared := false
		mut arg_is_mut := false
		if p.tok == .key_shared {
			arg_is_shared = true
			p.next()
		} else if p.tok == .key_mut {
			arg_is_mut = true
			p.next()
		}
		// vararg spread: ...expr
		if p.tok == .ellipsis {
			p.next()
			inner := p.expr(.lowest)
			sstart := p.add_child(inner)
			ids << p.a.add_node(flat.Node{
				kind:           .prefix
				op:             .none
				value:          '...'
				children_start: sstart
				children_count: 1
			})
		} else if p.tok == .logical_or {
			// lambda no args: || expr
			p.next()
			lambda_body := p.expr(.lowest)
			lstart := p.add_child(lambda_body)
			ids << p.a.add_node(flat.Node{
				kind:           .lambda_expr
				children_start: lstart
				children_count: 1
			})
		} else if p.tok == .pipe {
			// lambda with args: |a, b| expr
			p.next()
			mut lambda_params := []flat.NodeId{}
			lambda_params << p.a.add_val(.ident, p.expect_name())
			for p.tok == .comma {
				p.next()
				lambda_params << p.a.add_val(.ident, p.expect_name())
			}
			p.check(.pipe)
			lambda_body := p.expr(.lowest)
			mut lids := lambda_params.clone()
			lids << lambda_body
			lstart := p.add_children(lids)
			ids << p.a.add_node(flat.Node{
				kind:           .lambda_expr
				children_start: lstart
				children_count: flat.child_count(lids.len)
			})
		} else {
			mut arg := p.expr(.lowest)
			if arg_is_mut {
				p.a.set_node_is_mut(arg, true)
			}
			if arg_is_shared {
				arg = p.a.add_node(flat.Node{
					kind:           .prefix
					value:          'shared'
					children_start: p.add_child(arg)
					children_count: 1
				})
			}
			// struct config syntax: name: value
			if p.tok == .colon {
				p.next()
				val := p.expr(.lowest)
				arg_node := p.a.nodes[int(arg)]
				vstart := p.add_child(val)
				ids << p.a.add_node(flat.Node{
					kind:           .field_init
					value:          arg_node.value
					children_start: vstart
					children_count: 1
				})
				if p.tok == .semicolon {
					p.next()
				}
			} else {
				ids << arg
			}
		}
		if p.tok == .comma {
			p.next()
		}
		if p.s.offset == prev_offset && p.tok == prev_tok {
			p.next()
		}
	}
	p.check(.rpar)
	cstart := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .call
		children_start: cstart
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) index_expr(lhs flat.NodeId) flat.NodeId {
	// `a#[..]` gated slice: clamped, non-panicking (lowers to slice_ni/substr_ni)
	gated_op := if p.lit == '#' { flat.Op.gated_index } else { flat.Op.none }
	p.check(.lsbr)
	// range index: arr[..b]
	if p.tok == .dotdot {
		p.next()
		mut end_id := flat.empty_node
		if p.tok != .rsbr {
			end_id = p.expr(.lowest)
		}
		p.check(.rsbr)
		start_id := p.a.add(flat.NodeKind.empty)
		mut ids := []flat.NodeId{}
		ids << lhs
		ids << start_id
		if int(end_id) >= 0 {
			ids << end_id
		}
		istart := p.add_children(ids)
		return p.a.add_node(flat.Node{
			kind:           .index
			op:             gated_op
			value:          'range'
			children_start: istart
			children_count: flat.child_count(ids.len)
		})
	}
	idx := p.expr(.logical_or)
	// fast path for the common simple index `arr[i]`: avoid the children array
	if p.tok == .rsbr {
		p.next()
		type_arg := if p.tok == .lpar { p.generic_call_type_arg_id(idx) } else { idx }
		istart := p.add_children2(lhs, type_arg)
		return p.a.add_node(flat.Node{
			kind:           .index
			op:             gated_op
			children_start: istart
			children_count: 2
		})
	}
	mut ids := []flat.NodeId{}
	ids << lhs
	ids << idx
	for p.tok == .comma {
		p.next()
		ids << p.expr(.logical_or)
	}
	// range index: arr[a..b]
	if p.tok == .dotdot {
		p.next()
		mut end_id := flat.empty_node
		if p.tok != .rsbr {
			end_id = p.expr(.lowest)
		}
		p.check(.rsbr)
		mut range_ids := []flat.NodeId{}
		range_ids << lhs
		range_ids << idx
		if int(end_id) >= 0 {
			range_ids << end_id
		}
		istart := p.add_children(range_ids)
		return p.a.add_node(flat.Node{
			kind:           .index
			op:             gated_op
			value:          'range'
			children_start: istart
			children_count: flat.child_count(range_ids.len)
		})
	}
	p.check(.rsbr)
	if p.tok == .lpar {
		for i in 1 .. ids.len {
			ids[i] = p.generic_call_type_arg_id(ids[i])
		}
	}
	istart := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .index
		op:             gated_op
		children_start: istart
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) generic_call_type_arg_id(id flat.NodeId) flat.NodeId {
	name := p.type_expr_name(id)
	resolved := p.resolve_local_type_name(name)
	if name.len == 0 || resolved == name {
		return id
	}
	return p.a.add_node(flat.Node{
		kind:  .ident
		value: resolved
	})
}

fn (p &Parser) generic_struct_init_type_name(id flat.NodeId) ?string {
	type_name := p.resolve_local_type_name(p.type_expr_name(id))
	if !generic_type_name_can_init(type_name) {
		if p.index_expr_is_lifetime_erased_type_name(id) && type_name_can_init(type_name) {
			return type_name
		}
		return none
	}
	return type_name
}

fn (p &Parser) index_expr_is_lifetime_erased_type_name(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := p.a.nodes[int(id)]
	if node.kind != .index || node.children_count < 2 || node.value == 'range' {
		return false
	}
	mut saw_lifetime_arg := false
	for i in 1 .. node.children_count {
		child := p.a.child(&node, i)
		if p.type_expr_is_lifetime_arg(child) {
			saw_lifetime_arg = true
			continue
		}
		if p.type_expr_name(child).len > 0 {
			return false
		}
	}
	return saw_lifetime_arg
}

fn (p &Parser) type_expr_is_lifetime_arg(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	node := p.a.nodes[int(id)]
	return node.kind == .prefix && node.op == .xor && node.children_count == 1
}

fn (p &Parser) type_expr_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := p.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := p.type_expr_name(p.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return '${base}.${node.value}'
		}
		.index {
			if node.children_count < 2 || node.value == 'range' {
				return ''
			}
			base := p.type_expr_name(p.a.child(&node, 0))
			if base.len == 0 {
				return ''
			}
			mut args := []string{}
			for i in 1 .. node.children_count {
				child_id := p.a.child(&node, i)
				if p.type_expr_is_lifetime_arg(child_id) {
					continue
				}
				arg := p.resolve_local_type_name(p.type_expr_name(child_id))
				if arg.len == 0 {
					return ''
				}
				args << arg
			}
			if args.len == 0 {
				return base
			}
			return '${base}[${args.join(', ')}]'
		}
		.array_init {
			if node.value.len == 0 {
				return ''
			}
			return '[]${p.resolve_local_type_name(node.value)}'
		}
		.prefix {
			if node.children_count == 0 {
				return ''
			}
			child := p.resolve_local_type_name(p.type_expr_name(p.a.child(&node, 0)))
			if child.len == 0 {
				return ''
			}
			if node.op == .amp {
				return '&${child}'
			}
			return child
		}
		else {
			return ''
		}
	}
}

fn generic_type_name_can_init(type_name string) bool {
	if !type_name.contains('[') {
		return false
	}
	base := type_name.all_before('[')
	return type_name_can_init(base)
}

fn type_name_can_init(type_name string) bool {
	if type_name.len == 0 {
		return false
	}
	base := type_name.all_before('[')
	short := if base.contains('.') { base.all_after_last('.') } else { base }
	return short.len > 0 && short[0] >= `A` && short[0] <= `Z`
}

fn (mut p Parser) struct_init(name string) flat.NodeId {
	p.check(.lcbr)
	mut ids := []flat.NodeId{}
	// assoc syntax: Type{...base, field: val}
	if p.tok == .ellipsis {
		p.next()
		base_expr := p.expr(.lowest)
		if p.tok == .comma || p.tok == .semicolon {
			p.next()
		}
		mut field_ids := []flat.NodeId{}
		field_ids << base_expr
		for p.tok != .rcbr && p.tok != .eof {
			if p.tok == .comma {
				p.next()
			}
			fname := p.expect_name_or_keyword()
			p.check(.colon)
			val := p.expr(.lowest)
			vstart := p.add_child(val)
			field_ids << p.a.add_node(flat.Node{
				kind:           .field_init
				value:          fname
				children_start: vstart
				children_count: 1
			})
			if p.tok == .semicolon {
				p.next()
			}
		}
		p.check(.rcbr)
		start := p.add_children(field_ids)
		return p.a.add_node(flat.Node{
			kind:           .assoc
			value:          name
			children_start: start
			children_count: flat.child_count(field_ids.len)
		})
	}
	for p.tok != .rcbr && p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		// named field: name: expr
		if (p.tok == .name || p.tok.is_keyword()) && p.peek() == .colon {
			fname := p.expect_name_or_keyword()
			p.check(.colon)
			val := p.expr(.lowest)
			vstart := p.add_child(val)
			ids << p.a.add_node(flat.Node{
				kind:           .field_init
				value:          fname
				children_start: vstart
				children_count: 1
			})
		} else {
			// positional value (unnamed)
			val := p.expr(.lowest)
			vstart := p.add_child(val)
			ids << p.a.add_node(flat.Node{
				kind:           .field_init
				children_start: vstart
				children_count: 1
			})
		}
		if p.tok == .comma {
			p.next()
		}
		if p.tok == .semicolon {
			p.next()
		}
	}
	p.check(.rcbr)
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .struct_init
		value:          name
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) string_literal() flat.NodeId {
	mut q := u8(`'`)
	lit := p.lit
	if lit.len > 0 {
		if lit[0] == `r` && lit.len > 1 {
			q = p.lit[1]
		} else if lit[0] == `'` || lit[0] == `"` {
			q = lit[0]
		}
	}
	p.next()
	if int(p.tok) != 106 {
		val := strip_quotes(lit)
		return p.a.add_val_id(5, val)
	}
	// string interpolation
	val := strip_interp_start_quotes(lit)
	return p.string_interp(val, q)
}

// string_interp supports string interp handling for Parser.
fn (mut p Parser) string_interp(first_part string, quote u8) flat.NodeId {
	mut ids := []flat.NodeId{}
	if first_part.len > 0 {
		ids << p.a.add_val_id(5, first_part)
	}
	for int(p.tok) == 106 {
		p.next() // skip $
		p.check(.lcbr) // skip {
		expr_id := p.expr(.lowest)
		mut part_id := expr_id
		// format spec: :fmt
		if p.tok == .colon {
			p.next()
			mut fmt := ''
			for p.tok != .rcbr && p.tok != .eof {
				fmt += if p.lit.len > 0 { p.lit } else { p.tok.str() }
				p.next()
			}
			if fmt.len > 0 {
				start := p.add_child(expr_id)
				part_id = p.a.add_node(flat.Node{
					kind:           .directive
					value:          'string_interp_format'
					typ:            fmt
					children_start: start
					children_count: 1
				})
			}
		}
		ids << part_id
		p.check(.rcbr) // skip }
		if int(p.tok) == 107 {
			part := strip_interp_quotes(p.lit, quote)
			p.next()
			if part.len > 0 {
				ids << p.a.add_val_id(5, part)
			}
			// check for more interpolation after this string part
		}
	}
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .string_interp
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

// array_literal supports array literal handling for Parser.
fn (mut p Parser) array_literal() flat.NodeId {
	// Inside `[...]` a `{` cannot be the for-loop body, so struct inits stay
	// unambiguous even when this literal is a for-in container expression.
	was_in_for_container := p.in_for_container
	p.in_for_container = false
	defer {
		p.in_for_container = was_in_for_container
	}
	p.next() // skip '['
	// inferred-size fixed array literal: [..]u32[0x1, 0x2, ...]
	if p.tok == .dotdot && p.peek() == .rsbr {
		p.next()
		p.next()
		mut dimensions := 1
		for p.tok == .lsbr && p.peek() == .dotdot {
			p.next()
			p.next()
			p.check(.rsbr)
			dimensions++
		}
		elem_type := p.parse_type_name()
		lit, _ := p.inferred_fixed_array_literal_values(elem_type, dimensions)
		return lit
	}
	// empty array or fixed array type: []Type{} or [N]Type{}
	if p.tok == .rsbr {
		p.next()
		elem_type := p.parse_type_name()
		if p.tok == .lpar {
			p.next()
			inner := p.expr(.lowest)
			p.check(.rpar)
			cstart := p.add_child(inner)
			return p.a.add_node(flat.Node{
				kind:           .cast_expr
				value:          '[]${elem_type}'
				children_start: cstart
				children_count: 1
			})
		}
		// array init: []Type{len: n, cap: c, init: v}
		if p.tok == .lcbr {
			return p.array_init_after_element_type(elem_type)
		}
		return p.a.add_node(flat.Node{
			kind:  .array_init
			value: elem_type
			typ:   '[]${elem_type}'
		})
	}
	// array literal: [1, 2, 3]
	// or fixed array type: [3]int
	mut ids := []flat.NodeId{}
	size_start := p.tok_pos
	ids << p.expr(.lowest)
	// check if it's [N]Type (fixed array type)
	if p.tok == .rsbr {
		size_end := p.tok_pos
		p.next()
		if p.tok == .name || p.tok == .amp || p.tok == .question || p.tok == .not
			|| (p.tok == .lsbr && p.current_lbr_starts_array_type()) {
			// fixed array type: [N]Type. Use the literal node value for a plain integer
			// size, but recover the full source text for a const expression (e.g.
			// `[segs + 1]f32`) — an infix node has no `.value`, which would otherwise
			// collapse the type to a dynamic `[]f32`.
			lit_size := p.a.nodes[int(ids[0])].value
			size_str := if lit_size.len > 0 {
				lit_size
			} else if size_start >= 0 && size_end > size_start && size_end <= p.s.src.len {
				p.s.src[size_start..size_end].trim_space()
			} else {
				lit_size
			}
			elem_type := p.parse_type_name()
			fixed_type := '[${size_str}]${elem_type}'
			// may have init
			if p.tok == .lcbr {
				p.next()
				mut init_ids := []flat.NodeId{}
				for p.tok != .rcbr && p.tok != .eof {
					if p.tok == .semicolon {
						p.next()
						continue
					}
					if p.tok == .name && p.peek() == .colon {
						fname := p.expect_name()
						p.check(.colon)
						val := p.expr(.lowest)
						vstart := p.add_child(val)
						init_ids << p.a.add_node(flat.Node{
							kind:           .field_init
							value:          fname
							children_start: vstart
							children_count: 1
						})
					} else {
						init_ids << p.expr(.lowest)
					}
					if p.tok == .comma {
						p.next()
					}
				}
				p.check(.rcbr)
				start := p.add_children(init_ids)
				return p.a.add_node(flat.Node{
					kind:           .array_init
					value:          fixed_type
					typ:            fixed_type
					children_start: start
					children_count: flat.child_count(init_ids.len)
				})
			}
			return p.a.add_node(flat.Node{
				kind:  .array_init
				value: fixed_type
				typ:   fixed_type
			})
		}
		// single-element array: [expr]
		if p.tok == .not {
			p.next()
			start := p.add_child(ids[0])
			lit := p.a.add_node(flat.Node{
				kind:           .array_literal
				children_start: start
				children_count: 1
			})
			pstart := p.add_child(lit)
			return p.a.add_node(flat.Node{
				kind:           .postfix
				op:             .not
				children_start: pstart
				children_count: 1
			})
		}
		start := p.add_children(ids)
		return p.a.add_node(flat.Node{
			kind:           .array_literal
			children_start: start
			children_count: flat.child_count(ids.len)
		})
	}
	// multi-element array: `[a, b, c]` or newline-separated const tables.
	// Each subsequent element must be preceded by a separator: a single comma,
	// or a run of `;` that the scanner emits for newlines/blank lines. A missing
	// separator (`[1 2]`) or a repeated comma (`[1,,2]`) ends the element list
	// instead of merging operands; the stray tokens are then left to the
	// (permissive) `p.check(.rsbr)` below, matching how this parser recovers
	// from other malformed input.
	for p.tok == .comma || p.tok == .semicolon {
		if p.tok == .comma {
			p.next()
		}
		// newlines/blank lines after a separator are just whitespace
		for p.tok == .semicolon {
			p.next()
		}
		// a second comma with no element in between is not a separator
		if p.tok == .rsbr || p.tok == .eof || p.tok == .comma {
			break
		}
		ids << p.expr(.lowest)
	}
	p.check(.rsbr)
	// check for `!` (fixed array with values)
	mut is_fixed_literal := false
	if p.tok == .not {
		p.next()
		is_fixed_literal = true
	}
	start := p.add_children(ids)
	lit := p.a.add_node(flat.Node{
		kind:           .array_literal
		children_start: start
		children_count: flat.child_count(ids.len)
	})
	if is_fixed_literal {
		pstart := p.add_child(lit)
		return p.a.add_node(flat.Node{
			kind:           .postfix
			op:             .not
			children_start: pstart
			children_count: 1
		})
	}
	return lit
}

fn (mut p Parser) array_init_after_element_type(elem_type string) flat.NodeId {
	p.check(.lcbr)
	mut ids := []flat.NodeId{}
	for p.tok != .rcbr && p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		if p.tok == .name && p.peek() == .colon {
			fname := p.expect_name()
			p.check(.colon)
			val := p.expr(.lowest)
			ids << p.a.add_node(flat.Node{
				kind:           .field_init
				value:          fname
				children_start: p.add_child(val)
				children_count: 1
			})
		} else {
			ids << p.expr(.lowest)
		}
		if p.tok == .comma {
			p.next()
		}
	}
	p.check(.rcbr)
	return p.a.add_node(flat.Node{
		kind:           .array_init
		value:          elem_type
		typ:            '[]${elem_type}'
		children_start: p.add_children(ids)
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) inferred_fixed_array_literal_values(base_elem_type string, dimensions int) (flat.NodeId, string) {
	p.check(.lsbr)
	mut vals := []flat.NodeId{}
	mut elem_type := base_elem_type
	mut has_ragged_rows := false
	for p.tok != .rsbr && p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		if dimensions > 1 {
			val, nested_type := p.inferred_fixed_array_literal_values(base_elem_type,
				dimensions - 1)
			vals << val
			if vals.len == 1 {
				elem_type = nested_type
			} else if nested_type != elem_type {
				has_ragged_rows = true
			}
		} else {
			vals << p.expr(.lowest)
		}
		if p.tok == .comma {
			p.next()
		}
	}
	p.check(.rsbr)
	fixed_type := '[${vals.len}]${elem_type}'
	start := p.add_children(vals)
	lit := p.a.add_node(flat.Node{
		kind:           .array_literal
		typ:            fixed_type
		children_start: start
		children_count: flat.child_count(vals.len)
	})
	pstart := p.add_child(lit)
	return p.a.add_node(flat.Node{
		kind:           .postfix
		op:             .not
		value:          if has_ragged_rows { 'ragged_inferred_fixed_array' } else { '' }
		typ:            fixed_type
		children_start: pstart
		children_count: 1
	}), fixed_type
}

// fn_literal supports fn literal handling for Parser.
fn (mut p Parser) fn_literal() flat.NodeId {
	fn_start := p.tok_pos
	p.next() // skip 'fn'
	// capture list: fn [a, b] (params) ret { }
	mut capture_ids := []flat.NodeId{}
	if p.tok == .lsbr {
		p.next()
		for p.tok != .rsbr && p.tok != .eof {
			mut is_mut_capture := false
			if p.tok == .key_mut {
				is_mut_capture = true
				p.next()
			}
			capture_id := p.a.add_val(.ident, p.expect_name())
			if is_mut_capture {
				p.a.set_node_is_mut(capture_id, true)
			}
			capture_ids << capture_id
			if p.tok == .comma {
				p.next()
			}
		}
		p.check(.rsbr)
	}
	// params
	p.check(.lpar)
	mut param_ids := []flat.NodeId{}
	for p.tok != .rpar && p.tok != .eof {
		start_offset := p.s.offset
		param_ids << p.parse_param_group(false)
		if p.s.offset == start_offset && p.tok != .rpar && p.tok != .eof {
			p.next()
		}
	}
	p.check(.rpar)
	// return type
	mut ret_type := 'void'
	if p.tok != .lcbr && p.tok != .semicolon && p.tok != .eof {
		if p.tok == .name || p.tok == .amp || p.tok == .question || p.tok == .not || p.tok == .lsbr
			|| p.tok == .lpar || p.tok == .key_fn {
			ret_type = p.parse_type_name()
		}
	}
	// body
	mut body_ids := []flat.NodeId{}
	if p.tok == .lcbr {
		body_start := p.tok_pos
		p.push_local_type_scope(p.fn_literal_local_type_scope(fn_start))
		p.begin_comptime_value_scope()
		p.check(.lcbr)
		p.predeclare_local_type_names_in_block(body_start)
		for p.tok != .rcbr && p.tok != .eof {
			id := p.stmt()
			if int(id) >= 0 {
				body_ids << id
			}
		}
		p.check(.rcbr)
		p.end_comptime_value_scope()
		p.pop_local_type_scope()
	}
	mut all_ids := []flat.NodeId{cap: capture_ids.len + param_ids.len + body_ids.len}
	for id in capture_ids {
		all_ids << id
	}
	for id in param_ids {
		all_ids << id
	}
	for id in body_ids {
		all_ids << id
	}
	start := p.add_children(all_ids)
	return p.a.add_node(flat.Node{
		kind:           .fn_literal
		typ:            ret_type
		children_start: start
		children_count: flat.child_count(all_ids.len)
	})
}

// lock_expr supports lock expr handling for Parser.
fn (mut p Parser) lock_expr() flat.NodeId {
	is_rlock := p.tok == .key_rlock
	p.next() // skip 'lock' or 'rlock'
	mut obj_ids := []flat.NodeId{}
	// lock objects
	if p.tok != .lcbr {
		obj_ids << p.expr(.lowest)
		for p.tok == .comma {
			p.next()
			obj_ids << p.expr(.lowest)
		}
	}
	body := p.block_stmt()
	mut ids := obj_ids.clone()
	ids << body
	lstart := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .lock_expr
		value:          if is_rlock { 'rlock' } else { 'lock' }
		children_start: lstart
		children_count: flat.child_count(ids.len)
	})
}

// select_expr resolves select expr information for parser.
fn (mut p Parser) select_expr() flat.NodeId {
	p.next() // skip 'select'
	p.check(.lcbr)
	mut ids := []flat.NodeId{}
	for p.tok != .rcbr && p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		ids << p.select_branch()
	}
	p.check(.rcbr)
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .select_stmt
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

// select_branch resolves select branch information for parser.
fn (mut p Parser) select_branch() flat.NodeId {
	mut is_else := false
	mut is_recv_decl := false
	mut is_recv_assign := false
	mut recv_compound_op := ''
	mut cond_ids := []flat.NodeId{}
	if p.tok == .key_else {
		is_else = true
		p.next()
	} else {
		cond_ids << p.expr(.lowest)
		// could be assignment: ch <- val, val := <-ch, or val = <-ch
		if token_is_assignment(p.tok) || p.tok == .decl_assign {
			op := p.tok
			// Preserve receive declaration/assignment shape even though the parser
			// stores the lvalue and receive expression as separate branch children.
			if op == .decl_assign {
				is_recv_decl = true
			} else if op == .assign {
				is_recv_assign = true
			} else {
				recv_compound_op = op.str()
			}
			p.next()
			cond_ids << p.expr(.lowest)
		}
	}
	mut body_ids := []flat.NodeId{}
	if p.tok == .lcbr {
		body_ids = p.parse_block_body()
	}
	mut all_ids := cond_ids.clone()
	for id in body_ids {
		all_ids << id
	}
	start := p.add_children(all_ids)
	branch_value := if is_else {
		'else'
	} else if is_recv_decl {
		'recv'
	} else if is_recv_assign {
		'recv_assign'
	} else if recv_compound_op.len > 0 {
		'recv_compound:${recv_compound_op}'
	} else {
		''
	}
	return p.a.add_node(flat.Node{
		kind:           .select_branch
		value:          branch_value
		children_start: start
		children_count: flat.child_count(all_ids.len)
	})
}

// sizeof_expr supports sizeof expr handling for Parser.
fn (mut p Parser) sizeof_expr() flat.NodeId {
	p.next() // skip 'sizeof'
	p.check(.lpar)
	type_name := p.parse_type_name()
	p.check(.rpar)
	return p.a.add_val(.sizeof_expr, type_name)
}

// typeof_expr supports typeof expr handling for Parser.
fn (mut p Parser) typeof_expr() flat.NodeId {
	p.next() // skip 'typeof'
	if p.tok == .lsbr {
		p.next()
		type_name := p.parse_type_name()
		p.check(.rsbr)
		if p.tok == .lpar {
			p.next()
			p.check(.rpar)
		}
		return p.a.add_node(flat.Node{
			kind:  .typeof_expr
			value: type_name
		})
	}
	p.check(.lpar)
	inner := p.expr(.lowest)
	p.check(.rpar)
	tstart := p.add_child(inner)
	return p.a.add_node(flat.Node{
		kind:           .typeof_expr
		children_start: tstart
		children_count: 1
	})
}

// dump_expr updates dump expr state for Parser.
fn (mut p Parser) dump_expr() flat.NodeId {
	p.next() // skip 'dump'
	p.check(.lpar)
	inner := p.expr(.lowest)
	p.check(.rpar)
	dstart := p.add_child(inner)
	return p.a.add_node(flat.Node{
		kind:           .dump_expr
		children_start: dstart
		children_count: 1
	})
}

// offsetof_expr supports offsetof expr handling for Parser.
fn (mut p Parser) offsetof_expr() flat.NodeId {
	p.next() // skip '__offsetof'
	p.check(.lpar)
	type_name := p.parse_type_name()
	p.check(.comma)
	field_name := p.expect_name()
	p.check(.rpar)
	return p.a.add_node(flat.Node{
		kind:  .offsetof_expr
		value: type_name
		typ:   field_name
	})
}

// ==================== types ====================

// parse_type_name_progress reads parse type name progress input for parser.
fn (mut p Parser) parse_type_name_progress() string {
	start_offset := p.s.offset
	typ := p.parse_type_name()
	if p.s.offset == start_offset && p.tok != .eof {
		p.next()
	}
	return typ
}

// peek_lbr_starts_array_type supports peek lbr starts array type handling for Parser.
fn (mut p Parser) peek_lbr_starts_array_type() bool {
	if p.peek() != .lsbr {
		return false
	}
	return p.lbr_starts_array_type_from_offset(p.s.offset)
}

// current_lbr_starts_array_type reports whether the current `[` starts a nested
// array type, not an index expression.
fn (mut p Parser) current_lbr_starts_array_type() bool {
	if p.tok != .lsbr {
		return false
	}
	return p.lbr_starts_array_type_from_offset(p.s.offset)
}

fn (mut p Parser) current_generic_struct_init_suffix_followed_by_lcbr() bool {
	args := p.current_generic_suffix_args_followed_by_lcbr() or { return false }
	if args.len == 0 {
		return false
	}
	for arg in args {
		if !generic_struct_init_suffix_arg_can_be_type(arg) {
			return false
		}
	}
	return true
}

fn (mut p Parser) current_generic_suffix_args_followed_by_lcbr() ?[]string {
	if p.tok != .lsbr {
		return none
	}
	mut idx := p.s.offset
	mut arg_start := idx
	mut bracket_depth := 1
	mut paren_depth := 0
	mut args := []string{}
	for idx < p.s.src.len {
		c := p.s.src[idx]
		if c == `[` {
			bracket_depth++
		} else if c == `]` {
			bracket_depth--
			if bracket_depth == 0 {
				args << p.s.src[arg_start..idx].trim_space()
				idx++
				for idx < p.s.src.len && (p.s.src[idx] == ` ` || p.s.src[idx] == `\t`
					|| p.s.src[idx] == `\n` || p.s.src[idx] == `\r`) {
					idx++
				}
				if idx < p.s.src.len && p.s.src[idx] == `{` {
					return args
				}
				return none
			}
		} else if c == `(` {
			paren_depth++
		} else if c == `)` {
			if paren_depth > 0 {
				paren_depth--
			}
		} else if c == `,` && bracket_depth == 1 && paren_depth == 0 {
			args << p.s.src[arg_start..idx].trim_space()
			arg_start = idx + 1
		}
		idx++
	}
	return none
}

fn generic_struct_init_suffix_arg_can_be_type(arg string) bool {
	clean := arg.trim_space()
	if clean.len == 0 {
		return false
	}
	first := clean[0]
	if (first >= `A` && first <= `Z`) || first == `&` || first == `?` || first == `!`
		|| first == `[` || first == `(` || first == `^` {
		return true
	}
	if clean == 'fn' || clean.starts_with('fn ') || clean.starts_with('fn(') {
		return true
	}
	first_ident := generic_suffix_first_ident(clean)
	if first_ident in ['map', 'chan', 'thread', 'shared', 'atomic', 'mut', 'struct', 'none', 'nil'] {
		return true
	}
	if is_builtin_type(first_ident) {
		return true
	}
	dot_idx := clean.last_index_u8(`.`)
	if dot_idx >= 0 {
		after_dot := clean[dot_idx + 1..]
		return after_dot.len > 0 && after_dot[0] >= `A` && after_dot[0] <= `Z`
	}
	return false
}

fn generic_suffix_first_ident(s string) string {
	mut idx := 0
	for idx < s.len {
		c := s[idx]
		if !((c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_`) {
			break
		}
		idx++
	}
	return s[..idx]
}

fn (mut p Parser) lbr_starts_array_type_from_offset(offset int) bool {
	mut idx := offset
	for idx < p.s.src.len && (p.s.src[idx] == ` ` || p.s.src[idx] == `\t`
		|| p.s.src[idx] == `\n` || p.s.src[idx] == `\r`) {
		idx++
	}
	mut depth := 1
	for idx < p.s.src.len {
		c := p.s.src[idx]
		if c == `[` {
			depth++
		} else if c == `]` {
			depth--
			if depth == 0 {
				idx++
				for idx < p.s.src.len && (p.s.src[idx] == ` ` || p.s.src[idx] == `\t`) {
					idx++
				}
				if idx >= p.s.src.len {
					return false
				}
				if p.s.src[idx] == `\n` || p.s.src[idx] == `\r` || p.s.src[idx] == `;` {
					return false
				}
				next := p.s.src[idx]
				return (next >= `a` && next <= `z`) || (next >= `A` && next <= `Z`)
					|| next == `_` || next == `&` || next == `?` || next == `!`
					|| next == `[` || next == `(`
			}
		}
		idx++
	}
	return false
}

// can_start_type_name reports whether can start type name applies in parser.
fn (p &Parser) can_start_type_name() bool {
	return p.tok == .name || p.tok == .amp || p.tok == .question || p.tok == .not || p.tok == .lsbr
		|| p.tok == .lpar || p.tok == .key_fn || p.tok == .ellipsis || p.tok == .key_mut
		|| p.tok == .key_shared || p.tok == .key_atomic
}

// fn_type_param_with_mut supports fn type param with mut handling for parser.
fn fn_type_param_with_mut(typ string, is_mut bool) string {
	if !is_mut || typ.len == 0 || typ.starts_with('&') {
		return typ
	}
	return '&' + typ
}

// parse_fn_type_param reads parse fn type param input for parser.
fn (mut p Parser) parse_fn_type_param() string {
	mut is_mut := false
	if p.tok == .key_mut {
		is_mut = true
		p.next()
	}
	first := p.parse_type_name_progress()
	if first.len == 0 {
		return first
	}
	if p.tok != .comma && p.tok != .rpar && p.tok != .eof && p.can_start_type_name() {
		second := p.parse_type_name_progress()
		if second.len > 0 {
			return '${first} ${fn_type_param_with_mut(second, is_mut)}'
		}
	}
	return fn_type_param_with_mut(first, is_mut)
}

fn (mut p Parser) consume_lifetime_type_param() bool {
	if p.tok != .xor {
		return false
	}
	p.next()
	if p.tok == .name {
		p.next()
	}
	return true
}

fn (mut p Parser) parse_type_generic_suffix() string {
	if p.tok != .lsbr {
		return ''
	}
	pk := p.peek()
	if pk != .name && pk != .amp && pk != .lsbr && pk != .question && pk != .key_fn && pk != .xor {
		return ''
	}
	p.next() // skip [
	mut params := []string{}
	for p.tok != .rsbr && p.tok != .eof {
		if !p.consume_lifetime_type_param() {
			param := p.parse_type_name()
			if param.len > 0 {
				params << param
			}
		}
		if p.tok == .comma {
			p.next()
			continue
		}
		break
	}
	p.check(.rsbr)
	if params.len == 0 {
		return ''
	}
	return '[' + params.join(', ') + ']'
}

// parse_type_name reads parse type name input for parser.
fn (mut p Parser) parse_type_name() string {
	// Lifetime annotation `^a` (as in `&^a T`, `?&^a T`, `Type[^a]`). v3 erases lifetimes -
	// they have no runtime representation, so consume the caret and the lifetime name and
	// continue with the underlying type. Without this, `&^a T` parses as just `&` and the
	// stray `^` desyncs the enclosing declaration (e.g. a `&^a Recv` method receiver made the
	// method name get consumed as the return type).
	if p.consume_lifetime_type_param() {
		return p.parse_type_name()
	}
	if p.tok == .name && p.lit == '&' {
		p.next()
		return '&' + p.parse_type_name()
	}
	if p.tok == .key_mut {
		p.next()
		return fn_type_param_with_mut(p.parse_type_name(), true)
	}
	// option ?T
	if p.tok == .question {
		p.next()
		inner := p.parse_type_name()
		if inner.len > 0 {
			return '?' + inner
		}
		return '?'
	}
	// result !T
	if p.tok == .not {
		p.next()
		inner := p.parse_type_name()
		if inner.len > 0 {
			return '!' + inner
		}
		return '!'
	}
	// pointer &T
	if p.tok == .amp {
		p.next()
		return '&' + p.parse_type_name()
	}
	if p.tok == .and {
		p.next()
		return '&&' + p.parse_type_name()
	}
	// variadic ...T
	if p.tok == .ellipsis {
		p.next()
		return '...' + p.parse_type_name()
	}
	// array []T or fixed [N]T
	if p.tok == .lsbr {
		p.next()
		if p.tok == .rsbr {
			p.next()
			return '[]' + p.parse_type_name()
		}
		// fixed array [N]T — the size may be a const expression (`[segs + 1]f32`),
		// not just a single token, so parse the whole expression and recover its
		// source text when it is not a plain literal (mirrors the array-literal path).
		size_start := p.tok_pos
		size_node := p.expr(.lowest)
		size_end := p.tok_pos
		p.check(.rsbr)
		lit_size := p.a.nodes[int(size_node)].value
		size_str := if lit_size.len > 0 {
			lit_size
		} else if size_start >= 0 && size_end > size_start && size_end <= p.s.src.len {
			p.s.src[size_start..size_end].trim_space()
		} else {
			lit_size
		}
		return '[${size_str}]' + p.parse_type_name()
	}
	// multi-return (T, U)
	if p.tok == .lpar {
		p.next()
		mut type_list := []string{}
		type_list << p.parse_type_name()
		for p.tok == .comma {
			p.next()
			type_list << p.parse_type_name()
		}
		p.check(.rpar)
		return '(' + type_list.join(', ') + ')'
	}
	// function type fn(T) U
	if p.tok == .key_fn {
		p.next()
		if p.tok != .lpar {
			return 'fn'
		}
		p.next()
		mut ptypes := []string{}
		for p.tok != .rpar && p.tok != .eof {
			typ := p.parse_fn_type_param()
			if typ.len > 0 {
				ptypes << typ
			}
			if p.tok == .comma {
				p.next()
			}
		}
		p.check(.rpar)
		mut ret := ''
		if p.can_start_type_name() {
			ret = p.parse_type_name()
		}
		if ret.len > 0 {
			return 'fn(${ptypes.join(', ')}) ${ret}'
		}
		return 'fn(${ptypes.join(', ')})'
	}
	// atomic / shared modifier
	if p.tok == .key_atomic || p.tok == .key_shared {
		modifier := p.lit
		p.next()
		return '${modifier} ' + p.parse_type_name()
	}
	// nil type
	if p.tok == .key_nil {
		p.next()
		return 'nil'
	}
	// none type
	if p.tok == .key_none {
		p.next()
		return 'none'
	}
	// struct type (inline/anonymous)
	if p.tok == .key_struct {
		return p.parse_anonymous_struct_type()
	}
	// name
	mut name := ''
	if p.tok == .name {
		name = p.lit
		p.next()
		// map[K]V
		if name == 'map' && p.tok == .lsbr {
			p.next()
			key := p.parse_type_name()
			p.check(.rsbr)
			val := p.parse_type_name()
			return 'map[${key}]${val}'
		}
		// chan T
		if name == 'chan' {
			if p.tok == .name || p.tok == .amp || p.tok == .lsbr || p.tok == .question
				|| p.tok == .not {
				elem := p.parse_type_name()
				return 'chan ${elem}'
			}
			return 'chan'
		}
		// thread T
		if name == 'thread' {
			if p.tok == .name || p.tok == .amp || p.tok == .lsbr || p.tok == .question
				|| p.tok == .not {
				elem := p.parse_type_name()
				return 'thread ${elem}'
			}
			return 'thread'
		}
		// qualified: mod.Type
		if p.tok == .dot {
			p.next()
			if p.tok == .name {
				if p.lit != 'typ' {
					name += '.' + p.lit
				}
				p.next()
			}
		}
		// generic: Type[T, U]
		name += p.parse_type_generic_suffix()
		name = p.resolve_local_type_name(name)
	}
	return name
}

fn anonymous_struct_shape(fields []string) string {
	return fields.join(',')
}

fn (p &Parser) anonymous_struct_type_for_literal(init flat.Node) ?string {
	mut field_names := []string{cap: int(init.children_count)}
	for i in 0 .. init.children_count {
		field := p.a.child_node(&init, i)
		if field.value.len == 0 {
			return none
		}
		field_names << field.value
	}
	if candidates := p.anonymous_struct_types[anonymous_struct_shape(field_names)] {
		if candidates.len == 1 {
			return candidates[0]
		}
	}
	return none
}

fn (mut p Parser) parse_anonymous_struct_type() string {
	p.next() // skip `struct`
	if p.tok != .lcbr {
		return 'struct'
	}
	p.next()
	mut ids := []flat.NodeId{}
	mut field_names := []string{}
	for p.tok != .rcbr && p.tok != .eof {
		if p.tok == .semicolon || p.tok == .comma {
			p.next()
			continue
		}
		if p.tok != .name && !p.tok.is_keyword() {
			p.next()
			continue
		}
		field_name := p.expect_name_or_keyword()
		// Grouped fields use one trailing type for every preceding name:
		// `struct { x, y int }`.
		if p.tok == .comma {
			mut names := []string{cap: 2}
			names << field_name
			for p.tok == .comma {
				p.next()
				names << p.expect_name_or_keyword()
			}
			field_type := p.parse_type_name()
			mut attrs := []string{}
			if p.tok == .attribute || p.tok == .lsbr {
				attrs << p.parse_field_attrs()
			}
			for name in names {
				fid := p.a.add_node(flat.Node{
					kind:  .field_decl
					value: name
					typ:   field_type
				})
				p.apply_field_meta(fid, false, false, attrs)
				ids << fid
				field_names << name
			}
			if p.tok == .semicolon || p.tok == .comma {
				p.next()
			}
			continue
		}
		field_type := p.parse_type_name()
		mut default_id := flat.empty_node
		if p.tok == .assign {
			p.next()
			default_id = p.expr(.lowest)
		}
		mut children_start := 0
		mut children_count := 0
		if int(default_id) >= 0 {
			children_start = p.add_child(default_id)
			children_count = 1
		}
		fid := p.a.add_node(flat.Node{
			kind:           .field_decl
			value:          field_name
			typ:            field_type
			children_start: children_start
			children_count: flat.child_count(children_count)
		})
		if p.tok == .attribute || p.tok == .lsbr {
			p.apply_field_meta(fid, false, false, p.parse_field_attrs())
		}
		ids << fid
		field_names << field_name
		if p.tok == .semicolon || p.tok == .comma {
			p.next()
		}
	}
	p.check(.rcbr)
	p.anonymous_struct_count++
	name := 'AnonStruct_${local_type_scope_part(p.cur_file)}_${p.anonymous_struct_count}'
	start := p.add_children(ids)
	p.a.add_node(flat.Node{
		kind:           .struct_decl
		value:          name
		children_start: start
		children_count: flat.child_count(ids.len)
	})
	shape := anonymous_struct_shape(field_names)
	mut candidates := p.anonymous_struct_types[shape] or { []string{} }
	candidates << name
	p.anonymous_struct_types[shape] = candidates
	return name
}

fn (p &Parser) current_local_type_scope() string {
	if p.local_type_scopes.len == 0 {
		return ''
	}
	return p.local_type_scopes[p.local_type_scopes.len - 1]
}

fn (mut p Parser) push_local_type_scope(scope string) {
	if scope.len > 0 {
		p.local_type_scopes << scope
	}
}

fn (mut p Parser) pop_local_type_scope() {
	if p.local_type_scopes.len > 0 {
		p.local_type_scopes.delete_last()
	}
}

fn (p &Parser) fn_literal_local_type_scope(fn_start int) string {
	mut base := p.current_local_type_scope()
	if base.len == 0 {
		base = p.cur_fn
	}
	if base.len == 0 {
		base = 'fn_literal'
	}
	return '${base}__fn_literal_${fn_start}'
}

fn (p &Parser) block_local_type_scope(block_start int) string {
	base := p.current_local_type_scope()
	if base.len == 0 {
		return ''
	}
	return '${base}__block_${block_start}'
}

fn (p &Parser) local_type_key_for_scope(scope string, name string) string {
	if scope.len == 0 || name.len == 0 {
		return ''
	}
	return '${p.cur_file}\n${p.cur_module}\n${scope}\n${name}'
}

fn (mut p Parser) declare_local_type_name(name string, scope string) string {
	if scope.len == 0 || name.len == 0 || name.contains('.') {
		return name
	}
	key := p.local_type_key_for_scope(scope, name)
	if mapped := p.local_type_names[key] {
		return mapped
	}
	local_name := local_type_decl_name_for_scope(name, scope)
	p.local_type_names[key] = local_name
	return local_name
}

fn local_type_decl_name_for_scope(name string, scope string) string {
	return '${name}@local@${local_type_scope_part(scope)}'
}

fn (mut p Parser) predeclare_local_type_names_in_block(open_brace_pos int) {
	scope := p.current_local_type_scope()
	if scope.len == 0 || open_brace_pos < 0 || open_brace_pos >= p.s.src.len {
		return
	}
	mut s := scanner.new_scanner(p.prefs, .normal)
	s.init(p.s.current_file(), p.s.src)
	s.offset = open_brace_pos + 1
	s.pos = s.offset
	mut depth := 0
	for {
		tok := s.scan()
		if tok == .eof {
			return
		}
		if tok == .lcbr {
			depth++
			continue
		}
		if tok == .rcbr {
			if depth == 0 {
				return
			}
			depth--
			continue
		}
		if depth != 0 || (tok != .key_struct && tok != .key_union) {
			continue
		}
		name_tok := s.scan()
		if name_tok == .lcbr {
			depth++
			continue
		}
		if name_tok != .name {
			continue
		}
		name := s.lit
		after_name_tok := s.scan()
		if after_name_tok != .dot {
			p.declare_local_type_name(name, scope)
		}
		if after_name_tok == .lcbr {
			depth++
			continue
		}
	}
}

fn (p &Parser) resolve_local_type_name(name string) string {
	if p.local_type_scopes.len == 0 || name.len == 0 {
		return name
	}
	mut suffix := ''
	mut base := name
	idx := name.index_u8(`[`)
	if idx >= 0 {
		base = name[..idx]
		suffix = name[idx..]
	}
	if base.contains('.') {
		return name
	}
	for i := p.local_type_scopes.len - 1; i >= 0; i-- {
		scope := p.local_type_scopes[i]
		if mapped := p.local_type_names[p.local_type_key_for_scope(scope, base)] {
			return mapped + suffix
		}
	}
	return name
}

fn local_type_scope_part(name string) string {
	mut out := strings.new_builder(name.len)
	for ch in name {
		if (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || (ch >= `0` && ch <= `9`) {
			out.write_rune(ch)
		} else if ch == `_` {
			out.write_string('_u')
		} else {
			out.write_string('_x')
			out.write_string(int(ch).hex())
			out.write_u8(`_`)
		}
	}
	return out.str()
}

// ==================== helpers ====================

// strip_quotes supports strip quotes handling for parser.
fn strip_quotes(s string) string {
	mut raw := s
	is_raw := s.len >= 3 && s[0] == `r`
	if is_raw {
		raw = s[1..]
	}
	if raw.len >= 2 && ((raw[0] == `'` && raw[raw.len - 1] == `'`)
		|| (raw[0] == `"` && raw[raw.len - 1] == `"`)) {
		raw = raw[1..raw.len - 1]
	} else if raw.len >= 1 && (raw[0] == `'` || raw[0] == `"`) {
		raw = raw[1..]
	}
	if is_raw {
		return raw
	}
	return unescape_string(raw)
}

fn strip_interp_start_quotes(s string) string {
	mut raw := s
	is_raw := s.len >= 3 && s[0] == `r`
	if is_raw {
		raw = s[1..]
	}
	if raw.len >= 1 && (raw[0] == `'` || raw[0] == `"`) {
		raw = raw[1..]
	}
	if is_raw {
		return raw
	}
	return unescape_string(raw)
}

fn strip_interp_quotes(s string, quote u8) string {
	mut raw := s
	if raw.len >= 1 && raw[raw.len - 1] == quote {
		raw = raw[..raw.len - 1]
	}
	return unescape_string(raw)
}

fn unescape_string(s string) string {
	if !s.contains('\\') {
		return s
	}
	mut buf := unsafe { malloc(s.len + 1) }
	mut j := 0
	mut i := 0
	for i < s.len {
		if s[i] == `\\` && i + 1 < s.len {
			if s[i + 1] == `\n` {
				i += 2
				for i < s.len && (s[i] == ` ` || s[i] == `\t` || s[i] == `\r`) {
					i++
				}
				continue
			}
			if s[i + 1] == `\r` && i + 2 < s.len && s[i + 2] == `\n` {
				i += 3
				for i < s.len && (s[i] == ` ` || s[i] == `\t`) {
					i++
				}
				continue
			}
			if s[i + 1] == `x` && i + 3 < s.len {
				if code := parse_fixed_hex(s, i + 2, 2) {
					unsafe {
						buf[j] = u8(code)
					}
					j++
					i += 4
					continue
				}
			}
			if s[i + 1] == `u` && i + 5 < s.len {
				if code := parse_fixed_hex(s, i + 2, 4) {
					j = write_utf8_codepoint(buf, j, u32(code))
					i += 6
					continue
				}
			}
			if s[i + 1] == `U` && i + 9 < s.len {
				if code := parse_fixed_hex(s, i + 2, 8) {
					j = write_utf8_codepoint(buf, j, u32(code))
					i += 10
					continue
				}
			}
			c := match s[i + 1] {
				`n` { u8(`\n`) }
				`t` { u8(`\t`) }
				`r` { u8(`\r`) }
				`\\` { u8(`\\`) }
				`'` { u8(`'`) }
				`"` { u8(`"`) }
				`$` { u8(`$`) }
				`0` { u8(0) }
				`a` { u8(7) }
				`b` { u8(8) }
				`f` { u8(12) }
				`v` { u8(11) }
				else { u8(0xff) }
			}

			if c != 0xff {
				unsafe {
					buf[j] = c
				}
				j++
			} else {
				unsafe {
					buf[j] = s[i]
					buf[j + 1] = s[i + 1]
				}
				j += 2
			}
			i += 2
		} else {
			unsafe {
				buf[j] = s[i]
			}
			j++
			i++
		}
	}
	unsafe {
		buf[j] = 0
		return tos(buf, j)
	}
}

fn parse_fixed_hex(s string, start int, len int) ?u32 {
	mut code := u32(0)
	for i in 0 .. len {
		if start + i >= s.len {
			return none
		}
		n := hex_digit_value(s[start + i]) or { return none }
		code = (code << 4) | u32(n)
	}
	return code
}

fn hex_digit_value(c u8) ?int {
	if c >= `0` && c <= `9` {
		return int(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return int(c - `a`) + 10
	}
	if c >= `A` && c <= `F` {
		return int(c - `A`) + 10
	}
	return none
}

fn write_utf8_codepoint(buf &u8, j int, code u32) int {
	unsafe {
		if code <= 0x7f {
			buf[j] = u8(code)
			return j + 1
		}
		if code <= 0x7ff {
			buf[j] = u8(0xc0 | (code >> 6))
			buf[j + 1] = u8(0x80 | (code & 0x3f))
			return j + 2
		}
		if code <= 0xffff {
			buf[j] = u8(0xe0 | (code >> 12))
			buf[j + 1] = u8(0x80 | ((code >> 6) & 0x3f))
			buf[j + 2] = u8(0x80 | (code & 0x3f))
			return j + 3
		}
		buf[j] = u8(0xf0 | (code >> 18))
		buf[j + 1] = u8(0x80 | ((code >> 12) & 0x3f))
		buf[j + 2] = u8(0x80 | ((code >> 6) & 0x3f))
		buf[j + 3] = u8(0x80 | (code & 0x3f))
		return j + 4
	}
}

fn is_builtin_type(name string) bool {
	return name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
		'byte', 'bool', 'string', 'rune', 'char', 'voidptr', 'charptr', 'byteptr', 'usize', 'isize',
		'array', 'map', 'mapnode', '_result', '_option', 'any']
}

fn is_c_anon_simple_type_name(name string) bool {
	if name.len == 0 {
		return false
	}
	if name.ends_with('_t') {
		return true
	}
	return name[0] >= `A` && name[0] <= `Z`
}

fn is_all_upper_ident(s string) bool {
	if s.len == 0 {
		return false
	}
	mut has_letter := false
	for ch in s {
		if ch >= `a` && ch <= `z` {
			return false
		}
		if ch >= `A` && ch <= `Z` {
			has_letter = true
		}
	}
	return has_letter
}

fn token_to_op(tok token.Token) flat.Op {
	tv := int(tok)
	if tv == 95 {
		return flat.Op.plus
	}
	if tv == 81 {
		return flat.Op.minus
	}
	if tv == 85 {
		return flat.Op.mul
	}
	if tv == 13 {
		return flat.Op.div
	}
	if tv == 83 {
		return flat.Op.mod
	}
	if tv == 20 {
		return flat.Op.eq
	}
	if tv == 88 {
		return flat.Op.ne
	}
	if tv == 80 {
		return flat.Op.lt
	}
	if tv == 22 {
		return flat.Op.gt
	}
	if tv == 74 {
		return flat.Op.le
	}
	if tv == 21 {
		return flat.Op.ge
	}
	if tv == 0 {
		return flat.Op.amp
	}
	if tv == 94 {
		return flat.Op.pipe
	}
	if tv == 109 {
		return flat.Op.xor
	}
	if tv == 75 {
		return flat.Op.left_shift
	}
	if tv == 99 {
		return flat.Op.right_shift
	}
	if tv == 101 {
		return flat.Op.right_shift_unsigned
	}
	if tv == 1 {
		return flat.Op.logical_and
	}
	if tv == 77 {
		return flat.Op.logical_or
	}
	if tv == 89 {
		return flat.Op.not
	}
	if tv == 6 {
		return flat.Op.bit_not
	}
	if tv == 4 {
		return flat.Op.assign
	}
	if tv == 96 {
		return flat.Op.plus_assign
	}
	if tv == 82 {
		return flat.Op.minus_assign
	}
	if tv == 86 {
		return flat.Op.mul_assign
	}
	if tv == 14 {
		return flat.Op.div_assign
	}
	if tv == 84 {
		return flat.Op.mod_assign
	}
	if tv == 2 {
		return flat.Op.amp_assign
	}
	if tv == 93 {
		return flat.Op.pipe_assign
	}
	if tv == 110 {
		return flat.Op.xor_assign
	}
	if tv == 76 {
		return flat.Op.left_shift_assign
	}
	if tv == 100 {
		return flat.Op.right_shift_assign
	}
	if tv == 102 {
		return flat.Op.right_shift_unsigned_assign
	}
	if tv == 24 {
		return flat.Op.inc
	}
	if tv == 11 {
		return flat.Op.dec
	}
	if tv == 3 {
		return flat.Op.arrow
	}
	if tv == 12 {
		return flat.Op.assign
	}
	return flat.Op.none
}

fn overload_token_name(tok token.Token) string {
	tv := int(tok)
	if tv == 95 {
		return '+'
	}
	if tv == 81 {
		return '-'
	}
	if tv == 85 {
		return '*'
	}
	if tv == 13 {
		return '/'
	}
	if tv == 83 {
		return '%'
	}
	if tv == 20 {
		return '=='
	}
	if tv == 88 {
		return '!='
	}
	if tv == 80 {
		return '<'
	}
	if tv == 22 {
		return '>'
	}
	if tv == 74 {
		return '<='
	}
	if tv == 21 {
		return '>='
	}
	if tv == 94 {
		return '|'
	}
	if tv == 109 {
		return '^'
	}
	return ''
}
