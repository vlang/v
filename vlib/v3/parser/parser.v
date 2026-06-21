module parser

import os
import strings
import v3.flat
import v3.pref
import v3.scanner
import v3.token

fn C.open(charptr, int, int) int
fn C.read(int, voidptr, int) int
fn C.close(int) int
fn C.malloc(int) &u8

const max_source_file_size = 8388608

pub struct Parser {
	prefs &pref.Preferences
mut:
	s                scanner.Scanner
	tok              token.Token
	lit              string
	tok_pos          int
	tok_is_str_tail  bool
	prev_tok         token.Token
	peek_tok         token.Token = .eof
	peek_lit         string
	peek_pos         int
	peek_is_str_tail bool
	has_peek         bool
	cur_file         string
	cur_module       string
	cur_fn           string
	pending_flag     bool
	skip_next_decl   bool
	in_for_container bool
pub mut:
	a              &flat.FlatAst = unsafe { nil }
	parsed_v_files int
	parsed_v_lines int
}

pub fn Parser.new(prefs &pref.Preferences) &Parser {
	return &Parser{
		prefs: unsafe { prefs }
		s:     scanner.new_scanner(prefs, .normal)
		a:     &flat.FlatAst{
			nodes:    []flat.Node{cap: 256}
			children: []flat.NodeId{cap: 512}
		}
	}
}

pub fn (mut p Parser) parse_file(path string) &flat.FlatAst {
	p.parse_into(path)
	return p.a
}

pub fn (mut p Parser) parse_files(paths []string) &flat.FlatAst {
	for path in paths {
		p.parse_into(path)
	}
	return p.a
}

pub fn (mut p Parser) parse_into(path string) {
	p.cur_file = path
	// File marker before content so import resolver can track source files
	p.a.add_node(flat.Node{
		kind:  .file
		value: path
	})
	src := read_source_file_raw(path)
	if src.len == 0 {
		eprintln('error reading ${path}')
		return
	}
	if path.ends_with('.v') {
		p.parsed_v_files++
		p.parsed_v_lines += count_source_lines(src)
	}
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

fn count_source_lines(src string) int {
	if src.len == 0 {
		return 0
	}
	mut lines := 1
	for i in 0 .. src.len {
		if src[i] == `\n` && i + 1 < src.len {
			lines++
		}
	}
	return lines
}

fn read_source_file_raw(path string) string {
	cpath := cstring_from_vstring(path)
	fd := C.open(cpath, 0, 0)
	if fd < 0 {
		return ''
	}
	buf := C.malloc(max_source_file_size + 1)
	mut total := 0
	for total < max_source_file_size {
		nread := C.read(fd, unsafe { buf + total }, max_source_file_size - total)
		if nread <= 0 {
			break
		}
		total += nread
	}
	C.close(fd)
	if total <= 0 {
		return ''
	}
	unsafe {
		return tos(buf, total)
	}
}

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

fn (mut p Parser) next() {
	p.prev_tok = p.tok
	if p.has_peek {
		p.tok = p.peek_tok
		p.lit = p.peek_lit
		p.tok_pos = p.peek_pos
		p.tok_is_str_tail = p.peek_is_str_tail
		p.has_peek = false
		p.normalize_current_token()
		return
	}
	p.tok_is_str_tail = p.s.in_str_incomplete
	p.tok = p.s.scan()
	p.lit = p.s.lit
	p.tok_pos = p.s.pos
	p.normalize_current_token()
	for p.tok == .comment {
		p.tok_is_str_tail = p.s.in_str_incomplete
		p.tok = p.s.scan()
		p.lit = p.s.lit
		p.tok_pos = p.s.pos
		p.normalize_current_token()
	}
}

fn (mut p Parser) peek() token.Token {
	if !p.has_peek {
		p.peek_is_str_tail = p.s.in_str_incomplete
		p.peek_tok = p.s.scan()
		p.peek_lit = p.s.lit
		p.peek_pos = p.s.pos
		p.normalize_peek_token()
		for p.peek_tok == .comment {
			p.peek_is_str_tail = p.s.in_str_incomplete
			p.peek_tok = p.s.scan()
			p.peek_lit = p.s.lit
			p.peek_pos = p.s.pos
			p.normalize_peek_token()
		}
		p.has_peek = true
	}
	return p.peek_tok
}

fn (mut p Parser) peek_is(tok token.Token) bool {
	return p.peek() == tok
}

fn (mut p Parser) normalize_current_token() {
	p.tok = normalize_scanned_token(p.tok, p.lit, p.s.src, p.tok_pos, p.tok_is_str_tail)
}

fn (mut p Parser) normalize_peek_token() {
	p.peek_tok = normalize_scanned_token(p.peek_tok, p.peek_lit, p.s.src, p.peek_pos,
		p.peek_is_str_tail)
}

@[inline]
fn token_from_id(id int) token.Token {
	return unsafe { token.Token(id) }
}

fn keyword_token_id(lit string) int {
	return match lit {
		'as' { 25 }
		'asm' { 26 }
		'assert' { 27 }
		'atomic' { 28 }
		'break' { 29 }
		'const' { 30 }
		'continue' { 31 }
		'defer' { 32 }
		'dump' { 33 }
		'else' { 34 }
		'enum' { 35 }
		'false' { 36 }
		'fn' { 37 }
		'for' { 38 }
		'__global' { 39 }
		'go' { 40 }
		'goto' { 41 }
		'if' { 42 }
		'import' { 43 }
		'in' { 44 }
		'interface' { 45 }
		'is' { 46 }
		'isreftype' { 47 }
		'_likely_' { 48 }
		'lock' { 49 }
		'match' { 50 }
		'module' { 51 }
		'mut' { 52 }
		'nil' { 53 }
		'none' { 54 }
		'__offsetof' { 55 }
		'or' { 56 }
		'pub' { 57 }
		'return' { 58 }
		'rlock' { 59 }
		'select' { 60 }
		'shared' { 61 }
		'sizeof' { 62 }
		'spawn' { 63 }
		'static' { 64 }
		'struct' { 65 }
		'true' { 66 }
		'type' { 67 }
		'typeof' { 68 }
		'union' { 69 }
		'_unlikely_' { 70 }
		'unsafe' { 71 }
		'volatile' { 72 }
		else { -1 }
	}
}

fn normalize_scanned_token(tok token.Token, lit string, src string, pos int, is_str_tail bool) token.Token {
	if int(tok) == 19 || int(tok) == 105 || int(tok) == 106 {
		return tok
	}
	if lit.len > 0 {
		first := lit[0]
		if int(tok) == 87 || int(tok) == 0 {
			if is_str_tail && int(tok) == 0 {
				return token_from_id(107)
			}
			if first >= `0` && first <= `9` {
				return token_from_id(92)
			}
			if first == `.` && lit.len > 1 && lit[1] >= `0` && lit[1] <= `9` {
				return token_from_id(92)
			}
			if first == `'` || first == `"`
				|| (first == `r` && lit.len > 1 && (lit[1] == `'` || lit[1] == `"`)) {
				return token_from_id(107)
			}
			if first == `\`` || lit.starts_with('c:') {
				return token_from_id(7)
			}
		}
		if first != `@` {
			keyword_id := keyword_token_id(lit)
			if keyword_id >= 0 {
				return token_from_id(keyword_id)
			}
		}
		if int(tok) == 0 {
			return token_from_id(87)
		}
		return tok
	}
	if pos < 0 || pos >= src.len {
		return tok
	}
	c := src[pos]
	if c == `.` {
		if pos + 1 < src.len && src[pos + 1] == `.` {
			if pos + 2 < src.len && src[pos + 2] == `.` {
				return token_from_id(18)
			}
			return token_from_id(17)
		}
		return token_from_id(16)
	}
	if c == `:` {
		if pos + 1 < src.len && src[pos + 1] == `=` {
			return token_from_id(12)
		}
		return token_from_id(8)
	}
	if int(tok) != 108 && int(tok) != 0 {
		return tok
	}
	if c == `{` {
		return token_from_id(73)
	}
	if c == `}` {
		return token_from_id(98)
	}
	if c == `(` {
		return token_from_id(78)
	}
	if c == `)` {
		return token_from_id(103)
	}
	if c == `[` {
		return token_from_id(79)
	}
	if c == `]` {
		return token_from_id(104)
	}
	if c == `,` {
		return token_from_id(9)
	}
	if c == `;` {
		return token_from_id(105)
	}
	if c == `!` {
		if pos + 1 < src.len && src[pos + 1] == `=` {
			return token_from_id(88)
		}
		if pos + 3 < src.len && src[pos + 1] == `i` && src[pos + 2] == `n`
			&& (src[pos + 3] == ` ` || src[pos + 3] == `\t`) {
			return token_from_id(90)
		}
		if pos + 3 < src.len && src[pos + 1] == `i` && src[pos + 2] == `s`
			&& (src[pos + 3] == ` ` || src[pos + 3] == `\t`) {
			return token_from_id(91)
		}
		return token_from_id(89)
	}
	if c == `=` {
		if pos + 1 < src.len && src[pos + 1] == `=` {
			return token_from_id(20)
		}
		return token_from_id(4)
	}
	if c == `&` {
		if pos + 1 < src.len && src[pos + 1] == `&` {
			return token_from_id(1)
		}
		if pos + 1 < src.len && src[pos + 1] == `=` {
			return token_from_id(2)
		}
		return token_from_id(0)
	}
	if c == `|` {
		if pos + 1 < src.len && src[pos + 1] == `|` {
			return token_from_id(77)
		}
		if pos + 1 < src.len && src[pos + 1] == `=` {
			return token_from_id(93)
		}
		return token_from_id(94)
	}
	if c == `<` {
		if pos + 1 < src.len && src[pos + 1] == `<` {
			if pos + 2 < src.len && src[pos + 2] == `=` {
				return token_from_id(76)
			}
			return token_from_id(75)
		}
		if pos + 1 < src.len && src[pos + 1] == `-` {
			return token_from_id(3)
		}
		if pos + 1 < src.len && src[pos + 1] == `=` {
			return token_from_id(74)
		}
		return token_from_id(80)
	}
	if c == `>` {
		if pos + 1 < src.len && src[pos + 1] == `>` {
			if pos + 2 < src.len && src[pos + 2] == `>` {
				if pos + 3 < src.len && src[pos + 3] == `=` {
					return token_from_id(102)
				}
				return token_from_id(101)
			}
			if pos + 2 < src.len && src[pos + 2] == `=` {
				return token_from_id(100)
			}
			return token_from_id(99)
		}
		if pos + 1 < src.len && src[pos + 1] == `=` {
			return token_from_id(21)
		}
		return token_from_id(22)
	}
	if c == `+` {
		if pos + 1 < src.len && src[pos + 1] == `+` {
			return token_from_id(24)
		}
		if pos + 1 < src.len && src[pos + 1] == `=` {
			return token_from_id(96)
		}
		return token_from_id(95)
	}
	if c == `-` {
		if pos + 1 < src.len && src[pos + 1] == `-` {
			return token_from_id(11)
		}
		if pos + 1 < src.len && src[pos + 1] == `=` {
			return token_from_id(82)
		}
		return token_from_id(81)
	}
	if c == `*` {
		if pos + 1 < src.len && src[pos + 1] == `=` {
			return token_from_id(86)
		}
		return token_from_id(85)
	}
	if c == `/` {
		if pos + 1 < src.len && src[pos + 1] == `=` {
			return token_from_id(14)
		}
		return token_from_id(13)
	}
	if c == `%` {
		if pos + 1 < src.len && src[pos + 1] == `=` {
			return token_from_id(84)
		}
		return token_from_id(83)
	}
	if c == `~` {
		return token_from_id(6)
	}
	if c == `$` {
		return token_from_id(15)
	}
	if c == `#` {
		if pos + 1 < src.len && src[pos + 1] == `[` {
			return token_from_id(79)
		}
		return token_from_id(23)
	}
	if c == `?` {
		return token_from_id(97)
	}
	return tok
}

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

fn token_id_is_infix(tv int) bool {
	return tv == 0 || tv == 1 || tv == 3 || tv == 13 || tv == 20 || tv == 21 || tv == 22 || tv == 44
		|| tv == 46 || tv == 74 || tv == 75 || tv == 77 || tv == 80 || tv == 81 || tv == 83
		|| tv == 85 || tv == 88 || tv == 90 || tv == 91 || tv == 94 || tv == 95 || tv == 99
		|| tv == 101 || tv == 109
}

fn token_is_infix(tok token.Token) bool {
	return token_id_is_infix(int(tok))
}

fn token_id_is_postfix(tv int) bool {
	return tv == 11 || tv == 24
}

fn token_is_postfix(tok token.Token) bool {
	return token_id_is_postfix(int(tok))
}

fn token_id_is_assignment(tv int) bool {
	return tv == 2 || tv == 4 || tv == 12 || tv == 14 || tv == 76 || tv == 82 || tv == 84
		|| tv == 86 || tv == 93 || tv == 96 || tv == 100 || tv == 102 || tv == 110
}

fn token_is_assignment(tok token.Token) bool {
	return token_id_is_assignment(int(tok))
}

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

fn token_left_binding_power(tok token.Token) token.BindingPower {
	return token_id_left_binding_power(int(tok))
}

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

fn token_right_binding_power(tok token.Token) token.BindingPower {
	return token_id_right_binding_power(int(tok))
}

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

fn (mut p Parser) add_child(id flat.NodeId) int {
	start := p.a.children.len
	p.a.children << id
	return start
}

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
			p.skip_attrs()
			if p.skip_next_decl {
				p.skip_next_decl = false
				p.skip_top_level_stmt()
				return flat.empty_node
			}
			return p.top_level_stmt()
		}
		.lsbr {
			p.skip_attrs()
			if p.skip_next_decl {
				p.skip_next_decl = false
				p.skip_top_level_stmt()
				return flat.empty_node
			}
			return p.top_level_stmt()
		}
		.dollar {
			return p.parse_comptime_if()
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

fn (mut p Parser) fn_decl() flat.NodeId {
	p.check(.key_fn)
	mut name := ''
	mut receiver_name := ''
	mut receiver_type := ''
	mut is_method := false

	// method receiver: fn (mut r Type) name()
	if p.tok == .lpar {
		is_method = true
		p.next()
		mut is_mut := false
		if p.tok == .key_mut || p.tok == .key_shared {
			is_mut = p.tok == .key_mut
			p.next()
		}
		receiver_name = p.expect_name()
		receiver_type = p.parse_type_name()
		if is_mut {
			receiver_type = '&' + receiver_type
		}
		p.check(.rpar)

		// operator overload: fn (r Type) + (other Type) RetType { }
		if p.tok != .name && p.tok != .eof {
			if p.tok.is_overloadable() {
				op_name := overload_token_name(p.tok)
				p.next()
				return p.fn_operator_overload(receiver_name, receiver_type, op_name)
			}
		}
	}

	// function name
	if p.tok_can_be_decl_name() {
		name = p.expect_name_or_keyword()
		if p.tok == .dot {
			p.next()
			if name == 'C' || name == 'JS' {
				// C.func or JS.func
				name = p.expect_name_or_keyword()
				for p.tok == .dot {
					p.next()
					name += '.' + p.expect_name_or_keyword()
				}
				if is_method {
					clean_type := receiver_type.trim_left('&')
					name = '${clean_type}.${name}'
				}
				return p.fn_decl_body(name, receiver_name, receiver_type, is_method, true)
			}
			// module.func or Type.static_method
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
		clean_type := receiver_type.trim_left('&')
		name = '${clean_type}.${name}'
	}

	return p.fn_decl_body(name, receiver_name, receiver_type, is_method, false)
}

fn (mut p Parser) fn_operator_overload(receiver_name string, receiver_type string, op_name string) flat.NodeId {
	// parse parameter
	p.check(.lpar)
	mut param_ids := []flat.NodeId{}
	// receiver
	param_ids << p.a.add_node(flat.Node{
		kind:  .param
		value: receiver_name
		typ:   receiver_type
	})
	// operator param
	if p.tok == .key_mut {
		p.next()
	}
	for p.tok != .rpar && p.tok != .eof {
		start_offset := p.s.offset
		param_ids << p.parse_param_group()
		if p.s.offset == start_offset && p.tok != .rpar && p.tok != .eof {
			p.next()
		}
	}
	p.check(.rpar)

	mut ret_type := 'void'
	if p.tok != .lcbr && p.tok != .semicolon && p.tok != .eof {
		ret_type = p.parse_type_name()
	}

	clean_type := receiver_type.trim_left('&')
	name := '${clean_type}.${op_name}'

	mut body_ids := []flat.NodeId{}
	if p.tok == .lcbr {
		prev_fn := p.cur_fn
		p.cur_fn = name
		p.check(.lcbr)
		for p.tok != .rcbr && p.tok != .eof {
			id := p.stmt()
			if int(id) >= 0 {
				body_ids << id
			}
		}
		p.check(.rcbr)
		p.cur_fn = prev_fn
	}

	mut all_ids := []flat.NodeId{cap: param_ids.len + body_ids.len}
	for id in param_ids {
		all_ids << id
	}
	for id in body_ids {
		all_ids << id
	}
	start := p.add_children(all_ids)
	return p.a.add_node(flat.Node{
		kind:           .fn_decl
		value:          name
		typ:            ret_type
		children_start: start
		children_count: flat.child_count(all_ids.len)
	})
}

fn (mut p Parser) fn_decl_body(name string, receiver_name string, receiver_type string, is_method bool, _ bool) flat.NodeId {
	// generic params — skip
	if p.tok == .lsbr {
		p.skip_brackets()
	}

	// params
	p.check(.lpar)
	mut param_ids := []flat.NodeId{}
	if is_method && receiver_name.len > 0 {
		param_ids << p.a.add_node(flat.Node{
			kind:  .param
			value: receiver_name
			typ:   receiver_type
		})
	}
	for p.tok != .rpar && p.tok != .eof {
		start_offset := p.s.offset
		param_ids << p.parse_param_group()
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
		return p.a.add_node(flat.Node{
			kind:           .c_fn_decl
			value:          name
			typ:            ret_type
			children_start: start
			children_count: flat.child_count(param_ids.len)
		})
	}

	// body
	mut body_ids := []flat.NodeId{}
	prev_fn := p.cur_fn
	p.cur_fn = name
	p.check(.lcbr)
	for p.tok != .rcbr && p.tok != .eof {
		id := p.stmt()
		if int(id) >= 0 {
			body_ids << id
		}
	}
	p.check(.rcbr)
	p.cur_fn = prev_fn

	mut all_ids := []flat.NodeId{cap: param_ids.len + body_ids.len}
	for id in param_ids {
		all_ids << id
	}
	for id in body_ids {
		all_ids << id
	}
	start := p.add_children(all_ids)
	return p.a.add_node(flat.Node{
		kind:           .fn_decl
		value:          name
		typ:            ret_type
		children_start: start
		children_count: flat.child_count(all_ids.len)
	})
}

fn (mut p Parser) parse_param_group() []flat.NodeId {
	mut ids := []flat.NodeId{}
	mut names := []string{}
	mut is_mut := false
	if p.tok == .key_mut {
		is_mut = true
		p.next()
	}
	if p.tok == .key_shared {
		p.next()
	}
	// variadic ...Type (no param name)
	if p.tok == .ellipsis {
		typ := p.parse_type_name()
		ids << p.a.add_node(flat.Node{
			kind:  .param
			value: ''
			typ:   typ
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
	if is_mut {
		typ = '&' + typ
	}
	for name in names {
		ids << p.a.add_node(flat.Node{
			kind:  .param
			value: name
			typ:   typ
		})
	}
	if p.tok == .comma {
		p.next()
	}
	return ids
}

fn (mut p Parser) struct_decl() flat.NodeId {
	is_union := p.tok == .key_union
	p.next() // skip 'struct' or 'union'
	mut name := p.expect(.name)
	if (name == 'C' || name == 'JS') && p.tok == .dot {
		for p.tok == .dot {
			p.next()
			name += '.' + p.expect_name_or_keyword()
		}
	}
	// generic params — skip
	mut is_generic := false
	if p.tok == .lsbr {
		is_generic = true
		p.skip_brackets()
	}
	// implements clause
	if p.tok == .name && p.lit == 'implements' {
		p.next()
		p.parse_type_name()
		for p.tok == .comma {
			p.next()
			p.parse_type_name()
		}
	}
	// no body (C struct forward decl)
	if p.tok != .lcbr {
		if p.tok == .semicolon {
			p.next()
		}
		return p.a.add_node(flat.Node{
			kind:  .struct_decl
			value: name
			typ:   struct_decl_typ(is_union, is_generic)
		})
	}
	p.check(.lcbr)
	mut ids := []flat.NodeId{}
	for p.tok != .rcbr && p.tok != .eof {
		// access modifiers
		if p.tok == .key_pub {
			p.next()
			if p.tok == .key_mut {
				p.next()
			}
			if p.tok == .name && p.lit == 'module_mut' {
				p.next()
			}
			if p.tok == .colon {
				p.next()
			}
			continue
		}
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
		// comptime $if in struct fields
		if p.tok == .dollar {
			id := p.parse_comptime_if()
			if int(id) >= 0 {
				ids << id
			}
			continue
		}
		// attributes — skip
		if p.tok == .attribute {
			p.skip_attrs()
			continue
		}
		// field: name type [= default] [@[attrs]]
		if p.tok == .name || p.tok.is_keyword() {
			field_name := p.expect_name_or_keyword()
			if p.tok == .dot {
				p.next()
				second := p.expect_name_or_keyword()
				full_type := '${field_name}.${second}'
				ids << p.a.add_node(flat.Node{
					kind:  .field_decl
					value: full_type
					typ:   full_type
				})
				if p.tok == .semicolon {
					p.next()
				}
				continue
			}
			// embedded struct (type on its own line, followed by semicolon)
			if p.tok == .semicolon || p.tok == .rcbr {
				ids << p.a.add_node(flat.Node{
					kind:  .field_decl
					value: field_name
					typ:   field_name
				})
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
				for n in names {
					ids << p.a.add_node(flat.Node{
						kind:  .field_decl
						value: n
						typ:   field_type
					})
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
			// trailing field attributes — skip
			if p.tok == .attribute || p.tok == .lsbr {
				p.skip_attrs()
			}
			ids << p.a.add_node(flat.Node{
				kind:           .field_decl
				value:          field_name
				typ:            field_type
				children_start: children_start
				children_count: flat.child_count(children_count)
			})
			if p.tok == .semicolon {
				p.next()
			}
		} else {
			p.next()
		}
	}
	p.check(.rcbr)
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .struct_decl
		value:          name
		typ:            struct_decl_typ(is_union, is_generic)
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn struct_decl_typ(is_union bool, is_generic bool) string {
	mut parts := []string{}
	if is_union {
		parts << 'union'
	}
	if is_generic {
		parts << 'generic'
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
			if p.tok == .assign {
				// global with initializer: __global name = expr
				p.next()
				val_id := p.expr(.lowest)
				vstart := p.add_child(val_id)
				ids << p.a.add_node(flat.Node{
					kind:           .field_decl
					value:          full_name
					typ:            ''
					children_start: vstart
					children_count: 1
				})
			} else {
				gtype := p.parse_type_name()
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
	// `as` type
	if p.tok == .key_as {
		p.next()
		p.parse_type_name()
	}
	p.check(.lcbr)
	mut ids := []flat.NodeId{}
	for p.tok != .rcbr && p.tok != .eof {
		if p.tok == .semicolon {
			p.next()
			continue
		}
		// attributes on enum fields — skip
		if p.tok == .attribute || p.tok == .lsbr {
			p.skip_attrs()
			continue
		}
		field_name := p.expect_name_or_keyword()
		if p.tok == .assign {
			p.next()
			val_id := p.expr(.lowest)
			vstart := p.add_child(val_id)
			ids << p.a.add_node(flat.Node{
				kind:           .enum_field
				value:          field_name
				children_start: vstart
				children_count: 1
			})
		} else {
			ids << p.a.add_node(flat.Node{
				kind:  .enum_field
				value: field_name
			})
		}
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
	return p.a.add_node(flat.Node{
		kind:           .enum_decl
		value:          name
		typ:            typ
		children_start: start
		children_count: flat.child_count(ids.len)
	})
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
	if p.tok == .lsbr {
		p.skip_brackets()
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
			children_start: start
			children_count: flat.child_count(variants.len)
		})
	}
	if p.tok == .semicolon {
		p.next()
	}
	// type alias
	return p.a.add_node(flat.Node{
		kind:  .type_decl
		value: name
		typ:   first_type
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
	if p.tok == .lsbr {
		p.skip_brackets()
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
		field_name := p.expect_name_or_keyword()
		if p.tok == .lpar {
			// method: name(params) ret_type
			p.next() // skip (
			mut params := []flat.NodeId{}
			for p.tok != .rpar && p.tok != .eof {
				if p.tok == .key_mut {
					p.next()
				}
				ptype := p.parse_type_name()
				if p.tok == .name {
					// param has a name before type, consume the actual type
					ptype2 := p.parse_type_name()
					params << p.a.add_node(flat.Node{
						kind: .param
						typ:  ptype2
					})
				} else {
					params << p.a.add_node(flat.Node{
						kind: .param
						typ:  ptype
					})
				}
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
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) import_stmt() flat.NodeId {
	p.next() // skip 'import'
	mut name := p.expect_name()
	mut alias := name
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
			p.expect_name_or_keyword()
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
		kind:  .import_decl
		value: name
		typ:   alias
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
	full := p.lit
	p.next() // skip '#' (lit already contains the full line)
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
		if p.tok == .name && p.lit == 'flag' {
			p.pending_flag = true
		}
		if p.tok == .key_if {
			p.next()
			mut cond := strings.new_builder(32)
			for p.tok != .rsbr && p.tok != .eof {
				tok_str := p.comptime_cond_token_text()
				if cond.len > 0 && tok_str != '?' {
					cond.write_string(' ')
				}
				cond.write_string(tok_str)
				p.next()
			}
			if !eval_comptime_cond(p.prefs, cond.str()) {
				p.skip_next_decl = true
			}
		}
		for p.tok != .rsbr && p.tok != .eof {
			p.next()
		}
		p.check(.rsbr)
		return
	}
	if p.tok == .lsbr {
		p.skip_brackets()
	}
}

fn (mut p Parser) skip_top_level_stmt() {
	for p.tok == .attribute || p.tok == .lsbr {
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
		// $for or other comptime — skip
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
	cond := p.parse_comptime_cond()
	if comptime_cond_has_type_test(cond) {
		p.skip_block()
		p.skip_comptime_else()
		return flat.empty_node
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

fn (mut p Parser) parse_comptime_cond() string {
	mut cond := strings.new_builder(64)
	for p.tok != .lcbr && p.tok != .eof {
		tok_str := p.comptime_cond_token_text()
		if cond.len > 0 && tok_str != '?' {
			cond.write_string(' ')
		}
		cond.write_string(tok_str)
		p.next()
	}
	return cond.str()
}

fn (p &Parser) comptime_cond_token_text() string {
	if p.lit.len > 0 {
		return p.lit
	}
	if p.s.pos >= 0 && p.s.pos < p.s.src.len {
		c := p.s.src[p.s.pos]
		if c == `&` && p.s.pos + 1 < p.s.src.len && p.s.src[p.s.pos + 1] == `&` {
			return '&&'
		}
		if c == `|` && p.s.pos + 1 < p.s.src.len && p.s.src[p.s.pos + 1] == `|` {
			return '||'
		}
		if c == `!` {
			return '!'
		}
		if c == `?` {
			return '?'
		}
	}
	tok := p.tok
	if tok == .and {
		return '&&'
	}
	if tok == .logical_or {
		return '||'
	}
	if tok == .not {
		return '!'
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
	return ''
}

fn comptime_cond_has_type_test(cond string) bool {
	return cond.contains(' is ') || cond.contains(' !is ')
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

fn eval_comptime_cond(prefs &pref.Preferences, cond string) bool {
	c := cond.trim_space()
	if c.starts_with('!') {
		return !eval_comptime_cond(prefs, c[1..])
	}
	if c.contains('&&') {
		left := c.all_before('&&')
		right := c.all_after('&&')
		return eval_comptime_cond(prefs, left) && eval_comptime_cond(prefs, right)
	}
	if c.contains('||') {
		left := c.all_before('||')
		right := c.all_after('||')
		return eval_comptime_cond(prefs, left) || eval_comptime_cond(prefs, right)
	}
	flag := c.trim_space().trim_right('? ')
	return pref.comptime_flag_value(prefs, flag)
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
	if p.peek() == .key_if || p.peek() == .key_for {
		return p.parse_comptime_if()
	}
	p.next() // skip $
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
	for p.tok != .semicolon && p.tok != .eof {
		p.next()
	}
	return flat.empty_node
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
			return p.assign_or_expr_stmt()
		}
		.key_static {
			return p.static_decl_stmt()
		}
		.key_pub {
			p.next()
			if p.tok == .key_fn {
				return p.fn_decl()
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
			spawn_expr := p.expr(.lowest)
			if p.tok == .semicolon {
				p.next()
			}
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
			return p.parse_comptime_if()
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

fn (mut p Parser) static_decl_stmt() flat.NodeId {
	p.next() // skip `static`
	if p.tok == .key_mut {
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
	p.next() // skip 'return'
	mut ids := []flat.NodeId{}
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

	// Check for for-in: `for x in ...` or `for i, x in ...` or `for mut x in ...`
	if p.tok == .name && (p.peek() == .key_in || p.peek() == .comma) {
		first_expr := p.expr(.bit_or)
		return p.for_in(first_expr)
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
		if p.tok == .key_in || p.tok == .comma {
			return p.for_in(first_expr)
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

	// for-in: `for x in expr` or `for i, x in expr`
	if p.tok == .key_in || p.tok == .comma {
		return p.for_in(first_expr)
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

fn (mut p Parser) for_in(first_expr flat.NodeId) flat.NodeId {
	// first_expr is either the key var or the only var
	mut key_id := first_expr
	mut val_id := flat.empty_node

	if p.tok == .comma {
		p.next()
		// second variable
		if p.tok == .key_mut {
			p.next()
		}
		val_id = p.a.add_val(.ident, p.expect_name())
	}

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
	return p.a.add_node(flat.Node{
		kind:           .match_stmt
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) match_branch_cond() flat.NodeId {
	if p.tok == .name && p.peek() == .dot {
		mod_name := p.lit
		p.next()
		p.next()
		if p.tok == .name && p.lit.len > 0 && p.lit[0] >= `A` && p.lit[0] <= `Z` {
			type_name := p.lit
			p.next()
			mod_id := p.a.add_val(.ident, mod_name)
			start := p.add_child(mod_id)
			return p.a.add_node(flat.Node{
				kind:           .selector
				value:          type_name
				children_start: start
				children_count: 1
			})
		}
		base_id := p.a.add_val(.ident, mod_name)
		start := p.add_child(base_id)
		sel := p.a.add_node(flat.Node{
			kind:           .selector
			value:          p.lit
			children_start: start
			children_count: 1
		})
		if p.tok == .name {
			p.next()
		}
		if p.tok != .lcbr && p.tok != .comma {
			return p.expr_with_lhs(sel, .lowest)
		}
		return sel
	}
	if p.tok == .name && p.lit.len > 0 && p.lit[0] >= `A` && p.lit[0] <= `Z` {
		name := p.lit
		p.next()
		return p.a.add_val(.ident, name)
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

	p.check(.lcbr)
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
	p.check(.lcbr)
	mut ids := []flat.NodeId{}
	for p.tok != .rcbr && p.tok != .eof {
		id := p.stmt()
		if int(id) >= 0 {
			ids << id
		}
	}
	p.check(.rcbr)
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
			if p.tok == .key_mut {
				p.next()
			}
			lhs_ids << p.expr(.lowest)
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
		return p.a.add_node(flat.Node{
			kind:           .block
			children_start: bstart
			children_count: flat.child_count(stmt_ids.len)
		})
	}

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
			children_start: istart
			children_count: 2
		})
	}

	if token_is_assignment(p.tok) {
		op_id := int(p.tok)
		p.next()
		rhs := p.expr(.lowest)
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
	if p.tok == .lpar {
		p.next()
		if p.tok == .key_fn {
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
		// selector / method call
		if p.tok == .dot {
			lhs = p.selector_or_method(lhs)
			continue
		}
		// module-qualified struct init: module.Type{} or module.Type{field: val, ...}
		if p.tok == .lcbr {
			lhs_node := p.a.nodes[int(lhs)]
			if lhs_node.kind == .selector && lhs_node.value.len > 0
				&& (p.peek() == .rcbr || p.peek() == .name || p.peek() == .ellipsis) {
				base := p.a.child_node(&lhs_node, 0)
				is_c_struct := base.kind == .ident && base.value == 'C'
					&& !is_all_upper_ident(lhs_node.value)
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
			if token_is_infix(peek_tok) && int(peek_tok) != 85 && int(peek_tok) != 0 {
				p.next()
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
			name := p.lit
			p.next()
			if name == '@FILE' {
				return p.a.add_val_id(5, p.cur_file)
			}
			if name == '@VMODROOT' {
				return p.a.add_val_id(5, vmod_root_for_file(p.cur_file))
			}
			if name == '@VEXEROOT' {
				return p.a.add_val_id(5, p.prefs.vroot)
			}
			if name == '@VEXE' {
				return p.a.add_val_id(5, p.prefs.vroot + '/v')
			}
			if name == '@LINE' {
				return p.a.add_val_id(1, '0')
			}
			if name == '@FN' || name == '@METHOD' {
				return p.a.add_val_id(5, p.cur_fn)
			}
			if name == '@LOCATION' {
				return p.a.add_val_id(5, '${p.cur_file}:0: ${p.cur_fn}')
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
			// struct init: Name{...}; vlib/builtin also uses concrete lowercase
			// runtime structs like array{} and string{}.
			if !p.in_for_container && p.tok == .lcbr && name.len > 0
				&& ((name[0] >= `A` && name[0] <= `Z`)
				|| name in ['array', 'string', 'map', 'mapnode', '_result', '_option']) {
				return p.struct_init(name)
			}
			// type cast: TypeName(expr) or builtin_type(expr)
			if p.tok == .lpar && name.len > 0 && ((name[0] >= `A` && name[0] <= `Z`)
				|| is_builtin_type(name)) {
				p.next() // skip (
				inner := p.expr(.lowest)
				p.check(.rpar)
				cstart := p.add_child(inner)
				return p.a.add_node(flat.Node{
					kind:           .cast_expr
					value:          name
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
			} else if p.tok == .name && is_builtin_type(p.lit) {
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

fn (mut p Parser) selector_or_method(lhs flat.NodeId) flat.NodeId {
	p.next() // skip '.'
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
		if p.tok == .key_mut || p.tok == .key_shared {
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
			arg := p.expr(.lowest)
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
			value:          'range'
			children_start: istart
			children_count: flat.child_count(ids.len)
		})
	}
	idx := p.expr(.logical_or)
	// range index: arr[a..b]
	if p.tok == .dotdot {
		p.next()
		mut end_id := flat.empty_node
		if p.tok != .rsbr {
			end_id = p.expr(.lowest)
		}
		p.check(.rsbr)
		mut ids := []flat.NodeId{}
		ids << lhs
		ids << idx
		if int(end_id) >= 0 {
			ids << end_id
		}
		istart := p.add_children(ids)
		return p.a.add_node(flat.Node{
			kind:           .index
			value:          'range'
			children_start: istart
			children_count: flat.child_count(ids.len)
		})
	}
	p.check(.rsbr)
	istart := p.add_children2(lhs, idx)
	return p.a.add_node(flat.Node{
		kind:           .index
		children_start: istart
		children_count: 2
	})
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
	if p.lit.len > 0 {
		if p.lit[0] == `r` && p.lit.len > 1 {
			q = p.lit[1]
		} else if p.lit[0] == `'` || p.lit[0] == `"` {
			q = p.lit[0]
		}
	}
	val := strip_quotes(p.lit)
	p.next()
	if int(p.tok) != 106 {
		return p.a.add_val_id(5, val)
	}
	// string interpolation
	return p.string_interp(val, q)
}

fn (mut p Parser) string_interp(first_part string, quote u8) flat.NodeId {
	mut ids := []flat.NodeId{}
	if first_part.len > 0 {
		ids << p.a.add_val_id(5, first_part)
	}
	for int(p.tok) == 106 {
		p.next() // skip $
		p.check(.lcbr) // skip {
		ids << p.expr(.lowest)
		// format spec: :fmt
		if p.tok == .colon {
			p.next()
			for p.tok != .rcbr && p.tok != .eof {
				p.next()
			}
		}
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

fn (mut p Parser) array_literal() flat.NodeId {
	p.next() // skip '['
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
			p.next()
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
					vstart := p.add_child(val)
					ids << p.a.add_node(flat.Node{
						kind:           .field_init
						value:          fname
						children_start: vstart
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
			start := p.add_children(ids)
			return p.a.add_node(flat.Node{
				kind:           .array_init
				value:          elem_type
				children_start: start
				children_count: flat.child_count(ids.len)
			})
		}
		return p.a.add_val(.array_init, elem_type)
	}
	// array literal: [1, 2, 3]
	// or fixed array type: [3]int
	mut ids := []flat.NodeId{}
	ids << p.expr(.lowest)
	// check if it's [N]Type (fixed array type)
	if p.tok == .rsbr {
		p.next()
		if p.tok == .name || p.tok == .amp || p.tok == .lsbr || p.tok == .question {
			// fixed array type: [N]Type
			size_str := p.a.nodes[int(ids[0])].value
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
					children_start: start
					children_count: flat.child_count(init_ids.len)
				})
			}
			return p.a.add_val(.array_init, fixed_type)
		}
		// single-element array: [expr]
		start := p.add_children(ids)
		return p.a.add_node(flat.Node{
			kind:           .array_literal
			children_start: start
			children_count: flat.child_count(ids.len)
		})
	}
	// multi-element array: [a, b, c]
	for p.tok == .comma {
		p.next()
		if p.tok == .rsbr {
			break
		}
		ids << p.expr(.lowest)
	}
	p.check(.rsbr)
	// check for `!` (fixed array with values)
	if p.tok == .not {
		p.next()
	}
	start := p.add_children(ids)
	return p.a.add_node(flat.Node{
		kind:           .array_literal
		children_start: start
		children_count: flat.child_count(ids.len)
	})
}

fn (mut p Parser) fn_literal() flat.NodeId {
	p.next() // skip 'fn'
	// capture list: fn [a, b] (params) ret { }
	mut capture_ids := []flat.NodeId{}
	if p.tok == .lsbr {
		p.next()
		for p.tok != .rsbr && p.tok != .eof {
			if p.tok == .key_mut {
				p.next()
			}
			capture_ids << p.a.add_val(.ident, p.expect_name())
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
		param_ids << p.parse_param_group()
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
		p.check(.lcbr)
		for p.tok != .rcbr && p.tok != .eof {
			id := p.stmt()
			if int(id) >= 0 {
				body_ids << id
			}
		}
		p.check(.rcbr)
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

fn (mut p Parser) select_branch() flat.NodeId {
	mut is_else := false
	mut cond_ids := []flat.NodeId{}
	if p.tok == .key_else {
		is_else = true
		p.next()
	} else {
		cond_ids << p.expr(.lowest)
		// could be assignment: ch <- val or val := <-ch
		if token_is_assignment(p.tok) || p.tok == .decl_assign {
			op := p.tok
			p.next()
			cond_ids << p.expr(.lowest)
			_ = op
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
	return p.a.add_node(flat.Node{
		kind:           .select_branch
		value:          if is_else { 'else' } else { '' }
		children_start: start
		children_count: flat.child_count(all_ids.len)
	})
}

fn (mut p Parser) sizeof_expr() flat.NodeId {
	p.next() // skip 'sizeof'
	p.check(.lpar)
	type_name := p.parse_type_name()
	p.check(.rpar)
	return p.a.add_val(.sizeof_expr, type_name)
}

fn (mut p Parser) typeof_expr() flat.NodeId {
	p.next() // skip 'typeof'
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

fn (mut p Parser) parse_type_name_progress() string {
	start_offset := p.s.offset
	typ := p.parse_type_name()
	if p.s.offset == start_offset && p.tok != .eof {
		p.next()
	}
	return typ
}

fn (p &Parser) can_start_type_name() bool {
	return p.tok == .name || p.tok == .amp || p.tok == .question || p.tok == .not || p.tok == .lsbr
		|| p.tok == .lpar || p.tok == .key_fn || p.tok == .ellipsis || p.tok == .key_mut
		|| p.tok == .key_shared || p.tok == .key_atomic
}

fn fn_type_param_with_mut(typ string, is_mut bool) string {
	if !is_mut || typ.len == 0 || typ.starts_with('&') {
		return typ
	}
	return '&' + typ
}

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
			return fn_type_param_with_mut(second, is_mut)
		}
	}
	return fn_type_param_with_mut(first, is_mut)
}

fn (mut p Parser) parse_type_name() string {
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
		// fixed array [N]T
		mut len_lit := p.lit
		p.next()
		p.check(.rsbr)
		return '[${len_lit}]' + p.parse_type_name()
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
		p.next()
		if p.tok == .lcbr {
			p.skip_block()
		}
		return 'struct'
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
			if p.tok == .name || p.tok == .amp || p.tok == .lsbr || p.tok == .question {
				elem := p.parse_type_name()
				return 'chan ${elem}'
			}
			return 'chan'
		}
		// thread T
		if name == 'thread' {
			if p.tok == .name || p.tok == .amp || p.tok == .lsbr || p.tok == .question {
				elem := p.parse_type_name()
				return 'thread ${elem}'
			}
			return 'thread'
		}
		// qualified: mod.Type
		if p.tok == .dot {
			p.next()
			if p.tok == .name {
				name += '.' + p.lit
				p.next()
			}
		}
		// generic: Type[T, U]
		if p.tok == .lsbr {
			// peek ahead to distinguish generic from index
			pk := p.peek()
			if pk == .name || pk == .amp || pk == .lsbr || pk == .question || pk == .rsbr
				|| pk == .key_fn {
				p.next() // skip [
				mut params := []string{}
				params << p.parse_type_name()
				for p.tok == .comma {
					p.next()
					params << p.parse_type_name()
				}
				p.check(.rsbr)
				name += '[' + params.join(', ') + ']'
			}
		}
	}
	return name
}

// ==================== helpers ====================

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
			c := match s[i + 1] {
				`n` { u8(`\n`) }
				`t` { u8(`\t`) }
				`r` { u8(`\r`) }
				`\\` { u8(`\\`) }
				`'` { u8(`'`) }
				`"` { u8(`"`) }
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

fn is_builtin_type(name string) bool {
	return name in ['int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
		'byte', 'bool', 'string', 'rune', 'char', 'voidptr', 'charptr', 'byteptr', 'usize', 'isize',
		'array', 'map', 'mapnode', '_result', '_option']
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
