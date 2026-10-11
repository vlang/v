module parser

import v.flat
import os
import v.pref
import v.token

// ParsedSumVariant is one `|`-separated entry of a sum type declaration: a plain
// variant type (`Foo`, `[]int`) or a named variant (`Count(int)`, `Void`).
struct ParsedSumVariant {
	text        string // the variant type text, or the name of a named variant
	payload     string // the payload type text of a named variant
	has_payload bool
	is_named    bool // written as `Name(...)`
	malformed   bool // an invalid payload list was already reported
	start       int
	end         int
	name_end    int
}

// parse_sum_type_variant finishes a sum type variant whose leading type text was
// already parsed. A following `(` makes it a named variant with one payload type.
fn (mut p Parser) parse_sum_type_variant(text string, start int) ParsedSumVariant {
	name_end := p.prev_tok_end
	if p.tok != .lpar {
		return ParsedSumVariant{
			text:     text
			start:    start
			end:      p.prev_tok_end
			name_end: name_end
		}
	}
	p.next() // skip (
	if p.tok == .rpar {
		p.next()
		if !p.prefs.is_fmt {
			p.record_diagnostic_span('invalid sum type variant `${text}()`: a variant without a payload is written without parentheses, e.g. `${text}`',
				start, p.prev_tok_end)
		}
		return ParsedSumVariant{
			text:      text
			is_named:  true
			malformed: true
			start:     start
			end:       p.prev_tok_end
			name_end:  name_end
		}
	}
	payload := p.parse_type_name()
	mut malformed := false
	if p.tok != .rpar {
		// `Rect(w f64, h f64)` or `Pair(int, int)`: skip to the closing `)` without
		// leaving the declaration, so the parser does not fall into script mode.
		mut depth := 1
		for p.tok != .eof && p.tok !in [.lcbr, .rcbr, .key_fn, .key_struct, .key_type, .key_pub,
			.key_const, .key_enum, .key_interface, .key_import] {
			if p.tok == .lpar {
				depth++
			} else if p.tok == .rpar {
				depth--
				if depth == 0 {
					break
				}
			}
			p.next()
		}
		end := if p.tok == .rpar { p.tok_end } else { p.prev_tok_end }
		if !p.prefs.is_fmt {
			src := p.s.src[clamp_source_offset(start, p.s.src.len)..clamp_source_offset(end,
				p.s.src.len)]
			p.record_diagnostic_span('invalid sum type variant `${src}`: a named variant holds a single payload type; use a struct payload for several fields, e.g. `${text}(${text}Data)`',
				start, end)
		}
		malformed = true
	}
	if p.tok == .rpar {
		p.next()
	}
	return ParsedSumVariant{
		text:        text
		payload:     payload
		has_payload: true
		is_named:    true
		malformed:   malformed
		start:       start
		end:         p.prev_tok_end
		name_end:    name_end
	}
}

fn is_plain_capitalized_ident(name string) bool {
	if name.len == 0 || name[0] < `A` || name[0] > `Z` {
		return false
	}
	for c in name {
		if !is_name_char(c) {
			return false
		}
	}
	return true
}

// named_sum_type_decl builds a sum type whose variants are identified by name:
// `type Expr = IntLit(int) | Count(int) | Void`. Each named variant becomes a
// hidden struct, `Expr@variant@Count { payload int }`, and the sum type holds
// those structs, so storage, tags, casts and matching reuse the regular sum type
// machinery. The formatter keeps the source spelling of each variant instead.
fn (mut p Parser) named_sum_type_decl(name string, is_pub bool, language_prefix string, generic_params []string, generic_constraints []string, variants []ParsedSumVariant, type_start int) flat.NodeId {
	if !p.prefs.is_fmt {
		p.validate_named_sum_variants(name, language_prefix, variants)
	}
	mut ids := []flat.NodeId{cap: variants.len + 1}
	mut variant_ids := []flat.NodeId{cap: variants.len}
	for variant in variants {
		variant_pos := token.new_span(p.cur_file_id, variant.start, variant.end)
		if p.prefs.is_fmt {
			variant_ids << p.add_node(flat.Node{
				kind:  .ident
				value: if variant.has_payload {
					'${variant.text}(${variant.payload})'
				} else {
					variant.text
				}
				pos:   variant_pos
			})
			continue
		}
		hidden := flat.named_variant_type_name(name, variant.text)
		// Every variant struct of a generic sum type takes all its type parameters,
		// so `Opt[int].Nothing` names `Opt@variant@Nothing[int]` without knowing
		// which parameters the payload uses.
		used_params := generic_params
		mut fields := []flat.NodeId{}
		if variant.has_payload {
			fid := p.add_node(flat.Node{
				kind:  .field_decl
				value: flat.named_variant_payload_field
				typ:   variant.payload
				pos:   variant_pos
			})
			p.apply_field_meta(fid, false, true, false, false, []string{}, false)
			fields << fid
		}
		fields_start := p.add_children(fields)
		ids << p.add_node(flat.Node{
			kind:           .struct_decl
			op:             if is_pub { .arrow } else { .none }
			value:          hidden
			typ:            if used_params.len > 0 { 'generic' } else { '' }
			payload:        flat.node_payload(used_params)
			children_start: fields_start
			children_count: flat.child_count(fields.len)
			pos:            variant_pos
		})
		variant_ids << p.add_node(flat.Node{
			kind:  .ident
			value: if used_params.len > 0 {
				'${hidden}[${used_params.join(', ')}]'
			} else {
				hidden
			}
			pos:   variant_pos
		})
	}
	variants_start := p.add_children(variant_ids)
	decl_id := p.add_node(flat.Node{
		kind:           .type_decl
		op:             if is_pub { .arrow } else { .none }
		value:          name
		payload:        flat.node_payload_with_constraints(generic_params, named_constraints(generic_constraints))
		children_start: variants_start
		children_count: flat.child_count(variant_ids.len)
		pos:            p.span_to(type_start)
	})
	if p.prefs.is_fmt {
		return decl_id
	}
	ids << decl_id
	block_start := p.add_children(ids)
	return p.add_node(flat.Node{
		kind:           .block
		children_start: block_start
		children_count: flat.child_count(ids.len)
		pos:            p.span_to(type_start)
	})
}

fn (mut p Parser) validate_named_sum_variants(name string, language_prefix string, variants []ParsedSumVariant) {
	if language_prefix.len > 0 {
		p.record_diagnostic_span('`${name}`: ${language_prefix.trim_right('.')} types cannot have named variants',
			variants[0].start, variants[variants.len - 1].end)
		return
	}
	mut seen := map[string]bool{}
	for variant in variants {
		if !variant.is_named {
			if !is_plain_capitalized_ident(variant.text) {
				p.record_diagnostic_span('invalid sum type variant `${variant.text}`: sum type `${name}` has named variants, so each variant must be a capitalized name with an optional payload, e.g. `Value(${variant.text})`',
					variant.start, variant.end)
				continue
			}
		} else if !is_plain_capitalized_ident(variant.text) {
			first := if variant.text.len > 0 { variant.text[0] } else { u8(0) }
			if first >= `a` && first <= `z` && !variant.text.contains('.')
				&& !variant.text.contains('[') {
				p.record_diagnostic_span('invalid sum type variant name `${variant.text}`: variant names must start with a capital letter',
					variant.start, variant.name_end)
			} else {
				p.record_diagnostic_span('invalid sum type variant name `${variant.text}`: a variant name must be a single capitalized name',
					variant.start, variant.name_end)
			}
			continue
		}
		if variant.text in seen {
			p.record_diagnostic_span('duplicate sum type variant `${variant.text}` in `${name}`',
				variant.start, variant.name_end)
			continue
		}
		seen[variant.text] = true
		if variant.has_payload && !variant.malformed && variant.payload in ['none', 'void'] {
			p.record_diagnostic_span('invalid payload type `${variant.payload}` for sum type variant `${name}.${variant.text}`',
				variant.start, variant.end)
		}
	}
}

// named_variant_owner returns the sum type text (`Expr`, `mod.Expr`) when the
// selector `lhs.variant` names a sum type variant. The variant is capitalized;
// the owner is a capitalized type or a declared generated type. Modules,
// constants, fields, methods and enum values are snake_case. C and JS names
// and capitalized import aliases keep their meaning.
fn (mut p Parser) named_variant_owner(lhs flat.NodeId, variant string) ?string {
	if p.prefs.is_fmt || p.is_translated || !is_plain_capitalized_ident(variant) || int(lhs) < 0
		|| int(lhs) >= p.a.nodes.len {
		return none
	}
	node := p.a.nodes[int(lhs)]
	if node.kind == .ident {
		if p.is_named_variant_owner_name(node.value) {
			return node.value
		}
		return none
	}
	if node.kind == .selector && node.children_count == 1 {
		base := p.a.child_node(&node, 0)
		if base.kind == .ident && base.value in p.imported_module_names
			&& !p.is_local_binding(base.value) && p.is_imported_named_variant_owner_name(base.value, node.value) {
			return '${base.value}.${node.value}'
		}
	}
	// A generic sum type with explicit type arguments, `Opt[int].Some(3)`.
	if node.kind == .index && node.children_count >= 2 && node.pos.is_valid() {
		base_id := p.a.child(&node, 0)
		base_text := p.named_variant_owner(base_id, variant) or { return none }
		start := clamp_source_offset(int(node.pos.offset), p.s.src.len)
		end := clamp_source_offset(int(node.pos.end), p.s.src.len)
		text := p.s.src[start..end]
		bracket := text.index_u8(`[`)
		if bracket > 0 && text.ends_with(']') {
			return base_text + text[bracket..]
		}
	}
	return none
}

fn (mut p Parser) is_named_variant_owner_name(name string) bool {
	// Single capital letters are generic parameters.
	return ((name.len > 1 && is_plain_capitalized_ident(name)) || p.is_generated_type_name(name))
		&& name !in ['C', 'JS']
		&& name !in p.imported_module_names && !p.is_local_binding(name)
}

// is_imported_named_variant_owner_name also accepts declared generated type names.
// Imported translated values can have capitalized fields, so a lowercase owner
// must be a type rather than a constant or variable in the imported module.
fn (mut p Parser) is_imported_named_variant_owner_name(module_alias string, name string) bool {
	if name.len == 0 || name in ['C', 'JS'] {
		return false
	}
	if name.len > 1 && is_plain_capitalized_ident(name) {
		return true
	}
	for c in name {
		if !is_name_char(c) {
			return false
		}
	}
	key := '${p.cur_file}\x00${module_alias}'
	if !p.named_variant_import_scans[key] {
		p.named_variant_import_scans[key] = true
		for node in p.a.nodes {
			if node.kind != .import_decl || node.pos.id != p.cur_file_id || node.typ != module_alias {
				continue
			}
			dir := p.prefs.get_module_path(node.value, p.cur_file)
			if dir.len == 0 {
				break
			}
			for path in p.prefs.without_excluded(pref.get_v_files_from_dir_for_target(dir,
				p.prefs.user_defines, p.prefs.target)) {
				source := os.read_file(path) or { continue }
				mut declarations := Parser.new(p.prefs)
				declarations.cur_file = path
				declarations.s.init(p.s.current_file(), source)
				for {
					kind := declarations.s.scan()
					if kind == .eof {
						break
					}
					if kind == .key_module {
						if declarations.s.scan() == .name {
							declarations.cur_module = declarations.s.lit
						}
						break
					}
				}
				declarations.scan_translated_sizeof_source(source)
				for type_key in declarations.translated_sizeof_type_names.keys() {
					p.named_variant_import_types['${key}\x00${type_key.all_after_last('\x00')}'] = true
				}
			}
			break
		}
	}
	return p.named_variant_import_types['${key}\x00${name}']
}

// imported_named_variant_pattern_starts_here recognizes a generated owner
// followed by a capitalized variant, including an owner's generic arguments.
fn (mut p Parser) imported_named_variant_pattern_starts_here(module_name string) bool {
	if module_name !in p.imported_module_names || p.is_local_binding(module_name)
		|| p.tok != .name || !p.is_imported_named_variant_owner_name(module_name, p.lit) {
		return false
	}
	mut next := p.peek()
	mut lookahead := p.s
	if next == .lsbr {
		if !scan_past_closing(mut lookahead, .lsbr, .rsbr) {
			return false
		}
		next = lookahead.scan()
	}
	if next != .dot {
		return false
	}
	return lookahead.scan() == .name && is_plain_capitalized_ident(lookahead.lit)
}

// named_variant_value lowers a payload-less variant reference `Expr.Void` to
// `Expr(Expr@variant@Void{})`. The checker reports a missing payload when the
// variant has one.
fn (mut p Parser) named_variant_value(sum string, variant string, lhs flat.NodeId) flat.NodeId {
	return p.named_variant_init(sum, variant, flat.empty_node, lhs)
}

// named_variant_constructor lowers `Expr.Count(value)` to
// `Expr(Expr@variant@Count{payload: value})`. The current token is `(`.
fn (mut p Parser) named_variant_constructor(sum string, variant string, lhs flat.NodeId) flat.NodeId {
	args_start := p.tok_pos
	p.next() // skip (
	mut args := []flat.NodeId{}
	for p.tok != .rpar && p.tok != .eof {
		args << p.expr(.lowest)
		if p.tok == .comma {
			p.next()
			continue
		}
		break
	}
	p.check(.rpar)
	if args.len != 1 {
		message := if args.len == 0 {
			'`${sum}.${variant}()`: a variant constructor takes exactly one payload value; a variant without a payload is written without parentheses, e.g. `${sum}.${variant}`'
		} else {
			'`${sum}.${variant}` takes a single payload value, not ${args.len}; use a struct payload for several values'
		}
		p.record_diagnostic_span(message, args_start, p.prev_tok_end)
	}
	return p.named_variant_init(sum, variant, if args.len > 0 { args[0] } else { flat.empty_node },
		lhs)
}

fn (mut p Parser) named_variant_init(sum string, variant string, payload flat.NodeId, lhs flat.NodeId) flat.NodeId {
	mut fields := []flat.NodeId{}
	if int(payload) >= 0 {
		fields << p.add_node_from(flat.Node{
			kind:           .field_init
			value:          flat.named_variant_payload_field
			children_start: p.add_child(payload)
			children_count: 1
		}, payload)
	}
	fields_start := p.add_children(fields)
	// `Opt[int]` and `Some` name the variant struct `Opt@variant@Some[int]`.
	bracket := sum.index_u8(`[`)
	hidden := if bracket > 0 {
		flat.named_variant_type_name(sum[..bracket], variant) + sum[bracket..]
	} else {
		flat.named_variant_type_name(sum, variant)
	}
	init := p.add_node_from(flat.Node{
		kind:           .struct_init
		value:          hidden
		children_start: fields_start
		children_count: flat.child_count(fields.len)
	}, lhs)
	return p.add_node_from(flat.Node{
		kind:           .cast_expr
		value:          sum
		children_start: p.add_child(init)
		children_count: 1
	}, lhs)
}

// named_variant_pattern_type maps a variant written in a type position, as in
// `e is Expr.Count` or a `match` branch, to its hidden struct name.
fn (mut p Parser) named_variant_pattern_type(type_name string) ?string {
	if p.prefs.is_fmt || p.is_translated || !type_name.contains('.') {
		return none
	}
	// Dots inside generic arguments belong to their payload types, not the variant.
	mut depth := 0
	mut dot := -1
	for i, c in type_name {
		if c == `[` {
			depth++
		} else if c == `]` {
			depth--
		} else if c == `.` && depth == 0 {
			dot = i
		}
	}
	if dot < 0 || !is_plain_capitalized_ident(type_name[dot + 1..]) {
		return none
	}
	owner := type_name[..dot]
	base := owner.all_before('[')
	args := owner[base.len..]
	parts := base.split('.')
	if (parts.len == 1 && p.is_named_variant_owner_name(parts[0]))
		|| (parts.len == 2 && parts[0] in p.imported_module_names
			&& !p.is_local_binding(parts[0]) && p.is_imported_named_variant_owner_name(parts[0], parts[1])) {
		return flat.named_variant_type_name(base, type_name[dot + 1..]) + args
	}
	return none
}

// named_variant_pattern_type_name also reads the variant after explicit type arguments.
fn (mut p Parser) named_variant_pattern_type_name() string {
	mut name := p.parse_type_name()
	if name.ends_with(']') && p.tok == .dot && p.peek() == .name {
		p.next()
		name += '.' + p.expect_name()
	}
	return name
}

// named_variant_match_pattern parses the optional payload binding of a variant
// pattern in a `match` branch, `Expr.Count(n)` or `Expr.Count(mut n)`, after the
// variant name. The binding is stored as a `.param` child of the pattern node.
fn (mut p Parser) named_variant_match_pattern(type_name string, start int) ?flat.NodeId {
	is_variant_spelling := type_name.contains('.')
	if p.prefs.is_fmt {
		if p.tok != .lpar || !is_variant_spelling {
			return none
		}
		// Keep the source shape, `Expr.Count(n)`, so vfmt prints it back unchanged.
		callee := p.match_type_pattern_node(type_name)
		p.next() // skip (
		mut args := []flat.NodeId{}
		for p.tok != .rpar && p.tok != .eof {
			arg_start := p.tok_pos
			is_mut := p.tok == .key_mut
			if is_mut {
				p.next()
			}
			arg := p.add_node(flat.Node{
				kind:   .ident
				value:  p.lit
				is_mut: is_mut
				pos:    token.new_span(p.cur_file_id, arg_start, p.tok_end)
			})
			p.next()
			args << arg
			if p.tok != .comma {
				break
			}
			p.next()
		}
		p.check(.rpar)
		mut children := []flat.NodeId{cap: args.len + 1}
		children << callee
		children << args
		return p.add_node(flat.Node{
			kind:           .call
			children_start: p.add_children(children)
			children_count: flat.child_count(children.len)
			pos:            p.span_to(start)
		})
	}
	hidden := p.named_variant_pattern_type(type_name) or {
		if p.tok == .lpar && is_variant_spelling && !p.is_translated {
			p.record_diagnostic_span('`${type_name}` is not a sum type variant that can bind a payload',
				start, p.prev_tok_end)
			p.skip_named_variant_binding()
		}
		return none
	}
	mut binding := flat.empty_node
	if p.tok == .lpar {
		binding_start := p.tok_pos
		p.next() // skip (
		is_mut := p.tok == .key_mut
		if is_mut {
			p.next()
		}
		if p.tok == .name && p.peek() == .rpar {
			name := p.lit
			name_pos := p.current_pos()
			p.next()
			p.next() // skip )
			if name != '_' {
				binding = p.add_node(flat.Node{
					kind:   .param
					value:  name
					is_mut: is_mut
					pos:    name_pos
				})
			}
		} else {
			p.record_diagnostic_span('invalid payload binding for `${type_name}`: expected a single name, e.g. `${type_name}(value)`',
				binding_start, p.tok_end)
			p.skip_named_variant_binding_rest()
		}
	}
	if dot := top_level_dot_index(hidden) {
		base_id := p.add_val(.ident, hidden[..dot])
		mut children := [base_id]
		if int(binding) >= 0 {
			children << binding
		}
		return p.add_node(flat.Node{
			kind:           .selector
			value:          hidden[dot + 1..]
			children_start: p.add_children(children)
			children_count: flat.child_count(children.len)
			pos:            p.span_to(start)
		})
	}
	mut children := []flat.NodeId{}
	if int(binding) >= 0 {
		children << binding
	}
	return p.add_node(flat.Node{
		kind:           .ident
		value:          hidden
		children_start: p.add_children(children)
		children_count: flat.child_count(children.len)
		pos:            p.span_to(start)
	})
}

// skip_named_variant_binding skips a `( ... )` group that follows a pattern.
fn (mut p Parser) skip_named_variant_binding() {
	if p.tok != .lpar {
		return
	}
	p.next()
	p.skip_named_variant_binding_rest()
}

fn (mut p Parser) skip_named_variant_binding_rest() {
	mut depth := 1
	for p.tok != .eof && p.tok != .lcbr {
		if p.tok == .lpar {
			depth++
		} else if p.tok == .rpar {
			depth--
			if depth == 0 {
				p.next()
				return
			}
		}
		p.next()
	}
}

// is_expr_type_name parses the type of an `is` check. A sum type variant,
// `e is Expr.Count`, checks for its hidden struct.
fn (mut p Parser) is_expr_type_name() string {
	type_start := p.tok_pos
	type_name := p.named_variant_pattern_type_name()
	hidden := p.named_variant_pattern_type(type_name) or { return type_name }
	if p.tok == .lpar {
		p.record_diagnostic_span('cannot bind the payload of `${type_name}` in an `is` check; use `match` to bind it',
			type_start, p.tok_end)
		p.skip_named_variant_binding()
	}
	return hidden
}

// check_named_variant_bindings_in_multi_pattern_branch rejects payload bindings
// in a branch with several patterns, `Expr.IntLit(n), Expr.Count(n) {`: each
// pattern would have to bind a value of its own payload type.
fn (mut p Parser) check_named_variant_bindings_in_multi_pattern_branch(conds []flat.NodeId) {
	if p.prefs.is_fmt {
		return
	}
	for cond_id in conds {
		cond := p.a.node(cond_id)
		if cond.kind !in [.ident, .selector] {
			continue
		}
		for i in 0 .. cond.children_count {
			child := p.a.child_node(cond, i)
			if child.kind == .param {
				p.record_diagnostic_span('cannot bind the payload `${child.value}` in a match branch with several patterns; use a separate branch for each payload',
					int(child.pos.offset), int(child.pos.end))
				return
			}
		}
	}
}

// declare_named_variant_bindings makes the payload bindings of a branch's
// patterns local bindings of the branch body, so closures can capture them.
fn (mut p Parser) declare_named_variant_bindings(conds []flat.NodeId) {
	for cond_id in conds {
		cond := p.a.node(cond_id)
		if p.prefs.is_fmt && cond.kind == .call {
			for i in 1 .. cond.children_count {
				p.declare_local_binding(p.a.child_node(cond, i).value)
			}
			continue
		}
		if cond.kind !in [.ident, .selector] {
			continue
		}
		for i in 0 .. cond.children_count {
			child := p.a.child_node(cond, i)
			if child.kind == .param {
				p.declare_local_binding(child.value)
			}
		}
	}
}
