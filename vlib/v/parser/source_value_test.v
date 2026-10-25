module parser

import v.flat
import v.pref
import v.token

fn test_parser_type_operands_are_indexed_independently_of_annotations() {
	mut p := Parser.new(pref.new_preferences())
	a := p.parse_text('operand.v', 'module main
struct Item { field int }
fn main() {
_ = Item{}
_ = []int{}
_ = map[string]int{}
_ = sizeof(int)
_ = typeof[int]()
_ = __offsetof(Item, field)
}
')
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut seen := map[flat.NodeKind]bool{}
	for node in a.nodes {
		if node.kind !in [.struct_init, .array_init, .map_init, .sizeof_expr, .typeof_expr,
			.offsetof_expr] {
			continue
		}
		seen[node.kind] = true
		assert node.value == ''
		assert node.source_value != 0
		assert node.source_value_id() == flat.source_type_id(node.value_text())
		if node.kind == .offsetof_expr {
			assert node.source_value_id() == flat.source_type_id('Item')
			assert node.typ == flat.source_type_id('field')
		}
	}
	for kind in [flat.NodeKind.struct_init, .array_init, .map_init, .sizeof_expr, .typeof_expr,
		.offsetof_expr] {
		assert seen[kind]
	}
}

fn test_parser_type_only_generic_arguments_keep_their_original_ids() {
	for spelling in ['int', '?Item', '&&Item', '[3]int', 'Item[int]'] {
		mut p := Parser.new(pref.new_preferences())
		mut files := token.FileSet.new()
		file := files.add_file('argument.v', spelling.len)
		p.s.init(file, spelling)
		p.next()
		id := p.parse_comptime_type_arg()
		node := p.a.node(id)
		assert node.kind == .ident
		assert node.value == ''
		assert node.source_value_id() == flat.source_type_id(spelling)
		assert p.type_expr_name(id) == node.source_value_id()
		assert p.diagnostics.len == 0, p.diagnostics.str()
	}
}

fn test_parser_sum_variants_store_original_type_ids() {
	for formatter in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.is_fmt = formatter
		mut p := Parser.new(prefs)
		a := p.parse_text('variants.v', 'module main\ntype Number = int | f64\n')
		assert p.diagnostics.len == 0, p.diagnostics.str()
		decls := a.nodes.filter(it.kind == .type_decl)
		assert decls.len == 1
		decl := decls[0]
		assert decl.children_count == 2
		for i, spelling in ['int', 'f64'] {
			variant := a.child_node(&decl, i)
			assert variant.kind == .ident
			assert variant.value == ''
			assert variant.source_value_id() == flat.source_type_id(spelling)
		}
	}
}

fn test_parser_generic_argument_index_keeps_value_identifiers() {
	mut p := Parser.new(pref.new_preferences())
	int_id := p.add_val(.ident, 'int')
	assert p.generic_call_type_arg_id(int_id) == int_id
	assert p.a.node(int_id).value == ''
	assert p.a.node(int_id).source_value_id() == flat.source_type_int
	value_id := p.add_val(.ident, 'i')
	assert p.generic_call_type_arg_id(value_id) == value_id
	assert p.a.node(value_id).value == 'i'
	assert p.a.node(value_id).source_value == 0
}

fn test_parser_builtin_match_patterns_use_type_operands() {
	mut p := Parser.new(pref.new_preferences())
	a := p.parse_text('pattern.v', 'module main
type Number = int | f64
fn check(n Number) { match n { int {} f64 {} } }
')
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut branches := 0
	for node in a.nodes {
		if node.kind != .match_branch || node.children_count == 0 {
			continue
		}
		pattern := a.child_node(&node, 0)
		assert pattern.kind == .ident
		assert pattern.value == ''
		assert pattern.source_value_id() in [flat.source_type_int, flat.source_type_f64]
		branches++
	}
	assert branches == 2
}

fn test_parser_deprecated_generic_type_argument_reads_indexed_spelling() {
	mut p := Parser.new(pref.new_preferences())
	mut files := token.FileSet.new()
	file := files.add_file('legacy_argument.v', 5)
	p.s.init(file, 'usize')
	p.next()
	id := p.a.add_node(flat.Node{
		kind:         .ident
		source_value: flat.source_type_usize
		pos:          p.current_pos()
	})
	canonical := p.canonical_generic_type_arg(id)
	assert canonical != id
	assert p.a.node(id).source_value_id() == flat.source_type_usize
	assert p.a.node(canonical).value == ''
	assert p.a.node(canonical).source_value_id() == flat.source_type_uint
	assert p.diagnostics.len == 1
	assert p.diagnostics[0].message == '`usize` is deprecated; use `uint` instead'
}

fn test_parser_local_type_ids_keep_scope_precedence_and_exact_suffixes() {
	mut p := Parser.new(pref.new_preferences())
	p.cur_file = 'aliases.v'
	p.cur_module = 'main'
	p.push_local_type_scope('outer')
	outer := p.declare_local_type_name('Item', 'outer')
	item_id := flat.source_type_id('Item')
	assert p.resolve_local_type_id(item_id) == flat.source_type_id(outer)
	p.push_local_type_scope('inner')
	inner := p.declare_local_type_name('Item', 'inner')
	inner_id := flat.source_type_id(inner)
	assert p.resolve_local_type_id(item_id) == inner_id
	assert p.resolve_local_type_id(flat.source_type_id('Item[ int,  ?u8 ]')) == flat.source_type_id(inner + '[ int,  ?u8 ]')
	for source in ['other.Item', 'Missing[ int ]', ' Item '] {
		source_id := flat.source_type_id(source)
		assert p.resolve_local_type_id(source_id) == source_id
	}
	argument := p.add_node(flat.Node{ kind: .ident, source_value: item_id })
	resolved_argument := p.generic_call_type_arg_id(argument)
	assert p.a.node(resolved_argument).source_value_id() == inner_id
	assert p.a.node(resolved_argument).value == ''
	p.pop_local_type_scope()
	assert p.resolve_local_type_id(item_id) == flat.source_type_id(outer)
	p.cur_file = 'other_aliases.v'
	assert p.resolve_local_type_id(item_id) == item_id
	p.cur_file = 'aliases.v'
	p.cur_module = 'other'
	assert p.resolve_local_type_id(item_id) == item_id
}

fn test_parser_local_type_ids_distinguish_empty_aliases_from_missing_aliases() {
	mut p := Parser.new(pref.new_preferences())
	p.push_local_type_scope('outer')
	p.declare_local_type_name('Item', 'outer')
	p.push_local_type_scope('inner')
	p.local_type_names.set_id(p.local_type_key_for_scope('inner', 'Item'), 0)
	assert p.resolve_local_type_id(flat.source_type_id('Item')) == 0
	assert p.resolve_local_type_id(flat.source_type_id('Item[ int ]')) == flat.source_type_id('[ int ]')
	missing := flat.source_type_id('Missing')
	assert p.resolve_local_type_id(missing) == missing
	assert p.resolve_local_type_id(0) == 0
}
