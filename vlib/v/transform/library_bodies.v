module transform

import os
import v.flat
import v.types

// LibraryBodies decides which instances of library generics a check clones
// without their bodies.
//
// A check (-check) checks the instances of the program's generics only: the
// clone of a library generic is neither lowered nor checked (check_skips_body).
// Its body still costs its clone and every later scan over it, and the calls in
// it ask for more library instances: in a program that uses json2 those clones
// are most of the check of the instances. A library body matters to the check
// only where it asks for an instance of the program, and the program's code is
// out of its reach but for the types it is given. So such a clone keeps only
// its parameters, which its signature needs, unless one of its type arguments
// reaches, through its elements, fields, variants, alias target or the
// parameters and results of its methods:
//
// - an interface: a call on one reaches the methods of every type it holds;
// - a generic type of the program: its methods and operators are instances;
// - a type of the program with a method that has type parameters of its own.
//
// Every library instance keeps its body when the program has
//
// - a method of a generic type: a call on an interface in a library body asks
//   for it, with no type argument that names the type;
// - a `$for` in a function without type parameters: a call on an interface in a
//   library body can get that function lowered, and only then does the `$for`
//   ask for the instances in it;
// - a module that a library file belongs to or imports: that code can name the
//   program's generics.
//
// V_CHECK_LIBRARY_BODIES=1 keeps every body, as before. V_DIAGNOSTICS_TRACE tells
// how many instances were cloned without their bodies.
struct LibraryBodies {
mut:
	ready     bool
	keep_all  string              // why every library instance keeps its body, if one does
	receivers map[string]bool     // program types with a method that has type parameters of its own
	methods   map[string][]string // a type -> the keys of its methods
	harmless  map[string]bool     // types that reach none of the above
}

// library_instance_needs_body reports whether a check clones the body of an
// instance of a library generic for the type arguments `args`.
fn (mut t Transformer) library_instance_needs_body(args []string) bool {
	if !t.library_bodies.ready {
		t.prepare_library_bodies()
	}
	if t.library_bodies.keep_all != '' {
		return true
	}
	for arg in args {
		if t.library_type_reaches_program(arg) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) prepare_library_bodies() {
	t.library_bodies.ready = true
	if os.getenv('V_CHECK_LIBRARY_BODIES') == '1' {
		t.keep_library_bodies('V_CHECK_LIBRARY_BODIES=1 asks for them')
		return
	}
	for _, decl in t.cached_generic_fn_decls() {
		if decl.file !in t.tc.diagnostic_files || !decl.node.value.contains('.') {
			continue
		}
		if t.generic_decl_receiver_is_generic(decl) {
			t.keep_library_bodies('`${decl.key}` is a method of a generic type of the program')
			return
		}
		receiver := generic_fn_decl_base_value(decl.node.value).all_before_last('.').trim_left('&')
		t.library_bodies.receivers[library_type_key(receiver, decl.module)] = true
	}
	if reason := t.program_comptime_for() {
		t.keep_library_bodies(reason)
		return
	}
	if reason := t.library_file_of_program_module() {
		t.keep_library_bodies(reason)
		return
	}
	for key, _ in t.tc.fn_ret_types {
		dot := key.last_index_u8(`.`)
		if dot <= 0 {
			continue
		}
		receiver := key[..dot]
		if receiver in t.tc.structs || receiver in t.tc.sum_types || receiver in t.tc.type_aliases
			|| receiver in t.tc.enum_names {
			t.library_bodies.methods[receiver] << key
		}
	}
}

fn (mut t Transformer) keep_library_bodies(reason string) {
	t.library_bodies.keep_all = reason
	t.tc.library_bodies_kept_for = reason
}

// generic_decl_receiver_is_generic reports whether the method `decl` belongs to
// a generic type (`fn (b Box[T]) name()`), rather than having type parameters of
// its own only (`fn (r Runner) run[T](x T)`).
fn (mut t Transformer) generic_decl_receiver_is_generic(decl GenericFnDecl) bool {
	mut params := []string{}
	t.collect_generic_param_names_from_type(decl.node.value.all_before_last('.'), decl.module, mut
		params)
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind == .param {
			t.collect_generic_param_names_from_type(child.typ, decl.module, mut params)
			break
		}
	}
	return params.len > 0
}

// library_type_key spells the type `name` declared in `module_name` as the
// checker's maps key it: bare in `main` and `builtin`, qualified elsewhere.
fn library_type_key(name string, module_name string) string {
	if module_name in ['', 'main', 'builtin'] || name.contains('.') {
		return name
	}
	return '${module_name}.${name}'
}

// program_comptime_for tells where a function of the program without type
// parameters has a `$for`, if one does.
fn (mut t Transformer) program_comptime_for() ?string {
	mut file := ''
	mut module_name := ''
	for idx in t.tc.top_level_idx {
		node := t.a.nodes[idx]
		if node.kind == .file {
			file = node.value
			module_name = ''
		} else if node.kind == .module_decl {
			module_name = node.value
		} else if node.kind == .fn_decl && file in t.tc.diagnostic_files
			&& !t.fn_decl_has_unresolved_generics(node, module_name)
			&& t.subtree_has_comptime_for(node) {
			return 'a `\$for` in `${node.value}`'
		}
	}
	return none
}

fn (t &Transformer) subtree_has_comptime_for(root flat.Node) bool {
	mut pending := []flat.NodeId{cap: 64}
	for i in 0 .. root.children_count {
		pending << t.a.child(&root, i)
	}
	for pending.len > 0 {
		id := pending.pop()
		if int(id) < 0 || int(id) >= t.a.nodes.len {
			continue
		}
		node := t.a.nodes[int(id)]
		if node.kind == .comptime_for {
			return true
		}
		for i in 0 .. node.children_count {
			pending << t.a.child(&node, i)
		}
	}
	return false
}

// library_file_of_program_module tells which library file belongs to a module of
// the program or imports one, if one does: its code can name the program's
// generics. Modules are matched by their last name, which can only keep more
// bodies than needed.
fn (t &Transformer) library_file_of_program_module() ?string {
	mut modules := map[string]bool{}
	for file, _ in t.tc.diagnostic_files {
		name := t.tc.file_modules[file] or { continue }
		if name !in ['', 'main'] {
			modules[name.all_after_last('.')] = true
		}
	}
	if modules.len == 0 {
		return none
	}
	for file, name in t.tc.file_modules {
		if file !in t.tc.diagnostic_files && name.all_after_last('.') in modules {
			return '`${file}` is in the module `${name}` of the program'
		}
	}
	for key, imported in t.tc.file_imports {
		file := key.all_before('\n')
		if file !in t.tc.diagnostic_files && imported.all_after_last('.') in modules {
			return '`${file}` imports the module `${imported}` of the program'
		}
	}
	return none
}

// library_type_reaches_program reports whether a library body given a value of
// the type `typ` can reach, through it, what keeps the body (see LibraryBodies).
fn (mut t Transformer) library_type_reaches_program(typ string) bool {
	root := library_type_text(typ)
	if t.library_bodies.harmless[root] {
		return false
	}
	mut seen := map[string]bool{}
	mut pending := [root]
	for pending.len > 0 {
		text := pending.pop()
		if text.len == 0 || seen[text] || t.library_bodies.harmless[text] {
			continue
		}
		seen[text] = true
		pending << t.library_type_parts(text) or { return true }
	}
	for text, _ in seen {
		t.library_bodies.harmless[text] = true
	}
	return false
}

fn library_type_text(typ string) string {
	text := typ.trim_space()
	return if text.starts_with('main.') { text['main.'.len..] } else { text }
}

// library_type_parts returns the types that a value of the type `text` gives a
// library body, or none when the type keeps the body (see LibraryBodies) or is
// not known here.
fn (mut t Transformer) library_type_parts(text string) ?[]string {
	for prefix in ['mut ', 'shared ', 'atomic ', '...', '&', '?', '!', '[]', 'chan ', 'thread '] {
		if text.starts_with(prefix) {
			return [library_type_text(text[prefix.len..])]
		}
	}
	if text.starts_with('map[') {
		end := generic_matching_bracket(text, 3)
		if end + 1 >= text.len {
			return none
		}
		return [library_type_text(text[4..end]), library_type_text(text[end + 1..])]
	}
	if text.starts_with('[') {
		end := generic_matching_bracket(text, 0)
		if end + 1 >= text.len {
			return none
		}
		return [library_type_text(text[end + 1..])]
	}
	if text.starts_with('fn(') || text.starts_with('fn (') {
		params, ret := fn_type_text_parts(text) or { return none }
		mut parts := params.map(library_type_text(generic_fn_type_param_payload(it)))
		if ret.len > 0 {
			parts << library_type_text(ret)
		}
		return parts
	}
	if text.starts_with('(') && text.ends_with(')') {
		return split_generic_args(text[1..text.len - 1]).map(library_type_text(it))
	}
	if text.starts_with('C.') || text.starts_with('JS.') || types.is_builtin_type_name(text) {
		return []string{}
	}
	base, args, is_app := generic_app_parts(text)
	name := if is_app { base } else { text }
	if name in ['IError', 'builtin.IError'] || name in t.tc.interface_names
		|| name in t.library_bodies.receivers {
		return none
	}
	mut parts := []string{}
	if is_app {
		// A generic type of the program keeps the body; of a library one, what it
		// holds of the program comes in through its type arguments.
		file := t.tc.struct_files[base] or { return none }
		if file in t.tc.diagnostic_files {
			return none
		}
		for arg in args {
			parts << library_type_text(arg)
		}
	}
	if fields := t.tc.structs[text] {
		for field in fields {
			parts << library_type_text(field.typ.name())
		}
	} else if is_app {
		fields := t.tc.structs[base] or { return none }
		params := t.tc.struct_generic_params[base] or { []string{} }
		for field in fields {
			parts << library_type_text(substitute_generic_type_text_with_params(field.typ.name(),
				args, params))
		}
	} else if variants := t.tc.sum_types[name] {
		for variant in variants {
			parts << library_type_text(variant)
		}
	} else if target := t.tc.type_aliases[name] {
		parts << library_type_text(target)
	} else if name !in t.tc.enum_names {
		return none
	}
	for key in t.library_bodies.methods[name] {
		// What a method with type parameters of its own returns depends on the
		// types a library body passes it, which come in through its arguments.
		if key in t.tc.fn_generic_params {
			continue
		}
		if ret := t.tc.fn_ret_types[key] {
			parts << library_type_text(ret.name())
		}
		for param in t.tc.fn_param_types[key] or { []types.Type{} } {
			parts << library_type_text(param.name())
		}
	}
	return parts
}
