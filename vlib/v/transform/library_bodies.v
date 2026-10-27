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
// V_CHECK_LIBRARY_BODIES=all keeps every body, as before. V_DIAGNOSTICS_TRACE tells
// how many instances were cloned without their bodies.
//
// When the check is asked to (TypeChecker.check_program_lib_instances), an
// instance that the program's own code asks for, as `arrays.sum(foos)` asks for
// `arrays.sum[Foo]`, keeps its body too, and is checked as an instance of the
// program's generics is (see TypeChecker.check_concrete_instance_members): its
// types are the program's choice. Its body is not lowered either, so the
// instances that it asks for in turn are the library's, and are not checked.
struct LibraryBodies {
mut:
	ready         bool
	keep_all      string               // why every library instance keeps its body, if one does
	receivers     map[u32]bool         // program types with a method that has type parameters of its own
	methods       map[u32][]string     // a type -> the keys of its methods
	harmless      map[u32]bool         // types that reach none of the above
	program_specs GenericFnSpecBoolMap // the keys of the library instances that the program's code asks for
	request_file  string               // the file of the call that asks for an instance, when the scan knows it
}

// note_library_instance_request remembers, in a check of the instances that the
// program asks for, that the instance `key` of the library generic `decl` is
// asked for by the program's own code (see LibraryBodies): the call that asks
// for it is in `request_file`, or, when the scan does not tell, in the file of
// the body that is being lowered.
fn (mut t Transformer) note_library_instance_request(decl GenericFnDecl, key GenericFnSpecKey) {
	if isnil(t.tc) || !t.tc.check_concrete_generic_bodies || !t.tc.check_program_lib_instances
		|| decl.file in t.tc.diagnostic_files {
		return
	}
	request_file := if t.library_bodies.request_file != '' {
		t.library_bodies.request_file
	} else {
		t.cur_file
	}
	if request_file in t.tc.diagnostic_files {
		t.library_bodies.program_specs.set(key, true)
	}
}

// library_instance_needs_body reports whether a check clones the body of an
// instance of a library generic for the type arguments `args`.
fn (mut t Transformer) library_instance_needs_body(args_ids []u32) bool {
	if !t.library_bodies.ready {
		t.prepare_library_bodies()
	}
	if t.library_bodies.keep_all != '' {
		return true
	}
	for arg_source_id in args_ids {
		arg_source_id_2 := arg_source_id

		if t.library_type_reaches_program(arg_source_id_2) {
			return true
		}
	}
	return false
}

fn (mut t Transformer) prepare_library_bodies() {
	t.library_bodies.ready = true
	if os.getenv('V_CHECK_LIBRARY_BODIES') == 'all' {
		t.keep_library_bodies('V_CHECK_LIBRARY_BODIES=all asks for them')
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
		t.library_bodies.receivers[library_type_key(flat.source_type_id(receiver), decl.module)] = true
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
		if flat.source_type_id(receiver) in t.tc.structs || t.tc.sum_types.contains(flat.source_type_id(receiver)) || t.tc.type_aliases.contains(flat.source_type_id(receiver))
			|| flat.source_type_id(receiver) in t.tc.enum_names {
			t.library_bodies.methods[flat.source_type_id(receiver)] << key
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
	decl_id := flat.source_type_id(decl.node.value)
	receiver_id := if flat.source_type_shape(decl_id).has(.dotted) {
		flat.source_type_syntax(decl_id).qualifier()
	} else {
		decl_id
	}
	t.collect_generic_param_names_from_type_id(receiver_id, decl.module, mut params)
	for i in 0 .. decl.node.children_count {
		child := t.a.child_node(&decl.node, i)
		if child.kind == .param {
			t.collect_generic_param_names_from_type_id(child.typ, decl.module, mut params)
			break
		}
	}
	return params.len > 0
}

// library_type_key spells the type `name` declared in `module_name` as the
// checker's maps key it: bare in `main` and `builtin`, qualified elsewhere.
fn library_type_key(name_id u32, module_name string) u32 {
	if module_name in ['', 'main', 'builtin'] || flat.source_type_shape(name_id).has(.dotted) {
		return name_id
	}
	return flat.source_type_composition_two(.qualified, flat.source_type_id(module_name), name_id)
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
fn (mut t Transformer) library_type_reaches_program(typ_id u32) bool {
	root := library_type_text(typ_id)
	if t.library_bodies.harmless[root] { return false }
	mut seen := map[u32]bool{}
	mut pending := [root]
	for pending.len > 0 {
		text := pending.pop()
		if text == 0 || seen[text] || t.library_bodies.harmless[text] { continue }
		seen[text] = true
		pending << t.library_type_parts(text) or { return true }
	}
	for text, _ in seen { t.library_bodies.harmless[text] = true }
	return false
}

fn library_type_text(typ_id u32) u32 {
	text := transform_trimmed_type_id(typ_id)
	return if flat.source_type_query(text).has(.main_prefix) {
		flat.source_type_syntax(text).main_base()
	} else {
		text
	}
}

// library_type_parts returns the types that a value of the type `text` gives a
// library body, or none when the type keeps the body (see LibraryBodies) or is
// not known here.
fn (mut t Transformer) library_type_parts(text_id u32) ?[]u32 {
	shape := flat.source_type_shape(text_id)
	syntax := flat.source_type_syntax(text_id)
	if shape.has(.mutable_type) || shape.has(.shared_type) || shape.has(.atomic_type)
		|| shape.has(.variadic) || shape.has(.pointer) || shape.has(.optional)
		|| shape.has(.array_type) || shape.has(.channel_type) || shape.has(.thread_type) {
		return [library_type_text(syntax.raw_base())]
	}
	if shape.has(.map_type) {
		if !syntax.bracket_closed() || syntax.bracket_suffix() == 0 { return none }
		return [library_type_text(syntax.bracket_raw_inner()),
			library_type_text(syntax.bracket_suffix())]
	}
	if shape.has(.leading_bracket) {
		if !syntax.bracket_closed() || syntax.bracket_suffix() == 0 { return none }
		return [library_type_text(syntax.bracket_suffix())]
	}
	if shape.has(.function_compact) || shape.has(.function_spaced) {
		if !syntax.function_closed() { return none }
		mut parts := []u32{cap: syntax.function_params().len + 1}
		for param in syntax.function_params() {
			parts << library_type_text(flat.source_type_syntax(param).generic_parameter_payload())
		}
		if syntax.function_result() != 0 { parts << library_type_text(syntax.function_result()) }
		return parts
	}
	if syntax.tuple_like() {
		return syntax.tuple_components().map(library_type_text(it))
	}
	query := flat.source_type_query(text_id)
	if query.has(.c_prefix) || query.has(.js_prefix) || types.is_builtin_type_name_id(text_id) {
		return []u32{}
	}
	is_app := syntax.generic_app_valid()
	name_id := if is_app { syntax.generic_app_base() } else { text_id }
	if name_id in [transform_identity_ierror, transform_identity_builtin_ierror] || t.generic_checker_interface_name_known(name_id)
		|| name_id in t.library_bodies.receivers {
		return none
	}
	// Checker declaration tables still use declaration names as keys.
	mut parts := []u32{}
	if is_app {
		// A generic type of the program keeps the body; of a library one, what it
		// holds of the program comes in through its type arguments.
		file := t.tc.struct_files[name_id] or { return none }
		if file in t.tc.diagnostic_files { return none }
		for arg in syntax.generic_app_args() { parts << library_type_text(arg) }
	}
	if fields := t.tc.structs[text_id] {
		for field in fields { parts << library_type_text(field.typ.source_type_id()) }
	} else if is_app {
		fields := t.tc.structs[name_id] or { return none }
		params := t.tc.struct_generic_params[name_id] or { []string{} }
		args_ids := syntax.generic_app_args()
		for field in fields {
			parts << library_type_text(substitute_generic_type_text_with_params(field.typ.source_type_id(),
				args_ids, params))
		}
	} else if variants_source_ids := t.tc.sum_types.type_ids(name_id) {
		for variant_source_id in variants_source_ids {
			parts << library_type_text(variant_source_id)
		}
	} else if target_source_id := t.tc.type_aliases.type_id(name_id) {
		parts << library_type_text(target_source_id)
	} else if name_id !in t.tc.enum_names {
		return none
	}
	for key in t.library_bodies.methods[name_id] {
		// What a method with type parameters of its own returns depends on the
		// types a library body passes it, which come in through its arguments.
		if key in t.tc.fn_generic_params { continue }
		if ret := t.tc.fn_ret_types[key] { parts << library_type_text(ret.source_type_id()) }
		for param in t.tc.fn_param_types[key] or { []types.Type{} } {
			parts << library_type_text(param.source_type_id())
		}
	}
	return parts
}
