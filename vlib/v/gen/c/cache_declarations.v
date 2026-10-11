module c

import strings
import v.flat
import v.types

// set_cache_const_modules assigns constant storage to the modules linked as objects.
pub fn (mut g FlatGen) set_cache_const_modules(modules []string, source_modules []string) {
	g.cache_const_modules.clear()
	g.cache_const_source_modules.clear()
	for name in modules {
		g.cache_const_modules[name] = true
	}
	for name in source_modules {
		g.cache_const_source_modules[name] = true
	}
}

fn (mut g FlatGen) emit_cache_owned_const(name string, val_id flat.NodeId, owner string, typ types.Type, ct string, cname string) {
	declaration := if fixed := array_fixed_type(default_init_unalias_type(typ)) {
		elem, dims := g.fixed_array_decl_parts(fixed)
		'extern ${elem} ${cname}${dims};'
	} else {
		'extern ${ct} ${cname};'
	}
	if g.cache_decl_demand {
		g.cache_const_declarations[cname] = declaration
		return
	}
	g.writeln(declaration)
	if (g.const_files[name] or { '' }).ends_with('.vh') {
		return
	}
	// Lower once in the owning module. The public declaration never evaluates
	// the initializer of a constant restored from a module interface.
	old_sb := g.sb
	old_line_start := g.line_start
	g.sb = strings.new_builder(128)
	g.line_start = true
	// Every source constant needs stable storage even if only a future program
	// takes its address. Keep scalar literal values in interfaces for folding.
	primary := g.const_primary_name(name)
	had_storage := g.fixed_storage_consts[primary]
	base := default_init_unalias_type(typ)
	needs_scalar_storage := base is types.Primitive || base is types.Enum || base is types.Char
		|| base is types.Rune || base is types.ISize || base is types.USize
	if needs_scalar_storage {
		g.fixed_storage_consts[primary] = true
	}
	g.cache_const_modules.delete(owner)
	g.emit_const(name, val_id)
	g.cache_const_modules[owner] = true
	if needs_scalar_storage && !had_storage {
		g.fixed_storage_consts.delete(primary)
	}
	definition := g.sb.str()
	unsafe { g.sb.free() }
	g.sb = old_sb
	g.line_start = old_line_start
	// Preserve literal folding in module functions while exporting addressable
	// storage for later programs. Undefine the macro only around its definition.
	mut scalar_macro := ''
	if needs_scalar_storage && !had_storage && ct != 'u8'
		&& !g.name_collides_with_struct_field(cname)
		&& definition.starts_with('static const ${ct} ${cname} = ') {
		value := definition.trim_space().all_after(' = ').all_before_last(';')
		scalar_macro = '#define ${cname} (${value})'
		g.writeln(scalar_macro)
	}
	// V enforces constness. Uniform writable C storage gives declaration-only
	// headers the same ABI for static and runtime aggregate initializers.
	mut lines := []string{}
	if scalar_macro.len > 0 {
		lines << '#undef ${cname}'
	}
	for line in definition.split_into_lines() {
		if line.starts_with('static const ') {
			lines << line['static const '.len..]
		} else if line.starts_with('const ') {
			lines << line['const '.len..]
		} else {
			lines << line
		}
	}
	if scalar_macro.len > 0 {
		lines << scalar_macro
	}
	g.cache_const_definitions[owner] << lines.join('\n') + '\n'
}

// cache_collect_declaration_refs records C identifiers, excluding comments and
// literals, so text printed by the program cannot request an unused declaration.
fn (mut g FlatGen) cache_collect_declaration_refs(source string) {
	for name, _ in cache_c_identifier_refs(source) {
		g.cache_decl_refs[name] = true
	}
}

fn cache_c_skip_literal_or_comment(source string, start int) int {
	mut i := start
	if source[i] in [`"`, `'`] {
		quote := source[i]
		i++
		for i < source.len && source[i] != quote {
			if source[i] == `\\` {
				i++
			}
			i++
		}
		return if i < source.len { i + 1 } else { source.len }
	}
	if i + 1 < source.len && source[i] == `/` && source[i + 1] == `/` {
		i += 2
		for i < source.len && source[i] != `\n` {
			i++
		}
		return i
	}
	if i + 1 < source.len && source[i] == `/` && source[i + 1] == `*` {
		i += 2
		for i + 1 < source.len && !(source[i] == `*` && source[i + 1] == `/`) {
			i++
		}
		return if i + 1 < source.len { i + 2 } else { source.len }
	}
	return start
}

fn cache_c_identifier_refs(source string) map[string]bool {
	mut refs := map[string]bool{}
	mut i := 0
	for i < source.len {
		next := cache_c_skip_literal_or_comment(source, i)
		if next > i {
			i = next
		} else if c_identifier_start(source[i]) {
			start := i
			i++
			for i < source.len && c_identifier_continue(source[i]) {
				i++
			}
			refs[source[start..i]] = true
		} else {
			i++
		}
	}
	return refs
}

struct CacheSupportDefinition {
	name  string
	start int
	end   int
}

fn cache_collect_support_refs(source string, start int, end int, definitions map[string][]int, mut refs map[string]bool, mut pending []string) {
	mut i := start
	for i < end {
		if source[i] in [`"`, `'`, `/`] {
			next := cache_c_skip_literal_or_comment(source, i)
			if next > i {
				i = next
				continue
			}
		}
		if !c_identifier_start(source[i]) {
			i++
			continue
		}
		word_start := i
		i++
		for i < end && c_identifier_continue(source[i]) { i++ }
		// These views stay inside the pruning call, while source remains alive.
		// Only support names enter the worklist; other identifiers need no copies.
		name := source.substr_unsafe(word_start, i)
		if name in definitions && !refs[name] {
			refs[name] = true
			pending << name
		}
	}
}

// cache_prune_generated_support keeps inline helpers and literal storage reached
// by a warm program. TinyCC still tokenizes unused inline bodies, so omit
// unreachable helpers and literal storage before sending C to the compiler.
fn cache_prune_generated_support(source string) string {
	inline_prefix := 'static inline '
	mut support := []CacheSupportDefinition{}
	mut definitions := map[string][]int{}
	mut i := 0
	mut depth := 0
	for i < source.len {
		if source[i] in [`"`, `'`, `/`] {
			next := cache_c_skip_literal_or_comment(source, i)
			if next > i {
				i = next
				continue
			}
		}
		if depth == 0 && source[i] == `s` && (i == 0 || source[i - 1] == `\n`) {
			mut end := i
			for end < source.len && source[end] != `\n` { end++ }
			line := source.substr_unsafe(i, end)
			literal_prefix := if line.starts_with('static const string _v3_lit_') {
				'static const string '
			} else if line.starts_with('static string _v3_lit_') {
				'static string '
			} else {
				''
			}
			if literal_prefix.len > 0 && line.ends_with(';') {
				mut name_end := literal_prefix.len
				for name_end < line.len && c_identifier_continue(line[name_end]) { name_end++ }
				mut value_start := name_end
				for value_start < line.len && line[value_start] in [` `, `\t`] { value_start++ }
				if value_start < line.len && line[value_start] == `=` {
					name := line.substr_unsafe(literal_prefix.len, name_end)
					definitions[name] << support.len
					support << CacheSupportDefinition{ name: name, start: i, end: end }
					i = end
					continue
				}
			}
			open := line.index_u8(`(`)
			brace := line.index_u8(`{`)
			if line.starts_with(inline_prefix) && open > 0 && brace > open {
				mut name_start := open
				for name_start > 0 && c_identifier_continue(line[name_start - 1]) {
					name_start--
				}
				if name_start < open {
					name := line.substr_unsafe(name_start, open)
					mut pos := i + brace + 1
					mut body_depth := 1
					for pos < source.len && body_depth > 0 {
						if source[pos] in [`"`, `'`, `/`] {
							after := cache_c_skip_literal_or_comment(source, pos)
							if after > pos {
								pos = after
								continue
							}
						}
						if source[pos] == `{` { body_depth++ }
						if source[pos] == `}` { body_depth-- }
						pos++
					}
					if body_depth != 0 {
						return source.clone()
					}
					definitions[name] << support.len
					support << CacheSupportDefinition{ name: name, start: i, end: pos }
					i = pos
					continue
				}
			}
		}
		if source[i] == `{` { depth++ }
		if source[i] == `}` { depth-- }
		i++
	}
	if support.len == 0 {
		return source.clone()
	}
	mut refs := map[string]bool{}
	mut pending := []string{}
	mut offset := 0
	for definition in support {
		cache_collect_support_refs(source, offset, definition.start, definitions, mut refs, mut pending)
		offset = definition.end
	}
	cache_collect_support_refs(source, offset, source.len, definitions, mut refs, mut pending)
	for pending.len > 0 {
		name := pending.pop()
		// A helper can have separate native and portable definitions. Keep both
		// branches and their dependencies, leaving the preprocessor in charge.
		for index in definitions[name] {
			definition := support[index]
			cache_collect_support_refs(source, definition.start, definition.end, definitions, mut refs, mut pending)
		}
	}
	mut out := strings.new_builder(source.len)
	offset = 0
	for definition in support {
		out.write_string(source.substr_unsafe(offset, definition.start))
		if refs[definition.name] {
			out.write_string(source.substr_unsafe(definition.start, definition.end))
		}
		offset = definition.end
	}
	out.write_string(source.substr_unsafe(offset, source.len))
	result := out.str()
	unsafe { out.free() }
	return result
}

fn (mut g FlatGen) prepare_cache_declaration_demand(fn_code string) {
	// A cold build publishes complete module objects. Only a build whose modules
	// are already linked may omit their private declarations from the program.
	if g.cache_const_source_modules.len > 0 {
		return
	}
	g.cache_decl_demand = true
	g.cache_collect_declaration_refs(fn_code)
	for segment in g.fn_segs {
		g.cache_collect_declaration_refs(segment)
	}
	for definition in g.callback_wrapper_defs {
		g.cache_collect_declaration_refs(definition)
	}
	for definition in g.spawn_wrapper_defs {
		g.cache_collect_declaration_refs(definition)
	}
}

fn (mut g FlatGen) cache_collect_support_declaration_refs() {
	old_sb := g.sb
	old_line_start := g.line_start
	g.sb = strings.new_builder(4096)
	g.line_start = true
	g.interface_method_stubs()
	g.parallel_interface_stubs = g.sb.str()
	g.cache_collect_declaration_refs(g.parallel_interface_stubs)
	unsafe { g.sb.free() }
	g.sb = strings.new_builder(4096)
	g.c_extern_forward_decls()
	g.cache_collect_declaration_refs(g.sb.str())
	unsafe { g.sb.free() }
	g.sb = strings.new_builder(4096)
	g.gen_global_declaration_block()
	g.parallel_global_decls = g.sb.str()
	g.cache_collect_declaration_refs(g.parallel_global_decls)
	for init in g.runtime_inits {
		g.cache_collect_declaration_refs(init)
	}
	unsafe { g.sb.free() }
	// These bodies are emitted after the declaration prefix. Collect their calls
	// before filtering cached prototypes, and retain the bodies for final output.
	g.sb = strings.new_builder(4096)
	if !g.skip_enum_autostr {
		if g.parallel_enum_str_defs.len == 0 {
			g.enum_str_defs()
			g.parallel_enum_str_defs = g.sb.str()
		}
		g.cache_collect_declaration_refs(g.parallel_enum_str_defs)
	}
	unsafe { g.sb.free() }
	g.sb = strings.new_builder(4096)
	if g.parallel_init_defs.len == 0 {
		g.gen_vinit()
		g.gen_vcleanup()
		g.parallel_init_defs = g.sb.str()
	}
	g.cache_collect_declaration_refs(g.parallel_init_defs)
	unsafe { g.sb.free() }
	g.sb = strings.new_builder(4096)
	g.forward_decls()
	g.parallel_forward_decls = g.sb.str()
	g.cache_collect_declaration_refs(g.parallel_forward_decls)
	unsafe { g.sb.free() }
	g.sb = strings.new_builder(4096)
	g.gen_pre_body_support_declarations()
	g.parallel_support_decls = g.sb.str()
	g.cache_collect_declaration_refs(g.parallel_support_decls)
	unsafe { g.sb.free() }
	g.sb = old_sb
	g.line_start = old_line_start
}

fn (mut g FlatGen) finish_cache_declaration_demand(const_code string) {
	if !g.cache_decl_demand {
		return
	}
	g.cache_collect_declaration_refs(const_code)
	for init in g.runtime_inits {
		g.cache_collect_declaration_refs(init)
	}
	for init in g.const_runtime_inits {
		g.cache_collect_declaration_refs(init)
	}
	// Program constant initializers can be the only callers of cached functions
	// or readers of module constants. Capture support only after lowering them.
	g.cache_collect_support_declaration_refs()
	g.cache_collect_declaration_refs(g.demanded_cache_constant_declarations())
	// Preparation may have seeded types from every cached signature before the
	// bodies established demand. Retain only the wrappers named by emitted code;
	// the selected signatures and field walk below restore their dependencies.
	for name in g.needed_optional_types.keys() {
		if !g.cache_decl_refs[name] {
			g.needed_optional_types.delete(name)
		}
	}
	g.decl_types_ready = false
	g.collect_declaration_signature_types()
	for init in g.runtime_inits {
		g.cache_collect_declaration_refs(init)
	}
	for init in g.const_runtime_inits {
		g.cache_collect_declaration_refs(init)
	}
	// Fn-pointer and optional typedefs also name their payload types.
	for encoded, used in g.used_fn_ptr_types {
		if used {
			g.cache_collect_declaration_refs(encoded)
		}
	}
	for _, payload in g.needed_optional_types {
		g.cache_collect_declaration_refs(payload)
	}
	mut seen := map[string]bool{}
	for typ in g.multi_return_types {
		g.cache_require_declaration_type(typ, mut seen)
	}
	for name, _ in g.tc.structs {
		if g.cache_struct_declaration_needed(name) {
			g.cache_require_declaration_type(types.Struct{ name: name }, mut seen)
		}
	}
	for name, _ in g.interfaces {
		g.cache_require_declaration_type(types.Interface{ name: name }, mut seen)
	}
	for name, _ in g.tc.sum_types {
		if g.cache_decl_refs[g.cname(name)] {
			g.cache_require_declaration_type(types.SumType{ name: name }, mut seen)
		}
	}
}

fn (g &FlatGen) demanded_cache_constant_declarations() string {
	mut names := g.cache_const_declarations.keys()
	names.sort()
	mut declarations := []string{cap: names.len}
	for name in names {
		if g.cache_decl_refs[name] {
			declarations << g.cache_const_declarations[name]
		}
	}
	return declarations.join('\n') + '\n'
}

fn (g &FlatGen) cache_struct_declaration_needed(name string) bool {
	ct := g.struct_cname(name)
	return g.cache_decl_refs[ct] || g.cache_decl_refs[ct.all_after_last(' ')]
}

fn (g &FlatGen) cache_function_declaration_needed(name string) bool {
	owner := g.tc.fn_type_modules[name] or { '' }
	return g.cache_decl_refs[g.cname(name)]
		|| g.cache_decl_refs[g.fn_c_name_in_module(owner, name)]
}

fn (mut g FlatGen) cache_require_declaration_type(typ types.Type, mut seen map[string]bool) {
	key := typ.name()
	if seen[key] {
		return
	}
	seen[key] = true
	g.collect_declaration_signature_type(typ)
	g.cache_collect_declaration_refs(g.tc.c_type(typ))
	match typ {
		types.Struct {
			g.cache_decl_refs[g.struct_cname(typ.name)] = true
			for field in g.tc.structs[typ.name] or { []types.StructField{} } {
				g.cache_require_declaration_type(field.typ, mut seen)
			}
		}
		types.Interface {
			for field in g.tc.interface_field_list(typ.name) {
				g.cache_require_declaration_type(field.typ, mut seen)
			}
		}
		types.SumType {
			for variant in g.tc.sum_types[typ.name] or { []string{} } {
				g.cache_require_declaration_type(g.tc.parse_type(variant), mut seen)
			}
		}
		types.Pointer, types.OptionType, types.ResultType, types.Alias {
			g.cache_require_declaration_type(typ.base_type, mut seen)
		}
		types.Array, types.ArrayFixed, types.Channel {
			g.cache_require_declaration_type(typ.elem_type, mut seen)
		}
		types.Map {
			g.cache_require_declaration_type(typ.key_type, mut seen)
			g.cache_require_declaration_type(typ.value_type, mut seen)
		}
		types.FnType {
			g.cache_require_declaration_type(typ.return_type, mut seen)
			for param in typ.params {
				g.cache_require_declaration_type(param, mut seen)
			}
		}
		types.MultiReturn {
			for item in typ.types {
				g.cache_require_declaration_type(item, mut seen)
			}
		}
		else {}
	}
}

fn (g &FlatGen) cache_const_init_name(owner string) string {
	// Keep both helpers outside the module-qualified namespace of user functions.
	return '__v3_cache_${g.cname(owner)}__init_consts'
}

fn (mut g FlatGen) emit_cache_const_init_declarations() {
	mut modules := g.cache_const_modules.keys()
	modules.sort()
	for owner in modules {
		g.writeln('void ${g.cache_const_init_name(owner)}(void);')
		g.writeln('void ${g.cache_const_init_name(owner)}_defaults(void);')
	}
}

fn (mut g FlatGen) emit_cache_const_global_defaults(modules []string) {
	for owner in modules {
		if g.cache_const_modules[owner] {
			g.writeln('\t${g.cache_const_init_name(owner)}_defaults();')
		}
	}
}

fn (mut g FlatGen) emit_cache_module_constants() {
	if g.cache_const_modules.len == 0 {
		return
	}
	mut modules := g.cache_const_modules.keys()
	modules.sort()
	// Preserve the existing early initialization of implicit global defaults
	// read by constants. Both the selection and the code live with the module.
	mut early := []bool{len: g.runtime_inits.len}
	old_sb := g.sb
	old_line_start := g.line_start
	g.sb = strings.new_builder(128)
	g.emit_const_referenced_global_defaults(mut early, true)
	unsafe { g.sb.free() }
	g.sb = old_sb
	g.line_start = old_line_start
	for owner in modules {
		// The interface retains the module's source location. An owner with no
		// definitions was read from its interface and already has an init symbol.
		if !g.cache_const_source_modules[owner] {
			continue
		}
		g.writeln('/* V3CACHE_MODULE ${owner} */')
		for definition in g.cache_const_definitions[owner] {
			g.sb.write_string(definition)
		}
		g.writeln('void ${g.cache_const_init_name(owner)}_defaults(void) {')
		for i, init in g.runtime_inits {
			if early[i] && g.runtime_init_modules[i] == owner {
				g.writeln(init)
			}
		}
		g.writeln('}')
		g.writeln('void ${g.cache_const_init_name(owner)}(void) {')
		for i, init in g.const_runtime_inits {
			if g.const_runtime_init_modules[i] == owner {
				g.writeln(init)
			}
		}
		for i, init in g.runtime_inits {
			if !early[i] && g.runtime_init_modules[i] == owner {
				g.writeln(init)
			}
		}
		g.writeln('}')
	}
}
