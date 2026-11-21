module fastc

import os
import v.pref
import v.scanner
import v.flat
import v.token

// Generic structs and generic sum types are monomorphized at the source level across every project
// module (and the entry module, which may live in vlib): each concrete `Box[int]`,
// `ds.Queue[Message]` or `Box[[]ds.Item]` gets a copy of the struct and of its methods in the
// defining module, named `Box_mono_<arguments>`, and the reference is renamed to it. Arguments from
// other modules are spelled in the defining module by their C names (`other__Item`); `main` types
// keep their plain names, as builtin types do. Copies may reference further instances, so the pass
// repeats until no concrete reference is left. References that still use a type parameter stay
// generic: on-demand instances of generic functions map them to these copies through
// fastc_struct_instance_for_arguments, and generic structs left without a copy keep FastC's erased
// layout.

const fastc_struct_mono_max_rounds = 24

struct FastcStructGeneric {
	module_name   string
	name          string
	type_params   []string
	source_index  int
	start         int // offset of `struct` (or a leading `pub`)
	end           int // offset just past the body `}`
	name_start    int
	name_end      int
	bracket_start int // offset of `[`
	bracket_end   int // offset just past `]`
	// library: declared in a vlib module that keeps its erased layout and methods for its own
	// code; only the instances project code names are copied.
	library bool
}

struct FastcStructGenericMethod {
	struct_key     string
	type_params    []string // the receiver's type parameter names, in order
	source_index   int
	start          int // offset of `fn` (or a leading `pub`)
	end            int // offset just past the body `}`
	receiver_start int // offset of `S` in the receiver type `S[T]`
	receiver_end   int // offset just past `]`
}

// FastcStructGenericFn is a generic free function, including a lowered generic static method.
// Its explicit instantiations are scanned for the generic struct instances they need.
struct FastcStructGenericFn {
	module_name   string
	type_params   []string
	source_index  int
	start         int
	end           int
	bracket_start int
	bracket_end   int
}

struct FastcStructInstance {
	struct_key string
	name       string
	canonical  []string // the arguments, spelled as fastc_c_type_display_name spells them
	spellings  []string // the arguments, spelled in the defining module
}

struct FastcStructRef {
	start    int
	end      int
	instance string // the key of the referenced instance
}

// FastcTypeText is one parsed type argument.
struct FastcTypeText {
	canonical string
	spelling  string
	concrete  bool
	ok        bool
}

struct FastcStructMono {
	prefs &pref.Preferences = unsafe { nil }
mut:
	sources            []FastcSourceFile
	eligible           []bool
	module_types       map[string]map[string]bool            // module -> type names it declares
	generics           map[string]FastcStructGeneric         // `module.Name` -> generic struct
	methods            map[string][]FastcStructGenericMethod // `module.Name` -> its generic methods
	instances          map[string]FastcStructInstance        // struct key + `|` + arguments -> instance
	pending            []string                        // instances whose copies are not emitted yet
	functions          map[string]FastcStructGenericFn // `module.name` -> generic function
	function_instances map[string]bool                 // function key + `|` + arguments, already scanned
	// vlib modules are indexed when project code first names one of their generics.
	library_files   map[string][]int // module -> its sources outside the project
	indexed_modules map[string]bool
	// library_start is the length of a vlib source before copies were appended to it; only
	// the copies are scanned for further instances.
	library_start map[int]int
}

fn fastc_struct_mono_module(module_name string) string {
	return if module_name == '' { 'main' } else { module_name }
}

fn fastc_struct_mono_key(module_name string, name string) string {
	return '${fastc_struct_mono_module(module_name)}.${name}'
}

// fastc_struct_mono_eligible reports whether generic structs of `source_file` are specialized:
// project sources and the entry module. Other vlib modules keep the erased layout.
fn fastc_struct_mono_eligible(sources []FastcSourceFile, source_file FastcSourceFile, prefs &pref.Preferences) bool {
	if fastc_is_entry_module(sources, source_file.header.module_name, prefs) {
		return true
	}
	return !source_file.path.starts_with(os.join_path_single(prefs.vroot, 'vlib') + os.path_separator)
}

// fastc_monomorphize_generic_structs runs the whole-program generic struct specialization.
fn fastc_monomorphize_generic_structs(sources []FastcSourceFile, prefs &pref.Preferences) ![]FastcSourceFile {
	mut m := FastcStructMono{
		prefs:   unsafe { prefs }
		sources: sources.clone()
	}
	mut any_generic := false
	for i, source_file in sources {
		eligible := fastc_struct_mono_eligible(sources, source_file, prefs)
		m.eligible << eligible
		module_name := fastc_struct_mono_module(source_file.header.module_name)
		if eligible {
			m.indexed_modules[module_name] = true
			// A generic of the project or of a vlib module (`stdatomic.AtomicVal[u32]`).
			if source_file.source.contains(']') {
				any_generic = true
			}
		} else {
			m.library_files[module_name] << i
		}
	}
	if !any_generic {
		return sources
	}
	for i, source_file in m.sources {
		if m.eligible[i] {
			m.index_file(i, source_file)
		}
	}
	mut dirty := []int{}
	for i, eligible in m.eligible {
		if eligible {
			dirty << i
		}
	}
	for _ in 0 .. fastc_struct_mono_max_rounds {
		mut edits := map[int][]FastcSourceEdit{}
		for i in dirty {
			for ref in m.scan_refs(i) {
				instance := m.instances[ref.instance] or { continue }
				edits[i] << FastcSourceEdit{
					source_index: i
					start:        ref.start
					end:          ref.end
					replacement:  instance.name
				}
			}
		}
		mut copies := map[int][]FastcSourceFile{}
		for key in m.pending {
			instance := m.instances[key] or { continue }
			generic := m.generics[instance.struct_key] or { continue }
			copies[generic.source_index] << m.render_struct(generic, instance)
			for method in m.methods[instance.struct_key] {
				copies[method.source_index] << m.render_method(method, instance)
			}
		}
		m.pending.clear()
		if edits.len == 0 && copies.len == 0 {
			break
		}
		dirty.clear()
		for i in 0 .. m.sources.len {
			file_edits := edits[i] or { []FastcSourceEdit{} }
			file_copies := copies[i] or { []FastcSourceFile{} }
			if file_edits.len == 0 && file_copies.len == 0 {
				continue
			}
			mut rewritten := fastc_apply_mapped_source_edits(m.sources[i], file_edits)
			for copy in file_copies {
				rewritten = fastc_append_mapped_source(rewritten, copy)
			}
			m.sources[i] = rewritten
			m.reindex_file(i)
			dirty << i
		}
	}
	m.remove_instantiated_generics()
	return m.sources
}

// ensure_module_indexed indexes the generics of a vlib module that project code names.
fn (mut m FastcStructMono) ensure_module_indexed(module_name string) {
	if module_name in m.indexed_modules {
		return
	}
	m.indexed_modules[module_name] = true
	for index in m.library_files[module_name] or { []int{} } {
		m.library_start[index] = m.sources[index].source.len
		m.index_file(index, m.sources[index])
	}
}

// remove_instantiated_generics blanks the generic methods of every struct with a copy, as
// entry-module specialization always did: an erased method body such as `a.v += by` does not
// compile once `T` is `voidptr`. The erased declaration stays for the placeholders of generic
// functions that name the struct.
fn (mut m FastcStructMono) remove_instantiated_generics() {
	mut instantiated := map[string]bool{}
	for _, instance in m.instances {
		instantiated[instance.struct_key] = true
	}
	mut edits := map[int][]FastcSourceEdit{}
	for key, generic in m.generics {
		if key !in instantiated || generic.library {
			continue
		}
		for method in m.methods[key] {
			edits[method.source_index] << FastcSourceEdit{
				start: method.start
				end:   method.end
			}
		}
	}
	for i, file_edits in edits {
		m.sources[i] = fastc_apply_mapped_source_edits(m.sources[i], file_edits)
	}
}

// index_file records the type names, generic structs and their methods of one source.
fn (mut m FastcStructMono) index_file(index int, source_file FastcSourceFile) {
	module_name := fastc_struct_mono_module(source_file.header.module_name)
	if module_name !in m.module_types {
		m.module_types[module_name] = map[string]bool{}
	}
	source := source_file.source
	file := token.File.unindexed(source_file.path, source.len)
	mut s := scanner.new_scanner(m.prefs, .normal)
	s.init(file, source)
	mut previous := token.Token.unknown
	mut previous_pos := 0
	mut pending_methods := []FastcStructGenericMethod{}
	mut tok := s.scan()
	for tok != .eof {
		if tok in [.key_struct, .key_union, .key_enum, .key_interface, .key_type]
			&& previous != .dot {
			decl_start := if previous == .key_pub { previous_pos } else { s.pos }
			kind := tok
			tok = s.scan()
			if tok != .name || s.lit == 'C' {
				previous = kind
				continue
			}
			name := s.lit
			name_start := s.pos
			name_end := s.offset
			m.module_types[module_name][name] = true
			tok = s.scan()
			if kind == .key_struct && tok == .lsbr {
				bracket_start := s.pos
				params, next := fastc_struct_mono_read_params(mut s)
				tok = next
				if tok != .rsbr || params.len == 0 {
					previous = tok
					continue
				}
				bracket_end := s.offset
				tok = s.scan()
				if tok != .lcbr {
					previous = tok
					continue
				}
				mut braces := 0
				for tok != .eof {
					if tok == .lcbr {
						braces++
					} else if tok == .rcbr {
						braces--
						if braces == 0 {
							break
						}
					}
					tok = s.scan()
				}
				if tok == .eof {
					break
				}
				m.generics[fastc_struct_mono_key(module_name, name)] = FastcStructGeneric{
					module_name:   module_name
					name:          name
					type_params:   params
					source_index:  index
					start:         decl_start
					end:           s.offset
					name_start:    name_start
					name_end:      name_end
					bracket_start: bracket_start
					bracket_end:   bracket_end
					library:       !m.eligible[index]
				}
				previous = .rcbr
				previous_pos = s.pos
				tok = s.scan()
				continue
			}
			if kind == .key_type && tok == .lsbr {
				// A generic sum type (`type Data[T] = Small[T] | Big[T]`) is copied like a
				// generic struct; its variants become instances through the copy.
				bracket_start := s.pos
				params, next := fastc_struct_mono_read_params(mut s)
				tok = next
				if tok != .rsbr || params.len == 0 {
					previous = tok
					continue
				}
				bracket_end := s.offset
				tok = s.scan()
				if tok != .assign {
					previous = tok
					continue
				}
				decl_end, after := fastc_struct_mono_type_decl_end(mut s, source)
				m.generics[fastc_struct_mono_key(module_name, name)] = FastcStructGeneric{
					module_name:   module_name
					name:          name
					type_params:   params
					source_index:  index
					start:         decl_start
					end:           decl_end
					name_start:    name_start
					name_end:      name_end
					bracket_start: bracket_start
					bracket_end:   bracket_end
					library:       !m.eligible[index]
				}
				previous = .name
				previous_pos = s.pos
				tok = after
				continue
			}
			previous = .name
			continue
		}
		if tok == .key_fn && previous != .dot {
			fn_start := if previous == .key_pub { previous_pos } else { s.pos }
			mut fn_look := s
			if name, generic_fn := fastc_struct_mono_read_generic_fn(mut fn_look, module_name, index,
				fn_start)
			{
				m.functions[fastc_struct_mono_key(module_name, name)] = generic_fn
			}
			mut look := s
			if method := fastc_struct_mono_read_method(mut look, module_name, index, fn_start) {
				pending_methods << method
				s = look
				previous = .rcbr
				previous_pos = s.pos
				tok = s.scan()
				continue
			}
		}
		previous = tok
		previous_pos = s.pos
		tok = s.scan()
	}
	// Methods may precede their struct in the file or live in another file of the module.
	for method in pending_methods {
		m.methods[method.struct_key] << method
	}
}

// fastc_struct_mono_type_decl_end reads the right side of a type declaration with the scanner
// just after `=`, and returns the offset after its last token and the token that follows it. The
// declaration ends at a line break that no `|` continues.
fn fastc_struct_mono_type_decl_end(mut s scanner.Scanner, source string) (int, token.Token) {
	mut depth := 0
	mut end := s.offset
	mut previous := token.Token.assign
	mut tok := s.scan()
	for tok != .eof {
		if depth == 0 && tok != .pipe && previous !in [.pipe, .assign] {
			if tok == .semicolon || source[end..s.pos].contains('\n') {
				if tok == .semicolon {
					mut look := s
					if look.scan() == .pipe {
						previous = tok
						tok = s.scan()
						continue
					}
				}
				break
			}
		}
		if tok in [.lsbr, .lpar, .lcbr] {
			depth++
		} else if tok in [.rsbr, .rpar, .rcbr] {
			depth--
		}
		if tok != .semicolon {
			end = s.offset
		}
		previous = tok
		tok = s.scan()
	}
	return end, tok
}

// reindex_file refreshes the offsets of the generic declarations of an edited source.
fn (mut m FastcStructMono) reindex_file(index int) {
	for key, generic in m.generics {
		if generic.source_index == index {
			m.generics.delete(key)
		}
	}
	for key, methods in m.methods {
		m.methods[key] = methods.filter(it.source_index != index)
	}
	for key, generic_fn in m.functions {
		if generic_fn.source_index == index {
			m.functions.delete(key)
		}
	}
	m.index_file(index, m.sources[index])
}

// fastc_struct_mono_read_params reads the names in `[T, U]`; the current token is `[`.
fn fastc_struct_mono_read_params(mut s scanner.Scanner) ([]string, token.Token) {
	mut params := []string{}
	mut tok := s.scan()
	for tok == .name {
		params << s.lit
		tok = s.scan()
		if tok != .comma {
			break
		}
		tok = s.scan()
	}
	return params, tok
}

// fastc_struct_mono_read_method recognizes `fn (r S[T]) name(...) ... { ... }` with the
// scanner just after `fn`, and leaves it on the closing `}`.
fn fastc_struct_mono_read_method(mut s scanner.Scanner, module_name string, index int, fn_start int) ?FastcStructGenericMethod {
	mut tok := s.scan()
	if tok != .lpar {
		return none
	}
	tok = s.scan()
	if tok in [.key_mut, .key_shared] {
		tok = s.scan()
	}
	if tok != .name {
		return none
	}
	tok = s.scan()
	for (tok in [.amp, .and]) {
		tok = s.scan()
	}
	if tok != .name {
		return none
	}
	struct_name := s.lit
	receiver_start := s.pos
	tok = s.scan()
	if tok != .lsbr {
		return none
	}
	params, next := fastc_struct_mono_read_params(mut s)
	if next != .rsbr || params.len == 0 {
		return none
	}
	receiver_end := s.offset
	tok = s.scan()
	if tok != .rpar {
		return none
	}
	// Name, optional own type parameters, parameters and return type up to the body.
	mut depth := 0
	for {
		tok = s.scan()
		if tok == .eof {
			return none
		}
		if tok in [.lpar, .lsbr] {
			depth++
		} else if tok in [.rpar, .rsbr] {
			depth--
		} else if tok == .lcbr && depth == 0 {
			break
		}
	}
	mut braces := 0
	for {
		if tok == .lcbr {
			braces++
		} else if tok == .rcbr {
			braces--
			if braces == 0 {
				break
			}
		} else if tok == .eof {
			return none
		}
		tok = s.scan()
	}
	return FastcStructGenericMethod{
		struct_key:     fastc_struct_mono_key(module_name, struct_name)
		type_params:    params
		source_index:   index
		start:          fn_start
		end:            s.offset
		receiver_start: receiver_start
		receiver_end:   receiver_end
	}
}

// FastcStructMonoFile is the name resolution context of the source being scanned.
struct FastcStructMonoFile {
	module_name string
	imports     map[string]string
mut:
	type_params []string // of the enclosing generic declaration
}

// scan_refs finds the concrete generic struct references of one source and registers their
// instances. A reference that uses a type parameter of its enclosing declaration is skipped.
fn (mut m FastcStructMono) scan_refs(index int) []FastcStructRef {
	source_file := m.sources[index]
	return m.scan_text(source_file.path, source_file.source, FastcStructMonoFile{
		module_name: fastc_struct_mono_module(source_file.header.module_name)
		imports:     source_file.header.imports
	}, m.library_start[index] or { 0 })
}

// scan_text finds the concrete generic struct references in `source`, and follows explicit
// instantiations of generic functions (`ds.make[Item](...)`) into their substituted bodies.
fn (mut m FastcStructMono) scan_text(path string, source string, file_ctx FastcStructMonoFile, min_offset int) []FastcStructRef {
	mut ctx := file_ctx
	file := token.File.unindexed(path, source.len)
	mut s := scanner.new_scanner(m.prefs, .normal)
	s.init(file, source)
	mut refs := []FastcStructRef{}
	mut previous := token.Token.unknown
	mut depth := 0
	mut scope_depth := -1
	mut tok := s.scan()
	for tok != .eof {
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			depth--
			if scope_depth >= 0 && depth <= scope_depth {
				scope_depth = -1
				ctx.type_params = []string{}
			}
		}
		if scope_depth < 0 && tok in [.key_fn, .key_struct] && previous != .dot {
			// Collect the type parameters of a declaration's header; its body starts at the
			// next top-level `{`.
			mut look := s
			params := fastc_struct_mono_header_params(mut look, tok)
			if params.len > 0 {
				ctx.type_params = params
				scope_depth = depth
			}
		}
		if tok != .name || previous in [.dot, .key_struct, .key_fn] {
			previous = tok
			tok = s.scan()
			continue
		}
		name_start := s.pos
		if name_start < min_offset {
			// The original text of a vlib source keeps its erased generics.
			previous = tok
			tok = s.scan()
			continue
		}
		mut struct_key := ''
		mut look := s
		mut next := look.scan()
		mut name_pos := name_start
		if next == .dot {
			if module_name := ctx.imports[s.lit] {
				m.ensure_module_indexed(module_name)
				if look.scan() == .name {
					key := fastc_struct_mono_key(module_name, look.lit)
					if key in m.generics {
						struct_key = key
						name_pos = look.pos
						next = look.scan()
					} else if key in m.functions {
						mut call := look
						if call.scan() == .lsbr {
							m.discover_function_call(mut call, key, ctx)
						}
					}
				}
			}
		} else if next == .lsbr {
			key := fastc_struct_mono_key(ctx.module_name, s.lit)
			if key in m.generics {
				struct_key = key
			} else if key in m.functions {
				mut call := look
				m.discover_function_call(mut call, key, ctx)
			} else if module_name := ctx.imports[fastc_selective_import_key(s.lit)] {
				m.ensure_module_indexed(module_name)
				selected := fastc_struct_mono_key(module_name, s.lit)
				if selected in m.generics {
					struct_key = selected
				}
			}
		}
		if struct_key == '' || next != .lsbr {
			previous = tok
			tok = s.scan()
			continue
		}
		generic := m.generics[struct_key] or { FastcStructGeneric{} }
		mut arguments := []FastcTypeText{}
		mut arg_tok := look.scan()
		mut ok := true
		for {
			argument, after := m.parse_type(mut look, arg_tok, ctx, generic.module_name)
			if !argument.ok {
				ok = false
				break
			}
			arguments << argument
			if after == .comma {
				arg_tok = look.scan()
				continue
			}
			if after != .rsbr {
				ok = false
			}
			break
		}
		if !ok || arguments.len != generic.type_params.len || arguments.any(!it.concrete) {
			previous = tok
			tok = s.scan()
			continue
		}
		instance_key := m.instance(struct_key, arguments)
		refs << FastcStructRef{
			start:    name_pos
			end:      look.offset
			instance: instance_key
		}
		s = look
		previous = .name
		tok = s.scan()
	}
	return refs
}

// fastc_struct_mono_header_params returns the type parameters a `fn` or `struct` header
// declares, in `fn (r S[T]) m[U]()`, `fn f[T]()`, `fn S.new[T]()` and `struct S[T]`. The
// scanner is a copy positioned on the keyword.
fn fastc_struct_mono_header_params(mut s scanner.Scanner, keyword token.Token) []string {
	mut params := []string{}
	mut tok := s.scan()
	if keyword == .key_struct {
		if tok != .name {
			return params
		}
		tok = s.scan()
		if tok == .lsbr {
			names, _ := fastc_struct_mono_read_params(mut s)
			params << names
		}
		return params
	}
	if tok == .lpar {
		// The receiver.
		mut depth := 1
		mut previous_name := false
		for depth > 0 {
			tok = s.scan()
			if tok == .eof {
				return params
			}
			if tok == .lsbr && previous_name {
				names, after := fastc_struct_mono_read_params(mut s)
				params << names
				tok = after
				if tok != .rsbr {
					return params
				}
				previous_name = false
				continue
			}
			if tok == .lpar {
				depth++
			} else if tok == .rpar {
				depth--
			}
			previous_name = tok == .name
		}
		tok = s.scan()
		// A receiver is followed by the method name; a function type or literal is not.
		mut method_look := s
		after_name := method_look.scan()
		if tok != .name || after_name !in [.lpar, .lsbr] {
			return []string{}
		}
	}
	if tok != .name {
		return params
	}
	tok = s.scan()
	if tok == .dot {
		tok = s.scan()
		if tok != .name {
			return params
		}
		tok = s.scan()
	}
	if tok == .lsbr {
		names, _ := fastc_struct_mono_read_params(mut s)
		params << names
	}
	return params
}

// fastc_struct_mono_builtin_type reports the types every module names without a qualifier.
fn fastc_struct_mono_builtin_type(name string) bool {
	if fastc_primitive_c_type(name) != none {
		return true
	}
	return name in ['string', 'voidptr', 'byteptr', 'charptr', 'array', 'map', 'IError', 'Error',
		'MessageError', 'rune', 'bool', 'f32', 'f64', 'int', 'uint', 'i8', 'i16', 'i32', 'i64',
		'u8', 'u16', 'u32', 'u64', 'byte', 'char', 'any', 'none']
}

// parse_type parses one type argument at `first`, resolving it in `ctx` and spelling it for
// the module `def_module` that defines the generic struct.
fn (mut m FastcStructMono) parse_type(mut s scanner.Scanner, first token.Token, ctx FastcStructMonoFile, def_module string) (FastcTypeText, token.Token) {
	mut tok := first
	match tok {
		.amp, .and, .question, .not {
			prefix := match tok {
				.amp { '&' }
				.and { '&&' }
				.question { '?' }
				else { '!' }
			}
			inner, after := m.parse_type(mut s, s.scan(), ctx, def_module)
			return FastcTypeText{
				canonical: prefix + inner.canonical
				spelling:  prefix + inner.spelling
				concrete:  inner.concrete
				ok:        inner.ok
			}, after
		}
		.lsbr {
			tok = s.scan()
			mut prefix := '[]'
			if tok != .rsbr {
				if tok != .number {
					return FastcTypeText{}, tok
				}
				prefix = '[${s.lit}]'
				tok = s.scan()
				if tok != .rsbr {
					return FastcTypeText{}, tok
				}
			}
			inner, after := m.parse_type(mut s, s.scan(), ctx, def_module)
			return FastcTypeText{
				canonical: prefix + inner.canonical
				spelling:  prefix + inner.spelling
				concrete:  inner.concrete
				ok:        inner.ok
			}, after
		}
		.name {}
		else {
			return FastcTypeText{}, tok
		}
	}
	name := s.lit
	if name in ['usize', 'isize'] {
		// FastC rejects these types, so a reference naming one is a compile-time pattern
		// (`$if T is Box[usize]`) or inactive code, not an instance.
		return FastcTypeText{}, s.scan()
	}
	if name == 'map' {
		if s.scan() != .lsbr {
			return FastcTypeText{}, tok
		}
		key, after_key := m.parse_type(mut s, s.scan(), ctx, def_module)
		if after_key != .rsbr {
			return FastcTypeText{}, after_key
		}
		value, after := m.parse_type(mut s, s.scan(), ctx, def_module)
		return FastcTypeText{
			canonical: 'map[${key.canonical}]${value.canonical}'
			spelling:  'map[${key.spelling}]${value.spelling}'
			concrete:  key.concrete && value.concrete
			ok:        key.ok && value.ok
		}, after
	}
	if name == 'chan' {
		inner, after := m.parse_type(mut s, s.scan(), ctx, def_module)
		return FastcTypeText{
			canonical: 'chan ' + inner.canonical
			spelling:  'chan ' + inner.spelling
			concrete:  inner.concrete
			ok:        inner.ok
		}, after
	}
	tok = s.scan()
	mut type_module := ''
	mut type_name := name
	if tok == .dot {
		tok = s.scan()
		if tok != .name {
			return FastcTypeText{}, tok
		}
		if name == 'C' {
			type_module = 'C'
		} else {
			type_module = ctx.imports[name] or { return FastcTypeText{}, tok }
			m.ensure_module_indexed(type_module)
		}
		type_name = s.lit
		tok = s.scan()
	} else if name.contains('__') && !name.starts_with('_') {
		// A type from another module spelled in a copy by its C name (`cli__Event`).
		return FastcTypeText{
			canonical: fastc_c_type_display_name(flat.source_type_id(name))
			spelling:  name
			concrete:  true
			ok:        true
		}, tok
	} else if name in ctx.type_params {
		return FastcTypeText{
			canonical: name
			spelling:  name
			concrete:  false
			ok:        true
		}, tok
	} else if fastc_struct_mono_builtin_type(name) {
		type_module = 'builtin'
	} else if name in m.module_types[ctx.module_name] {
		type_module = ctx.module_name
	} else if selected := ctx.imports[fastc_selective_import_key(name)] {
		type_module = selected
	} else if name.len == 1 && name[0].is_capital() {
		// An unresolved single capital letter is a type parameter.
		return FastcTypeText{
			canonical: name
			spelling:  name
			concrete:  false
			ok:        true
		}, tok
	} else {
		type_module = 'builtin'
	}
	if tok == .lsbr {
		// A nested generic type argument (`Box[Queue[Item]]`).
		key := fastc_struct_mono_key(type_module, type_name)
		mut arguments := []FastcTypeText{}
		mut arg_tok := s.scan()
		for {
			generic := m.generics[key] or { FastcStructGeneric{} }
			argument, after := m.parse_type(mut s, arg_tok, ctx, if key in m.generics {
				generic.module_name
			} else {
				def_module
			})
			if !argument.ok {
				return FastcTypeText{}, after
			}
			arguments << argument
			if after == .comma {
				arg_tok = s.scan()
				continue
			}
			if after != .rsbr {
				return FastcTypeText{}, after
			}
			break
		}
		tok = s.scan()
		if key !in m.generics || arguments.any(!it.concrete) {
			// Only specialized generic structs nest; others keep the erased layout.
			return FastcTypeText{
				canonical: type_name
				spelling:  m.spell_type(type_module, type_name, def_module)
				concrete:  !arguments.any(!it.concrete)
				ok:        key !in m.generics
			}, tok
		}
		instance_key := m.instance(key, arguments)
		instance := m.instances[instance_key] or { FastcStructInstance{} }
		generic := m.generics[key] or { FastcStructGeneric{} }
		return FastcTypeText{
			canonical: m.canonical_type(generic.module_name, instance.name)
			spelling:  m.spell_type(generic.module_name, instance.name, def_module)
			concrete:  true
			ok:        true
		}, tok
	}
	return FastcTypeText{
		canonical: m.canonical_type(type_module, type_name)
		spelling:  m.spell_type(type_module, type_name, def_module)
		concrete:  true
		ok:        true
	}, tok
}

// canonical_type spells a resolved type as fastc_c_type_display_name does.
fn (m &FastcStructMono) canonical_type(type_module string, type_name string) string {
	if type_module in ['builtin', 'main', ''] {
		return type_name
	}
	return '${type_module}.${type_name}'
}

// spell_type spells a resolved type inside the module `def_module`.
fn (m &FastcStructMono) spell_type(type_module string, type_name string, def_module string) string {
	if type_module in ['builtin', 'main', '', def_module] {
		return type_name
	}
	if type_module == 'C' {
		return 'C.${type_name}'
	}
	return fastc_c_declared_type_name(fastc_type_key(type_module, type_name))
}

// instance returns the key of the instance of `struct_key` for `arguments`, registering it.
fn (mut m FastcStructMono) instance(struct_key string, arguments []FastcTypeText) string {
	canonical := arguments.map(it.canonical)
	key := '${struct_key}|${canonical.join(',')}'
	if key in m.instances {
		return key
	}
	generic := m.generics[struct_key] or { FastcStructGeneric{} }
	instance_name := fastc_struct_instance_name(generic.name, canonical)
	// Copies spell the instance by its plain name in its own module.
	m.module_types[generic.module_name][instance_name] = true
	m.instances[key] = FastcStructInstance{
		struct_key: struct_key
		name:       instance_name
		canonical:  canonical
		spellings:  arguments.map(it.spelling)
	}
	m.pending << key
	return key
}

// fastc_struct_instance_name names the copy of `name` for canonical type arguments. It must not
// contain `__`, which FastC reads as a module separator.
fn fastc_struct_instance_name(name string, canonical []string) string {
	mut parts := []string{cap: canonical.len}
	for argument in canonical {
		mut part := argument.replace('map[', 'Map_').replace('[]', 'Array_').replace('&',
			'Ptr_').replace('?', 'Option_').replace('!', 'Result_').replace('chan ', 'Chan_').replace('.',
			'_m_').replace('[', 'Fixed_').replace(']', '_')
		for part.contains('__') {
			part = part.replace('__', '_')
		}
		parts << part
	}
	return '${name}_mono_${parts.join('_and_')}'
}

// render_struct copies a generic struct for one instance.
fn (m &FastcStructMono) render_struct(generic FastcStructGeneric, instance FastcStructInstance) FastcSourceFile {
	source_file := m.sources[generic.source_index]
	mut edits := []FastcSourceEdit{}
	edits << FastcSourceEdit{
		start:       generic.name_start - generic.start
		end:         generic.bracket_end - generic.start
		replacement: instance.name
	}
	return m.render_copy(source_file, generic.start, generic.end, generic.type_params, instance,
		generic.bracket_start, generic.bracket_end, mut edits)
}

// render_method copies a method of a generic struct for one instance.
fn (m &FastcStructMono) render_method(method FastcStructGenericMethod, instance FastcStructInstance) FastcSourceFile {
	source_file := m.sources[method.source_index]
	mut edits := []FastcSourceEdit{}
	edits << FastcSourceEdit{
		start:       method.receiver_start - method.start
		end:         method.receiver_end - method.start
		replacement: instance.name
	}
	// A method that lists a receiver parameter among its own (`read_bits[E, T]` on
	// `BitReader[E]`) keeps it generic; its calls pass the same type explicitly. One that only
	// repeats the receiver's parameters (`fn (d Data[T]) slice[T]()`) means the same types, so
	// the copy substitutes them and drops its own list.
	own_params := fastc_struct_mono_method_own_params(source_file.source, method)
	if own_params.len > 0 && own_params == method.type_params {
		if own_start, own_end := fastc_struct_mono_method_own_params_span(source_file.source,
			method)
		{
			edits << FastcSourceEdit{
				start:       own_start - method.start
				end:         own_end - method.start
				replacement: ''
			}
			return m.render_copy(source_file, method.start, method.end, method.type_params,
				instance, method.receiver_start, method.receiver_end, mut edits)
		}
	}
	type_params := method.type_params.map(if it in own_params { '' } else { it })
	return m.render_copy(source_file, method.start, method.end, type_params, instance,
		method.receiver_start, method.receiver_end, mut edits)
}

// fastc_struct_mono_method_own_params_span returns the source offsets of the `[...]` a generic
// struct's method declares after its name.
fn fastc_struct_mono_method_own_params_span(source string, method FastcStructGenericMethod) ?(int, int) {
	rest := source[method.receiver_end..method.end]
	close := rest.index(')') or { return none }
	mut i := close + 1
	for i < rest.len && rest[i] in [` `, `\t`] {
		i++
	}
	name_start := i
	for i < rest.len && (rest[i].is_letter() || rest[i].is_digit() || rest[i] == `_`) {
		i++
	}
	if i == name_start || i >= rest.len || rest[i] != `[` {
		return none
	}
	end := rest.index_after(']', i) or { return none }
	return method.receiver_end + i, method.receiver_end + end + 1
}

// fastc_struct_mono_method_own_params returns the type parameters a generic struct's method
// declares after its name.
fn fastc_struct_mono_method_own_params(source string, method FastcStructGenericMethod) []string {
	rest := source[method.receiver_end..method.end]
	close := rest.index(')') or { return []string{} }
	after := rest[close + 1..].trim_left(' \t')
	mut i := 0
	for i < after.len && (after[i].is_letter() || after[i].is_digit() || after[i] == `_`) {
		i++
	}
	if i == 0 || i >= after.len || after[i] != `[` {
		return []string{}
	}
	end := after.index_after(']', i) or { return []string{} }
	return after[i + 1..end].split(',').map(it.trim_space())
}

// render_copy substitutes the type parameters in `[start, end)` outside the already edited
// span `[skip_start, skip_end)`. `T.name` becomes the argument's V name.
fn (m &FastcStructMono) render_copy(source_file FastcSourceFile, start int, end int, type_params []string, instance FastcStructInstance, skip_start int, skip_end int, mut edits []FastcSourceEdit) FastcSourceFile {
	text := source_file.source[start..end]
	mut substitutions := map[string]int{}
	for i, param in type_params {
		if i < instance.spellings.len {
			substitutions[param] = i
		}
	}
	file := token.File.unindexed(source_file.path, text.len)
	mut s := scanner.new_scanner(m.prefs, .normal)
	s.init(file, text)
	low := skip_start - start
	high := skip_end - start
	// The caller's edits (the renamed header, a dropped parameter list) are not substituted.
	preset := edits.clone()
	mut previous := token.Token.unknown
	mut tok := s.scan()
	for tok != .eof {
		if tok == .name && previous != .dot && !(s.pos >= low && s.pos < high)
			&& !preset.any(s.pos >= it.start && s.pos < it.end) {
			if i := substitutions[s.lit] {
				mut look := s
				if look.scan() == .dot && look.scan() == .name && look.lit == 'name'
					&& look.scan() != .lpar {
					edits << FastcSourceEdit{
						start:       s.pos
						end:         s.offset + '.name'.len
						replacement: fastc_string_literal(instance.canonical[i])
					}
					s.scan()
					s.scan()
					previous = .name
					tok = s.scan()
					continue
				}
				edits << FastcSourceEdit{
					start:       s.pos
					end:         s.offset
					replacement: instance.spellings[i]
				}
			}
		}
		previous = tok
		tok = s.scan()
	}
	return fastc_apply_mapped_source_edits(FastcSourceFile{
		...source_file
		source:        text
		source_offset: source_file.source_offset + start
	}, edits)
}

// map_mono_struct_instances renames `S[args]` in an on-demand generic instance to the
// specialized copy of `S` for those arguments, when the source-level pass created it.
// References without a copy keep their arguments for erase_mono_generic_type_arguments.
fn (g &Parser) map_mono_struct_instances(source_file FastcSourceFile, src FastcGenericMethodSource) FastcSourceFile {
	if !source_file.source.contains('[') {
		return source_file
	}
	file := token.File.unindexed(src.path, source_file.source.len)
	mut scan := scanner.new_scanner(g.prefs, .normal)
	scan.init(file, source_file.source)
	mut edits := []FastcSourceEdit{}
	mut previous := token.Token.unknown
	mut tok := scan.scan()
	for tok != .eof {
		if tok != .name || previous == .dot || scan.lit == 'map' {
			previous = tok
			tok = scan.scan()
			continue
		}
		mut name_pos := scan.pos
		mut struct_module := fastc_struct_mono_module(src.module_name)
		mut struct_name := scan.lit
		mut look := scan
		mut next := look.scan()
		if next == .dot {
			qualified_module := src.imports[scan.lit] or { '' }
			if qualified_module == '' || look.scan() != .name {
				previous = tok
				tok = scan.scan()
				continue
			}
			struct_module = qualified_module
			struct_name = look.lit
			name_pos = look.pos
			next = look.scan()
		}
		if next != .lsbr || struct_name.len == 0 || !struct_name[0].is_capital() {
			previous = tok
			tok = scan.scan()
			continue
		}
		mut arguments := []string{}
		mut current := []string{}
		mut depth := 1
		for depth > 0 {
			arg_tok := look.scan()
			if arg_tok == .eof {
				return source_file
			}
			if arg_tok == .lsbr {
				depth++
			} else if arg_tok == .rsbr {
				depth--
				if depth == 0 {
					break
				}
			} else if arg_tok == .comma && depth == 1 {
				arguments << current.join('')
				current = []string{}
				continue
			}
			current << if arg_tok == .name { look.lit } else { arg_tok.str() }
		}
		arguments << current.join('')
		end := look.offset
		mut canonical := []string{cap: arguments.len}
		for argument in arguments {
			canonical << g.mono_canonical_type_argument(argument, src)
		}
		instance := fastc_struct_instance_name(struct_name, canonical)
		if fastc_source_type_key_exists(g.declared_types, fastc_type_key(struct_module, instance)) {
			edits << FastcSourceEdit{
				start:       name_pos
				end:         end
				replacement: instance
			}
			scan = look
			previous = .name
			tok = scan.scan()
			continue
		}
		previous = tok
		tok = scan.scan()
	}
	return fastc_apply_mapped_source_edits(source_file, edits)
}

// mono_canonical_type_argument spells a substituted type argument of an on-demand instance
// (`Message`, `other__Item`, `&Node`, `[]int`, `Array_int`) as fastc_struct_instance_name
// expects it.
fn (g &Parser) mono_canonical_type_argument(argument string, src FastcGenericMethodSource) string {
	if argument.starts_with('&&') {
		return '&&' + g.mono_canonical_type_argument(argument[2..], src)
	}
	if argument.len > 0 && argument[0] in [`&`, `?`, `!`] {
		return argument[..1] + g.mono_canonical_type_argument(argument[1..], src)
	}
	if argument.starts_with('[]') {
		return '[]' + g.mono_canonical_type_argument(argument[2..], src)
	}
	if argument.ends_with(']') && argument.contains('[') && !argument.starts_with('[') {
		// A nested specialized struct (`AutoHashContext[u32]`) is named by its copy.
		if nested := g.mono_nested_instance_canonical(argument, src) {
			return nested
		}
	}
	if argument.contains('__') || argument.starts_with('Array_') || argument.starts_with('Map_')
		|| argument.ends_with('*') {
		return fastc_c_type_display_name(flat.source_type_id(argument))
	}
	if argument.contains('.') {
		alias := argument.all_before('.')
		if module_name := src.imports[alias] {
			return '${module_name}.${argument.all_after('.')}'
		}
		return argument
	}
	module_name := fastc_struct_mono_module(src.module_name)
	if module_name != 'main' && !fastc_struct_mono_builtin_type(argument)
		&& fastc_source_type_key_exists(g.declared_types, fastc_type_key(module_name, argument)) {
		return '${module_name}.${argument}'
	}
	return argument
}

// fastc_struct_mono_read_generic_fn recognizes `fn name[T](...) ... { ... }` with the scanner
// just after `fn`.
fn fastc_struct_mono_read_generic_fn(mut s scanner.Scanner, module_name string, index int, fn_start int) ?(string, FastcStructGenericFn) {
	mut tok := s.scan()
	if tok != .name {
		return none
	}
	name := s.lit
	tok = s.scan()
	if tok != .lsbr {
		return none
	}
	bracket_start := s.pos
	params, next := fastc_struct_mono_read_params(mut s)
	if next != .rsbr || params.len == 0 {
		return none
	}
	bracket_end := s.offset
	mut depth := 0
	for {
		tok = s.scan()
		if tok == .eof {
			return none
		}
		if tok in [.lpar, .lsbr] {
			depth++
		} else if tok in [.rpar, .rsbr] {
			depth--
		} else if tok == .lcbr && depth == 0 {
			break
		}
	}
	mut braces := 0
	for {
		if tok == .lcbr {
			braces++
		} else if tok == .rcbr {
			braces--
			if braces == 0 {
				break
			}
		} else if tok == .eof {
			return none
		}
		tok = s.scan()
	}
	return name, FastcStructGenericFn{
		module_name:   module_name
		type_params:   params
		source_index:  index
		start:         fn_start
		end:           s.offset
		bracket_start: bracket_start
		bracket_end:   bracket_end
	}
}

// discover_function_call follows the explicit instantiation `fn_key[Args](`, with `call` on
// the `[`.
fn (mut m FastcStructMono) discover_function_call(mut call scanner.Scanner, fn_key string, ctx FastcStructMonoFile) {
	if m.function_instances.len > 4000 {
		return
	}
	generic_fn := m.functions[fn_key] or { return }
	mut arguments := []FastcTypeText{}
	mut arg_tok := call.scan()
	for {
		argument, after := m.parse_type(mut call, arg_tok, ctx, generic_fn.module_name)
		if !argument.ok || !argument.concrete {
			return
		}
		arguments << argument
		if after == .comma {
			arg_tok = call.scan()
			continue
		}
		if after != .rsbr {
			return
		}
		break
	}
	if arguments.len != generic_fn.type_params.len || call.scan() != .lpar {
		return
	}
	m.discover_function(fn_key, generic_fn, arguments)
}

// discover_function scans one explicit instantiation of a generic function for the generic
// struct instances it needs. The substituted text is only scanned, not emitted.
fn (mut m FastcStructMono) discover_function(fn_key string, generic_fn FastcStructGenericFn, arguments []FastcTypeText) {
	key := '${fn_key}|${arguments.map(it.canonical).join(',')}'
	if key in m.function_instances {
		return
	}
	m.function_instances[key] = true
	source_file := m.sources[generic_fn.source_index]
	mut edits := []FastcSourceEdit{}
	edits << FastcSourceEdit{
		start: generic_fn.bracket_start - generic_fn.start
		end:   generic_fn.bracket_end - generic_fn.start
	}
	instance := FastcStructInstance{
		canonical: arguments.map(it.canonical)
		spellings: arguments.map(it.spelling)
	}
	copy := m.render_copy(source_file, generic_fn.start, generic_fn.end, generic_fn.type_params,
		instance, generic_fn.bracket_start, generic_fn.bracket_end, mut edits)
	m.scan_text(source_file.path, copy.source, FastcStructMonoFile{
		module_name: generic_fn.module_name
		imports:     source_file.header.imports
	}, 0)
}

// mono_nested_instance_canonical names the copy of a nested generic struct argument
// (`AutoHashContext[u32]`, `ds.Box[int]`) as `module.Copy`, when the copy exists.
fn (g &Parser) mono_nested_instance_canonical(argument string, src FastcGenericMethodSource) ?string {
	open := argument.index('[') or { return none }
	mut struct_name := argument[..open]
	mut struct_module := fastc_struct_mono_module(src.module_name)
	if struct_name.contains('.') {
		struct_module = src.imports[struct_name.all_before('.')] or { return none }
		struct_name = struct_name.all_after('.')
	}
	inner := argument[open + 1..argument.len - 1]
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i, c in inner {
		if c == `[` {
			depth++
		} else if c == `]` {
			depth--
		} else if c == `,` && depth == 0 {
			parts << inner[start..i]
			start = i + 1
		}
	}
	parts << inner[start..]
	canonical := parts.map(g.mono_canonical_type_argument(it.trim_space(), src))
	instance := fastc_struct_instance_name(struct_name, canonical)
	if !fastc_source_type_key_exists(g.declared_types, fastc_type_key(struct_module, instance)) {
		return none
	}
	return if struct_module == 'main' { instance } else { '${struct_module}.${instance}' }
}
