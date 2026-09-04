module fastc

import v3.pref
import v3.scanner
import v3.token

// Source-level generic monomorphization. FastC is scanner-only and has no AST,
// so instead of substituting types in a tree it rewrites SOURCE: for every
// concrete instantiation of a generic function it emits a copy with the type
// parameter textually substituted, then rewrites the call sites to the mangled
// name. The concrete copies then flow through the ordinary FastC pipeline with
// no generics left. The pass is a strict no-op unless an entry-module generic
// definition is present.
//
// IMPORTANT: this file is part of gen/fastc and is therefore compiled by the
// FastC self-host, so it must stay inside the V subset FastC can generate — no
// nested-map indexing, no map-value member access, no `.sort(closure)`, no
// `.delete`. Data is kept in flat lists and simple maps.
//
// First increment scope: entry-module free functions with a single type
// parameter, whose every call site resolves to a single concrete type — an
// explicit `f[Concrete](...)` or an `f(literal)` whose first parameter is the
// type parameter. Library-module generics (called with a module qualifier) need
// cross-module resolution and are left to the existing path. A generic whose
// calls cannot all be resolved is left untouched.

// FastcGenericMethodSource keeps the full source text of a generic method with its
// own type parameter (`fn (mut e Enc) m[T](...) { ... }`), so the parser can generate a
// concrete instance ON DEMAND when it resolves a `recv.m(arg)` call whose arg type is
// only known at parse time (e.g. a `$for`-unrolled field access). The source-level pass
// cannot handle those (the arg is comptime), so those generic methods are left in the
// source and collected here. Keyed by `<receiver_type>.<method>`.
struct FastcGenericMethodSource {
	name                       string
	type_param                 string
	receiver_type              string
	module_name                string
	path                       string
	imports                    map[string]string
	return_type_source         string
	first_param_is_type_param  bool
	type_param_parameter_index int
	source                     string
}

// fastc_collect_generic_method_source_chunk indexes the generic methods and free
// functions in one contiguous source range. The parallel wrapper merges ranges in
// source order so duplicate keys retain the serial scan's last-definition behavior.
// FastcGenericScanPartial is one chunk's generic method index plus the
// declaration keyword flags of every file in the chunk, in chunk order.
struct FastcGenericScanPartial {
mut:
	sources map[string]FastcGenericMethodSource
	flags   []FastcSourceScanFlags
}

fn fastc_collect_generic_method_source_chunk(sources []FastcSourceFile, prefs &pref.Preferences, start int, end int) FastcGenericScanPartial {
	mut result := map[string]FastcGenericMethodSource{}
	mut flags := []FastcSourceScanFlags{cap: end - start}
	for i in start .. end {
		source_file := sources[i]
		file_flags := fastc_source_scan_flags(source_file.source)
		flags << file_flags
		if !file_flags.has_generic_fn_syntax {
			continue
		}
		fastc_collect_generic_methods_in_file(source_file, prefs, i, mut result)
	}
	return FastcGenericScanPartial{
		sources: result
		flags: flags
	}
}

// fastc_collect_generic_methods_in_file indexes the generic methods and free
// functions of one file whose scan flags reported generic syntax.
fn fastc_collect_generic_methods_in_file(source_file FastcSourceFile, prefs &pref.Preferences, i int, mut result map[string]FastcGenericMethodSource) {
	module_name := source_file.header.module_name
	for generic in fastc_scan_generic_fns(source_file.source, source_file.path, prefs, i) {
		receiver_type := if generic.receiver_type != '' {
			fastc_c_declared_type_name(fastc_type_key(module_name, generic.receiver_type))
		} else {
			''
		}
		key := if receiver_type != '' {
			'${receiver_type}.${generic.name}'
		} else {
			fastc_function_key(module_name, generic.name)
		}
		result[key] = FastcGenericMethodSource{
			name: generic.name
			type_param: generic.type_param
			receiver_type: receiver_type
			module_name: module_name
			path: source_file.path
			imports: source_file.header.imports
			return_type_source: generic.return_type_source
			first_param_is_type_param: generic.first_param_is_type_param
			type_param_parameter_index: generic.type_param_parameter_index
			source: source_file.source[generic.fn_start..generic.def_end]
		}
	}
}

// mono_argument_type infers the first argument's concrete type for an on-demand
// generic call, reading from a scanner positioned just after the opening `(`.
// Collecting the complete expression lets ordinary FastC inference handle compound
// numeric operands and indexed members as well as literals and bare locals.
fn (g &Parser) mono_argument_type(mut look scanner.Scanner) string {
	mut tokens := []FastcExpressionToken{}
	mut parens := 0
	mut brackets := 0
	mut braces := 0
	for {
		tok := look.scan()
		if tok == .eof || (parens == 0 && brackets == 0 && braces == 0 && tok in [
			.comma,
			.rpar,
		]) {
			break
		}
		tokens << FastcExpressionToken{
			tok: tok
			lit: look.lit
		}
		match tok {
			.lpar { parens++ }
			.rpar { parens-- }
			.lsbr { brackets++ }
			.rsbr { brackets-- }
			.lcbr { braces++ }
			.rcbr { braces-- }
			else {}
		}
	}
	if tokens.len == 0 {
		return ''
	}
	argument_tokens := if tokens[0].tok == .key_mut { tokens[1..] } else { tokens }
	if argument_tokens.len == 1 {
		literal := fastc_infer_literal_type(argument_tokens[0].tok, argument_tokens[0].lit)
		if literal != '' {
			return literal
		}
	}
	inferred := g.infer_expression_type(argument_tokens) or { return '' }
	return fastc_normalize_inferred_type(inferred).trim_right('*')
}

// mono_argument_type_at infers the concrete type from one positional call argument.
// The scanner starts immediately after `(`; preceding arguments are skipped with their
// nested delimiters intact before mono_argument_type examines the selected argument.
fn (g &Parser) mono_argument_type_at(mut look scanner.Scanner, parameter_index int) string {
	for _ in 0 .. parameter_index {
		mut parens := 0
		mut brackets := 0
		mut braces := 0
		for {
			tok := look.scan()
			if tok == .eof || (tok == .rpar && parens == 0 && brackets == 0 && braces == 0) {
				return ''
			}
			if tok == .comma && parens == 0 && brackets == 0 && braces == 0 {
				break
			}
			match tok {
				.lpar { parens++ }
				.rpar { parens-- }
				.lsbr { brackets++ }
				.rsbr { brackets-- }
				.lcbr { braces++ }
				.rcbr { braces-- }
				else {}
			}
		}
	}
	return g.mono_argument_type(mut look)
}

// queue_mono_method records a needed generic-method instance (`<receiver>.<method>` with
// `concrete`), registering a per-Parser signature so the call resolves during rendering
// and queueing it for the drain in run(). Returns the mangled method name to emit.
fn (mut g Parser) queue_mono_method(receiver_type string, method string, concrete string) string {
	mono := fastc_monomorphized_name(method, concrete)
	key := '${g.semantic_type_key(receiver_type)}.${mono}'
	if key !in g.mono_functions {
		source_key := '${receiver_type}.${method}'
		source := g.generic_method_sources[source_key] or { FastcGenericMethodSource{} }
		base_key := '${g.semantic_type_key(receiver_type)}.${method}'
		base := g.functions[base_key] or { FastcFunctionSignature{} }
		mut parameter_types := base.parameter_types.clone()
		parameter_index := source.type_param_parameter_index + 1
		if source.type_param_parameter_index >= 0 && parameter_index < parameter_types.len {
			parameter_types[parameter_index] = fastc_specialized_generic_parameter_type(parameter_types[parameter_index], concrete)
		}
		return_source := source.return_type_source.trim_space()
		mut return_type := base.return_type
		mut return_types := base.return_types.clone()
		mut option_type := base.option_type
		if fastc_generic_type_source_uses_parameter(return_source, source.type_param, g.prefs) {
			return_type = fastc_specialized_generic_result_type(return_type, concrete)
			return_types = fastc_specialized_generic_result_types(return_types, concrete)
			option_type = fastc_specialized_generic_result_type(option_type, concrete)
		}
		if return_source == '' {
			return_type = 'void'
		}
		g.type_memo.clear()
		g.mono_functions[key] = FastcFunctionSignature{
			parameter_types: parameter_types
			parameter_mutability: base.parameter_mutability.clone()
			return_type: return_type
			return_types: return_types
			option_type: option_type
			is_variadic: base.is_variadic
			last_parameter_is_params: base.last_parameter_is_params
			module_name: source.module_name
			path: source.path
			is_public: true
		}
		g.pending_mono << FastcMonoRequest{
			source_key: source_key
			concrete: concrete
		}
	}
	return mono
}

// queue_expression_monomorphization specializes the current call-name token when possible.
// Callers have already rejected names absent from `generic_method_names`.
fn (mut g Parser) queue_expression_monomorphization(tokens []FastcExpressionToken) ?string {
	if !g.selfhost || g.in_generic_placeholder || g.generic_method_sources.len == 0 {
		return none
	}
	// Every recognizer below keys the generic source by the expression's last
	// name, so an expression ending in any other name cannot queue anything.
	if tokens.len == 0 || tokens.last().tok != .name {
		return none
	}
	if mono := g.queue_explicit_mono_method(tokens) {
		return mono
	}
	if mono := g.queue_implicit_mono_method(tokens) {
		return mono
	}
	if mono := g.queue_explicit_mono_function(tokens) {
		return mono
	}
	return g.queue_implicit_mono_function(tokens)
}

// queue_explicit_mono_method recognizes `receiver.method[Type](...)` and queues the
// concrete generic-method body before ordinary method rendering validates the call.
fn (mut g Parser) queue_explicit_mono_method(tokens []FastcExpressionToken) ?string {
	if tokens.len < 3 || tokens.last().tok != .name || tokens[tokens.len - 2].tok != .dot {
		return none
	}
	dot_index := tokens.len - 2
	receiver_start := fastc_method_receiver_start(tokens, dot_index)
	receiver_type := g.infer_expression_type(tokens[receiver_start..dot_index]) or { return none }
	receiver_base := fastc_normalize_inferred_type(receiver_type).trim_right('*')
	method := tokens.last().lit
	if '${receiver_base}.${method}' !in g.generic_method_sources {
		return none
	}
	mut look := g.s
	if look.scan() != .lsbr {
		return none
	}
	first := look.scan()
	concrete, next := fastc_scan_type(mut look, first, g.path, g.module_name, g.imports, g.declared_types, g.selfhost) or { return none }
	after := look.scan()
	if next != .rsbr || after != .lpar {
		return none
	}
	return g.queue_mono_method(receiver_base, method, concrete)
}

// queue_implicit_mono_method recognizes `receiver.method(arg)` for generic methods and
// infers the method's concrete type from the corresponding argument.
fn (mut g Parser) queue_implicit_mono_method(tokens []FastcExpressionToken) ?string {
	if tokens.len < 3 || tokens.last().tok != .name || tokens[tokens.len - 2].tok != .dot {
		return none
	}
	mut look := g.s
	if look.scan() != .lpar {
		return none
	}
	dot_index := tokens.len - 2
	receiver_start := fastc_method_receiver_start(tokens, dot_index)
	receiver_type := g.infer_expression_type(tokens[receiver_start..dot_index]) or { return none }
	receiver_base := fastc_normalize_inferred_type(receiver_type).trim_right('*')
	method := tokens.last().lit
	source_key := '${receiver_base}.${method}'
	if receiver_base == '' || source_key !in g.generic_method_sources {
		return none
	}
	source := g.generic_method_sources[source_key] or { return none }
	base_key := '${g.semantic_type_key(receiver_base)}.${method}'
	base := g.functions[base_key] or { return none }
	parameter_index := source.type_param_parameter_index
	signature_index := parameter_index + 1
	argument_type := g.mono_argument_type_at(mut look, parameter_index)
	concrete := if parameter_index >= 0 && signature_index < base.parameter_types.len {
		fastc_infer_generic_type_from_parameter(base.parameter_types[signature_index], argument_type)
	} else {
		''
	}
	if concrete == '' {
		return none
	}
	return g.queue_mono_method(receiver_base, method, concrete)
}

// queue_explicit_mono_function recognizes an explicit generic free-function call at the
// current name token, registers its concrete signature, and queues its body for the same
// on-demand drain used by generic methods.
fn (mut g Parser) queue_explicit_mono_function(tokens []FastcExpressionToken) ?string {
	if tokens.len == 0 || tokens.last().tok != .name {
		return none
	}
	function_key := g.function_key_for_call(tokens, tokens.len - 1)
	source := g.generic_method_sources[function_key] or { return none }
	if source.receiver_type != '' {
		return none
	}
	mut look := g.s
	if look.scan() != .lsbr {
		return none
	}
	first := look.scan()
	concrete, next := fastc_scan_type(mut look, first, g.path, g.module_name, g.imports, g.declared_types, g.selfhost) or { return none }
	if next != .rsbr || look.scan() != .lpar {
		return none
	}
	return g.queue_mono_function(function_key, concrete)
}

// queue_implicit_mono_function specializes a single-parameter generic free function whose
// argument reveals its type, such as json2's `encode(value)`.
fn (mut g Parser) queue_implicit_mono_function(tokens []FastcExpressionToken) ?string {
	if tokens.len == 0 || tokens.last().tok != .name {
		return none
	}
	function_key := g.function_key_for_call(tokens, tokens.len - 1)
	source := g.generic_method_sources[function_key] or { return none }
	if source.receiver_type != '' || source.type_param.contains(',') || source.type_param_parameter_index < 0 {
		return none
	}
	mut look := g.s
	if look.scan() != .lpar {
		return none
	}
	base := g.functions[function_key] or { return none }
	parameter_index := source.type_param_parameter_index
	if parameter_index >= base.parameter_types.len {
		return none
	}
	argument_type := g.mono_argument_type_at(mut look, parameter_index)
	concrete := fastc_infer_generic_type_from_parameter(base.parameter_types[parameter_index], argument_type)
	if concrete == '' {
		return none
	}
	return g.queue_mono_function(function_key, concrete)
}

fn (mut g Parser) queue_mono_function(function_key string, concrete string) ?string {
	source := g.generic_method_sources[function_key] or { return none }
	mono := fastc_monomorphized_name(source.name, concrete)
	mono_key := fastc_function_key(source.module_name, mono)
	if mono_key !in g.mono_functions {
		base := g.functions[function_key] or { return none }
		mut parameter_types := base.parameter_types.clone()
		if source.type_param_parameter_index >= 0 && source.type_param_parameter_index < parameter_types.len {
			parameter_types[source.type_param_parameter_index] = fastc_specialized_generic_parameter_type(parameter_types[source.type_param_parameter_index], concrete)
		}
		return_source := source.return_type_source.trim_space()
		mut return_type := base.return_type
		mut return_types := base.return_types.clone()
		mut option_type := base.option_type
		if fastc_generic_type_source_uses_parameter(return_source, source.type_param, g.prefs) {
			return_type = fastc_specialized_generic_result_type(return_type, concrete)
			return_types = fastc_specialized_generic_result_types(return_types, concrete)
			option_type = fastc_specialized_generic_result_type(option_type, concrete)
		}
		g.type_memo.clear()
		g.mono_functions[mono_key] = FastcFunctionSignature{
			parameter_types: parameter_types
			parameter_mutability: base.parameter_mutability.clone()
			return_type: return_type
			return_types: return_types
			option_type: option_type
			is_variadic: base.is_variadic
			last_parameter_is_params: base.last_parameter_is_params
			is_public: base.is_public
			is_disabled: base.is_disabled
			module_name: source.module_name
			path: source.path
		}
		g.pending_mono << FastcMonoRequest{
			source_key: function_key
			concrete: concrete
		}
	}
	return mono
}

fn fastc_specialized_generic_parameter_type(erased string, concrete string) string {
	return fastc_specialized_generic_result_type(erased, concrete)
}

// fastc_infer_generic_type_from_parameter extracts the concrete type argument from
// an erased parameter spelling. Exact `T` parameters erase to `voidptr`; composite
// parameters retain that marker inside spellings such as `Array_voidptr` and
// `Map_string_voidptr`.
fn fastc_infer_generic_type_from_parameter(erased string, actual string) string {
	if erased == '' || actual == '' {
		return ''
	}
	erased_base := erased.trim_right('*')
	if erased_base == 'voidptr' {
		return actual.trim_right('*')
	}
	marker := 'voidptr'
	marker_index := erased.index(marker) or { return '' }
	prefix := erased[..marker_index]
	suffix := erased[marker_index + marker.len..]
	if !actual.starts_with(prefix) || !actual.ends_with(suffix) || actual.len < prefix.len + suffix.len {
		return ''
	}
	end := actual.len - suffix.len
	mut concrete := actual[prefix.len..end]
	mut pointers := 0
	for concrete.ends_with('_ptr') {
		concrete = concrete[..concrete.len - '_ptr'.len]
		pointers++
	}
	return concrete + '*'.repeat(pointers)
}

// fastc_specialized_generic_result_type replaces an erased generic placeholder at any
// depth in FastC's composite C spelling. For example, `Array_voidptr` becomes
// `Array_int`, while a direct `voidptr*` preserves its pointer depth as `int*`.
fn fastc_specialized_generic_result_type(erased string, concrete string) string {
	if erased == '' || !erased.contains('voidptr') {
		return erased
	}
	base := erased.trim_right('*')
	if base == 'voidptr' {
		return concrete + '*'.repeat(erased.len - base.len)
	}
	return erased.replace('voidptr', fastc_composite_type_part(concrete))
}

fn fastc_specialized_generic_result_types(erased []string, concrete string) []string {
	mut specialized := []string{cap: erased.len}
	for typ in erased {
		specialized << fastc_specialized_generic_result_type(typ, concrete)
	}
	return specialized
}

fn fastc_generic_type_source_uses_parameter(source string, type_params string, prefs &pref.Preferences) bool {
	if source == '' {
		return false
	}
	mut parameters := map[string]bool{}
	for type_param in type_params.split(',') {
		parameters[type_param] = true
	}
	file := token.File.unindexed('generic_return_type', source.len)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source)
	mut tok := scan.scan()
	for tok != .eof {
		if tok == .name && scan.lit in parameters {
			return true
		}
		tok = scan.scan()
	}
	return false
}

// render_mono_method produces the concrete source of one generic-method instance by
// reusing the generic-function renderer: it re-scans the stored method source to locate
// its `[T]` bracket + name, then substitutes the type parameter and renames the method.
fn (g &Parser) render_mono_method(src FastcGenericMethodSource, concrete string) string {
	generics := fastc_scan_generic_fns(src.source, g.path, g.prefs, 0)
	if generics.len == 0 {
		return ''
	}
	return fastc_render_generic_instance(src.source, generics[0], concrete, g.prefs)
}

// erase_mono_generic_type_arguments removes type arguments from references to generic
// structs in a late on-demand instance. Imported generic structs use FastC's erased
// declaration (their type parameters are `voidptr` fields), just as fastc_scan_type
// already does for signatures. The generated instance must receive the same treatment
// before expression parsing, otherwise `Result[Concrete]{...}` is mistaken for an array
// access followed by an ordinary block.
fn (g &Parser) erase_mono_generic_type_arguments(source string, src FastcGenericMethodSource) string {
	file := token.File.unindexed(src.path, source.len)
	mut scan := scanner.new_scanner(g.prefs, .normal)
	scan.init(file, source)
	mut edits := []FastcSourceEdit{}
	mut tok := scan.scan()
	for tok != .eof {
		if tok != .name {
			tok = scan.scan()
			continue
		}
		first_name := scan.lit
		mut type_key := fastc_resolve_declared_type_key(src.module_name, first_name, src.imports, g.declared_types) or { '' }
		tok = scan.scan()
		if tok == .dot {
			qualified_module := src.imports[first_name] or { '' }
			tok = scan.scan()
			if qualified_module == '' || tok != .name {
				continue
			}
			type_key = fastc_type_key(qualified_module, scan.lit)
			tok = scan.scan()
		}
		if tok != .lsbr || type_key == '' || type_key !in g.declared_types {
			continue
		}
		bracket_start := scan.pos
		mut depth := 1
		for depth > 0 {
			tok = scan.scan()
			if tok == .eof {
				return source
			}
			if tok == .lsbr {
				depth++
			} else if tok == .rsbr {
				depth--
			}
		}
		edits << FastcSourceEdit{
			start: bracket_start
			end: scan.offset
		}
		tok = scan.scan()
	}
	if edits.len == 0 {
		return source
	}
	return fastc_apply_source_edits(source, edits)
}

struct FastcGenericFn {
	name                       string
	type_param                 string
	source_index               int
	fn_start                   int // offset of `fn` (or a leading `pub`/`static`)
	def_end                    int // offset just past the body `}`
	bracket_start              int // offset of `[`
	bracket_end                int // offset just past `]`
	first_param_is_type_param  bool
	type_param_parameter_index int
	// For a method with its own type parameter (`fn (mut e Enc) encode_value[T](val T)`),
	// the receiver's declared type name (`Enc`); empty for a free function. Used to
	// disambiguate a `recv.method(arg)` call from an unrelated same-named method.
	receiver_type string
	// The V return-type text between the parameter `)` and the body `{` (e.g. `string`,
	// `!T`, `[]T`), used to register an on-demand instance's signature.
	return_type_source string
}

struct FastcGenericCall {
	name         string
	source_index int
	start        int // offset of the name token
	end          int // offset just past `[Concrete]` or the name
	concrete     string
}

struct FastcConcretePair {
	name     string
	concrete string
}

struct FastcSourceEdit {
	source_index int
	start        int
	end          int
	replacement  string
}

// FastcGenericMethod is a method whose receiver is a known generic struct, e.g.
// `fn (mut s Stack[T]) push(x T)`. It is monomorphized once per struct
// instantiation: the receiver reference `S[T]` becomes `S__mono_<C>` and the
// type parameter is substituted elsewhere. Method calls resolve by receiver type
// in C, so no call-site rewriting is needed.
struct FastcGenericMethod {
	struct_name        string
	type_param         string
	source_index       int
	fn_start           int // offset of `fn` (or a leading `pub`/`static`)
	def_end            int // offset just past the body `}`
	receiver_ref_start int // offset of `S` in the receiver type `S[T]`
	receiver_ref_end   int // offset just past `]`
}

// fastc_monomorphize_sources monomorphizes entry-module generic functions and
// then generic structs. Structs run second so that a generic function body's
// `S[T]` becomes a concrete `S[int]` (in the emitted instance) before the struct
// pass looks for instantiations.
fn fastc_monomorphize_sources(sources []FastcSourceFile, prefs &pref.Preferences) ![]FastcSourceFile {
	after_functions := fastc_monomorphize_functions(sources, prefs)!
	return fastc_monomorphize_structs(after_functions, prefs)
}

// fastc_monomorphize_functions repeatedly applies one monomorphization pass until
// no generic remains resolvable. A single pass only specializes calls whose concrete
// type is visible in the ORIGINAL source; a nested call inside a generic body
// (`fn outer[T](x T) { inner(x) }`) becomes concrete only in the emitted instance
// (`outer_mono_int`'s `inner(x)` with `x int`), so it is discovered on the next pass.
// Each pass blanks the generics it resolves, so the loop terminates in at most the
// nesting depth of instantiations; the cap is a safety backstop.
fn fastc_monomorphize_functions(sources []FastcSourceFile, prefs &pref.Preferences) ![]FastcSourceFile {
	mut current := sources.clone()
	for _ in 0 .. 24 {
		next, changed := fastc_monomorphize_functions_once(current, prefs)!
		if !changed {
			return next
		}
		current = next.clone()
	}
	return current
}

// fastc_monomorphize_functions_once rewrites `sources` for one round, returning the
// new sources and whether any generic was monomorphized (so the caller can iterate).
fn fastc_monomorphize_functions_once(sources []FastcSourceFile, prefs &pref.Preferences) !([]FastcSourceFile, bool) {
	mut generics := []FastcGenericFn{}
	for i, source_file in sources {
		if source_file.header.module_name !in ['', 'main'] {
			continue
		}
		for generic in fastc_scan_generic_fns(source_file.source, source_file.path, prefs, i) {
			generics << generic
		}
	}
	if generics.len == 0 {
		return sources, false
	}
	// Index generics by name, dropping any duplicated (ambiguous) name.
	mut seen_once := map[string]bool{}
	mut ambiguous := map[string]bool{}
	for generic in generics {
		if generic.name in seen_once {
			ambiguous[generic.name] = true
		}
		seen_once[generic.name] = true
	}
	mut by_name := map[string]FastcGenericFn{}
	for generic in generics {
		if generic.name in ambiguous {
			continue
		}
		// Generic METHODS (own type param) are handled by on-demand parse-time
		// monomorphization (see the drain in run()), not the source-level pass — their
		// bodies recurse through comptime `$for`, which the source-level scanner cannot
		// follow (it would blank the method and leave the `$for`-unrolled call dangling).
		if generic.receiver_type != '' {
			continue
		}
		by_name[generic.name] = generic
	}
	if by_name.len == 0 {
		return sources, false
	}
	// Collect call sites and the concrete type each resolves to.
	mut calls := []FastcGenericCall{}
	mut unresolvable := map[string]bool{}
	mut pairs := []FastcConcretePair{}
	mut seen_pair := map[string]bool{}
	mut has_concrete := map[string]bool{}
	for i, source_file in sources {
		if source_file.header.module_name !in ['', 'main'] {
			continue
		}
		fastc_scan_generic_calls(source_file.source, source_file.path, prefs, i, by_name, mut calls, mut unresolvable, mut pairs, mut seen_pair, mut has_concrete)
	}
	// A generic is monomorphized only when every call resolves.
	mut active := map[string]bool{}
	for name in by_name.keys() {
		if name in unresolvable {
			continue
		}
		if name !in has_concrete {
			continue
		}
		active[name] = true
	}
	if active.len == 0 {
		return sources, false
	}
	// Build a flat edit list: blank each active generic definition, append its
	// concrete copies to that source, and rewrite each resolved call site.
	mut edits := []FastcSourceEdit{}
	mut appends := []string{len: sources.len, init: ''}
	for name in active.keys() {
		generic := by_name[name] or { FastcGenericFn{} }
		definition_file := sources[generic.source_index]
		definition_source := definition_file.source
		edits << FastcSourceEdit{
			source_index: generic.source_index
			start: generic.fn_start
			end: generic.def_end
			replacement: ''
		}
		mut nested_calls := []FastcGenericCall{}
		for call in calls {
			if call.name in active && call.source_index == generic.source_index && call.start >= generic.fn_start && call.end <= generic.def_end {
				nested_calls << call
			}
		}
		mut copies := ''
		for pair in pairs {
			if pair.name != name {
				continue
			}
			copies = copies + '\n' + fastc_render_generic_instance_with_call_rewrites(definition_source, generic, pair.concrete, prefs, nested_calls) + '\n'
		}
		appends[generic.source_index] = appends[generic.source_index] + copies
	}
	for call in calls {
		if call.name !in active {
			continue
		}
		mut inside_removed_generic := false
		for _, generic in by_name {
			if generic.name in active && call.source_index == generic.source_index && call.start >= generic.fn_start && call.end <= generic.def_end {
				inside_removed_generic = true
				break
			}
		}
		if inside_removed_generic {
			continue
		}
		edits << FastcSourceEdit{
			source_index: call.source_index
			start: call.start
			end: call.end
			replacement: fastc_monomorphized_name(call.name, call.concrete)
		}
	}
	mut result := []FastcSourceFile{cap: sources.len}
	for i, source_file in sources {
		mut file_edits := []FastcSourceEdit{}
		for edit in edits {
			if edit.source_index == i {
				file_edits << edit
			}
		}
		mut new_source := source_file.source
		if file_edits.len > 0 {
			new_source = fastc_apply_source_edits(new_source, file_edits)
		}
		if appends[i] != '' {
			new_source = new_source + appends[i]
		}
		result << FastcSourceFile{
			path: source_file.path
			source: new_source
			header: source_file.header
		}
	}
	return result, true
}

// fastc_apply_source_edits applies non-overlapping [start,end) replacements,
// processed right-to-left so earlier offsets stay valid. Sorted with a manual
// selection sort to keep the pass self-hostable.
fn fastc_apply_source_edits(source string, edits []FastcSourceEdit) string {
	mut ordered := edits.clone()
	for i in 0 .. ordered.len {
		mut max_index := i
		current := ordered[i]
		mut max_start := current.start
		for j in i + 1 .. ordered.len {
			candidate := ordered[j]
			if candidate.start > max_start {
				max_index = j
				max_start = candidate.start
			}
		}
		if max_index != i {
			swap := ordered[i]
			ordered[i] = ordered[max_index]
			ordered[max_index] = swap
		}
	}
	mut result := source
	for edit in ordered {
		result = result[..edit.start] + edit.replacement + result[edit.end..]
	}
	return result
}

fn fastc_monomorphized_name(name string, concrete string) string {
	// A single-underscore separator on purpose: `__` is FastC's module separator,
	// so a `Foo__mono_int` receiver would be read as type `mono_int` in module
	// `Foo` and its methods would be mis-named. Multiple type arguments are joined
	// with `,` internally; flatten to `_` for the C identifier.
	return '${name}_mono_${concrete.replace(',', '_')}'
}

// fastc_read_bracket_names reads a comma-separated list of `.name` tokens (the
// current token being the first name) — used for both `[T, U]` type parameters
// and `[int, string]` type arguments — and returns them joined with `,` plus the
// token that follows the list (expected `]`).
fn fastc_read_bracket_names(mut s scanner.Scanner) (string, token.Token) {
	mut names := s.lit
	mut tok := s.scan()
	for tok == .comma {
		tok = s.scan()
		if tok != .name {
			return names, tok
		}
		names = names + ',' + s.lit
		tok = s.scan()
	}
	return names, tok
}

// fastc_type_param_substitutions builds the map from each comma-joined type
// parameter to its corresponding concrete type argument.
fn fastc_type_param_substitutions(type_param string, concrete string) map[string]string {
	type_params := type_param.split(',')
	concretes := concrete.split(',')
	mut substitutions := map[string]string{}
	for index, name in type_params {
		if index < concretes.len {
			substitutions[name] = concretes[index]
		}
	}
	return substitutions
}

// fastc_render_generic_instance produces the concrete source of one instance:
// the generic definition with `[T]` removed, the type parameter substituted, and
// the function renamed.
fn fastc_render_generic_instance(source string, generic FastcGenericFn, concrete string, prefs &pref.Preferences) string {
	return fastc_render_generic_instance_with_call_rewrites(source, generic, concrete, prefs, []FastcGenericCall{})
}

fn fastc_render_generic_instance_with_call_rewrites(source string, generic FastcGenericFn, concrete string, prefs &pref.Preferences, nested_calls []FastcGenericCall) string {
	definition := source[generic.fn_start..generic.def_end]
	base := generic.fn_start
	mut edits := []FastcSourceEdit{}
	edits << FastcSourceEdit{
		start: generic.bracket_start - base
		end: generic.bracket_end - base
		replacement: ''
	}
	for call in nested_calls {
		if call.source_index == generic.source_index && call.start >= generic.fn_start && call.end <= generic.def_end {
			edits << FastcSourceEdit{
				start: call.start - base
				end: call.end - base
				replacement: fastc_monomorphized_name(call.name, call.concrete)
			}
		}
	}
	file := token.File.unindexed('mono', definition.len)
	mut s := scanner.new_scanner(prefs, .normal)
	s.init(file, definition)
	bracket_low := generic.bracket_start - base
	bracket_high := generic.bracket_end - base
	substitutions := fastc_type_param_substitutions(generic.type_param, concrete)
	mut tok := s.scan()
	mut renamed := false
	for tok != .eof {
		if !renamed && tok in [.name, .key_shared] && s.lit == generic.name {
			edits << FastcSourceEdit{
				start: s.pos
				end: s.offset
				replacement: fastc_monomorphized_name(generic.name, concrete)
			}
			renamed = true
		} else if tok == .name && !(s.pos >= bracket_low && s.pos < bracket_high) {
			// Substitute each type parameter everywhere except inside `[...]`, which
			// is removed by the edit above (overlapping edits would corrupt it).
			if replacement := substitutions[s.lit] {
				edits << FastcSourceEdit{
					start: s.pos
					end: s.offset
					replacement: replacement
				}
			}
		}
		tok = s.scan()
	}
	return fastc_apply_source_edits(definition, edits)
}

// fastc_scan_generic_fns finds free functions with a single type parameter whose
// body can be cleanly delimited. Anything it cannot parse confidently is skipped,
// so it never misclassifies ordinary code.
fn fastc_scan_generic_fns(source string, path string, prefs &pref.Preferences, source_index int) []FastcGenericFn {
	mut result := []FastcGenericFn{}
	file := token.File.unindexed(path, source.len)
	mut s := scanner.new_scanner(prefs, .normal)
	s.init(file, source)
	mut previous := token.Token.unknown
	mut previous_pos := 0
	mut tok := s.scan()
	for tok != .eof {
		if tok != .key_fn {
			previous = tok
			previous_pos = s.pos
			tok = s.scan()
			continue
		}
		mut fn_start := s.pos
		if previous in [.key_pub, .key_static] {
			fn_start = previous_pos
		}
		tok = s.scan()
		mut receiver_type := ''
		if tok == .lpar {
			// A method with its own type parameter (`fn (mut e Enc) m[T](...)`). Parse the
			// receiver clause and record its type; a generic-STRUCT receiver (`(e S[T])`) is
			// a different case handled by fastc_scan_generic_methods, so bail on a `[` there.
			tok = s.scan()
			if tok in [.key_mut, .key_shared] {
				tok = s.scan()
			}
			if tok != .name {
				continue
			}
			tok = s.scan()
			for tok == .amp || tok == .and || tok == .mul {
				tok = s.scan()
			}
			if tok != .name {
				continue
			}
			receiver_type = s.lit
			tok = s.scan()
			if tok != .rpar {
				continue
			}
			tok = s.scan()
		}
		if tok !in [.name, .key_shared] {
			continue
		}
		name := s.lit
		tok = s.scan()
		if tok != .lsbr {
			continue
		}
		bracket_start := s.pos
		tok = s.scan()
		if tok != .name {
			continue
		}
		type_param, bracket_next := fastc_read_bracket_names(mut s)
		tok = bracket_next
		if tok != .rsbr {
			continue
		}
		bracket_end := s.offset
		tok = s.scan()
		if tok != .lpar {
			continue
		}
		params_open := s.pos
		mut depth := 0
		mut ok := true
		for {
			if tok == .lpar {
				depth++
			} else if tok == .rpar {
				depth--
				if depth == 0 {
					break
				}
			} else if tok == .eof {
				ok = false
				break
			}
			tok = s.scan()
		}
		if !ok {
			continue
		}
		params_close := s.offset
		tok = s.scan()
		for tok !in [.lcbr, .eof] {
			tok = s.scan()
		}
		if tok != .lcbr {
			continue
		}
		return_type_source := source[params_close..s.pos].trim_space()
		mut brace_depth := 0
		for {
			if tok == .lcbr {
				brace_depth++
			} else if tok == .rcbr {
				brace_depth--
				if brace_depth == 0 {
					break
				}
			} else if tok == .eof {
				ok = false
				break
			}
			tok = s.scan()
		}
		if !ok {
			continue
		}
		def_end := s.offset
		type_param_parameter_index := fastc_params_type_param_index(source[params_open..params_close], type_param, prefs)
		result << FastcGenericFn{
			name: name
			type_param: type_param
			source_index: source_index
			fn_start: fn_start
			def_end: def_end
			bracket_start: bracket_start
			bracket_end: bracket_end
			first_param_is_type_param: type_param_parameter_index == 0
			type_param_parameter_index: type_param_parameter_index
			receiver_type: receiver_type
			return_type_source: return_type_source
		}
		previous = .rcbr
		previous_pos = def_end
		tok = s.scan()
	}
	return result
}

// fastc_params_type_param_index returns the first parameter whose type contains a
// generic type parameter, including composite uses such as `[]T` and `map[string]T`.
fn fastc_params_type_param_index(params_source string, type_param string, prefs &pref.Preferences) int {
	mut type_params := map[string]bool{}
	for name in type_param.split(',') {
		type_params[name] = true
	}
	no_imports := map[string]string{}
	file := token.File.unindexed('params', params_source.len)
	mut s := scanner.new_scanner(prefs, .normal)
	s.init(file, params_source)
	mut tok := s.scan()
	if tok != .lpar {
		return -1
	}
	tok = s.scan()
	mut parameter_index := 0
	for tok !in [.rpar, .eof] {
		shared_is_name := tok == .key_shared && fastc_shared_parameter_is_name(s, 'params', '', no_imports, type_params, prefs.building_v)
		if tok == .key_mut || (tok == .key_shared && !shared_is_name) {
			tok = s.scan()
		}
		if tok !in [.name, .key_shared] {
			return -1
		}
		tok = s.scan()
		mut uses_type_param := false
		mut brackets := 0
		mut parens := 0
		for tok != .eof {
			if tok == .name && s.lit in type_params {
				uses_type_param = true
			}
			if tok == .lsbr {
				brackets++
			} else if tok == .rsbr {
				brackets--
			} else if tok == .lpar {
				parens++
			} else if tok == .rpar && parens > 0 {
				parens--
			} else if brackets == 0 && parens == 0 && tok in [.comma, .rpar] {
				break
			}
			tok = s.scan()
		}
		if uses_type_param {
			return parameter_index
		}
		if tok != .comma {
			break
		}
		parameter_index++
		tok = s.scan()
	}
	return -1
}

fn fastc_infer_literal_type(tok token.Token, lit string) string {
	return match tok {
		.number {
			if lit.contains('.') {
				'f64'
			} else {
				'int'
			}
		}
		.string {
			'string'
		}
		.key_true, .key_false {
			'bool'
		}
		else {
			''
		}
	}
}

// fastc_scan_generic_calls records each call to a known generic and the concrete
// type it resolves to, or marks the generic unresolvable when a call cannot be
// resolved at source level (a non-literal implicit argument, a bare function
// reference, or a malformed explicit type argument).
fn fastc_scan_generic_calls(source string, path string, prefs &pref.Preferences, source_index int, by_name map[string]FastcGenericFn, mut calls []FastcGenericCall, mut unresolvable map[string]bool, mut pairs []FastcConcretePair, mut seen_pair map[string]bool, mut has_concrete map[string]bool) {
	file := token.File.unindexed(path, source.len)
	mut s := scanner.new_scanner(prefs, .normal)
	s.init(file, source)
	// A call inside a generic body may resolve its argument to the ENCLOSING generic's
	// type parameter (e.g. `e.enc_val(val)` with `val T` → concrete `T`). Such a call is
	// only concrete once the enclosing generic is itself specialized (a future fixpoint),
	// so recording it now would emit a `..._mono_T` instance with an unbound `T` and
	// corrupt the edits. Treat a concrete that is any generic's type parameter as
	// unresolvable so the generic is left alone rather than mis-specialized.
	mut type_param_names := map[string]bool{}
	for _, gen in by_name {
		for tp in gen.type_param.split(',') {
			type_param_names[tp] = true
		}
	}
	// Lightweight, function-scoped variable-type table so an `f(x)` call can infer
	// its concrete type from the argument variable (a parameter with a single-name
	// type, or a local declared from a literal). A name recorded with conflicting
	// types is marked ambiguous and no longer resolves (keeping inference safe).
	mut var_types := map[string]string{}
	mut var_ambiguous := map[string]bool{}
	mut pending_var := ''
	// `name := Foo{...}` is confirmed a struct-literal local only once the `{` after
	// the type name is seen; hold the candidate until the next token.
	mut pending_struct_var := ''
	mut pending_struct_type := ''
	mut previous := token.Token.unknown
	mut previous_lit := ''
	// The identifier just before a `.` — the receiver of a `recv.method(...)` call, used
	// to look up the receiver's type and confirm a generic-method call.
	mut before_dot_receiver := ''
	mut tok := s.scan()
	for tok != .eof {
		if tok == .dot {
			before_dot_receiver = previous_lit
		}
		if pending_struct_var != '' {
			if tok == .lcbr {
				fastc_record_var(pending_struct_var, pending_struct_type, mut var_types, mut var_ambiguous)
			}
			pending_struct_var = ''
			pending_struct_type = ''
			// Fall through: process the current token normally.
		}
		if pending_var != '' {
			literal_type := fastc_infer_literal_type(tok, s.lit)
			if literal_type != '' {
				fastc_record_var(pending_var, literal_type, mut var_types, mut var_ambiguous)
			} else if tok == .name {
				// Possibly `name := Type{...}`; defer confirmation to the next token.
				pending_struct_var = pending_var
				pending_struct_type = s.lit
			}
			pending_var = ''
			// Fall through: this token may still start (or be) a generic call.
		}
		if tok == .key_fn {
			var_types = map[string]string{}
			var_ambiguous = map[string]bool{}
			pending_var = ''
			pending_struct_var = ''
			pending_struct_type = ''
			tok = fastc_collect_params(mut s, mut var_types, prefs)
			previous = .rpar
			previous_lit = ''
			continue
		}
		if tok == .decl_assign && previous == .name {
			pending_var = previous_lit
			previous = tok
			previous_lit = ''
			tok = s.scan()
			continue
		}
		mut should_skip := tok != .name || s.lit !in by_name || previous == .key_fn
		if !should_skip {
			probe := by_name[s.lit] or { FastcGenericFn{} }
			if probe.receiver_type != '' {
				// A generic METHOD resolves only as a `recv.method(...)` call whose receiver's
				// declared type matches — never a bare or foreign-typed call.
				recv_type := var_types[before_dot_receiver] or { '' }
				should_skip = previous != .dot || recv_type != probe.receiver_type
			} else {
				// A free generic function is never a `.`-qualified field/module access.
				should_skip = previous == .dot
			}
		}
		if should_skip {
			previous = tok
			previous_lit = s.lit
			tok = s.scan()
			continue
		}
		name := s.lit
		name_pos := s.pos
		name_end := s.offset
		// `name` is guaranteed present (checked at the loop head); the default is
		// never taken and is written plainly to stay self-hostable.
		generic := by_name[name] or { FastcGenericFn{} }
		tok = s.scan()
		if tok == .lsbr {
			tok = s.scan()
			if tok == .name {
				concrete, bracket_next := fastc_read_bracket_names(mut s)
				tok = bracket_next
				if tok == .rsbr {
					after := s.offset
					if concrete in type_param_names {
						unresolvable[name] = true
						previous = .rsbr
						previous_lit = ''
						tok = s.scan()
						continue
					}
					fastc_record_concrete(name, concrete, mut pairs, mut seen_pair, mut has_concrete)
					calls << FastcGenericCall{
						name: name
						source_index: source_index
						start: name_pos
						end: after
						concrete: concrete
					}
					previous = .rsbr
					previous_lit = ''
					tok = s.scan()
					continue
				}
			}
			unresolvable[name] = true
			previous = tok
			previous_lit = ''
			continue
		}
		if tok == .lpar {
			// Implicit argument inference only resolves a single type parameter;
			// multi-parameter generics must use an explicit `f[A, B](...)`.
			if !generic.first_param_is_type_param || generic.type_param.contains(',') {
				unresolvable[name] = true
				previous = .lpar
				previous_lit = ''
				tok = s.scan()
				continue
			}
			tok = s.scan()
			mut concrete := fastc_infer_literal_type(tok, s.lit)
			if concrete == '' && tok in [.name, .key_shared] {
				// A bare variable argument (`f(x)`) or a struct-literal argument
				// (`f(Foo{...})`); peek one token to tell them apart.
				argument_name := s.lit
				next_token := s.scan()
				if next_token == .lcbr {
					concrete = argument_name
				} else if next_token in [.comma, .rpar] && argument_name !in var_ambiguous {
					concrete = var_types[argument_name] or { '' }
				}
				previous = tok
				previous_lit = argument_name
				tok = next_token
			} else {
				previous = tok
				previous_lit = ''
			}
			if concrete == '' || concrete in type_param_names {
				unresolvable[name] = true
				continue
			}
			fastc_record_concrete(name, concrete, mut pairs, mut seen_pair, mut has_concrete)
			calls << FastcGenericCall{
				name: name
				source_index: source_index
				start: name_pos
				end: name_end
				concrete: concrete
			}
			continue
		}
		// The generic name appears somewhere other than a call (e.g. passed as a
		// value); source-level monomorphization cannot resolve it.
		unresolvable[name] = true
		previous = tok
		previous_lit = ''
		continue
	}
}

// fastc_collect_params records the single-name-typed parameters of the function
// whose `fn` keyword was just consumed, so `f(param)` calls can infer their
// concrete type. It returns the token just past the parameter list.
fn fastc_collect_params(mut s scanner.Scanner, mut var_types map[string]string, prefs &pref.Preferences) token.Token {
	no_imports := map[string]string{}
	no_declared_types := map[string]bool{}
	mut tok := s.scan()
	if tok == .lpar {
		// Method receiver `(recv Type)`: skip it.
		mut depth := 0
		for {
			if tok == .lpar {
				depth++
			} else if tok == .rpar {
				depth--
				if depth == 0 {
					break
				}
			} else if tok == .eof {
				return tok
			}
			tok = s.scan()
		}
		tok = s.scan()
	}
	if tok != .name {
		return tok
	}
	tok = s.scan()
	if tok == .lsbr {
		// Generic `[T]`: skip it.
		mut depth := 0
		for {
			if tok == .lsbr {
				depth++
			} else if tok == .rsbr {
				depth--
				if depth == 0 {
					break
				}
			} else if tok == .eof {
				return tok
			}
			tok = s.scan()
		}
		tok = s.scan()
	}
	if tok != .lpar {
		return tok
	}
	tok = s.scan()
	for tok !in [.rpar, .eof] {
		shared_is_name := tok == .key_shared && fastc_shared_parameter_is_name(s, 'params', '', no_imports, no_declared_types, prefs.building_v)
		if tok == .key_mut || (tok == .key_shared && !shared_is_name) {
			tok = s.scan()
		}
		if tok !in [.name, .key_shared] {
			tok = fastc_skip_to_param_boundary(mut s, tok)
			if tok == .comma {
				tok = s.scan()
				continue
			}
			break
		}
		parameter_name := s.lit
		tok = s.scan()
		if tok == .name {
			type_name := s.lit
			next_token := s.scan()
			if next_token in [.comma, .rpar] {
				var_types[parameter_name] = type_name
			}
			tok = next_token
		}
		tok = fastc_skip_to_param_boundary(mut s, tok)
		if tok == .comma {
			tok = s.scan()
			continue
		}
		break
	}
	if tok == .rpar {
		return s.scan()
	}
	return tok
}

// fastc_skip_to_param_boundary advances to the next top-level `,` or the closing
// `)` of the parameter list, honoring nested brackets.
fn fastc_skip_to_param_boundary(mut s scanner.Scanner, start token.Token) token.Token {
	mut tok := start
	mut depth := 0
	for {
		if tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if tok in [.rsbr, .rcbr] {
			if depth > 0 {
				depth--
			}
		} else if tok == .rpar {
			if depth == 0 {
				return tok
			}
			depth--
		} else if tok == .comma && depth == 0 {
			return tok
		} else if tok == .eof {
			return tok
		}
		tok = s.scan()
	}
	return tok
}

// fastc_record_var records a variable's concrete type, marking it ambiguous (and
// thereafter unusable for inference) if it is later seen with a different type.
fn fastc_record_var(name string, typ string, mut var_types map[string]string, mut var_ambiguous map[string]bool) {
	if name in var_ambiguous {
		return
	}
	existing := var_types[name] or { '' }
	if existing == '' {
		var_types[name] = typ
	} else if existing != typ {
		var_ambiguous[name] = true
	}
}

fn fastc_record_concrete(name string, concrete string, mut pairs []FastcConcretePair, mut seen_pair map[string]bool, mut has_concrete map[string]bool) {
	pair_key := '${name}#${concrete}'
	if pair_key !in seen_pair {
		seen_pair[pair_key] = true
		pairs << FastcConcretePair{
			name: name
			concrete: concrete
		}
	}
	has_concrete[name] = true
}

// fastc_monomorphize_structs rewrites entry-module generic structs the same way
// as functions: it emits a concrete copy per instantiation `S[Concrete]` and
// rewrites every reference. A struct is left untouched unless every reference is
// a concrete instantiation and its body has no nested generic type references.
fn fastc_monomorphize_structs(sources []FastcSourceFile, prefs &pref.Preferences) []FastcSourceFile {
	mut structs := []FastcGenericFn{}
	for i, source_file in sources {
		if source_file.header.module_name !in ['', 'main'] {
			continue
		}
		for generic in fastc_scan_generic_structs(source_file.source, source_file.path, prefs, i) {
			structs << generic
		}
	}
	if structs.len == 0 {
		return sources
	}
	mut seen_once := map[string]bool{}
	mut ambiguous := map[string]bool{}
	for generic in structs {
		if generic.name in seen_once {
			ambiguous[generic.name] = true
		}
		seen_once[generic.name] = true
	}
	mut by_name := map[string]FastcGenericFn{}
	for generic in structs {
		if generic.name in ambiguous {
			continue
		}
		by_name[generic.name] = generic
	}
	if by_name.len == 0 {
		return sources
	}
	// Collect generic methods (`fn (recv S[T]) m(...)`) of the known generic
	// structs, the positions of their receiver type references (skipped by the
	// instance scan below), and any struct whose method cannot be monomorphized
	// (which marks that struct unresolvable).
	mut methods := []FastcGenericMethod{}
	mut receiver_skip := map[string]bool{}
	mut method_problematic := map[string]bool{}
	for i, source_file in sources {
		if source_file.header.module_name !in ['', 'main'] {
			continue
		}
		fastc_scan_generic_methods(source_file.source, source_file.path, prefs, i, by_name, mut methods, mut receiver_skip, mut method_problematic)
	}
	mut refs := []FastcGenericCall{}
	mut unresolvable := map[string]bool{}
	mut pairs := []FastcConcretePair{}
	mut seen_pair := map[string]bool{}
	mut has_concrete := map[string]bool{}
	for i, source_file in sources {
		if source_file.header.module_name !in ['', 'main'] {
			continue
		}
		fastc_scan_generic_struct_instances(source_file.source, source_file.path, prefs, i, by_name, receiver_skip, mut refs, mut unresolvable, mut pairs, mut seen_pair, mut has_concrete)
	}
	for name in method_problematic.keys() {
		unresolvable[name] = true
	}
	mut active := map[string]bool{}
	for name in by_name.keys() {
		if name in unresolvable {
			continue
		}
		if name !in has_concrete {
			continue
		}
		active[name] = true
	}
	if active.len == 0 {
		return sources
	}
	mut edits := []FastcSourceEdit{}
	mut appends := []string{len: sources.len, init: ''}
	for name in active.keys() {
		generic := by_name[name] or { FastcGenericFn{} }
		definition_file := sources[generic.source_index]
		definition_source := definition_file.source
		edits << FastcSourceEdit{
			source_index: generic.source_index
			start: generic.fn_start
			end: generic.def_end
			replacement: ''
		}
		mut copies := ''
		for pair in pairs {
			if pair.name != name {
				continue
			}
			copies = copies + '\n' + fastc_render_generic_instance(definition_source, generic, pair.concrete, prefs) + '\n'
		}
		appends[generic.source_index] = appends[generic.source_index] + copies
	}
	// Emit each generic method once per instantiation of its struct and blank the
	// original generic method definition.
	for method in methods {
		if method.struct_name !in active {
			continue
		}
		method_file := sources[method.source_index]
		method_source := method_file.source
		edits << FastcSourceEdit{
			source_index: method.source_index
			start: method.fn_start
			end: method.def_end
			replacement: ''
		}
		mut method_copies := ''
		for pair in pairs {
			if pair.name != method.struct_name {
				continue
			}
			method_copies = method_copies + '\n' + fastc_render_generic_method(method_source, method, pair.concrete, prefs) + '\n'
		}
		appends[method.source_index] = appends[method.source_index] + method_copies
	}
	for ref in refs {
		if ref.name !in active {
			continue
		}
		edits << FastcSourceEdit{
			source_index: ref.source_index
			start: ref.start
			end: ref.end
			replacement: fastc_monomorphized_name(ref.name, ref.concrete)
		}
	}
	mut result := []FastcSourceFile{cap: sources.len}
	for i, source_file in sources {
		mut file_edits := []FastcSourceEdit{}
		for edit in edits {
			if edit.source_index == i {
				file_edits << edit
			}
		}
		mut new_source := source_file.source
		if file_edits.len > 0 {
			new_source = fastc_apply_source_edits(new_source, file_edits)
		}
		if appends[i] != '' {
			new_source = new_source + appends[i]
		}
		result << FastcSourceFile{
			path: source_file.path
			source: new_source
			header: source_file.header
		}
	}
	return result
}

// fastc_scan_generic_structs finds `struct S[T] { ... }` definitions with a single
// type parameter and no nested generic type reference in the body.
fn fastc_scan_generic_structs(source string, path string, prefs &pref.Preferences, source_index int) []FastcGenericFn {
	mut result := []FastcGenericFn{}
	file := token.File.unindexed(path, source.len)
	mut s := scanner.new_scanner(prefs, .normal)
	s.init(file, source)
	mut previous := token.Token.unknown
	mut previous_pos := 0
	mut tok := s.scan()
	for tok != .eof {
		if tok != .key_struct {
			previous = tok
			previous_pos = s.pos
			tok = s.scan()
			continue
		}
		mut struct_start := s.pos
		if previous in [.key_pub, .key_static] {
			struct_start = previous_pos
		}
		tok = s.scan()
		if tok != .name {
			continue
		}
		name := s.lit
		tok = s.scan()
		if tok != .lsbr {
			continue
		}
		bracket_start := s.pos
		tok = s.scan()
		if tok != .name {
			continue
		}
		type_param, bracket_next := fastc_read_bracket_names(mut s)
		tok = bracket_next
		if tok != .rsbr {
			continue
		}
		bracket_end := s.offset
		tok = s.scan()
		if tok != .lcbr {
			continue
		}
		body_open := s.pos
		mut brace_depth := 0
		mut ok := true
		for {
			if tok == .lcbr {
				brace_depth++
			} else if tok == .rcbr {
				brace_depth--
				if brace_depth == 0 {
					break
				}
			} else if tok == .eof {
				ok = false
				break
			}
			tok = s.scan()
		}
		if !ok {
			continue
		}
		def_end := s.offset
		if !fastc_body_has_nested_generic(source[body_open..def_end], prefs) {
			result << FastcGenericFn{
				name: name
				type_param: type_param
				source_index: source_index
				fn_start: struct_start
				def_end: def_end
				bracket_start: bracket_start
				bracket_end: bracket_end
			}
		}
		previous = .rcbr
		previous_pos = def_end
		tok = s.scan()
	}
	return result
}

// fastc_body_has_nested_generic reports whether a struct body contains a nested
// generic type reference (`Foo[X]`). Plain arrays (`[]T`, `[N]T`) and maps
// (`map[K]V`) are allowed; only a `Name[...]` with non-empty brackets counts.
fn fastc_body_has_nested_generic(body string, prefs &pref.Preferences) bool {
	file := token.File.unindexed('body', body.len)
	mut s := scanner.new_scanner(prefs, .normal)
	s.init(file, body)
	mut previous := token.Token.unknown
	mut previous_lit := ''
	mut tok := s.scan()
	for tok != .eof {
		if tok == .lsbr && previous == .name && previous_lit != 'map' {
			next_token := s.scan()
			if next_token != .rsbr {
				return true
			}
			previous = .rsbr
			previous_lit = ''
			tok = s.scan()
			continue
		}
		previous = tok
		previous_lit = s.lit
		tok = s.scan()
	}
	return false
}

// fastc_all_concrete_type_args reports whether every comma-joined argument of a
// (possibly multi-parameter) instantiation is a concrete type.
fn fastc_all_concrete_type_args(concrete string) bool {
	for part in concrete.split(',') {
		if !fastc_is_concrete_type_arg(part) {
			return false
		}
	}
	return true
}

// fastc_is_concrete_type_arg reports whether a type-argument spelling is a
// concrete type (a primitive, or a multi-character capitalized type name) rather
// than a single-letter type parameter like `T`.
fn fastc_is_concrete_type_arg(name string) bool {
	if fastc_primitive_c_type(name) != none {
		return true
	}
	if name.len > 1 {
		first := name[0]
		if first >= u8(65) && first <= u8(90) {
			return true
		}
	}
	return false
}

// fastc_scan_generic_struct_instances records each `S[Concrete]` reference to a
// known generic struct, or marks the struct unresolvable when a reference is not
// a plain concrete instantiation (a type-parameter argument, a complex type
// argument, or a bare reference).
fn fastc_scan_generic_struct_instances(source string, path string, prefs &pref.Preferences, source_index int, by_name map[string]FastcGenericFn, receiver_skip map[string]bool, mut refs []FastcGenericCall, mut unresolvable map[string]bool, mut pairs []FastcConcretePair, mut seen_pair map[string]bool, mut has_concrete map[string]bool) {
	file := token.File.unindexed(path, source.len)
	mut s := scanner.new_scanner(prefs, .normal)
	s.init(file, source)
	mut previous := token.Token.unknown
	mut tok := s.scan()
	for tok != .eof {
		if tok != .name || s.lit !in by_name || previous in [.dot, .key_struct] {
			previous = tok
			tok = s.scan()
			continue
		}
		name := s.lit
		name_pos := s.pos
		// A `S[T]` that is a generic method receiver is handled by method
		// monomorphization, not an instantiation; skip it.
		if '${source_index}#${name_pos}' in receiver_skip {
			previous = tok
			tok = s.scan()
			continue
		}
		tok = s.scan()
		if tok != .lsbr {
			unresolvable[name] = true
			previous = tok
			continue
		}
		tok = s.scan()
		if tok != .name {
			unresolvable[name] = true
			previous = tok
			continue
		}
		concrete, bracket_next := fastc_read_bracket_names(mut s)
		tok = bracket_next
		if tok != .rsbr {
			unresolvable[name] = true
			previous = tok
			continue
		}
		after := s.offset
		if !fastc_all_concrete_type_args(concrete) {
			unresolvable[name] = true
			previous = .rsbr
			tok = s.scan()
			continue
		}
		fastc_record_concrete(name, concrete, mut pairs, mut seen_pair, mut has_concrete)
		refs << FastcGenericCall{
			name: name
			source_index: source_index
			start: name_pos
			end: after
			concrete: concrete
		}
		previous = .rsbr
		tok = s.scan()
	}
}

// fastc_scan_generic_methods finds methods whose receiver is a known generic
// struct (`fn (recv S[T]) m(...)`). A method that references any generic struct
// beyond its receiver, or carries its own type parameters, cannot be handled by
// simple substitution and marks its struct problematic (later unresolvable).
fn fastc_scan_generic_methods(source string, path string, prefs &pref.Preferences, source_index int, by_name map[string]FastcGenericFn, mut methods []FastcGenericMethod, mut receiver_skip map[string]bool, mut method_problematic map[string]bool) {
	file := token.File.unindexed(path, source.len)
	mut s := scanner.new_scanner(prefs, .normal)
	s.init(file, source)
	mut previous := token.Token.unknown
	mut previous_pos := 0
	mut tok := s.scan()
	for tok != .eof {
		if tok != .key_fn {
			previous = tok
			previous_pos = s.pos
			tok = s.scan()
			continue
		}
		mut fn_start := s.pos
		if previous in [.key_pub, .key_static] {
			fn_start = previous_pos
		}
		tok = s.scan()
		if tok != .lpar {
			continue
		}
		tok = s.scan()
		if tok in [.key_mut, .key_shared] {
			tok = s.scan()
		}
		if tok != .name {
			continue
		}
		tok = s.scan()
		if tok != .name || s.lit !in by_name {
			continue
		}
		struct_name := s.lit
		receiver_ref_start := s.pos
		tok = s.scan()
		if tok != .lsbr {
			continue
		}
		tok = s.scan()
		if tok != .name {
			continue
		}
		type_param, bracket_next := fastc_read_bracket_names(mut s)
		tok = bracket_next
		if tok != .rsbr {
			continue
		}
		receiver_ref_end := s.offset
		tok = s.scan()
		if tok != .rpar {
			continue
		}
		tok = s.scan()
		if tok != .name {
			continue
		}
		tok = s.scan()
		if tok != .lpar {
			// A method with its own type parameters (`m[U](...)`) is beyond this pass.
			method_problematic[struct_name] = true
			continue
		}
		mut has_nested := false
		mut prev_body := token.Token.unknown
		mut prev_lit := ''
		mut depth := 0
		for {
			if tok == .lpar {
				depth++
			} else if tok == .rpar {
				depth--
				if depth == 0 {
					break
				}
			} else if tok == .eof {
				break
			} else if tok == .lsbr && prev_body == .name && prev_lit in by_name {
				has_nested = true
			}
			prev_body = tok
			prev_lit = s.lit
			tok = s.scan()
		}
		tok = s.scan()
		for tok !in [.lcbr, .eof] {
			if tok == .lsbr && prev_body == .name && prev_lit in by_name {
				has_nested = true
			}
			prev_body = tok
			prev_lit = s.lit
			tok = s.scan()
		}
		if tok != .lcbr {
			continue
		}
		mut brace_depth := 0
		mut ok := true
		for {
			if tok == .lcbr {
				brace_depth++
			} else if tok == .rcbr {
				brace_depth--
				if brace_depth == 0 {
					break
				}
			} else if tok == .eof {
				ok = false
				break
			} else if tok == .lsbr && prev_body == .name && prev_lit in by_name {
				has_nested = true
			}
			prev_body = tok
			prev_lit = s.lit
			tok = s.scan()
		}
		if !ok {
			continue
		}
		def_end := s.offset
		if has_nested {
			method_problematic[struct_name] = true
		} else {
			methods << FastcGenericMethod{
				struct_name: struct_name
				type_param: type_param
				source_index: source_index
				fn_start: fn_start
				def_end: def_end
				receiver_ref_start: receiver_ref_start
				receiver_ref_end: receiver_ref_end
			}
			receiver_skip['${source_index}#${receiver_ref_start}'] = true
		}
		previous = .rcbr
		previous_pos = def_end
		tok = s.scan()
	}
}

// fastc_render_generic_method produces the concrete source of one method
// instance: the receiver reference `S[T]` becomes `S__mono_<C>` and the type
// parameter is substituted everywhere else (parameters, return type, body).
fn fastc_render_generic_method(source string, method FastcGenericMethod, concrete string, prefs &pref.Preferences) string {
	definition := source[method.fn_start..method.def_end]
	base := method.fn_start
	receiver_low := method.receiver_ref_start - base
	receiver_high := method.receiver_ref_end - base
	mut edits := []FastcSourceEdit{}
	edits << FastcSourceEdit{
		start: receiver_low
		end: receiver_high
		replacement: fastc_monomorphized_name(method.struct_name, concrete)
	}
	file := token.File.unindexed('method', definition.len)
	mut s := scanner.new_scanner(prefs, .normal)
	s.init(file, definition)
	substitutions := fastc_type_param_substitutions(method.type_param, concrete)
	mut tok := s.scan()
	for tok != .eof {
		if tok == .name && !(s.pos >= receiver_low && s.pos < receiver_high) {
			if replacement := substitutions[s.lit] {
				edits << FastcSourceEdit{
					start: s.pos
					end: s.offset
					replacement: replacement
				}
			}
		}
		tok = s.scan()
	}
	return fastc_apply_source_edits(definition, edits)
}
