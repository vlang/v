// Lifetime validation for V2 compiler.
//
// V2's syntax allows annotating references and generic params with named
// lifetimes (e.g. `fn foo[^a](x &^a Foo) &^a Bar`). The parser captures the
// annotations, the transformer filters them out of generic instantiation,
// and the C backend erases them — but until this file there was no
// semantic check, so a typo like `&^b` in the body of a `[^a]`-generic
// function was silently passed through.
//
// This module is the lifetime checker. It runs once per build, gated by
// `-d ownership` (the same flag that turns on move/borrow tracking), and
// validates:
//
//   1. Every explicit lifetime name used in a signature (parameter type,
//      return type, struct field type, method receiver) refers to a
//      lifetime that is declared in the enclosing generic-param list.
//      Catches typos and omitted declarations.
//   2. No duplicate lifetime declarations within a single signature.
//   3. A return type may only mention an explicit lifetime that is also
//      mentioned by at least one parameter (including the receiver). This
//      is the signature-level "no dangling reference" rule: a function
//      cannot synthesize an output reference out of thin air.
//
// Phase 2 (this file): Lifetime elision and anonymous lifetimes.
//
//   Elision is OPT-IN per function. The user opts in by declaring at
//   least one lifetime generic parameter on the function (e.g. `fn
//   foo[^a](...)`). Existing V code with bare `&T` references in its
//   signatures is unaffected — elision rules only apply once the user
//   adds `[^a, ...]`. This is necessary because V allows returning
//   `&T` from value-receiver methods (`fn (a Array) head() &T`) under
//   its GC memory model; Rust-style strict elision would flag these
//   as errors. Opt-in lets us add real checking incrementally without
//   breaking the stdlib.
//
//   Once opted in, the validator applies Rust-style elision to bare
//   `&T` (no `^`) and the anonymous form `&^_ T`:
//
//     - Each elided input reference is assigned its own synthetic
//       input lifetime.
//     - For elided output references the rules are, in priority
//       order:
//         a. If the fn is a method whose receiver is a reference, the
//            receiver's lifetime is used.
//         b. Otherwise, if the signature has exactly one input
//            lifetime (explicit or synthetic), it is used.
//         c. Otherwise: the elision is ambiguous and the validator
//            errors, asking the user to annotate.
//
//   So `fn first[^a](h &Holder) &Holder` (a opted-in signature with
//   elided refs) is accepted under rule (b), but `fn pick[^a](a &A,
//   b &B) &Out` errors as ambiguous and the user must write the
//   intended lifetime on the return position.
//
// Variance: V2 currently has no reference subtyping (no `&'long → &'short`
// coercion), so variance rules are a no-op. Immutable references are
// effectively covariant by construction; there are no mutable shared
// references whose invariance would matter. Documented here for the
// follow-up phase that introduces lifetime subtyping.
//
// Still NOT implemented: body-level borrow analysis (variables outliving
// their referents). Requires control-flow-aware tracking.
module types

import v2.ast
import v2.errors
import v2.token

// lifetime_validate_files runs the Phase 1 validator across every fn and
// struct declaration in the given files. Errors abort the build with a
// helpful diagnostic; no warnings are emitted (unused lifetimes are
// tolerated for now, matching Rust's behaviour pre-`-W unused_lifetimes`).
fn (mut c Checker) lifetime_validate_files(files []ast.File) {
	for file in files {
		for stmt in file.stmts {
			c.lifetime_validate_stmt(stmt)
		}
	}
}

fn (mut c Checker) lifetime_validate_files_from_flat(flat &ast.FlatAst) {
	for ff in flat.files {
		stmts := ast.Cursor{
			flat: unsafe { flat }
			id:   ff.file_id
		}.list_at(2)
		for i in 0 .. stmts.len() {
			stmt := stmts.at(i)
			match stmt.kind() {
				.stmt_fn_decl {
					c.lifetime_validate_fn_decl_cursor(stmt)
				}
				.stmt_struct_decl {
					c.lifetime_validate_struct_decl_cursor(stmt)
				}
				else {}
			}
		}
	}
}

fn (mut c Checker) lifetime_validate_stmt(stmt ast.Stmt) {
	match stmt {
		ast.FnDecl {
			c.lifetime_validate_fn_decl(stmt)
		}
		ast.StructDecl {
			c.lifetime_validate_struct_decl(stmt)
		}
		else {}
	}
}

// lifetime_validate_fn_decl checks a single function or method declaration.
// Validates explicit lifetimes (declared/undeclared, return-unbound) and
// applies elision rules to elided refs (`&T`) and anonymous lifetimes
// (`&^_ T`).
fn (mut c Checker) lifetime_validate_fn_decl(decl ast.FnDecl) {
	declared := c.lifetime_collect_declared(decl.typ.generic_params, 'fn `${decl.name}`')
	mut input_lifetimes := map[string]bool{}
	mut receiver_has_ref := false
	mut elision_synth_seq := 0

	// Receiver counts as the 0th input.
	if decl.is_method {
		used := lifetime_collect_used_in_type(decl.receiver.typ)
		for name in used {
			c.lifetime_assert_declared(name, declared, decl.receiver.pos, 'fn `${decl.name}`')
			input_lifetimes[name] = true
			receiver_has_ref = true
		}
		recv_elided := lifetime_count_elided_slots(decl.receiver.typ)
		for _ in 0 .. recv_elided {
			synth := '\$self_${elision_synth_seq}'
			elision_synth_seq++
			input_lifetimes[synth] = true
			receiver_has_ref = true
		}
	}

	for param in decl.typ.params {
		used := lifetime_collect_used_in_type(param.typ)
		for name in used {
			c.lifetime_assert_declared(name, declared, param.pos, 'fn `${decl.name}`')
			input_lifetimes[name] = true
		}
		elided := lifetime_count_elided_slots(param.typ)
		for _ in 0 .. elided {
			synth := '\$elided_${elision_synth_seq}'
			elision_synth_seq++
			input_lifetimes[synth] = true
		}
	}

	if decl.typ.return_type !is ast.EmptyExpr {
		used := lifetime_collect_used_in_type(decl.typ.return_type)
		for name in used {
			c.lifetime_assert_declared(name, declared, decl.pos, 'fn `${decl.name}`')
		}
		return_ref_used := lifetime_collect_reference_lifetimes_in_type(decl.typ.return_type)
		for name in return_ref_used {
			if name !in input_lifetimes {
				c.lifetime_error_return_unbound(name, decl.name, decl.pos)
			}
		}
		// Elision rules apply ONLY to opted-in signatures (those that
		// declare at least one lifetime generic param). Without opt-in,
		// bare `&T` retains V's legacy unchecked semantics — this is
		// necessary because V's stdlib has many value-receiver methods
		// that return references (e.g. `array.data_header() &ArrayDataHeader`),
		// which would otherwise all fail Rust-style strict elision.
		if declared.len == 0 {
			return
		}
		ret_elided := lifetime_count_elided_slots(decl.typ.return_type)
		if ret_elided > 0 {
			// Elision rules, in priority order:
			//   a. Method receiver lifetime takes the return position.
			//   b. Otherwise, exactly one input lifetime → use it.
			//   c. Otherwise → ambiguous or impossible, error out.
			if receiver_has_ref {
				// OK — receiver lifetime covers the elided return.
			} else if input_lifetimes.len == 1 {
				// OK — single input lifetime is implicitly the return's.
			} else if input_lifetimes.len == 0 {
				c.lifetime_error_elision_no_input(decl.name, decl.pos)
			} else {
				c.lifetime_error_elision_ambiguous(decl.name, decl.pos, input_lifetimes)
			}
		}
	}
}

fn (mut c Checker) lifetime_validate_fn_decl_cursor(decl ast.Cursor) {
	typ := decl.edge(1)
	declared := c.lifetime_collect_declared_cursor(typ.list_at(0), 'fn `${decl.name()}`')
	mut input_lifetimes := map[string]bool{}
	mut receiver_has_ref := false
	mut elision_synth_seq := 0

	if decl.flag(ast.flag_is_method) {
		receiver := decl.edge(0)
		receiver_typ := receiver.edge(0)
		used := lifetime_collect_used_in_type_cursor(receiver_typ)
		for name in used {
			c.lifetime_assert_declared(name, declared, receiver.pos(), 'fn `${decl.name()}`')
			input_lifetimes[name] = true
			receiver_has_ref = true
		}
		recv_elided := lifetime_count_elided_slots_cursor(receiver_typ)
		for _ in 0 .. recv_elided {
			synth := '\$self_${elision_synth_seq}'
			elision_synth_seq++
			input_lifetimes[synth] = true
			receiver_has_ref = true
		}
	}

	params := typ.list_at(1)
	for i in 0 .. params.len() {
		param := params.at(i)
		param_typ := param.edge(0)
		used := lifetime_collect_used_in_type_cursor(param_typ)
		for name in used {
			c.lifetime_assert_declared(name, declared, param.pos(), 'fn `${decl.name()}`')
			input_lifetimes[name] = true
		}
		elided := lifetime_count_elided_slots_cursor(param_typ)
		for _ in 0 .. elided {
			synth := '\$elided_${elision_synth_seq}'
			elision_synth_seq++
			input_lifetimes[synth] = true
		}
	}

	return_typ := typ.edge(2)
	if return_typ.is_valid() && return_typ.kind() != .expr_empty {
		used := lifetime_collect_used_in_type_cursor(return_typ)
		for name in used {
			c.lifetime_assert_declared(name, declared, decl.pos(), 'fn `${decl.name()}`')
		}
		return_ref_used := lifetime_collect_reference_lifetimes_in_type_cursor(return_typ)
		for name in return_ref_used {
			if name !in input_lifetimes {
				c.lifetime_error_return_unbound(name, decl.name(), decl.pos())
			}
		}
		if declared.len == 0 {
			return
		}
		ret_elided := lifetime_count_elided_slots_cursor(return_typ)
		if ret_elided > 0 {
			if receiver_has_ref {
			} else if input_lifetimes.len == 1 {
			} else if input_lifetimes.len == 0 {
				c.lifetime_error_elision_no_input(decl.name(), decl.pos())
			} else {
				c.lifetime_error_elision_ambiguous(decl.name(), decl.pos(), input_lifetimes)
			}
		}
	}
}

// lifetime_validate_struct_decl checks lifetimes in struct field types.
// A struct may declare lifetimes in its generic-param list; every lifetime
// referenced from a field type must be declared there.
fn (mut c Checker) lifetime_validate_struct_decl(decl ast.StructDecl) {
	declared := c.lifetime_collect_declared(decl.generic_params, 'struct `${decl.name}`')
	for field in decl.fields {
		used := lifetime_collect_used_in_type(field.typ)
		for name in used {
			c.lifetime_assert_declared(name, declared, decl.pos, 'struct `${decl.name}`')
		}
	}
}

fn (mut c Checker) lifetime_validate_struct_decl_cursor(decl ast.Cursor) {
	declared := c.lifetime_collect_declared_cursor(decl.list_at(3), 'struct `${decl.name()}`')
	fields := decl.list_at(4)
	for i in 0 .. fields.len() {
		field := fields.at(i)
		used := lifetime_collect_used_in_type_cursor(field.edge(0))
		for name in used {
			c.lifetime_assert_declared(name, declared, decl.pos(), 'struct `${decl.name()}`')
		}
	}
}

// lifetime_collect_declared extracts the lifetime names declared in a
// generic-param list (e.g. `[^a, T, ^b]` → ['a', 'b']). Reports duplicate
// declarations as errors.
fn (mut c Checker) lifetime_collect_declared(params []ast.Expr, ctx_label string) map[string]bool {
	mut declared := map[string]bool{}
	for gp in params {
		if gp is ast.LifetimeExpr {
			le := gp as ast.LifetimeExpr
			if le.name in declared {
				c.lifetime_error_duplicate(le.name, ctx_label, le.pos)
			}
			declared[le.name] = true
		}
	}
	return declared
}

fn (mut c Checker) lifetime_collect_declared_cursor(params ast.CursorList, ctx_label string) map[string]bool {
	mut declared := map[string]bool{}
	for i in 0 .. params.len() {
		gp := params.at(i)
		if gp.kind() == .expr_lifetime {
			name := gp.name()
			if name in declared {
				c.lifetime_error_duplicate(name, ctx_label, gp.pos())
			}
			declared[name] = true
		}
	}
	return declared
}

// lifetime_assert_declared errors if `name` is not present in `declared`.
fn (mut c Checker) lifetime_assert_declared(name string, declared map[string]bool, pos token.Pos, ctx_label string) {
	if name in declared {
		return
	}
	mut known := []string{}
	for k, _ in declared {
		known << '^' + k
	}
	known.sort()
	if pos.is_valid() {
		file := c.file_set.file(pos)
		errors.error('undeclared lifetime `^${name}` in ${ctx_label}', errors.details(file,
			file.position(pos), 2), .error, file.position(pos))
	} else {
		eprintln('error: undeclared lifetime `^${name}` in ${ctx_label}')
	}
	if known.len > 0 {
		eprintln('  --> declared lifetimes in scope: ${known.join(', ')}')
		eprintln('help: did you mean one of the above, or add `^${name}` to the generic params?')
	} else {
		eprintln('  --> no lifetimes declared in this scope')
		eprintln('help: add `[^${name}]` to the generic params of ${ctx_label}')
	}
	exit(1)
}

fn (mut c Checker) lifetime_error_duplicate(name string, ctx_label string, pos token.Pos) {
	if pos.is_valid() {
		file := c.file_set.file(pos)
		errors.error('lifetime `^${name}` declared more than once in ${ctx_label}', errors.details(file,
			file.position(pos), 2), .error, file.position(pos))
	} else {
		eprintln('error: lifetime `^${name}` declared more than once in ${ctx_label}')
	}
	exit(1)
}

fn (mut c Checker) lifetime_error_return_unbound(name string, fn_name string, pos token.Pos) {
	if pos.is_valid() {
		file := c.file_set.file(pos)
		errors.error('return type of fn `${fn_name}` references lifetime `^${name}` that does not appear in any parameter', errors.details(file,
			file.position(pos), 2), .error, file.position(pos))
	} else {
		eprintln('error: return type of fn `${fn_name}` references lifetime `^${name}` that does not appear in any parameter')
	}
	eprintln('  --> a returned reference must be tied to an input the caller owns; otherwise it would dangle')
	eprintln('help: either accept a parameter borrowed as `&^${name} T`, or remove the `^${name}` from the return type')
	exit(1)
}

fn (mut c Checker) lifetime_error_elision_no_input(fn_name string, pos token.Pos) {
	if pos.is_valid() {
		file := c.file_set.file(pos)
		errors.error('fn `${fn_name}` returns a reference but takes no reference parameters; cannot infer return lifetime', errors.details(file,
			file.position(pos), 2), .error, file.position(pos))
	} else {
		eprintln('error: fn `${fn_name}` returns a reference but takes no reference parameters; cannot infer return lifetime')
	}
	eprintln('  --> elided return references must borrow from an input reference')
	eprintln('help: either take a `&T` parameter the return value can borrow from, or return an owned value')
	exit(1)
}

fn (mut c Checker) lifetime_error_elision_ambiguous(fn_name string, pos token.Pos, inputs map[string]bool) {
	mut names := []string{}
	for k, _ in inputs {
		// User-facing: hide synthetic lifetimes (start with `$`) behind a
		// generic "<elided>" so the diagnostic doesn't leak internal IDs.
		if k.starts_with('\$') {
			names << '<elided>'
		} else {
			names << '^' + k
		}
	}
	names.sort()
	if pos.is_valid() {
		file := c.file_set.file(pos)
		errors.error('cannot infer return lifetime for fn `${fn_name}`: multiple input lifetimes in scope (${names.join(', ')})', errors.details(file,
			file.position(pos), 2), .error, file.position(pos))
	} else {
		eprintln('error: cannot infer return lifetime for fn `${fn_name}`: multiple input lifetimes in scope (${names.join(', ')})')
	}
	eprintln('  --> elision rule 2 only applies when there is exactly one input lifetime')
	eprintln('help: annotate the return type with the lifetime you intend, e.g. `&^a T`')
	exit(1)
}

// lifetime_collect_used_in_type walks a type expression and returns the
// set of lifetime names it references (bare, without the `^` sigil).
// This is the workhorse that finds `^a` inside `&^a Foo`,
// `Match[Foo[^a, ^b]]`, `fn(&^a T) &^a U`, etc.
fn lifetime_collect_used_in_type(expr ast.Expr) []string {
	mut out := map[string]bool{}
	lifetime_walk_type(expr, mut out, 0)
	mut names := []string{}
	for name, _ in out {
		names << name
	}
	return names
}

fn lifetime_collect_used_in_type_cursor(expr ast.Cursor) []string {
	mut out := map[string]bool{}
	lifetime_walk_type_cursor(expr, mut out, 0)
	mut names := []string{}
	for name, _ in out {
		names << name
	}
	return names
}

fn lifetime_collect_reference_lifetimes_in_type(expr ast.Expr) []string {
	mut out := map[string]bool{}
	lifetime_walk_reference_type(expr, mut out, 0)
	mut names := []string{}
	for name, _ in out {
		names << name
	}
	return names
}

fn lifetime_collect_reference_lifetimes_in_type_cursor(expr ast.Cursor) []string {
	mut out := map[string]bool{}
	lifetime_walk_reference_type_cursor(expr, mut out, 0)
	mut names := []string{}
	for name, _ in out {
		names << name
	}
	return names
}

fn lifetime_walk_type(expr ast.Expr, mut out map[string]bool, depth int) {
	if depth > 32 {
		// Defensive: pathological / self-referential generics. The parser
		// shouldn't produce these, but bail out to avoid infinite recursion.
		return
	}
	// Type expressions (PointerType, ArrayType, ...) are stored as
	// `Expr(Type(...))` — Type is a sumtype of all type variants and is
	// itself a variant of Expr. Unwrap one level before dispatching.
	if expr is ast.Type {
		lifetime_walk_type_inner(expr, mut out, depth)
		return
	}
	match expr {
		ast.LifetimeExpr {
			out[expr.name] = true
		}
		ast.GenericArgOrIndexExpr {
			lifetime_walk_type(expr.lhs, mut out, depth + 1)
			lifetime_walk_type(expr.expr, mut out, depth + 1)
		}
		else {}
	}
}

fn lifetime_walk_type_cursor(expr ast.Cursor, mut out map[string]bool, depth int) {
	if depth > 32 || !expr.is_valid() {
		return
	}
	match expr.kind() {
		.expr_lifetime {
			out[expr.name()] = true
		}
		.expr_generic_arg_or_index {
			lifetime_walk_type_cursor(expr.edge(0), mut out, depth + 1)
			lifetime_walk_type_cursor(expr.edge(1), mut out, depth + 1)
		}
		.typ_pointer {
			lifetime := expr.name()
			if lifetime.len > 0 && lifetime != '_' {
				out[lifetime] = true
			}
			lifetime_walk_type_cursor(expr.edge(0), mut out, depth + 1)
		}
		.typ_array {
			lifetime_walk_type_cursor(expr.edge(0), mut out, depth + 1)
		}
		.typ_array_fixed {
			lifetime_walk_type_cursor(expr.edge(1), mut out, depth + 1)
		}
		.typ_map {
			lifetime_walk_type_cursor(expr.edge(0), mut out, depth + 1)
			lifetime_walk_type_cursor(expr.edge(1), mut out, depth + 1)
		}
		.typ_option, .typ_result, .typ_thread {
			lifetime_walk_type_cursor(expr.edge(0), mut out, depth + 1)
		}
		.typ_tuple {
			for i in 0 .. expr.edge_count() {
				lifetime_walk_type_cursor(expr.edge(i), mut out, depth + 1)
			}
		}
		.typ_generic {
			for i in 1 .. expr.edge_count() {
				lifetime_walk_type_cursor(expr.edge(i), mut out, depth + 1)
			}
		}
		.typ_fn {
			params := expr.list_at(1)
			for i in 0 .. params.len() {
				lifetime_walk_type_cursor(params.at(i).edge(0), mut out, depth + 1)
			}
			lifetime_walk_type_cursor(expr.edge(2), mut out, depth + 1)
		}
		else {}
	}
}

fn lifetime_walk_reference_type(expr ast.Expr, mut out map[string]bool, depth int) {
	if depth > 32 {
		return
	}
	if expr is ast.Type {
		lifetime_walk_reference_type_inner(expr, mut out, depth)
		return
	}
	match expr {
		ast.GenericArgOrIndexExpr {
			lifetime_walk_reference_type(expr.lhs, mut out, depth + 1)
			lifetime_walk_reference_type(expr.expr, mut out, depth + 1)
		}
		else {}
	}
}

fn lifetime_walk_reference_type_cursor(expr ast.Cursor, mut out map[string]bool, depth int) {
	if depth > 32 || !expr.is_valid() {
		return
	}
	match expr.kind() {
		.expr_generic_arg_or_index {
			lifetime_walk_reference_type_cursor(expr.edge(0), mut out, depth + 1)
			lifetime_walk_reference_type_cursor(expr.edge(1), mut out, depth + 1)
		}
		.typ_pointer {
			lifetime := expr.name()
			if lifetime.len > 0 && lifetime != '_' {
				out[lifetime] = true
			}
			lifetime_walk_reference_type_cursor(expr.edge(0), mut out, depth + 1)
		}
		.typ_array {
			lifetime_walk_reference_type_cursor(expr.edge(0), mut out, depth + 1)
		}
		.typ_array_fixed {
			lifetime_walk_reference_type_cursor(expr.edge(1), mut out, depth + 1)
		}
		.typ_map {
			lifetime_walk_reference_type_cursor(expr.edge(0), mut out, depth + 1)
			lifetime_walk_reference_type_cursor(expr.edge(1), mut out, depth + 1)
		}
		.typ_option, .typ_result, .typ_thread {
			lifetime_walk_reference_type_cursor(expr.edge(0), mut out, depth + 1)
		}
		.typ_tuple {
			for i in 0 .. expr.edge_count() {
				lifetime_walk_reference_type_cursor(expr.edge(i), mut out, depth + 1)
			}
		}
		.typ_generic {
			for i in 1 .. expr.edge_count() {
				lifetime_walk_reference_type_cursor(expr.edge(i), mut out, depth + 1)
			}
		}
		.typ_fn {
			params := expr.list_at(1)
			for i in 0 .. params.len() {
				lifetime_walk_reference_type_cursor(params.at(i).edge(0), mut out, depth + 1)
			}
			lifetime_walk_reference_type_cursor(expr.edge(2), mut out, depth + 1)
		}
		else {}
	}
}

// lifetime_count_elided_slots returns the number of reference slots in a
// type that lack an explicit named lifetime. Both bare `&T` (no `^`) and
// the anonymous lifetime form `&^_ T` count. Used by elision rules to
// decide how many fresh synthetic lifetimes to mint for the signature.
fn lifetime_count_elided_slots(expr ast.Expr) int {
	mut counter := [0]
	lifetime_walk_count(expr, mut counter, 0)
	return counter[0]
}

fn lifetime_count_elided_slots_cursor(expr ast.Cursor) int {
	mut counter := [0]
	lifetime_walk_count_cursor(expr, mut counter, 0)
	return counter[0]
}

fn lifetime_walk_count(expr ast.Expr, mut counter []int, depth int) {
	if depth > 32 {
		return
	}
	if expr is ast.Type {
		lifetime_walk_count_inner(expr, mut counter, depth)
	}
}

fn lifetime_walk_count_cursor(expr ast.Cursor, mut counter []int, depth int) {
	if depth > 32 || !expr.is_valid() {
		return
	}
	match expr.kind() {
		.typ_pointer {
			lifetime := expr.name()
			if lifetime.len == 0 || lifetime == '_' {
				counter[0]++
			}
			lifetime_walk_count_cursor(expr.edge(0), mut counter, depth + 1)
		}
		.typ_array {
			lifetime_walk_count_cursor(expr.edge(0), mut counter, depth + 1)
		}
		.typ_array_fixed {
			lifetime_walk_count_cursor(expr.edge(1), mut counter, depth + 1)
		}
		.typ_map {
			lifetime_walk_count_cursor(expr.edge(0), mut counter, depth + 1)
			lifetime_walk_count_cursor(expr.edge(1), mut counter, depth + 1)
		}
		.typ_option, .typ_result, .typ_thread {
			lifetime_walk_count_cursor(expr.edge(0), mut counter, depth + 1)
		}
		.typ_tuple {
			for i in 0 .. expr.edge_count() {
				lifetime_walk_count_cursor(expr.edge(i), mut counter, depth + 1)
			}
		}
		.typ_generic {
			for i in 1 .. expr.edge_count() {
				lifetime_walk_count_cursor(expr.edge(i), mut counter, depth + 1)
			}
		}
		else {}
	}
}

fn lifetime_walk_count_inner(t ast.Type, mut counter []int, depth int) {
	match t {
		ast.PointerType {
			if t.lifetime.len == 0 || t.lifetime == '_' {
				counter[0]++
			}
			lifetime_walk_count(t.base_type, mut counter, depth + 1)
		}
		ast.ArrayType {
			lifetime_walk_count(t.elem_type, mut counter, depth + 1)
		}
		ast.ArrayFixedType {
			lifetime_walk_count(t.elem_type, mut counter, depth + 1)
		}
		ast.MapType {
			lifetime_walk_count(t.key_type, mut counter, depth + 1)
			lifetime_walk_count(t.value_type, mut counter, depth + 1)
		}
		ast.OptionType {
			lifetime_walk_count(t.base_type, mut counter, depth + 1)
		}
		ast.ResultType {
			lifetime_walk_count(t.base_type, mut counter, depth + 1)
		}
		ast.TupleType {
			for inner in t.types {
				lifetime_walk_count(inner, mut counter, depth + 1)
			}
		}
		ast.GenericType {
			for p in t.params {
				lifetime_walk_count(p, mut counter, depth + 1)
			}
		}
		ast.FnType {
			// Nested fn types are their own scope; their refs don't bubble
			// up as elision slots of the outer signature.
		}
		else {}
	}
}

fn lifetime_walk_reference_type_inner(t ast.Type, mut out map[string]bool, depth int) {
	match t {
		ast.PointerType {
			if t.lifetime.len > 0 && t.lifetime != '_' {
				out[t.lifetime] = true
			}
			lifetime_walk_reference_type(t.base_type, mut out, depth + 1)
		}
		ast.ArrayType {
			lifetime_walk_reference_type(t.elem_type, mut out, depth + 1)
		}
		ast.ArrayFixedType {
			lifetime_walk_reference_type(t.elem_type, mut out, depth + 1)
		}
		ast.MapType {
			lifetime_walk_reference_type(t.key_type, mut out, depth + 1)
			lifetime_walk_reference_type(t.value_type, mut out, depth + 1)
		}
		ast.OptionType {
			lifetime_walk_reference_type(t.base_type, mut out, depth + 1)
		}
		ast.ResultType {
			lifetime_walk_reference_type(t.base_type, mut out, depth + 1)
		}
		ast.TupleType {
			for inner in t.types {
				lifetime_walk_reference_type(inner, mut out, depth + 1)
			}
		}
		ast.GenericType {
			for p in t.params {
				lifetime_walk_reference_type(p, mut out, depth + 1)
			}
		}
		ast.FnType {
			for p in t.params {
				lifetime_walk_reference_type(p.typ, mut out, depth + 1)
			}
			lifetime_walk_reference_type(t.return_type, mut out, depth + 1)
		}
		else {}
	}
}

fn lifetime_walk_type_inner(t ast.Type, mut out map[string]bool, depth int) {
	match t {
		ast.PointerType {
			// Anonymous (`^_`) is not a "named" lifetime; it's elided.
			// Counted by lifetime_count_elided_slots, not here.
			if t.lifetime.len > 0 && t.lifetime != '_' {
				out[t.lifetime] = true
			}
			lifetime_walk_type(t.base_type, mut out, depth + 1)
		}
		ast.ArrayType {
			lifetime_walk_type(t.elem_type, mut out, depth + 1)
		}
		ast.ArrayFixedType {
			lifetime_walk_type(t.elem_type, mut out, depth + 1)
		}
		ast.MapType {
			lifetime_walk_type(t.key_type, mut out, depth + 1)
			lifetime_walk_type(t.value_type, mut out, depth + 1)
		}
		ast.OptionType {
			lifetime_walk_type(t.base_type, mut out, depth + 1)
		}
		ast.ResultType {
			lifetime_walk_type(t.base_type, mut out, depth + 1)
		}
		ast.TupleType {
			for inner in t.types {
				lifetime_walk_type(inner, mut out, depth + 1)
			}
		}
		ast.GenericType {
			for p in t.params {
				lifetime_walk_type(p, mut out, depth + 1)
			}
		}
		ast.FnType {
			for p in t.params {
				lifetime_walk_type(p.typ, mut out, depth + 1)
			}
			lifetime_walk_type(t.return_type, mut out, depth + 1)
		}
		else {}
	}
}
