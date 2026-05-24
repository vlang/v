// Lifetime validation for V2 compiler.
//
// V2's syntax allows annotating references and generic params with named
// lifetimes (e.g. `fn foo[^a](x &^a Foo) &^a Bar`). The parser captures the
// annotations, the transformer filters them out of generic instantiation,
// and the C backend erases them — but until this file there was no
// semantic check, so a typo like `&^b` in the body of a `[^a]`-generic
// function was silently passed through.
//
// This module is the Phase 1 lifetime checker. It runs once per build,
// gated by `-d ownership` (the same flag that turns on move/borrow
// tracking), and validates:
//
//   1. Every lifetime name used in a signature (parameter type, return
//      type, struct field type, method receiver) refers to a lifetime that
//      is declared in the enclosing generic-param list. Catches typos and
//      omitted declarations.
//   2. No duplicate lifetime declarations within a single signature.
//   3. A return type may only mention a lifetime that is also mentioned by
//      at least one parameter (including the receiver for methods). This
//      is the signature-level "no dangling reference" rule: a function
//      cannot synthesize an output reference out of thin air, so the
//      returned reference must be tied to an input that the caller owns.
//
// Phase 1 does NOT implement body-level borrow analysis (variables
// outliving their referents, lifetime subtyping). Those require
// control-flow-aware tracking and are left for a follow-up.
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
fn (mut c Checker) lifetime_validate_fn_decl(decl ast.FnDecl) {
	declared := c.lifetime_collect_declared(decl.typ.generic_params, 'fn `${decl.name}`')
	// Treat the receiver as the 0th parameter for the input-lifetime set.
	mut input_lifetimes := map[string]bool{}
	if decl.is_method {
		used := lifetime_collect_used_in_type(decl.receiver.typ)
		for name in used {
			c.lifetime_assert_declared(name, declared, decl.receiver.pos, 'fn `${decl.name}`')
			input_lifetimes[name] = true
		}
	}
	for param in decl.typ.params {
		used := lifetime_collect_used_in_type(param.typ)
		for name in used {
			c.lifetime_assert_declared(name, declared, param.pos, 'fn `${decl.name}`')
			input_lifetimes[name] = true
		}
	}
	if decl.typ.return_type !is ast.EmptyExpr {
		used := lifetime_collect_used_in_type(decl.typ.return_type)
		for name in used {
			c.lifetime_assert_declared(name, declared, decl.pos, 'fn `${decl.name}`')
			if name !in input_lifetimes {
				c.lifetime_error_return_unbound(name, decl.name, decl.pos)
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

fn lifetime_walk_type_inner(t ast.Type, mut out map[string]bool, depth int) {
	match t {
		ast.PointerType {
			if t.lifetime.len > 0 {
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
