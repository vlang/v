// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fmt

import os
import strings
import v.ast
import v.ast.walker
import v.util
import v.pref

const break_points = [0, 35, 60, 85, 93, 100]! // when to break a line depending on the penalty
const max_len = break_points[break_points.len - 1]
const bs = '\\'

fn call_arg_spread_str(arg ast.CallArg) string {
	return match arg.expr {
		ast.ArrayDecompose {
			decompose := arg.expr as ast.ArrayDecompose
			'...${decompose.expr.str()}'
		}
		else {
			arg.str()
		}
	}
}

@[minify]
pub struct Fmt {
pub:
	pref &pref.Preferences = unsafe { nil }
pub mut:
	file                     ast.File
	table                    &ast.Table = unsafe { nil }
	is_debug                 bool
	out                      strings.Builder
	indent                   int
	empty_line               bool
	line_len                 int    // the current line length, Note: it counts \t as 4 spaces, and starts at 0 after f.writeln
	buffering                bool   // disables line wrapping for exprs that will be analyzed later
	par_level                int    // how many parentheses are put around the current expression
	array_init_break         []bool // line breaks after elements in hierarchy level of multi dimensional array
	array_init_depth         int    // current level of hierarchy in array init
	single_line_if           bool
	cur_mod                  string
	import_pos               int               // position of the last import in the resulting string
	mod2alias                map[string]string // for `import time as t`, will contain: 'time'=>'t'
	mod2syms                 map[string]string // import time { now } 'time.now'=>'now'
	implied_import_str       string            // ​imports that the user's code uses but omitted to import explicitly
	processed_imports        []string
	has_import_stmt          bool
	use_short_fn_args        bool
	single_line_fields       bool // should struct fields be on a single line
	in_lambda_depth          int
	inside_const             bool
	inside_unsafe            bool
	inside_comptime_if       bool
	is_assign                bool
	is_index_expr            bool
	is_mbranch_expr          bool // match a { x...y { } }
	is_struct_init           bool
	is_array_init            bool
	fn_scope                 &ast.Scope = unsafe { nil }
	wsinfix_depth            int
	format_state             FormatState
	source_text              string // can be set by `echo "println('hi')" | v fmt`, i.e. when processing source not from a file, but from stdin. In this case, it will contain the entire input text. You can use f.file.path otherwise, and read from that file.
	global_processed_imports []string
	branch_processed_imports []string
	has_json2_import         bool // the file has a top-level `json2` import; used when migrating `import json`
	json2_prefix             string = 'json2' // local name of `json2` at call sites (its alias, if imported as one)
	keep_json_unmigrated     bool // the file has a json call the migration cannot rewrite losslessly (`encode_pretty`, or a `decode` with a commented type arg); its json usage is left as-is (see process_file_imports)
	migrate_json_calls       bool // the file actually imports the legacy `json` module and it is being migrated, so `json.encode`/`json.decode` calls should be rewritten (guards against `import json2 as json`, whose json2 calls carry the same call kind)
	migrate_json2            bool // opt-in master switch for the json->json2 migration (FmtOptions.migrate_json2); when false, plain `v fmt` leaves all json usage untouched
	is_translated_module     bool // @[translated]
	is_c_function            bool // C.func(...)
}

@[params]
pub struct FmtOptions {
pub:
	source_text   string
	migrate_json2 bool // opt in to rewriting deprecated `json` module usage to `json2`; off by default so plain `v fmt` never changes json semantics
}

pub fn fmt(file ast.File, mut table ast.Table, pref_ &pref.Preferences, is_debug bool, options FmtOptions) string {
	mut f := Fmt{
		file:     file
		table:    table
		pref:     pref_
		is_debug: is_debug
		out:      strings.new_builder(1000)
	}
	f.source_text = options.source_text
	f.migrate_json2 = options.migrate_json2
	f.process_file_imports(file)
	// Compensate for indent increase of toplevel stmts done in `f.stmts()`.
	f.indent--
	f.stmts(file.stmts)
	f.indent++
	res := f.out.str().trim_space() + '\n'

	// `implied_imports` should append to end of `import` block
	if res.len == 1 {
		return f.implied_import_str + '\n'
	}
	if res.len <= f.import_pos {
		if f.implied_import_str.len > 0 {
			return res + '\n' + f.implied_import_str + '\n'
		}
		return res
	}
	mut import_start_pos := f.import_pos
	if f.import_pos == 0 && file.stmts.len > 1 {
		// Check shebang.
		stmt := file.stmts[1]
		if stmt is ast.ExprStmt && stmt.expr is ast.Comment
			&& (stmt.expr as ast.Comment).text.starts_with('#!') {
			import_start_pos = stmt.pos.len
		}
	}
	if f.has_import_stmt || f.implied_import_str.len == 0 {
		return res[..import_start_pos] + f.implied_import_str + res[import_start_pos..]
	} else {
		return res[..import_start_pos] + f.implied_import_str + '\n' + res[import_start_pos..]
	}
}

/*
// vfmt has a special type_to_str which calls Table.type_to_str, but does extra work.
// Having it here and not in Table saves cpu cycles when not running the compiler in vfmt mode.
pub fn (f &Fmt) type_to_str_using_aliases(typ ast.Type, import_aliases map[string]string) string {
	mut s := f.type_to_str_using_aliases(typ, import_aliases)
	if s.contains('Result') {
		println('${s}')
	}
	return s
}

pub fn (f &Fmt) type_to_str(typ ast.Type) string {
	return f.type_to_str(typ)
}
*/
fn (f &Fmt) type_to_str_using_aliases(typ ast.Type, import_aliases map[string]string) string {
	if f.table.new_int && typ == ast.int_type && (f.is_translated_module || f.is_c_function) {
		return f.type_to_str_using_aliases(ast.i32_type, import_aliases)
	}
	return f.table.type_to_str_using_aliases(typ, import_aliases)
}

fn (f &Fmt) type_to_str(typ ast.Type) string {
	if f.table.new_int && typ == ast.int_type && (f.is_translated_module || f.is_c_function) {
		return 'i32'
	}
	return f.table.type_to_str(typ)
}

pub fn (mut f Fmt) process_file_imports(file &ast.File) {
	mut sb := strings.new_builder(128)
	for imp in file.implied_imports {
		sb.writeln('import ${imp}')
	}
	f.implied_import_str = sb.str()

	// The json->json2 migration is opt-in (FmtOptions.migrate_json2, the `-migrate-json2`
	// vfmt flag). When it is off, plain `v fmt` must leave all json usage untouched, so skip
	// the whole detection block — no import rewrite, no call rewrite, no scanning I/O.
	if f.migrate_json2 {
		// Detect an existing top-level `json2` import, which the json->json2 migration
		// reuses instead of emitting a duplicate `import json2` (see import_stmt). The
		// migrated call sites are rewritten with that import's local name (its alias if
		// `import json2 as x`, else `json2`), so they stay in scope and idempotent.
		// Only top-level imports qualify: a branch-local (`$if { import json2 }`) one is
		// nested inside a comptime-if statement rather than being a direct file.stmt, so
		// it is not picked up here and a top-level `import json2` is emitted instead.
		mut json2_alias_is_blank := false
		mut toplevel_json2_positions := []int{}
		mut toplevel_json_positions := []int{}
		for stmt in file.stmts {
			if stmt is ast.Import && stmt.source_name == 'json' {
				toplevel_json_positions << stmt.pos.pos
			}
			if stmt is ast.Import && stmt.source_name == 'json2' {
				toplevel_json2_positions << stmt.pos.pos
				if !f.has_json2_import && !json2_alias_is_blank {
					if stmt.alias == '_' {
						// `import json2 as _` only silences unused-import warnings; `_` is not a
						// usable qualifier and does not bring `json2` into scope, and a second
						// plain `import json2` would be a duplicate import. So there is no way
						// to emit valid `json2.x` calls — leave the file's json usage unmigrated.
						json2_alias_is_blank = true
					} else {
						f.has_json2_import = true
						f.json2_prefix = if stmt.alias != '' {
							stmt.alias
						} else {
							stmt.source_name.all_after_last('.')
						}
					}
				}
			}
		}
		// A `json2` import inside a top-level `$if` branch is present in file.imports but
		// is not a direct file.stmt (it is nested in a comptime-if). Migrating `import
		// json` would add a plain top-level `import json2`, which then coexists with the
		// branch import and trips the checker's duplicate-module error. The vfmt dedup
		// cannot be relied on: a selective/aliased branch import has a different formatted
		// string, and even a plain branch import emitted *before* the top-level one goes
		// to branch_processed_imports (not global_processed_imports), so the later
		// top-level import is not deduped. So leave the file unmigrated whenever any
		// branch-local json2 import is present.
		mut has_branch_local_json2 := false
		for imp in file.imports {
			if imp.source_name == 'json2' && imp.pos.pos !in toplevel_json2_positions {
				has_branch_local_json2 = true
				break
			}
		}
		// A `json` import inside a top-level `$if` branch has the same problem: migrating it
		// emits `import json2` into branch_processed_imports, which is not deduped against a
		// top-level migrated `import json2` in global_processed_imports (e.g. a distinct
		// top-level `import json as old` plus a branch `import json { encode }` both become
		// `import json2`). Leave the file unmigrated when a branch-local json import exists.
		mut has_branch_local_json := false
		for imp in file.imports {
			if imp.source_name == 'json' && imp.pos.pos !in toplevel_json_positions {
				has_branch_local_json = true
				break
			}
		}
		// The migrated qualifier (f.json2_prefix) must be free. If another module is
		// imported under that local name — e.g. `import rand as json2` — adding
		// `import json2` and rewriting calls to `json2.x` would create two imports with
		// the same alias (the checker's duplicate-import check compares aliases), and the
		// calls would resolve to the wrong module. Leave the file unmigrated in that case.
		mut json2_name_taken := false
		for imp in file.imports {
			if imp.source_name != 'json2' && imp.alias == f.json2_prefix {
				json2_name_taken = true
				break
			}
		}
		// When the file already has a `json2` import, the migrated `import json` is dropped
		// (merged) rather than rewritten. That drop path only re-emits the import's
		// next_comments, so a same-line comment on the import (e.g. `import json // note`)
		// would be lost. Leave such a file unmigrated so the comment is preserved.
		mut json_import_has_line_comment := false
		if f.has_json2_import {
			for imp in file.imports {
				if imp.source_name == 'json' && imp.comments.len > 0 {
					json_import_has_line_comment = true
					break
				}
			}
		}
		// Some `json` usage cannot be rewritten to `json2` without changing or losing
		// source (see file_has_unmigratable_json_usage): `json.encode_pretty`, a
		// `json.decode` with comments on its type argument, or a struct field whose JSON
		// shape differs between the modules (`time.Time`, `@[raw]`). When the file has any
		// such usage, leave its whole `json` usage unmigrated (see call_expr /
		// imp_stmt_str), so calls and the `import json` stay byte-identical. Only scan
		// when the file actually imports `json`, to avoid walking unrelated files.
		// `file.imports` includes branch-local (`$if { import json }`) imports, so those
		// are covered too — otherwise vfmt could migrate the branch import to `json2`
		// while leaving an unmigrated `json.*` call behind.
		// `json` can be present only in file.implied_imports when vfmt auto-adds a missing
		// import. That path is left unmigrated: vfmt would emit the implied import verbatim
		// (not through imp_stmt_str), and whether a `json.encode` there even rewrites
		// depends on if `json` resolved as a module, so migrating risks an import/call
		// mismatch. Treat implied json as a reason to keep the file's json usage as-is.
		implied_json := 'json' in file.implied_imports
		mut imports_json := implied_json
		// An aliased legacy import (`import json as old`) qualifies its calls as
		// `old.encode(...)`. Whether the parser tags those as the literal `json.encode`
		// call kind that call_expr migrates depends on the alias being resolved to the
		// canonical module, which is not guaranteed in every parse context — so migrating
		// could drop the `old` alias (imp_stmt_str emits plain `import json2`) while
		// leaving `old.encode(...)` behind. Leave such files unmigrated.
		mut has_aliased_json_import := false
		for imp in file.imports {
			if imp.source_name == 'json' {
				imports_json = true
				if imp.alias != 'json' {
					has_aliased_json_import = true
				}
			}
		}
		// Importing `json2` into a file whose own module is `json2` is rejected by the
		// checker (cannot import a module into a module with the same name), so the
		// qualifier is effectively taken — leave such a file unmigrated.
		cur_module_is_json2 := file.mod.name == 'json2'
		if imports_json {
			f.keep_json_unmigrated = implied_json || json2_alias_is_blank || has_branch_local_json2
				|| has_branch_local_json || has_aliased_json_import || json2_name_taken
				|| json_import_has_line_comment || cur_module_is_json2
				|| f.file_has_vfmt_off_region() || f.file_has_unmigratable_json_usage(file)
				|| f.module_has_sibling_json_hook(file)
		}
		// Only rewrite `json.encode`/`json.decode` calls when the file actually imports the
		// legacy `json` module and it is being migrated. The parser tags a call by its literal
		// qualifier name, so a file using `import json2 as json` has its already-json2 calls
		// (`json.decode[User](s)`) tagged with the same `.json_decode`/`.json_encode` kind;
		// migrating those would corrupt them (e.g. `json.decode[User](s)` → `json.decode[s]()`,
		// since json2_migrate_call reads the type from the legacy first argument). An implied
		// json import keeps the file unmigrated, so it is excluded via keep_json_unmigrated.
		f.migrate_json_calls = imports_json && !f.keep_json_unmigrated
	}

	for imp in file.imports {
		f.mod2alias[imp.mod] = imp.alias
		f.mod2alias[imp.mod.all_after('${file.mod.name}.')] = imp.alias
		for sym in imp.syms {
			f.mod2alias['${imp.mod}.${sym.name}'] = sym.name
			f.mod2alias['${imp.mod.all_after_last('.')}.${sym.name}'] = sym.name
			f.mod2alias[sym.name] = sym.name
			f.mod2syms['${imp.mod}.${sym.name}'] = sym.name
			f.mod2syms['${imp.mod.all_after_last('.')}.${sym.name}'] = sym.name
			f.mod2syms[sym.name] = sym.name
		}
	}
}

// JsonUnmigratableScan is the walker state for file_has_unmigratable_json_usage:
// it detects `json` usage the json->json2 migration cannot rewrite losslessly, so
// the file is left entirely unmigrated (see process_file_imports and the visitor).
struct JsonUnmigratableScan {
	table        &ast.Table = unsafe { nil }
	json2_prefix string // the local name migrated calls would use; shadowing it breaks them
	cur_mod      string // the file's module, to qualify a bare decode target type name
mut:
	found bool
}

// struct_field_attr_needs_legacy_json reports whether a struct field's attributes force
// legacy json. A `@[raw]` field keeps the normalized subtree in json but the raw source
// slice in json2. An `@[omitempty]` field is dropped by legacy json when it equals the
// type default, but json2's check_not_empty only guards string/number/(dynamic-)array/map/
// option/pointer values and always emits bool / struct / sum type / enum / fixed-array
// fields — so migrating would start emitting fields legacy json omitted. (json2's `$array`
// emptiness check is a `.len` test that does not match a fixed array, while legacy json
// compares the fixed array to its default value.)
fn struct_field_attr_needs_legacy_json(t &ast.Table, field ast.StructField) bool {
	if field.attrs.any(it.name == 'raw') {
		return true
	}
	if field.attrs.any(it.name == 'omitempty')
		&& t.final_sym(field.typ).kind in [.bool, .struct, .sum_type, .enum, .array_fixed, .any] {
		// `.any` is an unresolved generic parameter (`val T @[omitempty]`): its concrete type
		// is unknown at fmt time and could be one of the emitted-always kinds, so keep.
		return true
	}
	return false
}

// type_needs_legacy_json reports whether `typ` is — or contains as the element, key
// or value of an array/fixed-array/map/chan, a generic type argument, a sum type
// variant, or a struct field, recursively and following aliases — a type whose JSON
// differs between the legacy `json` module and `json2`, so a payload of it must not be
// migrated:
//   - `time.Time`: a unix number in json vs an RFC3339 string in json2 (Time.to_json);
//   - `rune`: a JSON string in json (encode_rune) vs a u32 number in json2;
//   - `f32`/`f64`: json emits `null` for NaN/±Inf, json2 emits `nan`/`inf`;
//   - an option type, or a `@[raw]`/`@[omitempty]` struct field (see the helper above).
fn type_needs_legacy_json(t &ast.Table, typ ast.Type) bool {
	mut visited := []int{}
	return type_needs_legacy_json_rec(t, typ, mut visited)
}

fn type_needs_legacy_json_rec(t &ast.Table, typ ast.Type, mut visited []int) bool {
	if typ == 0 {
		return false
	}
	// An option type (e.g. a `?int` sum-type variant or field) serialises differently:
	// legacy json emits a `none` as `{}`, while json2 emits an empty slot (`[1,,3]`,
	// invalid JSON). The option flag is stripped by final_sym, so check it up front.
	if typ.has_flag(.option) {
		return true
	}
	sym := t.final_sym(typ)
	if sym.name in ['time.Time', 'rune', 'f32', 'f64'] {
		return true
	}
	if sym.kind == .placeholder {
		// v fmt parses only the current file, so an imported payload type (`models.Event`)
		// resolves only to a placeholder here, with no field info. Its fields may be
		// legacy-sensitive (a time.Time, a `@[raw]`/`@[omitempty]` field, ...), so keep such
		// unresolved payloads on legacy json rather than risk silently changing the contract.
		return true
	}
	// A recursive type (`type Val = []Val | int`, `type Tree = []Tree | int`) would
	// otherwise loop forever through its self-referential variant/element. A repeat of a
	// type index cannot introduce a new legacy-sensitive leaf (any such leaf is caught by
	// the name check above before this guard), so stop descending once one is seen.
	idx := typ.idx()
	if idx in visited {
		return false
	}
	visited << idx
	if sym.info is ast.Array {
		return type_needs_legacy_json_rec(t, sym.info.elem_type, mut visited)
	} else if sym.info is ast.ArrayFixed {
		return type_needs_legacy_json_rec(t, sym.info.elem_type, mut visited)
	} else if sym.info is ast.Map {
		return type_needs_legacy_json_rec(t, sym.info.key_type, mut visited)
			|| type_needs_legacy_json_rec(t, sym.info.value_type, mut visited)
	} else if sym.info is ast.Chan {
		return type_needs_legacy_json_rec(t, sym.info.elem_type, mut visited)
	} else if sym.info is ast.GenericInst {
		// A generic instantiation like `Box[time.Time]` carries the payload type only in
		// its concrete type arguments (e.g. `struct User { box Box[time.Time] }`).
		for concrete in sym.info.concrete_types {
			if type_needs_legacy_json_rec(t, concrete, mut visited) {
				return true
			}
		}
		// The base struct's own fields/attributes matter too: `struct Box[T] { val T
		// @[omitempty] }` is legacy-sensitive for a concrete `Box[bool]` (legacy omits the
		// default `false`, json2 emits it), but the concrete_types loop above only sees
		// `bool`, not the omitempty field. Recurse the parent struct — a generic-parameter
		// field type resolves to kind `.any`, which struct_field_attr_needs_legacy_json
		// treats as omitempty-sensitive since its concrete type is unknown at fmt time.
		parent := t.sym_by_idx(sym.info.parent_idx)
		if parent.info is ast.Struct {
			for field in parent.info.fields {
				if struct_field_attr_needs_legacy_json(t, field)
					|| type_needs_legacy_json_rec(t, field.typ, mut visited) {
					return true
				}
			}
		}
	} else if sym.info is ast.SumType {
		// A sum type variant (`type Val = rune | int`) can itself be a legacy-sensitive
		// payload: legacy json encodes a `rune` variant as a string and a non-finite
		// float variant as `null`, where json2 emits a number / raw `nan`/`inf`.
		for variant in sym.info.variants {
			if type_needs_legacy_json_rec(t, variant, mut visited) {
				return true
			}
		}
	} else if sym.info is ast.Struct {
		// A struct payload with no file-local StructDecl for the scan to catch — an
		// anonymous `struct { t time.Time }` literal, or a type from another module — is
		// only reachable here, so recurse into its fields (type and attributes alike). The
		// visited guard above stops self-referential structs (`struct Node { next &Node }`).
		for field in sym.info.fields {
			if struct_field_attr_needs_legacy_json(t, field)
				|| type_needs_legacy_json_rec(t, field.typ, mut visited) {
				return true
			}
		}
	}
	return false
}

// decode_target_expr_needs_legacy_json checks a `json.decode` target argument that is not a
// resolved TypeNode. With a selective import (`import json { decode }`) the parser does not
// know the call is json.decode until after parsing its arguments, so the target type is
// left as a plain name expression (`time.Time` selector, `Foo` ident) rather than a
// TypeNode. Resolve the name against the table and apply type_needs_legacy_json; when it
// cannot be resolved, keep the file unmigrated so a legacy root contract (time.Time/rune/…)
// is never silently changed.
fn decode_target_expr_needs_legacy_json(t &ast.Table, cur_mod string, expr ast.Expr) bool {
	name := match expr {
		ast.Ident {
			expr.name
		}
		ast.SelectorExpr {
			if expr.expr is ast.Ident { '${expr.expr.name}.${expr.field_name}' } else { '' }
		}
		else {
			''
		}
	}

	if name == '' {
		// Not a plain type-name expression (unexpected for a well-formed decode) — be safe.
		return true
	}
	mut typ := t.find_type(name)
	if typ == 0 && cur_mod != '' && !name.contains('.') {
		// A bare local type is registered module-qualified (`main.Foo`).
		typ = t.find_type('${cur_mod}.${name}')
	}
	if typ == 0 {
		// Unresolved at fmt time; keep unmigrated rather than risk changing the contract.
		return true
	}
	return type_needs_legacy_json(t, typ)
}

// encode_ident_needs_legacy_json handles a `json.encode(x)` payload that is a plain
// identifier (a parameter or variable) rather than a literal. A function parameter carries a
// declared type at parse time, so resolve it and apply type_needs_legacy_json — this keeps
// e.g. `fn f(e models.Event) string { return json.encode(e) }` on legacy json when
// `models.Event` is (or contains) a legacy-sensitive field. A local variable's type is
// usually unresolved without the checker; keep those too, since the payload cannot be
// proven safe.
fn encode_ident_needs_legacy_json(t &ast.Table, ident ast.Ident) bool {
	mut typ := ast.Type(0)
	if ident.obj is ast.Var {
		typ = ident.obj.typ
	}
	if typ == 0 && ident.scope != unsafe { nil } {
		if v := ident.scope.find_var(ident.name) {
			typ = v.typ
		}
	}
	if typ == 0 {
		return true
	}
	return type_needs_legacy_json(t, typ)
}

// payload_expr_needs_legacy_json reports whether a `json.encode` payload expression is (or
// contains) a typed literal whose type serialises differently under legacy json. A root
// container literal (`json.encode([]time.Time{})`, `json.encode({'x': rune(..)})`) has no
// struct field for the StructDecl scan to catch, and — without the checker — its own
// `typ`/`elem_type`/`value_type` are often unresolved at fmt time, so recurse through the
// literal and inspect each element/value literal or cast, which does carry a type.
fn payload_expr_needs_legacy_json(t &ast.Table, expr ast.Expr) bool {
	match expr {
		ast.ParExpr {
			// A parenthesised payload (`json.encode((rune(233)))`) hides the typed literal
			// one level down; unwrap it so the StructInit/CastExpr below is still inspected.
			return payload_expr_needs_legacy_json(t, expr.expr)
		}
		ast.StructInit {
			return type_needs_legacy_json(t, expr.typ)
		}
		ast.CastExpr {
			return type_needs_legacy_json(t, expr.typ)
		}
		ast.ArrayInit {
			if type_needs_legacy_json(t, expr.typ) || type_needs_legacy_json(t, expr.elem_type) {
				return true
			}
			for e in expr.exprs {
				if payload_expr_needs_legacy_json(t, e) {
					return true
				}
			}
			return false
		}
		ast.MapInit {
			if type_needs_legacy_json(t, expr.typ) || type_needs_legacy_json(t, expr.key_type)
				|| type_needs_legacy_json(t, expr.value_type) {
				return true
			}
			for k in expr.keys {
				if payload_expr_needs_legacy_json(t, k) {
					return true
				}
			}
			for v in expr.vals {
				if payload_expr_needs_legacy_json(t, v) {
					return true
				}
			}
			return false
		}
		else {
			return false
		}
	}
}

// assign_declares_name reports whether a `:=` declaration introduces `name` on its
// left-hand side (e.g. `name := ...` or `a, name := ...`).
fn assign_declares_name(assign ast.AssignStmt, name string) bool {
	if assign.op != .decl_assign {
		return false
	}
	for lx in assign.left {
		if lx is ast.Ident && lx.name == name {
			return true
		}
	}
	return false
}

// walk_or_block sub-walks the statements of an `or { ... }` block. Node.children()
// exposes the or-block only for CallExpr; on identifier / index / selector / prefix /
// infix / comptime expressions the or-block is a separate field the walker never
// reaches, so an unmigratable `json` call inside e.g. `opt or { json.encode_pretty(u) }`
// or `m[k] or { json.encode_pretty(u) }` would otherwise be missed.
fn walk_or_block(or_block ast.OrExpr, data voidptr) {
	if or_block.kind != .absent {
		walker.inspect(ast.Expr(or_block), data, json_unmigratable_scan_visit)
	}
}

// walk_sql_query_data_items sub-walks the leaf/branch condition expressions of a dynamic
// ORM query-data expression (`{ cond: value, if x { .. } }`). Node.children() has no
// SqlQueryDataExpr case, but sql_query_data_expr prints these expressions unchanged.
fn walk_sql_query_data_items(items []ast.SqlQueryDataItem, data voidptr) {
	for item in items {
		match item {
			ast.SqlQueryDataLeaf {
				walker.inspect(item.expr, data, json_unmigratable_scan_visit)
			}
			ast.SqlQueryDataIf {
				for branch in item.branches {
					walker.inspect(branch.cond, data, json_unmigratable_scan_visit)
					walk_sql_query_data_items(branch.items, data)
				}
			}
		}
	}
}

// json_verbatim_arg_scan_visit flags any `json.encode`/`json.decode`/`json.encode_pretty`
// call inside an expression that the formatter prints verbatim (via str()) rather than
// through call_expr — an inline-asm operand, or a comptime method-call argument (emitted by
// comptime_call through call_arg_spread_str). Such a call can never be rewritten, so the
// file must stay unmigrated whenever one appears there, even a normally-migratable one.
fn json_verbatim_arg_scan_visit(node &ast.Node, data voidptr) bool {
	mut s := unsafe { &JsonUnmigratableScan(data) }
	if s.found {
		return false
	}
	if node is ast.Expr && node is ast.CallExpr {
		call := node as ast.CallExpr
		if call.kind in [.json_encode, .json_decode, .json_encode_pretty] {
			s.found = true
		}
	}
	return true
}

fn json_unmigratable_scan_visit(node &ast.Node, data voidptr) bool {
	mut s := unsafe { &JsonUnmigratableScan(data) }
	if s.found {
		return false
	}
	if node is ast.Expr && node is ast.CallExpr {
		call := node as ast.CallExpr
		if call.kind == .json_encode_pretty {
			// json.encode_pretty output cannot be reproduced by json2.
			s.found = true
		} else if call.kind == .json_decode && call.args.len >= 1 {
			// A comment on the type arg cannot survive the migrated `[T]` bracket, and a
			// decode target that is (or contains) `time.Time` serialises differently in
			// json2 (RFC3339 via Time.to_json vs the legacy root path). A qualified call
			// carries a resolved TypeNode; a selective-import `decode(T, s)` leaves the
			// target as a name expression, resolved by decode_target_expr_needs_legacy_json.
			if call.args[0].comments.len > 0 {
				s.found = true
			} else {
				type_arg := call.args[0].expr
				if type_arg is ast.TypeNode {
					if type_needs_legacy_json(s.table, type_arg.typ) {
						s.found = true
					}
				} else if decode_target_expr_needs_legacy_json(s.table, s.cur_mod, type_arg) {
					s.found = true
				}
			}
		} else if call.kind == .json_encode && call.args.len >= 1 {
			// A root `time.Time`/rune/float/option encode payload also serialises
			// differently. Inspect a literal payload and its element/value literals; for a
			// plain identifier payload, resolve its declared type (parameters carry one) and
			// keep the file on legacy json when it is unresolved or legacy-sensitive.
			arg := call.args[0].expr
			if payload_expr_needs_legacy_json(s.table, arg) {
				s.found = true
			} else if arg is ast.Ident && encode_ident_needs_legacy_json(s.table, arg) {
				s.found = true
			}
		}
	} else if node is ast.Stmt && node is ast.StructDecl {
		decl := node as ast.StructDecl
		// Some field kinds serialise differently in json2, with no option to restore the
		// legacy bytes: a `@[raw]` field keeps the normalized subtree in json but the raw
		// source slice in json2, and time.Time/rune/f32/f64 fields differ too (see
		// type_needs_legacy_json). vfmt has no type info for encode/decode payloads, so as
		// a conservative proxy any struct declared in the file with such a field leaves the
		// file unmigrated. Only file-local structs are visible.
		for field in decl.fields {
			// type_needs_legacy_json follows aliases and looks inside array/map/chan,
			// generic wrappers and nested struct fields, so `[]time.Time`,
			// `map[string]MyRune`, `Box[f64]`, etc. are all recognised; the attr helper
			// covers `@[raw]`/`@[omitempty]` on the field itself.
			if struct_field_attr_needs_legacy_json(s.table, field)
				|| type_needs_legacy_json(s.table, field.typ) {
				s.found = true
				break
			}
			// A field default (`payload string = json.encode_pretty(...)`) can hold an
			// unmigratable json call, but StructField.default_expr is outside
			// Node.children(), so sub-walk it explicitly.
			if field.has_default_expr {
				walker.inspect(field.default_expr, data, json_unmigratable_scan_visit)
			}
		}
	} else if node is ast.Stmt && node is ast.FnDecl {
		decl := node as ast.FnDecl
		// A parameter or receiver named the same as the migrated qualifier (e.g. a
		// param `json2`) shadows the module, so the rewritten `json2.encode(...)`
		// would bind to the local instead. A top-level function with that name
		// collides outright (`fn json2()` duplicates the imported module symbol).
		// Leave the file unmigrated in either case.
		if decl.receiver.name == s.json2_prefix
			|| decl.params.any(it.name == s.json2_prefix)
			|| (!decl.is_method && decl.short_name == s.json2_prefix) {
			s.found = true
		}
		// A payload type that implements a json2 custom (de)serialization hook —
		// `to_json`/`json_str` for encode, `from_json_*` for decode — makes json2
		// dispatch to that hook, emitting/parsing different bytes than the legacy
		// field-based json path. Leave such files unmigrated (the hook is a file-local
		// method declaration on the payload type).
		if decl.is_method {
			method_name := decl.name.all_after_last('.')
			if method_name in ['to_json', 'json_str'] || method_name.starts_with('from_json_') {
				s.found = true
			}
		}
	} else if node is ast.Expr && node is ast.IfExpr {
		// The walker does not descend into a branch condition (IfBranch.children()
		// yields only its stmts), so an unmigratable call or a shadowing guard variable
		// in `if <cond> { }` would be missed. Sub-walk each branch condition explicitly
		// (this reaches nested calls and the IfGuardExpr case below).
		ifexpr := node as ast.IfExpr
		for branch in ifexpr.branches {
			walker.inspect(branch.cond, data, json_unmigratable_scan_visit)
		}
	} else if node is ast.Expr && node is ast.IfGuardExpr {
		// `if json2 := opt() { ... }` introduces a local named `json2` for the branch,
		// shadowing the module qualifier for calls inside it.
		ifg := node as ast.IfGuardExpr
		if ifg.vars.any(it.name == s.json2_prefix) {
			s.found = true
		}
	} else if node is ast.Expr && node is ast.SpawnExpr {
		// `spawn json.encode_pretty(u)` — SpawnExpr.call_expr is outside
		// Node.children(), so sub-walk the spawned call explicitly. It is wrapped as
		// ast.Expr because walker.inspect converts only one level to ast.Node.
		spawn_expr := node as ast.SpawnExpr
		walker.inspect(ast.Expr(spawn_expr.call_expr), data, json_unmigratable_scan_visit)
	} else if node is ast.Expr && node is ast.GoExpr {
		// `go json.encode_pretty(u)` — GoExpr.call_expr is likewise outside children().
		go_expr := node as ast.GoExpr
		walker.inspect(ast.Expr(go_expr.call_expr), data, json_unmigratable_scan_visit)
	} else if node is ast.Expr && node is ast.DumpExpr {
		// `dump(json.encode_pretty(u))` — DumpExpr.expr is outside Node.children().
		dump_expr := node as ast.DumpExpr
		walker.inspect(dump_expr.expr, data, json_unmigratable_scan_visit)
	} else if node is ast.Expr && node is ast.SqlExpr {
		// The db/where/order/limit/offset exprs and joined sub-queries of a
		// `sql db { select ... where ... }` query are outside Node.children() but are
		// printed by sql_expr, so sub-walk them (sub_structs recurse into this case).
		sqlexpr := node as ast.SqlExpr
		walker.inspect(sqlexpr.db_expr, data, json_unmigratable_scan_visit)
		if sqlexpr.has_where {
			walker.inspect(sqlexpr.where_expr, data, json_unmigratable_scan_visit)
		}
		if sqlexpr.has_order {
			walker.inspect(sqlexpr.order_expr, data, json_unmigratable_scan_visit)
		}
		if sqlexpr.has_limit {
			walker.inspect(sqlexpr.limit_expr, data, json_unmigratable_scan_visit)
		}
		if sqlexpr.has_offset {
			walker.inspect(sqlexpr.offset_expr, data, json_unmigratable_scan_visit)
		}
		for _, sub in sqlexpr.sub_structs {
			walker.inspect(ast.Expr(sub), data, json_unmigratable_scan_visit)
		}
		for join in sqlexpr.joins {
			// Each JOIN's `on` predicate is printed by sql_expr (f.expr(join.on_expr)) but
			// is outside Node.children().
			walker.inspect(join.on_expr, data, json_unmigratable_scan_visit)
		}
		walk_or_block(sqlexpr.or_expr, data)
	} else if node is ast.Expr && node is ast.ArrayInit {
		// The len:/cap:/init: exprs of `[]T{len: .., init: ..}` and the spread expr of
		// `[...a, b]` are outside ArrayInit.children() (only elements are), so sub-walk
		// them.
		ai := node as ast.ArrayInit
		if ai.has_len {
			walker.inspect(ai.len_expr, data, json_unmigratable_scan_visit)
		}
		if ai.has_cap {
			walker.inspect(ai.cap_expr, data, json_unmigratable_scan_visit)
		}
		if ai.has_init {
			walker.inspect(ai.init_expr, data, json_unmigratable_scan_visit)
		}
		if ai.has_update_expr {
			walker.inspect(ai.update_expr, data, json_unmigratable_scan_visit)
		}
	} else if node is ast.Expr && node is ast.StructInit {
		// The spread/update expr of `User{ ...base(json.encode_pretty(u)), name: 'x' }` is
		// on StructInit.update_expr, outside Node.children() (only fields are), but
		// struct_init prints it unchanged.
		si := node as ast.StructInit
		if si.has_update_expr {
			walker.inspect(si.update_expr, data, json_unmigratable_scan_visit)
		}
	} else if node is ast.Expr && node is ast.MapInit {
		// Likewise the spread expr of `{ ...parts(json.encode_pretty(u)), 'x': 'y' }` is on
		// MapInit.update_expr, outside Node.children().
		mi := node as ast.MapInit
		if mi.has_update_expr {
			walker.inspect(mi.update_expr, data, json_unmigratable_scan_visit)
		}
	} else if node is ast.Expr && node is ast.SqlQueryDataExpr {
		// The leaf/branch conditions of a dynamic ORM query-data expression are printed by
		// sql_query_data_expr but are outside Node.children().
		walk_sql_query_data_items((node as ast.SqlQueryDataExpr).items, data)
	} else if node is ast.Expr && node is ast.Ident {
		// `opt or { json.encode_pretty(u) }` — an identifier's `or {}` block is outside
		// Node.children() (only CallExpr exposes its or-block), so sub-walk it.
		walk_or_block((node as ast.Ident).or_expr, data)
	} else if node is ast.Expr && node is ast.IndexExpr {
		// `m[k] or { json.encode_pretty(u) }` — IndexExpr.or_expr is not a child either.
		walk_or_block((node as ast.IndexExpr).or_expr, data)
	} else if node is ast.Expr && node is ast.SelectorExpr {
		// `obj.field or { json.encode_pretty(u) }` — the base is a child but or_block isn't.
		walk_or_block((node as ast.SelectorExpr).or_block, data)
	} else if node is ast.Expr && node is ast.InfixExpr {
		walk_or_block((node as ast.InfixExpr).or_block, data)
	} else if node is ast.Expr && node is ast.PrefixExpr {
		walk_or_block((node as ast.PrefixExpr).or_block, data)
	} else if node is ast.Expr && node is ast.ComptimeCall {
		// `app.$method(json.encode(u))` — comptime_call prints ComptimeCall.args verbatim via
		// call_arg_spread_str (arg.str()), not call_expr, so a json call there can never be
		// rewritten. Like an asm operand, any json call in an arg keeps the file unmigrated
		// (the general scan would miss a normally-migratable json.encode). The or-block is
		// printed normally, so it uses the migration-aware scan.
		cc := node as ast.ComptimeCall
		for arg in cc.args {
			walker.inspect(arg.expr, data, json_verbatim_arg_scan_visit)
		}
		walk_or_block(cc.or_block, data)
	} else if node is ast.Expr && node is ast.ComptimeSelector {
		walk_or_block((node as ast.ComptimeSelector).or_block, data)
	} else if node is ast.Expr && node is ast.LambdaExpr {
		// A lambda parameter named like the qualifier (`|json2| json.encode(u)`) shadows
		// the module for the lambda body.
		lambda := node as ast.LambdaExpr
		if lambda.params.any(it.name == s.json2_prefix) {
			s.found = true
		}
	} else if node is ast.Stmt && node is ast.AssignStmt {
		// A local declaration `json2 := ...` shadows the qualifier too.
		if assign_declares_name(node as ast.AssignStmt, s.json2_prefix) {
			s.found = true
		}
	} else if node is ast.Stmt && node is ast.ForStmt {
		// The walker does not descend into a loop header (For*.children() yields only
		// the body), so an unmigratable call in `for <cond> {}` would be missed.
		forstmt := node as ast.ForStmt
		if !forstmt.is_inf {
			walker.inspect(forstmt.cond, data, json_unmigratable_scan_visit)
		}
	} else if node is ast.Stmt && node is ast.ForCStmt {
		// The init / cond / inc of a C-style for are all outside For*.children(). Sub-
		// walk them: init also catches a loop variable that shadows the qualifier (via
		// the AssignStmt case), cond/inc catch unmigratable calls in the header.
		forc := node as ast.ForCStmt
		if forc.has_init {
			walker.inspect(forc.init, data, json_unmigratable_scan_visit)
		}
		if forc.has_cond {
			walker.inspect(forc.cond, data, json_unmigratable_scan_visit)
		}
		if forc.has_inc {
			walker.inspect(forc.inc, data, json_unmigratable_scan_visit)
		}
	} else if node is ast.Stmt && node is ast.ForInStmt {
		fin := node as ast.ForInStmt
		if fin.key_var == s.json2_prefix || fin.val_var == s.json2_prefix {
			s.found = true
		}
		// The iterable (and range high) are loop-header exprs outside For*.children().
		walker.inspect(fin.cond, data, json_unmigratable_scan_visit)
		if fin.is_range {
			walker.inspect(fin.high, data, json_unmigratable_scan_visit)
		}
	} else if node is ast.Stmt && node is ast.ComptimeFor {
		// `$for json2 in User.fields { ... }` introduces a compile-time loop variable
		// named like the qualifier, shadowing the module for calls in the loop body.
		cfor := node as ast.ComptimeFor
		if cfor.val_var == s.json2_prefix {
			s.found = true
		}
	} else if node is ast.Stmt && node is ast.ConstDecl {
		cd := node as ast.ConstDecl
		for field in cd.fields {
			if field.name == s.json2_prefix || field.name.all_after_last('.') == s.json2_prefix {
				s.found = true
				break
			}
		}
	} else if node is ast.Stmt && node is ast.GlobalDecl {
		// Under `-enable-globals`, a `__global json2 ...` declaration is an existing
		// identifier that would shadow the migrated qualifier, so `json2.encode` would
		// resolve against the global value instead of the module.
		gd := node as ast.GlobalDecl
		for field in gd.fields {
			if field.name == s.json2_prefix || field.name.all_after_last('.') == s.json2_prefix {
				s.found = true
				break
			}
		}
	} else if node is ast.Stmt && node is ast.AssertStmt {
		// `assert ok, json.encode_pretty(u)` — the assert message (AssertStmt.extra) is
		// outside Node.children(), so sub-walk it explicitly.
		assert_stmt := node as ast.AssertStmt
		walker.inspect(assert_stmt.extra, data, json_unmigratable_scan_visit)
	} else if node is ast.Stmt && node is ast.SqlStmt {
		// `sql db { update ... set x = json.encode_pretty(u) where ... }` — the db expr
		// and each line's where/update exprs are outside Node.children().
		sqlstmt := node as ast.SqlStmt
		walker.inspect(sqlstmt.db_expr, data, json_unmigratable_scan_visit)
		for line in sqlstmt.lines {
			walker.inspect(line.where_expr, data, json_unmigratable_scan_visit)
			walker.inspect(line.update_data_expr, data, json_unmigratable_scan_visit)
			for update_expr in line.update_exprs {
				walker.inspect(update_expr, data, json_unmigratable_scan_visit)
			}
		}
		walk_or_block(sqlstmt.or_expr, data)
	} else if node is ast.Stmt && node is ast.AsmStmt {
		// An `asm { ... }` operand (`r (json.encode_pretty(u))`) is an AsmIO.expr that
		// asm_ios prints verbatim via `${io.expr}` (its str()), bypassing call_expr — so it
		// is both outside Node.children() and beyond the migration rewrite. Any json call
		// there (even a normally-migratable `json.encode`) can never be rewritten, so keep
		// the whole file unmigrated.
		asm_stmt := node as ast.AsmStmt
		for io in asm_stmt.output {
			walker.inspect(io.expr, data, json_verbatim_arg_scan_visit)
		}
		for io in asm_stmt.input {
			walker.inspect(io.expr, data, json_verbatim_arg_scan_visit)
		}
	}
	return true
}

fn (f &Fmt) file_has_unmigratable_json_usage(file &ast.File) bool {
	mut scan := JsonUnmigratableScan{
		table:        f.table
		json2_prefix: f.json2_prefix
		cur_mod:      file.mod.name
	}
	walker.inspect(file, &scan, json_unmigratable_scan_visit)
	return scan.found
}

// file_has_vfmt_off_region reports whether the source has a `// vfmt off` region.
// Such a region is copied back to the output verbatim, so a raw `json.*` call or
// `import json` inside it is not migrated in step with the rest of the file (e.g.
// `import json` migrates to `import json2` while the off-block still calls
// `json.encode`, leaving the file with no `json` import). Since a `// vfmt off`
// comment can even sit between the import and a call, the whole file's json usage is
// left unmigrated whenever any such region is present.
fn (mut f Fmt) file_has_vfmt_off_region() bool {
	for line in f.get_source_lines() {
		if line.contains('// vfmt off') {
			return true
		}
	}
	return false
}

// module_has_sibling_json_hook reports whether another `.v` file in the same module
// directory declares a json2 custom (de)serialization hook (`to_json`/`json_str`/
// `from_json_*`). v fmt parses only the file being formatted (cmd/tools/vfmt.v), so a hook
// on a payload type whose method lives in a sibling file is invisible to the in-file scan;
// migrating would then let json2 dispatch to that hook while legacy json used the
// field-based path, changing the JSON contract. As with the in-file hook check this is a
// coarse module-level signal — a real hook anywhere in the module keeps the file
// unmigrated. Only `.v` sources are read, so the `.vv` fmt fixtures never trip it.
fn (f &Fmt) module_has_sibling_json_hook(file &ast.File) bool {
	if file.path == '' {
		return false
	}
	dir := os.dir(file.path)
	entries := os.ls(dir) or { return false }
	for entry in entries {
		if entry == file.path_base || !entry.ends_with('.v') {
			continue
		}
		src := os.read_file(os.join_path(dir, entry)) or { continue }
		if source_declares_json_hook(src) {
			return true
		}
	}
	return false
}

// source_declares_json_hook reports whether the given V source text declares a method named
// `to_json`/`json_str`/`from_json_*` (a json2 custom hook). It is a line-level scan of the
// method-declaration form `fn (recv) hook(...)`, which is enough to detect a hook without
// parsing the sibling file.
fn source_declares_json_hook(src string) bool {
	for line in src.split_into_lines() {
		mut t := line.trim_space()
		// Strip any leading same-line attributes, so a hook like
		// `@[manualfree] fn (u User) to_json() string` is still recognised.
		for t.starts_with('@[') {
			attr_close := t.index(']') or { break }
			t = t[attr_close + 1..].trim_space()
		}
		if !(t.starts_with('fn ') || t.starts_with('pub fn ')) {
			continue
		}
		// The receiver is the first parenthesised group; the method name follows its `)`.
		close := t.index(')') or { continue }
		after := t[close + 1..].trim_space()
		if after.starts_with('to_json(') || after.starts_with('json_str(')
			|| after.starts_with('from_json_') {
			return true
		}
	}
	return false
}

//=== Basic buffer write operations ===//

pub fn (mut f Fmt) write(s string) {
	if f.indent > 0 && f.empty_line {
		f.write_indent()
	}
	f.out.write_string(s)
	f.line_len += s.len
	f.empty_line = false
}

pub fn (mut f Fmt) writeln(s string) {
	if f.indent > 0 && f.empty_line && s != '' {
		f.write_indent()
	}
	f.out.writeln(s)
	f.empty_line = true
	f.line_len = 0
}

fn (mut f Fmt) write_indent() {
	f.out.write_string(util.tabs(f.indent))
	f.line_len += f.indent * 4
}

pub fn (mut f Fmt) wrap_long_line(penalty_idx int, add_indent bool) bool {
	if f.buffering {
		return false
	}
	if penalty_idx > 0 && f.line_len <= break_points[penalty_idx] {
		return false
	}
	if f.out.last() == ` ` {
		f.out.go_back(1)
	}
	f.write('\n')
	f.line_len = 0
	if add_indent {
		f.indent++
	}
	f.write_indent()
	if add_indent {
		f.indent--
	}
	return true
}

// When the removal action actually occurs, the string of the last line after the removal is returned
pub fn (mut f Fmt) remove_new_line() string {
	mut buffer := unsafe { &f.out }
	mut i := 0
	for i = buffer.len - 1; i >= 0; i-- {
		if !buffer.byte_at(i).is_space() { // != `\n` {
			break
		}
	}
	if i == buffer.len - 1 {
		return ''
	}
	buffer.go_back(buffer.len - i - 1)
	f.empty_line = false
	mut line_len := 0
	mut last_line_str := []u8{}
	for i = buffer.len - 1; i >= 0; i-- {
		ch := buffer.byte_at(i)
		if ch == `\n` {
			break
		}
		line_len += if ch == `\t` { 4 } else { 1 }
		last_line_str << ch
	}
	f.line_len = line_len
	return last_line_str.reverse().bytestr()
}

//=== Specialized write methods ===//

fn (mut f Fmt) write_language_prefix(lang ast.Language) {
	match lang {
		.c { f.write('C.') }
		.js { f.write('JS.') }
		.wasm { f.write('WASM.') }
		else {}
	}
}

fn (mut f Fmt) write_generic_types(gtypes []ast.Type) {
	if gtypes.len > 0 {
		f.write('[')
		gtypes_string := gtypes.map(f.type_to_str(it)).join(', ')
		f.write(gtypes_string)
		f.write(']')
	}
}

//=== Module handling helper methods ===//

pub fn (mut f Fmt) set_current_module_name(cmodname string) {
	f.cur_mod = cmodname
	f.table.cmod_prefix = cmodname + '.'
	// Drop type_to_str strings that were memoized before the module context was
	// known (during parsing `cmod_prefix` is still empty), so that types of the
	// current module are not left with a stale `mod.` prefix when a `fn` typed
	// field signature is rebuilt later on (issue #27475).
	f.table.invalidate_type_to_str_cache()
}

fn (f &Fmt) get_modname_prefix(mname string) (string, string) {
	// ./tests/proto_module_importing_vproto_keep.vv to know, why here is checked for ']' and '&'
	if !mname.contains(']') && !mname.contains('&') {
		return mname, ''
	}
	after_rbc := mname.all_after_last(']')
	after_ref := mname.all_after_last('&')
	modname := if after_rbc.len < after_ref.len { after_rbc } else { after_ref }
	return modname, mname.trim_string_right(modname)
}

fn (mut f Fmt) is_external_name(name string) bool {
	if name.len > 2 && name[0] == `C` && name[1] == `.` {
		return true
	}
	if name.len > 3 && name[0] == `J` && name[1] == `S` && name[2] == `.` {
		return true
	}
	return false
}

pub fn (mut f Fmt) no_cur_mod(typename string) string {
	return util.no_cur_mod(typename, f.cur_mod)
}

// foo.bar.fn() => bar.fn()
pub fn (mut f Fmt) short_module(name string) string {
	if !name.contains('.') || name.starts_with('JS.') {
		return name
	}
	if name in f.mod2syms {
		return f.mod2syms[name]
	}
	if name.ends_with(']') {
		generic_levels := name.trim_string_right(']').split('[')
		mut res := '${f.short_module(generic_levels[0])}'
		for i in 1 .. generic_levels.len {
			genshorts := generic_levels[i].split(', ').map(f.short_module(it)).join(', ')
			res += '[${genshorts}'
		}
		res += ']'
		return res
	}
	vals := name.split('.')
	if vals.len < 2 {
		return name
	}
	idx := vals.len - 1
	mname, tprefix := f.get_modname_prefix(vals[..idx].join('.'))
	symname := vals.last()
	mut aname := f.mod2alias[mname]
	if aname == '' {
		for _, v in f.mod2alias {
			if v == mname {
				aname = mname
				break
			}
		}
	}
	if aname == '' {
		return '${tprefix}${symname}'
	}
	return '${tprefix}${aname}.${symname}'
}

//=== Import-related methods ===//

pub fn (mut f Fmt) import_stmt(imp ast.Import) {
	f.has_import_stmt = true
	if imp.mod in f.file.auto_imports && imp.mod !in f.file.used_imports {
		// Skip hidden imports like preludes.
		return
	}
	if imp.source_name == 'json' && f.has_json2_import && !f.keep_json_unmigrated {
		// Migrating deprecated `import json` to `import json2`, but the file already
		// has a top-level `json2` import (possibly with symbols or an alias, e.g.
		// `import json2 { Any }` or `import json2 as j2`). Drop the migrated import
		// so it merges into the existing one; the rewritten call sites use that
		// import's local name (f.json2_prefix), so they stay in scope. Emitting a
		// second `import json2` would be rejected by the checker as already imported.
		f.import_comments(imp.next_comments)
		return
	}
	imp_stmt := f.imp_stmt_str(imp)
	if imp_stmt in f.global_processed_imports
		|| (f.inside_comptime_if && imp_stmt in f.branch_processed_imports) {
		// Skip duplicates.
		f.import_comments(imp.next_comments)
		return
	}
	if f.inside_comptime_if {
		f.branch_processed_imports << imp_stmt
	} else {
		f.global_processed_imports << imp_stmt
	}
	if !f.format_state.is_vfmt_on {
		original_imp_line :=
			f.get_source_lines()#[imp.pos.line_nr..imp.pos.last_line + 1].join('\n')
		// Same line comments(`imp.comments`) are included in the `original_imp_line`.
		f.writeln(original_imp_line)
		f.import_comments(imp.next_comments)
	} else {
		f.writeln('import ${imp_stmt}')
		f.import_comments(imp.comments, same_line: true)
		f.import_comments(imp.next_comments)
	}
	f.import_pos = f.out.len
}

pub fn (f &Fmt) imp_stmt_str(imp ast.Import) string {
	// The `json` module is deprecated; migrate any `import json` form to a plain
	// `import json2`, to match the call rewriting done in json2_migrate_call.
	// This also covers aliased imports (`import json as x`) and selective imports
	// (`import json { decode }`): the alias/symbols are dropped, since json's whole
	// public API (decode/encode/encode_pretty) is rewritten to qualified `json2.x`
	// calls, so nothing needs to be brought into scope unqualified anymore.
	// Exception: a file with a json call the migration can't rewrite losslessly is
	// left unmigrated (see process_file_imports), so its `import json` is kept as-is.
	// migrate_json_calls is `imports_json && !keep_json_unmigrated`, and is false unless the
	// opt-in migration is enabled — so plain `v fmt` leaves `import json` untouched.
	if imp.source_name == 'json' && f.migrate_json_calls {
		return 'json2'
	}
	// Format / remove unused selective import symbols
	// E.g.: `import foo { Foo }` || `import foo as f { Foo }`
	has_alias := imp.alias != imp.source_name.all_after_last('.')
	mut suffix := if has_alias { ' as ${imp.alias}' } else { '' }
	mut syms := imp.syms.map(it.name).filter(f.file.imported_symbols_used[it])
	syms.sort()
	if syms.len > 0 {
		suffix += if imp.syms[0].pos.line_nr == imp.pos.line_nr {
			' { ' + syms.join(', ') + ' }'
		} else {
			' {\n\t' + syms.join(',\n\t') + ',\n}'
		}
	}
	return '${imp.source_name}${suffix}'
}

//=== Node helpers ===//

fn (f &Fmt) should_insert_newline_before_node(node ast.Node, prev_node ast.Node) bool {
	// No need to insert a newline if there is already one
	if f.out.last_n(2) == '\n\n' {
		return false
	}
	prev_line_nr := prev_node.pos().last_line
	// The nodes are Stmts
	if node is ast.Stmt && prev_node is ast.Stmt {
		match prev_node {
			// Force a newline after a block of HashStmts
			ast.HashStmt {
				if node !in [ast.HashStmt, ast.ExprStmt] {
					return true
				}
			}
			// Force a newline after function declarations
			// The only exception is inside a block of no_body functions
			ast.FnDecl {
				if node !is ast.FnDecl || !prev_node.no_body {
					return true
				}
			}
			ast.SemicolonStmt {
				return false
			}
			// Force a newline after struct declarations
			ast.StructDecl {
				return true
			}
			// Empty line after a block of type declarations
			ast.TypeDecl {
				if node !is ast.TypeDecl {
					return true
				}
			}
			// Force a newline after imports
			ast.Import {
				return node !is ast.Import
			}
			ast.ConstDecl {
				mut is_comment_expr_stmt := false
				if node is ast.ExprStmt {
					expr_stmt := node
					is_comment_expr_stmt = expr_stmt.expr is ast.Comment
				}
				if node !is ast.ConstDecl && !is_comment_expr_stmt {
					return true
				}
			}
			else {}
		}

		match node {
			// Attributes are not respected in the stmts position, so this requires manual checking
			ast.StructDecl, ast.EnumDecl, ast.FnDecl {
				if node.attrs.len > 0 && node.attrs[0].pos.line_nr - prev_line_nr <= 1 {
					return false
				}
			}
			ast.Import {
				return false
			}
			else {}
		}
	}
	// The node shouldn't have a newline before
	if node.pos().line_nr - prev_line_nr <= 1 {
		return false
	}
	return true
}

pub fn (mut f Fmt) node_str(node ast.Node) string {
	was_empty_line := f.empty_line
	prev_line_len := f.line_len
	pos := f.out.len
	match node {
		ast.Stmt { f.stmt(node) }
		ast.Expr { f.expr(node) }
		else { panic('´f.node_str()´ is not implemented for ${node}.') }
	}

	str := f.out.after(pos)
	f.out.go_back_to(pos)
	f.empty_line = was_empty_line
	f.line_len = prev_line_len
	return str
}

//=== General Stmt-related methods and helpers ===//

pub fn (mut f Fmt) stmts(stmts []ast.Stmt) {
	mut prev_stmt := ast.empty_stmt
	f.indent++
	for i, stmt in stmts {
		if i > 0 && f.should_insert_newline_before_node(stmt, prev_stmt) {
			f.out.writeln('')
		}
		f.stmt(stmt)
		prev_stmt = stmt
	}
	f.indent--
}

pub fn (mut f Fmt) stmt(node ast.Stmt) {
	if f.is_debug {
		eprintln('stmt ${node.type_name():-20} | pos: ${node.pos.line_str()}')
	}
	match node {
		ast.EmptyStmt, ast.NodeError {}
		ast.AsmStmt {
			f.asm_stmt(node)
		}
		ast.AssertStmt {
			f.assert_stmt(node)
		}
		ast.AssignStmt {
			f.assign_stmt(node)
		}
		ast.Block {
			if node.is_unsafe {
				f.inside_unsafe = true
				f.block(node)
				f.inside_unsafe = false
			} else {
				f.block(node)
			}
		}
		ast.BranchStmt {
			f.branch_stmt(node)
		}
		ast.ComptimeFor {
			f.comptime_for(node)
		}
		ast.ConstDecl {
			f.const_decl(node)
		}
		ast.DebuggerStmt {
			f.debugger_stmt(node)
		}
		ast.DeferStmt {
			f.defer_stmt(node)
		}
		ast.EnumDecl {
			f.enum_decl(node)
		}
		ast.ExprStmt {
			f.expr_stmt(node)
		}
		ast.FnDecl {
			f.fn_decl(node)
		}
		ast.ForCStmt {
			f.for_c_stmt(node)
		}
		ast.ForInStmt {
			f.for_in_stmt(node)
		}
		ast.ForStmt {
			f.for_stmt(node)
		}
		ast.GlobalDecl {
			f.global_decl(node)
		}
		ast.GotoLabel {
			f.goto_label(node)
		}
		ast.GotoStmt {
			f.goto_stmt(node)
		}
		ast.HashStmt {
			f.hash_stmt(node)
		}
		ast.Import {
			f.import_stmt(node)
		}
		ast.InterfaceDecl {
			f.interface_decl(node)
		}
		ast.Module {
			f.module_stmt(node)
		}
		ast.Return {
			f.return_stmt(node)
		}
		ast.SemicolonStmt {}
		ast.SqlStmt {
			f.sql_stmt(node)
		}
		ast.StructDecl {
			f.struct_decl(node, false)
		}
		ast.TypeDecl {
			f.type_decl(node)
		}
	}
}

fn stmt_is_single_line(stmt ast.Stmt) bool {
	return match stmt {
		ast.ExprStmt, ast.AssertStmt { expr_is_single_line(stmt.expr) }
		ast.Return, ast.AssignStmt, ast.BranchStmt { true }
		ast.SemicolonStmt { true }
		else { false }
	}
}

//=== General Expr-related methods and helpers ===//

pub fn (mut f Fmt) expr(node_ ast.Expr) {
	mut node := unsafe { node_ }
	if f.is_debug {
		eprintln('expr ${node.type_name():-20} | pos: ${node.pos().line_str()} | ${node.str()}')
	}
	match mut node {
		ast.NodeError {}
		ast.EmptyExpr {}
		ast.AnonFn {
			f.anon_fn(node)
		}
		ast.ArrayDecompose {
			f.array_decompose(node)
		}
		ast.ArrayInit {
			f.array_init(node)
		}
		ast.AsCast {
			f.as_cast(node)
		}
		ast.Assoc {
			f.assoc(node)
		}
		ast.AtExpr {
			f.at_expr(node)
		}
		ast.BoolLiteral {
			f.write(node.val.str())
		}
		ast.CallExpr {
			f.call_expr(node)
		}
		ast.CastExpr {
			f.cast_expr(node)
		}
		ast.ChanInit {
			f.chan_init(mut node)
		}
		ast.CharLiteral {
			f.char_literal(node)
		}
		ast.Comment {
			f.comment(node, same_line: true)
		}
		ast.ComptimeCall {
			f.comptime_call(node)
		}
		ast.ComptimeSelector {
			f.comptime_selector(node)
		}
		ast.ConcatExpr {
			f.concat_expr(node)
		}
		ast.CTempVar {
			eprintln('ast.CTempVar of ${node.orig.str()} should be generated/used only in cgen')
		}
		ast.DumpExpr {
			f.dump_expr(node)
		}
		ast.EnumVal {
			f.enum_val(node)
		}
		ast.FloatLiteral {
			f.write(node.val)
			if node.val.ends_with('.') {
				f.write('0')
			}
		}
		ast.GoExpr {
			f.go_expr(node)
		}
		ast.SpawnExpr {
			f.spawn_expr(node)
		}
		ast.Ident {
			f.ident(node)
		}
		ast.IfExpr {
			f.if_expr(node)
		}
		ast.IfGuardExpr {
			f.if_guard_expr(node)
		}
		ast.IndexExpr {
			f.index_expr(node)
		}
		ast.InfixExpr {
			f.infix_expr(node)
		}
		ast.IntegerLiteral {
			f.write(node.val)
		}
		ast.LambdaExpr {
			f.write('|')
			for i, x in node.params {
				f.expr(x)
				if i < node.params.len - 1 {
					f.write(', ')
				}
			}
			f.write('| ')
			f.expr(node.expr)
		}
		ast.Likely {
			f.likely(node)
		}
		ast.LockExpr {
			f.lock_expr(node)
		}
		ast.MapInit {
			f.map_init(node)
		}
		ast.MatchExpr {
			f.match_expr(node)
		}
		ast.None {
			f.write('none')
		}
		ast.Nil {
			f.write('nil')
		}
		ast.OffsetOf {
			f.offset_of(node)
		}
		ast.OrExpr {
			// shouldn't happen, an or expression is always linked to a call expr or index expr
			panic('fmt: OrExpr should be linked to ast.CallExpr or ast.IndexExpr')
		}
		ast.ParExpr {
			f.par_expr(node)
		}
		ast.PostfixExpr {
			f.postfix_expr(node)
		}
		ast.PrefixExpr {
			f.prefix_expr(node)
		}
		ast.RangeExpr {
			f.range_expr(node)
		}
		ast.SelectExpr {
			f.select_expr(node)
		}
		ast.SelectorExpr {
			f.selector_expr(node)
		}
		ast.SizeOf {
			f.size_of(node)
		}
		ast.IsRefType {
			f.is_ref_type(node)
		}
		ast.SqlExpr {
			f.sql_expr(node)
		}
		ast.SqlQueryDataExpr {
			f.sql_query_data_expr(node)
		}
		ast.StringLiteral {
			f.string_literal(node)
		}
		ast.StringInterLiteral {
			f.string_inter_literal(node)
		}
		ast.StructInit {
			f.struct_init(node)
		}
		ast.TypeNode {
			f.type_expr(node)
		}
		ast.TypeOf {
			f.type_of(node)
		}
		ast.UnsafeExpr {
			f.inside_unsafe = true
			f.unsafe_expr(node)
			f.inside_unsafe = false
		}
		ast.ComptimeType {
			match node.kind {
				.unknown { f.write('\$unknown') }
				.array { f.write('\$array') }
				.array_dynamic { f.write('\$array_dynamic') }
				.array_fixed { f.write('\$array_fixed') }
				.struct { f.write('\$struct') }
				.iface { f.write('\$interface') }
				.map { f.write('\$map') }
				.int { f.write('\$int') }
				.float { f.write('\$float') }
				.sum_type { f.write('\$sumtype') }
				.enum { f.write('\$enum') }
				.alias { f.write('\$alias') }
				.function { f.write('\$function') }
				.option { f.write('\$option') }
				.shared { f.write('\$shared') }
				.string { f.write('\$string') }
				.pointer { f.write('\$pointer') }
				.voidptr { f.write('\$voidptr') }
			}
		}
	}
}

fn expr_is_single_line(expr ast.Expr) bool {
	match expr {
		ast.Comment, ast.IfExpr, ast.MapInit, ast.MatchExpr, ast.SqlQueryDataExpr {
			return false
		}
		ast.AnonFn {
			if !expr.decl.no_body {
				return false
			}
		}
		ast.StructInit {
			if !expr.no_keys && (expr.init_fields.len > 0 || expr.pre_comments.len > 0) {
				return false
			}
		}
		ast.CallExpr {
			if expr.or_block.stmts.len > 1 || expr.args.any(it.expr is ast.CallExpr
				&& it.expr.or_block.stmts.len > 1) {
				return false
			}
		}
		ast.ArrayInit {
			for e in expr.exprs {
				if !expr_is_single_line(e) {
					return false
				}
			}
		}
		ast.ConcatExpr {
			for e in expr.vals {
				if !expr_is_single_line(e) {
					return false
				}
			}
		}
		ast.StringLiteral {
			return expr.pos.line_nr == expr.pos.last_line
		}
		ast.OrExpr {
			if expr.stmts.len == 1 && stmt_is_single_line(expr.stmts[0]) {
				stmt := expr.stmts[0]
				if stmt is ast.ExprStmt && stmt.expr is ast.CallExpr
					&& (stmt.expr as ast.CallExpr).comments.len > 0 {
					if comment := (stmt.expr as ast.CallExpr).comments[0] {
						if !comment.is_multi {
							return false
						}
					}
				}
				return true
			}
			return false
		}
		else {}
	}

	return true
}

fn (mut f Fmt) write_expr_list(exprs []ast.Expr) {
	for i, expr in exprs {
		f.expr(expr)
		if i < exprs.len - 1 {
			f.write(', ')
		}
	}
}

//=== Specific Stmt methods ===//

pub fn (mut f Fmt) assert_stmt(node ast.AssertStmt) {
	f.write('assert ')
	mut expr := node.expr
	expr = expr.remove_par()
	f.expr(expr)
	if node.extra !is ast.EmptyExpr {
		f.write(', ')
		f.expr(node.extra)
	}
	f.writeln('')
}

pub fn (mut f Fmt) assign_stmt(node ast.AssignStmt) {
	for i, left in node.left {
		f.expr(left)
		if i < node.left.len - 1 {
			f.write(', ')
		}
	}
	f.is_assign = true
	f.write(' ${node.op.str()} ')
	right_start_pos := f.out.len
	right_start_len := f.line_len
	can_wrap_rhs := node.right.len == 1 && node.right[0] in [ast.CallExpr, ast.StructInit]
	f.write_expr_list(node.right)
	if can_wrap_rhs && !f.single_line_if && f.line_len > max_len {
		right_str := f.out.after(right_start_pos)
		if !right_str.contains('\n') {
			f.out.go_back_to(right_start_pos)
			f.line_len = right_start_len
			if f.out.last() == ` ` {
				f.out.go_back(1)
				f.line_len--
			}
			f.writeln('')
			f.indent++
			f.write_expr_list(node.right)
			f.indent--
		}
	}
	if node.attr.name != '' {
		f.write(' @[${node.attr.name}]')
	}
	f.comments(node.end_comments, has_nl: false, same_line: true, level: .keep)
	if !f.single_line_if {
		f.writeln('')
	}
	f.is_assign = false
}

pub fn (mut f Fmt) block(node ast.Block) {
	if node.is_unsafe {
		f.write('unsafe ')
	}
	f.write('{')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) debugger_stmt(node ast.DebuggerStmt) {
	f.writeln('\$dbg;')
}

pub fn (mut f Fmt) branch_stmt(node ast.BranchStmt) {
	f.writeln(node.str())
}

pub fn (mut f Fmt) comptime_for(node ast.ComptimeFor) {
	f.write('\$for ${node.val_var} in ')
	if node.typ != ast.void_type {
		f.write(f.no_cur_mod(f.type_to_str_using_aliases(node.typ, f.mod2alias)))
	} else {
		f.expr(node.expr)
	}
	f.write('.${node.kind.str()} {')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) const_decl(node ast.ConstDecl) {
	if node.fields.len == 0 && node.pos.line_nr == node.pos.last_line {
		// remove "const()"
		return
	}

	f.attrs(node.attrs)
	if !node.is_block {
		if node.is_pub {
			f.write('pub ')
		}
	}
	f.inside_const = true
	defer { f.inside_const = false }
	if !node.is_block {
		f.write('const ')
	}
	mut prev_field := if node.fields.len > 0 {
		ast.Node(node.fields[0])
	} else {
		ast.Node(ast.NodeError{})
	}
	for fidx, field in node.fields {
		if field.comments.len > 0 {
			if f.should_insert_newline_before_node(ast.Expr(field.comments[0]), prev_field) {
				f.writeln('')
			}
			f.comments(field.comments, same_line: true)
			prev_field = ast.Expr(field.comments.last())
		}
		if node.is_block && f.should_insert_newline_before_node(field, prev_field) {
			f.writeln('')
		}
		name := field.name.after('.')
		if node.is_block {
			// const() blocks are deprecated, prepend "const" before each value
			if node.is_pub {
				f.write('pub ')
			}
			f.write('const ')
		}
		if field.is_virtual_c {
			f.write('C.')
		}
		f.write('${name} ')
		if field.is_virtual_c {
			// f.typ(field.typ)
			f.write(f.type_to_str(field.typ))
		} else {
			f.write('= ')
			f.expr(field.expr)
		}
		f.comments(field.end_comments, same_line: true)
		if node.is_block && fidx < node.fields.len - 1 && node.fields.len > 1 {
			// old style grouped consts, converted to the new style ungrouped const
			f.writeln('')
		} else if node.end_comments.len > 0 {
			// Write out single line comments after const expr if present
			// E.g.: `const x = 1 // <comment>`
			if node.end_comments[0].text.contains('\n') {
				f.writeln('\n')
			}
			f.comments(node.end_comments, same_line: true, has_nl: false)
		}
		prev_field = field
	}

	f.writeln('')
}

fn (mut f Fmt) defer_stmt(node ast.DeferStmt) {
	f.write('defer')
	if node.mode == .function {
		f.write('(fn)')
	}
	if node.stmts.len == 0 {
		f.writeln(' {}')
	} else if node.stmts.len == 1 && node.pos.line_nr == node.pos.last_line
		&& stmt_is_single_line(node.stmts[0]) {
		f.write(' { ')
		// the control stmts (return/break/continue...) print a newline inside them,
		// so, since this'll all be on one line, trim any possible whitespace
		str := f.node_str(node.stmts[0]).trim_space()
		// single_line := ' defer { ${str} }'
		// if single_line.len + f.line_len <= fmt.max_len {
		// f.write(single_line)
		// return
		//}
		f.write(str)

		// f.stmt(node.stmts[0])
		f.writeln(' }')
	} else {
		f.writeln(' {')
		f.stmts(node.stmts)
		f.writeln('}')
	}
}

pub fn (mut f Fmt) expr_stmt(node ast.ExprStmt) {
	f.comments(node.comments)
	f.expr(node.expr)
	if !f.single_line_if {
		f.writeln('')
	}
}

pub fn (mut f Fmt) enum_decl(node ast.EnumDecl) {
	f.attrs(node.attrs)
	if node.is_pub {
		f.write('pub ')
	}
	mut name := node.name.after('.')
	if node.typ != ast.int_type && node.typ != ast.invalid_type {
		senum_type := f.type_to_str_using_aliases(node.typ, f.mod2alias)
		name += ' as ${senum_type}'
	}
	if node.fields.len == 0 && node.pos.line_nr == node.pos.last_line {
		f.writeln('enum ${name} {}\n')
		return
	}
	f.writeln('enum ${name} {')
	f.comments(node.comments, same_line: true, level: .indent)

	mut value_align := new_field_align(use_break_line: true)
	mut attr_align := new_field_align(use_threshold: true)
	mut comment_align := new_field_align(use_threshold: true)
	for field in node.fields {
		if field.has_expr {
			value_align.add_info(field.name.len, field.pos.line_nr, field.has_break_line)
		}
		attrs_len := inline_attrs_len(field.attrs)
		if field.attrs.len > 0 {
			if field.has_expr {
				attr_align.add_info(field.expr.str().len + 2, field.pos.line_nr,
					field.has_break_line)
			} else {
				attr_align.add_info(field.name.len, field.pos.line_nr, field.has_break_line)
			}
		}
		if field.comments.len > 0 {
			if field.attrs.len > 0 {
				comment_align.add_info(attrs_len, field.pos.line_nr, field.has_break_line)
			} else if field.has_expr {
				comment_align.add_info(field.expr.str().len + 2, field.pos.line_nr,
					field.has_break_line)
			} else {
				comment_align.add_info(field.name.len, field.pos.line_nr, field.has_break_line)
			}
		}
	}

	for i, field in node.fields {
		if i > 0 && field.has_prev_newline {
			f.writeln('')
		}
		if field.pre_comments.len > 0 {
			f.comments(field.pre_comments, has_nl: true, level: .indent)
		}
		f.write('\t${field.name}')
		if field.has_expr {
			f.write(' '.repeat(value_align.max_len(field.pos.line_nr) - field.name.len))
			f.write(' = ')
			f.expr(field.expr)
		}
		attrs_len := inline_attrs_len(field.attrs)
		if field.attrs.len > 0 {
			if field.has_expr {
				f.write(' '.repeat(attr_align.max_len(field.pos.line_nr) - field.expr.str().len - 1))
			} else {
				f.write(' '.repeat(attr_align.max_len(field.pos.line_nr) - field.name.len + 1))
			}
			f.single_line_attrs(field.attrs, same_line: true)
		}
		// f.comments(field.comments, same_line: true, has_nl: false, level: .indent)
		if field.comments.len > 0 {
			if field.attrs.len > 0 {
				f.write(' '.repeat(comment_align.max_len(field.pos.line_nr) - attrs_len + 1))
			} else if field.has_expr {
				f.write(' '.repeat(comment_align.max_len(field.pos.line_nr) - field.expr.str().len -
					1))
			} else {
				f.write(' '.repeat(comment_align.max_len(field.pos.line_nr) - field.name.len + 1))
			}
			f.comments(field.comments, same_line: true, has_nl: false)
		}
		f.writeln('')
		f.comments(field.next_comments, has_nl: true, level: .indent)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) fn_decl(node ast.FnDecl) {
	f.attrs(node.attrs)
	if node.name.starts_with('C.') {
		f.is_c_function = true
	}
	f.table.new_int_fmt_fix = f.table.new_int && (f.is_translated_module || f.is_c_function)
	f.write(f.table.stringify_fn_decl(&node, f.cur_mod, f.mod2alias, true))
	f.table.new_int_fmt_fix = false
	f.is_c_function = false
	// Handle trailing comments after fn header declarations
	if node.no_body && node.end_comments.len > 0 {
		first_comment := node.end_comments[0]
		if first_comment.text.contains('\n') {
			f.writeln('\n')
		} else {
			f.write(' ')
		}
		f.comment(first_comment)
		if node.end_comments.len > 1 {
			f.writeln('\n')
			comments := node.end_comments[1..]
			for i, comment in comments {
				f.comment(comment)
				if i != comments.len - 1 {
					f.writeln('\n')
				}
			}
		}
	}
	f.fn_body(node)
}

pub fn (mut f Fmt) anon_fn(node ast.AnonFn) {
	f.table.new_int_fmt_fix = f.table.new_int && (f.is_translated_module || f.is_c_function)
	f.write(f.table.stringify_anon_decl(&node, f.cur_mod, f.mod2alias)) // `Expr` instead of `ast.Expr` in mod ast
	f.table.new_int_fmt_fix = false
	f.fn_body(node.decl)
}

fn (mut f Fmt) fn_body(node ast.FnDecl) {
	prev_fn_scope := f.fn_scope
	f.fn_scope = node.scope
	defer { f.fn_scope = prev_fn_scope }
	if node.language == .v || (node.is_method && node.language == .js) {
		if !node.no_body {
			f.write(' {')
			pre_comments := node.comments.filter(it.pos.pos < node.name_pos.pos)
			body_comments := node.comments[pre_comments.len..]
			f.comments(body_comments, same_line: true)
			if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
				if body_comments.len == 0 {
					f.writeln('')
				}
				f.stmts(node.stmts)
			}
			f.write('}')
			if node.end_comments.len > 0 {
				first_comment := node.end_comments[0]
				if first_comment.text.contains('\n') {
					f.writeln('\n')
				} else {
					f.write(' ')
				}
				f.comment(first_comment)
				if node.end_comments.len > 1 {
					f.writeln('\n')
					comments := node.end_comments[1..]
					for i, comment in comments {
						f.comment(comment)
						if i != comments.len - 1 {
							f.writeln('\n')
						}
					}
				}
			}
		}
		if !node.is_anon {
			f.writeln('')
		}
	} else {
		f.writeln('')
	}
}

pub fn (mut f Fmt) for_c_stmt(node ast.ForCStmt) {
	if node.label.len > 0 {
		f.write('${node.label}: ')
	}
	init_comments := node.comments.filter(it.pos.pos < node.init.pos.pos)
	cond_comments := node.comments[init_comments.len..].filter(it.pos.pos < node.cond.pos().pos)
	inc_comments :=
		node.comments[(init_comments.len + cond_comments.len)..].filter(it.pos.pos < node.inc.pos.pos)
	after_inc_comments := node.comments[(init_comments.len + cond_comments.len + inc_comments.len)..]
	f.write('for ')
	if node.has_init {
		if init_comments.len > 0 {
			f.comments(init_comments)
			f.write(' ')
		}
		f.single_line_if = true // to keep all for ;; exprs on the same line
		f.stmt(node.init)
		f.single_line_if = false
	}
	f.write('; ')
	if cond_comments.len > 0 {
		f.comments(cond_comments)
		f.write(' ')
	}
	f.expr(node.cond)
	f.write('; ')
	if inc_comments.len > 0 {
		f.comments(inc_comments)
		f.write(' ')
	}
	f.stmt(node.inc)
	f.remove_new_line()
	if after_inc_comments.len > 0 {
		f.comments(after_inc_comments)
	}
	if f.out.len > 1 && !f.out.last_n(1)[0].is_space() {
		f.write(' ')
	}
	f.write('{')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) for_in_stmt(node ast.ForInStmt) {
	if node.label.len > 0 {
		f.write('${node.label}: ')
	}
	kv_comments := node.comments.filter(it.pos.pos < node.kv_pos.pos)
	cond_comments := node.comments[kv_comments.len..].filter(it.pos.pos < node.cond.pos().pos)
	after_comments := node.comments[(kv_comments.len + cond_comments.len)..]
	f.write('for ')
	if kv_comments.len > 0 {
		f.comments(kv_comments)
		f.write(' ')
	}
	if node.key_var != '' {
		f.write(node.key_var)
	}
	if node.val_var != '' {
		if node.key_var != '' {
			f.write(', ')
		}
		if node.val_is_mut {
			f.write('mut ')
		}
		f.write(node.val_var)
	}
	f.write(' in ')
	if cond_comments.len > 0 {
		f.comments(cond_comments)
		f.write(' ')
	}
	f.expr(node.cond)
	if node.is_range {
		f.write(' .. ')
		f.expr(node.high)
	}
	if after_comments.len > 0 {
		f.comments(after_comments)
	}
	if f.out.len > 1 && !f.out.last_n(1)[0].is_space() {
		f.write(' ')
	}
	f.write('{')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) for_stmt(node ast.ForStmt) {
	if node.label.len > 0 {
		f.write('${node.label}: ')
	}
	f.write('for ')
	if node.comments.len > 0 {
		f.comments(node.comments)
	}
	f.expr(node.cond)
	if f.out.len > 1 && !f.out.last_n(1)[0].is_space() {
		f.write(' ')
	}
	f.write('{')
	if node.stmts.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
		f.stmts(node.stmts)
	}
	f.writeln('}')
}

pub fn (mut f Fmt) global_decl(node ast.GlobalDecl) {
	f.attrs(node.attrs)
	if node.fields.len == 0 && node.pos.line_nr == node.pos.last_line {
		// remove "__global()"
		return
	}
	f.write('__global ')
	mut max := 0
	if node.is_block {
		f.writeln('(')
		f.indent++
		for field in node.fields {
			if field.name.len > max {
				max = field.name.len
			}
		}
	}
	for field in node.fields {
		f.comments(field.comments, same_line: true)
		if field.is_const {
			f.write('const ')
		}
		if field.is_volatile {
			f.write('volatile ')
		}
		f.write('${field.name} ')
		f.write(' '.repeat(max - field.name.len))
		if field.has_expr {
			f.write('= ')
			f.expr(field.expr)
		} else {
			f.write('${f.type_to_str_using_aliases(field.typ, f.mod2alias)}')
		}
		if node.is_block {
			f.writeln('')
		}
	}
	f.comments_after_last_field(node.end_comments)
	if node.is_block {
		f.indent--
		f.writeln(')')
	} else {
		f.writeln('')
	}
}

pub fn (mut f Fmt) spawn_expr(node ast.SpawnExpr) {
	f.write('spawn ')
	f.call_expr(node.call_expr)
}

pub fn (mut f Fmt) go_expr(node ast.GoExpr) {
	f.write('go ')
	f.call_expr(node.call_expr)
}

pub fn (mut f Fmt) goto_label(node ast.GotoLabel) {
	f.writeln('${node.name}:')
}

pub fn (mut f Fmt) goto_stmt(node ast.GotoStmt) {
	f.writeln('goto ${node.name}')
}

pub fn (mut f Fmt) hash_stmt(node ast.HashStmt) {
	f.attrs(node.attrs)
	f.writeln('#${node.val}')
}

pub fn (mut f Fmt) interface_decl(node ast.InterfaceDecl) {
	f.attrs(node.attrs)
	if node.is_pub {
		f.write('pub ')
	}
	f.write('interface ')
	f.write_language_prefix(node.language)
	name := node.name.after('.') // strip prepended module
	f.write(name)
	f.write_generic_types(node.generic_types)
	f.write(' {')
	if node.fields.len > 0 || node.methods.len > 0 || node.pos.line_nr < node.pos.last_line {
		f.writeln('')
	}
	f.comments_before_field(node.pre_comments)
	for embed in node.embeds {
		f.write('\t${embed.name}')
		f.comments(embed.comments, same_line: true, has_nl: false, level: .indent)
		f.writeln('')
	}
	immut_fields := if node.mut_pos < 0 { node.fields } else { node.fields[..node.mut_pos] }
	mut_fields := if node.mut_pos < 0 { []ast.StructField{} } else { node.fields[node.mut_pos..] }

	mut immut_methods := node.methods.clone()
	mut mut_methods := []ast.FnDecl{}
	for i, method in node.methods {
		if method.params[0].is_mut {
			immut_methods = node.methods[..i].clone()
			mut_methods = node.methods[i..].clone()
			break
		}
	}

	mut type_align := new_field_align(use_break_line: true)
	mut comment_align := new_field_align(use_threshold: true)
	mut default_expr_align := new_field_align(use_threshold: true)
	mut attr_align := new_field_align(use_threshold: true)
	mut field_types := []string{cap: node.fields.len}

	// Calculate the alignments first
	f.calculate_alignment(node.fields, mut type_align, mut comment_align, mut default_expr_align, mut
		attr_align, mut field_types)

	mut method_comment_align := new_field_align(use_threshold: true)
	for method in node.methods {
		end_comments := method.comments.filter(it.pos.pos > method.pos.pos)
		if end_comments.len > 0 {
			f.table.new_int_fmt_fix = f.table.new_int && (f.is_translated_module || f.is_c_function)
			method_str :=
				f.table.stringify_fn_decl(&method, f.cur_mod, f.mod2alias, false).all_after_first('fn ')
			f.table.new_int_fmt_fix = false
			method_comment_align.add_info(method_str.len, method.pos.line_nr, method.has_break_line)
		}
	}

	// TODO: alignment, comments, etc.
	for field in immut_fields {
		if field.has_prev_newline {
			f.writeln('')
		}
		f.interface_field(field, mut type_align, mut comment_align)
	}
	for method in immut_methods {
		if method.has_prev_newline {
			f.writeln('')
		}
		f.interface_method(method, mut method_comment_align)
	}
	if mut_fields.len + mut_methods.len > 0 {
		f.writeln('mut:')
		for field in mut_fields {
			if field.has_prev_newline {
				f.writeln('')
			}
			f.interface_field(field, mut type_align, mut comment_align)
		}
		for method in mut_methods {
			if method.has_prev_newline {
				f.writeln('')
			}
			f.interface_method(method, mut method_comment_align)
		}
	}
	f.writeln('}\n')
}

enum AlignState {
	plain
	has_attributes
	has_default_expression
	has_everything
}

pub fn (mut f Fmt) calculate_alignment(fields []ast.StructField, mut type_align FieldAlign, mut comment_align FieldAlign,
	mut default_expr_align FieldAlign, mut attr_align FieldAlign, mut field_types []string) {
	// Calculate the alignments first
	mut prev_state := AlignState.plain
	for field in fields {
		ft := f.no_cur_mod(f.type_to_str_using_aliases(field.typ, f.mod2alias))
		// Handle anon structs recursively
		field_types << ft
		attrs_len := inline_attrs_len(field.attrs)
		end_pos := field.pos.pos + field.pos.len
		type_align.add_info(field.name.len, field.pos.line_nr, field.has_break_line)
		if field.has_default_expr {
			default_expr_align.add_info(ft.len, field.pos.line_nr, field.has_break_line)
		}
		if field.attrs.len > 0 {
			attr_align.add_info(ft.len, field.pos.line_nr, field.has_break_line)
		}
		for comment in field.comments {
			if comment.pos.pos >= end_pos {
				if comment.pos.line_nr == field.pos.line_nr {
					if field.attrs.len > 0 {
						if prev_state != AlignState.has_attributes {
							comment_align.add_new_info(attrs_len, comment.pos.line_nr)
						} else {
							comment_align.add_info(attrs_len, comment.pos.line_nr,
								field.has_break_line)
						}
						prev_state = AlignState.has_attributes
					} else if field.has_default_expr {
						if prev_state != AlignState.has_default_expression {
							comment_align.add_new_info(field.default_expr.str().len + 2,
								comment.pos.line_nr)
						} else {
							comment_align.add_info(field.default_expr.str().len + 2,
								comment.pos.line_nr, field.has_break_line)
						}
						prev_state = AlignState.has_default_expression
					} else {
						if prev_state != AlignState.has_everything {
							comment_align.add_new_info(ft.len, comment.pos.line_nr)
						} else {
							comment_align.add_info(ft.len, comment.pos.line_nr,
								field.has_break_line)
						}
						prev_state = AlignState.has_everything
					}
				}
				continue
			}
		}
	}
}

pub fn (mut f Fmt) interface_field(field ast.StructField, mut type_align FieldAlign, mut comment_align FieldAlign) {
	ft := f.no_cur_mod(f.type_to_str_using_aliases(field.typ, f.mod2alias))
	mut pre_cmts, mut end_cmts, mut next_line_cmts := []ast.Comment{}, []ast.Comment{}, []ast.Comment{}
	for cmt in field.comments {
		match true {
			cmt.pos.pos < field.pos.pos { pre_cmts << cmt }
			cmt.pos.line_nr > field.pos.last_line { next_line_cmts << cmt }
			else { end_cmts << cmt }
		}
	}
	if pre_cmts.len > 0 {
		f.comments(pre_cmts, level: .indent)
	}

	sym := f.table.sym(field.typ)
	if sym.info is ast.Struct {
		if sym.info.is_anon {
			f.write('\t${field.name} ')
			f.write_anon_struct_field_decl(field.typ, ast.StructDecl{ fields: sym.info.fields })
		} else {
			f.write('\t${field.name} ')
		}
	} else {
		f.write('\t${field.name} ')
	}
	if !(sym.info is ast.Struct && sym.info.is_anon) {
		f.write(' '.repeat(type_align.max_len(field.pos.line_nr) - field.name.len))
		f.write(ft)
	}
	if end_cmts.len > 0 {
		f.write(' '.repeat(comment_align.max_len(field.pos.line_nr) - ft.len + 1))
		f.comments(end_cmts, level: .indent)
	} else {
		f.writeln('')
	}
	if next_line_cmts.len > 0 {
		f.comments(next_line_cmts, level: .indent)
	}
}

pub fn (mut f Fmt) interface_method(method ast.FnDecl, mut comment_align FieldAlign) {
	before_comments := method.comments.filter(it.pos.pos < method.pos.pos)
	end_comments := method.comments.filter(it.pos.pos > method.pos.pos)
	if before_comments.len > 0 {
		f.comments(before_comments, level: .indent)
	}
	f.write('\t')
	f.table.new_int_fmt_fix = f.table.new_int && (f.is_translated_module || f.is_c_function)
	method_str :=
		f.table.stringify_fn_decl(&method, f.cur_mod, f.mod2alias, false).all_after_first('fn ')
	f.table.new_int_fmt_fix = false
	f.write(method_str)
	if end_comments.len > 0 {
		f.write(' '.repeat(comment_align.max_len(method.pos.line_nr) - method_str.len + 1))
		f.comments(end_comments, level: .indent)
	} else {
		f.writeln('')
	}
	f.comments(method.next_comments, level: .indent)
}

pub fn (mut f Fmt) module_stmt(mod ast.Module) {
	f.set_current_module_name(mod.name)
	if mod.is_skipped {
		return
	}
	f.is_translated_module = mod.attrs.any(it.name == 'translated')
	f.attrs(mod.attrs)
	f.writeln('module ${mod.short_name}\n')
	if f.import_pos == 0 {
		f.import_pos = f.out.len
	}
}

pub fn (mut f Fmt) return_stmt(node ast.Return) {
	f.write('return')
	if node.exprs.len > 0 {
		f.write(' ')
		mut sum_len := 0
		// Loop over all return values. In normal returns this will only run once.
		for i, expr in node.exprs {
			pre_comments := node.comments[sum_len..].filter(it.pos.pos < expr.pos().pos)
			sum_len += pre_comments.len
			if pre_comments.len > 0 {
				f.comments(pre_comments)
				f.write(' ')
			}
			if expr is ast.ParExpr && expr.comments.len == 0 {
				f.expr(expr.expr)
			} else {
				f.expr(expr)
			}
			if i < node.exprs.len - 1 {
				f.write(', ')
			}
		}
	}
	if !f.single_line_if {
		f.writeln('')
	}
}

pub fn (mut f Fmt) sql_stmt(node ast.SqlStmt) {
	f.write('sql ')
	f.expr(node.db_expr)
	f.writeln(' {')

	for line in node.lines {
		f.comments(line.pre_comments, level: .indent)
		f.sql_stmt_line(line)
		f.comments(line.end_comments, level: .indent)
	}
	f.write('}')
	f.or_expr(node.or_expr)
	f.writeln('')
}

pub fn (mut f Fmt) sql_stmt_line(node ast.SqlStmtLine) {
	sym := f.table.sym(node.table_expr.typ)
	mut table_name := sym.name
	if !table_name.starts_with('C.') && !table_name.starts_with('JS.') {
		table_name = f.no_cur_mod(f.short_module(sym.name)) // TODO: f.type_to_str?
	}

	f.write('\t')
	match node.kind {
		.insert {
			f.writeln('insert ${node.object_var} into ${table_name}')
		}
		.upsert {
			f.writeln('upsert ${node.object_var} into ${table_name}')
		}
		.update {
			if node.is_dynamic {
				f.write('dynamic update ${table_name} set ')
				f.expr(node.update_data_expr)
				f.write(' ')
				f.write('where ')
				f.expr(node.where_expr)
				f.writeln('')
			} else {
				mut has_multiline_update_expr := false
				for expr in node.update_exprs {
					if f.node_str(expr).contains('\n') {
						has_multiline_update_expr = true
						break
					}
				}
				if has_multiline_update_expr {
					f.writeln('update ${table_name} set')
					// SQL block lines use a manual extra tab, so nested update values need two
					// formatter indent levels to stay visually nested.
					f.indent += 2
					for i, col in node.updated_columns {
						f.write('${col} = ')
						f.expr(node.update_exprs[i])
						if i < node.updated_columns.len - 1 {
							f.write(',')
						}
						f.writeln('')
					}
					f.indent -= 2
				} else {
					f.write('update ${table_name} set ')
					for i, col in node.updated_columns {
						f.write('${col} = ')
						f.expr(node.update_exprs[i])
						if i < node.updated_columns.len - 1 {
							f.write(', ')
						} else {
							f.write(' ')
						}
						f.wrap_long_line(3, true)
					}
				}
				if has_multiline_update_expr {
					f.write('\twhere ')
				} else {
					f.write('where ')
				}
				f.expr(node.where_expr)
				f.writeln('')
			}
		}
		.delete {
			f.write('delete from ${table_name} where ')
			f.expr(node.where_expr)
			f.writeln('')
		}
		.create {
			f.writeln('create table ${table_name}')
		}
		.drop {
			f.writeln('drop table ${table_name}')
		}
	}
}

pub fn (mut f Fmt) type_decl(node ast.TypeDecl) {
	match node {
		ast.AliasTypeDecl { f.alias_type_decl(node) }
		ast.FnTypeDecl { f.fn_type_decl(node) }
		ast.SumTypeDecl { f.sum_type_decl(node) }
	}

	f.writeln('')
}

pub fn (mut f Fmt) alias_type_decl(node ast.AliasTypeDecl) {
	f.attrs(node.attrs)
	if node.is_pub {
		f.write('pub ')
	}
	// aliases of anon struct: `type Foo = struct {}`
	sym := f.table.sym(node.parent_type)
	if sym.info is ast.Struct {
		if sym.info.is_anon {
			f.write('type ${node.name} = ')
			f.struct_decl(ast.StructDecl{ fields: sym.info.fields }, true)
			f.comments(node.comments, has_nl: false)
			return
		}
	}
	ptype := f.type_to_str_using_aliases(node.parent_type, f.mod2alias)
	f.write('type ${node.name} = ${ptype}')

	f.comments(node.comments, has_nl: false)
}

pub fn (mut f Fmt) fn_type_decl(node ast.FnTypeDecl) {
	f.attrs(node.attrs)
	if node.is_pub {
		f.write('pub ')
	}
	typ_sym := f.table.sym(node.typ)
	fn_typ_info := typ_sym.info as ast.FnType
	fn_info := fn_typ_info.func
	fn_name := f.no_cur_mod(node.name)
	mut generic_types_str := ''
	if node.generic_types.len > 0 {
		generic_names := node.generic_types.map(f.table.sym(it).name)
		generic_types_str = '[${generic_names.join(', ')}]'
	}
	f.write('type ${fn_name}${generic_types_str} = fn (')
	for i, arg in fn_info.params {
		if arg.is_mut {
			f.write(arg.typ.share().str() + ' ')
		}
		f.write(arg.name)
		mut s := f.no_cur_mod(f.type_to_str_using_aliases(arg.typ, f.mod2alias))
		if arg.is_mut {
			if s.starts_with('&') {
				s = s[1..]
			}
			s = s.trim_left('shared ')
		}
		is_last_arg := i == fn_info.params.len - 1
		should_add_type := true || is_last_arg
			|| fn_info.params[i + 1].typ != arg.typ
			|| (fn_info.is_variadic && i == fn_info.params.len - 2)
		if should_add_type {
			ns := if arg.name == '' { '' } else { ' ' }
			if fn_info.is_variadic && is_last_arg {
				f.write(ns + '...' + s)
			} else {
				f.write(ns + s)
			}
		}
		if !is_last_arg {
			f.write(', ')
		}
	}
	f.write(')')
	if fn_info.return_type.idx() != ast.void_type_idx {
		ret_str := f.no_cur_mod(f.type_to_str_using_aliases(fn_info.return_type, f.mod2alias))
		f.write(' ${ret_str}')
	} else if fn_info.return_type.has_flag(.option) {
		f.write(' ?')
	} else if fn_info.return_type.has_flag(.result) {
		f.write(' !')
	}

	f.comments(node.comments, has_nl: false)
	f.writeln('')
}

struct Variant {
	name string
	id   int
}

fn (mut f Fmt) sum_type_variant_comments(variant ast.TypeNode) {
	if variant.end_comments.len == 0 {
		return
	}
	mut same_line_comments := []ast.Comment{}
	mut follow_up_comments := []ast.Comment{}
	for comment in variant.end_comments {
		if comment.pos.line_nr == variant.pos.last_line {
			same_line_comments << comment
		} else {
			follow_up_comments << comment
		}
	}
	if same_line_comments.len > 0 {
		f.comments(same_line_comments, has_nl: false)
	}
	if follow_up_comments.len > 0 {
		f.writeln('')
		f.comments(follow_up_comments, has_nl: false, level: .indent)
	}
}

pub fn (mut f Fmt) sum_type_decl(node ast.SumTypeDecl) {
	f.attrs(node.attrs)
	start_pos := f.out.len
	if node.is_pub {
		f.write('pub ')
	}
	f.write('type ${node.name}')
	f.write_generic_types(node.generic_types)
	f.write(' = ')

	mut variants := []Variant{cap: node.variants.len}
	for i, variant in node.variants {
		variants << Variant{f.type_to_str_using_aliases(variant.typ, f.mod2alias), i}
	}
	// The first variant is now used as the default variant when doing `a:= Sumtype{}`, i.e. a change in semantics.
	// Sorting is disabled, because it is no longer a cosmetic change - it can change the default variant.
	// variants.sort(a.name < b.name)

	mut separator := ' | '
	mut is_multiline := false
	// if line length is too long, put each type on its own line
	mut line_length := f.out.len - start_pos
	for variant in variants {
		// 3 = length of ' = ' or ' | '
		line_length += 3 + variant.name.len
		if line_length > max_len || (variant.id != node.variants.len - 1
			&& node.variants[variant.id].end_comments.len > 0) {
			separator = '\n\t| '
			is_multiline = true
			break
		}
	}

	for i, variant in variants {
		if i > 0 {
			f.write(separator)
		}
		f.write(variant.name)
		if node.variants[variant.id].end_comments.len > 0 && is_multiline {
			f.sum_type_variant_comments(node.variants[variant.id])
		}
	}
	if !is_multiline {
		f.comments(node.variants.last().end_comments,
			has_nl: false
		)
	}
}

//=== Specific Expr methods ===//

pub fn (mut f Fmt) array_decompose(node ast.ArrayDecompose) {
	f.write('...')
	f.expr(node.expr)
}

pub fn (mut f Fmt) array_init(node ast.ArrayInit) {
	typed_fixed_literal := node.is_fixed && node.has_val && node.typ != 0
		&& node.typ != ast.void_type
	if node.is_fixed && node.is_option {
		f.write('?')
	}
	if node.exprs.len == 0 && ((node.typ != 0 && node.typ != ast.void_type)
		|| node.elem_type_expr !is ast.EmptyExpr) {
		// `x := []string{}`
		if node.alias_type != ast.void_type {
			f.write(f.type_to_str_using_aliases(node.alias_type, f.mod2alias))
		} else if node.elem_type_expr !is ast.EmptyExpr {
			f.write('[]')
			f.expr(node.elem_type_expr)
		} else {
			f.write(f.type_to_str_using_aliases(node.typ, f.mod2alias))
		}
		f.write('{')
		if node.has_len {
			f.write('len: ')
			f.expr(node.len_expr)
			if node.has_cap || node.has_init {
				f.write(', ')
			}
		}
		if node.has_cap {
			f.write('cap: ')
			f.expr(node.cap_expr)
			if node.has_init {
				f.write(', ')
			}
		}
		if node.has_init {
			f.write('init: ')
			old_is_array_init := f.is_array_init
			f.is_array_init = true
			f.expr(node.init_expr)
			f.is_array_init = old_is_array_init
		}
		f.write('}')
		return
	}
	if typed_fixed_literal && f.array_init_depth == 0 {
		fixed_literal_type := if node.literal_typ != ast.void_type {
			node.literal_typ
		} else {
			node.typ.clear_option_and_result()
		}
		f.write(f.type_to_str_using_aliases(fixed_literal_type, f.mod2alias))
	}
	// `[1,2,3]`
	f.write('[')
	mut inc_indent := false
	mut last_line_nr := node.pos.line_nr // to have the same newlines between array elements
	f.array_init_depth++
	if node.pre_cmnts.len > 0 {
		if node.pre_cmnts[0].pos.line_nr > last_line_nr {
			f.writeln('')
		}
	}
	if node.has_update_expr {
		f.write('...')
		f.expr(node.update_expr)
		if node.exprs.len > 0 {
			f.write(',')
		}
		if node.update_expr_comments.len > 0 {
			f.write(' ')
			f.comments(node.update_expr_comments,
				prev_line: node.update_expr_pos.last_line
				has_nl:    false
			)
			last_line_nr = node.update_expr_comments.last().pos.last_line
		} else {
			last_line_nr = node.update_expr_pos.last_line
		}
		if node.exprs.len > 0 && node.update_expr_comments.len == 0 {
			f.write(' ')
		}
	}
	for i, c in node.pre_cmnts {
		if i < node.pre_cmnts.len - 1 {
			if c.pos.last_line < node.pre_cmnts[i + 1].pos.line_nr {
				f.comment(c, level: .indent)
				f.writeln('')
			} else {
				f.comment(c, level: .indent)
				f.write(' ')
			}
		} else {
			next_line := if node.exprs.len > 0 {
				node.exprs[0].pos().line_nr
			} else {
				node.pos.last_line
			}
			if c.pos.last_line < next_line {
				f.comment(c, level: .indent)
				if node.exprs.len == 0 {
					f.writeln('')
				}
			} else {
				f.comment(c, level: .indent)
				if node.exprs.len > 0 {
					f.write(' ')
				}
			}
		}
		last_line_nr = c.pos.last_line
	}
	mut set_comma := false
	for i, expr in node.exprs {
		pos := expr.pos()
		if i == 0 {
			if f.array_init_depth > f.array_init_break.len {
				f.array_init_break << pos.line_nr > last_line_nr
					|| f.line_len + expr.pos().len > break_points[3]
			}
		}
		mut line_break := f.array_init_break[f.array_init_depth - 1]
		mut penalty := if line_break { 0 } else { 4 }
		if penalty > 0 {
			if i == 0
				|| node.exprs[i - 1] in [ast.ArrayInit, ast.StructInit, ast.MapInit, ast.CallExpr] {
				penalty--
			}
			if expr in [ast.ArrayInit, ast.StructInit, ast.MapInit, ast.CallExpr] {
				penalty--
			}
		}
		mut is_new_line := f.wrap_long_line(penalty, !inc_indent)
		if is_new_line && !inc_indent {
			f.indent++
			inc_indent = true
		}
		single_line_expr := expr_is_single_line(expr)
		if single_line_expr {
			mut estr := ''
			if !is_new_line && !f.buffering && f.line_len + expr.pos().len > max_len {
				if inc_indent {
					estr = f.node_str(expr)
				}
				f.writeln('')
				is_new_line = true
				if !inc_indent {
					f.indent++
					inc_indent = true
					f.write_indent()
					f.empty_line = false
					estr = f.node_str(expr)
				}
				if i == 0 {
					f.array_init_break[f.array_init_depth - 1] = true
					line_break = true
				}
			} else {
				estr = f.node_str(expr)
			}
			if !is_new_line && i > 0 {
				f.write(' ')
			}
			f.write(estr)
		} else {
			if !is_new_line && i > 0 {
				f.write(' ')
			}
			// When the array stays on a single source line, keep nested
			// struct inits on a single line too (instead of expanding their
			// named fields onto multiple lines).
			keep_struct_inline := !line_break && expr is ast.StructInit
				&& expr.pos.line_nr == expr.pos.last_line && expr.pre_comments.len == 0
			old_single_line_fields := f.single_line_fields
			if keep_struct_inline {
				f.single_line_fields = true
			}
			f.expr(expr)
			f.single_line_fields = old_single_line_fields
		}
		mut last_comment_was_inline := false
		mut has_comments := node.ecmnts[i].len > 0
		if i < node.ecmnts.len && has_comments {
			expr_pos := expr.pos()
			for icmt, cmt in node.ecmnts[i] {
				if !set_comma && cmt.pos.pos > expr_pos.pos + expr_pos.len + 2 {
					if icmt > 0 {
						if last_comment_was_inline {
							f.write(',')
							set_comma = true
						}
					} else {
						f.write(',') // first comment needs a comma
						set_comma = true
					}
				}
				if cmt.pos.line_nr > expr_pos.last_line {
					f.writeln('')
					f.comment(cmt)
				} else {
					if !set_comma {
						f.write(',')
						set_comma = true
					}
					f.write(' ')
					f.comment(cmt)
					if !line_break {
						f.writeln('')
					}
				}
			}
		} else if i == node.exprs.len - 1 && !line_break {
			is_new_line = false
		}

		mut put_comma := !set_comma
		if has_comments && !last_comment_was_inline {
			put_comma = false
		}
		if i == node.exprs.len - 1 {
			if is_new_line {
				if put_comma {
					f.write(',')
				}
				f.writeln('')
			}
		} else if put_comma {
			f.write(',')
		}
		last_line_nr = pos.last_line
		set_comma = false
	}
	f.array_init_depth--
	if f.array_init_depth == 0 {
		f.array_init_break = []
	}
	if inc_indent {
		f.indent--
	}
	f.write(']')
	// `[100]u8`
	if node.is_fixed {
		if node.has_val {
			if typed_fixed_literal {
				return
			}
			if node.from_to_fixed_size {
				f.write('.to_fixed_size()')
			} else {
				f.write('!')
			}
			return
		}
		f.write(f.type_to_str_using_aliases(node.elem_type, f.mod2alias))
		if node.has_init {
			f.write('{init: ')
			f.expr(node.init_expr)
			f.write('}')
		} else {
			f.write('{}')
		}
	}
}

pub fn (mut f Fmt) as_cast(node ast.AsCast) {
	type_str := f.type_to_str_using_aliases(node.typ, f.mod2alias)
	f.expr(node.expr)
	f.write(' as ${type_str}')
}

pub fn (mut f Fmt) assoc(node ast.Assoc) {
	f.writeln('{')
	f.indent++
	f.writeln('...${node.var_name}')
	for i, field in node.fields {
		f.write('${field}: ')
		f.expr(node.exprs[i])
		f.writeln('')
	}
	f.indent--
	f.write('}')
}

pub fn (mut f Fmt) at_expr(node ast.AtExpr) {
	f.write(node.name)
}

fn (mut f Fmt) write_static_method(_name string, short_name string) {
	if short_name.contains('.') {
		indx := short_name.index_('.') + 1
		f.write(short_name[0..indx] + short_name[indx..].replace('__static__', '.').capitalize())
	} else {
		f.write(short_name.replace('__static__', '.').capitalize())
	}
}

pub fn (mut f Fmt) call_expr(node ast.CallExpr) {
	// The `json` module is deprecated in favour of the pure V `json2` module.
	// vfmt migrates its calls automatically:
	//   json.decode(T, s)  => json2.decode[T](s)
	//   json.encode(x)     => json2.encode(x)
	// Calls the migration cannot rewrite losslessly (`json.encode_pretty`, or a
	// `json.decode` with comments on its type arg) leave the whole file unmigrated,
	// so every `json.*` call and the `import json` stay as-is (see
	// process_file_imports and file_has_unmigratable_json_call).
	if node.kind in [.json_decode, .json_encode] && f.migrate_json_calls {
		f.json2_migrate_call(node)
		return
	}
	mut is_method_newline := false
	if node.is_method {
		if ast.builtin_array_generic_methods_no_sort_matcher.matches(node.name) {
			f.in_lambda_depth++
			defer(fn) { f.in_lambda_depth-- }
		}
		f.expr(node.left)
		is_method_newline = node.left.pos().last_line != node.name_pos.line_nr
		if is_method_newline {
			f.indent++
			f.writeln('')
		}
		f.write('.' + node.name)
	} else {
		f.write_language_prefix(node.language)
		if node.left is ast.AnonFn {
			f.anon_fn(node.left)
		} else if node.language != .v {
			f.write('${node.name.after_char(`.`)}')
		} else {
			name := f.short_module(node.name)
			if node.is_static_method {
				f.write_static_method(node.name, name)
			} else if node.is_paren_wrapped_call {
				f.write('(${name})')
			} else {
				f.write(name)
			}
		}
	}
	if node.mod == '' && node.name == '' {
		if node.left is ast.CallExpr {
			f.expr(node.left)
		} else {
			f.write(node.left.str())
		}
	}
	f.write_generic_call_if_require(node)
	f.write('(')
	f.call_args(node.args)
	f.write(')')
	f.or_expr(node.or_block)
	f.comments(node.comments, has_nl: false)
	if is_method_newline {
		f.indent--
	}
}

// json2_migrate_call rewrites a call to the deprecated `json` module into the
// equivalent call to the pure V `json2` module (see call_expr for the mapping).
// The `json2` prefix follows f.json2_prefix, so an existing `import json2 as x`
// is respected and the output stays in scope and idempotent.
fn (mut f Fmt) json2_migrate_call(node ast.CallExpr) {
	j2 := f.json2_prefix
	match node.kind {
		.json_decode {
			// json.decode(T, s) => json2.decode[T](s)
			// The first argument is the target type; the rest are forwarded.
			// Guard against malformed calls, since vfmt runs on unchecked ASTs.
			// A call whose type argument carries comments is never migrated (the file
			// is left unmigrated, see file_has_unmigratable_json_call), because a
			// comment inside the generic bracket cannot be re-parsed by vfmt.
			if node.args.len >= 1 {
				f.write('${j2}.decode[')
				f.expr(node.args[0].expr)
				f.write('](')
				f.call_args(node.args[1..])
				f.write(')')
			} else {
				f.write('${j2}.decode()')
			}
		}
		.json_encode {
			// json.encode(x) => json2.encode(x, escape_unicode: true)
			// Legacy json.encode escapes non-ASCII as `\uXXXX`, while json2 defaults
			// `escape_unicode` to false (raw unicode); pass it explicitly so the
			// migrated call produces the exact same bytes for Unicode fields.
			f.write('${j2}.encode(')
			f.call_args(node.args)
			if node.args.len > 0 {
				f.write(', escape_unicode: true')
			}
			f.write(')')
		}
		else {}
	}

	f.or_expr(node.or_block)
	f.comments(node.comments, has_nl: false)
}

fn (mut f Fmt) write_generic_call_if_require(node ast.CallExpr) {
	if node.concrete_types.len > 0 {
		f.write('[')
		for i, concrete_type in node.concrete_types {
			tsym := f.table.sym(concrete_type)
			if !f.write_anon_struct_type(concrete_type) {
				mut name := f.type_to_str_using_aliases(concrete_type, f.mod2alias)
				if tsym.language != .js && !tsym.name.starts_with('JS.') {
					name = f.short_module(name)
				} else if tsym.language == .js && !tsym.name.starts_with('JS.') {
					name = 'JS.' + name
				}
				if tsym.language == .c {
					name = 'C.' + name
				}
				f.write(name)
			}
			if i != node.concrete_types.len - 1 {
				f.write(', ')
			}
		}
		f.write(']')
	}
}

pub fn (mut f Fmt) call_args(args []ast.CallArg) {
	old_single_line_fields_state := f.single_line_fields
	old_short_arg_state := f.use_short_fn_args
	f.single_line_fields = true
	f.use_short_fn_args = false
	defer {
		f.single_line_fields = old_single_line_fields_state
		f.use_short_fn_args = old_short_arg_state
	}
	for i, arg in args {
		pre_comments := arg.comments.filter(it.pos.pos < arg.expr.pos().pos)
		post_comments := arg.comments[pre_comments.len..]
		if pre_comments.len > 0 {
			f.comments(pre_comments)
			f.write(' ')
		}
		if i == args.len - 1 && arg.expr is ast.StructInit {
			if arg.expr.typ == ast.void_type {
				f.use_short_fn_args = true
			}
		}
		if arg.is_mut {
			f.write(arg.share.str() + ' ')
		}
		if i > 0 && !f.single_line_if && !f.use_short_fn_args && arg.expr !is ast.StructInit {
			arg_str := f.node_str(arg.expr)
			tail_len := if i < args.len - 1 { 2 } else { 1 }
			is_tiny_last_assign_arg := f.is_assign && i == args.len - 1 && arg_str.len <= 4
			if !is_tiny_last_assign_arg && !arg_str.contains('\n')
				&& f.line_len + arg_str.len + tail_len > max_len {
				f.wrap_long_line(0, true)
			}
		}
		f.expr(arg.expr)
		if post_comments.len > 0 {
			f.comments(post_comments)
			f.write(' ')
		}
		if i < args.len - 1 {
			f.write(', ')
		}
	}
}

pub fn (mut f Fmt) cast_expr(node ast.CastExpr) {
	typ := f.type_to_str_using_aliases(node.typ, f.mod2alias)
	if typ == 'voidptr' {
		// `voidptr(0)` => `nil`
		if node.expr is ast.IntegerLiteral {
			if node.expr.val == '0' {
				if f.inside_unsafe {
					f.write('nil')
				} else {
					f.write('unsafe { nil }')
				}
				return
			}
		}
	}
	f.write('${typ}(')
	f.expr(node.expr)
	if node.has_arg {
		f.write(', ')
		f.expr(node.arg)
	}
	f.write(')')
}

pub fn (mut f Fmt) chan_init(mut node ast.ChanInit) {
	info := f.table.sym(node.typ).chan_info()
	if node.elem_type == 0 && node.typ > 0 {
		node.elem_type = info.elem_type
	}
	is_mut := info.is_mut
	el_typ := if is_mut {
		node.elem_type.set_nr_muls(node.elem_type.nr_muls() - 1)
	} else {
		node.elem_type
	}
	f.write('chan ')
	if is_mut {
		f.write('mut ')
	}
	f.write(f.type_to_str_using_aliases(el_typ, f.mod2alias))
	f.write('{')
	if node.has_cap {
		f.write('cap: ')
		f.expr(node.cap_expr)
	}
	f.write('}')
}

pub fn (mut f Fmt) comptime_call(node ast.ComptimeCall) {
	if node.is_template {
		if node.kind == .html {
			if node.args.len == 1 && node.args[0].expr is ast.StringLiteral {
				f.write('\$veb.html(')
				f.expr(node.args[0].expr)
				f.write(')')
			} else {
				f.write('\$veb.html()')
			}
		} else {
			f.write('\$tmpl(')
			f.expr(node.args[0].expr)
			f.write(')')
		}
	} else {
		match true {
			node.kind == .embed_file {
				f.write('\$embed_file(')
				f.expr(node.args[0].expr)
				if node.embed_file.compression_type != 'none' {
					f.write(', .${node.embed_file.compression_type}')
				}
				f.write(')')
			}
			node.kind == .env {
				f.write("\$env('${node.args_var}')")
			}
			node.kind == .pkgconfig {
				f.write("\$pkgconfig('${node.args_var}')")
			}
			node.kind in [.compile_error, .compile_warn] {
				if node.args.len == 0 {
					if node.args_var.contains("'") {
						f.write('\$${node.method_name}("${node.args_var}")')
					} else {
						f.write("\$${node.method_name}('${node.args_var}')")
					}
				} else {
					f.write('\$${node.method_name}(')
					f.expr(node.args[0].expr)
					f.write(')')
				}
			}
			node.kind == .d {
				f.write("\$d('${node.args_var}', ")
				f.expr(node.args[0].expr)
				f.write(')')
			}
			node.kind == .res {
				if node.args_var != '' {
					f.write('\$res(${node.args_var})')
				} else {
					f.write('\$res()')
				}
			}
			node.kind in [.zero, .new] {
				f.write('\$${node.method_name}(')
				f.expr(node.args[0].expr)
				f.write(')')
			}
			else {
				inner_args := if node.args_var != '' {
					node.args_var
				} else {
					node.args.map(call_arg_spread_str).join(', ')
				}
				method_expr := if node.has_parens {
					'(${node.method_name}(${inner_args}))'
				} else {
					'${node.method_name}(${inner_args})'
				}
				f.expr(node.left)
				f.write('.$${method_expr}')
				f.or_expr(node.or_block)
			}
		}
	}
}

pub fn (mut f Fmt) comptime_selector(node ast.ComptimeSelector) {
	f.expr(node.left)
	f.write('.\$(${node.field_expr})')
}

pub fn (mut f Fmt) concat_expr(node ast.ConcatExpr) {
	for i, val in node.vals {
		if i != 0 {
			f.write(', ')
		}
		f.expr(val)
	}
}

pub fn (mut f Fmt) dump_expr(node ast.DumpExpr) {
	f.write('dump(')
	f.expr(node.expr)
	f.write(')')
}

pub fn (mut f Fmt) enum_val(node ast.EnumVal) {
	name := f.short_module(node.enum_name)
	f.write(name + '.' + node.val)
}

pub fn (mut f Fmt) ident(node ast.Ident) {
	if node.info is ast.IdentVar {
		if node.comptime && node.name in ast.valid_comptime_not_user_defined {
			f.write(node.name)
			return
		}
		if node.info.is_mut {
			f.write(node.info.share.str() + ' ')
		}
		var_info := node.var_info()
		if var_info.is_static {
			f.write('static ')
		}
		if var_info.is_volatile {
			f.write('volatile ')
		}
	}
	f.write_language_prefix(node.language)
	if node.kind == .blank_ident {
		f.write('_')
	} else {
		mut is_local := false
		if f.fn_scope != unsafe { nil } {
			if _ := f.fn_scope.find_var(node.name) {
				is_local = true
			}
		}
		if !is_local && !node.name.contains('.') && !f.inside_const {
			if _ := f.file.global_scope.find_const('${f.cur_mod}.${node.name}') {
				const_name := node.name.all_after_last('.')
				f.write(const_name)
				if node.or_expr.kind == .block {
					f.or_expr(node.or_expr)
				}
				return
			}
		}
		name := f.short_module(node.name)
		if node.name.contains('__static__') {
			f.write_static_method(node.name, name)
		} else if f.is_array_init && name == 'it' {
			f.write('index')
		} else {
			f.write(name)
		}
		if node.concrete_types.len > 0 {
			f.write('[')
			for i, concrete_type in node.concrete_types {
				if !f.write_anon_struct_type(concrete_type) {
					typ_name := f.type_to_str_using_aliases(concrete_type, f.mod2alias)
					f.write(typ_name)
				}
				if i != node.concrete_types.len - 1 {
					f.write(', ')
				}
			}
			f.write(']')
		}
		if node.or_expr.kind == .propagate_option {
			f.write('?')
		} else if node.or_expr.kind == .block {
			f.or_expr(node.or_expr)
		}
	}
}

pub fn (mut f Fmt) if_expr(node ast.IfExpr) {
	dollar := if node.is_comptime { '$' } else { '' }
	f.inside_comptime_if = node.is_comptime
	mut keep_single_line := node.branches.len == 1 && branch_is_single_line(node.branches[0])
	is_ternary := node.branches.len == 2 && node.has_else && branch_is_single_line(node.branches[0])
		&& branch_is_single_line(node.branches[1]) && (node.is_expr || f.is_assign
		|| f.inside_const || f.is_struct_init || f.single_line_fields)
	keep_single_line = keep_single_line || is_ternary
	f.single_line_if = keep_single_line
	start_pos := f.out.len
	start_len := f.line_len
	for {
		for i, branch in node.branches {
			f.branch_processed_imports.clear()
			mut sum_len := 0
			if i > 0 {
				// `else`, close previous branch
				if branch.comments.len > 0 {
					f.writeln('}')
					pre_comments := branch.comments.filter(it.pos.pos < branch.pos.pos)
					sum_len += pre_comments.len
					if pre_comments.len > 0 {
						f.comments(pre_comments)
					}
				} else {
					f.write('} ')
				}
				f.write('${dollar}else ')
			}
			if i < node.branches.len - 1 || !node.has_else {
				f.write('${dollar}if ')
				cur_pos := f.out.len
				pre_comments :=
					branch.comments[sum_len..].filter(it.pos.pos < branch.cond.pos().pos)
				sum_len += pre_comments.len
				post_comments := branch.comments[sum_len..]
				if pre_comments.len > 0 {
					f.comments(pre_comments)
					f.write(' ')
				}
				f.expr(branch.cond)
				if post_comments.len > 0 {
					f.comments(post_comments)
					f.write(' ')
				}
				cond_len := f.out.len - cur_pos
				is_cond_wrapped := cond_len > 0 && branch.cond in [ast.IfGuardExpr, ast.CallExpr]
					&& f.out.last_n(cond_len).contains('\n')
				if is_cond_wrapped {
					f.writeln('')
				} else {
					f.write(' ')
				}
			}
			f.write('{')
			if keep_single_line {
				f.write(' ')
			} else {
				f.writeln('')
			}
			f.stmts(branch.stmts)
			if keep_single_line {
				f.write(' ')
			}
		}
		if keep_single_line && f.line_len > max_len && !f.buffering {
			keep_single_line = false
			f.single_line_if = false
			f.out.go_back_to(start_pos)
			f.line_len = start_len
			f.empty_line = start_len == 0
			continue
		}
		break
	}
	f.write('}')
	f.single_line_if = false
	f.inside_comptime_if = false
	if node.post_comments.len > 0 {
		if keep_single_line {
			f.comments(node.post_comments,
				has_nl:    false
				same_line: true
				prev_line: node.branches.last().body_pos.last_line
			)
		} else {
			f.writeln('')
			f.comments(node.post_comments,
				has_nl:    false
				prev_line: node.branches.last().body_pos.last_line
			)
		}
	}
}

fn branch_is_single_line(b ast.IfBranch) bool {
	if b.stmts.len == 1 && b.comments.len == 0 && stmt_is_single_line(b.stmts[0])
		&& b.pos.line_nr == b.stmts[0].pos.line_nr {
		return true
	}
	return false
}

fn sql_query_data_item_is_single_line(item ast.SqlQueryDataItem) bool {
	return match item {
		ast.SqlQueryDataLeaf {
			item.pre_comments.len == 0 && item.end_comments.len == 0
				&& item.pos.line_nr == item.pos.last_line && expr_is_single_line(item.expr)
		}
		ast.SqlQueryDataIf {
			false
		}
	}
}

fn sql_query_data_branch_is_single_line(branch ast.SqlQueryDataBranch) bool {
	return branch.end_comments.len == 0 && branch.pos.line_nr == branch.pos.last_line
		&& branch.items.len == 1 && sql_query_data_item_is_single_line(branch.items[0])
}

fn sql_query_data_item_pre_comments(item ast.SqlQueryDataItem) []ast.Comment {
	return match item {
		ast.SqlQueryDataLeaf { item.pre_comments }
		ast.SqlQueryDataIf { item.pre_comments }
	}
}

fn sql_query_data_item_end_comments(item ast.SqlQueryDataItem) []ast.Comment {
	return match item {
		ast.SqlQueryDataLeaf { item.end_comments }
		ast.SqlQueryDataIf { item.end_comments }
	}
}

fn sql_query_data_item_last_line(item ast.SqlQueryDataItem) int {
	return match item {
		ast.SqlQueryDataLeaf { item.pos.last_line }
		ast.SqlQueryDataIf { item.pos.last_line }
	}
}

pub fn (mut f Fmt) if_guard_expr(node ast.IfGuardExpr) {
	for i, var in node.vars {
		if var.is_mut {
			f.write('mut ')
		}
		f.write(var.name)
		if i != node.vars.len - 1 {
			f.write(', ')
		}
	}
	f.write(' := ')
	f.expr(node.expr)
}

pub fn (mut f Fmt) index_expr(node ast.IndexExpr) {
	f.expr(node.left)
	if node.is_gated {
		f.write('#')
	}
	last_index_expr_state := f.is_index_expr
	f.is_index_expr = true
	f.write('[')
	parts := if node.indices.len > 0 { node.indices } else { [node.index] }
	for i, part in parts {
		if i > 0 {
			f.write(', ')
		}
		f.expr(part)
	}
	f.write(']')
	f.is_index_expr = last_index_expr_state
	if node.or_expr.kind != .absent {
		f.or_expr(node.or_expr)
	}
}

pub fn (mut f Fmt) infix_expr(node ast.InfixExpr) {
	buffering_save := f.buffering
	is_wrappable_additive_minus := node.op == .minus
		&& (is_additive_infix(node.left) || is_additive_infix(node.right))
	if !f.buffering && (node.op in [.logical_or, .and, .plus] || is_wrappable_additive_minus) {
		f.buffering = true
	}
	is_assign_save := f.is_assign
	if node.op == .left_shift {
		f.is_assign = true // To write ternary if on a single line
	}
	start_pos := f.out.len
	start_len := f.line_len
	mut redundant_par := false
	if node.left is ast.ParExpr && node.op in [.and, .logical_or] {
		if node.left.expr is ast.InfixExpr {
			if node.left.expr.op !in [.and, .logical_or] {
				redundant_par = true
				f.expr(node.left.expr)
			}
		}
	}
	if !redundant_par {
		f.expr(node.left)
	}
	if node.before_op_comments.len > 0 {
		f.comments(node.before_op_comments)
	}
	is_one_val_array_init := node.op in [.key_in, .not_in] && node.right is ast.ArrayInit
		&& node.right.exprs.len == 1
	is_and := node.op == .amp && f.node_str(node.right).starts_with('&')
	if is_one_val_array_init && !f.inside_comptime_if {
		// `var in [val]` => `var == val`
		op := if node.op == .key_in { ' == ' } else { ' != ' }
		f.write(op)
	} else if is_and {
		f.write(' && ')
	} else {
		f.write(' ${node.op.str()} ')
	}
	if node.after_op_comments.len > 0 {
		f.comments(node.after_op_comments)
		f.write(' ')
	}
	if is_one_val_array_init && !f.inside_comptime_if {
		// `var in [val]` => `var == val`
		f.expr((node.right as ast.ArrayInit).exprs[0])
	} else if is_and {
		f.write(f.node_str(node.right).trim_string_left('&'))
	} else {
		redundant_par = false
		if node.right is ast.ParExpr && node.op in [.and, .logical_or] {
			if node.right.expr is ast.InfixExpr {
				if node.right.expr.op !in [.and, .logical_or] {
					redundant_par = true
					f.expr(node.right.expr)
				}
			}
		}
		if !redundant_par {
			f.expr(node.right)
		}
	}
	if !buffering_save && f.buffering {
		f.buffering = false
		if !f.single_line_if && f.line_len > max_len {
			is_cond := node.op in [.and, .logical_or]
			f.wrap_infix(start_pos, start_len, is_cond)
		}
	}
	f.is_assign = is_assign_save
	f.or_expr(node.or_block)
}

pub fn (mut f Fmt) wrap_infix(start_pos int, start_len int, is_cond bool) {
	cut_span := f.out.len - start_pos
	infix_str := f.out.cut_last(cut_span)
	if !infix_str.contains_any_substr(['&&', '||', '+', '-']) {
		f.write(infix_str)
		return
	}
	f.line_len = start_len
	if start_len == 0 {
		f.empty_line = true
	}
	conditions, penalties := split_up_infix(infix_str, false, is_cond)
	f.write_splitted_infix(conditions, penalties, false, is_cond)
}

fn is_additive_infix(expr ast.Expr) bool {
	return match expr {
		ast.InfixExpr { expr.op in [.plus, .minus] }
		else { false }
	}
}

fn split_up_infix(infix_str string, ignore_paren bool, is_cond_infix bool) ([]string, []int) {
	mut conditions := ['']
	mut penalties := [5]
	or_pen := if infix_str.contains('&&') { 3 } else { 5 }
	parts := infix_str.split(' ')
	mut inside_paren := false
	mut ind := 0
	for p in parts {
		if is_cond_infix && p in ['&&', '||'] {
			if inside_paren {
				conditions[ind] += '${p} '
			} else {
				pen := if p == '||' { or_pen } else { 5 }
				penalties << pen
				conditions << '${p} '
				ind++
			}
		} else if !is_cond_infix && p in ['+', '-'] {
			if inside_paren {
				conditions[ind] += '${p} '
			} else {
				penalties << 5
				conditions[ind] += '${p} '
				conditions << ''
				ind++
			}
		} else {
			conditions[ind] += '${p} '
			if ignore_paren {
				continue
			}
			if p.starts_with('(') {
				inside_paren = true
			} else if p.ends_with(')') {
				inside_paren = false
			}
		}
	}
	return conditions, penalties
}

const wsinfix_depth_max = 10

fn (mut f Fmt) write_splitted_infix(conditions []string, penalties []int, ignore_paren bool, is_cond bool) {
	f.wsinfix_depth++
	defer { f.wsinfix_depth-- }
	for i, cnd in conditions {
		c := cnd.trim_space()
		if f.line_len + c.len < break_points[penalties[i]] {
			if (i > 0 && i < conditions.len) || (ignore_paren && i == 0 && c.len > 5 && c[3] == `(`) {
				f.write(' ')
			}
			f.write(c)
		} else {
			is_paren_expr := (c[0] == `(` || (c.len > 5 && c[3] == `(`)) && c.ends_with(')')
			final_len := ((f.indent + 1) * 4) + c.len
			if f.wsinfix_depth > wsinfix_depth_max {
				// limit indefinite recursion, by just giving up splitting:
				f.write(c)
				continue
			}
			if final_len > max_len && is_paren_expr {
				conds, pens := split_up_infix(c, true, is_cond)
				f.write_splitted_infix(conds, pens, true, is_cond)
				continue
			}
			mut is_wrap_needed := true
			if i == 0 {
				last_line_str := f.remove_new_line().trim_space()
				if last_line_str in ['if', 'for'] {
					f.write(' ')
					is_wrap_needed = false
				}
			}
			if is_wrap_needed {
				f.writeln('')
			}
			f.indent++
			f.write(c)
			f.indent--
		}
	}
}

pub fn (mut f Fmt) likely(node ast.Likely) {
	if node.is_likely {
		f.write('_likely_')
	} else {
		f.write('_unlikely_')
	}
	f.write('(')
	f.expr(node.expr)
	f.write(')')
}

pub fn (mut f Fmt) lock_expr(node ast.LockExpr) {
	mut num_locked := 0
	mut num_rlocked := 0
	for is_rlock in node.is_rlock {
		if is_rlock {
			num_rlocked++
		} else {
			num_locked++
		}
	}
	if num_locked > 0 || num_rlocked == 0 {
		f.write('lock')
		if num_locked > 0 {
			f.write(' ')
		}
		mut n := 0
		for i, v in node.lockeds {
			if !node.is_rlock[i] {
				if n > 0 {
					f.write(', ')
				}
				f.expr(v)
				n++
			}
		}
	}
	if num_rlocked > 0 {
		if num_locked > 0 {
			f.write('; ')
		}
		f.write('rlock ')
		mut n := 0
		for i, v in node.lockeds {
			if node.is_rlock[i] {
				if n > 0 {
					f.write(', ')
				}
				f.expr(v)
				n++
			}
		}
	}
	f.writeln(' {')
	f.stmts(node.stmts)
	f.write('}')
}

pub fn (mut f Fmt) map_init(node ast.MapInit) {
	if node.keys.len == 0 && !node.has_update_expr {
		if node.typ > ast.void_type {
			f.write(f.type_to_str_using_aliases(node.typ, f.mod2alias))
		}
		if node.pos.line_nr == node.pos.last_line {
			f.write('{}')
		} else {
			f.writeln('{')
			f.comments(node.pre_cmnts, level: .indent)
			f.write('}')
		}
		return
	}
	f.writeln('{')
	f.indent++
	f.comments(node.pre_cmnts)
	if node.has_update_expr {
		f.write('...')
		f.expr(node.update_expr)
		f.comments(node.update_expr_comments,
			prev_line: node.update_expr_pos.last_line
			has_nl:    false
		)
		f.writeln('')
	}
	mut max_field_len := 0
	mut skeys := []string{}
	for key in node.keys {
		skey := f.node_str(key).trim_space()
		skeys << skey
		skey_len := utf8_str_visible_length(skey)
		if skey_len > max_field_len {
			max_field_len = skey_len
		}
	}
	for i, _ in node.keys {
		skey := skeys[i]
		f.write(skey)
		f.write(': ')
		skey_len := utf8_str_visible_length(skey)
		f.write(' '.repeat(max_field_len - skey_len))
		f.expr(node.vals[i])
		f.comments(node.comments[i], prev_line: node.vals[i].pos().last_line, has_nl: false)
		f.writeln('')
	}
	f.indent--
	f.write('}')
}

fn (mut f Fmt) match_branch(branch ast.MatchBranch, single_line bool, is_comptime bool) {
	if !branch.is_else {
		// normal branch
		f.is_mbranch_expr = true
		for j, expr in branch.exprs {
			estr := f.node_str(expr).trim_space()
			if f.line_len + estr.len + 2 > max_len {
				f.remove_new_line()
				f.writeln('')
			}
			f.write(estr)
			if j < branch.exprs.len - 1 {
				f.write(', ')
			}
			if j < branch.ecmnts.len && branch.ecmnts[j].len > 0 {
				f.write(' ')
				f.comments(branch.ecmnts[j])
			}
		}
		f.is_mbranch_expr = false
	} else {
		// else branch
		if is_comptime {
			f.write('\$else')
		} else {
			f.write('else')
		}
	}
	if branch.stmts.len == 0 {
		f.writeln(' {}')
	} else {
		if single_line {
			f.write(' { ')
		} else if branch.ecmnts.len > 0 && branch.ecmnts.last().len > 0 {
			f.writeln('{')
		} else {
			f.writeln(' {')
		}
		f.stmts(branch.stmts)
		if single_line {
			f.remove_new_line()
			f.writeln(' }')
		} else {
			f.writeln('}')
		}
	}
	f.comments(branch.post_comments, same_line: true)
}

pub fn (mut f Fmt) match_expr(node ast.MatchExpr) {
	dollar := if node.is_comptime { '$' } else { '' }
	cond, cond_or_expr := match_cond_with_trailing_or_expr(node.cond)
	f.write('${dollar}match ')
	f.expr(cond)
	f.writeln(' {')
	f.indent++
	f.comments(node.comments)
	mut single_line := true
	for branch in node.branches {
		if branch.stmts.len > 1 || branch.pos.line_nr < branch.pos.last_line {
			single_line = false
			break
		}
		if branch.stmts.len == 0 {
			continue
		}
		if !stmt_is_single_line(branch.stmts[0]) {
			single_line = false
			break
		}
	}
	mut else_idx := -1
	for i, branch in node.branches {
		if branch.is_else {
			else_idx = i
			continue
		}
		f.match_branch(branch, single_line, node.is_comptime)
	}
	if else_idx >= 0 {
		f.match_branch(node.branches[else_idx], single_line, node.is_comptime)
	}
	f.indent--
	f.write('}')
	f.or_expr(cond_or_expr)
}

fn match_cond_with_trailing_or_expr(expr ast.Expr) (ast.Expr, ast.OrExpr) {
	match expr {
		ast.CallExpr {
			if expr.or_block.kind == .block {
				mut cond := expr
				or_expr := cond.or_block
				cond.or_block = ast.OrExpr{}
				return ast.Expr(cond), or_expr
			}
		}
		ast.Ident {
			if expr.or_expr.kind == .block {
				mut cond := expr
				or_expr := cond.or_expr
				cond.or_expr = ast.OrExpr{}
				return ast.Expr(cond), or_expr
			}
		}
		ast.IndexExpr {
			if expr.or_expr.kind == .block {
				mut cond := expr
				or_expr := cond.or_expr
				cond.or_expr = ast.OrExpr{}
				return ast.Expr(cond), or_expr
			}
		}
		ast.ParExpr {
			cond, or_expr := match_cond_with_trailing_or_expr(expr.expr)
			if or_expr.kind == .block {
				mut par_expr := expr
				par_expr.expr = cond
				return ast.Expr(par_expr), or_expr
			}
		}
		ast.PrefixExpr {
			if expr.op == .arrow && expr.or_block.kind == .block {
				mut cond := expr
				or_expr := cond.or_block
				cond.or_block = ast.OrExpr{}
				return ast.Expr(cond), or_expr
			}
		}
		ast.SelectorExpr {
			if expr.or_block.kind == .block {
				mut cond := expr
				or_expr := cond.or_block
				cond.or_block = ast.OrExpr{}
				return ast.Expr(cond), or_expr
			}
		}
		else {}
	}

	return expr, ast.OrExpr{}
}

pub fn (mut f Fmt) offset_of(node ast.OffsetOf) {
	f.write('__offsetof(${f.type_to_str_using_aliases(node.struct_type, f.mod2alias)}, ${node.field})')
}

pub fn (mut f Fmt) or_expr(node ast.OrExpr) {
	match node.kind {
		.absent {}
		.block {
			if node.stmts.len == 0 {
				f.write(' or {')
				if node.pos.line_nr != node.pos.last_line {
					f.writeln('')
				}
				f.write('}')
				return
			} else if expr_is_single_line(node) {
				// the control stmts (return/break/continue...) print a newline inside them,
				// so, since this'll all be on one line, trim any possible whitespace
				str := f.node_str(node.stmts[0]).trim_space()
				single_line := ' or { ${str} }'
				if single_line.len + f.line_len <= max_len {
					f.write(single_line)
					return
				}
			}
			// Make it multiline if the blocks has at least two stmts
			// or a single line would be too long
			f.writeln(' or {')
			f.stmts(node.stmts)
			f.write('}')
		}
		.propagate_option {
			f.write('?')
		}
		.propagate_result {
			f.write('!')
		}
	}
}

pub fn (mut f Fmt) par_expr(node ast.ParExpr) {
	mut expr := node.expr
	expr = expr.remove_par()
	requires_paren := expr !is ast.Ident || node.comments.len > 0
	if requires_paren {
		f.par_level++
		f.write('(')
	}
	pre_comments := node.comments.filter(it.pos.pos < expr.pos().pos)
	post_comments := node.comments[pre_comments.len..]
	if pre_comments.len > 0 {
		f.comments(pre_comments)
		f.write(' ')
	}
	f.expr(expr)
	if post_comments.len > 0 {
		f.comments(post_comments)
		f.write(' ')
	}
	if requires_paren {
		f.par_level--
		f.write(')')
	}
}

pub fn (mut f Fmt) postfix_expr(node ast.PostfixExpr) {
	f.expr(node.expr)
	// `$if foo ?`
	if node.op == .question {
		f.write(' ?')
	} else {
		f.write('${node.op}')
	}
	if node.is_c2v_prefix {
		f.write('$')
	}
}

pub fn (mut f Fmt) prefix_expr(node ast.PrefixExpr) {
	// !(a in b) => a !in b, !(a is b) => a !is b
	if node.op == .not && node.right is ast.ParExpr {
		if node.right.expr is ast.InfixExpr {
			if node.right.expr.op in [.key_in, .not_in, .key_is, .not_is]
				&& node.right.expr.right !is ast.InfixExpr {
				f.expr(node.right.expr.left)
				match node.right.expr.op {
					.key_in { f.write(' !in ') }
					.not_in { f.write(' in ') }
					.key_is { f.write(' !is ') }
					.not_is { f.write(' is ') }
					else {}
				}

				f.expr(node.right.expr.right)
				return
			}
		}
	}
	f.write(node.op.str())
	f.expr(node.right)
	f.or_expr(node.or_block)
}

pub fn (mut f Fmt) range_expr(node ast.RangeExpr) {
	f.expr(node.low)
	if f.is_mbranch_expr && !f.is_index_expr {
		f.write('...')
	} else {
		f.write('..')
	}
	f.expr(node.high)
}

pub fn (mut f Fmt) select_expr(node ast.SelectExpr) {
	f.writeln('select {')
	f.indent++
	for branch in node.branches {
		if branch.comment.text != '' {
			f.comment(branch.comment, same_line: true)
			f.writeln('')
		}
		if branch.is_else {
			f.write('else {')
		} else {
			f.single_line_if = true
			match branch.stmt {
				ast.ExprStmt { f.expr(branch.stmt.expr) }
				else { f.stmt(branch.stmt) }
			}

			f.single_line_if = false
			f.write(' {')
		}
		if branch.stmts.len > 0 {
			f.writeln('')
			f.stmts(branch.stmts)
		}
		f.writeln('}')
		if branch.post_comments.len > 0 {
			f.comments(branch.post_comments, same_line: true)
		}
	}
	f.indent--
	f.write('}')
}

pub fn (mut f Fmt) selector_expr(node ast.SelectorExpr) {
	// TODO(StunxFS): Even though we ignored the JS backend, the `v/gen/js/tests/js.v`
	// file was still formatted/transformed, so it is specifically ignored here. Fix this.
	if f.file.language != .js && node.expr is ast.StringLiteral && node.field_name == 'str'
		&& !f.pref.backend.is_js()
		&& !f.file.path.ends_with(os.join_path('v', 'gen', 'js', 'tests', 'js.v')) {
		f.write('c')
		f.expr(node.expr)
		return
	}
	f.expr(node.expr)
	f.write('.')
	f.write(node.field_name)
	f.or_expr(node.or_block)
}

pub fn (mut f Fmt) size_of(node ast.SizeOf) {
	f.write('sizeof')
	if node.is_type && !node.guessed_type {
		// the new form was explicitly written in the source code; keep it:
		f.write('[')
		f.write(f.type_to_str_using_aliases(node.typ, f.mod2alias))
		f.write(']()')
		return
	}
	if node.is_type {
		f.write('(')
		f.write(f.type_to_str_using_aliases(node.typ, f.mod2alias))
		f.write(')')
	} else {
		f.write('(')
		f.expr(node.expr)
		f.write(')')
	}
}

pub fn (mut f Fmt) is_ref_type(node ast.IsRefType) {
	f.write('isreftype')
	if node.is_type && !node.guessed_type {
		// the new form was explicitly written in the source code; keep it:
		f.write('[')
		f.write(f.type_to_str_using_aliases(node.typ, f.mod2alias))
		f.write(']()')
		return
	}
	if node.is_type {
		f.write('(')
		f.write(f.type_to_str_using_aliases(node.typ, f.mod2alias))
		f.write(')')
	} else {
		f.write('(')
		f.expr(node.expr)
		f.write(')')
	}
}

pub fn (mut f Fmt) sql_expr(node ast.SqlExpr) {
	// sql app.db { select from Contributor where repo == id && user == 0 }
	f.write('sql ')
	f.expr(node.db_expr)
	f.writeln(' {')
	f.write('\t')
	if node.is_dynamic {
		f.write('dynamic ')
	}
	if node.is_insert {
		f.write('insert ')
	} else {
		f.write('select ')
	}
	if node.has_distinct {
		f.write('distinct ')
	}
	sym := f.table.sym(node.table_expr.typ)
	mut table_name := sym.name
	if !table_name.starts_with('C.') && !table_name.starts_with('JS.') {
		table_name = f.no_cur_mod(f.short_module(sym.name)) // TODO: f.type_to_str?
	}
	if node.aggregate_kind != .none {
		match node.aggregate_kind {
			.count {
				f.write('count ')
			}
			.sum, .avg, .min, .max {
				f.write('${node.aggregate_kind}(${node.aggregate_field}) ')
			}
			.none {}
		}
	} else if node.requested_fields.len > 0 {
		for i, requested_field in node.requested_fields {
			f.write(requested_field.name)
			if i < node.requested_fields.len - 1 {
				f.write(', ')
			}
		}
	} else {
		for i, fd in node.fields {
			f.write(fd.name)
			if i < node.fields.len - 1 {
				f.write(', ')
			}
		}
	}
	if node.aggregate_kind == .none && (node.requested_fields.len > 0 || node.fields.len > 0) {
		f.write(' ')
	}
	if node.is_insert {
		f.write('${node.inserted_var} into ${table_name}')
	} else {
		f.write('from ${table_name}')
	}
	// Format JOIN clauses
	for join in node.joins {
		f.writeln('')
		f.write('\t')
		match join.kind {
			.inner { f.write('join ') }
			.left { f.write('left join ') }
			.right { f.write('right join ') }
			.full_outer { f.write('full outer join ') }
		}

		join_sym := f.table.sym(join.table_expr.typ)
		mut join_table_name := join_sym.name
		if !join_table_name.starts_with('C.') && !join_table_name.starts_with('JS.') {
			join_table_name = f.no_cur_mod(f.short_module(join_sym.name))
		}
		f.write('${join_table_name} on ')
		f.expr(join.on_expr)
	}
	if node.has_where {
		f.write(' where ')
		f.expr(node.where_expr)
	}
	if node.has_order {
		f.write(' order by ')
		f.expr(node.order_expr)
		if node.has_desc {
			f.write(' desc')
		}
	}
	if node.has_limit {
		f.write(' limit ')
		f.expr(node.limit_expr)
	}
	if node.has_offset {
		f.write(' offset ')
		f.expr(node.offset_expr)
	}
	f.writeln('')
	f.write('}')
	f.or_expr(node.or_expr)
}

pub fn (mut f Fmt) sql_query_data_expr(node ast.SqlQueryDataExpr) {
	if node.items.len == 0 && node.end_comments.len == 0 {
		f.write('{}')
		return
	}
	f.writeln('{')
	f.indent++
	f.sql_query_data_items(node.items, node.end_comments)
	f.indent--
	f.write('}')
}

fn (mut f Fmt) sql_query_data_items(items []ast.SqlQueryDataItem, end_comments []ast.Comment) {
	for idx, item in items {
		f.sql_query_data_comment_lines(sql_query_data_item_pre_comments(item))
		f.sql_query_data_item(item)
		if idx < items.len - 1 || end_comments.len > 0 {
			f.write(',')
		}
		item_end_comments := sql_query_data_item_end_comments(item)
		if item_end_comments.len > 0 {
			if item_end_comments[0].pos.line_nr == sql_query_data_item_last_line(item) {
				f.comments(item_end_comments, same_line: true, has_nl: true, level: .keep)
			} else {
				f.writeln('')
				f.sql_query_data_comment_lines(item_end_comments)
			}
		} else {
			f.writeln('')
		}
	}
	f.sql_query_data_comment_lines(end_comments)
}

fn (mut f Fmt) sql_query_data_comment_lines(comments []ast.Comment) {
	for comment in comments {
		f.comment(comment)
		f.writeln('')
	}
}

fn (mut f Fmt) sql_query_data_item(item ast.SqlQueryDataItem) {
	match item {
		ast.SqlQueryDataLeaf {
			f.expr(item.expr)
		}
		ast.SqlQueryDataIf {
			for idx, branch in item.branches {
				if idx == 0 {
					f.write('if ')
					f.expr(branch.cond)
					f.write(' ')
				} else if branch.cond is ast.EmptyExpr {
					f.write('else ')
				} else {
					f.write('else if ')
					f.expr(branch.cond)
					f.write(' ')
				}
				f.sql_query_data_branch_items(branch.items, branch.end_comments,
					sql_query_data_branch_is_single_line(branch))
				if idx < item.branches.len - 1 {
					f.write(' ')
				}
			}
		}
	}
}

fn (mut f Fmt) sql_query_data_branch_items(items []ast.SqlQueryDataItem, end_comments []ast.Comment, keep_single_line bool) {
	if items.len == 0 && end_comments.len == 0 {
		f.write('{}')
		return
	}
	if keep_single_line {
		start_pos := f.out.len
		start_len := f.line_len
		f.write('{ ')
		f.sql_query_data_item(items[0])
		f.write(' }')
		if !f.out.after(start_pos).contains('\n') && f.line_len <= max_len {
			return
		}
		f.out.go_back_to(start_pos)
		f.line_len = start_len
		f.empty_line = start_len == 0
	}
	f.writeln('{')
	f.indent++
	f.sql_query_data_items(items, end_comments)
	f.indent--
	f.write('}')
}

pub fn (mut f Fmt) char_literal(node ast.CharLiteral) {
	if node.val == r"\'" {
		f.write("`'`")
		return
	}
	if node.val.len == 1 {
		clit := node.val[0]
		if clit < 32 || clit > 127 || clit == 92 || clit == 96 {
			f.write('`\\x${clit.hex()}`')
			return
		}
	}
	f.write('`${node.val}`')
}

pub fn (mut f Fmt) string_literal(node ast.StringLiteral) {
	quote := if node.val.contains("'") && !node.val.contains('"') { '"' } else { "'" }
	if node.is_raw {
		f.write('r')
	} else if node.language == ast.Language.c {
		f.write('c')
	} else if node.language == ast.Language.js {
		f.write('js')
	}
	if node.is_raw {
		f.write('${quote}${node.val}${quote}')
	} else {
		unescaped_val := node.val.replace('${bs}${bs}', '\x01').replace_each([
			"${bs}'",
			"'",
			'${bs}"',
			'"',
		])
		s := unescaped_val.replace_each(['\x01', '${bs}${bs}', quote, '${bs}${quote}'])
		f.write('${quote}${s}${quote}')
	}
}

pub fn (mut f Fmt) string_inter_literal(node ast.StringInterLiteral) {
	mut quote := "'"
	for val in node.vals {
		if val.contains('\\"') {
			quote = '"'
			break
		}
		if val.contains("\\'") {
			quote = "'"
			break
		}
		if val.contains('"') {
			quote = "'"
		}
		if val.contains("'") {
			quote = '"'
		}
	}
	// TODO: this code is very similar to ast.Expr.str()
	// serkonda7: it can not fully be replaced tho as ´f.expr()´ and `ast.Expr.str()`
	//	work too different for the various exprs that are interpolated
	f.write(quote)
	for i, val in node.vals {
		unescaped_val := val.replace('${bs}${bs}', '\x01').replace_each([
			"${bs}'",
			"'",
			'${bs}"',
			'"',
		])
		s := unescaped_val.replace_each(['\x01', '${bs}${bs}', quote, '${bs}${quote}'])
		f.write('${s}')
		if i >= node.exprs.len {
			break
		}
		f.write('$')
		fspec_str := node.get_fspec(i)

		f.write('{')
		f.expr(node.exprs[i])
		f.write(fspec_str)
		f.write('}')
	}
	f.write(quote)
}

pub fn (mut f Fmt) type_expr(node ast.TypeNode) {
	if node.stmt == ast.empty_stmt {
		f.write(f.type_to_str_using_aliases(node.typ, f.mod2alias))
	} else {
		f.struct_decl(ast.StructDecl{ fields: (node.stmt as ast.StructDecl).fields }, true)
	}
}

pub fn (mut f Fmt) type_of(node ast.TypeOf) {
	f.write('typeof')
	if node.is_type {
		f.write('[')
		f.write(f.type_to_str_using_aliases(node.typ, f.mod2alias))
		f.write(']()')
	} else {
		f.write('(')
		f.expr(node.expr)
		f.write(')')
	}
}

pub fn (mut f Fmt) unsafe_expr(node ast.UnsafeExpr) {
	single_line := node.pos.line_nr >= node.pos.last_line
	f.write('unsafe {')
	if single_line {
		f.write(' ')
	} else {
		f.writeln('')
		f.indent++
		f.empty_line = true
	}
	f.expr(node.expr)
	if single_line {
		f.write(' ')
	} else {
		f.writeln('')
		f.indent--
	}
	f.write('}')
}

fn (mut f Fmt) trace[T](fbase string, x &T) {
	if f.file.path_base == fbase {
		println('> f.trace | ${fbase:-10s} | ${voidptr(x):16} | ${x}')
	}
}
