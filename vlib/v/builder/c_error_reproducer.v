module builder

import os
import v.ast

// Building a self-contained reproducer for a C error: starting from the failing V declaration
// (the one the C error's `#line` maps back to), inline the transitive closure of the declarations
// it references (types, consts, globals, functions), together with the imports and C-interop `#`
// directives they need, into a single `module main` file.
//
// To stay correct and to keep the amount of uploaded source down, this only ever inlines
// declarations from the *single module* that the failing file belongs to, and only when that
// module contains `fn main` (i.e. an ordinary single-module program). Anything that would need
// declarations from another module — a failure inside an imported local module, a cross-module
// call chain — makes it give up and return '', so the caller falls back to a plain source window
// rather than emitting a broken multi-module flatten. The reference graph is approximated by
// matching identifier tokens (ignoring string and comment contents) against the module's top-level
// names, and the assembled output is bounded as whole declarations.

const c_error_repro_max_decls = 80

// ReproDecl is one top-level declaration, ready to be inlined into a reproducer.
struct ReproDecl {
	names   []string // the (short, unqualified) top-level names it defines
	source  string   // its exact source text, including any leading attributes
	file_id int      // which contributing file it came from (imports are file-scoped)
}

// ReproImport is an `import` statement, reconstructed from the AST so selective symbols
// (`import x { a }`), aliases (`import x as y`) and side-effect imports (`import x as _`) survive
// (including multi-line ones) as a single valid line.
struct ReproImport {
	source      string   // the reconstructed import statement
	triggers    []string // the names that make this import needed (alias + selected symbols)
	side_effect bool     // `import x as _`: always keep, it is imported for its side effects
	is_local    bool     // the imported module is another parsed user module (not inlinable here)
	file_id     int      // the file this import belongs to (imports are file-scoped)
}

// ReproHash is a top-level `#include` / `#flag` / ... C-interop directive and the file it is in.
struct ReproHash {
	source  string
	file_id int
}

// v_source_reproducer builds a self-contained reproducer for the failing V line, or '' when it
// cannot (no mapping, the failure is not in an ordinary single-module `main` program, or the
// reproducer would not fit the byte budget).
fn (v &Builder) v_source_reproducer(v_file string, v_line int, max_bytes int) string {
	if v_file == '' || v_line <= 0 || v.parsed_files.len == 0 || !is_user_repro_file(v_file) {
		return ''
	}
	target_path := os.real_path(v_file)
	// find the failing file and its module; only that module's declarations are inlined
	mut root_mod := ''
	for pf in v.parsed_files {
		if os.real_path(pf.path) == target_path {
			root_mod = pf.mod.name
			break
		}
	}
	if root_mod == '' {
		return ''
	}
	// modules backed by parsed user files: importing one of these (other than `root_mod`) means
	// the reproducer would depend on source we do not inline, so it cannot be standalone.
	mut local_mods := map[string]bool{}
	for pf in v.parsed_files {
		if is_user_repro_file(pf.path) {
			local_mods[pf.mod.name] = true
		}
	}
	mut decls := []ReproDecl{}
	mut name_to_decl := map[string][]int{}
	mut imports := []ReproImport{}
	mut hashes := []ReproHash{}
	mut root_id := -1
	mut main_id := -1
	mut extra_seeds := []int{} // module-lifecycle roots (`init`/`cleanup`) that must stay reachable
	mut mod_header := 'module main'
	mut file_id := -1
	for pf in v.parsed_files {
		file := pf
		if file.mod.name != root_mod || !is_user_repro_file(file.path) {
			continue
		}
		file_id++
		is_root_file := os.real_path(file.path) == target_path
		src := os.read_file(file.path) or { continue }
		lines := src.split_into_lines()
		if lines.len == 0 {
			continue
		}
		// keep the failing file's `module` declaration verbatim, including any leading attributes
		// (`@[has_globals]`, `@[manualfree]`, ...), which affect whether the source even checks
		if is_root_file {
			for li, ln in lines {
				if ln.trim_space().starts_with('module ') {
					mod_header = lines[repro_attr_start(lines, li)..li + 1].join('\n')
					break
				}
			}
		}
		for imp in file.imports {
			mut triggers := [repro_import_alias(imp)]
			for s in imp.syms {
				triggers << s.name
			}
			imports << ReproImport{
				source:      repro_import_stmt(imp)
				triggers:    triggers
				side_effect: imp.alias == '_'
				is_local:    imp.mod in local_mods && imp.mod != root_mod
				file_id:     file_id
			}
		}
		// attribute-aware start line (0-based) of every top-level statement partitions the file;
		// top-level attributes are parsed before the declaration, so extend each start up over them
		mut starts := []int{}
		for stmt in file.stmts {
			starts << repro_attr_start(lines, ast.Node(stmt).pos().line_nr)
		}
		for i, stmt in file.stmts {
			start := starts[i]
			if start < 0 || start >= lines.len {
				continue
			}
			mut end := lines.len
			for s in starts {
				if s > start && s < end {
					end = s
				}
			}
			source := repro_decl_source(lines, start, end)
			if source == '' {
				continue
			}
			if stmt is ast.HashStmt {
				hashes << ReproHash{
					source:  source
					file_id: file_id
				}
				continue
			}
			mut names := repro_top_decl_names(stmt)
			if names.len == 0 {
				// declarations nested in a top-level comptime `$if` block are wrapped in an
				// ExprStmt; index them under the whole block so referencing one keeps the block
				names = repro_comptime_decl_names(stmt)
			}
			if names.len == 0 {
				continue
			}
			id := decls.len
			decls << ReproDecl{
				names:   names
				source:  source
				file_id: file_id
			}
			for n in names {
				name_to_decl[n] << id
			}
			if stmt is ast.FnDecl && stmt.is_main {
				main_id = id
			}
			// `init`/`cleanup` are called automatically, so keep them (and thus any helper they
			// reach) even though nothing references them by name
			if stmt is ast.FnDecl && !stmt.is_method
				&& repro_short_name(stmt.name) in ['init', 'cleanup'] {
				extra_seeds << id
			}
			if is_root_file && v_line - 1 >= start && v_line - 1 < end {
				root_id = id
			}
		}
	}
	// only ordinary single-module `main` programs are handled; otherwise fall back
	if root_id < 0 || main_id < 0 {
		return ''
	}
	// seed from the failing declaration (for its dependencies) and from `main` (so the failing
	// code stays reachable and the program is runnable); the failing declaration is seeded first
	mut seeds := [root_id]
	if main_id != root_id {
		seeds << main_id
	}
	// plus the module-lifecycle roots, so a failing helper reached only from `init`/`cleanup`
	// stays reachable under the default `skip_unused`
	for s in extra_seeds {
		if s !in seeds {
			seeds << s
		}
	}
	// `none` means the closure hit the declaration cap, i.e. it is incomplete; fall back rather
	// than upload a program with missing symbols
	ordered := repro_closure(decls, name_to_decl, seeds) or { return '' }
	if ordered.len == 0 {
		return ''
	}
	mut referenced := map[string]bool{}
	for id in ordered {
		for name in repro_identifiers(decls[id].source) {
			referenced[name] = true
		}
	}
	// imports are file-scoped: only consider imports from files that actually contributed an
	// inlined declaration, so a same-alias import from an unselected file is never emitted
	mut included_files := map[int]bool{}
	for id in ordered {
		included_files[decls[id].file_id] = true
	}
	mut scoped_imports := []ReproImport{}
	mut bound := map[string]string{} // binding name -> the import source that provides it
	for imp in imports {
		if imp.file_id !in included_files {
			continue
		}
		if !imp.side_effect && !imp.triggers.any(referenced[it]) {
			continue
		}
		// a needed project-local import cannot be satisfied by the single uploaded file
		if imp.is_local {
			return ''
		}
		// two files binding the same name to different modules cannot be flattened into one file
		for t in imp.triggers {
			if t in bound && bound[t] != imp.source {
				return ''
			}
			bound[t] = imp.source
		}
		scoped_imports << imp
	}
	// hash directives are file-scoped too: only include `#include`/`#flag`/... from files that
	// contributed a declaration, so an unrelated file's project-local directive is not emitted
	mut scoped_hashes := []string{}
	for h in hashes {
		if h.file_id in included_files {
			scoped_hashes << h.source
		}
	}
	// every declaration in the closure is required, so an over-budget reproducer is dropped whole
	// (the caller then falls back to a source window) rather than emitting a partial program
	out := repro_render(mod_header, scoped_imports, scoped_hashes, decls, ordered)
	if out == '' || (max_bytes > 0 && out.len > max_bytes) {
		return ''
	}
	return out
}

// repro_attr_start returns the first line (0-based) of the declaration at `line_nr`, walking up
// over any leading `@[...]` attribute lines (which are parsed before the declaration node).
fn repro_attr_start(lines []string, line_nr int) int {
	mut s := if line_nr < 0 {
		0
	} else if line_nr >= lines.len {
		lines.len
	} else {
		line_nr
	}
	for s > 0 && lines[s - 1].trim_space().starts_with('@[') {
		s--
	}
	return s
}

// repro_decl_source returns lines `[start, end)` (0-based) joined, with trailing blank lines trimmed.
fn repro_decl_source(lines []string, start int, end int) string {
	mut e := if end > lines.len { lines.len } else { end }
	for e > start + 1 && lines[e - 1].trim_space() == '' {
		e--
	}
	if start < 0 || start >= e {
		return ''
	}
	return lines[start..e].join('\n')
}

// repro_closure returns the indices of declarations reachable from `seeds` by identifier reference.
// The seeds are always kept. It returns `none` when the closure would exceed
// `c_error_repro_max_decls` declarations: every reached declaration is required, so a truncated
// closure would be a program with missing symbols and the caller should fall back instead.
fn repro_closure(decls []ReproDecl, name_to_decl map[string][]int, seeds []int) ?[]int {
	mut included := []bool{len: decls.len}
	mut order := []int{}
	mut queue := []int{}
	mut is_seed := []bool{len: decls.len}
	for s in seeds {
		if s >= 0 && s < decls.len {
			queue << s
			is_seed[s] = true
		}
	}
	if queue.len == 0 {
		return []
	}
	for queue.len > 0 {
		id := queue[0]
		queue.delete(0)
		if id < 0 || id >= decls.len || included[id] {
			continue
		}
		if !is_seed[id] && order.len >= c_error_repro_max_decls {
			// a required declaration cannot be included within the cap
			return none
		}
		included[id] = true
		order << id
		mut refs := repro_identifiers(decls[id].source)
		// overloaded operators are indexed under their punctuation method name (`+`, `[]`, ...),
		// which is not an identifier token, so add the operator methods a declaration may use
		refs << repro_operator_refs(decls[id].source)
		for name in refs {
			for ref in name_to_decl[name] {
				if ref >= 0 && ref < decls.len && !included[ref] {
					queue << ref
				}
			}
		}
	}
	order.sort()
	return order
}

// repro_operator_refs returns the operator-overload method names (`+`, `[]`, ...) that `source`
// might use, so their declarations are pulled into the closure. It over-approximates (a `+` on
// ints also lists `+`), which is harmless: unmatched operator names simply resolve to nothing.
fn repro_operator_refs(source string) []string {
	mut ops := []string{}
	if source.contains('[') {
		ops << '[]'
		ops << '[]='
	}
	if source.contains('==') || source.contains('!=') {
		ops << '=='
	}
	if source.contains('<') || source.contains('>') {
		ops << '<'
		ops << '=='
	}
	if source.contains('**') {
		ops << '**'
	}
	for c in ['+', '-', '*', '/', '%'] {
		if source.contains(c) {
			ops << c
		}
	}
	return ops
}

// repro_import_stmt reconstructs a single valid `import` line from the AST, so multi-line, aliased
// and selective imports (including the combined `import x as y { Sym }` form) survive as one
// syntactically complete statement.
fn repro_import_stmt(imp ast.Import) string {
	mut s := 'import ${imp.source_name}'
	if imp.alias != '' && imp.alias != imp.source_name.all_after_last('.') {
		s += ' as ${imp.alias}'
	}
	if imp.syms.len > 0 {
		s += ' { ${imp.syms.map(it.name).join(', ')} }'
	}
	return s
}

// repro_render assembles the given declaration ids into a single-file source string, headed by
// `mod_header` (the failing file's `module` line, including any module attributes).
fn repro_render(mod_header string, imports []ReproImport, hashes []string, decls []ReproDecl, ids []int) string {
	mut parts := []string{cap: ids.len}
	for id in ids {
		if id >= 0 && id < decls.len {
			parts << decls[id].source
		}
	}
	body := parts.join('\n\n')
	mut referenced := map[string]bool{}
	for name in repro_identifiers(body) {
		referenced[name] = true
	}
	mut out := '${mod_header}\n\n'
	mut emitted := map[string]bool{}
	for imp in imports {
		if imp.source == '' || imp.source in emitted {
			continue
		}
		mut needed := imp.side_effect
		for t in imp.triggers {
			if referenced[t] {
				needed = true
				break
			}
		}
		if !needed {
			continue
		}
		emitted[imp.source] = true
		out += imp.source + '\n'
	}
	for h in hashes {
		out += h + '\n'
	}
	out += '\n' + body
	return out
}

// repro_identifiers returns the identifier tokens in `source`, skipping the contents of comments,
// string and rune literals (so incidental words there do not pull unrelated declarations into the
// reproducer). Identifiers inside `${...}` string interpolations are still collected.
fn repro_identifiers(source string) []string {
	mut ids := []string{}
	mut i := 0
	for i < source.len {
		c := source[i]
		if c == `/` && i + 1 < source.len && source[i + 1] == `/` {
			for i < source.len && source[i] != `\n` {
				i++
			}
			continue
		}
		if c == `/` && i + 1 < source.len && source[i + 1] == `*` {
			i += 2
			for i + 1 < source.len && !(source[i] == `*` && source[i + 1] == `/`) {
				i++
			}
			i += 2
			continue
		}
		if c == `'` || c == `"` {
			i = scan_string_literal(source, i, mut ids)
			continue
		}
		if c == 96 { // backtick rune literal
			i++
			for i < source.len && source[i] != 96 {
				i += if source[i] == `\\` { 2 } else { 1 }
			}
			i++
			continue
		}
		if c == `_` || c.is_letter() {
			start := i
			for i < source.len && is_ident_char(source[i]) {
				i++
			}
			ids << source[start..i]
			continue
		}
		i++
	}
	return ids
}

// scan_string_literal skips a `'`/`"` string starting at `start`, collecting identifiers found in
// its `${...}` interpolations into `ids`, and returns the index just past the closing quote.
fn scan_string_literal(source string, start int, mut ids []string) int {
	quote := source[start]
	mut i := start + 1
	for i < source.len && source[i] != quote {
		if source[i] == `\\` {
			i += 2
			continue
		}
		if source[i] == `$` && i + 1 < source.len && source[i + 1] == `{` {
			i += 2
			mut depth := 1
			for i < source.len && depth > 0 {
				ci := source[i]
				if ci == `{` {
					depth++
					i++
				} else if ci == `}` {
					depth--
					i++
				} else if ci == `_` || ci.is_letter() {
					st := i
					for i < source.len && is_ident_char(source[i]) {
						i++
					}
					ids << source[st..i]
				} else {
					i++
				}
			}
			continue
		}
		i++
	}
	return i + 1
}

@[inline]
fn is_ident_char(c u8) bool {
	return c == `_` || c.is_letter() || c.is_digit()
}

// repro_short_name returns the last, unqualified component of a possibly module-qualified name
// (`main.Foo.bar` -> `bar`, `math.abs` -> `abs`, `Foo` -> `Foo`).
fn repro_short_name(name string) string {
	return name.all_after_last('.')
}

// repro_import_alias returns the name an import is referred to by in code.
fn repro_import_alias(imp ast.Import) string {
	if imp.alias != '' {
		return imp.alias
	}
	return imp.mod.all_after_last('.')
}

// repro_comptime_decl_names returns the names declared inside a top-level comptime `$if` block
// (which the parser wraps in an `ExprStmt`), so the whole block is inlined when one is referenced.
// Returns [] for anything that is not such a block.
fn repro_comptime_decl_names(stmt ast.Stmt) []string {
	if stmt is ast.ExprStmt {
		if stmt.expr is ast.IfExpr {
			if stmt.expr.is_comptime {
				mut names := []string{}
				for branch in stmt.expr.branches {
					for s in branch.stmts {
						inner := repro_top_decl_names(s)
						if inner.len > 0 {
							names << inner
						} else {
							names << repro_comptime_decl_names(s)
						}
					}
				}
				return names
			}
		}
	}
	return []
}

// repro_top_decl_names returns the top-level names a statement defines, or [] when it is not an
// inlinable top-level declaration.
fn repro_top_decl_names(stmt ast.Stmt) []string {
	match stmt {
		ast.FnDecl {
			return [repro_short_name(stmt.name)]
		}
		ast.StructDecl {
			return [repro_short_name(stmt.name)]
		}
		ast.EnumDecl {
			return [repro_short_name(stmt.name)]
		}
		ast.InterfaceDecl {
			return [repro_short_name(stmt.name)]
		}
		ast.TypeDecl {
			match stmt {
				ast.AliasTypeDecl { return [repro_short_name(stmt.name)] }
				ast.SumTypeDecl { return [repro_short_name(stmt.name)] }
				ast.FnTypeDecl { return [repro_short_name(stmt.name)] }
			}
		}
		ast.ConstDecl {
			return stmt.fields.map(repro_short_name(it.name))
		}
		ast.GlobalDecl {
			return stmt.fields.map(repro_short_name(it.name))
		}
		else {
			return []
		}
	}
}

// is_user_repro_file reports whether `path` is user code that is safe to inline into a reproducer
// (as opposed to vlib/stdlib or an installed module, which are referenced via `import` instead).
fn is_user_repro_file(path string) bool {
	if path == '' || !is_v_source_file(path) {
		return false
	}
	norm := path.replace('\\', '/')
	return !norm.contains('/vlib/') && !norm.contains('/.vmodules/')
		&& !norm.contains('/thirdparty/')
}
