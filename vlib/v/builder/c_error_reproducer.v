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
	mod         string   // the imported module (imports of one module must merge after flattening)
	triggers    []string // the names that make this import needed (alias + selected symbols)
	side_effect bool     // `import x as _`: always keep, it is imported for its side effects
	is_local    bool     // the imported module's source is not uploaded (another parsed user module,
	// or an installed `.vmodules` package): needs the source-window fallback
	file_id int // the file this import belongs to (imports are file-scoped)
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
	// the reproducer would depend on source we do not inline, so it cannot be standalone. Likewise,
	// modules backed by installed `~/.vmodules` packages are kept out of the upload (no sources, no
	// version metadata), so importing one also forces the source-window fallback.
	mut local_mods := map[string]bool{}
	mut vmodule_mods := map[string]bool{}
	for pf in v.parsed_files {
		if is_user_repro_file(pf.path) {
			local_mods[pf.mod.name] = true
		} else if pf.path.replace('\\', '/').contains('/.vmodules/') {
			vmodule_mods[pf.mod.name] = true
		}
	}
	// `-d used_fns=main.entry,glob*` explicitly roots functions in the mark-used pass, and the
	// report replays the recorded define, so those functions must be seeded here too
	used_fns_patterns := v.pref.compile_values['used_fns'].split_any(',').filter(it != '')
	mut decls := []ReproDecl{}
	mut name_to_decl := map[string][]int{}
	mut imports := []ReproImport{}
	mut hashes := []ReproHash{}
	mut root_id := -1
	mut main_id := -1
	mut extra_seeds := []int{} // skip_unused roots (init/cleanup/@[markused]/@[export])
	mut file_headers := map[int]string{} // per-file `module` header (attributes are per-file)
	mut start_to_decl := map[string]int{} // '<file_id>:<start line>' -> decl id (solver clone dedup)
	// the `veb.Result` type index, used to seed veb route actions (functions returning `veb.Result`,
	// which the mark-used pass roots even though nothing in source calls them); 0 when veb is unused
	veb_result_idx := v.table.find_type('veb.Result')
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
		// capture this file's `module` declaration verbatim, including any leading attributes
		// (`@[has_globals]`, `@[manualfree]`, ...), which the parser treats as per-file
		mut header := 'module main'
		for li, ln in lines {
			if ln.trim_space().starts_with('module ') {
				header = lines[repro_attr_start(lines, li)..li + 1].join('\n')
				break
			}
		}
		file_headers[file_id] = header
		for imp in file.imports {
			mut triggers := [repro_import_alias(imp)]
			for s in imp.syms {
				triggers << s.name
			}
			imports << ReproImport{
				source:      repro_import_stmt(imp)
				mod:         imp.mod
				triggers:    triggers
				side_effect: imp.alias == '_'
				is_local:    (imp.mod in local_mods && imp.mod != root_mod)
					|| imp.mod in vmodule_mods
				file_id:     file_id
			}
		}
		// attribute-aware start line (0-based) of every top-level statement partitions the file;
		// top-level attributes are parsed before the declaration, so extend each start up over them
		mut starts := []int{}
		for stmt in file.stmts {
			mut line_nr := ast.Node(stmt).pos().line_nr
			if stmt is ast.Block {
				// a comptime `$if <os>` block that `comptime.solve_files` already hoisted becomes a
				// bare `ast.Block` with no pos of its own; locate its `$if`/`$match` opener instead
				line_nr = repro_block_opener_line(stmt, lines)
			}
			starts << if line_nr < 0 { -1 } else { repro_attr_start(lines, line_nr) }
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
			mut force_seed := false
			if names.len == 0 {
				// declarations nested in a top-level comptime `$if` block are wrapped in an
				// ExprStmt; index them under the whole block so referencing one keeps the block
				names = repro_comptime_decl_names(stmt)
			}
			if names.len == 0 {
				// an active hash-only comptime block declares no symbol, but its `#flag`/
				// `#include` directives still apply module-wide in the original build; keep the
				// block verbatim as an always-included declaration (the embedded local-path scan
				// below still forces the fallback for project-local directives). The synthetic
				// name starts with `#`, so no identifier token can ever reference it.
				if repro_is_comptime_block_stmt(stmt) && repro_source_has_hash_directive(source) {
					names = ['#condhash']
					force_seed = true
				} else {
					continue
				}
			}
			// `-new-generic-solver` (generics.solve_files) removes a generic `fn foo[T]`
			// declaration and appends concrete clones named `foo_T_int` that keep the original
			// position. The copied source and its call sites still say `foo`, so also index the
			// name the source text itself declares.
			if stmt is ast.FnDecl {
				src_name := repro_source_fn_name(source)
				if src_name != '' && src_name !in names {
					names << src_name
				}
			}
			// ... and collapse clones sharing one source range into a single declaration:
			// emitting the same copied source once per concrete instantiation would not compile
			start_key := '${file_id}:${start}'
			mut id := -1
			if prev := start_to_decl[start_key] {
				id = prev
				mut merged_names := decls[id].names.clone()
				for n in names {
					if n !in merged_names {
						merged_names << n
						name_to_decl[n] << id
					}
				}
				decls[id] = ReproDecl{
					...decls[id]
					names: merged_names
				}
			} else {
				id = decls.len
				start_to_decl[start_key] = id
				decls << ReproDecl{
					names:   names
					source:  source
					file_id: file_id
				}
				for n in names {
					name_to_decl[n] << id
				}
			}
			// index instance methods under a synthetic `<ReceiverType>#methods` key too, so a
			// declaration that reflects over `<Type>.methods` (`$for m in Foo.methods`) can pull in
			// the type's methods even though their names never appear as identifier tokens
			if stmt is ast.FnDecl && stmt.is_method {
				recv := repro_short_name(v.table.sym(stmt.receiver.typ).name)
				if recv != '' {
					name_to_decl['${recv}#methods'] << id
					// the mark-used finalizer roots these operator methods whenever their
					// receiver type is used, even without a source-level use of the operator;
					// index them under `<Recv>#op` so the closure can do the same
					if repro_short_name(stmt.name) in ['+', '-', '*', '%', '/', '<', '=='] {
						name_to_decl['${recv}#op'] << id
					}
				}
			}
			if stmt is ast.FnDecl && stmt.is_main {
				main_id = id
			}
			// skip_unused roots are kept regardless of references, so a failing declaration reached
			// only from one stays reachable. For functions: init/cleanup, `@[markused]`/`@[export]`,
			// veb hooks and actions, and — for a `-shared` build — every public function. For
			// other declaration kinds (consts, globals, structs/interfaces, enums, declared
			// types): those tagged `@[markused]` or `@[export]`, both of which the mark-used pass
			// roots (see markused/walker.v mark_markused_consts/mark_markused_globals).
			mut is_root := false
			if stmt is ast.FnDecl {
				is_root = repro_is_markused_root(stmt, int(veb_result_idx), v.pref.translated)
					|| (v.pref.is_shared && stmt.is_pub)
					|| repro_fn_in_used_fns(stmt.name, used_fns_patterns)
			} else {
				is_root = repro_decl_attrs(stmt).any(it.name in ['markused', 'export'])
				// a -shared build also retains every public constant and walks its initializer
				// (markused.v), so a helper called only from one must stay reachable
				if !is_root && v.pref.is_shared && stmt is ast.ConstDecl && stmt.is_pub {
					is_root = true
				}
			}
			if is_root || force_seed {
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
	// under -autofree, generated cleanup calls custom `free` methods without any source-level
	// reference; the mark-used pass retains them (see markused/walker.v), so seed them here too,
	// or a helper reachable only from a `Foo.free()` is dropped on replay (the recorded build
	// options include -autofree) and the C error vanishes
	if v.pref.autofree {
		for id in name_to_decl['free'] {
			extra_seeds << id
		}
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
	// track which files contributed a declaration, and the identifiers each such file references,
	// so imports are judged only against declarations from their own file (a same-named local var
	// in another file must not make an unrelated import look needed)
	mut included_files := map[int]bool{}
	mut file_referenced := map[string]bool{} // key: "<file_id>\x00<name>"
	mut included_names := map[string]bool{} // all names the inlined declarations define
	mut reflection_targets := []string{} // types reflected over with `<Type>.methods`
	for id in ordered {
		fid := decls[id].file_id
		included_files[fid] = true
		// a retained declaration using a compile-time resource (`$embed_file`/`$tmpl`/`$res`)
		// needs a project-local file we do not upload, so the reproducer cannot be standalone;
		// a machine- or layout-dependent pseudo variable (`@FILE`, `@LINE`, `@VMODROOT`, ...)
		// would likewise evaluate differently in the flattened replay
		if repro_uses_local_resource(decls[id].source)
			|| repro_uses_machine_pseudo(decls[id].source) {
			return ''
		}
		// a retained top-level comptime block (`$if`/`$match`) that carries its own conditional
		// `import` would collide with the same import re-emitted at the top of the flattened file
		// (`file.imports` also lists it), which the checker rejects as "module already imported";
		// only such a block can hold an `import` in its copied source, so fall back on it
		if repro_source_has_toplevel_import(decls[id].source) {
			return ''
		}
		// a retained top-level comptime block can likewise embed its own `#` directive; a
		// project-local one (`#include "private.h"`, `#flag ./local.o`) references a file that
		// is not uploaded, so the flattened program cannot be standalone
		if repro_embedded_local_hash(decls[id].source) {
			return ''
		}
		reflection_targets << repro_reflection_method_targets(decls[id].source)
		for n in decls[id].names {
			included_names[n] = true
		}
		for name in repro_identifiers(decls[id].source) {
			file_referenced['${fid}\x00${name}'] = true
		}
	}
	// `$for m in T.methods` over a target we did not inline as a concrete type (a generic parameter,
	// or a type resolved from another module) cannot be modelled here: the replayed loop would then
	// iterate a different method set and the C error may vanish, so fall back instead
	for t in reflection_targets {
		if t !in included_names {
			return ''
		}
	}
	// the flattened file can carry only one module header: if contributing files disagree on their
	// module attributes (e.g. one has `@[has_globals]`), we cannot faithfully merge them, so fall back
	mut mod_header := ''
	for fid, _ in included_files {
		h := file_headers[fid]
		if mod_header == '' {
			mod_header = h
		} else if h != mod_header {
			return ''
		}
	}
	if mod_header == '' {
		mod_header = 'module main'
	}
	// which names each file binds through its own imports: a name used in a file that also
	// imports it is an import reference, not a local binding
	mut file_binds := map[string]bool{} // key: "<file_id>\x00<name>"
	for imp in imports {
		for t in imp.triggers {
			file_binds['${imp.file_id}\x00${t}'] = true
		}
	}
	mut scoped_imports := []ReproImport{}
	mut bound := map[string]string{} // binding name -> the import source that provides it
	for imp in imports {
		// Side-effect imports affect the module even when their source file contributes no
		// declaration, so keep them just like module-wide hash directives.
		if imp.file_id !in included_files && !imp.side_effect {
			continue
		}
		if !imp.side_effect && !imp.triggers.any('${imp.file_id}\x00${it}' in file_referenced) {
			continue
		}
		// a needed project-local import cannot be satisfied by the single uploaded file
		if imp.is_local {
			return ''
		}
		for t in imp.triggers {
			// two files binding the same name to different modules cannot be flattened into one file
			if t in bound && bound[t] != imp.source {
				return ''
			}
			// a file-scoped import binding (e.g. `import time { Time }`) that clashes with an
			// inlined declaration of the same name would, when flattened, shadow it module-wide
			if t != '_' && t in included_names {
				return ''
			}
			// the binding may also collide with a parameter or local variable in a retained
			// declaration from another contributing file (the checker rejects those as duplicate
			// import symbols once the import becomes file-visible after flattening). Token-level
			// scanning cannot tell a local binding from a reference, so any use of the name in a
			// file that does not import it itself is treated as a collision.
			if t != '_'
				&& repro_import_collides(t, imp.file_id, included_files, file_referenced, file_binds) {
				return ''
			}
			bound[t] = imp.source
		}
		scoped_imports << imp
	}
	// imports are file-scoped in the original program, but the flattened file has one scope: two
	// import statements naming the same module are rejected by the checker as a duplicate import
	// even when their trigger names differ (`import log` + `import log as _`)
	scoped_imports = repro_merge_module_imports(scoped_imports) or { return '' }
	// hash directives are module-wide: the original build applies `#flag`/`#include`/... from
	// every parsed file of the module, whether or not any of that file's declarations are
	// retained (a hash-only `.c.v` companion file is common), so all of them are kept. A
	// project-local dependency (`#include "private.h"`, a path/`@V...`-based `#flag`, ...)
	// references a file we do not upload, so fall back instead of emitting a directive the
	// receiver cannot satisfy.
	mut scoped_hashes := []string{}
	for h in hashes {
		if repro_hash_is_local(h.source) {
			return ''
		}
		if h.source !in scoped_hashes {
			scoped_hashes << h.source
		}
	}
	if repro_covers_any_whole_file(decls, imports, hashes, scoped_imports, ordered) {
		// The payload includes every substantive item from at least one contributing
		// file, so it would reconstruct that whole file (differing only by normalized
		// blank lines). Give up and let the caller use the bounded source-window path,
		// which enforces the same strict-subset guarantee.
		return ''
	}
	// every declaration in the closure is required, so an over-budget reproducer is dropped whole
	// (the caller then falls back to a source window) rather than emitting a partial program
	out := repro_render(mod_header, scoped_imports, scoped_hashes, decls, ordered)
	if out == '' || (max_bytes > 0 && out.len > max_bytes) {
		return ''
	}
	return out
}

// repro_block_opener_line returns the 0-based source line of the `$if`/`$match` opener of a solved
// comptime `ast.Block` (whose own pos is unset), found by scanning up from its first inner
// declaration. Returns -1 when the block is empty or no opener line is found.
fn repro_block_opener_line(block ast.Block, lines []string) int {
	first, depth := repro_block_first_positioned_line(block)
	if first < 0 {
		return -1
	}
	// the first positioned statement may sit inside `depth` nested solved blocks: walk up past
	// their openers to the one that belongs to this block
	mut j := if first < lines.len { first } else { lines.len - 1 }
	mut openers := 0
	for j >= 0 {
		t := lines[j].trim_space()
		if t.starts_with('\$if ') || t.starts_with('\$match ') {
			openers++
			if openers > depth {
				return j
			}
		}
		j--
	}
	return -1
}

// repro_block_first_positioned_line returns the smallest source line of any statement inside a
// solved comptime block together with how many nested solved blocks enclose it, descending into
// nested blocks (whose own pos is unset too: `$if linux { $if linux { fn helper() {} } }` nests
// one positionless block in another) so the outer block still finds a usable line. Returns
// (-1, 0) when nothing inside carries a position.
fn repro_block_first_positioned_line(block ast.Block) (int, int) {
	mut first := -1
	mut first_depth := 0
	for s in block.stmts {
		mut ln := ast.Node(s).pos().line_nr
		mut depth := 0
		if s is ast.Block {
			inner_ln, inner_depth := repro_block_first_positioned_line(s)
			ln = inner_ln
			depth = inner_depth + 1
		}
		if ln >= 0 && (first < 0 || ln < first) {
			first = ln
			first_depth = depth
		}
	}
	return first, first_depth
}

// repro_attr_start returns the first line (0-based) of the declaration at `line_nr`, walking up
// over any leading `@[...]` attribute groups (which are parsed before the declaration node). An
// attribute may span several lines (`@[footer: 'Hello\nWorld']`), so the line directly above a
// declaration can be an attribute *continuation* rather than one starting with `@[`; balance the
// `[`/`]` brackets upward to find the line that opens the group instead of checking only prefixes.
fn repro_attr_start(lines []string, line_nr int) int {
	mut s := if line_nr < 0 {
		0
	} else if line_nr >= lines.len {
		lines.len
	} else {
		line_nr
	}
	for s > 0 {
		// the parser skips blank and comment lines between an attribute group and its
		// declaration, so walk over that trivia before looking for the group's closing `]`
		mut end := s - 1
		for end > 0 {
			t := lines[end].trim_space()
			if t != '' && !t.starts_with('//') {
				break
			}
			end--
		}
		if !lines[end].trim_space().ends_with(']') {
			break // the line above the declaration does not close an attribute group
		}
		open := repro_attr_open_line(lines, end)
		if open < 0 {
			break
		}
		s = open
	}
	return s
}

// repro_attr_open_line locates the line that opens the attribute group closing on line `end`, or
// -1 when the closing bracket does not belong to an attribute. The per-line bracket balance walk
// resolves single-line and stacked attributes (and stops a preceding const array ending in `]`
// from being swallowed); an attribute string spanning source lines defeats per-line balances, so
// the nearest `@[` line above is then verified by a forward scan of the joined candidate text.
fn repro_attr_open_line(lines []string, end int) int {
	mut open := end
	mut bal := 0
	for open >= 0 {
		bal += repro_bracket_delta(lines[open])
		if bal == 0 {
			break
		}
		open--
	}
	if open >= 0 && bal == 0 && lines[open].trim_space().starts_with('@[') {
		return open
	}
	mut cand := end
	for cand >= 0 && end - cand < 16 {
		if lines[cand].trim_space().starts_with('@[') {
			if repro_attr_group_spans(lines[cand..end + 1].join('\n')) {
				return cand
			}
			break
		}
		cand--
	}
	return -1
}

// repro_attr_group_spans reports whether `text` is one uninterrupted `@[...]` attribute group:
// it opens with `@[`, its structural bracket depth only returns to zero at the very end, and
// string literals (which may span lines and contain brackets) are skipped. Ordinary code between
// an unrelated attribute above and the closing line fails this scan, because the earlier
// attribute's own `]` returns the depth to zero early.
fn repro_attr_group_spans(text string) bool {
	t := text.trim_space()
	if !t.starts_with('@[') {
		return false
	}
	mut i := 0
	mut depth := 0
	for i < t.len {
		c := t[i]
		if c == `'` || c == `"` {
			i++
			for i < t.len && t[i] != c {
				i += if t[i] == `\\` { 2 } else { 1 }
			}
			i++
			continue
		}
		if c == `/` && i + 1 < t.len && t[i + 1] == `/` {
			for i < t.len && t[i] != `\n` {
				i++
			}
			continue
		}
		if c == `[` {
			depth++
		} else if c == `]` {
			depth--
			if depth == 0 {
				// the group must end here (only whitespace may follow)
				return t[i + 1..].trim_space() == ''
			}
			if depth < 0 {
				return false
			}
		}
		i++
	}
	return false
}

// repro_bracket_delta returns the net structural bracket balance of a line: the count of `[`
// minus the count of `]`, ignoring brackets inside string literals (an attribute value like
// `@[footer: 'World]']` must not skew the balance) and after a `//` comment. Used to locate
// where a possibly multi-line `@[...]` attribute group opens.
fn repro_bracket_delta(line string) int {
	mut d := 0
	mut i := 0
	for i < line.len {
		c := line[i]
		if c == `'` || c == `"` {
			i++
			for i < line.len && line[i] != c {
				i += if line[i] == `\\` { 2 } else { 1 }
			}
			i++
			continue
		}
		if c == `/` && i + 1 < line.len && line[i + 1] == `/` {
			break
		}
		if c == `[` {
			d++
		} else if c == `]` {
			d--
		}
		i++
	}
	return d
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

// repro_covers_any_whole_file reports whether the selected declarations, emitted imports and
// module-wide hash directives reconstruct every substantive item from at least one contributing
// file. Coverage is tracked per file, not across the whole module, so a multi-file program cannot
// expose one complete small companion while excluding an unrelated declaration elsewhere.
fn repro_covers_any_whole_file(decls []ReproDecl, imports []ReproImport, hashes []ReproHash, emitted_imports []ReproImport, ordered []int) bool {
	mut total_per_file := map[int]int{}
	mut included_per_file := map[int]int{}
	for d in decls {
		total_per_file[d.file_id]++
	}
	for id in ordered {
		included_per_file[decls[id].file_id]++
	}
	mut emitted_import_sources := map[string]bool{}
	for imp in emitted_imports {
		emitted_import_sources[imp.source] = true
	}
	for imp in imports {
		total_per_file[imp.file_id]++
		// Imports can be deduplicated while flattening. If an identical source line is
		// emitted for another file, it still reveals this file's complete import line.
		if imp.source in emitted_import_sources {
			included_per_file[imp.file_id]++
		}
	}
	// Hash directives (`#flag`/`#include`/...) are module-wide and are emitted for every
	// parsed file whether or not any of that file's declarations are retained (see the
	// scoped_hashes loop), so each file's hashes always count as fully included. A
	// declaration-free `.c.v` companion (e.g. `module main` + `#flag -DSECRET=...`) is
	// thus reconstructed whole and must count as whole-file coverage, even though it
	// contributes no ReproDecl.
	for h in hashes {
		total_per_file[h.file_id]++
		included_per_file[h.file_id]++
	}
	for file_id, total in total_per_file {
		if total > 0 && included_per_file[file_id] == total {
			return true
		}
	}
	return false
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
		// string interpolation and the print/dump/assert family invoke custom `str` methods
		// implicitly: the token scan sees `Foo` in `'\${Foo{}}'` but never the method name, and
		// replaying with the default `skip_unused` would then drop `Foo.str` (and anything only
		// it reaches), making the C error vanish. Retain every custom `str` method instead —
		// a safe over-approximation, mirroring the mark-used walker, which roots string methods
		// for interpolation.
		if repro_source_stringifies(decls[id].source, refs) {
			refs << 'str'
		}
		// `for item in Iterator{}` invokes the iterator's `next` method implicitly: the token
		// scan sees the type but never `next`, and the replayed check then fails on the loop
		// (or replayed `skip_unused` drops a helper only `next` reaches). Retain every custom
		// `next` method when a declaration iterates, mirroring the mark-used walker's struct
		// iteration handling — the same safe over-approximation as `str` above.
		if repro_source_iterates(decls[id].source) {
			refs << 'next'
		}
		// json2.encode invokes a user type's `to_json`/`json_str` hook implicitly when the type
		// implements the JsonEncoder/Encodable interface: the token scan sees the type and
		// `encode` but never the hook name, and replaying without the hook selects ordinary
		// field encoding (dropping a helper only the hook reaches). Retain the hooks whenever a
		// declaration references `encode`, mirroring the mark-used walker.
		if 'encode' in refs {
			refs << 'to_json'
			refs << 'json_str'
		}
		// the json2 decoders likewise invoke a local type's decoding hooks implicitly
		// (from_json_string/_number/_boolean/_null); retain them when a decode API is
		// referenced, or the replay stops recognizing the decoder interface (or drops a
		// helper only a hook reaches)
		if 'decode' in refs || 'raw_decode' in refs {
			refs << 'from_json_string'
			refs << 'from_json_number'
			refs << 'from_json_boolean'
			refs << 'from_json_null'
		}
		// `$for m in Foo.methods` reflection needs Foo's methods, which are indexed under the
		// synthetic `Foo#methods` key; add those refs so the whole method set is retained
		for t in repro_reflection_method_targets(decls[id].source) {
			refs << '${t}#methods'
		}
		// the mark-used finalizer roots `+`/`-`/`*`/`%`/`/`/`<`/`==` methods whose receiver
		// type is used even when the operator never appears in source (walker.v); mirror it by
		// referencing every retained name's `#op` set (only type names index anything there)
		for n in decls[id].names {
			refs << '${n}#op'
		}
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

// repro_source_stringifies reports whether a declaration may implicitly invoke `str` methods:
// it interpolates values into a string, or calls one of the print/dump functions (whose
// arguments are stringified), or asserts (failure messages stringify the operands).
fn repro_source_stringifies(source string, refs []string) bool {
	if source.contains('\${') {
		return true
	}
	for r in refs {
		if r in ['println', 'eprintln', 'print', 'eprint', 'dump', 'assert'] {
			return true
		}
	}
	return false
}

// repro_source_iterates reports whether a declaration contains a `for ... in ...` loop, whose
// iterable may be a custom iterator (invoking its `next` method implicitly). Matched on the
// keyword sequence with identifier boundaries; `$for` comptime loops iterate metadata, not
// values, so they do not count.
fn repro_source_iterates(source string) bool {
	mut i := 0
	for i + 3 < source.len {
		if source[i] == `f` && source[i..i + 3] == 'for'
			&& (i == 0 || (!is_ident_char(source[i - 1]) && source[i - 1] != `$`))
			&& !is_ident_char(source[i + 3]) {
			// a `for` header runs to its `{`; an ` in ` inside it marks iteration
			mut j := i + 3
			for j < source.len && source[j] != `{` && source[j] != `\n` {
				if source[j] == ` ` && j + 3 < source.len && source[j + 1] == `i`
					&& source[j + 2] == `n` && source[j + 3] == ` ` {
					return true
				}
				j++
			}
		}
		i++
	}
	return false
}

// repro_reflection_method_targets returns the names a declaration reflects over with
// `<target>.methods`, i.e. the iterables of `$for m in <target>.methods` loops. The target may be a
// concrete type (`Foo`), a generic parameter (`T`), or a metadata variable from an outer `$for`
// (`field`, as in the nested `$for field in C.fields { $for m in field.methods {} }` form). Method
// names never appear as identifier tokens in such a loop, so these targets are used to pull the
// type's methods into the closure — or, when a target cannot be resolved to an inlined type (a
// generic parameter or metadata variable), to trigger the source-window fallback.
fn repro_reflection_method_targets(source string) []string {
	// method reflection only appears inside a comptime `$for` loop; without one, any `.methods`
	// is an ordinary member access (`registry.methods()`) and must not drive retention or fallback
	if !source.contains('\$for') {
		return []
	}
	suffix := '.methods'
	mut targets := []string{}
	mut i := 0
	for i + suffix.len <= source.len {
		if source[i] == `.` && source[i..i + suffix.len] == suffix {
			after := i + suffix.len
			// `.methods` must be a whole token (not `.methodsX`) to be reflection
			if after >= source.len || !is_ident_char(source[after]) {
				// a following `(` is a runtime call to a method named `methods`, not reflection
				mut k := after
				for k < source.len && (source[k] == ` ` || source[k] == `\t`) {
					k++
				}
				is_call := k < source.len && source[k] == `(`
				// the target identifier immediately before `.methods`
				mut j := i
				for j > 0 && is_ident_char(source[j - 1]) {
					j--
				}
				// only a `$for <var> in <target>.methods` iterable is reflection: require the `in`
				// keyword right before the target, so an ordinary `obj.methods` member access is
				// not mistaken for it (this admits lowercase metadata variables like `field`)
				mut p := j
				for p > 0 && (source[p - 1] == ` ` || source[p - 1] == `\t`) {
					p--
				}
				preceded_by_in := p >= 2 && source[p - 1] == `n` && source[p - 2] == `i`
					&& (p == 2 || !is_ident_char(source[p - 3]))
				if j < i && !is_call && preceded_by_in {
					targets << source[j..i]
				}
			}
		}
		i++
	}
	return targets
}

// repro_import_collides reports whether an import-bound name is also used in a retained
// declaration from a contributing file that does not import it itself: after flattening, the
// import becomes visible there and the checker rejects same-named parameters and locals as
// duplicate import symbols. Uses of the name in files carrying an equivalent import are ordinary
// references and do not collide.
fn repro_import_collides(name string, imp_file_id int, included_files map[int]bool, file_referenced map[string]bool, file_binds map[string]bool) bool {
	for fid, _ in included_files {
		if fid == imp_file_id {
			continue
		}
		key := '${fid}\x00${name}'
		if key in file_referenced && key !in file_binds {
			return true
		}
	}
	return false
}

// repro_merge_module_imports collapses retained imports of the same module into one statement:
// exact duplicates are dropped, and a side-effect-only import (`import x as _`) is subsumed by
// any other import of the module (importing it at all runs its initializers). Returns none when
// two genuinely different forms of one module remain (`import log` + `import log as l`), which
// one flattened file cannot express.
fn repro_merge_module_imports(imports []ReproImport) ?[]ReproImport {
	mut primary := map[string]string{} // module -> its non-side-effect import statement
	for imp in imports {
		if !imp.side_effect {
			if existing := primary[imp.mod] {
				if existing != imp.source {
					return none
				}
			} else {
				primary[imp.mod] = imp.source
			}
		}
	}
	mut merged := []ReproImport{}
	mut seen := map[string]bool{}
	for imp in imports {
		if imp.side_effect && imp.mod in primary {
			continue
		}
		if imp.source in seen {
			continue
		}
		seen[imp.source] = true
		merged << imp
	}
	return merged
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

// repro_decl_attrs returns the attributes attached to a top-level declaration, for any of the
// declaration kinds that can carry `@[markused]` and become a mark-used root.
fn repro_decl_attrs(stmt ast.Stmt) []ast.Attr {
	match stmt {
		ast.FnDecl {
			return stmt.attrs
		}
		ast.StructDecl {
			return stmt.attrs
		}
		ast.InterfaceDecl {
			return stmt.attrs
		}
		ast.EnumDecl {
			return stmt.attrs
		}
		ast.ConstDecl {
			return stmt.attrs
		}
		ast.GlobalDecl {
			return stmt.attrs
		}
		ast.TypeDecl {
			match stmt {
				ast.AliasTypeDecl { return stmt.attrs }
				ast.SumTypeDecl { return stmt.attrs }
				ast.FnTypeDecl { return stmt.attrs }
			}
		}
		else {
			return []
		}
	}
}

// repro_is_markused_root reports whether a function is a `skip_unused` root that is retained even
// when nothing references it: an `init`/`cleanup` lifecycle function, a `lock`/`unlock`/`rlock`/
// `runlock` method (the mark-used pass roots all of these — see markused.v), or one tagged
// `@[markused]`/`@[export]`. Such a function must be inlined so any helper it alone reaches stays
// reachable when `skip_unused` runs again on the replayed reproducer.
fn repro_is_markused_root(f ast.FnDecl, veb_res_idx int, translated bool) bool {
	short := repro_short_name(f.name)
	// the mark-used pass roots every function key ending in `.init`/`.cleanup` (markused.v),
	// which covers plain lifecycle functions and methods like `Foo.init` alike
	if short in ['init', 'cleanup'] {
		return true
	}
	// lock helpers for `shared` types are rooted by name regardless of references
	if f.is_method && short in ['lock', 'unlock', 'rlock', 'runlock'] {
		return true
	}
	// veb `before_request` hooks are invoked by the framework without any source-level call;
	// the mark-used pass roots every function whose name ends with `before_request`
	// (markused.v: `k.ends_with('before_request')`), so mirror that suffix match here
	if short.ends_with('before_request') {
		return true
	}
	// veb route actions are invoked by the router, again without a source-level call: the
	// mark-used pass roots every function returning `veb.Result`
	// (walker.v mark_markused_fns), identified by the table's cached type index
	if veb_res_idx > 0 && int(f.return_type) == veb_res_idx {
		return true
	}
	// a -translated build roots every function carrying a `c` attribute (markused.v), so a
	// helper called only from one must stay reachable in the replay
	if translated && f.attrs.any(it.name == 'c') {
		return true
	}
	return f.attrs.any(it.name == 'markused' || it.name == 'export')
}

// repro_fn_in_used_fns reports whether a function's qualified name is selected by the
// `-d used_fns=...` define, matching exactly like the mark-used pass: literal names compare
// against the full key, and patterns containing `*` glob-match it.
fn repro_fn_in_used_fns(name string, patterns []string) bool {
	for p in patterns {
		if p.contains('*') {
			if name.match_glob(p) {
				return true
			}
		} else if name == p {
			return true
		}
	}
	return false
}

// repro_hash_is_local reports whether a `#`-directive depends on a project-local file or path that
// is not uploaded with the reproducer (a quoted/relative `#include`, or a `#flag`/other directive
// naming a path or `@V...` root), as opposed to a system header or library.
fn repro_hash_is_local(directive string) bool {
	t := directive.trim_space()
	for kw in ['#include', '#insert', '#preinclude'] {
		if !t.starts_with(kw) {
			continue
		}
		// system headers use `<...>`; a quoted or relative include is project-local. Only the
		// first delimiter after the keyword decides: a trailing comment containing `<`
		// (`#include "private.h" // see <API>`) must not make a quoted include look like a
		// system one.
		mut i := kw.len
		for i < t.len && (t[i] == ` ` || t[i] == `\t`) {
			i++
		}
		return i >= t.len || t[i] != `<`
	}
	if t.starts_with('#flag') {
		return t.contains('"') || t.contains('@V') || t.contains('./') || t.contains('../')
			|| t.contains('-I') || t.contains('-L') || t.contains('-include')
			|| repro_flag_has_abs_path(t) || repro_flag_has_bare_file(t)
	}
	if t.starts_with('#pkgconfig') {
		return false // resolved from installed pkg-config packages, like a system library
	}
	// any other directive: be conservative and treat it as a local dependency
	return t.starts_with('#')
}

// repro_flag_has_abs_path reports whether a `#flag` directive names a bare absolute path argument
// (e.g. `#flag /path/to/ffi.a`, or a Windows `C:\...`/`C:/...` path), which points at a file that is
// absent on the receiver and would fail linking before the original C error is reached.
fn repro_flag_has_abs_path(directive string) bool {
	for field in directive.fields() {
		if field.starts_with('/') {
			return true
		}
		// windows drive-absolute path: `C:\lib\x.a` or `C:/lib/x.a`
		if field.len >= 3 && field[1] == `:` && (field[2] == `\\` || field[2] == `/`)
			&& u8(field[0]).is_letter() {
			return true
		}
	}
	return false
}

// repro_flag_has_bare_file reports whether a `#flag` directive names a bare relative file
// argument (`#flag helper.o`, `#flag -include config.h`'s header is caught by the `-include`
// check): a non-option field containing a `.` is a filename, which the receiver does not have.
// Platform prefixes (`linux`, `windows`, ...) carry no dot and option fields start with `-`.
fn repro_flag_has_bare_file(directive string) bool {
	fields := directive.fields()
	for field in fields[1..] {
		if field.starts_with('-') || field.starts_with('$') || field.starts_with('@') {
			continue
		}
		if field.contains('.') {
			return true
		}
	}
	return false
}

// repro_source_has_toplevel_import reports whether a declaration's copied source contains an
// `import` statement. In valid V, `import` may only appear at the top of a file or inside a
// beginning-of-file comptime `$if`/`$match` block, so a non-empty result means a retained comptime
// block carries a conditional import (which the flattener would otherwise duplicate at the top).
fn repro_source_has_toplevel_import(source string) bool {
	for line in source.split_into_lines() {
		if line.trim_space().starts_with('import ') {
			return true
		}
	}
	return false
}

// repro_embedded_local_hash reports whether a declaration's copied source embeds a
// project-local `#` directive. Ordinary top-level hash statements are separate declarations and
// never appear inside another declaration's source, so any `#` line here comes from a retained
// comptime `$if`/`$match` block (they are uploaded verbatim, which is fine for system headers
// and libraries, but a local path cannot be satisfied on the receiver).
fn repro_embedded_local_hash(source string) bool {
	for line in source.split_into_lines() {
		t := line.trim_space()
		if t.starts_with('#') && repro_hash_is_local(t) {
			return true
		}
	}
	return false
}

// repro_is_comptime_block_stmt reports whether a top-level statement is a comptime conditional
// block: a bare `ast.Block` (an active branch hoisted by comptime.solve_files) or an
// unsolved comptime `$if`/`$match` expression statement.
fn repro_is_comptime_block_stmt(stmt ast.Stmt) bool {
	if stmt is ast.Block {
		return true
	}
	if stmt is ast.ExprStmt {
		if stmt.expr is ast.IfExpr {
			return stmt.expr.is_comptime
		}
		if stmt.expr is ast.MatchExpr {
			return stmt.expr.is_comptime
		}
	}
	return false
}

// repro_source_has_hash_directive reports whether any line of a declaration's copied source is a
// `#` directive.
fn repro_source_has_hash_directive(source string) bool {
	for line in source.split_into_lines() {
		if line.trim_space().starts_with('#') {
			return true
		}
	}
	return false
}

// repro_uses_local_resource reports whether a declaration uses a compile-time construct whose
// value depends on the build machine and is not carried by the report: a project-local file
// (`$embed_file`, `$tmpl`, `$res`) or the builder's environment (`$env`, baked in at compile
// time - replaying it against the receiver's environment can change the generated C and lose
// the error).
fn repro_uses_local_resource(source string) bool {
	for name in ['embed_file', 'tmpl', 'res', 'env', 'veb.html', 'pkgconfig'] {
		if repro_has_comptime_call(source, name) {
			return true
		}
	}
	return false
}

// repro_has_comptime_call reports whether `source` calls the compile-time function `$<name>`.
// V tokenizes the call, so whitespace between the name and its `(` is valid
// (`$embed_file ('asset.bin')`) and an exact-substring check would miss it.
fn repro_has_comptime_call(source string, name string) bool {
	pat := '$' + name
	mut i := 0
	for i + pat.len <= source.len {
		if source[i] != `$` || source[i..i + pat.len] != pat {
			i++
			continue
		}
		mut j := i + pat.len
		// the name must end here: `$reserve(` must not match `$res`
		if j < source.len && is_ident_char(source[j]) {
			i++
			continue
		}
		for j < source.len && (source[j] == ` ` || source[j] == `\t`) {
			j++
		}
		if j < source.len && source[j] == `(` {
			return true
		}
		i++
	}
	return false
}

// Pseudo variables whose compile-time value depends on the file layout, the project root or the
// builder installation: flattening the declarations into one uploaded file changes path and line
// values, and replaying on another machine changes the root/hash/build values, so retained code
// using one can generate different C and lose the error. (`@FN`/`@METHOD`/`@MOD`/`@STRUCT` and
// `@COLUMN` survive the verbatim flatten, and `@OS`/`@PLATFORM`/`@BACKEND`/`@CCOMPILER` are
// reproducible from the recorded build options, so those stay allowed.)
const c_error_repro_machine_pseudos = ['@VROOT', '@VMODROOT', '@VEXEROOT', '@VEXE', '@FILE_LINE',
	'@FILE', '@DIR', '@LINE', '@LOCATION', '@VHASH', '@VCURRENTHASH', '@VMOD_FILE', '@VMODHASH',
	'@BUILD_DATE', '@BUILD_TIME', '@BUILD_TIMESTAMP']

// repro_uses_machine_pseudo reports whether a declaration uses one of the machine- or
// layout-dependent pseudo variables above. Occurrences are matched with an identifier boundary,
// so `bob@FILEserver` inside a string does not count, while a real `@FILE` token does; a stray
// mention in a plain string only causes a harmless fallback.
fn repro_uses_machine_pseudo(source string) bool {
	mut i := 0
	for i < source.len {
		if source[i] != `@` {
			i++
			continue
		}
		for p in c_error_repro_machine_pseudos {
			if i + p.len <= source.len && source[i..i + p.len] == p
				&& (i + p.len == source.len || !is_ident_char(source[i + p.len])) {
				return true
			}
		}
		i++
	}
	return false
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
				if ci == `/` && i + 1 < source.len && source[i + 1] == `/` {
					// a line comment inside the interpolation: skip it so `}` in the comment text
					// does not close the scan
					for i < source.len && source[i] != `\n` {
						i++
					}
				} else if ci == `/` && i + 1 < source.len && source[i + 1] == `*` {
					// a block comment: skip to its `*/`
					i += 2
					for i + 1 < source.len && !(source[i] == `*` && source[i + 1] == `/`) {
						i++
					}
					i += 2
				} else if ci == 96 {
					// a backtick rune literal (e.g. `` `}` ``): skip it so its brace is not counted
					i++
					for i < source.len && source[i] != 96 {
						i += if source[i] == `\\` { 2 } else { 1 }
					}
					i++
				} else if ci == `'` || ci == `"` {
					// a nested string inside the interpolation: skip its contents (its own braces
					// must not be counted) while still collecting its `${...}` identifiers
					i = scan_string_literal(source, i, mut ids)
				} else if ci == `{` {
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

// repro_source_fn_name parses the function name out of a declaration's copied source text. The
// AST name can differ from the source when a pass rewrites declarations in place — notably
// `-new-generic-solver`, whose concrete clones are named `foo_T_int` while the copied source and
// every call site still say `foo` — so the closure indexes the source-level name as well.
// Returns '' when the declaration has no source-visible plain name (operator overloads), or when
// no `fn` line is found.
fn repro_source_fn_name(source string) string {
	// skip leading whitespace, comments and attribute groups; attributes are scanned as bracket
	// groups with string-aware depth, so a multiline value (`@[footer: 'Hello\nWorld']`) is
	// skipped whole instead of its continuation line being mistaken for the declaration
	mut i := 0
	for i < source.len {
		c := source[i]
		if c == ` ` || c == `\t` || c == `\n` || c == `\r` {
			i++
			continue
		}
		if c == `/` && i + 1 < source.len && source[i + 1] == `/` {
			for i < source.len && source[i] != `\n` {
				i++
			}
			continue
		}
		if c == `@` && i + 1 < source.len && source[i + 1] == `[` {
			i += 2
			mut depth := 1
			for i < source.len && depth > 0 {
				ch := source[i]
				if ch == `'` || ch == `"` {
					i++
					for i < source.len && source[i] != ch {
						i += if source[i] == `\\` { 2 } else { 1 }
					}
					i++
					continue
				}
				if ch == `[` {
					depth++
				} else if ch == `]` {
					depth--
				}
				i++
			}
			continue
		}
		break
	}
	mut rest := source[i..]
	if rest.starts_with('pub ') {
		rest = rest['pub '.len..].trim_left(' \t')
	}
	if !rest.starts_with('fn ') && !rest.starts_with('fn(') {
		return ''
	}
	mut j := 2
	for j < rest.len && (rest[j] == ` ` || rest[j] == `\t`) {
		j++
	}
	// skip a method's parenthesized receiver
	if j < rest.len && rest[j] == `(` {
		for j < rest.len && rest[j] != `)` {
			j++
		}
		j++
		for j < rest.len && (rest[j] == ` ` || rest[j] == `\t`) {
			j++
		}
	}
	// the name: an identifier, possibly dotted (`C.puts`, `JS.alert`); keep the last segment
	// to match how AST names are indexed (repro_short_name)
	mut last := ''
	for {
		name_start := j
		for j < rest.len && is_ident_char(rest[j]) {
			j++
		}
		if j == name_start {
			return ''
		}
		last = rest[name_start..j]
		if j < rest.len && rest[j] == `.` {
			j++
			continue
		}
		break
	}
	return last
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

// repro_comptime_decl_names returns the names declared inside a top-level comptime `$if` or
// `$match` block, so the whole block is inlined when one of its declarations is referenced. Such a
// block appears either as an `ExprStmt` holding an `IfExpr`/`MatchExpr` (unsolved) or, after
// `comptime.solve_files` hoists an active single branch, as a bare `ast.Block`. Returns [] otherwise.
fn repro_comptime_decl_names(stmt ast.Stmt) []string {
	if stmt is ast.Block {
		return repro_comptime_branch_names(stmt.stmts)
	}
	if stmt is ast.ExprStmt {
		if stmt.expr is ast.IfExpr {
			if stmt.expr.is_comptime {
				mut names := []string{}
				for branch in stmt.expr.branches {
					names << repro_comptime_branch_names(branch.stmts)
				}
				return names
			}
		} else if stmt.expr is ast.MatchExpr {
			if stmt.expr.is_comptime {
				mut names := []string{}
				for branch in stmt.expr.branches {
					names << repro_comptime_branch_names(branch.stmts)
				}
				return names
			}
		}
	}
	return []
}

// repro_comptime_branch_names collects the top-level declaration names in a comptime branch body,
// recursing into any further nested comptime blocks.
fn repro_comptime_branch_names(stmts []ast.Stmt) []string {
	mut names := []string{}
	for s in stmts {
		inner := repro_top_decl_names(s)
		if inner.len > 0 {
			names << inner
		} else {
			names << repro_comptime_decl_names(s)
		}
	}
	return names
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
