module driver

import os
import strings
import strconv
import v.cmdexec
import v.workers

// The native toolchain supplies declaration and macro manifests. V never opens
// native headers or parses their C/Objective-C text. An unavailable or incomplete
// manifest keeps the program in one translation unit.
struct V3ParallelCHeaderHazard {
	file   string
	line   int
	reason string
	// Only unreferenced, nonemitted constants can be cleared by a body probe.
	unused_constant string
}

fn (h V3ParallelCHeaderHazard) str() string {
	return '${h.file}:${h.line}: ${h.reason}'
}

// Only the fields needed from Clang's JSON declaration manifest are decoded.
struct V3ParallelCDeclLocation {
	file string
	line int
}

struct V3ParallelCDeclType {
	qual_type string
}

struct V3ParallelCDecl {
	kind            string
	name            string
	mangled_name    string
	storage_class   string
	has_initializer bool
	is_inline       bool
	is_referenced   bool
	loc             V3ParallelCDeclLocation
	typ             V3ParallelCDeclType
	inner           []V3ParallelCDecl
}

struct V3ParallelCManifest {
mut:
	declared              map[string]bool
	emitted_symbols       map[string]bool
	has_emission_metadata bool
	macros                map[string]string
	hazards               []V3ParallelCHeaderHazard
}

struct V3ParallelCManifestTask {
	compiler     string
	flags        []string
	objective_c  bool
	path         string
	source       string
	generated    []string
	used         map[string]bool
	check_header bool
mut:
	manifest V3ParallelCManifest
	failure  string
}

fn v3_parallel_c_check_native_code(c_compiler string, flags []string, objective_c bool, header_path string, check_header bool, owner_source string, parts []string, is_shared bool, mut pool workers.Pool) ([]V3ParallelCHeaderHazard, map[string]bool) {
	if !check_header {
		return []V3ParallelCHeaderHazard{}, map[string]bool{}
	}
	used := v3_c_used_identifiers(parts)
	mut header := &V3ParallelCManifestTask{
		compiler:     c_compiler
		flags:        flags
		objective_c:  objective_c
		path:         header_path + '.unit.c'
		source:       v3_parallel_c_unit_source(header_path, '', false, is_shared)
		generated:    [header_path, header_path + '.unit.c']
		used:         used
		check_header: true
	}
	mut owner := &V3ParallelCManifestTask{
		compiler:    c_compiler
		flags:       flags
		objective_c: objective_c
		path:        header_path + '.owner.c'
		source:      owner_source
		used:        used
	}
	pool.run([
		workers.Task{ run: run_v3_parallel_c_manifest_task, arg: voidptr(header) },
		workers.Task{ run: run_v3_parallel_c_manifest_task, arg: voidptr(owner) },
	])
	mut hazards := header.manifest.hazards.clone()
	candidates := hazards.map(it.unused_constant).filter(it.len > 0)
	if candidates.len > 0 && hazards.all(it.unused_constant.len > 0)
		&& v3_parallel_c_constants_unused(c_compiler, flags, objective_c,
			header_path, header.source, candidates, parts) {
		hazards = hazards.filter(it.unused_constant.len == 0)
	}
	for task in [header, owner] {
		if task.failure.len > 0 {
			hazards << V3ParallelCHeaderHazard{ file: task.path, reason: task.failure }
		}
	}
	return hazards, v3_parallel_c_owner_names(owner.manifest, header.manifest)
}

fn run_v3_parallel_c_manifest_task(raw_task voidptr) voidptr {
	mut task := unsafe { &V3ParallelCManifestTask(raw_task) }
	task.manifest = v3_parallel_c_native_manifest(task.compiler, task.flags, task.objective_c,
		task.source, task.path, task.generated, task.used, task.check_header) or {
		task.failure = 'native toolchain could not establish parallel C safety: ${err.msg()}'
		V3ParallelCManifest{}
	}
	return unsafe { nil }
}

fn v3_parallel_c_native_manifest(compiler string, flags []string, objective_c bool, source string, path string, generated []string, used map[string]bool, check_header bool) !V3ParallelCManifest {
	os.write_file(path, source)!
	defer { os.rm(path) or {} }
	mut args := flags.clone()
	args << ['-w', '-x', if objective_c { 'objective-c' } else { 'c' }]
	mut decl_args := args.clone()
	decl_args << ['-fsyntax-only', '-Xclang', '-ast-dump=json', path]
	declarations := cmdexec.run_in(compiler, decl_args, os.dir(path))
	if declarations.exit_code != 0 {
		return error(declarations.output)
	}
	mut reader := V3ParallelCManifestReader{ text: declarations.output }
	root := reader.declaration()!
	reader.space()
	if reader.pos != reader.text.len { return error('trailing native declaration metadata') }
	if root.kind != 'TranslationUnitDecl' {
		return error('missing translation-unit declaration manifest')
	}
	mut manifest := V3ParallelCManifest{}
	if check_header && v3_parallel_c_needs_symbols(root) {
		manifest.emitted_symbols = v3_parallel_c_native_symbols(compiler, args, path)!
		manifest.has_emission_metadata = true
	}
	mut file := ''
	mut generated_paths := generated.clone()
	generated_paths << generated.map(os.real_path(it))
	for declaration in root.inner {
		file = manifest.collect_declaration(declaration, used, generated_paths, check_header, file)
	}
	args << ['-E', '-dM', path]
	macros := cmdexec.run_in(compiler, args, os.dir(path))
	if macros.exit_code != 0 {
		return error(macros.output)
	}
	manifest.macros = v3_parallel_c_macro_manifest(macros.output, used)
	return manifest
}

// Probe headers with ambiguous external inline bodies or constant objects. The C compiler,
// including its selected inline flags and redeclarations, decides which symbols
// are defined. No native header text or compiler IR is inspected by V.
fn v3_parallel_c_needs_symbols(decl V3ParallelCDecl) bool {
	if decl.kind == 'VarDecl' && decl.storage_class == 'static'
		&& v3_parallel_c_const_object_type(decl.typ.qual_type) {
		return true
	}
	if decl.kind == 'FunctionDecl' && decl.is_inline && decl.storage_class != 'static'
		&& decl.inner.any(it.kind == 'CompoundStmt')
		&& !decl.inner.any(it.kind == 'WeakAttr') {
		return true
	}
	if decl.kind in ['TranslationUnitDecl', 'LinkageSpecDecl'] {
		for child in decl.inner {
			if v3_parallel_c_needs_symbols(child) { return true }
		}
	}
	return false
}

fn v3_parallel_c_native_symbols(compiler string, flags []string, path string) !map[string]bool {
	object := path + '.symbols.o'
	defer { os.rm(object) or {} }
	mut args := flags.clone()
	args << ['-c', '-o', object, path]
	compiled := cmdexec.run_in(compiler, args, os.dir(path))
	if compiled.exit_code != 0 {
		return error('native symbol probe failed: ${compiled.output}')
	}
	nm := os.find_abs_path_of_executable('llvm-nm') or {
		os.find_abs_path_of_executable('nm') or {
			return error('native symbol metadata requires llvm-nm or nm')
		}
	}
	result := cmdexec.run(nm, ['-P', object])
	if result.exit_code != 0 {
		return error('native symbol metadata failed: ${result.output}')
	}
	mut symbols := map[string]bool{}
	for line in result.output.split_into_lines() {
		if line.trim_space().len == 0 { continue }
		fields := line.fields()
		if fields.len < 2 || fields.len > 4 || fields[1].len != 1 {
			return error('unrecognized native symbol metadata')
		}
		if fields[1] in ['U', 'w', 'v'] { continue }
		if fields.len < 3 || !fields[2].bytes().all(it.is_hex_digit())
			|| (fields.len == 4 && !fields[3].bytes().all(it.is_hex_digit())) {
			return error('incomplete native symbol metadata')
		}
		symbols[fields[0].clone()] = true
	}
	return symbols
}

// Ask the preprocessor whether any generated body can name these constants,
// including through native macros or token pasting. The error macro catches
// expansions of macros defined before GCC poison (which poison alone exempts).
// Poison also prevents later undef/redefinition from hiding an access. V neither
// expands macros nor inspects the preprocessed native text.
fn v3_parallel_c_constants_unused(compiler string, flags []string, objective_c bool, header_path string, prefix string, names []string, parts []string) bool {
	path := header_path + '.constants.c'
	defer { os.rm(path) or {} }
	mut source := strings.new_builder(prefix.len)
	source.write_string(prefix)
	for name in names {
		source.writeln('#define ${name} _Pragma("GCC error \\"native constant used\\"")')
		source.writeln('#pragma GCC poison ${name}')
	}
	for part in parts { source.writeln(part) }
	os.write_file(path, source.str()) or { return false }
	mut args := flags.clone()
	args << ['-w', '-x', if objective_c { 'objective-c' } else { 'c' }, '-E', '-o', os.path_devnull,
		path]
	return cmdexec.run_in(compiler, args, os.dir(path)).exit_code == 0
}

fn (mut manifest V3ParallelCManifest) collect_declaration(decl V3ParallelCDecl, used map[string]bool, generated []string, check_header bool, previous_file string) string {
	mut file := if decl.loc.file.len > 0 { decl.loc.file } else { previous_file }
	if decl.name in used && decl.kind !in ['FieldDecl', 'IndirectFieldDecl'] {
		manifest.declared[decl.name.clone()] = true
	}
	// Enum constants and anonymous record typedefs are nested declarations;
	// function-local names must not enter the set visible to other units.
	if decl.kind in ['LinkageSpecDecl', 'EnumDecl', 'RecordDecl'] {
		for child in decl.inner {
			file = manifest.collect_declaration(child, used, generated, check_header, file)
		}
	}
	if !check_header || decl.kind.ends_with('Attr') || decl.kind.ends_with('Comment') {
		return file
	}
	// These generated assert-printer helpers deliberately use per-unit scratch
	// buffers; their callers consume the text immediately. Native header helpers
	// with static locals still require one owner.
	if file in generated && decl.kind == 'FunctionDecl'
		&& decl.name in ['__v_u128_str', '__v_i128_str'] {
		return file
	}
	// V constants and literal objects in the compiler-owned prefix retain the
	// existing generated-C value semantics. Native header objects, even const
	// ones, must keep a single identity.
	if decl.kind == 'VarDecl' && file in generated
		&& (v3_parallel_c_const_object_type(decl.typ.qual_type)
			|| decl.name.starts_with('_v3_lit_') || decl.name.starts_with('_str_')) {
		return file
	}
	mut reason := ''
	mut unused_constant := ''
	match decl.kind {
		'VarDecl' {
			if decl.storage_class == 'static' {
				reason = if v3_parallel_c_const_object_type(decl.typ.qual_type) {
					'static constant `${decl.name}` would get a separate identity in every unit'
				} else {
					'mutable static `${decl.name}` would get a separate copy in every unit'
				}
				if v3_parallel_c_const_object_type(decl.typ.qual_type) && !decl.is_referenced
					&& decl.name.len > 0 && decl.name !in used
					&& manifest.has_emission_metadata && decl.mangled_name.len > 0
					&& decl.mangled_name !in manifest.emitted_symbols {
					unused_constant = decl.name.clone()
				}
			} else if (decl.storage_class != 'extern' || decl.has_initializer
				|| decl.inner.any(it.kind == 'AliasAttr'))
				&& !decl.inner.any(it.kind == 'WeakAttr') {
				reason = 'external object `${decl.name}` would be defined in every unit'
			}
		}
		'FunctionDecl' {
			if decl.inner.any(it.kind in ['CompoundStmt', 'AliasAttr', 'IFuncAttr']) {
				// C99/GNU modes, attributes and redeclarations affect inline emission.
				// Match the AST's native symbol name against the toolchain's probe
				// object instead of guessing linkage from extern/inline specifiers.
				if decl.storage_class != 'static' && !decl.inner.any(it.kind == 'WeakAttr')
					&& (!decl.is_inline || !manifest.has_emission_metadata
						|| decl.mangled_name.len == 0 || decl.mangled_name in manifest.emitted_symbols) {
					reason = 'external function `${decl.name}` would be defined in every unit'
				} else if local := v3_parallel_c_static_local(decl) {
					reason = '`${decl.name}` has the static local `${local}`, which every unit would copy'
				} else if decl.inner.any(it.kind in ['ConstructorAttr', 'DestructorAttr']) {
					reason = '`${decl.name}` would run a native lifecycle hook in every unit'
				}
			}
		}
		'ObjCImplementationDecl', 'ObjCCategoryImplDecl' {
			reason = 'Objective-C @implementation `${decl.name}` would be defined in every unit'
		}
		'FileScopeAsmDecl' {
			reason = 'native file-scope assembly cannot be repeated safely'
		}
		else {
			if decl.kind !in ['TypedefDecl', 'TypeAliasDecl', 'RecordDecl', 'EnumDecl',
				'EnumConstantDecl', 'FieldDecl', 'IndirectFieldDecl', 'ObjCInterfaceDecl',
				'ObjCProtocolDecl', 'ObjCCategoryDecl', 'ObjCCompatibleAliasDecl', 'EmptyDecl',
				'StaticAssertDecl', 'ImportDecl', 'LinkageSpecDecl'] {
				reason = 'unsupported native declaration metadata `${decl.kind}`'
			}
		}
	}
	if reason.len > 0 {
		manifest.hazards << V3ParallelCHeaderHazard{
			file:            file.clone()
			line:            decl.loc.line
			reason:          reason
			unused_constant: unused_constant
		}
	}
	return file
}

// qual_type is compiler metadata, never text read from a C header. Const data
// behind a mutable pointer does not make the pointer itself read-only.
fn v3_parallel_c_const_object_type(qual_type string) bool {
	// Function-pointer parameter qualifiers do not qualify the stored pointer.
	if qual_type.contains('(') || qual_type.contains('^') {
		return false
	}
	return qual_type.all_after_last('*').replace('[', ' ').replace(']', ' ').fields().contains('const')
}

fn v3_parallel_c_static_local(decl V3ParallelCDecl) ?string {
	if decl.kind == 'VarDecl' && decl.storage_class == 'static' {
		return decl.name
	}
	for child in decl.inner {
		if name := v3_parallel_c_static_local(child) { return name }
	}
	return none
}

// -dM is a compiler-produced macro table, with no expanded header declarations.
fn v3_parallel_c_macro_manifest(text string, used map[string]bool) map[string]string {
	mut macros := map[string]string{}
	for line in text.split_into_lines() {
		if !line.starts_with('#define ') { continue }
		definition := line['#define '.len..]
		mut end := 0
		for end < definition.len && (definition[end].is_alnum() || definition[end] == `_`) { end++ }
		name := definition[..end]
		if name in used { macros[name.clone()] = definition.clone() }
	}
	return macros
}

fn v3_parallel_c_owner_names(owner V3ParallelCManifest, header V3ParallelCManifest) map[string]bool {
	mut names := map[string]bool{}
	for name, _ in owner.declared {
		if name !in header.declared { names[name] = true }
	}
	for name, definition in owner.macros {
		if header.macros[name] != definition { names[name] = true }
	}
	for name, _ in header.macros {
		if name !in owner.macros { names[name] = true }
	}
	return names
}

// v3_c_used_identifiers returns the identifiers that C text uses outside its
// comments and literals.
fn v3_c_used_identifiers(parts []string) map[string]bool {
	mut used := map[string]bool{}
	for text in parts {
		mut i := 0
		for i < text.len {
			c := text[i]
			if c == `/` {
				after := v3_c_skip_comment(text, i)
				i = if after > i { after } else { i + 1 }
			} else if c in [`"`, `\'`] {
				i = v3_c_skip_literal(text, i)
			} else if c.is_digit() {
				i = v3_c_skip_number(text, i)
			} else if c == `_` || c.is_letter() {
				end := v3_c_identifier_end(text, i)
				name := text.substr_unsafe(i, end)
				if name !in used {
					used[name.clone()] = true
				}
				i = end
			} else {
				i++
			}
		}
	}
	return used
}

// v3_c_partition_items splits generated body C into its top-level items and
// returns, in their original order, the items that use one of `names` and the
// others. An item runs up to the `;` or closing `}` that ends it at the top level,
// with the comments and blanks before it. The generated body holds functions that
// the declaration header declares, so they can be compiled in any unit.
fn v3_c_partition_items(text string, names map[string]bool) (string, string) {
	// Keep generated conditional groups intact when a native caller moves.
	if text.split_into_lines().any(it.trim_space().starts_with('#')) {
		return if v3_c_text_uses_any(text, names) { text } else { '' }, if v3_c_text_uses_any(text, names) {
			''
		} else {
			text
		}
	}
	mut owner := strings.new_builder(0)
	mut rest := strings.new_builder(text.len)
	mut start := 0
	mut depth := 0
	mut i := 0
	for i < text.len {
		c := text[i]
		if c == `/` {
			after := v3_c_skip_comment(text, i)
			if after > i {
				i = after
				continue
			}
		} else if c in [`"`, `\'`] {
			i = v3_c_skip_literal(text, i)
			continue
		} else if c == `{` {
			depth++
		} else if c == `}` {
			depth--
		}
		i++
		if depth == 0 && (c == `}` || c == `;`) {
			item := text.substr_unsafe(start, i)
			if v3_c_text_uses_any(item, names) {
				owner.write_string(item)
				owner.write_u8(`\n`)
			} else {
				rest.write_string(item)
			}
			start = i
		}
	}
	rest.write_string(text.substr_unsafe(start, text.len))
	return owner.str(), rest.str()
}

// v3_c_text_uses_any reports whether C text uses an identifier from `names`
// outside its comments and literals.
fn v3_c_text_uses_any(text string, names map[string]bool) bool {
	if names.len == 0 {
		return false
	}
	mut i := 0
	for i < text.len {
		c := text[i]
		if c == `/` {
			after := v3_c_skip_comment(text, i)
			i = if after > i { after } else { i + 1 }
		} else if c in [`"`, `\'`] {
			i = v3_c_skip_literal(text, i)
		} else if c.is_digit() {
			i = v3_c_skip_number(text, i)
		} else if c == `_` || c.is_letter() {
			end := v3_c_identifier_end(text, i)
			if text.substr_unsafe(i, end) in names {
				return true
			}
			i = end
		} else {
			i++
		}
	}
	return false
}

// Compiler self-builds deliberately avoid generic instantiation. Decode only
// the required JSON fields here, without importing a generic JSON serializer.
struct V3ParallelCManifestReader {
	text string
mut:
	pos   int
	depth int
}

fn (mut r V3ParallelCManifestReader) space() {
	for r.pos < r.text.len && r.text[r.pos] in [` `, `\t`, `\r`, `\n`] { r.pos++ }
}

fn (mut r V3ParallelCManifestReader) take(c u8) ! {
	r.space()
	if r.pos >= r.text.len || r.text[r.pos] != c { return error('invalid native declaration JSON') }
	r.pos++
}

fn (mut r V3ParallelCManifestReader) quoted() !string {
	r.space()
	start := r.pos
	r.take(`"`)!
	mut escaped := false
	for r.pos < r.text.len {
		c := r.text[r.pos]
		r.pos++
		if c == `"` {
			if !escaped {
				// The decoder owns its input throughout declaration collection. Borrow
				// ordinary JSON strings; only names and paths retained in the manifest
				// are cloned. This keeps native analysis within small memory budgets.
				return r.text.substr_unsafe(start + 1, r.pos - 1)
			}
			return strconv.unquote(r.text.substr_unsafe(start, r.pos))!
		}
		if c < ` ` { return error('invalid control character in native metadata string') }
		if c == `\\` {
			escaped = true
			r.pos++
		}
	}
	return error('unterminated native metadata string')
}

fn (mut r V3ParallelCManifestReader) skip() ! {
	r.space()
	if r.pos >= r.text.len { return error('incomplete native declaration JSON') }
	c := r.text[r.pos]
	if c == `"` {
		r.quoted()!
		return
	}
	if c in [`{`, `[`] {
		r.depth++
		if r.depth > 256 { return error('native declaration metadata is too deeply nested') }
		r.pos++
		close := if c == `{` { `}` } else { `]` }
		for {
			r.space()
			if r.pos >= r.text.len { return error('incomplete native metadata container') }
			if r.text[r.pos] == close {
				r.pos++
				r.depth--
				return
			}
			if c == `{` {
				r.quoted()!
				r.take(`:`)!
			}
			r.skip()!
			r.space()
			if r.pos < r.text.len && r.text[r.pos] == `,` {
				r.pos++
				continue
			}
			r.take(close)!
			r.depth--
			return
		}
	}
	start := r.pos
	for r.pos < r.text.len && r.text[r.pos] !in [`,`, `}`, `]`, ` `, `\t`, `\r`, `\n`] { r.pos++ }
	if r.pos == start { return error('invalid native metadata value') }
}

fn (mut r V3ParallelCManifestReader) declaration() !V3ParallelCDecl {
	r.depth++
	if r.depth > 256 { return error('native declaration metadata is too deeply nested') }
	r.take(`{`)!
	mut kind := ''
	mut name := ''
	mut mangled_name := ''
	mut is_inline := false
	mut is_referenced := false
	mut storage := ''
	mut has_initializer := false
	mut file := ''
	mut line := 0
	mut qual_type := ''
	mut loc := V3ParallelCDeclLocation{}
	mut typ := V3ParallelCDeclType{}
	mut inner := []V3ParallelCDecl{}
	for {
		r.space()
		if r.pos >= r.text.len { return error('incomplete native declaration') }
		if r.text[r.pos] == `}` {
			r.pos++
			break
		}
		key := r.quoted()!
		r.take(`:`)!
		match key {
			'kind' { kind = r.quoted()! }
			'name' { name = r.quoted()! }
			'mangledName' { mangled_name = r.quoted()! }
			'inline' {
				r.space()
				is_inline = r.pos < r.text.len && r.text[r.pos] == `t`
				r.skip()!
			}
			'isUsed', 'isReferenced' {
				r.space()
				is_referenced = is_referenced || (r.pos < r.text.len && r.text[r.pos] == `t`)
				r.skip()!
			}
			'storageClass' { storage = r.quoted()! }
			'init' {
				has_initializer = true
				r.skip()!
			}
			'file' { file = r.quoted()! }
			'line' {
				r.space()
				start := r.pos
				r.skip()!
				line = r.text[start..r.pos].int()
			}
			'spellingLoc', 'expansionLoc' {
				// Macro-produced declarations retain the expansion file's ownership.
				item := r.declaration()!
				if item.loc.file.len > 0 { file = item.loc.file }
				if item.loc.line > 0 { line = item.loc.line }
			}
			'qualType' { qual_type = r.quoted()! }
			'loc' {
				item := r.declaration()!
				loc = V3ParallelCDeclLocation{ file: item.loc.file, line: item.loc.line }
			}
			'type' {
				item := r.declaration()!
				typ = item.typ
			}
			'inner' {
				r.take(`[`)!
				for {
					r.space()
					if r.pos >= r.text.len { return error('incomplete native declaration array') }
					if r.text[r.pos] == `]` {
						r.pos++
						break
					}
					inner << r.declaration()!
					r.space()
					if r.pos < r.text.len && r.text[r.pos] == `,` {
						r.pos++
					} else {
						r.take(`]`)!
						break
					}
				}
			}
			else { r.skip()! }
		}
		r.space()
		if r.pos < r.text.len && r.text[r.pos] == `,` {
			r.pos++
		} else {
			r.take(`}`)!
			break
		}
	}
	r.depth--
	return V3ParallelCDecl{
		kind:            kind
		name:            name
		mangled_name:    mangled_name
		is_inline:       is_inline
		is_referenced:   is_referenced
		storage_class:   storage
		has_initializer: has_initializer
		loc:             if file.len > 0 || line > 0 {
			V3ParallelCDeclLocation{ file: file, line: line }
		} else {
			loc
		}
		typ:             if qual_type.len > 0 {
			V3ParallelCDeclType{ qual_type: qual_type }
		} else {
			typ
		}
		inner:           inner
	}
}
