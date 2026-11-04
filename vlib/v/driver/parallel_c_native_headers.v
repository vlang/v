module driver

import os
import strings
import v.cmdexec
import v.workers

// V3ParallelCHeaderHazard is an item of a native header that would behave
// differently once the declaration header is compiled into every parallel C unit.
struct V3ParallelCHeaderHazard {
	file   string
	line   int
	reason string
}

fn (h V3ParallelCHeaderHazard) str() string {
	return '${h.file}:${h.line}: ${h.reason}'
}

const v3_parallel_c_hazard_limit = 4096

// v3_parallel_c_check_native_code preprocesses the first unit and the start of
// the units after it (the declaration header) with the flags they are compiled
// with, and returns what keeps the program in one unit, together with the names
// that the body has to be compiled in the first unit to see. The preprocessing,
// and then the scans, run side by side on `pool`.
//
// - With `check_header`, the quoted native headers that every unit compiles with
//   the declaration header must hold no storage or definitions of their own; see
//   v3_parallel_c_preprocessed_hazards.
// - Only the first unit compiles the native source directives (Objective-C
//   sources), the late directives, and the implementation sections that defines
//   such as `SOKOL_APP_IMPL` select; the declaration header leaves them out. What
//   they declare, and the macros whose definition they change, are owner names:
//   the body functions that use one are compiled in the first unit.
// A check that cannot run counts as a hazard.
fn v3_parallel_c_check_native_code(c_compiler string, flags []string, objective_c bool, header_path string, check_header bool, owner_source string, parts []string, is_shared bool, mut pool workers.Pool) ([]V3ParallelCHeaderHazard, map[string]bool) {
	mut analysis := &V3ParallelCAnalysis{
		c_compiler:    c_compiler
		flags:         flags
		objective_c:   objective_c
		header_path:   header_path
		header_source: v3_parallel_c_unit_source(header_path, '', false, is_shared)
		owner_source:  owner_source
		parts:         parts
		check_header:  check_header
	}
	mut first := []workers.Task{}
	mut second := []workers.Task{}
	for kind in [V3ParallelCAnalysisKind.preprocess_header, .preprocess_owner, .used_identifiers] {
		first << workers.Task{
			run: run_v3_parallel_c_analysis_task
			arg: voidptr(&V3ParallelCAnalysisTask{
				kind:     kind
				analysis: analysis
			})
		}
	}
	for kind in [V3ParallelCAnalysisKind.scan_header, .scan_owner] {
		second << workers.Task{
			run: run_v3_parallel_c_analysis_task
			arg: voidptr(&V3ParallelCAnalysisTask{
				kind:     kind
				analysis: analysis
			})
		}
	}
	pool.run(first)
	if analysis.header_failure.len == 0 && analysis.owner_failure.len == 0 {
		pool.run(second)
	}
	mut hazards := analysis.header.hazards.clone()
	for failure in [analysis.header_failure, analysis.owner_failure] {
		if failure.len > 0 {
			hazards << V3ParallelCHeaderHazard{
				file:   header_path
				reason: failure
			}
		}
	}
	return hazards, v3_parallel_c_owner_names(analysis.owner, analysis.header)
}

enum V3ParallelCAnalysisKind {
	preprocess_header
	preprocess_owner
	used_identifiers
	scan_header
	scan_owner
}

// V3ParallelCAnalysis is the state that the tasks of v3_parallel_c_check_native_code
// share; each task writes its own fields.
struct V3ParallelCAnalysis {
	c_compiler    string
	flags         []string
	objective_c   bool
	header_path   string
	header_source string
	owner_source  string
	parts         []string
	check_header  bool
mut:
	header_preprocessed string
	owner_preprocessed  string
	header_failure      string
	owner_failure       string
	used                map[string]bool
	header              V3ParallelCScan
	owner               V3ParallelCScan
}

struct V3ParallelCAnalysisTask {
	kind     V3ParallelCAnalysisKind
	analysis &V3ParallelCAnalysis
}

fn run_v3_parallel_c_analysis_task(raw_task voidptr) voidptr {
	task := unsafe { &V3ParallelCAnalysisTask(raw_task) }
	mut a := unsafe { task.analysis }
	match task.kind {
		.preprocess_header {
			a.header_preprocessed = v3_parallel_c_preprocess(a.c_compiler, a.flags, a.objective_c,
				a.header_source, a.header_path + '.unit.c') or {
				a.header_failure = 'failed to preprocess the parallel C header: ${err.msg()}'
				''
			}
		}
		.preprocess_owner {
			a.owner_preprocessed = v3_parallel_c_preprocess(a.c_compiler, a.flags, a.objective_c,
				a.owner_source, a.header_path + '.owner.c') or {
				a.owner_failure = 'failed to preprocess the first parallel C unit: ${err.msg()}'
				''
			}
		}
		.used_identifiers {
			a.used = v3_c_used_identifiers(a.parts)
		}
		.scan_header {
			a.header = V3ParallelCScan{
				generated:       [a.header_path, a.header_path + '.unit.c']
				generated_real:  [os.real_path(a.header_path), os.real_path(a.header_path + '.unit.c')]
				collect_hazards: a.check_header
			}
			a.header.run(a.header_preprocessed, a.used)
		}
		.scan_owner {
			a.owner = V3ParallelCScan{}
			a.owner.run(a.owner_preprocessed, a.used)
		}
	}
	return unsafe { nil }
}

// v3_parallel_c_preprocess preprocesses a unit, keeping its macro definitions.
fn v3_parallel_c_preprocess(c_compiler string, compile_flags []string, objective_c bool, unit_source string, unit_path string) !string {
	preprocessed := unit_path + '.i'
	os.write_file(unit_path, unit_source)!
	defer {
		os.rm(unit_path) or {}
		os.rm(preprocessed) or {}
	}
	mut args := compile_flags.clone()
	args << ['-E', '-dD', '-x', if objective_c { 'objective-c' } else { 'c' }, '-o', preprocessed,
		unit_path]
	result := cmdexec.run_in(c_compiler, args, os.dir(unit_path))
	if result.exit_code != 0 {
		return error(result.output)
	}
	return os.read_file(preprocessed)!
}

// v3_parallel_c_owner_names returns the names that the first unit declares and
// the declaration header does not, and the macros whose definition at the end
// of the two differs. Both scans only record the names that the body uses.
fn v3_parallel_c_owner_names(owner V3ParallelCScan, header V3ParallelCScan) map[string]bool {
	mut names := map[string]bool{}
	for name, _ in owner.declared {
		if name !in header.declared {
			names[name] = true
		}
	}
	for name, definition in owner.macros {
		if header.macros[name] != definition {
			names[name] = true
		}
	}
	for name, _ in header.macros {
		if name !in owner.macros {
			names[name] = true
		}
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

// v3_parallel_c_preprocessed_hazards scans preprocessed C (with line markers) and
// reports the items, from headers other than system headers and the `generated` files,
// that would change the program if every unit compiled them:
// - mutable `static` objects, which every unit would get its own copy of,
// - `static` locals of functions, which every unit would also copy,
// - external definitions of functions or objects, which every unit would define,
// - Objective-C `@implementation` blocks, which would define a class per unit.
// Types, declarations, read-only `static` data and functions without `static`
// locals are safe to repeat, and so are items of the generated header, whose
// native directive text is checked separately.
fn v3_parallel_c_preprocessed_hazards(source string, generated []string) []V3ParallelCHeaderHazard {
	mut scan := V3ParallelCScan{
		generated:       generated
		generated_real:  generated.map(os.real_path(it))
		collect_hazards: true
	}
	scan.run(source, map[string]bool{})
	return scan.hazards
}

// V3ParallelCItem is a top-level item of preprocessed C being scanned, with the
// file and line it starts at.
struct V3ParallelCItem {
mut:
	text   strings.Builder
	file   string
	line   int
	user   bool
	objc   bool
	active bool
	// head_end is the length of the text before the first top-level `{`, or -1.
	head_end int = -1
}

// V3ParallelCScan is one pass over preprocessed C. It collects the replication
// hazards of the program's own items with `collect_hazards`, and the names that
// items declare and the macro definitions, of the identifiers in the `used` map
// that `run` gets.
struct V3ParallelCScan {
	generated       []string
	generated_real  []string
	collect_hazards bool
mut:
	hazards  []V3ParallelCHeaderHazard
	declared map[string]bool
	macros   map[string]string
	resolved map[string]bool
}

// is_generated_path compares a file of a line marker with the generated files
// after resolving links, once per file name.
fn (mut scan V3ParallelCScan) is_generated_path(file string) bool {
	if scan.generated_real.len == 0 {
		return false
	}
	if known := scan.resolved[file] {
		return known
	}
	generated := os.real_path(file) in scan.generated_real
	scan.resolved[file] = generated
	return generated
}

fn (mut scan V3ParallelCScan) run(source string, used map[string]bool) {
	mut file := ''
	mut line := 0
	mut user := false
	mut depth := 0
	mut in_string := false
	mut in_char := false
	mut escaped := false
	mut item := V3ParallelCItem{
		text: strings.new_builder(256)
	}
	mut pos := 0
	for pos < source.len {
		mut line_end := source.index_after('\n', pos) or { source.len }
		// The line is only read while it is scanned; items copy what they keep.
		raw := source.substr_unsafe(pos, line_end)
		pos = line_end + 1
		if !in_string && !in_char && raw.len > 0 && raw[0] == `#` {
			if marker_file, marker_line, marker_flags := v3_c_line_marker(raw) {
				file = marker_file
				line = marker_line - 1
				user = !marker_flags.contains('3') && !file.starts_with('<')
					&& file !in scan.generated && !scan.is_generated_path(file)
				continue
			}
			// Pragmas and other directives have no storage; `-dD` keeps the macros.
			if used.len > 0 {
				scan.record_macro(raw, used)
			}
			continue
		}
		line++
		for i := 0; i < raw.len; i++ {
			c := raw[i]
			if !item.active {
				if c in [` `, `\t`, `\r`] {
					continue
				}
				item.active = true
				item.file = file
				item.line = line
				item.user = user
				item.head_end = -1
				item.objc = false
			}
			item.text.write_u8(c)
			if in_string {
				if escaped {
					escaped = false
				} else if c == `\\` {
					escaped = true
				} else if c == `"` {
					in_string = false
				}
				continue
			}
			if in_char {
				if escaped {
					escaped = false
				} else if c == `\\` {
					escaped = true
				} else if c == `'` {
					in_char = false
				}
				continue
			}
			match c {
				`"` {
					in_string = true
				}
				`'` {
					in_char = true
				}
				`{` {
					if depth == 0 && item.head_end < 0 {
						item.head_end = item.text.len - 1
					}
					depth++
				}
				`}` {
					depth--
					if depth == 0 && !item.objc && v3_c_item_is_function_definition(item) {
						scan.finish_item(mut item, used)
					}
				}
				`;` {
					if depth == 0 && !item.objc {
						scan.finish_item(mut item, used)
					}
				}
				`@` {
					// Attributes can come first, as `API_AVAILABLE(...)` puts them before
					// `@interface`; the Objective-C block then runs to its `@end`.
					if depth == 0 && item.objc && v3_c_starts_with_at(raw, i, '@end') {
						item.text.write_string('end')
						i += 3
						scan.finish_item(mut item, used)
					} else if depth == 0 && !item.objc && v3_c_objc_block_starts_at(raw, i) {
						item.objc = true
					}
				}
				else {}
			}
		}
		if item.active {
			item.text.write_u8(`\n`)
		}
	}
}

// record_macro keeps the definition that a `#define` gives a used macro, or
// forgets it at an `#undef`.
fn (mut scan V3ParallelCScan) record_macro(line string, used map[string]bool) {
	rest := line[1..].trim_left(' \t')
	for directive in ['define', 'undef'] {
		if !rest.starts_with(directive) || rest.len <= directive.len
			|| rest[directive.len] !in [` `, `\t`] {
			continue
		}
		definition := rest[directive.len..].trim_space()
		name := v3_c_identifier_at(definition)
		if name.len == 0 || name !in used {
			return
		}
		if directive == 'define' {
			scan.macros[name] = definition
		} else {
			scan.macros.delete(name)
		}
		return
	}
}

fn (mut scan V3ParallelCScan) finish_item(mut item V3ParallelCItem, used map[string]bool) {
	text := item.text.str()
	head_end := item.head_end
	item.active = false
	item.objc = false
	item.head_end = -1
	if used.len > 0 && v3_c_text_uses_any(text, used) {
		for name in v3_c_item_declared_names(text, head_end) {
			if name in used {
				scan.declared[name] = true
			}
		}
	}
	if !scan.collect_hazards || !item.user || scan.hazards.len >= v3_parallel_c_hazard_limit {
		return
	}
	if reason := v3_c_item_replication_hazard(text, head_end) {
		scan.hazards << V3ParallelCHeaderHazard{
			file:   item.file
			line:   item.line
			reason: reason
		}
	}
}

// v3_c_item_declared_names lists the names a top-level item declares: functions,
// objects and typedefs, struct, union and enum tags, enum constants, and
// Objective-C classes and protocols.
fn v3_c_item_declared_names(text string, head_end int) []string {
	clean := v3_c_objc_text(text)
	mut names := []string{}
	if clean.starts_with('@') {
		keyword := v3_c_identifier_at(clean[1..])
		rest := clean[1 + keyword.len..]
		if keyword == 'class' || (keyword == 'protocol' && v3_c_objc_forward_declaration(clean)) {
			for token in v3_c_tokens(rest.all_before(';')) {
				if v3_c_is_name(token) {
					names << token
				}
			}
		} else if keyword in ['interface', 'implementation', 'protocol'] {
			name := v3_c_identifier_at(rest.trim_left(' \t\n'))
			if v3_c_is_name(name) {
				names << name
			}
		}
		return names
	}
	head := if head_end >= 0 { text[..head_end] } else { text }
	head_tokens := v3_c_tokens(v3_c_without_attributes(head))
	for i := 0; i + 1 < head_tokens.len; i++ {
		if head_tokens[i] in ['struct', 'union', 'enum'] && v3_c_is_name(head_tokens[i + 1]) {
			names << head_tokens[i + 1]
		}
	}
	if head_tokens.len == 0 || head_tokens[0] in ['_Static_assert', 'static_assert'] {
		return names
	}
	if head_end >= 0 && v3_c_head_is_function(head) {
		names << v3_c_declarator_name(head)
		return names
	}
	names << v3_c_enum_constants(text)
	declarators := if head_end >= 0 && !head.contains('=') {
		v3_c_item_declarators(text)
	} else {
		v3_c_without_attributes(text).trim_space().trim_right(';')
	}
	for declarator in v3_c_split_top_level(declarators) {
		name := v3_c_declarator_name(declarator)
		if v3_c_is_name(name) {
			names << name
		}
	}
	return names
}

// v3_c_enum_constants lists the constants of the enum bodies in a declaration.
fn v3_c_enum_constants(text string) []string {
	mut names := []string{}
	mut pos := 0
	for {
		idx := text.index_after('enum', pos) or { break }
		pos = idx + 'enum'.len
		if (idx > 0 && v3_c_is_identifier_char(text[idx - 1]))
			|| (pos < text.len && v3_c_is_identifier_char(text[pos])) {
			continue
		}
		open := text.index_after('{', pos) or { break }
		if semicolon := text.index_after(';', pos) {
			if semicolon < open {
				continue
			}
		}
		mut depth := 0
		mut close := text.len
		for i := open; i < text.len; i++ {
			if text[i] == `{` {
				depth++
			} else if text[i] == `}` {
				depth--
				if depth == 0 {
					close = i
					break
				}
			}
		}
		for constant in v3_c_split_top_level(v3_c_without_attributes(text[open + 1..close])) {
			name := v3_c_identifier_at(constant.trim_space())
			if v3_c_is_name(name) {
				names << name
			}
		}
		pos = close
	}
	return names
}

// v3_c_split_top_level splits a declarator or enumerator list at the commas that
// are not inside parentheses, brackets or braces.
fn v3_c_split_top_level(text string) []string {
	mut parts := []string{}
	mut depth := 0
	mut start := 0
	for i, c in text {
		if c in [`(`, `[`, `{`] {
			depth++
		} else if c in [`)`, `]`, `}`] {
			depth--
		} else if c == `,` && depth == 0 {
			parts << text[start..i]
			start = i + 1
		}
	}
	parts << text[start..]
	return parts.filter(it.trim_space().len > 0)
}

// v3_c_is_name reports whether a token is an identifier other than a keyword.
fn v3_c_is_name(token string) bool {
	return token.len > 0 && (token[0].is_letter() || token[0] == `_`) && token !in v3_c_keywords
}

const v3_c_keywords = ['auto', 'break', 'case', 'char', 'const', 'continue', 'default', 'do', 'double',
	'else', 'enum', 'extern', 'float', 'for', 'goto', 'if', 'inline', 'int', 'long', 'register',
	'restrict', 'return', 'short', 'signed', 'sizeof', 'static', 'struct', 'switch', 'typedef',
	'union', 'unsigned', 'void', 'volatile', 'while', '_Bool', '_Complex', '_Atomic', '_Noreturn',
	'_Thread_local', '_Alignas', '_Alignof', '_Static_assert', '__inline', '__inline__', '__restrict',
	'__restrict__', '__const', '__volatile__', '__extension__', '__typeof__', 'typeof', '__signed',
	'__signed__', '__unsafe_unretained', '__strong', '__weak', '__autoreleasing', '_Nullable',
	'_Nonnull', '_Null_unspecified', '__kindof', 'id', 'instancetype', 'BOOL', 'SEL', 'Class',
	'IMP', 'nil', 'Nil', 'YES', 'NO']

// v3_c_partition_items splits generated body C into its top-level items and
// returns, in their original order, the items that use one of `names` and the
// others. An item runs up to the `;` or closing `}` that ends it at the top level,
// with the comments and blanks before it. The generated body holds functions that
// the declaration header declares, so they can be compiled in any unit.
fn v3_c_partition_items(text string, names map[string]bool) (string, string) {
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

// v3_c_item_replication_hazard classifies one top-level item; `head_end` is the
// offset of its first top-level `{`, or -1.
fn v3_c_item_replication_hazard(text string, head_end int) ?string {
	clean := v3_c_objc_text(text)
	if clean.starts_with('@implementation') {
		name := v3_c_identifier_at(clean['@implementation'.len..].trim_space())
		return 'Objective-C @implementation `${name}` would be defined in every unit'
	}
	if clean.starts_with('@') {
		return none
	}
	head := if head_end >= 0 { text[..head_end] } else { text }
	tokens := v3_c_tokens(v3_c_without_attributes(head))
	if tokens.len == 0 || tokens[0] in ['typedef', '_Static_assert', 'static_assert'] {
		return none
	}
	if head_end >= 0 && v3_c_head_is_function(head) {
		name := v3_c_declarator_name(head)
		// The linker keeps one weak definition, as in libgc's `GC_noop1_ptr` fallback.
		if v3_c_has_weak_attribute(head) {
			return none
		}
		if 'static' !in tokens && !(('inline' in tokens || '__inline' in tokens
			|| '__inline__' in tokens) && 'extern' !in tokens) {
			return 'external function `${name}` would be defined in every unit'
		}
		if local := v3_c_mutable_static_local(text[head_end..]) {
			return '`${name}` has the static local `${local}`, which every unit would copy'
		}
		return none
	}
	if 'extern' in tokens {
		return none
	}
	if head_end < 0 && v3_c_head_is_function(head) {
		return none
	}
	if !v3_c_item_declares_object(text, head_end) {
		return none
	}
	name := v3_c_declarator_name(if head_end >= 0 && !head.contains('=') {
		v3_c_item_declarators(text)
	} else {
		head
	})
	if 'static' in tokens {
		// Read-only data is the same in every unit.
		if v3_c_declaration_is_read_only(v3_c_before_initializer(text, head_end)) {
			return none
		}
		return 'mutable static `${name}` would get a separate copy in every unit'
	}
	if v3_c_has_weak_attribute(v3_c_before_initializer(text, head_end)) {
		return none
	}
	return 'external object `${name}` would be defined in every unit'
}

// v3_c_objc_text returns an item without leading blanks, or, for an item that
// starts with attributes followed by Objective-C, from its `@` keyword on.
fn v3_c_objc_text(text string) string {
	clean := text.trim_space()
	if clean.starts_with('@') || !clean.contains('@') {
		return clean
	}
	stripped := v3_c_without_attributes(clean).trim_space()
	return if stripped.starts_with('@') { stripped } else { clean }
}

// v3_c_has_weak_attribute reports whether a declaration head carries the `weak`
// attribute, whose definitions the linker merges.
fn v3_c_has_weak_attribute(head string) bool {
	return v3_c_without_attributes(head).len != head.len
		&& ('weak' in v3_c_tokens(head) || '__weak__' in v3_c_tokens(head))
}

// v3_c_item_is_function_definition reports whether the item being scanned, whose
// top-level braces just closed, is a function definition, so it ends there.
fn v3_c_item_is_function_definition(item V3ParallelCItem) bool {
	if item.head_end < 0 {
		return false
	}
	text := item.text.after(0)
	return v3_c_head_is_function(text[..item.head_end])
}

// v3_c_head_is_function reports whether the declaration head (the text before a
// body, or a whole declaration without one) declares a function rather than an
// object: its declarator name is followed by a parameter list, not by `(*`/`(^`.
fn v3_c_head_is_function(head string) bool {
	clean := v3_c_without_attributes(head).trim_space()
	if clean.contains('=') || clean.starts_with('struct') || clean.starts_with('union')
		|| clean.starts_with('enum') {
		mut type_end := clean.len
		if eq := clean.index('=') {
			type_end = eq
		}
		if !clean[..type_end].contains('(') {
			return false
		}
	}
	mut paren := -1
	for i, c in clean {
		if c == `(` {
			paren = i
			break
		}
		if c == `=` {
			return false
		}
	}
	if paren <= 0 {
		return false
	}
	before := clean[..paren].trim_right(' \t\n')
	if before.len == 0 || !(before[before.len - 1].is_letter() || before[before.len - 1].is_digit()
		|| before[before.len - 1] == `_`) {
		return false
	}
	after := clean[paren + 1..].trim_left(' \t\n')
	if after.starts_with('*') || after.starts_with('^') {
		// `(*name)(...)` declares a pointer, but `(*name(...))(...)` declares a
		// function that returns one.
		return v3_c_group_declares_function(after[1..])
	}
	return true
}

// v3_c_group_declares_function reports whether the parenthesized declarator group
// that continues after its `*` or `^` names a function, as `name(...)` does.
fn v3_c_group_declares_function(group string) bool {
	clean := group.trim_left(' \t\n*^')
	name := v3_c_identifier_at(clean)
	return name.len > 0 && clean[name.len..].trim_left(' \t\n').starts_with('(')
}

// v3_c_item_declares_object reports whether a declaration names an object, as
// opposed to only declaring a struct, union or enum type.
fn v3_c_item_declares_object(text string, head_end int) bool {
	if head_end >= 0 {
		head_tokens := v3_c_tokens(v3_c_without_attributes(text[..head_end]))
		if head_tokens.any(it in ['struct', 'union', 'enum']) && !text[..head_end].contains('=') {
			return v3_c_item_declarators(text).trim_space().len > 0
		}
		return true
	}
	tokens := v3_c_tokens(v3_c_without_attributes(text.trim_space().trim_right(';')))
	if tokens.len <= 2 && tokens.len > 0 && tokens[0] in ['struct', 'union', 'enum'] {
		return false
	}
	return tokens.len > 0
}

// v3_c_item_declarators returns the declarators after the closing brace of a
// struct, union or enum definition, as in `struct S { int a; } s = {1};`.
fn v3_c_item_declarators(text string) string {
	mut depth := 0
	mut first := -1
	for i, c in text {
		if c == `{` {
			depth++
		} else if c == `}` {
			depth--
			if depth == 0 {
				first = i
				break
			}
		}
	}
	if first < 0 {
		return ''
	}
	return v3_c_without_attributes(text[first + 1..]).trim_space().trim_right(';').trim_space()
}

fn v3_c_before_initializer(text string, head_end int) string {
	mut end := if head_end >= 0 { head_end } else { text.len }
	if eq := text[..end].index('=') {
		end = eq
	}
	return text[..end]
}

// v3_c_declaration_is_read_only reports whether the declared object itself is
// `const`: `const T x` and `T const x`, but for a pointer only `T *const x`;
// `const char *x` is a pointer that can still be assigned.
fn v3_c_declaration_is_read_only(declaration string) bool {
	tokens := v3_c_tokens(v3_c_without_attributes(declaration))
	mut last_pointer := -1
	for i, token in tokens {
		if token in ['*', '^'] {
			last_pointer = i
		}
	}
	return 'const' in tokens[last_pointer + 1..]
}

// v3_c_mutable_static_local returns the name of the first `static` local of a
// function body that is not read-only.
fn v3_c_mutable_static_local(body string) ?string {
	mut pos := 0
	for {
		idx := body.index_after('static', pos) or { return none }
		pos = idx + 'static'.len
		if (idx > 0 && v3_c_is_identifier_char(body[idx - 1]))
			|| (pos < body.len && v3_c_is_identifier_char(body[pos])) {
			continue
		}
		end := body.index_after(';', pos) or { body.len }
		declaration := body[pos..end]
		if v3_c_declaration_is_read_only(v3_c_before_initializer(declaration, -1)) {
			continue
		}
		return v3_c_declarator_name(declaration)
	}
	return none
}

// v3_c_declarator_name returns the identifier a declaration names: the last
// identifier before its parameter list, initializer or array bound.
fn v3_c_declarator_name(declaration string) string {
	clean := v3_c_without_attributes(declaration)
	mut end := clean.len
	for i, c in clean {
		if c in [`=`, `[`, `;`, `,`] {
			end = i
			break
		}
		if c == `(` {
			rest := clean[i + 1..].trim_left(' \t\n')
			if !(rest.starts_with('*') || rest.starts_with('^')) {
				end = i
				break
			}
		}
	}
	tokens := v3_c_tokens(clean[..end])
	for j := tokens.len - 1; j >= 0; j-- {
		if tokens[j].len > 0 && (tokens[j][0].is_letter() || tokens[j][0] == `_`) {
			return tokens[j]
		}
	}
	return '?'
}

fn v3_c_identifier_at(text string) string {
	mut end := 0
	for end < text.len && v3_c_is_identifier_char(text[end]) {
		end++
	}
	return text[..end]
}

fn v3_c_is_identifier_char(c u8) bool {
	return c.is_letter() || c.is_digit() || c == `_` || c == `$`
}

// v3_c_tokens splits C text into identifiers and single punctuation characters.
fn v3_c_tokens(text string) []string {
	mut tokens := []string{}
	mut i := 0
	for i < text.len {
		c := text[i]
		if v3_c_is_identifier_char(c) {
			start := i
			for i < text.len && v3_c_is_identifier_char(text[i]) {
				i++
			}
			tokens << text[start..i]
			continue
		}
		if c !in [` `, `\t`, `\n`, `\r`] {
			tokens << text[i..i + 1]
		}
		i++
	}
	return tokens
}

// v3_c_without_attributes removes `__attribute__((...))`, `__asm__(...)` and
// `__declspec(...)` groups, whose parentheses would read as parameter lists.
fn v3_c_without_attributes(text string) string {
	if !text.contains('__attribute') && !text.contains('asm') && !text.contains('__declspec') {
		return text
	}
	mut out := strings.new_builder(text.len)
	mut i := 0
	for i < text.len {
		c := text[i]
		if !v3_c_is_identifier_char(c) {
			out.write_u8(c)
			i++
			continue
		}
		mut word_end := i
		for word_end < text.len && v3_c_is_identifier_char(text[word_end]) {
			word_end++
		}
		word := text.substr_unsafe(i, word_end)
		if word !in ['__attribute__', '__attribute', '__asm__', '__asm', 'asm', '__declspec'] {
			out.write_string(word)
			i = word_end
			continue
		}
		mut j := word_end
		for j < text.len && text[j] in [` `, `\t`, `\n`] {
			j++
		}
		if j >= text.len || text[j] != `(` {
			out.write_string(word)
			i = word_end
			continue
		}
		mut depth := 0
		for j < text.len {
			if text[j] == `(` {
				depth++
			} else if text[j] == `)` {
				depth--
				if depth == 0 {
					j++
					break
				}
			}
			j++
		}
		out.write_u8(` `)
		i = j
	}
	return out.str()
}

// v3_c_starts_with_at reports whether `text` continues with `prefix` at `pos`,
// without copying the rest of the text as `text[pos..].starts_with` would.
fn v3_c_starts_with_at(text string, pos int, prefix string) bool {
	return pos + prefix.len <= text.len && text.substr_unsafe(pos, pos + prefix.len) == prefix
}

// v3_c_line_marker parses a preprocessor line marker such as
// `# 12 "/path/file.h" 2 3`, returning the file, line and flags.
fn v3_c_line_marker(line string) ?(string, int, string) {
	mut rest := line[1..].trim_left(' \t')
	if rest.starts_with('line') {
		rest = rest['line'.len..].trim_left(' \t')
	}
	if rest.len == 0 || !rest[0].is_digit() {
		return none
	}
	mut end := 0
	for end < rest.len && rest[end].is_digit() {
		end++
	}
	number := rest[..end].int()
	rest = rest[end..].trim_left(' \t')
	if !rest.starts_with('"') {
		return none
	}
	close := rest.index_after('"', 1) or { return none }
	return rest[1..close], number, rest[close + 1..].trim_space()
}

// v3_c_objc_block_starts_at reports whether an Objective-C block that ends with
// `@end` starts at `pos`: an interface, an implementation, or a protocol definition.
fn v3_c_objc_block_starts_at(text string, pos int) bool {
	return v3_c_starts_with_at(text, pos, '@interface')
		|| v3_c_starts_with_at(text, pos, '@implementation')
		|| (v3_c_starts_with_at(text, pos, '@protocol')
			&& !v3_c_objc_forward_declaration(text.substr_unsafe(pos, text.len)))
}

// v3_c_objc_forward_declaration reports whether `@protocol` starts a forward
// declaration such as `@protocol A, B;` rather than a definition ending in `@end`.
fn v3_c_objc_forward_declaration(text string) bool {
	semicolon := text.index(';') or { return false }
	newline := text.index('\n') or { text.len }
	return semicolon < newline
}
