module fastc

import v.pref
import v.scanner
import v.token

// Generic static methods (`fn Box.create[T](value T) Box[T]`) are lowered to generic free
// functions before monomorphization, so the existing generic function support handles their
// instances: the declaration and every `Box.create`/`mod.Box.create` reference are renamed to
// `static__Box__create`.

const fastc_generic_static_prefix = 'static__'

fn fastc_generic_static_function_name(owner string, method string) string {
	return '${fastc_generic_static_prefix}${owner}__${method}'
}

// fastc_lower_generic_static_methods renames generic static methods and their references.
// Sources without such declarations are returned unchanged after a byte scan.
fn fastc_lower_generic_static_methods(sources []FastcSourceFile, prefs &pref.Preferences) []FastcSourceFile {
	// module name -> owner type -> method names
	mut statics := map[string]map[string][]string{}
	for source_file in sources {
		for declaration in fastc_find_generic_static_declarations(source_file.source) {
			module_name := source_file.header.module_name
			if module_name !in statics {
				statics[module_name] = map[string][]string{}
			}
			if declaration.owner !in statics[module_name] {
				statics[module_name][declaration.owner] = []string{}
			}
			if declaration.method !in statics[module_name][declaration.owner] {
				statics[module_name][declaration.owner] << declaration.method
			}
		}
	}
	if statics.len == 0 {
		return sources
	}
	mut lowered := []FastcSourceFile{cap: sources.len}
	for source_file in sources {
		if !fastc_source_mentions_generic_static(source_file, statics) {
			lowered << source_file
			continue
		}
		lowered << fastc_apply_mapped_source_edits(source_file,
			fastc_generic_static_reference_edits(source_file, statics, prefs))
	}
	return lowered
}

struct FastcGenericStaticDeclaration {
	owner  string
	method string
}

// fastc_find_generic_static_declarations finds `fn Owner.method[` without tokenizing.
fn fastc_find_generic_static_declarations(source string) []FastcGenericStaticDeclaration {
	mut found := []FastcGenericStaticDeclaration{}
	mut i := 0
	for {
		i = source.index_after('fn ', i) or { break }
		if i > 0 && fastc_identifier_byte(source[i - 1]) {
			i += 3
			continue
		}
		mut j := i + 3
		for j < source.len && source[j] in [` `, `\t`] {
			j++
		}
		i = j
		if j >= source.len || !source[j].is_capital() {
			continue
		}
		owner_start := j
		for j < source.len && fastc_identifier_byte(source[j]) {
			j++
		}
		if j >= source.len || source[j] != `.` {
			continue
		}
		owner := source[owner_start..j]
		j++
		method_start := j
		for j < source.len && fastc_identifier_byte(source[j]) {
			j++
		}
		if j == method_start || j >= source.len || source[j] != `[` {
			continue
		}
		found << FastcGenericStaticDeclaration{
			owner:  owner
			method: source[method_start..j]
		}
	}
	return found
}

fn fastc_source_mentions_generic_static(source_file FastcSourceFile, statics map[string]map[string][]string) bool {
	for _, owners in statics {
		for owner, methods in owners {
			for method in methods {
				if source_file.source.contains('${owner}.${method}') {
					return true
				}
			}
		}
	}
	return false
}

// fastc_generic_static_reference_edits renames the declarations of generic static methods in
// `source_file` and its references to them, unqualified in their own module and qualified by
// an import alias elsewhere.
fn fastc_generic_static_reference_edits(source_file FastcSourceFile, statics map[string]map[string][]string, prefs &pref.Preferences) []FastcSourceEdit {
	module_name := source_file.header.module_name
	file := token.File.unindexed(source_file.path, source_file.source.len)
	mut s := scanner.new_scanner(prefs, .normal)
	s.init(file, source_file.source)
	mut edits := []FastcSourceEdit{}
	mut previous := token.Token.unknown
	mut tok := s.scan()
	for tok != .eof {
		if tok != .name || previous == .dot || s.lit.len == 0 {
			previous = tok
			tok = s.scan()
			continue
		}
		first := s.lit
		first_pos := s.pos
		mut look := s
		if look.scan() != .dot || look.scan() != .name {
			previous = tok
			tok = s.scan()
			continue
		}
		second := look.lit
		second_pos := look.pos
		second_end := look.offset
		// `Owner.method` in the declaring module, including the declaration itself.
		if methods := statics[module_name][first] {
			if second in methods {
				edits << FastcSourceEdit{
					start:       first_pos
					end:         second_end
					replacement: fastc_generic_static_function_name(first, second)
				}
				s = look
				previous = .name
				tok = s.scan()
				continue
			}
		}
		// `alias.Owner.method` in another module.
		if imported_module := source_file.header.imports[first] {
			mut owner_look := look
			if owner_look.scan() == .dot && owner_look.scan() == .name {
				if methods := statics[imported_module][second] {
					method := owner_look.lit
					if method in methods {
						edits << FastcSourceEdit{
							start:       second_pos
							end:         owner_look.offset
							replacement: fastc_generic_static_function_name(second, method)
						}
						s = owner_look
						previous = .name
						tok = s.scan()
						continue
					}
				}
			}
		}
		previous = tok
		tok = s.scan()
	}
	return edits
}
