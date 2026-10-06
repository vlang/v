module main

import os
import v.astquery

// sample is the fixture every AST query test runs against. It declares one of
// each kind and mentions every name at least once.
const sample = 'module probe\n\nimport os\n\n// pi is the usual constant.\nconst pi = 3.14\n\n/// Shape is a documented interface.\ninterface Shape {\n\tarea() f64\n}\n\nenum Color {\n\tred\n\tgreen\n}\n\ntype Number = int\n\ntype Value = int | f64\n\nstruct Point {\n\t// x is the horizontal coordinate.\n\tx int\n\ty int\n}\n\n// area returns the area of a point.\nfn (p Point) area() f64 {\n\treturn f64(p.x)\n}\n\nfn main() {\n\tp := Point{\n\t\tx: 1\n\t\ty: 2\n\t}\n\tprintln(p.area(), pi, Color.red)\n}\n'

const other = 'module probe\n\nfn helper() int {\n\treturn 1\n}\n'

// write_sample writes `source` to a file inside the temp directory and returns
// its path.
fn write_sample(name string, source string) !string {
	dir := os.join_path(os.vtmp_dir(), 'v_astquery_test_${os.getpid()}')
	os.mkdir_all(dir)!
	path := os.join_path(dir, name)
	os.write_file(path, source)!
	return path
}

// find_decl returns the declaration of `name`, failing the test when there is
// none or more than one.
fn find_decl(decls []astquery.Declaration, name string) astquery.Declaration {
	for decl in decls {
		if decl.name == name {
			return decl
		}
	}
	assert false, 'no declaration named `${name}`'
	return astquery.Declaration{}
}

// count_decls counts the declarations named `name`.
fn count_decls(decls []astquery.Declaration, name string) int {
	mut n := 0
	for decl in decls {
		if decl.name == name {
			n++
		}
	}
	return n
}

fn test_declarations_covers_every_kind() {
	decls := astquery.declarations(astquery.parse(write_sample('probe.v', sample)!))
	mut kinds := map[string]int{}
	for decl in decls {
		kinds[decl.kind.str()]++
	}
	assert kinds['module'] == 1
	assert kinds['import'] == 1
	assert kinds['const'] == 1
	assert kinds['interface'] == 1
	assert kinds['interface_method'] == 1
	assert kinds['enum'] == 1
	assert kinds['enum_value'] == 2
	assert kinds['type_alias'] == 1
	assert kinds['sumtype'] == 1
	assert kinds['struct'] == 1
	assert kinds['field'] == 2
	assert kinds['method'] == 1
	// `area` on Point plus `main`.
	assert kinds['fn'] == 1
}

fn test_declarations_report_the_receiver_of_a_method() {
	decls := astquery.declarations(astquery.parse(write_sample('probe.v', sample)!))
	// `area` appears twice: as an interface method and as a method on Point.
	assert count_decls(decls, 'area') == 2
	interface_method := decls.filter(it.kind == .interface_method)[0]
	assert interface_method.name == 'area'
	assert interface_method.receiver == ''
	method := decls.filter(it.kind == .method)[0]
	assert method.name == 'area'
	assert method.receiver == 'Point'
	assert method.type_name == 'f64'
}

fn test_declarations_carry_positions() {
	path := write_sample('probe.v', sample)!
	decls := astquery.declarations(astquery.parse(path))
	struct_point := find_decl(decls, 'Point')
	assert struct_point.file == path
	assert struct_point.line == 22
	assert struct_point.column >= 1
	assert struct_point.end_line >= struct_point.line
}

fn test_declarations_attach_a_doc_comment_only_when_it_is_adjacent() {
	decls := astquery.declarations(astquery.parse(write_sample('probe.v', sample)!))
	// `/// Shape is a documented interface.` sits directly above the interface.
	assert find_decl(decls, 'Shape').doc == 'Shape is a documented interface.'
	// `// area returns the area of a point.` documents the method on `Point`.
	assert decls.filter(it.kind == .method)[0].doc == 'area returns the area of a point.'
	// The struct field carries a plain `//` comment, which is still its doc.
	assert find_decl(decls, 'x').doc == 'x is the horizontal coordinate.'
	// Nothing is adjacent above `Point`, so it has no doc.
	assert find_decl(decls, 'Point').doc == ''
	// `pi` has a plain comment right above it.
	assert find_decl(decls, 'pi').doc == 'pi is the usual constant.'
}

fn test_a_comment_separated_by_a_blank_line_is_not_documentation() {
	source := 'module m\n\n// about the struct\n\nstruct Thing {\n\tx int\n}\n'
	decls := astquery.declarations(astquery.parse(write_sample('gap.v', source)!))
	assert find_decl(decls, 'Thing').doc == ''
}

fn test_a_multi_line_doc_comment_is_joined() {
	source := 'module m\n\n/// first line\n/// second line\nfn go() {}\n'
	decls := astquery.declarations(astquery.parse(write_sample('multi.v', source)!))
	assert find_decl(decls, 'go').doc == 'first line\nsecond line'
}

fn test_inline_public_fields_keep_their_names_and_types() {
	source := 'module m\n\nstruct S {\n\tpub count int\n\tplain int\n}\n\nfn total(s S) int {\n\treturn s.count + s.plain\n}\n'
	a := astquery.parse(write_sample('pubfield.v', source)!)
	decls := astquery.declarations(a)
	fields := decls.filter(it.kind == .field)
	assert fields.map(it.name) == ['count', 'plain']
	assert fields.map(it.type_name) == ['int', 'int']
	assert fields.map(it.line) == [4, 5]
	assert fields.map(it.column) == [6, 2]
	assert astquery.references(a, 'pub').len == 0
	count_refs := astquery.references(a, 'count')
	assert count_refs.len == 2
	assert count_refs[0].declaration
	assert count_refs[0].line == 4
	assert count_refs[0].column == 6
	assert count_refs[0].end_column == 11
	assert !count_refs[1].declaration
	assert count_refs[1].line == 9
	assert count_refs[1].column == 11
	assert count_refs[1].end_column == 16
}

fn test_field_sections_report_fields_without_marker_declarations() {
	source := 'module m\n\nstruct S {\n\tplain int\n\tpub:\n\tcount int\n\tmut:\n\tchanged string\n\tpub mut:\n\tshared bool\n}\n'
	a := astquery.parse(write_sample('field_sections.v', source)!)
	fields := astquery.declarations(a).filter(it.kind == .field)
	assert fields.map(it.name) == ['plain', 'count', 'changed', 'shared']
	assert fields.map(it.type_name) == ['int', 'int', 'string', 'bool']
	assert fields.map(it.line) == [4, 6, 8, 10]
	assert fields.all(it.column == 2)
	assert astquery.references(a, 'pub').len == 0
	assert astquery.references(a, 'mut').len == 0
	for field in fields {
		refs := astquery.references(a, field.name)
		assert refs.len == 1
		assert refs[0].declaration
		assert refs[0].node_kind == 'field_decl'
		assert refs[0].line == field.line
		assert refs[0].column == 2
		assert refs[0].end_column == 2 + field.name.len
	}
}

fn test_field_names_matching_visibility_words_are_reported() {
	source := 'module m\n\nstruct S {\n\tpub []int\n\tafter_pub string\n\tpriv map[string]int\n\tafter_priv bool\n}\n'
	a := astquery.parse(write_sample('visibility_names.v', source)!)
	decls := astquery.declarations(a)
	fields := decls.filter(it.kind == .field)
	assert fields.map(it.name) == ['pub', 'after_pub', 'priv', 'after_priv']
	assert fields.map(it.type_name) == ['[]int', 'string', 'map[string]int', 'bool']
	assert fields.map(it.line) == [4, 5, 6, 7]
	assert fields.all(it.column == 2)
	for field in fields {
		refs := astquery.references(a, field.name)
		assert refs.len == 1
		assert refs[0].declaration
		assert refs[0].line == field.line
		assert refs[0].column == 2
		assert refs[0].end_column == 2 + field.name.len
	}
}

fn test_references_finds_the_declaration_and_every_use() {
	a := astquery.parse(write_sample('probe.v', sample)!)
	found := astquery.references(a, 'pi')
	// The declaration plus the one mention inside `main`.
	assert found.len == 2
	assert found[0].declaration
	assert found[0].line == 6
	assert found[0].node_kind == 'const_field'
	assert !found[1].declaration
	assert found[1].node_kind == 'ident'
	assert found[1].line == 38
}

fn test_references_reports_a_method_and_its_calls() {
	a := astquery.parse(write_sample('probe.v', sample)!)
	found := astquery.references(a, 'area')
	// The interface method, the method declaration and the `p.area()` call.
	assert found.len == 3
	assert found.any(it.declaration)
	assert found.any(it.node_kind == 'selector')
}

fn test_references_reports_nested_multiline_and_assoc_initializer_keys() {
	source := 'module main
struct Host { hello int }
struct Nested { hello Host }
fn main() {
 hello := 42
 h := Host{
  hello:
   hello // hello
 }
 nested := Nested{
  hello: Host{
   hello: 42
  }
 }
 copied := Host{...h, hello: 43}
 assert h.hello == 42
 assert nested.hello.hello == 42
 assert copied.hello == 43
 println("hello") // hello
}
'
	a := astquery.parse(write_sample('field_initializers.v', source)!)
	fields := a.nodes.filter(it.kind == .field_init && it.value == 'hello')
	assert fields.len == 4
	for field in fields {
		assert source[field.pos.offset..field.pos.end] == 'hello'
	}
	found := astquery.references(a, 'hello')
	keys := found.filter(it.node_kind == 'field_init')
	assert keys.map(it.line) == [7, 11, 12, 15]
	assert keys.map(it.column) == [3, 3, 4, 23]
	assert keys.all(!it.declaration)
	assert found.any(it.node_kind == 'ident' && it.line == 8 && it.column == 4)
	assert !found.any(it.line == 19)
}

// A method declaration and a selector both span more source than their node
// value names, so an occurrence derived from an offset into that value pointed
// at the wrong bytes.
//
// The reviewer's reproduction: renaming `hello` to `greet` on
// `fn (h Host) hello() int` plus a `h.hello()` call reported success and wrote
// `fn (h Host) hellogreett` and `greetlo()`. The declaration span already starts
// at the name, so the `Host.` prefix length shifted the edit into the signature;
// the selector span starts at its receiver while its value holds only the method
// name, so the edit landed on the receiver.
fn test_occurrences_for_methods_and_selectors_point_at_the_name() {
	source := 'module main\n\nstruct Host {\n}\n\nfn (h Host) hello() int {\n\treturn 42\n}\n\nfn main() {\n\th := Host{}\n\tprintln(h.hello())\n}\n'
	path := write_sample('method.v', source)!
	found := astquery.references(astquery.parse(path), 'hello')
	assert found.len == 2, found.len.str()
	lines := source.split_into_lines()
	for occ in found {
		line := lines[occ.line - 1]
		start := occ.column - 1
		assert start >= 0 && start + occ.end_column - occ.column <= line.len, occ.str()
		assert line[start..occ.end_column - 1] == 'hello', 'line ${occ.line} column ${occ.column} is not the name: `hello` expected, got `${line[start..occ.end_column - 1]}` in `${line}`'
	}
}

// The declaration and the call are separate nodes with different spans, so both
// have to resolve; the bug made one of them land on the receiver.
fn test_a_selector_occurrence_does_not_start_on_its_receiver() {
	source := 'module main\n\nstruct Host {\n}\n\nfn (h Host) hello() int {\n\treturn 42\n}\n\nfn main() {\n\th := Host{}\n\tprintln(h.hello())\n}\n'
	path := write_sample('selector.v', source)!
	lines := source.split_into_lines()
	for occ in astquery.references(astquery.parse(path), 'hello') {
		if occ.node_kind != 'selector' {
			continue
		}
		start := occ.column - 1
		assert lines[occ.line - 1][start..start + 'hello'.len] == 'hello', 'the selector span starts on `${lines[occ.line - 1][start..start + 'hello'.len]}`'
	}
}

fn test_escaped_method_occurrences_point_after_the_escape_prefix() {
	source := 'module main\nstruct Host {}\nfn (h Host) hello() int { return 42 }\nfn main() {\n h := Host{}\n println(h.@hello())\n}\n'
	path := write_sample('escaped_method.v', source)!
	found := astquery.references(astquery.parse(path), 'hello')
	assert found.len == 2, found.str()
	lines := source.split_into_lines()
	for occ in found {
		line := lines[occ.line - 1]
		assert line[occ.column - 1..occ.end_column - 1] == 'hello'
		if !occ.declaration {
			assert line[occ.column - 2] == `@`
		}
	}
}

fn test_references_ignores_names_only_present_in_text() {
	source := "module m\n\nfn go() {\n\t// point is only mentioned here\n\ts := 'point'\n\tprintln(s)\n}\n"
	a := astquery.parse(write_sample('text.v', source)!)
	// `point` appears in a comment and in a string literal, neither of which is
	// an AST mention of an identifier.
	assert astquery.references(a, 'point').len == 0
}

fn test_references_of_an_unknown_name_is_empty() {
	a := astquery.parse(write_sample('probe.v', sample)!)
	assert astquery.references(a, 'nosuchname').len == 0
	// An empty name never matches, not even an empty node value.
	assert astquery.references(a, '').len == 0
}

fn test_occurrence_at_finds_the_name_under_a_position() {
	a := astquery.parse(write_sample('probe.v', sample)!)
	// The last line of the fixture is `\tprintln(p.area(), pi, Color.red)`, where
	// `pi` starts at column 20.
	found := astquery.occurrence_at(a, 'pi', 38, 20) or { panic('no occurrence at 38:20') }
	assert found.name == 'pi'
	assert found.line == 38
	assert found.declaration == false
	assert found.end_column == 22
}

fn test_occurrence_at_returns_the_innermost_match() {
	source := 'module m\n\nfn go() {\n\tprintln(go)\n}\n'
	a := astquery.parse(write_sample('inner.v', source)!)
	// On line 4 `\tprintln(go)` the name `go` spans columns 10 to 12. Both the
	// call node and the identifier node cover that span, so the narrowest one is
	// reported.
	found := astquery.occurrence_at(a, 'go', 4, 11) or { panic('no occurrence') }
	assert found.column == 10
	assert found.end_column == 12
	assert !found.declaration
	// The declaration itself is the widest mention, at columns 4 to 6.
	decl := astquery.occurrence_at(a, 'go', 3, 5) or { panic('no declaration') }
	assert decl.declaration
	assert decl.column == 4
}

fn test_occurrence_at_returns_none_outside_any_name() {
	a := astquery.parse(write_sample('probe.v', sample)!)
	assert astquery.occurrence_at(a, 'pi', 1, 1) == none
	assert astquery.occurrence_at(a, 'pi', 9999, 1) == none
}

fn test_a_file_with_only_declarations_parses() {
	// Guards against the traversal assuming there is a `main`.
	source := 'module m\n\nstruct A {\n\tx int\n}\n'
	decls := astquery.declarations(astquery.parse(write_sample('nomain.v', source)!))
	assert count_decls(decls, 'A') == 1
}

fn test_broken_source_is_still_queryable() {
	// The module promises to answer questions about code that does not compile:
	// this file names a function that is never declared.
	source := 'module m\n\nfn broken() {\n\tundefined_name\n}\n'
	a := astquery.parse(write_sample('broken.v', source)!)
	assert count_decls(astquery.declarations(a), 'broken') == 1
	assert astquery.references(a, 'undefined_name').len == 1
}

fn test_source_with_a_missing_parameter_is_still_readable() {
	// A syntax error the parser recovers from still yields a declaration, under a
	// generated name that shows the recovery happened.
	source := 'module m\n\nfn broken( {\n\tundefined_name\n}\n'
	decls := astquery.declarations(astquery.parse(write_sample('recover.v', source)!))
	assert decls.len == 2
	assert decls[0].kind == .module
	assert decls[1].kind == .fn
	assert decls[1].name.starts_with('V:')
}

fn test_two_files_keep_their_own_paths() {
	dir := os.join_path(os.vtmp_dir(), 'v_astquery_two_${os.getpid()}')
	os.mkdir_all(dir)!
	first := os.join_path(dir, 'first.v')
	second := os.join_path(dir, 'second.v')
	os.write_file(first, other)!
	os.write_file(second, 'module probe\n\nfn helper() int {\n\treturn 2\n}\n')!
	assert astquery.references(astquery.parse(first), 'helper')[0].file == first
	assert astquery.references(astquery.parse(second), 'helper')[0].file == second
}
