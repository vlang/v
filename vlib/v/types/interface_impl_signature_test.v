module types

import os
import v.parser
import v.pref

// collected_checker parses `source` as the only file of a program and collects
// its declarations.
fn collected_checker(name string, source string) &TypeChecker {
	path := os.join_path(os.vtmp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(path, source) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	assert tc.errors.len == 0, tc.errors.str()
	return tc
}

// The module cache decides from this signature which implementer lists the code of
// a module can depend on. Besides the lists it has to say which names without a
// module prefix are the program's, and which interfaces an implementer holds.
fn test_interface_impl_set_signature_names_the_module_of_bare_names() {
	tc := collected_checker('interface_signature_modules', 'module main

interface Shape {
	area() int
}

struct Square {
	side int
}

fn (s Square) area() int {
	return s.side * s.side
}
')
	lines := tc.interface_impl_set_signature().split_into_lines()
	assert 'Shape=Square' in lines
	assert '#module Shape=main' in lines
	assert '#module Square=main' in lines
	assert lines.filter(it.starts_with('#reach ')).len == 0
}

fn test_interface_impl_set_signature_lists_the_interfaces_among_the_fields_of_an_implementer() {
	tc := collected_checker('interface_signature_reach', 'module main

interface Label {
	text() string
}

interface Unused {
	nothing() bool
}

interface Shape {
	area() int
}

struct Name {
	value string
}

fn (n Name) text() string {
	return n.value
}

struct Tag {
	labels []Label
}

struct Square {
	side int
	tags map[string]?&Tag
}

fn (s Square) area() int {
	return s.side * s.side
}

struct Plain {
	side int
}

fn (p Plain) area() int {
	return p.side
}
')
	lines := tc.interface_impl_set_signature().split_into_lines()
	assert 'Shape=Plain,Square' in lines
	assert 'Label=Name' in lines
	// `Square` holds a `Label` three levels down; `Plain` and `Name` hold none.
	assert '#reach Square=Label' in lines
	assert lines.filter(it.starts_with('#reach ')).len == 1
}
