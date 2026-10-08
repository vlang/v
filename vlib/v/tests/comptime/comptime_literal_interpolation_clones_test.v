// These tests intentionally borrow mutable fixed buffers with unsafe vstring().
// Mutating the buffers after interpolation proves that the result copied the text.

import os
import time

@[literal_interpolation_flag]
struct LiteralInterpolationMarker {
	value int
}

fn (marker LiteralInterpolationMarker) probe() {}

fn literal_interpolation_parameter_probe(value int) {
	_ = value
}

enum LiteralInterpolationChoice {
	one
}

struct LiteralInterpolationAlternative {}

type LiteralInterpolationVariants = LiteralInterpolationAlternative | LiteralInterpolationMarker

fn literal_interpolation_fields(first string, second string) []string {
	mut out := []string{}
	$for field in LiteralInterpolationMarker.fields {
		out << '\${if true {${first}} else {${second}}}'
		out << '\${match 0 { 0 {${first}} else {${second}}}'
	}
	return out
}

fn literal_interpolation_attributes(first string, second string) []string {
	mut out := []string{}
	$for attr in LiteralInterpolationMarker.attributes {
		out << '\${if true {${first}} else {${second}}}'
		out << '\${match 0 { 0 {${first}} else {${second}}}'
	}
	return out
}

fn literal_interpolation_parameters(first string, second string) []string {
	mut out := []string{}
	$for param in literal_interpolation_parameter_probe.params {
		out << '\${if true {${first}} else {${second}}}'
		out << '\${match 0 { 0 {${first}} else {${second}}}'
	}
	return out
}

fn literal_interpolation_methods(first string, second string) []string {
	mut out := []string{}
	$for method in LiteralInterpolationMarker.methods {
		out << '\${if true {${first}} else {${second}}}'
		out << '\${match 0 { 0 {${first}} else {${second}}}'
	}
	return out
}

fn literal_interpolation_values(first string, second string) []string {
	mut out := []string{}
	$for value in LiteralInterpolationChoice.values {
		out << '\${if true {${first}} else {${second}}}'
		out << '\${match 0 { 0 {${first}} else {${second}}}'
	}
	return out
}

fn literal_interpolation_variants(first string, second string) []string {
	mut out := []string{}
	$for variant in LiteralInterpolationVariants.variants {
		out << '\${if true {${first}} else {${second}}}'
		out << '\${match 0 { 0 {${first}} else {${second}}}'
	}
	return out
}

fn check_literal_interpolation_clone(route string, format fn (string, string) []string, count int) {
	mut first := [u8(`A`), u8(0)]!
	mut second := [u8(`B`), u8(0)]!
	// Borrow stack buffers so mutation distinguishes a copied result from a retained view.
	first_view := unsafe { (&first[0]).vstring() }
	second_view := unsafe { (&second[0]).vstring() }
	out := format(first_view, second_view)
	first[0] = `C`
	second[0] = `D`
	assert out.len == count * 2, route
	for i in 0 .. count {
		assert out[i * 2] == r'${if true {A} else {B}}', '${route}: ${out}'
		assert out[i * 2 + 1] == r'${match 0 { 0 {A} else {B}}', '${route}: ${out}'
	}
	empty := format('', '')
	assert empty.len == count * 2, route
	for i in 0 .. count {
		assert empty[i * 2] == r'${if true {} else {}}', '${route}: ${empty}'
		assert empty[i * 2 + 1] == r'${match 0 { 0 {} else {}}', '${route}: ${empty}'
	}
}

fn test_real_nested_interpolation_still_evaluates_expressions() {
	first := 'A'
	second := 'B'
	assert 'before ${if first.len > 0 { first } else { second }} after' == 'before A after'
	assert 'before ${match first.len {
		1 { first }
		else { second }
	}} after' == 'before A after'
}

fn test_literal_interpolation_survives_field_clones() {
	check_literal_interpolation_clone('fields', literal_interpolation_fields, 1)
}

fn test_literal_interpolation_survives_attribute_clones() {
	check_literal_interpolation_clone('attributes', literal_interpolation_attributes, 1)
}

fn test_literal_interpolation_survives_parameter_clones() {
	check_literal_interpolation_clone('parameters', literal_interpolation_parameters, 1)
}

fn test_literal_interpolation_survives_method_clones() {
	check_literal_interpolation_clone('methods', literal_interpolation_methods, 1)
}

fn test_literal_interpolation_survives_value_clones() {
	check_literal_interpolation_clone('values', literal_interpolation_values, 1)
}

fn test_literal_interpolation_survives_variant_clones() {
	check_literal_interpolation_clone('variants', literal_interpolation_variants, 2)
}

@[prefix: r'${if true {']
struct LiteralInterpolationAttributePrefix {}

fn literal_interpolation_attribute_text(first string, second string) string {
	mut out := ''
	$for attr in LiteralInterpolationAttributePrefix.attributes {
		out = '${attr.arg}${first}} else {${second}}}'
	}
	return out
}

fn test_reflected_attribute_interpolation_text_remains_literal() {
	mut first := [u8(`A`), u8(0)]!
	mut second := [u8(`B`), u8(0)]!
	out := literal_interpolation_attribute_text(unsafe { (&first[0]).vstring() }, unsafe {
		(&second[0]).vstring()
	})
	first[0] = `C`
	second[0] = `D`
	assert out == r'${if true {A} else {B}}'
}

fn literal_interpolation_field_return(first string, second string) string {
	$for field in LiteralInterpolationMarker.fields {
		return '\${if true {${first}} else {${second}}}'
	}
	return ''
}

fn test_literal_interpolation_survives_field_return_clone() {
	mut first := [u8(`A`), u8(0)]!
	mut second := [u8(`B`), u8(0)]!
	out := literal_interpolation_field_return(unsafe { (&first[0]).vstring() }, unsafe {
		(&second[0]).vstring()
	})
	first[0] = `C`
	second[0] = `D`
	assert out == r'${if true {A} else {B}}'
}

struct LiteralInterpolationDefault {
	text   string
	marker int
}

struct LiteralInterpolationPromotedDefault {
	LiteralInterpolationDefault  = LiteralInterpolationDefault{
		text:   '\${if true {A} else {B}}'
		marker: 1
	}
}

fn test_literal_interpolation_survives_promoted_default_clone() {
	value := LiteralInterpolationPromotedDefault{ marker: 2 }
	assert value.text == r'${if true {A} else {B}}'
}

fn test_pseudo_file_interpolation_text_remains_literal() {
	root := os.join_path(os.vtmp_dir(), 'literal_interpolation_pseudo_${os.getpid()}_${time.sys_mono_now()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	binary := os.join_path(root, 'literal_interpolation_pseudo')
	os.write_file(source, r'#line 1 "${if true {"' + '\n' + r"fn main() {
	mut first := [u8(`A`), u8(0)]!
	mut second := [u8(`B`), u8(0)]!
	left := unsafe { (&first[0]).vstring() }
	right := unsafe { (&second[0]).vstring() }
	out := '${@FILE}${left}} else {${right}}}'
	first[0] = `C`
	second[0] = `D`
	assert out == r'${if true {A} else {B}}'
}")!
	compile := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-gc', 'none',
		'-cc', @CCOMPILER, '-o', binary, source])
	assert compile.exit_code == 0, compile.output
	run := os.exec([binary])
	assert run.exit_code == 0, run.output
}
