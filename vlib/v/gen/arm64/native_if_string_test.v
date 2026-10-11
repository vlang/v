module arm64

import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types

fn test_native_if_string_branches_preserve_pointer_field_map_keys() {
	$if macos && arm64 {
		source := r'module main
fn C.exit(int)
struct Binding { value string }
struct FlatAst { nodes []Binding }
struct Checker { a &FlatAst }
fn (a &FlatAst) node(id int) &Binding { return unsafe { &a.nodes[id] } }
fn (checker &Checker) contains(module_name string, values map[string]int) bool {
    binding := checker.a.node(0)
    qname := if module_name.len == 0 {
        binding.value
    } else if module_name == "main" {
        binding.value
    } else {
        "${module_name}.${binding.value}"
    }
    full_qname := "${if module_name.len > 0 { module_name } else { "main" }}.${binding.value}"
    return qname in values && full_qname in values
}
fn main() {
    a := FlatAst{nodes: [Binding{value: "kept"}]}
    checker := Checker{a: &a}
    mut values := map[string]int{}
    values["kept"] = 1
    values["main.kept"] = 2
    values["custom.kept"] = 3
    if !checker.contains("", values) { C.exit(1) }
    if !checker.contains("main", values) { C.exit(2) }
    if !checker.contains("custom", values) { C.exit(3) }
    if checker.contains("missing", values) { C.exit(4) }
}
'
		run_native_transformed_if_fixture('string_branches', source)
	}
}

fn test_native_pointer_map_or_keeps_type_across_terminating_branches() {
	$if macos && arm64 {
		source := r'module main
fn C.exit(int)
fn C.malloc(usize) voidptr
fn C.memcpy(voidptr, voidptr, usize) voidptr
fn memdup(source voidptr, size isize) voidptr {
    result := C.malloc(usize(size))
    return C.memcpy(result, source, usize(size))
}
struct SourceFile { name string }
fn find_continue(sources map[string]&SourceFile, names []string) string {
    for name in names {
        file := sources[name] or { continue }
        return file.name
    }
    return "missing"
}
fn find_return(sources map[string]&SourceFile, name string) string {
    file := sources[name] or { return "missing" }
    return file.name
}
fn find_break(sources map[string]&SourceFile, names []string) string {
    for name in names {
        file := sources[name] or { break }
        return file.name
    }
    return "missing"
}
fn choose(first &SourceFile, second &SourceFile, condition bool) &SourceFile {
    return if condition { first } else { second }
}
fn main() {
    first := SourceFile{name: "kept"}
    second := SourceFile{name: "other"}
    mut sources := map[string]&SourceFile{}
    sources["first"] = &first
    sources["second"] = &second
    if find_continue(sources, ["missing", "first"]) != "kept" { C.exit(1) }
    if find_continue(sources, ["missing"]) != "missing" { C.exit(2) }
    if find_return(sources, "first") != "kept" { C.exit(3) }
    if find_return(sources, "missing") != "missing" { C.exit(4) }
    if find_break(sources, ["first"]) != "kept" { C.exit(5) }
    if find_break(sources, ["missing", "first"]) != "missing" { C.exit(6) }
    if choose(&first, &second, true).name != "kept" { C.exit(7) }
    if choose(&first, &second, false).name != "other" { C.exit(8) }
}
'
		run_native_transformed_if_fixture('pointer_staging', source)
	}
}

fn run_native_transformed_if_fixture(name string, source string) {
	path := os.join_path(os.vtmp_dir(), 'arm64_if_${name}_${os.getpid()}.v')
	output := path.all_before_last('.')
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	os.write_file(path, source) or { panic(err) }
	mut preferences := pref.new_preferences()
	preferences.backend = 'arm64'
	mut p := parser.Parser.new(preferences)
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, tc)
	m := ssa.build_with_used(a, map[string]bool{}, tc)
	mut g := Gen.new(m)
	g.gen()
	g.write_and_link(output)
	result := os.exec([output])
	assert result.exit_code == 0, result.output
}
