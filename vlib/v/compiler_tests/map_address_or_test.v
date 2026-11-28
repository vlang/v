// vtest ownership
import os

fn test_map_address_fallbacks_and_contextual_enum_branches() {
	root := os.join_path(os.vtmp_dir(), 'map_address_or_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'interface Decoder { decode() int }
struct Simple {}
fn (s Simple) decode() int { return 42 }
struct Calls {
mut:
	key int
	fallback int
}
fn address(values &map[string]int, key string, fallback &int) &int {
	return unsafe { &values[key] or { fallback } }
}
fn next_key(mut calls Calls) string {
	calls.key++
	return "found".clone()
}
fn use_fallback(mut calls Calls, value &int) &int {
	calls.fallback++
	return value
}
fn main() {
	check_contextual_enum_branch()
	decoders := {"x": Decoder(Simple{})}
	decoder := unsafe { &decoders["x"] or { panic("missing") } }
	assert decoder.decode() == 42
	assert decoders["x"].decode() == 42
	mut values := {"found": 7}
	mut fallback := 11
	mut found := address(&values, "found", &fallback)
	assert *found == 7
	unsafe { *found = 9 }
	assert values["found"] == 9
	missing := address(&values, "missing", &fallback)
	assert voidptr(missing) == voidptr(&fallback)
	mut calls := Calls{}
	computed := unsafe {
		&values[next_key(mut calls)] or { use_fallback(mut calls, &fallback) }
	}
	assert *computed == 9
	assert calls.key == 1
	assert calls.fallback == 0
	computed_missing := unsafe {
		&values["missing"] or { use_fallback(mut calls, &fallback) }
	}
	assert voidptr(computed_missing) == voidptr(&fallback)
	assert calls.fallback == 1
	nil_value := unsafe { &values["missing"] or { nil } }
	assert nil_value == unsafe { nil }
}
')!
	os.write_file(os.join_path(root, 'enum_branch.v'), 'enum Choice { never auto always }
struct Args { mut: choice Choice }
fn choose(value string, mut args Args) ! {
	args.choice = match value {
		"never" { .never }
		"auto" { .auto }
		"always" { .always }
		else { return error("unrecognized") }
	}
}
fn check_contextual_enum_branch() {
	mut args := Args{}
	choose("always", mut args) or { panic(err) }
	assert args.choice == .always
	choose("invalid", mut args) or { assert err.msg() == "unrecognized" }
}
')!
	for ownership in [[]string{}, ['-ownership', '-d', 'ownership']] {
		for mode in [[]string{}, ['-no-parallel']] {
			result := os.exec([@VEXE, '-nocache', ...ownership, ...mode, 'run', root])
			assert result.exit_code == 0, '${ownership} ${mode}: ${result.output}'
		}
	}
}
