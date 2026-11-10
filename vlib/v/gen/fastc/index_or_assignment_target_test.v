module fastc

import os
import v.cmdexec
import v.pref

// fastc_selfhost_index_or_program generates `main_source` as a selfhost
// (`building_v`) program with FastC, compiles it with `cc` and returns its output.
fn fastc_selfhost_index_or_program(name string, main_source string) string {
	root := os.join_path(os.vtmp_dir(), 'fastc_index_or_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), main_source) or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.enable_globals = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	return run.output
}

fn test_fastc_selfhost_assignment_target_index_with_folded_tokens() {
	$if !macos && !linux {
		return
	}
	// Each construct in a target index below is read as one token. After an `or`
	// block or a `?` propagation the reader resets its source token count; after
	// a `.filter` call, a `$d` value, an interpolated string or an `if` value it
	// does not, so the count runs past the target's tokens (`.filter` panicked
	// with `slice bounds out of range (8 >= 6)`) or falls behind them and loses
	// the target type, which the sum-type `match` values need.
	output := fastc_selfhost_index_or_program('target', 'module main
struct Signature {
	n int
}
struct Item {
	n int
}
struct Name {
	s string
}
type Value = Item | Name
fn describe(value Value) string {
	return match value {
		Item { "item " + value.n.str() }
		Name { "name " + value.s }
	}
}
struct Bag {
	items []int
}
fn find(m map[string]int, key string) ?int {
	return m[key] or { return none }
}
fn fill(m map[string]int) ? {
	mut counts := []int{len: 3}
	counts[find(m, "b")?] = 5
	println(counts)
}
fn signature_name(key string, signature Signature) string {
	return key + "_" + signature.n.str()
}
fn main() {
	mut functions := map[string]Signature{}
	functions["a.x"] = Signature{ n: 1 }
	functions["b.y"] = Signature{ n: 2 }
	functions["c"] = Signature{ n: 3 }
	mut function_c_names := map[string]string{}
	function_c_names["a.x"] = "AX"
	mut method_c_names := map[string]bool{}
	for key, signature in functions {
		if !key.contains(".") {
			continue
		}
		method_c_names[function_c_names[key] or { signature_name(key, signature) }] = true
	}
	mut names := method_c_names.keys()
	names.sort()
	println(names)
	slots := { "one": 1, "two": 2 }
	mut counts := []int{len: 4}
	for key in ["one", "two", "two", "nine"] {
		counts[slots[key] or { 3 }] += 10
	}
	counts[slots["one"] or { 0 }] = 7
	println(counts)
	mut values := []Value{len: 2, init: Value(Item{})}
	for k in [1, 2] {
		values[slots[k.str()] or { k - 1 }] = match k {
			1 { Value(Item{ n: 3 }) }
			else { Name{ s: "b" } }
		}
	}
	println(values.map(describe(it)))
	bag := Bag{ items: [1, 2, 3] }
	mut filtered := []int{len: 4}
	filtered[bag.items.filter(it > 1).len] = 7
	println(filtered)
	fill({ "b": 2 }) or { println("missing") }
	values[$d("fastc_index_or_slot", 1)] = match bag.items.len {
		3 { Value(Item{ n: 9 }) }
		else { Name{ s: "x" } }
	}
	println(values.map(describe(it)))
	key := "a"
	mut by_name := map[string]Value{}
	by_name["\${key}"] = match key.len {
		1 { Value(Item{ n: 1 }) }
		else { Name{ s: "x" } }
	}
	values[if key.len == 1 { 0 } else { 1 }] = match key.len {
		1 { Value(Name{ s: "c" }) }
		else { Item{ n: 0 } }
	}
	println([by_name["a"] or { Value(Name{ s: "none" }) }, values[0]].map(describe(it)))
}
')
	assert output.trim_space().split_into_lines() == ["['AX', 'b.y_2']", '[0, 7, 20, 10]',
		"['item 3', 'name b']", '[0, 0, 7, 0]', '[0, 0, 5]', "['item 3', 'item 9']",
		"['item 1', 'name c']"]
}
