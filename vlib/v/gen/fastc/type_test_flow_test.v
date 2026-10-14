module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_member_array_name_shadowing_a_reference_local() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate('module main
struct NodeStore { nodes []bool }
struct Store { a &NodeStore }
fn min_count(store &Store, mut nodes []bool) int {
	return if nodes.len < store.a.nodes.len { nodes.len } else { store.a.nodes.len }
}
fn main() {
	store := Store{a: &NodeStore{nodes: [true]}}
	mut nodes := [true, true]
	println(min_count(&store, mut nodes))
}
', 'member_reference.v', prefs) or { panic(err) }
	assert !source.contains('store->a->nodes->len'), source
	assert source.contains('store->a->nodes.len'), source
}

fn test_fastc_type_tests_narrow_else_or_and_mutated_loop_subjects() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_type_test_flow_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

struct Leaf { mut: name string }
struct Link { base Kind }
type Kind = Leaf | Link

fn name(value Kind) string {
	if value !is Leaf {
		return "other"
	} else if value.name.len > 0 {
		return value.name
	}
	return "empty"
}

fn wrong(value Kind) bool {
	return value !is Leaf || value.name != "ready"
}

fn unwrap(mut item Kind) string {
	if item is Link {
		for item is Link {
			item = item.base
		}
		if item is Leaf && item.name == "ready" {
			return item.name
		}
	}
	return "missing"
}

fn leaf_name(mut value Kind) string {
	if value !is Leaf { return "other" }
	return value.name
}

fn change_after_guard(mut value Kind) {
	if value !is Leaf { return }
	value.name = "guard"
}

fn change_in_else(mut value Kind) {
	if value !is Leaf { return } else { value.name = "else" }
}

fn repeated_test(value Kind) string {
	if value is Leaf {
		if value !is Leaf { return "other" }
		return value.name
	}
	return "other"
}

struct NodeStore { nodes []bool }
struct Store { a &NodeStore }

fn min_count(store &Store, mut nodes []bool) int {
	return if nodes.len < store.a.nodes.len { nodes.len } else { store.a.nodes.len }
}

fn maybe() ?Leaf { return Leaf{name: "optional"} }

fn optional_name(enabled bool) string {
	value := if enabled { maybe() } else { none }
	if leaf := value { return leaf.name }
	return "missing"
}

fn main() {
	leaf := Kind(Leaf{name: "ready"})
	assert name(leaf) == "ready"
	assert repeated_test(leaf) == "ready"
	assert !wrong(leaf)
	assert wrong(Kind(Link{base: leaf}))
	mut item := Kind(Link{base: Kind(Link{base: leaf})})
	assert unwrap(mut item) == "ready"
	assert name(item) == "ready"
	assert leaf_name(mut item) == "ready"
	mut guarded := Kind(Leaf{name: "old"})
	change_after_guard(mut guarded)
	assert name(guarded) == "guard"
	change_in_else(mut guarded)
	assert name(guarded) == "else"
	mut local_guard := Kind(Leaf{name: "old"})
	guard_alias := &local_guard
	if local_guard !is Leaf { return }
	local_guard.name = "local guard"
	assert name(*guard_alias) == "local guard"
	mut local_else := Kind(Leaf{name: "old"})
	else_alias := &local_else
	if local_else !is Leaf { return } else { local_else.name = "local else" }
	assert name(*else_alias) == "local else"
	mut local := Kind(Link{base: leaf})
	if local is Link {
		for local is Link { local = local.base }
		assert name(local) == "ready"
	}
	store := Store{a: &NodeStore{nodes: [true, true, true]}}
	mut flags := [true, true]
	assert min_count(&store, mut flags) == 2
	assert optional_name(true) == "optional"
	assert optional_name(false) == "missing"
	println("type flow works")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'type flow works', run.output
}
