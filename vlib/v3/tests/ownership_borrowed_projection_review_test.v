import os

const borrowed_projection_review_vexe = @VEXE
const borrowed_projection_review_tests_dir = os.dir(@FILE)
const borrowed_projection_review_v3_dir = os.dir(borrowed_projection_review_tests_dir)
const borrowed_projection_review_vlib_dir = os.dir(borrowed_projection_review_v3_dir)
const borrowed_projection_review_v3_src = os.join_path(borrowed_projection_review_v3_dir, 'v3.v')

fn test_borrowed_projection_review_regressions() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_borrowed_projection_review_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	defer {
		os.rm(v3_bin) or {}
	}
	build :=
		os.execute('${borrowed_projection_review_vexe} -nocache -gc none -d ownership -path "${borrowed_projection_review_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${borrowed_projection_review_v3_src}')
	assert build.exit_code == 0, build.output

	source := os.join_path(os.temp_dir(), 'v3_borrowed_projection_review_input_${os.getpid()}.v')
	defer {
		os.rm(source) or {}
	}
	os.write_file(source, r'
struct Payload implements IClone {
mut:
	values []string
}

const cache = Payload{
	values: ["constant"]
}

fn (value &Payload) clone() Payload {
	println("clone")
	return Payload{
		values: value.values.clone()
	}
}

struct Holder {
	left  Payload
	right Payload
}

type Entry = Payload | int

struct EntryHolder {
	entry Entry
}

struct EntryCopy {
	entry Entry
}

struct OptionalPayloads {
mut:
	items ?[]Payload
}

type PayloadMap = map[string]Payload

interface Drop {
mut:
	drop()
}

struct Tracked implements Drop {
	id    int
	drops &int
}

type TrackedMap = map[string]Tracked

fn (mut value Tracked) drop() {
	if value.drops != unsafe { nil } {
		unsafe {
			*value.drops += 1
		}
	}
}

// A `to_owned` method retains its receiver while the by-value parameter consumes the map slot.
fn (entries PayloadMap) to_owned(value Payload) {
	_ = entries
	assert value.values[0] == "moved"
}

fn (holder &Holder) accept(value Payload) string {
	return value.values[0]
}

fn (holder &Holder) accept_entry(value Entry) string {
	if value is Payload {
		return value.values[0]
	}
	return ""
}

fn (holder &Holder) consume(values ...Payload) string {
	assert values.len == 1
	return values[0].values[0]
}

fn get_entry(holder &EntryHolder) Entry {
	return holder.entry
}

fn get_optional_entry(holder &EntryHolder) ?Entry {
	return holder.entry
}

fn test_reassign(holder &Holder) {
	mut target := Payload{
		values: ["old"]
	}
	target = holder.left
	target.values[0] = "changed"
	assert holder.left.values[0] == "left"
}

fn test_multi_assign(holder &Holder) {
	mut left := Payload{
		values: ["old-left"]
	}
	mut right := Payload{
		values: ["old-right"]
	}
	left, right = holder.left, holder.right
	left.values[0] = "changed-left"
	right.values[0] = "changed-right"
	assert holder.left.values[0] == "left"
	assert holder.right.values[0] == "right"
}

fn test_distinct_dynamic_map_keys() {
	mut entries := {
		"left":  Entry(Payload{
			values: ["left"]
		})
		"right": Entry(Payload{
			values: ["right"]
		})
	}
	left_key := "left"
	right_key := "right"
	alias := &(entries[left_key] as Payload)
	entries[right_key] = alias
	mut replacement := &(entries[right_key] as Payload)
	replacement.values[0] = "replacement"
	assert (entries[left_key] as Payload).values[0] == "left"
}

fn test_distinct_static_map_keys() {
	mut entries := {
		"left":  Entry(Payload{
			values: ["left"]
		})
		"right": Entry(Payload{
			values: ["right"]
		})
	}
	alias := &(entries["left"] as Payload)
	entries["right"] = alias
	mut replacement := &(entries["right"] as Payload)
	replacement.values[0] = "replacement"
	assert (entries["left"] as Payload).values[0] == "left"
}

fn remember_alias(mut entries map[string]Entry, key string) {
	alias := &(entries[key] as Payload)
	_ = alias
}

fn replace_after_alias_function(mut entries map[string]Entry, key string, alias Entry) {
	entries[key] = alias
}

fn test_function_alias_scope() {
	mut entries := {
		"old": Entry(Payload{
			values: ["old"]
		})
	}
	remember_alias(mut entries, "old")
	replace_after_alias_function(mut entries, "old", Entry(Payload{
		values: ["new"]
	}))
	assert (entries["old"] as Payload).values[0] == "new"
}

fn replace_in_sibling_branch(mut entries map[string]Entry, key string, mut alias Entry, cond bool) {
	if cond {
		alias = &(entries[key] as Payload)
	} else {
		entries[key] = alias
	}
}

fn test_sibling_branch_alias_scope() {
	mut entries := {
		"old": Entry(Payload{
			values: ["old"]
		})
	}
	mut replacement := Entry(Payload{
		values: ["new"]
	})
	replace_in_sibling_branch(mut entries, "old", mut replacement, false)
	assert (entries["old"] as Payload).values[0] == "new"
}

fn replace_after_branch_merge(mut entries map[string]Entry, key string, mut alias Entry, cond bool) {
	if cond {
		alias = &(entries[key] as Payload)
	}
	entries[key] = alias
}

fn test_branch_alias_merge_is_conservative() {
	mut entries := {
		"old": Entry(Payload{
			values: ["old"]
		})
	}
	mut replacement := Entry(Payload{
		values: ["new"]
	})
	replace_after_branch_merge(mut entries, "old", mut replacement, false)
	assert (entries["old"] as Payload).values[0] == "new"
}

fn test_receiver_index_move_is_not_cloned() {
	mut entries := PayloadMap(map[string]Payload{})
	entries["item"] = Payload{
		values: ["moved"]
	}
	entries.to_owned(entries["item"])
}

fn test_direct_receiver_field_clone_is_retained() {
	holder := Holder{
		left: Payload{
			values: ["retained"]
		}
		right: Payload{
			values: ["other"]
		}
	}
	assert holder.accept(holder.left) == "retained"
	assert holder.left.values[0] == "retained"
}

fn test_variadic_receiver_field_clone_is_retained() {
	holder := Holder{
		left: Payload{
			values: ["retained"]
		}
		right: Payload{
			values: ["other"]
		}
	}
	assert holder.consume(holder.left) == "retained"
	assert holder.left.values[0] == "retained"
}

fn test_borrowed_append_is_cloned_once(holder &Holder) ? {
	mut items := []Payload{}
	items << (*holder).left
	items[0].values[0] = "ordinary append"
	assert holder.left.values[0] == "left"

	mut optional := OptionalPayloads{}
	optional.items = []
	optional.items? << (*holder).left
	optional_items := optional.items or { panic(err) }
	assert optional_items[0].values[0] == "left"
	assert holder.left.values[0] == "left"
}

fn test_borrowed_sum_projection_is_cloned() {
	holder := &EntryHolder{
		entry: Payload{
			values: ["original"]
		}
	}
	mut copied := EntryCopy{
		entry: holder.entry
	}
	mut copied_payload := &(copied.entry as Payload)
	copied_payload.values[0] = "struct copy"
	assert (holder.entry as Payload).values[0] == "original"

	mut returned := get_entry(holder)
	mut returned_payload := &(returned as Payload)
	returned_payload.values[0] = "return copy"
	assert (holder.entry as Payload).values[0] == "original"

	mut optional := get_optional_entry(holder) or { panic(err) }
	mut optional_payload := &(optional as Payload)
	optional_payload.values[0] = "optional copy"
	assert (holder.entry as Payload).values[0] == "original"
}

fn test_copied_index_alias_is_cloned() {
	mut entries := {
		"item": Entry(Payload{
			values: ["retained"]
		})
	}
	key := "item"
	alias := &(entries[key] as Payload)
	alias2 := alias
	entries[key] = alias2
	assert (entries[key] as Payload).values[0] == "retained"
}

fn test_distinct_dynamic_moved_map_slot_is_dropped() {
	mut left_drops := 0
	mut right_drops := 0
	mut replacement_drops := 0
	mut entries := TrackedMap(map[string]Tracked{})
	left_key := "left"
	right_key := "right"
	entries[left_key] = Tracked{
		id:    1
		drops: &left_drops
	}
	entries[right_key] = Tracked{
		id:    2
		drops: &right_drops
	}
	assert left_drops == 0
	assert right_drops == 0
	moved := entries[left_key]
	replacement := Tracked{
		id:    3
		drops: &replacement_drops
	}
	entries[right_key] = replacement
	assert moved.id == 1
	assert left_drops == 0
	assert right_drops == 1
	assert replacement_drops == 0
}

fn test_same_module_const_shadow_is_moved() {
	cache := Payload{
		values: ["local"]
	}
	dst := cache
	assert dst.values[0] == "local"
}

fn main() {
	holder := &Holder{
		left: Payload{
			values: ["left"]
		}
		right: Payload{
			values: ["right"]
		}
	}
	test_reassign(holder)
	test_multi_assign(holder)
	assert holder.accept(holder.left) == "left"
	assert holder.accept_entry(holder.left) == "left"
	test_distinct_dynamic_map_keys()
	test_distinct_static_map_keys()
	test_function_alias_scope()
	test_sibling_branch_alias_scope()
	test_branch_alias_merge_is_conservative()
	test_receiver_index_move_is_not_cloned()
	test_direct_receiver_field_clone_is_retained()
	test_variadic_receiver_field_clone_is_retained()
	test_borrowed_append_is_cloned_once(holder) or { panic(err) }
	test_borrowed_sum_projection_is_cloned()
	test_copied_index_alias_is_cloned()
	test_distinct_dynamic_moved_map_slot_is_dropped()
	test_same_module_const_shadow_is_moved()
}
')!
	for mode in ['-no-parallel', ''] {
		out := os.execute('${v3_bin} -nocache -ownership -d ownership ${mode} run ${source}')
		assert out.exit_code == 0, out.output
		assert out.output.count('clone') == 16, out.output
	}

	project := os.join_path(os.temp_dir(), 'v3_owned_const_shadow_review_${os.getpid()}')
	os.rmdir_all(project) or {}
	os.mkdir_all(os.join_path(project, 'cachemod')) or { panic(err) }
	defer {
		os.rmdir_all(project) or {}
	}
	os.write_file(os.join_path(project, 'v.mod'), "Module { name: 'owned_const_shadow' }\n")!
	os.write_file(os.join_path(project, 'cachemod', 'cachemod.v'), r'
module cachemod

pub struct Payload implements IClone {
pub mut:
	values []string
}

pub fn (value &Payload) clone() Payload {
	println("clone")
	return Payload{
		values: value.values.clone()
	}
}

pub const cache = Payload{
	values: ["constant"]
}
')!
	os.write_file(os.join_path(project, 'main.v'), r'
module main

import cachemod

fn main() {
	cache := cachemod.Payload{
		values: ["local"]
	}
	dst := cache
	assert dst.values[0] == "local"
}
')!
	for mode in ['-no-parallel', ''] {
		out := os.execute('${v3_bin} -nocache -ownership -d ownership ${mode} run ${os.join_path(project,
			'main.v')}')
		assert out.exit_code == 0, out.output
		assert out.output.count('clone') == 0, out.output
	}
}
