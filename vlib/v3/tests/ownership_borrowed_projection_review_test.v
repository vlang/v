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

const cached_payloads = [Payload{
	values: ["cached"]
}]

fn (value &Payload) clone() Payload {
	println("clone")
	return Payload{
		values: value.values.clone()
	}
}

fn (value Payload) payload_text() string {
	return value.values[0]
}

struct ObservedPayload implements IClone {
mut:
	values []string
	clones &int
}

fn (value &ObservedPayload) clone() ObservedPayload {
	unsafe {
		*value.clones += 1
	}
	return ObservedPayload{
		values: value.values.clone()
		clones: value.clones
	}
}

struct Holder {
	left  Payload
	right Payload
}

struct FixedHolder {
	items [1]Payload
}

struct DynamicCopy {
mut:
	items []Payload
}

struct DynamicHolder {
	items []Payload
}

struct ObservedPair {
	left  ObservedPayload
	right ObservedPayload
}

type Entry = Payload | int

struct EntryHolder {
	entry Entry
}

struct EntryCopy {
	entry Entry
}

struct AssocCopy {
mut:
	left  Payload
	entry Entry
}

struct KeyHolder {
	key string
}

struct OptionalPayloads {
mut:
	items ?[]Payload
}

struct OptionalCopy {
mut:
	value ?Payload
}

type PayloadMap = map[string]Payload

interface Drop {
mut:
	drop()
}

interface PayloadReader {
	payload_text() string
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

struct DropObservedPayload implements IClone, Drop {
mut:
	values []string
	drops  &int
}

fn (value &DropObservedPayload) clone() DropObservedPayload {
	return DropObservedPayload{
		values: value.values.clone()
		drops:  value.drops
	}
}

fn (mut value DropObservedPayload) drop() {
	unsafe {
		*value.drops += 1
	}
	value.values = []
}

struct DropObservedHolder {
	value DropObservedPayload
}

struct DropObservedOptionalCopy {
	value ?DropObservedPayload
}

struct DefaultClonePayload {
	mut:
	values []DropObservedPayload
}

type DefaultCloneEntry = DefaultClonePayload | int

struct DefaultCloneArrayHolder {
	items []DefaultClonePayload
}

struct OwnedSlicePayload implements IClone, Drop {
mut:
	values []string
	clones &int
	drops  &int
}

fn (value &OwnedSlicePayload) clone() OwnedSlicePayload {
	unsafe {
		*value.clones += 1
	}
	return OwnedSlicePayload{
		values: value.values.clone()
		clones: value.clones
		drops:  value.drops
	}
}

fn (mut value OwnedSlicePayload) drop() {
	unsafe {
		*value.drops += 1
	}
	value.values = []
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

fn consume_payloads(values ...Payload) string {
	assert values.len == 1
	return values[0].values[0]
}

fn consume_payload_reader(value PayloadReader) string {
	return value.payload_text()
}

fn (value &ObservedPayload) accept(other ObservedPayload) string {
	assert value.values[0] == "left"
	return other.values[0]
}

fn get_entry(holder &EntryHolder) Entry {
	return holder.entry
}

fn get_optional_entry(holder &EntryHolder) ?Entry {
	return holder.entry
}

fn get_fixed_items(holder &FixedHolder) []Payload {
	return holder.items
}

fn consume_fixed_items(items []Payload) {
	mut item := items[0]
	item.values[0] = "argument copy"
}

fn consume_cached_payloads(items []Payload) string {
	return items[0].values[0]
}

fn consume_fixed_entries(items []Entry) {
	mut item := &(items[0] as Payload)
	item.values[0] = "converted argument copy"
}

fn copy_payload_pointer(value &Payload) Payload {
	return *value
}

fn maybe_payload() ?Payload {
	return none
}

fn maybe_payload_pair() ?(Payload, Payload) {
	return none
}

fn maybe_entry() ?Entry {
	return none
}

fn select_payload(holder &Holder, pick_left bool) Payload {
	return if pick_left {
		holder.left
	} else {
		holder.right
	}
}

fn apply_entry_map(mut entries map[string]Entry, callback fn (mut map[string]Entry)) {
	callback(mut entries)
}

fn test_reassign(holder &Holder) {
	mut target := Payload{
		values: ["old"]
	}
	target = holder.left
	target.values[0] = "changed"
	assert holder.left.values[0] == "left"
}

fn replace_borrowed_payload(mut target Payload, holder &Holder) {
	target = holder.left
	target.values[0] = "changed through mut parameter"
}

fn test_mut_value_param_borrowed_reassign(holder &Holder) {
	mut target := Payload{
		values: ["old"]
	}
	replace_borrowed_payload(mut target, holder)
	assert target.values[0] == "changed through mut parameter"
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

fn test_free_variadic_borrowed_projection_is_cloned(holder &Holder) {
	assert consume_payloads(holder.left) == "left"
	assert holder.left.values[0] == "left"
}

fn test_borrowed_interface_argument_is_cloned(holder &Holder) {
	assert consume_payload_reader(holder.left) == "left"
	assert holder.left.values[0] == "left"
}

fn test_disjoint_receiver_fields_are_not_cloned() {
	mut clones := 0
	pair := ObservedPair{
		left: ObservedPayload{
			values: ["left"]
			clones: &clones
		}
		right: ObservedPayload{
			values: ["right"]
			clones: &clones
		}
	}
	assert pair.left.accept(pair.right) == "right"
	assert clones == 0
	assert pair.left.values[0] == "left"
}

fn test_borrowed_fixed_array_conversions_are_cloned() {
	holder := &FixedHolder{
		items: [Payload{
			values: ["original"]
		}]!
	}
	mut copied := DynamicCopy{
		items: holder.items
	}
	mut copied_item := copied.items[0]
	copied_item.values[0] = "struct copy"
	assert holder.items[0].values[0] == "original"

	mut returned := get_fixed_items(holder)
	mut returned_item := returned[0]
	returned_item.values[0] = "return copy"
	assert holder.items[0].values[0] == "original"

	consume_fixed_items(holder.items)
	assert holder.items[0].values[0] == "original"

	consume_fixed_entries(holder.items)
	assert holder.items[0].values[0] == "original"
}

fn test_borrowed_dynamic_array_conversion_is_cloned() {
	holder := &DynamicHolder{
		items: [Payload{
			values: ["original"]
		}]
	}
	consume_fixed_entries(holder.items)
	assert holder.items[0].values[0] == "original"
}

fn test_pointer_backed_index_read_is_cloned() {
	holder := &DynamicHolder{
		items: [Payload{
			values: ["original"]
		}]
	}
	mut copied := holder.items[0]
	copied.values[0] = "copy"
	assert holder.items[0].values[0] == "original"
}

fn test_borrowed_array_push_many_is_cloned() ? {
	holder := &DynamicHolder{
		items: [Payload{
			values: ["original"]
		}]
	}
	mut items := []Payload{}
	items << holder.items
	items[0].values[0] = "ordinary push many"
	assert holder.items[0].values[0] == "original"

	mut optional := OptionalPayloads{}
	optional.items = []
	optional.items? << holder.items
	mut optional_items := optional.items or { panic(err) }
	optional_items[0].values[0] = "optional push many"
	assert holder.items[0].values[0] == "original"
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

fn test_borrowed_prepend_and_insert_are_cloned(holder &Holder) {
	mut items := []Payload{}
	items.prepend(holder.left)
	items[0].values[0] = "prepended"
	assert holder.left.values[0] == "left"

	items.insert(0, holder.right)
	items[0].values[0] = "inserted"
	assert holder.right.values[0] == "right"
}

fn test_borrowed_bulk_prepend_and_insert_are_cloned() {
	holder := &DynamicHolder{
		items: [Payload{
			values: ["original"]
		}]
	}
	mut prepended := []Payload{}
	prepended.prepend(holder.items)
	prepended[0].values[0] = "bulk prepended"
	assert holder.items[0].values[0] == "original"

	mut inserted := []Payload{}
	inserted.insert(0, holder.items)
	inserted[0].values[0] = "bulk inserted"
	assert holder.items[0].values[0] == "original"
}

fn test_borrowed_array_initializer_is_cloned_per_element(holder &Holder) {
	mut items := []Payload{len: 2, init: holder.left}
	items[0].values[0] = "first"
	assert items[1].values[0] == "left"
	assert holder.left.values[0] == "left"
}

fn test_borrowed_array_literal_element_is_cloned(holder &Holder) {
	mut items := [holder.left]
	items[0].values[0] = "literal copy"
	assert holder.left.values[0] == "left"
}

fn test_borrowed_fixed_array_literal_element_is_cloned(holder &Holder) {
	mut items := [holder.left]!
	items[0].values[0] = "fixed literal copy"
	assert holder.left.values[0] == "left"
}

fn test_borrowed_fixed_array_initializer_is_cloned_per_element(holder &Holder) {
	mut items := [2]Payload{init: holder.left}
	items[0].values[0] = "fixed first"
	assert items[1].values[0] == "left"
	assert holder.left.values[0] == "left"
}

fn test_borrowed_optional_struct_field_is_cloned(holder &Holder) {
	optional_copy := OptionalCopy{
		value: holder.left
	}
	mut value := optional_copy.value or { panic(err) }
	value.values[0] = "optional copy"
	assert holder.left.values[0] == "left"
}

fn test_borrowed_optional_wrapper_is_not_dropped() {
	mut drops := 0
	holder := &DropObservedHolder{
		value: DropObservedPayload{
			values: ["owned"]
			drops:  &drops
		}
	}
	optional_copy := DropObservedOptionalCopy{
		value: holder.value
	}
	assert optional_copy.value != none
	assert drops == 0
}

fn test_borrowed_channel_send_is_cloned(holder &Holder) {
	ch := chan Payload{cap: 1}
	ch <- holder.left
	mut received := <-ch
	received.values[0] = "channel copy"
	assert holder.left.values[0] == "left"

	handled_ch := chan Payload{cap: 1}
	handled_ch <- holder.right or { panic(err) }
	mut handled := <-handled_ch
	handled.values[0] = "handled channel copy"
	assert holder.right.values[0] == "right"
}

fn test_borrowed_map_literal_entries_are_cloned(holder &Holder) {
	mut items := {"payload": holder.left}
	mut item := items["payload"]
	item.values[0] = "map copy"
	assert holder.left.values[0] == "left"

	key_holder := &KeyHolder{
		key: ["borrowed", "key"].join("-")
	}
	{
		keyed := {key_holder.key: 1}
		assert keyed[key_holder.key] == 1
	}
	assert key_holder.key == "borrowed-key"
}

fn test_borrowed_map_assignment_is_cloned(holder &Holder) {
	mut items := map[string]Payload{}
	key := "payload"
	items[key] = holder.left
	mut item := items[key]
	item.values[0] = "map assignment copy"
	assert holder.left.values[0] == "left"
}

fn test_map_fixed_array_assignment_clones_borrowed_rhs(holder &Holder) {
	key := "payload"
	mut items := map[string][1]Payload{}
	items[key] = [Payload{
		values: ["old"]
	}]!
	items[key][0] = holder.left
	items[key][0].values[0] = "map copy"
	assert holder.left.values[0] == "left"
}

fn test_borrowed_assoc_overrides_are_cloned(holder &Holder) {
	base := AssocCopy{
		left: Payload{
			values: ["base"]
		}
		entry: 0
	}
	mut copied := AssocCopy{
		...base
		left:  holder.left
		entry: holder.right
	}
	copied.left.values[0] = "assoc copy"
	mut entry := &(copied.entry as Payload)
	entry.values[0] = "assoc sum copy"
	assert holder.left.values[0] == "left"
	assert holder.right.values[0] == "right"
}

fn test_pointer_dereferences_are_cloned(holder &Holder) {
	left_pointer := &holder.left
	mut left_copy := *left_pointer
	left_copy.values[0] = "dereference copy"
	assert holder.left.values[0] == "left"

	mut right_copy := copy_payload_pointer(&holder.right)
	right_copy.values[0] = "return copy"
	assert holder.right.values[0] == "right"
}

fn test_conditional_borrowed_branches_are_cloned(holder &Holder) {
	mut left := select_payload(holder, true)
	left.values[0] = "conditional left"
	assert holder.left.values[0] == "left"

	mut right := select_payload(holder, false)
	right.values[0] = "conditional right"
	assert holder.right.values[0] == "right"
}

fn test_multi_conditional_borrowed_branches_are_cloned(holder &Holder, cond bool) {
	mut first := Payload{
		values: ["first"]
	}
	mut second := Payload{
		values: ["second"]
	}
	first, second = if cond {
		holder.left
		holder.right
	} else {
		holder.right
		holder.left
	}
	first.values[0] = "conditional first"
	second.values[0] = "conditional second"
	assert holder.left.values[0] == "left"
	assert holder.right.values[0] == "right"
}

fn test_borrowed_or_fallback_is_cloned(holder &Holder) {
	mut fallback := maybe_payload() or { holder.left }
	fallback.values[0] = "fallback copy"
	assert holder.left.values[0] == "left"

	mut entry := maybe_entry() or { holder.right }
	mut entry_payload := &(entry as Payload)
	entry_payload.values[0] = "sum fallback copy"
	assert holder.right.values[0] == "right"

	mut first, mut second := maybe_payload_pair() or { holder.left, holder.right }
	first.values[0] = "first fallback copy"
	second.values[0] = "second fallback copy"
	assert holder.left.values[0] == "left"
	assert holder.right.values[0] == "right"
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

fn make_owned_entry() Entry {
	return Payload{
		values: ["owned temporary"]
	}
}

fn test_owned_rvalue_sum_projection_is_moved() {
	mut payload := make_owned_entry() as Payload
	payload.values[0] = "moved"
	assert payload.values[0] == "moved"
}

fn make_owned_slice_payloads(clones &int, drops &int) []OwnedSlicePayload {
	return [OwnedSlicePayload{
		values: ["owned slice temporary"]
		clones: clones
		drops:  drops
	}]
}

fn consume_owned_rvalue_slice(clones &int, drops &int) {
	mut copied := make_owned_slice_payloads(clones, drops)[..]
	assert unsafe { *clones } == 1
	copied[0].values[0] = "consumed"
	assert copied[0].values[0] == "consumed"
}

fn test_owned_rvalue_slice_projection_is_consumed() {
	mut clones := 0
	mut drops := 0
	consume_owned_rvalue_slice(&clones, &drops)
	assert clones == 1
	assert drops == 2
}

fn test_nonaddressable_borrowed_projections_are_stable() {
	mut sum_drops := 0
	entry := DefaultCloneEntry(DefaultClonePayload{
		values: [DropObservedPayload{
			values: ["sum original"]
			drops:  &sum_drops
		}]
	})
	mut copied_entry := entry as DefaultClonePayload
	assert sum_drops == 0
	copied_entry.values[0].values[0] = "sum copy"
	assert (entry as DefaultClonePayload).values[0].values[0] == "sum original"

	mut slice_drops := 0
	holder := &DefaultCloneArrayHolder{
		items: [DefaultClonePayload{
			values: [DropObservedPayload{
				values: ["slice original"]
				drops:  &slice_drops
			}]
		}]
	}
	mut copied_items := holder.items[..]
	assert slice_drops == 0
	copied_items[0].values[0].values[0] = "slice copy"
	assert holder.items[0].values[0].values[0] == "slice original"
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

fn test_long_index_alias_chain_is_cloned() {
	mut entries := {
		"item": Entry(Payload{
			values: ["retained"]
		})
	}
	key := "item"
	p0 := &(entries[key] as Payload)
	p1 := p0
	p2 := p1
	p3 := p2
	p4 := p3
	p5 := p4
	p6 := p5
	p7 := p6
	p8 := p7
	entries[key] = p8
	assert (entries[key] as Payload).values[0] == "retained"
}

fn test_shadowed_index_alias_is_restored() {
	mut entries := {
		"item": Entry(Payload{
			values: ["retained"]
		})
	}
	key := "item"
	alias := &(entries[key] as Payload)
	{
		alias := &Payload{
			values: ["shadow"]
		}
		assert alias.values[0] == "shadow"
	}
	entries[key] = alias
	assert (entries[key] as Payload).values[0] == "retained"
}

fn test_multi_assigned_index_alias_is_cloned() {
	mut entries := {
		"item": Entry(Payload{
			values: ["retained"]
		})
	}
	key := "item"
	mut alias := &Payload{
		values: ["placeholder"]
	}
	mut n := 0
	alias, n = &(entries[key] as Payload), 1
	assert n == 1
	entries[key] = alias
	assert (entries[key] as Payload).values[0] == "retained"
}

fn test_fn_literal_captured_index_alias_is_cloned() {
	mut entries := {
		"item": Entry(Payload{
			values: ["retained"]
		})
	}
	key := "item"
	alias := &(entries[key] as Payload)
	callback := fn [mut entries, alias, key] () {
		entries[key] = alias
		assert (entries[key] as Payload).values[0] == "retained"
	}
	callback()
}

fn test_lambda_captured_index_alias_is_cloned() {
	mut entries := {
		"item": Entry(Payload{
			values: ["retained"]
		})
	}
	key := "item"
	alias := &(entries[key] as Payload)
	apply_entry_map(mut entries, |mut entries| {
		entries[key] = alias
		assert (entries[key] as Payload).values[0] == "retained"
	})
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

fn test_same_typed_const_array_argument_is_cloned() {
	assert consume_cached_payloads(cached_payloads) == "cached"
	assert consume_cached_payloads(cached_payloads) == "cached"
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
	test_mut_value_param_borrowed_reassign(holder)
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
	test_free_variadic_borrowed_projection_is_cloned(holder)
	test_borrowed_interface_argument_is_cloned(holder)
	test_disjoint_receiver_fields_are_not_cloned()
	test_borrowed_fixed_array_conversions_are_cloned()
	test_borrowed_dynamic_array_conversion_is_cloned()
	test_pointer_backed_index_read_is_cloned()
	test_borrowed_array_push_many_is_cloned() or { panic(err) }
	test_borrowed_append_is_cloned_once(holder) or { panic(err) }
	test_borrowed_prepend_and_insert_are_cloned(holder)
	test_borrowed_bulk_prepend_and_insert_are_cloned()
	test_borrowed_array_initializer_is_cloned_per_element(holder)
	test_borrowed_array_literal_element_is_cloned(holder)
	test_borrowed_fixed_array_literal_element_is_cloned(holder)
	test_borrowed_fixed_array_initializer_is_cloned_per_element(holder)
	test_borrowed_optional_struct_field_is_cloned(holder)
	test_borrowed_optional_wrapper_is_not_dropped()
	test_borrowed_channel_send_is_cloned(holder)
	test_borrowed_map_literal_entries_are_cloned(holder)
	test_borrowed_map_assignment_is_cloned(holder)
	test_map_fixed_array_assignment_clones_borrowed_rhs(holder)
	test_borrowed_assoc_overrides_are_cloned(holder)
	test_pointer_dereferences_are_cloned(holder)
	test_conditional_borrowed_branches_are_cloned(holder)
	test_multi_conditional_borrowed_branches_are_cloned(holder, true)
	test_borrowed_or_fallback_is_cloned(holder)
	test_borrowed_sum_projection_is_cloned()
	test_owned_rvalue_sum_projection_is_moved()
	test_owned_rvalue_slice_projection_is_consumed()
	test_nonaddressable_borrowed_projections_are_stable()
	test_copied_index_alias_is_cloned()
	test_long_index_alias_chain_is_cloned()
	test_shadowed_index_alias_is_restored()
	test_multi_assigned_index_alias_is_cloned()
	test_fn_literal_captured_index_alias_is_cloned()
	test_lambda_captured_index_alias_is_cloned()
	test_distinct_dynamic_moved_map_slot_is_dropped()
	test_same_module_const_shadow_is_moved()
	test_same_typed_const_array_argument_is_cloned()
}
')!
	for mode in ['-no-parallel', ''] {
		out := os.execute('${v3_bin} -nocache -ownership -d ownership ${mode} run ${source}')
		assert out.exit_code == 0, out.output
		assert out.output.split_into_lines().filter(it == 'clone').len == 62, out.output
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
