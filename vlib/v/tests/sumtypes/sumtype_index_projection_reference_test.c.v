// C interop files permit taking addresses of projected element expressions.
struct ProjectionItem {
mut:
	value int
}

type FixedProjection = [2]ProjectionItem | bool
type NestedProjection = [2][2]ProjectionItem | bool
type DynamicProjection = []ProjectionItem | bool
type ReferencedProjection = &[2]ProjectionItem | bool

@[noinline]
fn fixed_projection(value int) FixedProjection {
	return [ProjectionItem{value}, ProjectionItem{value + 1}]!
}

@[noinline]
fn nested_projection(value int) NestedProjection {
	return [[ProjectionItem{value}, ProjectionItem{value + 1}]!,
		[ProjectionItem{value + 2}, ProjectionItem{value + 3}]!]!
}

@[noinline]
fn escaped_projection_element(value int) &ProjectionItem {
	return &((fixed_projection(value) as [2]ProjectionItem)[0])
}

@[noinline]
fn escaped_projection_field(value int) &int {
	return &((fixed_projection(value) as [2]ProjectionItem)[1].value)
}

@[noinline]
fn escaped_nested_projection_element(value int) &ProjectionItem {
	return &((nested_projection(value) as [2][2]ProjectionItem)[1][0])
}

@[noinline]
fn escaped_projection_slice_element(value int) &ProjectionItem {
	return &((fixed_projection(value) as [2]ProjectionItem)[..][1])
}

fn test_references_to_temporary_fixed_sum_elements_escape() {
	first := escaped_projection_element(31)
	second := escaped_projection_element(71)
	field := escaped_projection_field(91)
	nested := escaped_nested_projection_element(101)
	sliced := escaped_projection_slice_element(111)
	mut retained := []&ProjectionItem{}
	for value in 0 .. 16 {
		retained << &((fixed_projection(value) as [2]ProjectionItem)[1])
	}
	assert first.value == 31
	assert second.value == 71
	assert *field == 92
	assert nested.value == 103
	assert sliced.value == 112
	for value, item in retained {
		assert item.value == value + 1
	}
}

fn counted_fixed_projection(value int, mut calls []int) FixedProjection {
	calls[0]++
	return fixed_projection(value)
}

fn projection_index(mut calls []int) int {
	calls[1]++
	return 1
}

fn test_addressed_sum_element_evaluates_source_and_index_once() {
	mut calls := [0, 0]
	item := &((counted_fixed_projection(7, mut calls) as [2]ProjectionItem)[projection_index(mut calls)])
	assert item.value == 8
	assert calls == [1, 1]
}

fn test_addressed_sum_elements_preserve_existing_storage() {
	mut value := fixed_projection(5)
	item := &((value as [2]ProjectionItem)[0])
	if mut value is [2]ProjectionItem {
		value[0].value = 17
	}
	assert item.value == 17
	mut items := [ProjectionItem{3}, ProjectionItem{4}]
	dynamic := DynamicProjection(items)
	mut dynamic_item := &((dynamic as []ProjectionItem)[1])
	dynamic_item.value = 23
	assert items[1].value == 23
	mut fixed := [ProjectionItem{29}, ProjectionItem{30}]!
	referenced := ReferencedProjection(&fixed)
	mut referenced_item := &((*(referenced as &[2]ProjectionItem))[1])
	referenced_item.value = 37
	assert fixed[1].value == 37
}
