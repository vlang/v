struct InlinePair {
mut:
	x int
	y int
}

type InlineValue = int | InlinePair | string

fn inline_value(x int) InlineValue {
	return InlinePair{ x: x, y: x + 1 }
}

fn test_sumtype_inline_copy_and_mutation() {
	mut source := inline_value(7)
	copy := source
	if mut source is InlinePair {
		source.x = 42
	}
	assert (source as InlinePair).x == 42
	assert (copy as InlinePair).x == 7
	values := [copy, InlineValue(3), InlineValue('hello')]
	assert (values[0] as InlinePair).y == 8
	assert values[1] == InlineValue(3)
	assert values[2].str() == "InlineValue('hello')"
}

struct InlineLink {
	value int
	next  ?&InlineTree
}

type InlineTree = int | InlineLink

fn test_sumtype_explicit_recursive_reference() {
	leaf := InlineTree(9)
	tree := InlineTree(InlineLink{ value: 4, next: &leaf })
	link := tree as InlineLink
	next := link.next or { panic('missing child') }
	assert *next == InlineTree(9)
}

type InlineFixed = [3]int | bool

fn test_sumtype_fixed_array_value_copy() {
	items := [1, 2, 3]!
	value := InlineFixed(items)
	copy := value
	assert (copy as [3]int) == items
}

@[noinline]
fn escaped_inline_pair() &InlinePair {
	value := inline_value(19)
	if value is InlinePair {
		return &value
	}
	panic('unexpected variant')
}

fn test_sumtype_escaping_variant_reference() {
	escaped := escaped_inline_pair()
	mut values := []InlineValue{}
	for i in 0 .. 100 {
		values << inline_value(i)
	}
	assert escaped.x == 19
	assert escaped.y == 20
	assert (values[99] as InlinePair).x == 99
}

type InlineGeneric[T] = bool | T

struct FiniteNestedGeneric {
	child InlineGeneric[int]
}

fn test_sumtype_nested_generic_has_finite_layout() {
	assert sizeof(InlineGeneric[FiniteNestedGeneric]) > sizeof(FiniteNestedGeneric)
}

type InlineInner = int | string

struct InlineRecord {
	value InlineInner
}

type InlineArraySum = [2]InlineRecord | bool

fn test_sumtype_array_variant_orders_nested_value_dependencies() {
	first := InlineRecord{ value: InlineInner(4) }
	second := InlineRecord{ value: InlineInner(8) }
	records := [first, second]!
	value := InlineArraySum(records)
	copy := value as [2]InlineRecord
	assert copy[0].value == InlineInner(4)
	assert copy[1].value == InlineInner(8)
}

type InlineOuter = InlineValue | bool

fn replace_inline(mut value InlineValue) {
	value = InlinePair{ x: 91, y: 92 }
}

fn test_sumtype_mutable_nested_variant_borrows_original_storage() {
	mut outer := InlineOuter(inline_value(3))
	if mut outer is InlineValue {
		replace_inline(mut outer)
		assert (outer as InlinePair).x == 91
	} else {
		assert false
	}
	inner := outer as InlineValue
	assert (inner as InlinePair).y == 92
}

type InlineChoice[T] = int | T

fn test_sumtype_value_and_pointer_variants_are_distinct() {
	value := 42
	scalar := InlineChoice[&int](value)
	pointer := InlineChoice[&int](&value)
	assert scalar is int
	assert pointer is &int
	assert (scalar as int) == 42
	assert (pointer as &int) == &value
}

@[noinline]
fn escaped_pointer_variant() InlineChoice[&int] {
	value := 37
	return InlineChoice[&int](&value)
}

fn test_sumtype_pointer_variant_retains_explicit_reference() {
	value := escaped_pointer_variant()
	assert value is &int
	assert *(value as &int) == 37
}

fn inline_fixed_total(first &InlineFixed, second &InlineFixed) int {
	left := first as [3]int
	right := second as [3]int
	return left[0] + right[2]
}

fn test_sumtype_constructor_borrow_has_call_lifetime() {
	first := [2, 3, 5]!
	second := [7, 11, 13]!
	assert inline_fixed_total(InlineFixed(first), InlineFixed(second)) == 15
}

struct InlineWideField {
	padding string
mut:
	value int
}

struct InlineNarrowField {
mut:
	value int
}

type InlineOffsetValue = InlineWideField | InlineNarrowField | bool

fn inline_offset_read(value InlineOffsetValue) int {
	return match value {
		InlineWideField, InlineNarrowField { value.value }
		bool { -1 }
	}
}

fn inline_offset_increment(mut value InlineOffsetValue) {
	match mut value {
		InlineWideField, InlineNarrowField { value.value += 1 }
		bool {}
	}
}

fn test_multi_variant_match_keeps_each_field_layout() {
	mut values := [
		InlineOffsetValue(InlineWideField{'padding', 17}),
		InlineOffsetValue(InlineNarrowField{29}),
		InlineOffsetValue(false),
	]
	before := [17, 29, -1]
	after := [18, 30, -1]
	for i, mut value in values {
		assert inline_offset_read(value) == before[i]
		inline_offset_increment(mut value)
		assert inline_offset_read(value) == after[i]
	}
}

fn (value InlineValue) scalar() int {
	return match value {
		int { value }
		else { -1 }
	}
}

fn inline_snapshot(value InlineValue, mut source InlineValue) int {
	source = InlineValue(100)
	return value.scalar()
}

fn inline_scalar(value InlineValue) int {
	return value.scalar()
}

fn inline_retained_parameter(value InlineValue) &InlineValue {
	return &value
}

interface InlineVisitor {
	visit(value InlineValue) int
}

struct InlineVisitorImpl {}

fn (visitor InlineVisitorImpl) visit(value InlineValue) int {
	return value.scalar()
}

fn test_sumtype_argument_is_a_value_snapshot() {
	mut value := InlineValue(23)
	assert inline_snapshot(value, mut value) == 23
	assert value == InlineValue(100)
	callback := inline_scalar
	assert callback(InlineValue(41)) == 41
	visitor := InlineVisitor(InlineVisitorImpl{})
	assert visitor.visit(InlineValue(43)) == 43
}

fn test_sumtype_parameter_reference_outlives_the_call() {
	kept := inline_retained_parameter(InlineValue(47))
	mut values := []InlineValue{}
	for i in 0 .. 100 {
		values << inline_value(i)
	}
	assert kept.scalar() == 47
	assert (values[99] as InlinePair).x == 99
}

fn test_sumtype_spawn_owns_argument_storage() {
	worker := spawn inline_scalar(InlineValue(53))
	assert worker.wait() == 53
}

type InlineAlias = InlineValue

fn inline_alias_scalar(value InlineAlias) int {
	return InlineValue(value).scalar()
}

fn test_sumtype_alias_callback_and_bound_method() {
	callback := inline_alias_scalar
	assert callback(InlineAlias(InlineValue(59))) == 59
	visitor := InlineVisitorImpl{}
	bound := visitor.visit
	assert bound(InlineValue(61)) == 61
}

struct InlinePointerHolder {
	value &InlineValue
}

fn test_sumtype_pointer_field_cast_reads_the_complete_value() {
	stored := &InlineValue(67)
	holder := InlinePointerHolder{stored}
	if holder.value is int {
		copy := InlineValue(holder.value)
		assert copy.scalar() == 67
		assert InlineValue(holder.value).scalar() == 67
	}
}

type InlineSelfReference = int | &InlineSelfReference

fn test_sumtype_explicit_recursive_pointer_variant_keeps_its_tag() {
	value := InlineSelfReference(71)
	pointer := &value
	wrapped := InlineSelfReference(pointer)
	assert wrapped is &InlineSelfReference
	stored := wrapped as &InlineSelfReference
	assert *stored == value
}

type InlineReferenceDepth = InlinePair | &InlinePair | &&InlinePair

fn test_sumtype_pointer_depth_is_part_of_variant_identity() {
	pair := InlinePair{3, 5}
	pointer := &pair
	double_pointer := &pointer
	value := InlineReferenceDepth(pair)
	reference := InlineReferenceDepth(pointer)
	double_reference := InlineReferenceDepth(double_pointer)
	assert value is InlinePair
	assert reference is &InlinePair
	assert double_reference is &&InlinePair
	assert (reference as &InlinePair) == pointer
	assert (double_reference as &&InlinePair) == double_pointer
}

@[noinline]
fn inline_numeric_reference() &InlineValue {
	return &InlineValue(0)
}

fn test_explicit_numeric_sum_reference_owns_its_storage() {
	value := inline_numeric_reference()
	assert value is int
	assert value.scalar() == 0
}

@[noinline]
fn inline_fixed_reference() &InlineFixed {
	values := [2, 3, 5]!
	return &InlineFixed(values)
}

fn test_explicit_fixed_array_sum_reference_owns_its_storage() {
	value := inline_fixed_reference()
	assert ((*value) as [3]int) == [2, 3, 5]!
	worker := spawn inline_retained_parameter(InlineValue(79))
	retained := worker.wait()
	assert retained.scalar() == 79
}

struct InlineScalarArrays {
mut:
	values []InlineFixed
}

fn test_scalar_sum_array_storage_and_reference_retention() {
	mut scalars := []InlineFixed{}
	mut pairs := []InlinePair{}
	mut references := []InlineValue{}
	mut holder := InlineScalarArrays{}
	for index in 0 .. 128 {
		scalars << InlineFixed([index, index + 1, index + 2]!)
		pairs << InlinePair{ x: index }
		references << InlineValue(index.str().repeat(64))
		holder.values << InlineFixed(true)
	}
	gc_collect()
	for index in 0 .. 128 {
		assert scalars[index] == InlineFixed([index, index + 1, index + 2]!)
		assert pairs[index].x == index
		assert references[index] == InlineValue(index.str().repeat(64))
	}
	$if gcboehm_opt ? {
		assert scalars.flags.has(.noscan_data)
		assert pairs.flags.has(.noscan_data)
		assert holder.values.flags.has(.noscan_data)
		assert !references.flags.has(.noscan_data)
	}
}

fn test_scalar_option_array_storage_preserves_some_and_none() {
	mut values := []?InlinePair{}
	for index in 0 .. 128 {
		values << InlinePair{ x: index }
	}
	values << none
	gc_collect()
	for index in 0 .. 128 {
		value := values[index] or { panic('missing value') }
		assert value.x == index
	}
	assert values.last() == none
	$if gcboehm_opt ? {
		assert values.flags.has(.noscan_data)
	}
}
