@[heap]
struct Matrix[T] {
	data []T
}

fn (m &Matrix[T]) get() T { return m.data[0] }

fn split[T](m &Matrix[T]) !(&Matrix[T], &Matrix[T]) { return m, m }

fn compute[T](n T) !(T, T) {
	m := &Matrix[T]{ data: [n] }
	q, r := split(m)!
	x := q.get()
	y := r.get()
	return x, y
}

fn test_generic_tuple_receiver() {
	a, b := compute(f64(42))!
	assert a == 42
	assert b == 42
}

fn split_plain[T](m &Matrix[T]) (&Matrix[T], &Matrix[T]) {
	return m, m
}

fn select_second[A, B](first A, second B) B {
	_ = first
	m := &Matrix[B]{ data: [second] }
	q, _ := split_plain(m)
	return q.get()
}

fn test_generic_tuple_receiver_uses_second_generic_argument() {
	assert select_second(1, 'okay') == 'okay'
}

fn (m &Matrix[T]) pair() !(T, T) {
	return m.data[0], m.data[0]
}

fn pair_from_second[A, B](first A, second B) !(B, B) {
	_ = first
	m := &Matrix[B]{ data: [second] }
	q, _ := split_plain(m)
	return q.pair()!
}

fn test_generic_result_tuple_method_uses_second_generic_argument() {
	a, b := pair_from_second(1, 'okay')!
	assert a == 'okay'
	assert b == 'okay'
}

fn pair_local_from_first[A, B](first A, second B) !(B, B) {
	m := &Matrix[A]{ data: [first] }
	q, _ := split_plain(m)
	x, y := q.pair()!
	_ = x
	_ = y
	return second, second
}

fn test_generic_result_tuple_method_local_does_not_use_enclosing_return() {
	a, b := pair_local_from_first(1, 'okay')!
	assert a == 'okay'
	assert b == 'okay'
}

interface Named {
	name() string
}

struct NamedItem {
	label string
}

fn (item NamedItem) name() string {
	return item.label
}

fn widened_tuple_receiver[T](item T) !Named {
	m := &Matrix[T]{ data: [item] }
	q, _ := split(m)!
	return q.get()
}

fn test_generic_result_tuple_receiver_keeps_type_for_interface_return() {
	result := widened_tuple_receiver(NamedItem{ label: 'kept' })!
	assert result.name() == 'kept'
}

fn split_heterogeneous[T](m &Matrix[T]) !(&Matrix[T], int) {
	return m, 7
}

fn split_heterogeneous_last[T](m &Matrix[T]) !(int, &Matrix[T]) {
	return 7, m
}

fn heterogeneous_first[A, B](first A, second B) !B {
	_ = first
	m := &Matrix[B]{ data: [second] }
	q, _ := split_heterogeneous(m)!
	return q.get()
}

fn heterogeneous_last[A, B](first A, second B) !B {
	_ = first
	m := &Matrix[B]{ data: [second] }
	_, q := split_heterogeneous_last(m)!
	return q.get()
}

fn test_heterogeneous_result_tuple_receiver_preserves_slot_type() {
	assert heterogeneous_first(1, 'first')! == 'first'
	assert heterogeneous_last(1, 'last')! == 'last'
}

struct TupleFactory[T] {}

fn (f TupleFactory[T]) make[U]() ?U {
	return U{}
}

fn option_from_third[A, B, C](first A, second B, third C) ?C {
	_ = first
	_ = second
	_ = third
	factory := TupleFactory[A]{}
	return factory.make()?
}

fn test_generic_option_method_uses_enclosing_return_context() {
	assert option_from_third(1, true, 'value')? == ''
}

fn contextual_zero[T]() T {
	return T{}
}

fn contextual_result_zero[T](fail bool) !T {
	if fail { return error('fallback') }
	return T{}
}

fn contextual_if[A, B](flag bool, first A, second B) B {
	_ = first
	return if flag { contextual_zero() } else { second }
}

fn contextual_match[A, B](choice int, first A, second B) B {
	_ = first
	return match choice {
		0 { contextual_zero() }
		1 {
			if true { contextual_zero() } else { second }
		}
		else { second }
	}
}

fn contextual_folded_if[A, B](first A, second B) B {
	_ = first
	return if B.name == 'string' { contextual_zero() } else { second }
}

fn contextual_or[A, B](fail bool, first A, second B) B {
	_ = first
	_ = second
	return contextual_result_zero(fail) or { contextual_zero() }
}

fn test_generic_contextual_returns_reach_branch_tails_and_or_values() {
	assert contextual_if(true, 1, 'value') == ''
	assert contextual_if(false, 1, 'value') == 'value'
	assert contextual_match(0, 1, 'value') == ''
	assert contextual_match(1, 1, 'value') == ''
	assert contextual_match(2, 1, 'value') == 'value'
	assert contextual_folded_if(1, 'value') == ''
	assert contextual_or(false, 1, 'value') == ''
	assert contextual_or(true, 1, 'value') == ''
}

struct ContextualGuard {
mut:
	value int
}

fn contextual_rlock[A, B](shared guard ContextualGuard, first A, second B) B {
	_ = first
	_ = second
	return rlock guard {
		contextual_zero()
	}
}

fn contextual_lock[A, B](shared guard ContextualGuard, first A, second B) B {
	_ = first
	_ = second
	return lock guard {
		guard.value++
		contextual_zero()
	}
}

fn test_generic_contextual_returns_reach_lock_expression_body() {
	shared guard := ContextualGuard{}
	assert contextual_rlock(shared guard, 1, 'value') == ''
	assert contextual_lock(shared guard, 1, 'value') == ''
	rlock guard {
		assert guard.value == 1
	}
}

fn contextual_dump[A, B](first A, second B) B {
	_ = first
	_ = second
	return dump(contextual_zero())
}

fn test_generic_contextual_returns_reach_dump_value() {
	assert contextual_dump(1, 'value') == ''
}

type ContextualVariantSum = int | string

fn variant_contextual_zero[T]() T {
	return T{}
}

fn contextual_variant[A, B](value ContextualVariantSum, first A, second B) B {
	_ = first
	$for variant in value.variants {
		if value is variant {
			return variant_contextual_zero()
		}
	}
	return second
}

fn contextual_variant_dump[A, B](value ContextualVariantSum, first A, second B) B {
	_ = first
	$for variant in value.variants {
		if value is variant {
			return if true { dump(variant_contextual_zero()) } else { second }
		}
	}
	return second
}

fn test_generic_contextual_returns_survive_variant_smartcast_clones() {
	for value in [ContextualVariantSum(7), ContextualVariantSum('sum')] {
		assert contextual_variant(value, 1, 'value') == ''
		assert contextual_variant_dump(value, 1, 'value') == ''
	}
}

fn contextual_prefix_value[T]() T {
	$if T is f64 {
		return T(1.5)
	} $else $if T is u64 {
		return T(0x1_0000_0007)
	} $else {
		return T(7)
	}
}

fn contextual_negative[A, B](first A, second B) B {
	_ = first
	_ = second
	return -contextual_prefix_value()
}

fn contextual_positive[A, B](first A, second B) B {
	_ = first
	_ = second
	return +contextual_prefix_value()
}

fn contextual_complement[A, B](first A, second B) B {
	_ = first
	_ = second
	return ~contextual_prefix_value()
}

fn test_generic_contextual_returns_reach_arithmetic_prefixes() {
	assert contextual_negative(1, f64(0)) == -1.5
	assert contextual_positive(1, f64(0)) == 1.5
	assert contextual_complement(1, u64(0)) == ~u64(0x1_0000_0007)
}

fn contextual_infix_value[T]() T {
	$if T is f64 {
		return T(1.5)
	} $else $if T is u64 {
		return T(0x1_0000_0007)
	} $else $if T is ContextualFlags {
		return T(ContextualFlags.first)
	} $else $if T is string {
		return T('c')
	} $else {
		return T(7)
	}
}

fn contextual_infix_add[A, B](first A, second B) B {
	_ = first
	return contextual_infix_value() + second
}

fn contextual_infix_subtract[A, B](first A, second B) B {
	_ = first
	return contextual_infix_value() - second
}

fn contextual_infix_multiply[A, B](first A, second B) B {
	_ = first
	return contextual_infix_value() * second
}

fn contextual_infix_divide[A, B](first A, second B) B {
	_ = first
	return contextual_infix_value() / second
}

fn contextual_infix_modulo[A, B](first A, second B) B {
	_ = first
	return contextual_infix_value() % second
}

fn contextual_infix_and[A, B](first A, second B) B {
	_ = first
	return contextual_infix_value() & second
}

fn contextual_infix_or[A, B](first A, second B) B {
	_ = first
	return contextual_infix_value() | second
}

fn contextual_infix_xor[A, B](first A, second B) B {
	_ = first
	return contextual_infix_value() ^ second
}

fn contextual_infix_left_shift[A, B](first A, second B) B {
	_ = first
	return contextual_infix_value() << second
}

fn contextual_infix_right_shift[A, B](first A, second B) B {
	_ = first
	return contextual_infix_value() >> second
}

fn contextual_infix_rhs[A, B](first A, second B) B {
	_ = first
	return second - contextual_infix_value()
}

fn test_generic_contextual_returns_reach_numeric_infix_operands() {
	assert contextual_infix_add(1, f64(2)) == 3.5
	assert contextual_infix_add(1, 'b') == 'cb'
	assert contextual_infix_subtract(1, f64(2)) == -0.5
	assert contextual_infix_multiply(1, f64(2)) == 3.0
	assert contextual_infix_divide(1, f64(2)) == 0.75
	assert contextual_infix_rhs(1, f64(2)) == 0.5
	assert contextual_infix_modulo(1, u64(0x2_0000_0000)) == 0x1_0000_0007
	assert contextual_infix_and(1, u64(0x1_0000_0000)) == 0x1_0000_0000
	assert contextual_infix_or(1, u64(8)) == 0x1_0000_000f
	assert contextual_infix_xor(1, u64(1)) == 0x1_0000_0006
	assert contextual_infix_left_shift(1, u64(1)) == 0x2_0000_000e
	assert contextual_infix_right_shift(1, u64(1)) == 0x8000_0003
}

fn contextual_infix_shift_count[A, B](first A, second B) B {
	_ = first
	return second << contextual_infix_value()
}

fn contextual_infix_comparison[A, B](first A, second B) bool {
	_ = second
	return contextual_infix_value() < first
}

fn contextual_infix_pointer[A, B](first A, second B) B {
	_ = first
	// Keep the pointer arithmetic used to exercise an independent offset type.
	unsafe {
		return second + contextual_zero()
	}
}

fn test_generic_infix_return_context_keeps_independent_operand_types() {
	assert contextual_infix_shift_count(1, u64(1)) == 128
	assert contextual_infix_comparison(8, '')
	values := [11, 12]!
	unsafe {
		assert contextual_infix_pointer(1, &values[0]) == &values[0]
	}
}

@[flag]
enum ContextualFlags {
	first
	second
	third
}

fn contextual_infix_power[A, B](first A, second B) B {
	_ = first
	return contextual_infix_value() ** second
}

fn contextual_infix_unsigned_shift[A, B](first A, second B) B {
	_ = first
	return contextual_infix_value() >>> second
}

fn contextual_unsigned_shift_count[A, B](first A, second B) B {
	_ = first
	return second >>> contextual_infix_value()
}

fn contextual_boolean_value[T]() T {
	$if T is bool {
		return T(false)
	} $else {
		return T(7)
	}
}

fn contextual_not[A, B](first A, second B) bool {
	_ = first
	_ = second
	return !contextual_boolean_value()
}

fn contextual_logical_and[A, B](first A, second B) bool {
	_ = first
	return contextual_boolean_value() && second
}

fn contextual_logical_or[A, B](first A, second B) bool {
	_ = first
	return second || contextual_boolean_value()
}

fn test_generic_return_context_covers_boolean_power_unsigned_shift_and_flags() {
	assert contextual_not(1, '')
	assert !contextual_logical_and(1, true)
	assert !contextual_logical_or(1, false)
	assert contextual_infix_power(1, f64(2)) == 2.25
	assert contextual_infix_unsigned_shift(1, u64(1)) == 0x8000_0003
	assert contextual_unsigned_shift_count(1, u64(256)) == 2
	assert contextual_infix_or(1, ContextualFlags.second) == ContextualFlags.first | .second
}

fn contextual_nested_zero[T](flag bool) T {
	_ = flag
	return T{}
}

fn contextual_identity[T](value T) T { return value }

fn contextual_list[T](value T) []T { return [value] }

fn contextual_second[T](first T, second T) T {
	_ = first
	return second
}

fn contextual_from_int[T](value int) T {
	assert value == 0
	return T{}
}

fn contextual_nested[A, B](first A, second B) B {
	_ = first
	_ = second
	return contextual_identity(contextual_identity(contextual_nested_zero(false)))
}

fn contextual_nested_list[A, B](first A, second B) []B {
	_ = first
	_ = second
	return contextual_list(contextual_nested_zero(false))
}

fn contextual_nested_explicit[A, B](first A, second B) B {
	_ = first
	_ = second
	return contextual_identity[B](contextual_zero())
}

fn contextual_nested_argument[A, B](first A, second B) B {
	_ = second
	return contextual_identity(contextual_identity(first))
}

fn contextual_nested_concrete_parameter[A, B](first A, second B) B {
	_ = first
	_ = second
	return contextual_from_int(contextual_nested_zero(false))
}

fn contextual_nested_result[A, B](first A, second B) !B {
	_ = first
	_ = second
	return contextual_identity(contextual_result_zero(false)!)
}

fn contextual_nested_option_zero[T](fail bool) ?T {
	if fail { return none }
	return T{}
}

fn contextual_nested_option[A, B](first A, second B) ?B {
	_ = first
	_ = second
	return contextual_identity(contextual_nested_option_zero(false)?)
}

fn contextual_nested_fallback[A, B](first A, second B) B {
	_ = first
	_ = second
	return contextual_identity(contextual_result_zero(false) or { B{} })
}

fn contextual_nested_sibling[T, U](first T, second U) U {
	_ = first
	return contextual_second(contextual_identity(second), contextual_nested_zero(false))
}

fn contextual_nested_leading[T, U](first T, second U) U {
	_ = first
	return contextual_second(contextual_nested_zero(false), second)
}

fn (factory TupleFactory[T]) identity[U](value U) U { return value }

fn contextual_nested_method[A, B](first A, second B) B {
	_ = first
	_ = second
	factory := TupleFactory[A]{}
	return factory.identity(contextual_nested_zero(false))
}

fn contextual_nested_method_explicit[A, B](first A, second B) B {
	_ = first
	_ = second
	factory := TupleFactory[A]{}
	return factory.identity[B](contextual_nested_zero(false))
}

fn contextual_nested_variant[A, B](value ContextualVariantSum, first A, second B) B {
	_ = first
	$for variant in value.variants {
		if value is variant { return contextual_identity(variant_contextual_zero()) }
	}
	return second
}

fn test_nested_generic_call_arguments_use_the_callee_parameter_context() {
	assert contextual_nested(1, 'second') == ''
	assert contextual_nested_list(1, 'second') == ['']
	assert contextual_nested_explicit(1, 'second') == ''
	named := contextual_nested_argument(NamedItem{ label: 'kept' }, Named(NamedItem{}))
	assert named.name() == 'kept'
	assert contextual_nested_concrete_parameter('first', 'second') == ''
	assert contextual_nested_result(1, 'second') or { panic(err) } == ''
	assert contextual_nested_option(1, 'second') or { panic('missing value') } == ''
	assert contextual_nested_fallback(1, 'second') == ''
	assert contextual_nested_sibling(1, 'second') == ''
	assert contextual_nested_leading(1, 'second') == 'second'
	assert contextual_nested_method(1, 'second') == ''
	assert contextual_nested_method_explicit(1, 'second') == ''
	for value in [ContextualVariantSum(7), ContextualVariantSum('sum')] {
		assert contextual_nested_variant(value, 1, 'second') == ''
	}
}
