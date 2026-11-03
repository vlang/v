type GuardedChoice = int | string
type GuardedAlias = GuardedChoice

fn update_guarded_payload[T](mut value T) string {
	$if T is int {
		value += 1
	} $else $if T is string {
		value += '!'
	}
	return typeof(T).name
}

fn update_guarded_variant[T](mut value T, enabled bool) string {
	type_idx := value.type_idx()
	$for variant in T.variants {
		if mut value is variant && enabled && type_idx == variant.typ {
			return update_guarded_payload(mut value)
		}
	}
	return 'disabled'
}

fn guarded_payload_name[T](_ T) string {
	return typeof(T).name
}

fn nested_guarded_variant[T](value T, enabled bool) string {
	type_idx := value.type_idx()
	$for variant in T.variants {
		if enabled && (value is variant && type_idx == variant.typ) {
			return guarded_payload_name(value)
		}
	}
	return 'disabled'
}

fn disjunctive_variant[T](value T) string {
	$for variant in T.variants {
		if value is variant || true {
			return guarded_payload_name(value)
		}
	}
	return ''
}

fn test_compound_variant_guard_infers_and_mutates_the_alias_payload() {
	mut number := GuardedAlias(41)
	assert update_guarded_variant(mut number, true) == 'int'
	assert number == GuardedAlias(42)
	mut text := GuardedAlias('hello')
	assert update_guarded_variant(mut text, true) == 'string'
	assert text == GuardedAlias('hello!')
	assert update_guarded_variant(mut text, false) == 'disabled'
	assert text == GuardedAlias('hello!')

	mut direct := GuardedChoice(1)
	assert update_guarded_variant(mut direct, true) == 'int'
	assert direct == GuardedChoice(2)
}

fn test_nested_compound_variant_guard_infers_the_alias_payload() {
	assert nested_guarded_variant(GuardedAlias(42), true) == 'int'
	assert nested_guarded_variant(GuardedAlias('hello'), true) == 'string'
	assert nested_guarded_variant(GuardedAlias('hello'), false) == 'disabled'
}

fn test_disjunctive_variant_guard_keeps_the_sum_alias() {
	assert disjunctive_variant(GuardedAlias('hello')) == 'GuardedAlias'
}
