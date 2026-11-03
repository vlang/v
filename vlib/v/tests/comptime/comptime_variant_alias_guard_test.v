type GuardedVariant = int | string

type GuardedVariantAlias = GuardedVariant

fn mutate_guarded_variant[T](mut value T) string {
	$if T is int {
		value += 1
	} $else $if T is string {
		value += '!'
	}
	return T.name
}

fn visit_guarded_variant[T](mut value T, reversed bool) string {
	type_idx := value.type_idx()
	$for variant in T.variants {
		if reversed {
			if type_idx == variant.typ && (mut value is variant) {
				return mutate_guarded_variant(mut value)
			}
		} else {
			if (mut value is variant) && (type_idx == variant.typ && true) {
				return mutate_guarded_variant(mut value)
			}
		}
	}
	return 'unmatched'
}

fn test_reflected_sum_alias_guard_infers_the_selected_variant() {
	for reversed in [false, true] {
		mut number := GuardedVariantAlias(41)
		assert visit_guarded_variant(mut number, reversed) == 'int'
		assert number == GuardedVariantAlias(42)
		mut text := GuardedVariantAlias('hello')
		assert visit_guarded_variant(mut text, reversed) == 'string'
		assert text == GuardedVariantAlias('hello!')
	}
}

fn guarded_is_sumtype[T](_ T) bool {
	$if T is $sumtype {
		return true
	} $else {
		return false
	}
}

fn visit_disjunctive_variant[T](mut value T) bool {
	$for variant in T.variants {
		if (mut value is variant) || true {
			return guarded_is_sumtype(value)
		}
	}
	return false
}

fn test_disjunction_does_not_guarantee_the_selected_variant() {
	mut number := GuardedVariantAlias(41)
	assert visit_disjunctive_variant(mut number)
	mut text := GuardedVariantAlias('hello')
	assert visit_disjunctive_variant(mut text)
}
