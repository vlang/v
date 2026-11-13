import os

type GuardedChoice = int | string
type GuardedAlias = GuardedChoice
type GuardedNumeric = i32 | string

fn (value GuardedNumeric) i32() i32 {
	_ = value
	return 0
}

fn guarded_numeric_local[T](value T, enabled bool) i32 {
	$for variant in T.variants {
		if enabled && (value is variant) {
			$if variant.typ is i32 {
				local := value
				alias := local
				return alias
			}
		}
	}
	return -1
}

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

fn test_variant_guard_checks_derived_locals_with_the_concrete_payload_type() {
	assert guarded_numeric_local(GuardedNumeric(i32(42)), true) == 42
	assert guarded_numeric_local(GuardedNumeric(i32(42)), false) == -1
	assert guarded_numeric_local(GuardedNumeric('hello'), true) == -1
}

fn test_variant_guard_still_rejects_invalid_concrete_returns() {
	root := os.join_path(os.vtmp_dir(), 'variant_guard_invalid_return_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	source := 'type Choice = i32 | string
fn extract[T](value T) i32 {
	\$for variant in T.variants {
		if value is variant {
			\$if variant.typ is VARIANT_TYPE {
				RETURN_BODY
			}
		}
	}
	return -1
}
fn main() { _ = extract(Choice(i32(42))) }
'
	for variant_type, body in {
		'i32':    'return "bad"'
		'string': 'local := value\n\t\t\t\talias := local\n\t\t\t\treturn alias'
	} {
		os.write_file(path, source.replace('VARIANT_TYPE', variant_type).replace('RETURN_BODY', body))!
		result := os.exec([@VEXE, '-o', os.join_path(root, 'main.c'), path])
		assert result.exit_code != 0, result.output
		assert result.output.contains('cannot use `string` as type `i32` in return argument'), result.output
	}
}

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
