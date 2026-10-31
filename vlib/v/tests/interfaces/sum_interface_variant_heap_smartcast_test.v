// A sum type variable moved to the heap (a parameter, or a local copy of one)
// and smartcast to its interface variant already holds a complete interface
// value. Calling a method on it must dispatch through that value, not box it
// again as if it were a concrete implementer.
interface SumVariantNamed {
	get(name string) ?string
}

struct SumVariantRecord {
	x int
}

fn (r SumVariantRecord) get(name string) ?string {
	return '${name}${r.x}'
}

type SumVariantValue = string | SumVariantNamed

enum SumVariantKind {
	text
	named
}

fn sum_variant_param_get(value SumVariantValue) string {
	if value is SumVariantNamed {
		return value.get('a') or { 'none' }
	}
	return 'text'
}

fn sum_variant_local_get(value SumVariantValue) string {
	copy := value
	if copy is SumVariantNamed {
		got := copy.get('b') or { return 'none' }
		return got
	}
	return 'text'
}

fn sum_variant_match_get(kind SumVariantKind, value SumVariantValue) bool {
	return match kind {
		.text {
			value is string && value == 'a'
		}
		.named {
			if value is SumVariantNamed {
				got := value.get('c') or { return false }
				got == 'c7'
			} else {
				false
			}
		}
	}
}

fn test_heap_smartcast_interface_variant_dispatches_its_method() {
	value := SumVariantValue(SumVariantNamed(SumVariantRecord{
		x: 7
	}))
	assert sum_variant_param_get(value) == 'a7'
	assert sum_variant_local_get(value) == 'b7'
	assert sum_variant_match_get(.named, value)
	assert sum_variant_param_get(SumVariantValue('x')) == 'text'
}
