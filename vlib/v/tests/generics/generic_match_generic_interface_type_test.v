interface Named {
	get_name() string
}

struct GenericValue[T] implements Named {}

fn (g GenericValue[T]) get_name() string {
	return T.name
}

fn is_match[T](base Named) bool {
	match base {
		GenericValue[T] { return true }
		else { return false }
	}
}

fn test_match_generic_type_in_generic_fn_without_preinstantiation() {
	base := Named(GenericValue[int]{})
	assert !is_match[string](base)
}
