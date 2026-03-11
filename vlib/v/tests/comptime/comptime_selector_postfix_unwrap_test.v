fn generic_unwrap[T](value T) []int {
	mut out := []int{}
	$if T is $struct {
		$for field in T.fields {
			$if field.typ is ?int {
				out << value.$(field.name) ?
			}
		}
	}
	return out
}

struct PostfixOptionField {
	a ?int
}

fn test_generic_comptime_selector_postfix_unwrap() {
	assert generic_unwrap(PostfixOptionField{
		a: 7
	}) == [7]
}
