pub struct Set[T] {
mut:
	elements map[T]u8
}

pub fn (l Set[T]) == (r Set[T]) bool {
	if l.elements.len != r.elements.len {
		return false
	}
	for key, l_val in l.elements {
		r_val := r.elements[key] or { return false }
		if l_val != r_val {
			return false
		}
	}
	return true
}

fn test_main() {
	_ := Set[int]{}
}
