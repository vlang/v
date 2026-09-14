fn test_map_pointer_sized_scalar_values_compare_without_map_dispatch() {
	i64_left := {
		'value': i64(17)
	}
	i64_right := {
		'value': i64(17)
	}
	i64_different := {
		'value': i64(18)
	}
	assert i64_left == i64_right
	assert i64_left != i64_different

	u64_left := {
		'value': u64(17)
	}
	u64_right := {
		'value': u64(17)
	}
	assert u64_left == u64_right

	f64_left := {
		'value': f64(17.5)
	}
	f64_right := {
		'value': f64(17.5)
	}
	assert f64_left == f64_right
	positive_zero := {
		'value': f64(0.0)
	}
	negative_zero := {
		'value': -f64(0.0)
	}
	assert positive_zero == negative_zero

	value := 17
	pointer_left := {
		'value': voidptr(&value)
	}
	pointer_right := {
		'value': voidptr(&value)
	}
	assert pointer_left == pointer_right
}

fn test_nested_map_values_use_semantic_equality() {
	left := {
		'outer': {
			'inner': 17
		}
	}
	right := {
		'outer': {
			'inner': 17
		}
	}
	different := {
		'outer': {
			'inner': 18
		}
	}
	assert left == right
	assert left != different
}

fn test_map_value_lookup_clone_materializes_the_read_value() {
	values := {
		'present': {
			'value': 1
		}
	}
	cloned := values['missing'].clone()
	assert cloned.len == 0
}
