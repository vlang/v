type ReflectedValue = int | map[string]int

type ReflectedValueAlias = ReflectedValue

struct VariantInspector {}

fn (inspector VariantInspector) type_name[T](value T) string {
	return typeof(value).name
}

fn (inspector VariantInspector) variant_type_name[T](value T) string {
	$for variant in T.variants {
		if value is variant {
			return inspector.type_name(value)
		}
	}
	return ''
}

fn (inspector VariantInspector) explicit_variant_type_name[T](value T) string {
	$for variant in T.variants {
		if value is variant {
			return inspector.type_name[T](value)
		}
	}
	return ''
}

fn test_alias_variant_direct_generic_method_argument() {
	inspector := VariantInspector{}
	assert inspector.variant_type_name(ReflectedValueAlias(42)) == 'int'
	assert inspector.variant_type_name(ReflectedValueAlias(map[string]int{
		'a': 1
	})) == 'map[string]int'
	assert inspector.variant_type_name(ReflectedValue(42)) == 'int'
	assert inspector.variant_type_name(ReflectedValue(map[string]int{
		'a': 1
	})) == 'map[string]int'
}

fn test_alias_variant_explicit_generic_method_argument() {
	inspector := VariantInspector{}
	assert inspector.explicit_variant_type_name(ReflectedValueAlias(42)) == 'ReflectedValueAlias'
	assert inspector.explicit_variant_type_name(ReflectedValueAlias(map[string]int{
		'a': 1
	})) == 'ReflectedValueAlias'
}
