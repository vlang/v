// `T(v)` and `$zero(v.typ)` in `$for v in T.variants` must keep the variant's
// type: a literal zero alone is an `int`, `f64` or `string`, and was wrapped as
// another variant, or as no variant at all.

enum Color {
	red
	green
}

type MyStr = string

type Num = i32 | f32 | u8 | Color | string

type Named = MyStr | string

fn variant_cast_names[T]() []string {
	mut names := []string{}
	$for v in T.variants {
		value := T(v)
		names << value.type_name()
	}
	return names
}

fn variant_zero_names[T]() []string {
	mut names := []string{}
	$for v in T.variants {
		zero := $zero(v.typ)
		value := T(zero)
		names << value.type_name()
	}
	return names
}

fn test_variant_cast_keeps_the_variant_type() {
	assert variant_cast_names[Num]() == ['i32', 'f32', 'u8', 'Color', 'string']
	assert variant_cast_names[Named]() == ['MyStr', 'string']
}

fn test_variant_zero_keeps_the_variant_type() {
	assert variant_zero_names[Num]() == ['i32', 'f32', 'u8', 'Color', 'string']
	assert variant_zero_names[Named]() == ['MyStr', 'string']
}
