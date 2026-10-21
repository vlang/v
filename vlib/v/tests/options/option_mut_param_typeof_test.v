// `typeof(a).name` of a `mut a ?T` parameter names `?&T`, as in V 0.5.2: the option holds
// a reference to the caller's payload. Smartcast to its payload, it names `&T`, like a
// `mut a T` parameter.
struct Point {
mut:
	x int
	y int
}

type PointAlias = Point

fn struct_name(mut a ?Point) string {
	return typeof(a).name
}

fn array_name(mut a ?[]int) string {
	return typeof(a).name
}

fn map_name(mut a ?map[string]int) string {
	return typeof(a).name
}

fn alias_name(mut a ?PointAlias) string {
	return typeof(a).name
}

fn pointer_name(mut a ?&Point) string {
	return typeof(a).name
}

fn plain_struct_name(mut a Point) string {
	return typeof(a).name
}

fn plain_array_name(mut a []int) string {
	return typeof(a).name
}

fn value_name(a ?Point) string {
	return typeof(a).name
}

fn struct_smartcast_name(mut a ?Point) string {
	if a != none {
		return typeof(a).name
	}
	return 'none'
}

fn array_smartcast_name(mut a ?[]int) string {
	if a != none {
		return typeof(a).name
	}
	return 'none'
}

fn else_smartcast_name(mut a ?Point) string {
	if a == none {
		return 'none'
	} else {
		return typeof(a).name
	}
}

fn value_smartcast_name(a ?Point) string {
	if a != none {
		return typeof(a).name
	}
	return 'none'
}

fn generic_option_name[T](mut a ?T) string {
	return typeof(a).name
}

fn generic_name[T](mut a T) string {
	return typeof(a).name
}

fn test_mut_option_param_names_a_reference_payload() {
	mut p := ?Point(Point{1, 2})
	assert struct_name(mut p) == '?&Point'
	mut a := ?[]int([1, 2])
	assert array_name(mut a) == '?&[]int'
	mut m := ?map[string]int({
		'a': 1
	})
	assert map_name(mut m) == '?&map[string]int'
	mut pa := ?PointAlias(PointAlias(Point{}))
	assert alias_name(mut pa) == '?&PointAlias'
	mut r := ?&Point(&Point{3, 4})
	assert pointer_name(mut r) == '?&&Point'
	mut none_p := ?Point(none)
	assert struct_name(mut none_p) == '?&Point'
}

fn test_other_params_keep_their_names() {
	mut p := Point{1, 2}
	assert plain_struct_name(mut p) == '&Point'
	mut a := [1, 2]
	assert plain_array_name(mut a) == '&[]int'
	assert value_name(?Point(Point{})) == '?Point'
	assert value_smartcast_name(?Point(Point{})) == 'Point'
}

fn test_mut_option_param_smartcast_names_a_reference() {
	mut p := ?Point(Point{1, 2})
	assert struct_smartcast_name(mut p) == '&Point'
	assert else_smartcast_name(mut p) == '&Point'
	mut a := ?[]int([1, 2])
	assert array_smartcast_name(mut a) == '&[]int'
}

fn test_fn_literal_mut_option_param() {
	name := fn (mut a ?Point) string {
		return typeof(a).name
	}
	mut p := ?Point(Point{1, 2})
	assert name(mut p) == '?&Point'
}

fn test_generic_mut_option_param() {
	mut p := ?Point(Point{1, 2})
	assert generic_option_name(mut p) == '?&Point'
	mut a := ?[]int([1, 2])
	assert generic_option_name(mut a) == '?&[]int'
	mut i := ?int(3)
	assert generic_option_name(mut i) == '?&int'
	assert generic_name(mut p) == '?&Point'
	mut plain := Point{}
	assert generic_name(mut plain) == '&Point'
	mut plain_array := [1]
	assert generic_name(mut plain_array) == '&[]int'
}
