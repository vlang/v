// A `mut` optional parameter is passed as `&?&T`, whatever its payload. Its body reads a
// struct payload, or an alias of one, as `?&T`, and arrays, maps, fixed arrays, sum types
// and enums as `?T`; `a = none`, assignments and `or` work for all of them.
struct Named {
	name string
}

type Names = []Named

type NamedAlias = Named

type Shape = Named | int

enum Color {
	red
	green
}

fn clear_names(mut a ?[]Named) {
	a = none
}

fn set_names(mut a ?[]Named) {
	a = [Named{'set'}]
}

fn clear_ints(mut a ?[]int) {
	a = none
}

fn count_ints(mut a ?[]int) int {
	if v := a {
		return v.len
	}
	return -1
}

fn first_int(mut a ?[]int) int {
	v := a or { return -1 }
	return v[0]
}

fn clear_alias(mut a ?Names) {
	a = none
}

fn clear_map(mut a ?map[string]int) {
	a = none
}

fn set_map(mut a ?map[string]int) {
	a = {
		'k': 3
	}
}

fn clear_fixed(mut a ?[2]int) {
	a = none
}

fn set_fixed(mut a ?[2]int) {
	a = [7, 8]!
}

fn clear_sum(mut a ?Shape) {
	a = none
}

fn clear_enum(mut a ?Color) {
	a = none
}

fn set_enum(mut a ?Color) {
	a = .green
}

fn clear_struct(mut a ?Named) {
	a = none
}

fn set_struct(mut a ?Named) {
	a = Named{'struct'}
}

fn clear_struct_alias(mut a ?NamedAlias) {
	a = none
}

fn set_struct_alias(mut a ?NamedAlias) {
	a = NamedAlias(Named{'alias'})
}

fn struct_alias_name(mut a ?NamedAlias) string {
	v := a or { return 'none' }
	return v.name
}

fn clear_generic[T](mut a ?T) {
	a = none
}

fn test_mut_option_array_param() {
	mut a := ?[]Named([Named{'a'}])
	clear_names(mut a)
	assert a == none
	set_names(mut a)
	names := a or { []Named{} }
	assert names.len == 1
	assert names[0].name == 'set'
	mut b := ?[]int([1, 2])
	assert count_ints(mut b) == 2
	assert first_int(mut b) == 1
	clear_ints(mut b)
	assert b == none
	assert count_ints(mut b) == -1
	mut c := ?Names(Names([Named{'c'}]))
	clear_alias(mut c)
	assert c == none
}

fn test_mut_option_map_and_fixed_array_param() {
	mut m := ?map[string]int({
		'a': 1
	})
	clear_map(mut m)
	assert m == none
	set_map(mut m)
	assert m or { map[string]int{} } == {
		'k': 3
	}
	mut f := ?[2]int([1, 2]!)
	clear_fixed(mut f)
	assert f == none
	set_fixed(mut f)
	assert f or { [0, 0]! } == [7, 8]!
}

fn test_mut_option_other_payloads() {
	mut sh := ?Shape(Shape(3))
	clear_sum(mut sh)
	assert sh == none
	mut col := ?Color(.red)
	clear_enum(mut col)
	assert col == none
	set_enum(mut col)
	assert col or { Color.red } == .green
	mut s := ?Named(Named{'s'})
	clear_struct(mut s)
	assert s == none
	set_struct(mut s)
	assert (s or { Named{} }).name == 'struct'
	mut g := ?[]int([4])
	clear_generic(mut g)
	assert g == none
	mut gs := ?Named(Named{'g'})
	clear_generic(mut gs)
	assert gs == none
}

fn test_mut_option_struct_alias_param() {
	mut a := ?NamedAlias(NamedAlias(Named{'a'}))
	assert struct_alias_name(mut a) == 'a'
	clear_struct_alias(mut a)
	assert a == none
	assert struct_alias_name(mut a) == 'none'
	set_struct_alias(mut a)
	assert struct_alias_name(mut a) == 'alias'
}
