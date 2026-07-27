type EmptyBoxArr = [4]EmptyBox
type NestedEmptyBoxArr = [4][2]EmptyBox

struct EmptyBox {}

fn test_alias_fixed_array_of_empty_structs() {
	_ := EmptyBoxArr{}
	a := EmptyBoxArr{}
	assert a.len == 4

	b := &EmptyBoxArr{}
	assert b.len == 4
}

fn test_alias_nested_fixed_array_of_empty_structs() {
	_ := NestedEmptyBoxArr{}
	a := NestedEmptyBoxArr{}
	assert a.len == 4
	assert a[0].len == 2

	b := &NestedEmptyBoxArr{}
	assert b.len == 4
}

fn test_blank_plain_assign_nested_fixed_array_of_empty_structs() {
	// Regression for `_ = Arr{}` (blank plain-assign, not `_ := Arr{}`), which
	// exercises a different codegen path (expr_with_var) than blank decl-assign.
	_ = NestedEmptyBoxArr{}
}

type NonEmptyBoxArr = [4]NonEmptyBox

struct NonEmptyBox {
	n int
}

fn test_reassign_alias_fixed_array() {
	// Reassigning (not declaring) a fixed-array type alias via its struct-init
	// literal syntax used to generate invalid C, regardless of whether the
	// element type was an empty struct.
	mut c := EmptyBoxArr{}
	c = EmptyBoxArr{}
	assert c.len == 4

	mut d := NonEmptyBoxArr{}
	d = NonEmptyBoxArr{}
	assert d.len == 4
}
