// After `if x == none { return }`, a new declaration `y := x` gets the
// option's payload type, like reads of `x` itself. Other fallthrough
// narrowings (`||` conditions) keep the declared option type for `y`.
@[heap]
struct NoneGuardDeclNode {
	count  int
	parent ?&NoneGuardDeclNode
}

fn (node &NoneGuardDeclNode) bumped() int {
	return node.count + 100
}

fn none_guard_decl_ref(parent ?&NoneGuardDeclNode) int {
	if parent == none {
		return -1
	}
	node := parent
	if node.count > 3 {
		return node.bumped()
	}
	grandparent := node.parent
	extra := if gp := grandparent { gp.count } else { 0 }
	return node.count + extra
}

struct NoneGuardDeclDesc {
	value ?int
	flag  ?bool
}

fn (desc NoneGuardDeclDesc) has_value() bool {
	return desc.value != none
}

fn none_guard_decl_struct(extensible bool, current ?NoneGuardDeclDesc) int {
	if current == none {
		if !extensible {
			return -2
		}
		return -1
	}
	previous := current
	if !(previous.flag or { false }) {
		return if previous.has_value() { 1 } else { 0 }
	}
	return previous.value or { 7 }
}

fn none_guard_decl_mut(o ?int) int {
	if o == none {
		return -1
	}
	mut y := o
	y += 10
	return y
}

fn none_guard_decl_nested_block(o ?string, deeper bool) int {
	if o == none {
		return -1
	}
	if deeper {
		text := o
		return text.len * 10
	}
	text := o
	return text.len
}

fn none_guard_decl_panic(o ?int) int {
	if o == none {
		panic('none_guard_decl_panic: missing value')
	}
	y := o
	return y + 1
}

fn none_guard_or_condition_keeps_option(o ?int, skip bool) string {
	if o == none || skip {
		return 'skipped'
	}
	y := o
	return typeof(y).name
}

fn test_none_guard_decl_unwraps_pointer_option() {
	assert none_guard_decl_ref(none) == -1
	assert none_guard_decl_ref(&NoneGuardDeclNode{
		count:  2
		parent: &NoneGuardDeclNode{
			count: 9
		}
	}) == 11
	assert none_guard_decl_ref(&NoneGuardDeclNode{
		count: 5
	}) == 105
}

fn test_none_guard_decl_unwraps_struct_option() {
	assert none_guard_decl_struct(false, none) == -2
	assert none_guard_decl_struct(true, none) == -1
	assert none_guard_decl_struct(true, NoneGuardDeclDesc{
		value: 3
	}) == 1
	assert none_guard_decl_struct(true, NoneGuardDeclDesc{}) == 0
	assert none_guard_decl_struct(true, NoneGuardDeclDesc{
		value: 4
		flag:  true
	}) == 4
	assert none_guard_decl_struct(true, NoneGuardDeclDesc{
		flag: true
	}) == 7
}

fn test_none_guard_decl_unwraps_scalars() {
	assert none_guard_decl_mut(none) == -1
	assert none_guard_decl_mut(5) == 15
	assert none_guard_decl_nested_block(none, true) == -1
	assert none_guard_decl_nested_block('abc', true) == 30
	assert none_guard_decl_nested_block('abcd', false) == 4
	assert none_guard_decl_panic(41) == 42
}

fn test_none_guard_or_condition_decl_keeps_option() {
	assert none_guard_or_condition_keeps_option(3, false) == '?int'
	assert none_guard_or_condition_keeps_option(3, true) == 'skipped'
}
