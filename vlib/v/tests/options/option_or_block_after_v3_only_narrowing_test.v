// V 0.5.2 did not unwrap an option variable in a `for x != none` condition, in the
// right operand of `||`/`&&`, or after a compound `if a == none || b == none { return }`
// guard, so code written for it uses `x or { ... }` there. V3 narrows in those places;
// the unwrap must still compile, use the value and never run the fallback.
@[heap]
struct ChainNode {
	value  int
	parent ?&ChainNode
}

fn chain() &ChainNode {
	root := &ChainNode{
		value: 1
	}
	middle := &ChainNode{
		value:  2
		parent: root
	}
	return &ChainNode{
		value:  4
		parent: middle
	}
}

fn sum_with_break(leaf &ChainNode) int {
	mut current := ?&ChainNode(leaf)
	mut total := 0
	for current != none {
		node := current or { break }
		total += node.value
		current = node.parent
	}
	return total
}

fn sum_with_unsafe_cast(leaf &ChainNode) int {
	mut ancestor := ?&ChainNode(leaf)
	mut total := 0
	for ancestor != none {
		mut node := unsafe { &ChainNode(ancestor or { break }) }
		total += node.value
		ancestor = node.parent
	}
	return total
}

fn sum_with_return(leaf &ChainNode) int {
	mut current := ?&ChainNode(leaf)
	mut total := 0
	for current != none {
		node := current or { return -1 }
		total += node.value
		current = node.parent
	}
	return total
}

fn sum_with_propagation(leaf &ChainNode) ?int {
	mut current := ?&ChainNode(leaf)
	mut total := 0
	for current != none {
		node := current?
		total += node.value
		current = node.parent
	}
	return total
}

fn sum_in_generic[T](leaf &ChainNode, start T) T {
	mut current := ?&ChainNode(leaf)
	mut total := start
	for current != none {
		node := current or { break }
		total += T(node.value)
		current = node.parent
	}
	return total
}

fn forced_or(force ?bool) bool {
	return force == none || force or { false }
}

fn forced_not(force ?bool) bool {
	if force == none || !force or { false } {
		return true
	}
	return false
}

fn below_limit(limit ?int, value int) bool {
	return limit != none && value < limit or { 0 }
}

fn differs_from_one(flag bool, z ?string) bool {
	return flag || (z != none && z or { '' } != '1')
}

fn add_both(a ?int, b ?int) int {
	if a == none || b == none {
		return -1
	}
	x := a or { return -2 }
	y := b or { return -3 }
	return x + y
}

fn mix(p1 ?int, p2 ?int) int {
	if p1 == none && p2 == none {
		return 0
	} else if p2 == none {
		return p1 or { 7 }
	} else if p1 == none {
		return p2 or { 7 }
	} else {
		first := p1 or { 7 }
		second := p2 or { 7 }
		return first * 100 + second
	}
}

fn test_for_none_condition_or_block() {
	leaf := chain()
	assert sum_with_break(leaf) == 7
	assert sum_with_unsafe_cast(leaf) == 7
	assert sum_with_return(leaf) == 7
	assert sum_with_propagation(leaf) or { -1 } == 7
	assert sum_in_generic(leaf, 10) == 17
	assert sum_in_generic(leaf, f64(0.5)) == 7.5
}

fn test_short_circuit_operand_or_block() {
	assert forced_or(none)
	assert forced_or(true)
	assert !forced_or(false)
	assert forced_not(none)
	assert !forced_not(true)
	assert forced_not(false)
	assert below_limit(5, 3)
	assert !below_limit(none, 3)
	assert !below_limit(1, 3)
	assert differs_from_one(true, none)
	assert !differs_from_one(false, '1')
	assert differs_from_one(false, '2')
	assert !differs_from_one(false, none)
}

fn test_compound_guard_or_block() {
	assert add_both(1, 2) == 3
	assert add_both(none, 2) == -1
	assert add_both(1, none) == -1
	assert mix(none, none) == 0
	assert mix(3, none) == 3
	assert mix(none, 4) == 4
	assert mix(3, 4) == 304
}
