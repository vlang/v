// A local whose address is stored in a variable of an outer block outlives its own
// block, and a loop body reuses its storage in the next iteration: it is moved to the
// heap, as in V 0.5.2. vsql's parser builds its left-recursive expressions this way.
struct Node {
	next   &Node = unsafe { nil }
	factor int
}

struct Term {
	term   ?&Term
	op     string
	factor int
}

type Sym = Term | int

struct SymType {
mut:
	v Sym
}

fn node_depth(n Node) int {
	if n.next == unsafe { nil } {
		return 0
	}
	return 1 + node_depth(*n.next)
}

fn (t Term) eval() int {
	if inner := t.term {
		return inner.eval() * t.factor
	}
	return t.factor
}

fn test_loop_body_local_stored_in_an_outer_variable() {
	mut cur := Node{
		factor: 1
	}
	for f in [2, 3, 5] {
		prev := cur
		cur = Node{&prev, f}
	}
	assert node_depth(cur) == 3
	assert cur.next.next.factor == 2
}

fn test_option_reference_field_of_a_parser_rule() {
	mut stack := []SymType{len: 1}
	stack[0].v = Term{
		factor: 2
	}
	mut val := SymType{}
	mut dollar := []SymType{}
	for f in [3, 5, 7] {
		dollar = stack[0..1].clone()
		match f {
			3, 5, 7 {
				{
					t := dollar[0].v as Term
					val.v = Term{&t, '*', f}
				}
			}
			else {}
		}
		stack[0] = val
	}
	result := stack[0].v as Term
	assert result.eval() == 210
}

struct Holder {
	next &Node = unsafe { nil }
}

fn maybe_node(n Node) ?Node {
	return n
}

fn test_c_style_and_infinite_for_loop_bodies() {
	mut cur := Node{
		factor: 1
	}
	for i := 0; i < 3; i++ {
		prev := cur
		cur = Node{&prev, i}
	}
	assert node_depth(cur) == 3
	mut n := 0
	for {
		if n == 2 {
			break
		}
		prev := cur
		cur = Node{&prev, n}
		n++
	}
	assert node_depth(cur) == 5
}

fn test_multi_assignment_pairs() {
	mut cur := Node{
		factor: 1
	}
	mut count := 0
	for f in [2, 3, 5] {
		prev := cur
		count, cur = count + 1, Node{&prev, f}
	}
	assert count == 3
	assert node_depth(cur) == 3
	mut saved := Holder{}
	mut steps := 0
	for f in [2, 3] {
		x := Node{
			next:   saved.next
			factor: f
		}
		steps, saved = steps + 1, Holder{&x}
	}
	assert steps == 2
	assert saved.next.factor == 3
	assert saved.next.next.factor == 2
}

fn holder_depth(h Holder) int {
	if h.next == unsafe { nil } {
		return 0
	}
	return 1 + node_depth(*h.next)
}

// link_through_guard keeps `&x` past its `if`, in a `mut` parameter that is declared
// in the same block as that `if`. Each call reuses the frame of the previous one.
fn link_through_guard(mut saved Holder, f int) {
	if x := maybe_node(Node{
		next:   saved.next
		factor: f
	})
	{
		saved = Holder{&x}
	}
}

fn test_if_guard_binding() {
	mut saved := Holder{}
	for f in [2, 3, 5] {
		link_through_guard(mut saved, f)
	}
	assert holder_depth(saved) == 3
	assert saved.next.factor == 5
	mut cur := Node{
		factor: 1
	}
	for f in [2, 3, 5] {
		if prev := maybe_node(cur) {
			cur = Node{&prev, f}
		}
	}
	assert node_depth(cur) == 3
}

fn test_select_receive_binding() {
	ch := chan Node{cap: 1}
	mut cur := Node{
		factor: 1
	}
	for f in [2, 3, 5] {
		ch <- cur
		select {
			prev := <-ch {
				cur = Node{&prev, f}
			}
		}
	}
	assert node_depth(cur) == 3
}

struct Chain {
mut:
	next &Node = unsafe { nil }
}

fn test_store_through_a_pointer_alias_of_outer_storage() {
	mut saved := Chain{}
	for f in [2, 3, 5] {
		mut p := &saved
		mut x := Node{
			next:   saved.next
			factor: f
		}
		p.next = &x
	}
	assert saved.next.factor == 5
	assert node_depth(*saved.next) == 2
}

type Shape = Box(Node) | Nothing

fn test_named_variant_payload_binding() {
	mut cur := Node{
		factor: 1
	}
	for f in [2, 3, 5] {
		shape := Shape.Box(cur)
		match shape {
			Shape.Box(n) {
				cur = Node{&n, f}
			}
			Shape.Nothing {}
		}
	}
	assert node_depth(cur) == 3
}

fn chain_depth(n &Node) int {
	if n == unsafe { nil } {
		return 0
	}
	return 1 + chain_depth(n.next)
}

struct FixedNodes {
	items [1]Node
}

// `&w.items[0]` addresses the inline storage of the local `w` (a fixed array
// local itself can only be referenced in `unsafe`).
fn test_address_of_an_element_of_a_fixed_array_field() {
	mut saved := unsafe { &Node(nil) }
	for f in [2, 3, 5] {
		x := Node{
			next:   saved
			factor: f
		}
		w := FixedNodes{
			items: [Node{
				next:   &x
				factor: f * 10
			}]!
		}
		saved = &w.items[0]
	}
	assert saved.factor == 50
	assert chain_depth(saved) == 6
}

fn test_address_stored_through_an_aggregate_projection() {
	mut saved := Holder{}
	for f in [2, 3, 5] {
		x := Node{
			next:   saved.next
			factor: f
		}
		saved = [Holder{&x}][0]
	}
	assert saved.next.factor == 5
	assert holder_depth(saved) == 3
}

fn test_address_read_through_a_pointer_to_an_aggregate() {
	mut saved := Holder{}
	for f in [2, 3, 5] {
		x := Node{
			next:   saved.next
			factor: f
		}
		h := Holder{&x}
		p := &h
		saved = Holder{p.next}
	}
	assert saved.next.factor == 5
	assert holder_depth(saved) == 3
}

fn test_local_declared_in_a_branch_of_the_assigned_value() {
	mut saved := Holder{}
	for f in [2, 3, 5] {
		saved = if f > 0 {
			x := Node{
				next:   saved.next
				factor: f
			}
			Holder{&x}
		} else {
			Holder{}
		}
	}
	assert saved.next.factor == 5
	assert holder_depth(saved) == 3
}

fn test_address_written_into_a_field_of_a_local_aggregate() {
	mut saved := Chain{}
	for f in [2, 3, 5] {
		mut tmp := Chain{}
		mut x := Node{
			next:   saved.next
			factor: f
		}
		tmp.next = &x
		saved = tmp
	}
	assert saved.next.factor == 5
	assert node_depth(*saved.next) == 2
}

struct ErrHolder {
	err &IError = unsafe { nil }
}

fn fail(n int) !int {
	return error('fail ${n}')
}

fn test_implicit_err_binding_of_an_or_block() {
	mut first := ErrHolder{}
	for i in 0 .. 3 {
		mut saved := ErrHolder{}
		n := fail(i) or {
			saved = ErrHolder{&err}
			0
		}
		assert n == 0
		if i == 0 {
			first = saved
		}
	}
	// Each execution of the `or` block has an `err` of its own.
	assert first.err.msg() == 'fail 0'
}

fn test_implicit_err_binding_of_an_if_guard_else() {
	mut first := ErrHolder{}
	for i in 0 .. 3 {
		mut saved := ErrHolder{}
		if n := fail(i) {
			assert n < 0
		} else {
			saved = ErrHolder{&err}
		}
		if i == 0 {
			first = saved
		}
	}
	assert first.err.msg() == 'fail 0'
}
