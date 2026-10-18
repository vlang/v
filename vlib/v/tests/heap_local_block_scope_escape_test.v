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
