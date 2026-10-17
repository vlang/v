struct First {}

struct Second {}

type Node = First | Second

fn lower_first(_ First) !int {
	return 1
}

fn lower_second(_ Second) !int {
	return 2
}

fn cast_unsafe_match(node Node) !i64 {
	return i64(unsafe {
		match node {
			First { lower_first(node)! }
			Second { lower_second(node)! }
		}
	})
}

fn cast_unsafe_match_in_if_guard(node ?Node) !i64 {
	result := if value := node {
		i64(unsafe {
			match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}
		})
	} else {
		i64(0)
	}
	return result
}

// A cast of an `unsafe` block whose value is a `match` with propagating arms must lower
// the arms as values.
fn test_cast_of_unsafe_match_with_propagating_arms() {
	assert cast_unsafe_match(First{}) or { panic(err) } == 1
	assert cast_unsafe_match(Second{}) or { panic(err) } == 2
	assert cast_unsafe_match_in_if_guard(Second{}) or { panic(err) } == 2
	assert cast_unsafe_match_in_if_guard(none) or { panic(err) } == 0
}

struct Tracer {
mut:
	order []int
}

fn (mut tr Tracer) values() []int {
	tr.order << 1
	return [10, 20, 30]
}

fn (mut tr Tracer) row() int {
	tr.order << 1
	return 1
}

fn (mut tr Tracer) index_first(_ First) !int {
	tr.order << 2
	return 0
}

fn (mut tr Tracer) index_second(_ Second) !int {
	tr.order << 2
	return 1
}

struct Cell {
mut:
	v int
}

fn (mut c Cell) bump() int {
	c.v++
	return c.v
}

fn call_base(node Node) !int {
	mut tr := Tracer{}
	value := tr.values()[match node {
		First { tr.index_first(node)! }
		Second { tr.index_second(node)! }
	}]
	return value * 100 + tr.order[0] * 10 + tr.order[1]
}

fn call_base_nested_index(node Node) !int {
	mut tr := Tracer{}
	value := tr.values()[1 + (match node {
		First { tr.index_first(node)! }
		Second { tr.index_second(node)! }
	})]
	return value * 100 + tr.order[0] * 10 + tr.order[1]
}

fn indexed_base(node Node) !int {
	mut tr := Tracer{}
	rows := [[1, 2], [3, 4]]
	value := rows[tr.row()][match node {
		First { tr.index_first(node)! }
		Second { tr.index_second(node)! }
	}]
	return value * 100 + tr.order[0] * 10 + tr.order[1]
}

fn indexed_base_mut_receiver(node Node) !int {
	mut tr := Tracer{}
	mut grid := [[Cell{}, Cell{}]!, [Cell{}, Cell{}]!]
	bumped := grid[tr.row()][match node {
		First { tr.index_first(node)! }
		Second { tr.index_second(node)! }
	}].bump()
	return grid[1][0].v * 1000 + bumped * 100 + tr.order[0] * 10 + tr.order[1]
}

// A side-effecting index base runs before the prelude hoisted from a later `match` index
// with propagating arms: the trace records the base (1) before the index (2).
fn test_index_base_runs_before_a_hoisted_index_branch() {
	assert call_base(First{}) or { panic(err) } == 1012
	assert call_base_nested_index(First{}) or { panic(err) } == 2012
	assert indexed_base(First{}) or { panic(err) } == 312
	// The element is still mutated in place through the original grid.
	assert indexed_base_mut_receiver(First{}) or { panic(err) } == 1112
}
