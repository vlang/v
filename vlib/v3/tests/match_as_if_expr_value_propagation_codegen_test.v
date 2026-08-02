// Regression test for https://github.com/vlang/v/issues/28000
// A `match` whose arms use `!`/`?` propagation, used as the value of an
// `if`-expression, must assign the unwrapped result to the if-expression's
// result temp (it used to emit an empty expression `_t = ;`).
import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_match_as_if_expr_value_with_propagation() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_match_as_if_expr_value_propagation_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_match_as_if_expr_value_propagation_input.v')
	os.write_file(src, 'module main

struct First {}
struct Second {}

type Node = First | Second

fn lower_first(_ First) !int {
	return 1
}

fn lower_second(_ Second) !int {
	return 2
}

fn select_value(node ?Node) !int {
	result := if value := node {
		match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}
	} else {
		0
	}
	return result
}

fn select_value_paren(node ?Node) !int {
	result := if value := node {
		(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		0
	}
	return result
}

fn select_value_unsafe(node ?Node) !int {
	result := if value := node {
		(unsafe {
			match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}
		})
	} else {
		0
	}
	return result
}

fn select_value_cast(node ?Node) !i64 {
	result := if value := node {
		i64(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		i64(0)
	}
	return result
}

fn select_value_cast_unsafe(node ?Node) !i64 {
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

fn select_value_infix_right(node ?Node) !int {
	result := if value := node {
		1 + (match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		0
	}
	return result
}

fn select_value_infix_left(node ?Node) !int {
	result := if value := node {
		(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}) + 10
	} else {
		0
	}
	return result
}

fn wrap(x int) int {
	return x * 10
}

fn select_value_callarg(node ?Node) !int {
	result := if value := node {
		wrap(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		0
	}
	return result
}

fn select_value_nested_callarg(node ?Node) !int {
	result := if value := node {
		wrap(wrap(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}))
	} else {
		0
	}
	return result
}

fn select_value_callarg_infix(node ?Node) !int {
	result := if value := node {
		wrap(1 + (match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}))
	} else {
		0
	}
	return result
}

fn select_value_arraylit(node ?Node) ![]int {
	result := if value := node {
		[match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}]
	} else {
		[0]
	}
	return result
}

fn select_value_prefix(node ?Node) !int {
	result := if value := node {
		-(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})
	} else {
		0
	}
	return result
}

fn select_value_index(node ?Node) !int {
	values := [10, 20, 30]
	result := if value := node {
		values[match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}]
	} else {
		0
	}
	return result
}

fn select_value_slice_bound(node ?Node) ![]int {
	values := [10, 20, 30, 40]
	result := if value := node {
		values[(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		})..]
	} else {
		[0]
	}
	return result
}

fn select_value_membership(node ?Node) !bool {
	result := if value := node {
		(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}) in [1, 2]
	} else {
		false
	}
	return result
}

fn select_value_mapkey(node ?Node) !map[int]int {
	result := if value := node {
		{(match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}): 100}
	} else {
		{0: 100}
	}
	return result
}

fn select_value_interp(node ?Node) !string {
	result := if value := node {
		"x=\${match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}}"
	} else {
		"x=0"
	}
	return result
}

fn bool_first(_ First) !bool {
	return true
}

fn bool_second(_ Second) !bool {
	return false
}

fn select_value_likely(node ?Node) !bool {
	result := if value := node {
		_likely_(match value {
			First { bool_first(value)! }
			Second { bool_second(value)! }
		})
	} else {
		false
	}
	return result
}

struct Boxed {
	value int
}

fn boxed(v int) Boxed {
	return Boxed{v}
}

fn select_value_selector(node ?Node) !int {
	result := if value := node {
		(match value {
			First { boxed(lower_first(value)!) }
			Second { boxed(lower_second(value)!) }
		}).value
	} else {
		0
	}
	return result
}

struct Holder {
	value int
	other int
}

fn select_value_mapinit(node ?Node) !map[int]int {
	result := if value := node {
		{
			7: match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}
		}
	} else {
		{
			7: 0
		}
	}
	return result
}

fn select_value_structinit(node ?Node) !Holder {
	result := if value := node {
		Holder{
			value: match value {
				First { lower_first(value)! }
				Second { lower_second(value)! }
			}
			other: 100
		}
	} else {
		Holder{}
	}
	return result
}

struct Circle {
	r int
}

struct Square {
	s int
}

type Shape = Circle | Square

fn make_circle(r int) !Shape {
	return Circle{r}
}

fn select_value_ascast(node ?int) !int {
	shape := if v := node {
		(match v {
			0 { make_circle(v)! }
			else { make_circle(v + 1)! }
		}) as Circle
	} else {
		Circle{99}
	}
	return shape.r
}

fn select_value_ascast_unsafe(node ?int) !int {
	shape := if v := node {
		(unsafe {
			match v {
				0 { make_circle(v)! }
				else { make_circle(v + 1)! }
			}
		}) as Circle
	} else {
		Circle{99}
	}
	return shape.r
}

fn direct_match(node Node) !int {
	return match node {
		First { lower_first(node)! }
		Second { lower_second(node)! }
	}
}

struct Tracer {
mut:
	order []int
}

fn (mut tr Tracer) lhs() int {
	tr.order << 1
	return 1
}

fn (mut tr Tracer) rf(_ First) !int {
	tr.order << 2
	return 10
}

fn (mut tr Tracer) rs(_ Second) !int {
	tr.order << 2
	return 20
}

// The LHS call must be evaluated before the RHS match materialization prelude.
// Encodes sum (11) and the recorded order ([1,2] = LHS then RHS) as 1112; a
// reversed order would yield 1121.
fn select_value_infix_order(node ?Node) !int {
	mut tr := Tracer{}
	sum := if value := node {
		tr.lhs() + (match value {
			First { tr.rf(value)! }
			Second { tr.rs(value)! }
		})
	} else {
		0
	}
	return sum * 100 + tr.order[0] * 10 + tr.order[1]
}

fn (mut tr Tracer) shift_lhs() int {
	tr.order << 1
	return 1
}

// Left-shift ordering: the side-effecting LHS must run before the RHS match
// prelude. Encodes shift result (1 << 10 = 1024) and order ([1,2]) as 102412; a
// reversed order would be 102421.
fn select_value_shift_order(node Node) !int {
	mut tr := Tracer{}
	sum := tr.shift_lhs() << (match node {
		First { tr.rf(node)! }
		Second { tr.rs(node)! }
	})
	return sum * 100 + tr.order[0] * 10 + tr.order[1]
}

fn (mut tr Tracer) base_values() []int {
	tr.order << 1
	return [10, 20, 30]
}

fn (mut tr Tracer) idx_first(_ First) !int {
	tr.order << 2
	return 0
}

fn (mut tr Tracer) idx_second(_ Second) !int {
	tr.order << 2
	return 1
}

// Index ordering: the side-effecting base must run before the match-index prelude.
// Encodes indexed value ([10,20,30][0] = 10) and order ([1,2]) as 1012; a reversed
// order would be 1021.
fn select_value_index_order(node Node) !int {
	mut tr := Tracer{}
	val := tr.base_values()[match node {
		First { tr.idx_first(node)! }
		Second { tr.idx_second(node)! }
	}]
	return val * 100 + tr.order[0] * 10 + tr.order[1]
}

fn (mut tr Tracer) gated_first(_ First) !int {
	tr.order << 2
	return -1
}

fn (mut tr Tracer) gated_second(_ Second) !int {
	tr.order << 2
	return -2
}

// Gated index (`#[]`) with a propagating value match: the match tail must be
// lowered as a value (the gated helper otherwise lowers it with plain
// `transform_expr`). Encodes the gated value ([10,20,30]#[-1] = 30) and order
// ([1,2]) as 3012.
fn select_value_gated_index_order(node Node) !int {
	mut tr := Tracer{}
	val := tr.base_values()#[match node {
		First { tr.gated_first(node)! }
		Second { tr.gated_second(node)! }
	}]
	return val * 100 + tr.order[0] * 10 + tr.order[1]
}

// Address-of a value match (the checker permits `&` on a struct-typed match):
// the propagating branch tail is materialized to a value temp whose address is
// taken, then a field is read through it.
fn select_value_addr(node ?Node) !int {
	result := if value := node {
		p := &(match value {
			First { boxed(lower_first(value)!) }
			Second { boxed(lower_second(value)!) }
		})
		p.value
	} else {
		0
	}
	return result
}

fn main() {
	println(select_value(First{})!)
	println(select_value(Second{})!)
	println(select_value_paren(First{})!)
	println(select_value_unsafe(Second{})!)
	println(select_value_cast(First{})!)
	println(select_value_cast_unsafe(Second{})!)
	println(select_value_infix_right(First{})!)
	println(select_value_infix_left(Second{})!)
	println(select_value_callarg(Second{})!)
	println(select_value_nested_callarg(First{})!)
	println(select_value_callarg_infix(First{})!)
	println(select_value_arraylit(First{})!)
	println(select_value_prefix(First{})!)
	println(select_value_index(First{})!)
	println(select_value_slice_bound(First{})!)
	println(select_value_membership(First{})!)
	println(select_value_selector(First{})!)
	println(select_value_mapkey(First{})![1])
	println(select_value_interp(First{})!)
	println(select_value_likely(First{})!)
	println(select_value_structinit(First{})!.value)
	println(select_value_mapinit(Second{})![7])
	println(select_value_ascast(5)!)
	println(select_value_ascast_unsafe(5)!)
	println(direct_match(Second{})!)
	println(select_value_infix_order(First{})!)
	println(select_value_shift_order(First{})!)
	println(select_value_index_order(First{})!)
	println(select_value_gated_index_order(First{})!)
	println(select_value_addr(First{})!)
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_match_as_if_expr_value_propagation_out')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '1\n2\n1\n2\n1\n2\n2\n12\n20\n100\n20\n[1]\n-1\n20\n[20, 30, 40]\ntrue\n1\n100\nx=1\ntrue\n1\n2\n6\n6\n2\n1112\n102412\n1012\n3012\n1'
}
