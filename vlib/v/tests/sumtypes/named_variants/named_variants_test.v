import shapes

struct Point {
	x int
	y int
}

type Expr = IntLit(int) | Count(int) | Str(string) | Void

type Payload = Pt(Point) | Pts([]Point) | Tags(map[string]int) | Maybe(?int) | Inner(Expr)

type Tree = Leaf(int) | Node([]Tree) | Ref(&Tree) | Nil

// A user type and another sum type's variant may share a variant's name.
type Void = int

type Other = Void | Count(string)

type Opt[T] = Some(T) | Nothing

fn test_explicit_generic_named_variant_patterns() {
	value := Opt[int].Some(7)
	assert value is Opt[int].Some
	assert value !is Opt[int].Nothing
	match value {
		Opt[int].Some(n) {
			assert n == 7
		}
		Opt[int].Nothing {
			assert false
		}
	}
	empty := Opt[string].Nothing
	match empty {
		Opt[string].Some(_) {
			assert false
		}
		Opt[string].Nothing {
			assert true
		}
	}
	imported := shapes.Choice[shapes.Shape].Value(shapes.Shape.Nothing)
	assert imported is shapes.Choice[shapes.Shape].Value
	match imported {
		shapes.Choice[shapes.Shape].Value(payload) {
			assert payload is shapes.Shape.Nothing
		}
		shapes.Choice[shapes.Shape].Empty {
			assert false
		}
	}
}

type Single = Only(int)

fn eval(e Expr) string {
	return match e {
		Expr.IntLit(n) { 'int ${n}' }
		Expr.Count(n) { 'count ${n}' }
		Expr.Str(s) { 'str ${s}' }
		Expr.Void { 'void' }
	}
}

fn sum_tree(t Tree) int {
	match t {
		Tree.Leaf(v) {
			return v
		}
		Tree.Node(children) {
			mut total := 0
			for c in children {
				total += sum_tree(c)
			}
			return total
		}
		Tree.Ref(r) {
			return sum_tree(*r)
		}
		Tree.Nil {
			return 0
		}
	}
}

fn unwrap_or[T](o Opt[T], d T) T {
	return match o {
		Opt.Some(v) { v }
		Opt.Nothing { d }
	}
}

fn make_count(n int) Expr {
	return Expr.Count(n)
}

fn test_same_payload_type_twice() {
	assert eval(Expr.IntLit(3)) == 'int 3'
	assert eval(Expr.Count(3)) == 'count 3'
	assert Expr.IntLit(3) != Expr.Count(3)
	assert eval(Expr.Str('hi')) == 'str hi'
	assert eval(Expr.Void) == 'void'
}

fn test_is_checks() {
	e := Expr.Count(1)
	assert e is Expr.Count
	assert e !is Expr.IntLit
	assert !(e is Expr.Void)
}

fn test_equality() {
	assert Expr.Count(2) == Expr.Count(2)
	assert Expr.Count(2) != Expr.Count(3)
	assert Expr.Void == Expr.Void
	assert Expr.Void != Expr.Count(0)
	assert Payload.Pt(Point{1, 2}) == Payload.Pt(Point{1, 2})
	assert Payload.Pts([Point{}]) != Payload.Pts([])
}

fn test_str() {
	assert Expr.Count(3).str() == 'Expr.Count(3)'
	assert '${Expr.Str('a')}' == "Expr.Str('a')"
	assert '${Expr.Void}' == 'Expr.Void'
	assert '${[Expr.IntLit(1), Expr.Void]}' == '[Expr.IntLit(1), Expr.Void]'
	assert '${Payload.Maybe(none)}' == 'Payload.Maybe(Option(none))'
	assert '${Payload.Inner(Expr.Count(4))}' == 'Payload.Inner(Expr.Count(4))'
	assert Expr.Count(1).type_name() == 'Expr.Count'
	assert Expr.Void.type_name() == 'Expr.Void'
}

fn test_match_expression_value_and_else() {
	e := make_count(5)
	doubled := match e {
		Expr.Count(n) { n * 2 }
		else { 0 }
	}
	assert doubled == 10
	kind := match Expr.Str('x') {
		Expr.IntLit, Expr.Count { 'number' }
		Expr.Str, Expr.Void { 'other' }
	}
	assert kind == 'other'
}

fn test_match_on_call_subject_binds_payload() {
	mut got := 0
	match make_count(7) {
		Expr.Count(n) { got = n }
		else {}
	}
	assert got == 7
}

fn test_binding_is_a_copy_and_match_mut_reassigns() {
	mut e := Expr.Count(1)
	match mut e {
		Expr.Count(n) { e = Expr.Count(n + 1) }
		else {}
	}
	assert e == Expr.Count(2)
}

fn test_payload_types() {
	p := Payload.Pts([Point{1, 2}, Point{3, 4}])
	match p {
		Payload.Pts(points) {
			assert points.len == 2
			assert points[1].y == 4
		}
		else {
			assert false
		}
	}
	tags := Payload.Tags({
		'a': 1
	})
	match tags {
		Payload.Tags(m) {
			assert m['a'] == 1
		}
		else {
			assert false
		}
	}
	maybe := Payload.Maybe(5)
	match maybe {
		Payload.Maybe(v) {
			assert v or { 0 } == 5
		}
		else {
			assert false
		}
	}
	inner := Payload.Inner(Expr.IntLit(9))
	match inner {
		Payload.Inner(ex) {
			assert eval(ex) == 'int 9'
		}
		else {
			assert false
		}
	}
}

fn test_recursive_sum_type() {
	t := Tree.Node([Tree.Leaf(1), Tree.Leaf(2), Tree.Node([Tree.Leaf(3)]), Tree.Nil])
	assert sum_tree(t) == 6
	leaf := Tree.Leaf(10)
	assert sum_tree(Tree.Ref(&leaf)) == 10
}

fn test_recursive_named_sum_string() {
	tree := Tree.Node([Tree.Leaf(1), Tree.Nil])
	assert tree.str() == 'Tree.Node([Tree.Leaf(1), Tree.Nil])'
	assert '${tree}' == 'Tree.Node([Tree.Leaf(1), Tree.Nil])'
	nested := Tree.Node([Tree.Node([Tree.Leaf(1)]), Tree.Nil])
	assert nested.str() == 'Tree.Node([Tree.Node([Tree.Leaf(1)]), Tree.Nil])'
	leaf := Tree.Leaf(1)
	assert Tree.Ref(&leaf).str() == 'Tree.Ref(&Tree.Leaf(1))'
	mut cycle := Tree.Nil
	cycle = Tree.Ref(&cycle)
	assert cycle.str().contains('<circular>')
}

fn test_variant_names_are_scoped() {
	o := Other.Void
	v := Void(3)
	assert '${o}' == 'Other.Void'
	assert v == 3
	assert Other.Count('x') != Other.Void
	assert eval(Expr.Count(1)) == 'count 1'
}

fn test_closure_captures_binding() {
	e := Expr.Count(21)
	match e {
		Expr.Count(n) {
			f := fn [n] () int {
				return n * 2
			}
			assert f() == 42
		}
		else {
			assert false
		}
	}
}

fn test_generic_named_sum_type() {
	a := Opt[int].Some(3)
	b := Opt[int].Nothing
	assert unwrap_or(a, 0) == 3
	assert unwrap_or(b, 7) == 7
	assert '${a}' == 'Opt[int].Some(3)'
	s := Opt[string].Some('x')
	assert unwrap_or(s, '') == 'x'
}

fn test_single_variant() {
	s := Single.Only(4)
	match s {
		Single.Only(n) {
			assert n == 4
		}
	}
}

fn test_struct_field_and_comptime_variants() {
	mut names := []string{}
	$for v in Expr.variants {
		names << typeof(v.typ).name
	}
	assert names == ['Expr.IntLit', 'Expr.Count', 'Expr.Str', 'Expr.Void']
}

fn test_other_module() {
	s := shapes.Shape.Square(2.0)
	assert shapes.describe(s) == 'square 2.0'
	assert shapes.describe(shapes.Shape.Nothing) == 'nothing'
	assert s is shapes.Shape.Square
	match shapes.unit() {
		shapes.Shape.Circle(r) {
			assert r == 1.0
		}
		else {
			assert false
		}
	}
	assert '${s}' == 'Shape.Square(2.0)'
}
