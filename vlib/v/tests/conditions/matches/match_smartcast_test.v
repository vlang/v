type Node = Expr | string
type Expr = IfExpr | IntegerLiteral

struct IntegerLiteral {}

struct IfExpr {
	pos int
}

struct NodeWrapper {
	node Node
}

fn test_nested_sumtype_match_selector() {
	c := NodeWrapper{Node(Expr(IfExpr{
		pos: 1
	}))}
	match c.node {
		Expr {
			match c.node {
				IfExpr {
					assert c.node.pos == 1
				}
				else {
					assert false
				}
			}
		}
		else {
			assert false
		}
	}
}

fn test_nested_sumtype_match() {
	c := Node(Expr(IfExpr{
		pos: 1
	}))
	match c {
		Expr {
			match c {
				IfExpr {
					assert c.pos == 1
				}
				else {
					assert false
				}
			}
		}
		else {
			assert false
		}
	}
}

struct Milk {
mut:
	name string
}

struct Eggs {
mut:
	name string
}

type Food = Eggs | Milk

struct FoodWrapper {
mut:
	food Food
}

fn test_match_mut() {
	mut f := Food(Milk{'test'})
	match mut f {
		Eggs {
			f.name = 'eggs'
			assert f.name == 'eggs'
		}
		Milk {
			f.name = 'milk'
			assert f.name == 'milk'
		}
	}
}

fn test_match_mut_selector() {
	mut f := FoodWrapper{Food(Milk{'test'})}
	match mut f.food {
		Eggs {
			f.food.name = 'eggs'
			assert f.food.name == 'eggs'
		}
		Milk {
			f.food.name = 'milk'
			assert f.food.name == 'milk'
		}
	}
}

// for issue 20167
type Any_1 = f64 | int | string
type Any_2 = int | string

struct Struct {
	field Any_2
}

fn test_branches_return_struct_field() {
	any_2 := Struct{Any_2(42)}
	m := {
		'item1': Any_1('')
		'item2': match any_2.field {
			string { any_2.field }
			int { any_2.field }
		}
	}
	assert m['item2']! == Any_1(42)
}

struct NestedMatchA {
	a int
}

struct NestedMatchB {
	b int
}

struct NestedMatchC {
	c int
}

type NestedMatchD = NestedMatchA | NestedMatchB
type NestedMatchE = NestedMatchC | NestedMatchD

fn describe_nested_match(e NestedMatchE) string {
	return match e {
		NestedMatchA { 'A:${e.a}' }
		NestedMatchB { 'B:${e.b}' }
		NestedMatchC { 'C:${e.c}' }
	}
}

fn pass_nested_match(e NestedMatchE) NestedMatchE {
	return e
}

fn test_nested_sumtype_leaf_match_and_argument() {
	a := NestedMatchA{
		a: 1
	}
	b := NestedMatchB{
		b: 2
	}
	c := NestedMatchC{
		c: 3
	}

	assert describe_nested_match(a) == 'A:1'
	assert describe_nested_match(b) == 'B:2'
	assert describe_nested_match(c) == 'C:3'
	assert describe_nested_match(pass_nested_match(a)) == 'A:1'
}
