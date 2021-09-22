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
