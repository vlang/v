type Node = Expr | string
type Expr = IfExpr | IntegerLiteral

struct IntegerLiteral {}
struct IfExpr {
	pos int
}

struct NodeWrapper {
	node Node
}

fn test_nested_sumtype_selector() {
	c := NodeWrapper{Node(Expr(IfExpr{pos: 1}))}
	for c.node is Expr {
		assert typeof(c.node).name == 'Expr'
		break
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

type Food = Milk | Eggs

struct FoodWrapper {
mut:
	food Food
}

fn test_match_mut() {
	mut f := Food(Eggs{'test'})
	for mut f is Eggs {
		f.name = 'eggs'
		assert f.name == 'eggs'
		break
	}
}

fn test_conditional_break() {
	mut f := Food(Eggs{'test'})
	for mut f is Eggs {
		f = Milk{'test'}
	}
	assert true
}
