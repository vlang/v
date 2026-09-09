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
	c := NodeWrapper{Node(Expr(IfExpr{
		pos: 1
	}))}
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

type Food = Eggs | Milk

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

type ReceiverExpr = ReceiverPar | int

struct ReceiverPar {
	expr ReceiverExpr
}

fn (mut expr ReceiverExpr) strip_par() ReceiverExpr {
	for mut expr is ReceiverPar {
		expr = expr.expr
	}
	return expr
}

fn test_receiver_var_smartcast() {
	mut expr := ReceiverExpr(ReceiverPar{
		expr: ReceiverExpr(ReceiverPar{
			expr: ReceiverExpr(1)
		})
	})
	assert expr.strip_par() == ReceiverExpr(1)
}
