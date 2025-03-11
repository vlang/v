module build_constraint

// ast:
struct BExpr {
	expr BOr
}

struct BOr {
	exprs []BAnd
}

struct BAnd {
	exprs []BUnary
}

type BUnary = BNot | BExpr | BFact | BDefine

struct BNot {
	expr BUnary
}

type BFact = string
type BDefine = string
