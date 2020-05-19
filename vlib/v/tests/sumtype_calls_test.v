struct Foo {
}

type Expr = BinExpr | BoolExpr | DeclExprA | DeclExprB | Foo | UnaryExpr

type DeclExpr = DeclExprA | DeclExprB

struct BoolExpr {
	foo int
}

struct BinExpr {
	name string
}

struct DeclExprA {
	name string
}

struct DeclExprB {
	name string
}

fn expr1() Expr {
	mut e := Expr{}
	e = BinExpr{
		name: 'binexpr'
	}
	return e
	// return BinExpr{}
}

fn expr() Expr {
	return BinExpr{}
}

struct UnaryExpr {
}

fn handle_expr(e Expr) {
}

fn handle_decl_expr(de DeclExpr) {
}

fn parse_bool() BoolExpr {
	return BoolExpr{}
}

fn test_sum_type_cast() {
	a := expr1()
	b := a as BinExpr
	assert b.name == 'binexpr'
}

fn test_sum_types() {
	b := parse_bool()
	handle_expr(b)
	de := DeclExprA{}
	handle_expr(de)
	handle_decl_expr(de)
}

/*
#define ExprType_BoolExpr 0
#define ExprType_BinExpr 1
#define ExprType_UnaryExpr 2

struct Expr {
	int   typ;
	void* obj;
}
*/
