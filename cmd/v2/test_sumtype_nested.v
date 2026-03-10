// Test sum type with struct variants (not just primitives)
// This mirrors the AST's Expr sum type pattern

struct BasicLit {
	kind  int
	value string
}

struct Ident {
	name string
}

struct CallOrCast {
	lhs  Expr
	expr Expr
}

type Expr = BasicLit | CallOrCast | Ident

fn make_basic_lit(val string) Expr {
	return BasicLit{
		kind:  1
		value: val
	}
}

fn make_ident(name string) Expr {
	return Ident{
		name: name
	}
}

fn make_call_or_cast(lhs Expr, expr Expr) Expr {
	return CallOrCast{
		lhs:  lhs
		expr: expr
	}
}

fn test_basic_lit() {
	e := make_basic_lit('42')
	if e is BasicLit {
		assert e.value == '42'
		assert e.kind == 1
	} else {
		assert false
	}
}

fn test_ident() {
	e := make_ident('hello')
	if e is Ident {
		assert e.name == 'hello'
	} else {
		assert false
	}
}

fn test_call_or_cast() {
	lhs := make_ident('int')
	arg := make_basic_lit('0')
	e := make_call_or_cast(lhs, arg)
	if e is CallOrCast {
		if e.lhs is Ident {
			assert e.lhs.name == 'int'
		} else {
			assert false
		}
		if e.expr is BasicLit {
			assert e.expr.value == '0'
		} else {
			assert false
		}
	} else {
		assert false
	}
}

fn test_nested_sum_field_access() {
	// Create CallOrCast{lhs: Ident("int"), expr: BasicLit("0")}
	// This mirrors: __global g_main_argc = int(0)
	lhs := Expr(Ident{
		name: 'int'
	})
	arg := Expr(BasicLit{
		kind:  1
		value: '0'
	})
	coce := CallOrCast{
		lhs:  lhs
		expr: arg
	}
	e := Expr(coce)
	if e is CallOrCast {
		// Access the nested Expr fields
		inner_expr := e.expr
		if inner_expr is BasicLit {
			assert inner_expr.value == '0'
		} else {
			eprintln('ERROR: inner_expr is not BasicLit')
			assert false
		}
	} else {
		eprintln('ERROR: e is not CallOrCast')
		assert false
	}
}
