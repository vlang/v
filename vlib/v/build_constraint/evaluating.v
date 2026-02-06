module build_constraint

// evaluating the AST nodes, in the given environment
fn (b BExpr) eval(env &Environment) !bool {
	return b.expr.eval(env)
}

fn (b BOr) eval(env &Environment) !bool {
	for e in b.exprs {
		if e.eval(env)! {
			return true
		}
	}
	return false
}

fn (b BAnd) eval(env &Environment) !bool {
	for e in b.exprs {
		if !e.eval(env)! {
			return false
		}
	}
	return true
}

fn (b BUnary) eval(env &Environment) !bool {
	match b {
		BNot, BExpr, BFact, BDefine { return b.eval(env)! }
	}
	return false
}

fn (b BNot) eval(env &Environment) !bool {
	return !b.expr.eval(env)!
}

fn (b BFact) eval(env &Environment) !bool {
	return env.is_fact(b)
}

fn (b BDefine) eval(env &Environment) !bool {
	return env.is_define(b)
}
