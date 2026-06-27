import v2.ast

struct Parameter {
	typ ast.Expr
}

struct FnType {
	params []Parameter
}

fn classify(expr ast.Expr) int {
	match expr {
		ast.Type {
			if expr is ast.ArrayType {
				return 11
			}
			return 7
		}
		else {
			return 3
		}
	}
}

fn classify_param_types(fn_type FnType) int {
	mut out := 0
	idx := 0
	first := fn_type.params[idx]
	out += classify(first.typ)
	for i := 0; i < fn_type.params.len; i++ {
		param := fn_type.params[i]
		out += classify(param.typ)
	}
	for _, param in fn_type.params {
		out += classify(param.typ)
	}
	return out
}

fn main() {
	typ := ast.Type(ast.ArrayType{
		elem_type: ast.Expr(ast.Ident{
			name: 'int'
		})
	})
	fn_type := FnType{
		params: [
			Parameter{
				typ: ast.Expr(typ)
			},
		]
	}
	println(classify_param_types(fn_type))
}
