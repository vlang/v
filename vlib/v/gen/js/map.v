module js

import v.ast

const special_map_methods = [
	'keys',
	'values',
]

fn (mut g JsGen) gen_map_method_call(node ast.CallExpr) {
	g.write('map_${node.name}(')
	g.expr(node.left)
	g.gen_deref_ptr(node.left_type)
	g.write(')')
}
