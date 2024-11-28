import v.ast
import v.gen.wasm.serialise

fn test_alignment() {
	table := ast.new_table()

	mut pool := serialise.new_pool(table)
	pool.append(ast.BoolLiteral{ val: true }, 0) // +0, +1
	pool.append(ast.FloatLiteral{ val: '0' }, ast.f32_type) // +3, +4
	pool.append(ast.BoolLiteral{ val: true }, 0) // +0, +1
	assert pool.buf.len == 9
}
