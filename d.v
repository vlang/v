import v.ast
import v.pref
import v.eval
import v.serialise
import v.builder

fn main() {
	mut vpref, _ := pref.parse_args([], ['interpret', ''])
	vpref.is_script = true

	mut b := builder.new_builder(vpref)
	b.table.pointer_size = 4
	mut veval := eval.new_eval(b.table, vpref)

	mut files := b.get_builtin_files()
	b.set_module_lookup_paths()
	b.interpret_text('', files)!
	veval.register_symbols(mut b.parsed_files)

	mut pool := serialise.new_pool(b.table, veval, endianness: .little, store_relocs: false, intern_strings: true, null_terminated: false)

	/* pool.append(ast.BoolLiteral{val: true}, 0)
	println(pool.buf)
	
	
	pool.append(ast.FloatLiteral{val: "0"}, ast.f32_type)
	println(pool.buf)
	
	pool.append(ast.IntegerLiteral{val: "-1"}, ast.i64_type)
	println(pool.buf) */

	val := pool.append(ast.StringLiteral{val: "AAAAAAAAAAAAA\\n", is_raw: false}, ast.string_type)
	val2 := pool.append(ast.StringLiteral{val: "AAA", is_raw: false}, ast.string_type)
	println(pool.buf)
	println(val)
	println(val2)
}