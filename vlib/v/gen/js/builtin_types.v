module js
import v.table

fn (mut g JsGen) to_js_typ_def_val(s string) string {
	mut dval := ''
	match s {
		'JS.Number' { dval = '0' }
		'JS.String' { dval = '""' }
		'JS.Boolean' { dval = 'false' }
		'JS.Array', 'JS.Map' { dval = '' }
		else { dval = '{}' }
	}
	return dval
}

fn (mut g JsGen) to_js_typ_val(t table.Type) string {
	sym := g.table.get_type_symbol(t)
	mut styp := ''
	mut prefix := if g.file.mod.name == 'builtin' { 'new ' } else { '' } 
	match sym.kind {
		.i8, .i16, .int, .i64, .byte, .u16, .u32, .u64, .f32, .f64, .int_literal, .float_literal, .size_t {
			styp = '${prefix}${g.sym_to_js_typ(sym)}(0)'
		}
		.bool {
			styp = '${prefix}${g.sym_to_js_typ(sym)}(false)'
		}
		.string {
			styp = '${prefix}${g.sym_to_js_typ(sym)}("")'
		}
		.map {
			styp = 'new Map()'
		}
		.array {
			styp = '${prefix}${g.sym_to_js_typ(sym)}()'
		}
		.struct_ {
			styp = 'new ${g.js_name(sym.name)}(${g.to_js_typ_def_val(sym.name)})'
		}
		else {
			// TODO
			styp = 'undefined'
		}
	}
	return styp
}

fn (mut g JsGen) sym_to_js_typ(sym table.TypeSymbol) string {
	mut styp := ''
	match sym.kind {
		.i8 { styp = 'i8' }
		.i16 { styp = 'i16' }
		.int { styp = 'int' }
		.i64 { styp = 'i64' }
		.byte { styp = 'byte' }
		.u16 { styp = 'u16' }
		.u32 { styp = 'u32' }
		.u64 { styp = 'u64' }
		.f32 { styp = 'f32' }
		.f64 { styp = 'f64' }
		.int_literal { styp = 'int_literal' }
		.float_literal { styp = 'float_literal' }
		.size_t { styp = 'size_t' }
		.bool { styp = 'bool' }
		.string { styp = 'string' }
		.map { styp = 'map' }
		.array { styp = 'array' }
		else {
			// TODO
			styp = 'undefined'
		}
	}
	return styp
}

// V type to JS type
pub fn (mut g JsGen) typ(t table.Type) string {
	sym := g.table.get_type_symbol(t)
	mut styp := ''
	match sym.kind {
		.placeholder {
			// This should never happen: means checker bug
			styp = 'any'
		}
		.void {
			styp = 'void'
		}
		.voidptr {
			styp = 'any'
		}
		.byteptr, .charptr {
			styp = '${g.sym_to_js_typ(sym)}'
		}
		.i8, .i16, .int, .i64, .byte, .u16, .u32, .u64, .f32, .f64, .int_literal, .float_literal, .size_t {
			styp = '${g.sym_to_js_typ(sym)}'
		}
		.bool {
			styp = '${g.sym_to_js_typ(sym)}'
		}
		.none_ {
			styp = 'undefined'
		}
		.string, .ustring, .char {
			styp = '${g.sym_to_js_typ(sym)}'
		}
		// 'array_array_int' => 'number[][]'
		.array {
			info := sym.info as table.Array
			styp = '${g.sym_to_js_typ(sym)}(${g.typ(info.elem_type)})'
		}
		.array_fixed {
			info := sym.info as table.ArrayFixed
			styp = '${g.sym_to_js_typ(sym)}(${g.typ(info.elem_type)})'
		}
		.chan {
			styp = 'chan'
		}
		// 'map[string]int' => 'Map<string, number>'
		.map {
			info := sym.info as table.Map
			key := g.typ(info.key_type)
			val := g.typ(info.value_type)
			styp = 'Map<$key, $val>'
		}
		.any {
			styp = 'any'
		}
		// ns.Foo => alias["Foo"]["prototype"]
		.struct_ {
			styp = g.struct_typ(sym.name)
		}
		.generic_struct_inst {}
		// 'multi_return_int_int' => '[number, number]'
		.multi_return {
			info := sym.info as table.MultiReturn
			types := info.types.map(g.typ(it))
			joined := types.join(', ')
			styp = '[$joined]'
		}
		.sum_type {
			// TODO: Implement sumtypes
			styp = 'union_sym_type'
		}
		.alias {
			// TODO: Implement aliases
			styp = 'alias'
		}
		.enum_ {
			// NB: We could declare them as TypeScript enums but TS doesn't like
			// our namespacing so these break if declared in a different module.
			// Until this is fixed, We need to use the type of an enum's members
			// rather than the enum itself, and this can only be 'number' for now
			styp = 'number'
		}
		// 'anon_fn_7_7_1' => '(a number, b number) => void'
		.function {
			info := sym.info as table.FnType
			styp = g.fn_typ(info.func.params, info.func.return_type)
		}
		.interface_ {
			styp = g.js_name(sym.name)
		}
		.rune {
			styp = 'any'
		}
		.aggregate {
			panic('TODO: unhandled aggregate in JS')
		}
		.thread {
			panic('TODO: unhandled thread in JS')
		}
	}
	/*
	else {
			println('jsgen.typ: Unhandled type $t')
			styp = sym.name
		}
	*/
	if styp.starts_with('JS.') {
		return styp[3..]
	}
	return styp
}

fn (mut g JsGen) fn_typ(args []table.Param, return_type table.Type) string {
	mut res := '('
	for i, arg in args {
		res += '$arg.name: ${g.typ(arg.typ)}'
		if i < args.len - 1 {
			res += ', '
		}
	}
	return res + ') => ' + g.typ(return_type)
}

fn (mut g JsGen) struct_typ(s string) string {
	ns := get_ns(s)
	if ns == 'JS' { return s[3..] }
	mut name := if ns == g.ns.name { s.split('.').last() } else { g.get_alias(s) }
	mut styp := ''
	for i, v in name.split('.') {
		if i == 0 {
			styp = v
		} else {
			styp += '["$v"]'
		}
	}
	if ns in ['', g.ns.name] {
		return styp
	}
	return styp + '["prototype"]'
}

struct BuiltinPrototypeCongig {
	typ_name 		string
	val_name 		string = 'val'
	default_value 	string
	constructor 	string = 'this.val = val'
	value_of 		string = 'this.val'
	to_string 		string = 'this.val.toString()'
	eq				string = 'this.val === other.val'
	extras			string
}

// ugly arguments but not sure a config struct would be worth it
fn (mut g JsGen) gen_builtin_prototype(c BuiltinPrototypeCongig) {
	g.writeln('function ${c.typ_name}(${c.val_name} = ${c.default_value}) { ${c.constructor} }')
	g.writeln('${c.typ_name}.prototype = {')
	g.inc_indent()
	g.writeln('val: ${c.default_value},')
	if c.extras.len > 0 {
		g.writeln('${c.extras},')
	}
	for method in g.method_fn_decls[c.typ_name] {
		g.inside_def_typ_decl = true
		g.gen_method_decl(method)
		g.inside_def_typ_decl = false
		g.writeln(',')
	}
	g.writeln('valueOf() { return ${c.value_of} },')
	g.writeln('toString() { return ${c.to_string} },')
	g.writeln('eq(other) { return ${c.eq} },')
	g.writeln('str() { return new string(this.toString()) }')
	g.dec_indent()
	g.writeln('};\n')
}

// generate builtin type definitions, used for casting and methods.
fn (mut g JsGen) gen_builtin_type_defs() {
	g.inc_indent()
	for typ_name in v_types {
		// TODO: JsDoc
		match typ_name {
			'i8', 'i16', 'int', 'i64', 'u16', 'u32', 'u64', 'int_literal', 'size_t' {
				// TODO: Bounds checking
				g.gen_builtin_prototype({
					typ_name: typ_name
					default_value: 'new Number(0)'
					constructor: 'this.val = val | 0'
					value_of: 'this.val | 0'
					to_string: 'this.valueOf().toString()'
					eq: 'this.valueOf() === other.valueOf()'
				})
			}
			'byte' {
				g.gen_builtin_prototype({
					typ_name: typ_name
					default_value: 'new Number(0)'
					constructor: 'this.val = typeof(val) == "string" ? val.charCodeAt() : (val | 0)'
					value_of: 'this.val | 0'
					to_string: 'String.fromCharCode(this.val)'
					eq: 'this.valueOf() === other.valueOf()'
				})
			}
			'f32', 'f64', 'float_literal' {
				g.gen_builtin_prototype({
					typ_name: typ_name
					default_value: 'new Number(0)'
				})
			}
			'bool' {
				g.gen_builtin_prototype({
					typ_name: typ_name
					default_value: 'new Boolean(false)'
				})
			}
			'string' {
				g.gen_builtin_prototype({
					typ_name: typ_name
					val_name: 'str'
					default_value: 'new String("")'
					constructor: 'this.str = str.toString(); this.len = this.str.length'
					value_of: 'this.str'
					to_string: 'this.str'
					eq: 'this.str === other.str'
				})
			}
			'map' {
				g.gen_builtin_prototype({
					typ_name: typ_name
					val_name: 'map'
					default_value: 'new Map()'
					constructor: 'this.map = map'
					value_of: 'this.map'
					to_string: 'this.map.toString()'
					eq: 'vEq(this, other)'
				})
			}
			'array' {
				g.gen_builtin_prototype({
					typ_name: typ_name
					val_name: 'arr'
					default_value: 'new Array()'
					constructor: 'this.arr = arr'
					value_of: 'this.arr'
					to_string: 'JSON.stringify(this.arr.map(it => it.valueOf()))'
					eq: 'vEq(this, other)'
				})
			}
			else {}
		}
	}
	g.dec_indent()
}
