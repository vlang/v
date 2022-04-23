module js

import v.ast

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

fn (mut g JsGen) copy_val(t ast.Type, tmp string) string {
	fun := g.get_copy_fn(t)
	temp := g.new_tmp_var()
	g.writeln('let $temp = ${fun}($tmp);')
	return temp
}

fn (mut g JsGen) to_js_typ_val(t ast.Type) string {
	sym := g.table.sym(t)
	mut styp := ''
	mut prefix := 'new '
	match sym.kind {
		.i8, .i16, .int, .i64, .u8, .u16, .u32, .u64, .f32, .f64, .int_literal, .float_literal {
			styp = '$prefix${g.sym_to_js_typ(sym)}(0)'
		}
		.bool {
			styp = '$prefix${g.sym_to_js_typ(sym)}(false)'
		}
		.string {
			styp = '$prefix${g.sym_to_js_typ(sym)}("")'
		}
		.map {
			styp = 'new map(new Map())'
		}
		.array {
			styp = 'empty_array()'
		}
		.struct_ {
			styp = 'new ${g.js_name(sym.name)}(${g.to_js_typ_def_val(sym.name)})'
		}
		.voidptr {
			styp = 'new voidptr(null)'
		}
		else {
			// TODO
			styp = 'undefined'
		}
	}
	return styp
}

fn (mut g JsGen) sym_to_js_typ(sym ast.TypeSymbol) string {
	mut styp := ''
	match sym.kind {
		.i8 {
			styp = 'i8'
		}
		.i16 {
			styp = 'i16'
		}
		.int {
			styp = 'int'
		}
		.i64 {
			styp = 'i64'
		}
		.u8 {
			styp = 'u8'
		}
		.u16 {
			styp = 'u16'
		}
		.u32 {
			styp = 'u32'
		}
		.u64 {
			styp = 'u64'
		}
		.f32 {
			styp = 'f32'
		}
		.f64 {
			styp = 'f64'
		}
		.int_literal {
			styp = 'int_literal'
		}
		.float_literal {
			styp = 'float_literal'
		}
		.bool {
			styp = 'bool'
		}
		.string {
			styp = 'string'
		}
		.map {
			styp = 'map'
		}
		.array {
			styp = 'array'
		}
		.voidptr {
			styp = 'voidptr'
		}
		.rune {
			styp = 'rune'
		}
		else {
			// TODO
			styp = 'undefined'
		}
	}
	return styp
}

/*
pub fn (mut g JsGen) base_type(t ast.Type) string {
	mut styp := g.cc_type(t, true)
	return styp
}
*/
fn (mut g JsGen) base_type(_t ast.Type) string {
	t := g.unwrap_generic(_t)
	share := t.share()
	mut styp := if share == .atomic_t { t.atomic_typename() } else { g.cc_type(t, true) }
	return styp
}

pub fn (mut g JsGen) typ(t ast.Type) string {
	sym := g.table.final_sym(t)
	if sym.kind == .voidptr {
		return 'voidptr'
	}

	styp := g.base_type(t)
	return styp
}

// V type to JS type
pub fn (mut g JsGen) doc_typ(t ast.Type) string {
	sym := g.table.sym(t)
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
			styp = 'voidptr'
		}
		.byteptr, .charptr {
			styp = '${g.sym_to_js_typ(sym)}'
		}
		.i8, .i16, .int, .i64, .isize, .u8, .u16, .u32, .u64, .usize, .f32, .f64, .int_literal,
		.float_literal {
			styp = '${g.sym_to_js_typ(sym)}'
		}
		.bool {
			styp = '${g.sym_to_js_typ(sym)}'
		}
		.none_ {
			styp = 'undefined'
		}
		.string, .char {
			styp = '${g.sym_to_js_typ(sym)}'
		}
		// 'array_array_int' => 'number[][]'
		.array {
			info := sym.info as ast.Array
			styp = '${g.sym_to_js_typ(sym)}(${g.typ(info.elem_type)})'
		}
		.array_fixed {
			info := sym.info as ast.ArrayFixed
			styp = '${g.sym_to_js_typ(sym)}(${g.typ(info.elem_type)})'
		}
		.chan {
			styp = 'chan'
		}
		// 'map[string]int' => 'Map<string, number>'
		.map {
			info := sym.info as ast.Map
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
		.generic_inst {}
		// 'multi_return_int_int' => '[number, number]'
		.multi_return {
			info := sym.info as ast.MultiReturn
			types := info.types.map(g.typ(it))
			joined := types.join(', ')
			styp = '[$joined]'
		}
		.sum_type {
			// TODO: Implement sumtypes
			styp = 'union_sym_type'
		}
		.alias {
			fsym := g.table.final_sym(t)
			name := g.js_name(fsym.name)
			styp += '$name'
		}
		.enum_ {
			// Note: We could declare them as TypeScript enums but TS doesn't like
			// our namespacing so these break if declared in a different module.
			// Until this is fixed, We need to use the type of an enum's members
			// rather than the enum itself, and this can only be 'number' for now
			styp = 'number'
		}
		// 'anon_fn_7_7_1' => '(a number, b number) => void'
		.function {
			info := sym.info as ast.FnType
			styp = g.fn_typ(info.func.params, info.func.return_type)
		}
		.interface_ {
			styp = g.js_name(sym.name)
		}
		.rune {
			styp = 'rune'
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

fn (mut g JsGen) fn_typ(args []ast.Param, return_type ast.Type) string {
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
	if ns == 'JS' {
		return s[3..]
	}
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

struct BuiltinPrototypeConfig {
	typ_name      string
	val_name      string = 'val'
	default_value string
	constructor   string = 'this.val = val'
	value_of      string = 'this.val'
	to_string     string = 'this.val.toString()'
	eq            string = 'self.val === other.val'
	to_jsval      string = 'this'
	extras        string
	has_strfn     bool
}

fn (mut g JsGen) gen_builtin_prototype(c BuiltinPrototypeConfig) {
	g.writeln('function ${c.typ_name}($c.val_name) { if ($c.val_name === undefined) { $c.val_name = $c.default_value; }$c.constructor }')
	g.writeln('${c.typ_name}.prototype = {')
	g.inc_indent()
	g.writeln('$c.val_name: $c.default_value,')
	if c.extras.len > 0 {
		g.writeln('$c.extras,')
	}

	if g.pref.output_es5 {
		g.writeln('valueOf: (function() { return $c.value_of }).bind(this),')
		g.writeln('toString: (function() { return $c.to_string }).bind(this),')
		g.writeln('\$toJS: (function() { return $c.to_jsval }).bind(this), ')
		if c.has_strfn {
			g.writeln('str: (function() { return new string(this.toString())).bind(this) }')
		}
		// g.writeln('eq: (function(other) { return $c.eq }).bind(this),')
	} else {
		g.writeln('valueOf() { return $c.value_of   },')
		g.writeln('toString() { return $c.to_string },')
		g.writeln('\$toJS() { return $c.to_jsval }, ')
		if c.has_strfn {
			g.writeln('str() { return new string(this.toString()) }')
		}
		// g.writeln('eq(other) { return $c.eq },')
	}
	g.dec_indent()
	g.writeln('};\n')
	g.writeln('function ${c.typ_name}__eq(self,other) { return $c.eq; } ')
}

// generate builtin type definitions, used for casting and methods.
fn (mut g JsGen) gen_builtin_type_defs() {
	g.inc_indent()
	for typ_name in v_types {
		// TODO: JsDoc
		match typ_name {
			'i8', 'i16', 'int', 'u16', 'u32', 'int_literal' {
				// TODO: Bounds checking
				g.gen_builtin_prototype(
					typ_name: typ_name
					default_value: 'new Number(0)'
					// mask <=32 bit numbers with 0xffffffff
					constructor: 'this.val = Math.floor(Number(val) & 0xffffffff) '
					value_of: 'Number(this.val)'
					to_string: 'this.valueOf().toString()'
					eq: 'new bool(self.valueOf() === other.valueOf())'
					to_jsval: '+this'
				)
			}
			// u64 and i64 are so big that their values do not fit into JS number so we use BigInt.
			'u64' {
				if g.pref.output_es5 {
					g.gen_builtin_prototype(
						typ_name: typ_name
						default_value: '0'
						constructor: 'this.val =val.floor() >> 0'
						value_of: 'this.val'
						to_string: 'this.val.toString()'
						eq: 'new bool(self.valueOf() === other.valueOf())'
						to_jsval: 'this.val'
					)
				} else {
					g.gen_builtin_prototype(
						typ_name: typ_name
						default_value: 'BigInt(0)'
						constructor: 'this.val = BigInt.asUintN(64,BigInt(val))'
						value_of: 'this.val'
						to_string: 'this.val.toString()'
						eq: 'new bool(self.valueOf() === other.valueOf())'
						to_jsval: 'this.val'
					)
				}
			}
			'i64' {
				if g.pref.output_es5 {
					g.gen_builtin_prototype(
						typ_name: typ_name
						default_value: '0'
						constructor: 'this.val =val.floor() >> 0'
						value_of: 'this.val'
						to_string: 'this.val.toString()'
						eq: 'new bool(self.valueOf() === other.valueOf())'
						to_jsval: 'this.val'
					)
				} else {
					g.gen_builtin_prototype(
						typ_name: typ_name
						default_value: 'BigInt(0)'
						constructor: 'this.val = BigInt.asIntN(64,BigInt(val))'
						value_of: 'this.val'
						to_string: 'this.val.toString()'
						eq: 'new bool(self.valueOf() === other.valueOf())'
						to_jsval: 'this.val'
					)
				}
			}
			'u8' {
				g.gen_builtin_prototype(
					typ_name: typ_name
					default_value: 'new Number(0)'
					constructor: 'if (typeof(val) == "string") { this.val = val.charCodeAt() } else if (val instanceof string) { this.val = val.str.charCodeAt(); } else { this.val =  Math.round(Number(val)) }'
					value_of: 'this.val | 0'
					to_string: 'new string(this.val + "")'
					eq: 'new bool(self.valueOf() === other.valueOf())'
					to_jsval: '+this'
				)
			}
			'rune' {
				g.gen_builtin_prototype(
					typ_name: typ_name
					default_value: 'new Number(0)'
					constructor: 'val = val.valueOf(); if (typeof val == "string") {this.val = val.charCodeAt();}  else if (val instanceof string) { this.val = val.str.charCodeAt(); } else { this.val =  val | 0 }'
					value_of: 'this.val | 0'
					to_string: 'new string(this.val + "")'
					eq: 'new bool(self.valueOf() === other.valueOf())'
					to_jsval: '+this'
				)
			}
			'f32', 'f64', 'float_literal' {
				g.gen_builtin_prototype(
					typ_name: typ_name
					constructor: 'this.val = Number(val)'
					default_value: 'new Number(0)'
					to_jsval: '+this'
				)
			}
			'bool' {
				g.gen_builtin_prototype(
					constructor: 'this.val = val instanceof bool ? val.val : +val !== 0'
					typ_name: typ_name
					default_value: 'new Boolean(false)'
					to_jsval: '+this != 0'
					eq: 'new bool(self.val === other.valueOf())'
				)
			}
			'string' {
				g.gen_builtin_prototype(
					typ_name: typ_name
					val_name: 'str'
					default_value: 'new String("")'
					constructor: 'this.str = str.toString(); this.len = this.str.length'
					value_of: 'this.str'
					to_string: 'this.str'
					eq: 'new bool(self.str === other.str)'
					has_strfn: false
					to_jsval: 'this.str'
				)
			}
			'map' {
				g.gen_builtin_prototype(
					typ_name: typ_name
					val_name: 'map'
					default_value: 'new map({})'
					constructor: 'this.map = map; this.length = 0;'
					value_of: 'this'
					to_string: 'this.map.toString()'
					eq: 'new bool(vEq(self, other))'
					to_jsval: 'this.map'
				)
			}
			'array' {
				g.gen_builtin_prototype(
					typ_name: typ_name
					val_name: 'arr'
					default_value: 'new array_buffer({})'
					constructor: 'this.arr = arr\nif (arr.index_start.val != 0 || arr.has_slice.val) { v_makeSlice(this); } '
					value_of: 'this'
					to_string: 'JSON.stringify(this.arr.map(it => it.valueOf()))'
					eq: 'new bool(vEq(self, other))'
					to_jsval: 'this.arr'
				)
			}
			'voidptr' {
				g.gen_builtin_prototype(
					typ_name: typ_name
					val_name: 'val'
					default_value: 'null'
					constructor: 'this.val = val;'
					value_of: 'this'
					to_string: '"voidptr(" + this.val + ")"'
					eq: 'this.val === other.val'
					to_jsval: 'this.val'
				)
			}
			'any' {
				g.gen_builtin_prototype(
					typ_name: typ_name
					val_name: 'any'
					default_value: 'null'
					constructor: 'this.val = any'
					value_of: 'this.val'
					to_string: '"&" + this.val'
					eq: 'new bool(self == other)' // compare by ptr
					to_jsval: 'this.val.\$toJS()'
				)
			}
			else {}
		}
	}
	g.dec_indent()
}
