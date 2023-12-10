module c

import v.ast
import v.util

const cprefix = 'v__reflection__'

// reflection_string maps string to its idx
fn (mut g Gen) reflection_string(str string) int {
	return unsafe {
		g.reflection_strings[str] or {
			g.reflection_strings[str] = g.reflection_strings.len
			g.reflection_strings.len - 1
		}
	}
}

// gen_reflection_strings generates the reflectino string registration
@[inline]
fn (mut g Gen) gen_reflection_strings() {
	for str, idx in g.reflection_strings {
		g.writeln('\t${c.cprefix}add_string(_SLIT("${str}"), ${idx});')
	}
}

// gen_empty_array generates code for empty array
@[inline]
fn (g Gen) gen_empty_array(type_name string) string {
	return '__new_array_with_default(0, 0, sizeof(${type_name}), 0)'
}

// gen_functionarg_array generates the code for functionarg argument
@[inline]
fn (g Gen) gen_functionarg_array(type_name string, node ast.Fn) string {
	if node.params.len == 0 {
		return g.gen_empty_array(type_name)
	}
	mut out := 'new_array_from_c_array(${node.params.len},${node.params.len},sizeof(${type_name}),'
	out += '_MOV((${type_name}[${node.params.len}]){'
	out += node.params.map('((${type_name}){.name=_SLIT("${it.name}"),.typ=${int(it.typ)},.is_mut=${it.is_mut}})').join(',')
	out += '}))'
	return out
}

// gen_functionarg_array generates the code for functionarg argument
@[inline]
fn (mut g Gen) gen_function_array(nodes []ast.Fn) string {
	type_name := '${c.cprefix}Function'

	if nodes.len == 0 {
		return g.gen_empty_array(type_name)
	}

	mut out := 'new_array_from_c_array(${nodes.len},${nodes.len},sizeof(${type_name}),'
	out += '_MOV((${type_name}[${nodes.len}]){'
	out += nodes.map(g.gen_reflection_fn(it)).join(',')
	out += '}))'
	return out
}

// gen_reflection_fn generates C code for Function struct
@[inline]
fn (mut g Gen) gen_reflection_fn(node ast.Fn) string {
	mut arg_str := '((${c.cprefix}Function){'
	v_name := node.name.all_after_last('.')
	arg_str += '.mod_name=_SLIT("${node.mod}"),'
	arg_str += '.name=_SLIT("${v_name}"),'
	arg_str += '.args=${g.gen_functionarg_array(c.cprefix + 'FunctionArg', node)},'
	arg_str += '.file_idx=${g.reflection_string(util.cescaped_path(node.file))},'
	arg_str += '.line_start=${node.pos.line_nr},'
	arg_str += '.line_end=${node.pos.last_line},'
	arg_str += '.is_variadic=${node.is_variadic},'
	arg_str += '.return_typ=${int(node.return_type)},'
	arg_str += '.receiver_typ=${int(node.receiver_type)},'
	arg_str += '.is_pub=${node.is_pub}'
	arg_str += '})'
	return arg_str
}

// gen_reflection_sym generates C code for TypeSymbol struct
@[inline]
fn (mut g Gen) gen_reflection_sym(tsym ast.TypeSymbol) string {
	kind_name := if tsym.kind in [.none_, .struct_, .enum_, .interface_] {
		tsym.kind.str() + '_'
	} else {
		tsym.kind.str()
	}
	info := g.gen_reflection_sym_info(tsym)
	methods := g.gen_function_array(tsym.methods)
	return '(${c.cprefix}TypeSymbol){.name=_SLIT("${tsym.name}"),.idx=${tsym.idx},.parent_idx=${tsym.parent_idx},.language=${c.cprefix}VLanguage__${tsym.language},.kind=${c.cprefix}VKind__${kind_name},.info=${info},.methods=${methods}}'
}

// gen_attrs_array generates C code for []Attr
@[inline]
fn (g Gen) gen_attrs_array(attrs []ast.Attr) string {
	if attrs.len == 0 {
		return g.gen_empty_array('string')
	}
	mut out := 'new_array_from_c_array(${attrs.len},${attrs.len},sizeof(string),'
	out += '_MOV((string[${attrs.len}]){'
	out += attrs.map(if it.has_arg { '_SLIT("${it.name}=${it.arg}")' } else { '_SLIT("${it.name}")' }).join(',')
	out += '}))'
	return out
}

// gen_fields_array generates C code for []StructField
@[inline]
fn (g Gen) gen_fields_array(fields []ast.StructField) string {
	if fields.len == 0 {
		return g.gen_empty_array('${c.cprefix}StructField')
	}
	mut out := 'new_array_from_c_array(${fields.len},${fields.len},sizeof(${c.cprefix}StructField),'
	out += '_MOV((${c.cprefix}StructField[${fields.len}]){'
	out += fields.map('((${c.cprefix}StructField){.name=_SLIT("${it.name}"),.typ=${int(it.typ)},.attrs=${g.gen_attrs_array(it.attrs)},.is_pub=${it.is_pub},.is_mut=${it.is_mut}})').join(',')
	out += '}))'
	return out
}

// gen_type_array generates C code for []Type
@[inline]
fn (g Gen) gen_type_array(types []ast.Type) string {
	if types.len == 0 {
		return g.gen_empty_array(ast.int_type_name)
	}
	return 'new_array_from_c_array(${types.len},${types.len},sizeof(${ast.int_type_name}),_MOV((int[${types.len}]){${types.map(int(it).str()).join(',')}}))'
}

// gen_string_array generates C code for []string
@[inline]
fn (g Gen) gen_string_array(strs []string) string {
	if strs.len == 0 {
		return g.gen_empty_array('string')
	}
	items := strs.map('_SLIT("${it}")').join(',')
	return 'new_array_from_c_array(${strs.len},${strs.len},sizeof(string),_MOV((string[${strs.len}]){${items}}))'
}

// gen_reflection_sym_info generates C code for TypeSymbol's info sum type
@[inline]
fn (mut g Gen) gen_reflection_sym_info(tsym ast.TypeSymbol) string {
	match tsym.kind {
		.array {
			info := tsym.info as ast.Array
			s := 'ADDR(${c.cprefix}Array,(((${c.cprefix}Array){.nr_dims=${info.nr_dims},.elem_type=${int(info.elem_type)}})))'
			return '(${c.cprefix}TypeInfo){._${c.cprefix}Array = memdup(${s},sizeof(${c.cprefix}Array)),._typ=${g.table.find_type_idx('v.reflection.Array')}}'
		}
		.array_fixed {
			info := tsym.info as ast.ArrayFixed
			s := 'ADDR(${c.cprefix}ArrayFixed,(((${c.cprefix}ArrayFixed){.size=${info.size},.elem_type=${int(info.elem_type)}})))'
			return '(${c.cprefix}TypeInfo){._${c.cprefix}ArrayFixed=memdup(${s},sizeof(${c.cprefix}ArrayFixed)),._typ=${g.table.find_type_idx('v.reflection.ArrayFixed')}}'
		}
		.map {
			info := tsym.info as ast.Map
			s := 'ADDR(${c.cprefix}Map,(((${c.cprefix}Map){.key_type=${int(info.key_type)},.value_type=${int(info.value_type)}})))'
			return '(${c.cprefix}TypeInfo){._${c.cprefix}Map=memdup(${s},sizeof(${c.cprefix}Map)),._typ=${g.table.find_type_idx('v.reflection.Map')}}'
		}
		.sum_type {
			info := tsym.info as ast.SumType
			s := 'ADDR(${c.cprefix}SumType,(((${c.cprefix}SumType){.parent_idx=${info.parent_type.idx()},.variants=${g.gen_type_array(info.variants)}})))'
			return '(${c.cprefix}TypeInfo){._${c.cprefix}SumType=memdup(${s},sizeof(${c.cprefix}SumType)),._typ=${g.table.find_type_idx('v.reflection.SumType')}}'
		}
		.struct_ {
			info := tsym.info as ast.Struct
			attrs := g.gen_attrs_array(info.attrs)
			fields := g.gen_fields_array(info.fields)
			s := 'ADDR(${c.cprefix}Struct,(((${c.cprefix}Struct){.parent_idx=${(tsym.info as ast.Struct).parent_type.idx()},.attrs=${attrs},.fields=${fields}})))'
			return '(${c.cprefix}TypeInfo){._${c.cprefix}Struct=memdup(${s},sizeof(${c.cprefix}Struct)),._typ=${g.table.find_type_idx('v.reflection.Struct')}}'
		}
		.enum_ {
			info := tsym.info as ast.Enum
			vals := g.gen_string_array(info.vals)
			s := 'ADDR(${c.cprefix}Enum,(((${c.cprefix}Enum){.vals=${vals},.is_flag=${info.is_flag}})))'
			return '(${c.cprefix}TypeInfo){._${c.cprefix}Enum=memdup(${s},sizeof(${c.cprefix}Enum)),._typ=${g.table.find_type_idx('v.reflection.Enum')}}'
		}
		.function {
			info := tsym.info as ast.FnType
			s := 'ADDR(${c.cprefix}Function,${g.gen_reflection_fn(info.func)})'
			return '(${c.cprefix}TypeInfo){._${c.cprefix}Function=memdup(${s},sizeof(${c.cprefix}Function)),._typ=${g.table.find_type_idx('v.reflection.Function')}}'
		}
		.interface_ {
			name := tsym.name.all_after_last('.')
			info := tsym.info as ast.Interface
			methods := g.gen_function_array(info.methods)
			fields := g.gen_fields_array(info.fields)
			s := 'ADDR(${c.cprefix}Interface,(((${c.cprefix}Interface){.name=_SLIT("${name}"),.methods=${methods},.fields=${fields},.is_generic=${info.is_generic}})))'
			return '(${c.cprefix}TypeInfo){._${c.cprefix}Interface=memdup(${s},sizeof(${c.cprefix}Interface)),._typ=${g.table.find_type_idx('v.reflection.Interface')}}'
		}
		.alias {
			info := tsym.info as ast.Alias
			s := 'ADDR(${c.cprefix}Alias,(((${c.cprefix}Alias){.parent_idx=${info.parent_type.idx()},.language=${c.cprefix}VLanguage__${info.language.str()}})))'
			return '(${c.cprefix}TypeInfo){._${c.cprefix}Alias=memdup(${s},sizeof(${c.cprefix}Alias)),._typ=${g.table.find_type_idx('v.reflection.Alias')}}'
		}
		.multi_return {
			info := tsym.info as ast.MultiReturn
			s := 'ADDR(${c.cprefix}MultiReturn,(((${c.cprefix}MultiReturn){.types=${g.gen_type_array(info.types)}})))'
			return '(${c.cprefix}TypeInfo){._${c.cprefix}MultiReturn=memdup(${s},sizeof(${c.cprefix}MultiReturn)),._typ=${g.table.find_type_idx('v.reflection.MultiReturn')}}'
		}
		else {
			s := 'ADDR(${c.cprefix}None,(((${c.cprefix}None){.parent_idx=${tsym.parent_idx},})))'
			return '(${c.cprefix}TypeInfo){._${c.cprefix}None=memdup(${s},sizeof(${c.cprefix}None)),._typ=${g.table.find_type_idx('v.reflection.None')}}'
		}
	}
}

// gen_reflection_data generates code to initialized V reflection metadata
fn (mut g Gen) gen_reflection_data() {
	// modules declaration
	for mod_name in g.table.modules {
		g.writeln('\t${c.cprefix}add_module(_SLIT("${mod_name}"));')
	}

	// type symbols declaration
	for _, tsym in g.table.type_symbols {
		sym := g.gen_reflection_sym(tsym)
		g.writeln('\t${c.cprefix}add_type_symbol(${sym});')
	}

	// types declaration
	for full_name, idx in g.table.type_idxs {
		name := full_name.all_after_last('.')
		g.writeln('\t${c.cprefix}add_type((${c.cprefix}Type){.name=_SLIT("${name}"),.idx=${idx}});')
	}

	// func declaration (methods come from struct methods)
	for _, fn_ in g.table.fns {
		if fn_.no_body || fn_.is_method || fn_.language != .v {
			continue
		}
		func := g.gen_reflection_fn(fn_)
		g.writeln('\t${c.cprefix}add_func(${func});')
	}

	g.gen_reflection_strings()
}
