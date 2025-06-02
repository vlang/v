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
		g.writeln('\t${cprefix}add_string(_S("${str}"), ${idx});')
	}
}

// gen_empty_array generates code for empty array
@[inline]
fn (g &Gen) gen_empty_array(type_name string) string {
	return '__new_array_with_default(0, 0, sizeof(${type_name}), 0)'
}

// gen_functionarg_array generates the code for functionarg argument
@[inline]
fn (g &Gen) gen_functionarg_array(type_name string, node ast.Fn) string {
	if node.params.len == 0 {
		return g.gen_empty_array(type_name)
	}
	mut out := 'new_array_from_c_array(${node.params.len},${node.params.len},sizeof(${type_name}),'
	out += '_MOV((${type_name}[${node.params.len}]){'
	out += node.params.map('((${type_name}){.name=_S("${it.name}"),.typ=${int(it.typ)},.is_mut=${it.is_mut}})').join(',')
	out += '}))'
	return out
}

// gen_functionarg_array generates the code for functionarg argument
@[inline]
fn (mut g Gen) gen_function_array(nodes []ast.Fn) string {
	type_name := '${cprefix}Function'

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
	mut arg_str := '((${cprefix}Function){'
	v_name := node.name.all_after_last('.')
	arg_str += '.mod_name=_S("${node.mod}"),'
	arg_str += '.name=_S("${v_name}"),'
	arg_str += '.args=${g.gen_functionarg_array(cprefix + 'FunctionArg', node)},'
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
	kind_name := tsym.kind.str()
	name := tsym.name.all_after_last('.')
	info := g.gen_reflection_sym_info(tsym)
	methods := g.gen_function_array(tsym.methods)
	return '(${cprefix}TypeSymbol){.name=_S("${name}"),.mod=_S("${tsym.mod}"),.idx=${tsym.idx},.parent_idx=${tsym.parent_idx},.language=${cprefix}VLanguage__${tsym.language},.kind=${cprefix}VKind__${kind_name},.info=${info},.methods=${methods}}'
}

// gen_attrs_array generates C code for []Attr
@[inline]
fn (g &Gen) gen_attrs_array(attrs []ast.Attr) string {
	if attrs.len == 0 {
		return g.gen_empty_array('string')
	}
	mut out := 'new_array_from_c_array(${attrs.len},${attrs.len},sizeof(string),'
	out += '_MOV((string[${attrs.len}]){'
	out += attrs.map(if it.has_arg {
		'_S("${it.name}=${escape_quotes(it.arg)}")'
	} else {
		'_S("${it.name}")'
	}).join(',')
	out += '}))'
	return out
}

// gen_fields_array generates C code for []StructField
@[inline]
fn (g &Gen) gen_fields_array(fields []ast.StructField) string {
	if fields.len == 0 {
		return g.gen_empty_array('${cprefix}StructField')
	}
	mut out := 'new_array_from_c_array(${fields.len},${fields.len},sizeof(${cprefix}StructField),'
	out += '_MOV((${cprefix}StructField[${fields.len}]){'
	out += fields.map('((${cprefix}StructField){.name=_S("${it.name}"),.typ=${int(it.typ)},.attrs=${g.gen_attrs_array(it.attrs)},.is_pub=${it.is_pub},.is_mut=${it.is_mut}})').join(',')
	out += '}))'
	return out
}

// gen_type_array generates C code for []Type
@[inline]
fn (g &Gen) gen_type_array(types []ast.Type) string {
	if types.len == 0 {
		return g.gen_empty_array(ast.int_type_name)
	}
	return 'new_array_from_c_array(${types.len},${types.len},sizeof(${ast.int_type_name}),_MOV((int[${types.len}]){${types.map(int(it).str()).join(',')}}))'
}

// gen_string_array generates C code for []string
@[inline]
fn (g &Gen) gen_string_array(strs []string) string {
	if strs.len == 0 {
		return g.gen_empty_array('string')
	}
	items := strs.map('_S("${it}")').join(',')
	return 'new_array_from_c_array(${strs.len},${strs.len},sizeof(string),_MOV((string[${strs.len}]){${items}}))'
}

// gen_reflection_sym_info generates C code for TypeSymbol's info sum type
@[inline]
fn (mut g Gen) gen_reflection_sym_info(tsym ast.TypeSymbol) string {
	match tsym.kind {
		.array {
			info := tsym.info as ast.Array
			s := 'ADDR(${cprefix}Array,(((${cprefix}Array){.nr_dims=${info.nr_dims},.elem_type=${int(info.elem_type)}})))'
			return '(${cprefix}TypeInfo){._${cprefix}Array = memdup(${s},sizeof(${cprefix}Array)),._typ=${g.table.find_type_idx('v.reflection.Array')}}'
		}
		.array_fixed {
			info := tsym.info as ast.ArrayFixed
			s := 'ADDR(${cprefix}ArrayFixed,(((${cprefix}ArrayFixed){.size=${info.size},.elem_type=${int(info.elem_type)}})))'
			return '(${cprefix}TypeInfo){._${cprefix}ArrayFixed=memdup(${s},sizeof(${cprefix}ArrayFixed)),._typ=${g.table.find_type_idx('v.reflection.ArrayFixed')}}'
		}
		.map {
			info := tsym.info as ast.Map
			s := 'ADDR(${cprefix}Map,(((${cprefix}Map){.key_type=${int(info.key_type)},.value_type=${int(info.value_type)}})))'
			return '(${cprefix}TypeInfo){._${cprefix}Map=memdup(${s},sizeof(${cprefix}Map)),._typ=${g.table.find_type_idx('v.reflection.Map')}}'
		}
		.sum_type {
			info := tsym.info as ast.SumType
			s := 'ADDR(${cprefix}SumType,(((${cprefix}SumType){.parent_idx=${info.parent_type.idx()},.variants=${g.gen_type_array(info.variants)}})))'
			return '(${cprefix}TypeInfo){._${cprefix}SumType=memdup(${s},sizeof(${cprefix}SumType)),._typ=${g.table.find_type_idx('v.reflection.SumType')}}'
		}
		.struct {
			info := tsym.info as ast.Struct
			attrs := g.gen_attrs_array(info.attrs)
			fields := g.gen_fields_array(info.fields)
			s := 'ADDR(${cprefix}Struct,(((${cprefix}Struct){.parent_idx=${(tsym.info as ast.Struct).parent_type.idx()},.attrs=${attrs},.fields=${fields}})))'
			return '(${cprefix}TypeInfo){._${cprefix}Struct=memdup(${s},sizeof(${cprefix}Struct)),._typ=${g.table.find_type_idx('v.reflection.Struct')}}'
		}
		.enum {
			info := tsym.info as ast.Enum
			vals := g.gen_string_array(info.vals)
			s := 'ADDR(${cprefix}Enum,(((${cprefix}Enum){.vals=${vals},.is_flag=${info.is_flag}})))'
			return '(${cprefix}TypeInfo){._${cprefix}Enum=memdup(${s},sizeof(${cprefix}Enum)),._typ=${g.table.find_type_idx('v.reflection.Enum')}}'
		}
		.function {
			info := tsym.info as ast.FnType
			s := 'ADDR(${cprefix}Function,${g.gen_reflection_fn(info.func)})'
			return '(${cprefix}TypeInfo){._${cprefix}Function=memdup(${s},sizeof(${cprefix}Function)),._typ=${g.table.find_type_idx('v.reflection.Function')}}'
		}
		.interface {
			name := tsym.name.all_after_last('.')
			info := tsym.info as ast.Interface
			methods := g.gen_function_array(info.methods)
			fields := g.gen_fields_array(info.fields)
			s := 'ADDR(${cprefix}Interface,(((${cprefix}Interface){.name=_S("${name}"),.methods=${methods},.fields=${fields},.is_generic=${info.is_generic}})))'
			return '(${cprefix}TypeInfo){._${cprefix}Interface=memdup(${s},sizeof(${cprefix}Interface)),._typ=${g.table.find_type_idx('v.reflection.Interface')}}'
		}
		.alias {
			info := tsym.info as ast.Alias
			s := 'ADDR(${cprefix}Alias,(((${cprefix}Alias){.parent_idx=${info.parent_type.idx()},.language=${cprefix}VLanguage__${info.language.str()}})))'
			return '(${cprefix}TypeInfo){._${cprefix}Alias=memdup(${s},sizeof(${cprefix}Alias)),._typ=${g.table.find_type_idx('v.reflection.Alias')}}'
		}
		.multi_return {
			info := tsym.info as ast.MultiReturn
			s := 'ADDR(${cprefix}MultiReturn,(((${cprefix}MultiReturn){.types=${g.gen_type_array(info.types)}})))'
			return '(${cprefix}TypeInfo){._${cprefix}MultiReturn=memdup(${s},sizeof(${cprefix}MultiReturn)),._typ=${g.table.find_type_idx('v.reflection.MultiReturn')}}'
		}
		else {
			s := 'ADDR(${cprefix}None,(((${cprefix}None){.parent_idx=${tsym.parent_idx},})))'
			return '(${cprefix}TypeInfo){._${cprefix}None=memdup(${s},sizeof(${cprefix}None)),._typ=${g.table.find_type_idx('v.reflection.None')}}'
		}
	}
}

// gen_reflection_data generates code to initialized V reflection metadata
fn (mut g Gen) gen_reflection_data() {
	// modules declaration
	for mod_name in g.table.modules {
		g.writeln('\t${cprefix}add_module(_S("${mod_name}"));')
	}

	// type symbols declaration
	for _, tsym in g.table.type_symbols {
		sym := g.gen_reflection_sym(tsym)
		g.writeln('\t${cprefix}add_type_symbol(${sym});')
	}

	// types declaration
	for full_name, idx in g.table.type_idxs {
		name := full_name.all_after_last('.')
		g.writeln('\t${cprefix}add_type((${cprefix}Type){.name=_S("${name}"),.idx=${idx}});')
	}

	// func declaration (methods come from struct methods)
	for _, fn_ in g.table.fns {
		if fn_.no_body || fn_.is_method || fn_.language != .v {
			continue
		}
		func := g.gen_reflection_fn(fn_)
		g.writeln('\t${cprefix}add_func(${func});')
	}

	g.gen_reflection_strings()
}
