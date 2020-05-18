module js

import strings
import v.ast

struct JsDoc {
	gen        &JsGen
mut:
	out        strings.Builder
	empty_line bool
}

fn new_jsdoc(gen &JsGen) &JsDoc {
	return &JsDoc{
		out: strings.new_builder(20)
		gen: gen
	}
}

fn (mut d JsDoc) gen_indent() {
	if d.gen.indents[d.gen.namespace] > 0 && d.empty_line {
		d.out.write(tabs[d.gen.indents[d.gen.namespace]])
	}
	d.empty_line = false
}

fn (mut d JsDoc) write(s string) {
	d.gen_indent()
	d.out.write(s)
}

fn (mut d JsDoc) writeln(s string) {
	d.gen_indent()
	d.out.writeln(s)
	d.empty_line = true
}

fn (mut d JsDoc) reset() {
	d.out = strings.new_builder(20)
	d.empty_line = false
}

fn (mut d JsDoc) gen_typ(typ, name string) string {
	d.reset()
	d.write('/**')
	d.write(' @type {$typ}')
	if name.len > 0 {
		d.write(' - ${js_name(name)}')
	}
	d.write(' */')
	return d.out.str()
}

fn (mut d JsDoc) gen_ctor(fields []ast.StructField) string {
	d.reset()
	d.writeln('/**')
	d.write('* @param {{')
	for i, field in fields {
		d.write('$field.name: ${d.gen.type_name(field.typ)}')
		if i < fields.len - 1 {
			d.write(', ')
		}
	}
	d.writeln('}} values - values for this class fields')
	d.writeln('* @constructor')
	d.write('*/')
	return d.out.str()
}

fn (mut d JsDoc) gen_fn(it ast.FnDecl) string {
	d.reset()
	type_name := d.gen.type_name(it.return_type)
	d.writeln('/**')
	for i, arg in it.args {
		if it.is_method && i == 0 {
			continue
		}
		arg_type_name := d.gen.type_name(arg.typ)
		is_varg := i == it.args.len - 1 && it.is_variadic
		name := js_name(arg.name)
		if is_varg {
			d.writeln('* @param {...$arg_type_name} $name')
		} else {
			d.writeln('* @param {$arg_type_name} $name')
		}
	}
	d.writeln('* @return {$type_name}')
	d.write('*/')
	return d.out.str()
}
