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
	if !d.gen.enable_doc { return }
	d.gen_indent()
	d.out.write(s)
}

fn (mut d JsDoc) writeln(s string) {
	if !d.gen.enable_doc { return }
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
		d.write(' - ${d.gen.js_name(name)}')
	}
	d.write(' */')
	return d.out.str()
}

fn (mut d JsDoc) gen_const(typ string) string {
	d.reset()
	d.write('/** @constant {$typ} */')
	return d.out.str()
}

fn (mut d JsDoc) gen_fac_fn(fields []ast.StructField) string {
	d.reset()
	d.writeln('/**')
	d.write(' * @param {{')
	for i, field in fields {
		// Marked as optional: structs have default default values,
		// so all struct members don't have to be initialized.
		// TODO: Actually generate default struct init values :P
		d.write('$field.name?: ${d.gen.typ(field.typ)}')
		if i < fields.len - 1 { d.write(', ') }
	}
	d.writeln('}} values - values for this class fields')
	d.writeln(' * @constructor')
	d.write('*/')
	return d.out.str()
}

fn (mut d JsDoc) gen_fn(it ast.FnDecl) string {
	d.reset()
	type_name := d.gen.typ(it.return_type)
	d.writeln('/**')
	if it.is_deprecated {
		d.writeln(' * @deprecated')
	}
	for i, arg in it.args {
		if it.is_method && i == 0 {
			continue
		}
		arg_type_name := d.gen.typ(arg.typ)
		is_varg := i == it.args.len - 1 && it.is_variadic
		name := d.gen.js_name(arg.name)
		if is_varg {
			d.writeln(' * @param {...$arg_type_name} $name')
		} else {
			d.writeln(' * @param {$arg_type_name} $name')
		}
	}
	d.writeln(' * @returns {$type_name}')
	d.writeln(' * @function')
	d.write('*/')
	return d.out.str()
}

fn (mut d JsDoc) gen_namespace(ns string) string {
	d.reset()
	d.writeln('/** @namespace ${ns} */')
	return d.out.str()
}
