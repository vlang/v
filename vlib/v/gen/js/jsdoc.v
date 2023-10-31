module js

import v.ast

struct JsDoc {
mut:
	gen &JsGen = unsafe { nil }
}

fn new_jsdoc(gen &JsGen) &JsDoc {
	return &JsDoc{
		gen: gen
	}
}

fn (mut d JsDoc) write(s string) {
	if !d.gen.enable_doc {
		return
	}
	d.gen.write(s)
}

fn (mut d JsDoc) writeln(s string) {
	if !d.gen.enable_doc {
		return
	}
	d.gen.writeln(s)
}

fn (mut d JsDoc) gen_typ(typ string) {
	d.writeln('/** @type {${typ}} */')
}

fn (mut d JsDoc) gen_const(typ string) {
	d.writeln('/** @constant {${typ}} */')
}

fn (mut d JsDoc) gen_enum() {
	// Enum values can only be ints for now
	typ := 'number'
	d.writeln('/** @enum {${typ}} */')
}

fn (mut d JsDoc) gen_fac_fn(fields []ast.StructField) {
	d.writeln('/**')
	d.writeln(' * @constructor')
	d.write(' * @param {{')
	for i, field in fields {
		// Marked as optional: structs have default default values,
		// so all struct members don't have to be initialized.
		d.write('${field.name}?: ${d.gen.typ(field.typ)}')
		if i < fields.len - 1 {
			d.write(', ')
		}
	}
	d.writeln('}} init')
	d.writeln('*/')
}

fn (mut d JsDoc) gen_fn(it ast.FnDecl) {
	type_name := d.gen.typ(it.return_type)
	d.writeln('/**')
	d.writeln(' * @function')
	if it.is_deprecated {
		d.writeln(' * @deprecated')
	}
	for i, arg in it.params {
		if (it.is_method || it.receiver.typ == 0) && i == 0 {
			continue
		}
		arg_type_name := d.gen.typ(arg.typ)
		is_varg := i == it.params.len - 1 && it.is_variadic
		name := d.gen.js_name(arg.name)
		if is_varg {
			d.writeln(' * @param {...${arg_type_name}} ${name}')
		} else {
			d.writeln(' * @param {${arg_type_name}} ${name}')
		}
	}
	d.writeln(' * @returns {${type_name}}')
	d.writeln('*/')
}

fn (mut d JsDoc) gen_interface(it ast.InterfaceDecl) {
	name := d.gen.js_name(it.name)
	d.writeln('/**')
	d.writeln(' * @interface ${name}')
	d.writeln(' * @typedef ${name}')
	for method in it.methods {
		// Skip receiver
		typ := d.gen.fn_typ(method.params[1..], method.return_type)
		method_name := d.gen.js_name(method.name)
		d.writeln(' * @property {${typ}} ${method_name}')
	}
	d.writeln(' */\n')
}
