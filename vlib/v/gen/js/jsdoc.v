module js

import strings

struct JsDoc {
	gen &JsGen
	mut:
	out strings.Builder
	empty_line bool
}

fn new_jsdoc(gen &JsGen) &JsDoc {
	return &JsDoc {
		out: strings.new_builder(20)
		gen: gen
	}
}

fn (d mut JsDoc) gen_indent() {
	if d.gen.indent > 0 && d.empty_line {
		d.out.write(tabs[d.gen.indent])
	}
	d.empty_line = false
}

fn (d mut JsDoc) write(s string) {
	d.gen_indent()
	d.out.write(s)
}

fn (d mut JsDoc) writeln(s string) {
	d.gen_indent()
	d.out.writeln(s)
	d.empty_line = true
}

fn (d mut JsDoc) reset() {
	d.out = strings.new_builder(20)
	d.empty_line = false
}

fn (d mut JsDoc) gen_typ(typ string, name string) string {
	d.reset()
	d.write('/**')
	d.write(' @type {$typ}')
	if name.len > 0 {
		d.write(' - $name')
	}
	d.write(' */')
	return d.out.str()
}