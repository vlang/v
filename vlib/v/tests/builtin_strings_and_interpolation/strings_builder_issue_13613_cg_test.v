// vtest vflags: -cg
module main

import strings

struct Issue13613Writer {
mut:
	out strings.Builder = strings.new_builder(200)
}

fn (mut w Issue13613Writer) build() string {
	w.out.writeln('module main')
	w.out.writeln('')
	w.out.writeln('fn main() {')
	w.out.writeln('\tprint("You\'re")')
	w.out.writeln('\tmatch 27 {')
	w.out.writeln('\t\t18 {')
	w.out.writeln('\t\t\tmut age := 27')
	w.out.writeln("\t\t\tprintln(' of age')")
	w.out.writeln('\t\t}')
	w.out.writeln('\t\t10 {')
	w.out.writeln('\t\t\tmut age := 27')
	w.out.writeln("\t\t\tprintln(' a child')")
	w.out.writeln('\t\t}')
	w.out.writeln('\t\telse {')
	w.out.writeln('\t\t\tmut age := 27')
	w.out.writeln('\t\t\tprintln(".. I don\'t know my program is pretty dumb :/")')
	w.out.writeln('\t\t}')
	w.out.writeln('\t}')
	w.out.write_string('}')
	return w.out.str()
}

fn test_strings_builder_growth_with_cg_regression() {
	expected := 'module main\n\nfn main() {\n\tprint("You\'re")\n\tmatch 27 {\n\t\t18 {\n\t\t\tmut age := 27\n\t\t\tprintln(\' of age\')\n\t\t}\n\t\t10 {\n\t\t\tmut age := 27\n\t\t\tprintln(\' a child\')\n\t\t}\n\t\telse {\n\t\t\tmut age := 27\n\t\t\tprintln(".. I don\'t know my program is pretty dumb :/")\n\t\t}\n\t}\n}'
	mut w := Issue13613Writer{}
	got := w.build()
	assert got.len == expected.len
	assert got[0] == `m`
	assert got == expected
}
