module js

import (
	v.table
)

fn (g mut JsGen) gen_str_for_enum(info table.Enum, styp string) {
	s := styp.replace('.', '__')
	g.definitions.write('string ${s}_str($styp it) {\n\tswitch(it) {\n')
	for i, val in info.vals {
		g.definitions.write('\t\tcase ${s}_$val: return tos3("$val");\n')
	}
	g.definitions.write('\t\tdefault: return tos3("unknown enum value"); } }\n')
}

fn (g mut JsGen) gen_str_for_type(sym table.TypeSymbol, styp string) {
	if sym.has_method('str') || styp in g.str_types {
		return
	}
	g.str_types << styp
	match sym.info {
		table.Struct {
			g.gen_str_for_struct(it, styp)
		}
		table.Enum {
			g.gen_str_for_enum(it, styp)
		}
		else {
			println('cannot generate str() for $sym.name')
		}
	}
}

fn (g mut JsGen) gen_str_for_struct(info table.Struct, styp string) {
	// TODO: short it if possible
	// generates all definitions of substructs
	for i, field in info.fields {
		sym := g.table.get_type_symbol(field.typ)
		if sym.kind == .struct_ {
			field_styp := g.typ(field.typ)
			g.gen_str_for_type(sym, field_styp)
		}
	}
	s := styp.replace('.', '__')
	g.definitions.write('string ${s}_str($styp it, int indent_count) {\n')
	// generate ident / indent length = 4 spaces
	g.definitions.write('\tstring indents = tos3("");\n\tfor (int i = 0; i < indent_count; i++) { indents = string_add(indents, tos3("    ")); }\n')
	g.definitions.write('\treturn _STR("$styp {\\n')
	for field in info.fields {
		fmt := g.type_to_fmt(field.typ)
		g.definitions.write('%.*s    ' + '$field.name: $fmt\\n')
	}
	g.definitions.write('%.*s}"')
	if info.fields.len > 0 {
		g.definitions.write(', ')
		for i, field in info.fields {
			sym := g.table.get_type_symbol(field.typ)
			if sym.kind == .struct_ {
				field_styp := g.typ(field.typ)
				g.definitions.write('indents.len, indents.str, ${field_styp}_str(it.$field.name, indent_count + 1).len, ${field_styp}_str(it.$field.name, indent_count + 1).str')
			} else {
				g.definitions.write('indents.len, indents.str, it.$field.name')
				if field.typ == table.string_type {
					g.definitions.write('.len, it.${field.name}.str')
				} else if field.typ == table.bool_type {
					g.definitions.write(' ? 4 : 5, it.${field.name} ? "true" : "false"')
				}
				if i < info.fields.len - 1 {
					g.definitions.write(', ')
				}
			}
		}
	}
	g.definitions.writeln(', indents.len, indents.str);\n}')
}