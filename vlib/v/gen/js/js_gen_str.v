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
	// TODO	
}