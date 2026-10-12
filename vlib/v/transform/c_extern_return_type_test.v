module transform

import v.parser
import v.pref
import v.types

fn test_c_extern_return_types_preserve_v_signatures() {
	for extension in ['v', 'vh'] {
		for c_first in [false, true] {
			mut p := parser.Parser.new(pref.new_preferences())
			c_source := 'module builtin\nfn C.malloc(usize) voidptr\nfn C.external_only() int\n'
			v_source := if extension == 'vh' {
				'module builtin\npub fn malloc(n isize) &u8\n'
			} else {
				'module builtin\npub fn malloc(n isize) &u8 { return unsafe { &u8(C.malloc(usize(n))) } }\n'
			}
			if c_first {
				p.parse_text('extern.c.v', c_source)
				p.parse_text('allocation.${extension}', v_source)
			} else {
				p.parse_text('allocation.${extension}', v_source)
				p.parse_text('extern.c.v', c_source)
			}
			mut tc := types.TypeChecker.new(p.a)
			tc.collect(p.a)
			mut a := p.a
			mut t := new_transformer(mut a, &tc, map[string]bool{})
			t.collect_types()
			assert t.fn_ret_types['malloc'] == '&u8', '${extension}, C first: ${c_first}'
			assert t.fn_ret_types['C.malloc'] == 'voidptr'
			assert t.fn_ret_types['external_only'] == 'int'
			assert t.fn_ret_types['C.external_only'] == 'int'
		}
	}
}
