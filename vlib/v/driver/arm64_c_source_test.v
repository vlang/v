module driver

import v.flat

fn test_arm64_rejects_native_source_includes() {
	for directive in ['include', 'insert', 'preinclude', 'postinclude'] {
		for ext in ['c', 'cc', 'cpp', 'cxx', 'C', 'm', 'mm', 's', 'S'] {
			mut a := flat.FlatAst.new()
			a.add_node(flat.Node{
				kind:  .directive
				value: directive
				typ:   flat.source_type_id('"@VMODROOT/native.${ext}"')
			})
			message := unsupported_arm64_c_source_error(a) or { '' }
			assert message.contains('does not support including native source'), message
			assert message.contains('native.${ext}'), message
			assert message.contains('use the C backend'), message
		}
	}
}

fn test_arm64_accepts_header_includes_and_other_directives() {
	mut a := flat.FlatAst.new()
	for path in ['<signal.h>', '"local.c.h"', '"@VMODROOT/native.hpp"'] {
		a.add_node(flat.Node{
			kind:  .directive
			value: 'include'
			typ:   flat.source_type_id(path)
		})
	}
	a.add_node(flat.Node{
		kind:  .directive
		value: 'flag'
		typ:   flat.source_type_id('-DNAME=file.c')
	})
	assert unsupported_arm64_c_source_error(a) == none
}
