module parser

import v.ast
import v.pref

const enum_comptime_test_source = r'module main

enum EN {
	en_x = 1
	$if compile_time_option ? {
		en_y = 2147483647
	} $else {
		en_z = 2
	}
}
'

fn parse_enum_comptime_test_file(args []string) ast.EnumDecl {
	mut table := ast.new_table()
	prefs, _ := pref.parse_args_and_show_errors([], args, false)
	file := parse_text(enum_comptime_test_source, @FILE, mut table, .skip_comments, prefs)
	assert file.errors.len == 0
	for stmt in file.stmts {
		if stmt is ast.EnumDecl {
			return stmt
		}
	}
	assert false
	return ast.EnumDecl{}
}

fn test_enum_comptime_fields_with_define() {
	enum_decl := parse_enum_comptime_test_file(['-d', 'compile_time_option', @FILE])
	assert enum_decl.fields.len == 2
	assert enum_decl.fields[0].name == 'en_x'
	assert enum_decl.fields[1].name == 'en_y'
	assert enum_decl.fields[1].has_expr
}

fn test_enum_comptime_fields_without_define() {
	enum_decl := parse_enum_comptime_test_file([@FILE])
	assert enum_decl.fields.len == 2
	assert enum_decl.fields[0].name == 'en_x'
	assert enum_decl.fields[1].name == 'en_z'
	assert enum_decl.fields[1].has_expr
}
