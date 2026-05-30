module builder

import os
import v2.ast
import v2.pref

fn collect_cflags_for_test_source(source string, mut prefs pref.Preferences) string {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_builder_flag_target_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'main.v')
	os.write_file(source_path, source) or { panic(err) }
	prefs.skip_builtin = true
	mut b := new_builder(&prefs)
	b.files = [
		ast.File{
			name: source_path
		},
	]
	return b.collect_cflags_from_sources()
}

fn freestanding_test_call_name(name string, user_defines []string) string {
	return freestanding_test_call_name_with_hooks(name, user_defines, [])
}

fn freestanding_test_call_name_with_hooks(name string, user_defines []string, hooks []string) string {
	return freestanding_restricted_call_in_expr(ast.Expr(ast.CallExpr{
		lhs: ast.Expr(ast.Ident{
			name: name
		})
	}), FreestandingScanContext{
		user_defines:       user_defines
		target_os:          'linux'
		freestanding_hooks: hooks
	})
}

fn test_flag_directives_match_cross_and_freestanding_preferences() {
	mut cross_prefs := pref.new_preferences()
	cross_prefs.target_os = 'cross'
	cross_prefs.user_defines = ['cross']
	cross_flag := parse_flag_directive_line_with_pref('#flag cross -DCROSS_DIRECT',
		'/tmp/source.v', &cross_prefs) or { '' }
	linux_flag := parse_flag_directive_line_with_pref('#flag linux -DLINUX_DIRECT',
		'/tmp/source.v', &cross_prefs) or { '' }
	assert cross_flag == '-DCROSS_DIRECT'
	assert linux_flag == ''

	mut free_prefs := pref.new_preferences()
	free_prefs.target_os = 'linux'
	free_prefs.user_defines = ['freestanding']
	free_flag := parse_flag_directive_line_with_pref('#flag freestanding -DFREE_DIRECT',
		'/tmp/source.v', &free_prefs) or { '' }
	linux_in_free_flag := parse_flag_directive_line_with_pref('#flag linux -DLINUX_DIRECT',
		'/tmp/source.v', &free_prefs) or { '' }
	assert free_flag == '-DFREE_DIRECT'
	assert linux_in_free_flag == '-DLINUX_DIRECT'
}

fn test_comptime_flag_blocks_match_cross_and_freestanding_preferences() {
	mut cross_prefs := pref.new_preferences()
	cross_prefs.target_os = 'cross'
	cross_prefs.user_defines = ['cross']
	assert comptime_cond_matches_with_pref('cross', &cross_prefs)
	assert !comptime_cond_matches_with_pref('linux', &cross_prefs)
	assert comptime_cond_matches_with_pref('cross && !linux', &cross_prefs)

	mut free_prefs := pref.new_preferences()
	free_prefs.target_os = 'linux'
	free_prefs.user_defines = ['freestanding']
	assert comptime_cond_matches_with_pref('freestanding', &free_prefs)
	assert comptime_cond_matches_with_pref('linux', &free_prefs)
	assert comptime_cond_matches_with_pref('freestanding && linux', &free_prefs)
}

fn test_comptime_flag_blocks_use_or_and_not_precedence() {
	mut linux_prefs := pref.new_preferences()
	linux_prefs.target_os = 'linux'
	assert comptime_cond_matches_with_pref('!windows && linux', &linux_prefs)
	assert comptime_cond_matches_with_pref('linux || windows && macos', &linux_prefs)

	mut macos_prefs := pref.new_preferences()
	macos_prefs.target_os = 'macos'
	assert !comptime_cond_matches_with_pref('!windows && linux', &macos_prefs)
	assert !comptime_cond_matches_with_pref('linux || windows && macos', &macos_prefs)

	mut windows_prefs := pref.new_preferences()
	windows_prefs.target_os = 'windows'
	assert !comptime_cond_matches_with_pref('!windows && linux', &windows_prefs)
	assert !comptime_cond_matches_with_pref('linux || windows && macos', &windows_prefs)
}

fn test_parser_level_comptime_flags_match_cross_and_accepted_targets() {
	assert ast_comptime_flag_matches('cross', [], 'cross')
	assert ast_comptime_flag_matches('solaris', [], 'solaris')
	assert ast_comptime_flag_matches('qnx', [], 'qnx')
	assert ast_comptime_flag_matches('serenity', [], 'serenity')
	assert ast_comptime_flag_matches('plan9', [], 'plan9')
	assert ast_comptime_flag_matches('vinix', [], 'vinix')
	assert ast_comptime_flag_matches('ios', [], 'ios')
	assert !ast_comptime_flag_matches('linux', [], 'cross')
}

fn test_flag_directives_match_all_accepted_target_os_names() {
	for target in ['android', 'ios', 'solaris', 'qnx', 'serenity', 'plan9', 'vinix'] {
		flag := parse_flag_directive_line('#flag ${target} -D${target.to_upper()}',
			'/tmp/source.v', target) or { '' }
		assert flag == '-D${target.to_upper()}'
		assert comptime_cond_matches(target, target)
	}
}

fn test_collect_cflags_from_sources_matches_cross_and_freestanding_blocks() {
	cross_source := 'module main

$if cross {
#flag -DCROSS_BLOCK
}
$if linux {
#flag -DLINUX_BLOCK
}
$if macos {
#flag -DMACOS_BLOCK
}
$if windows {
#flag -DWINDOWS_BLOCK
}
#flag cross -DCROSS_DIRECT
#flag linux -DLINUX_DIRECT
#flag macos -DMACOS_DIRECT
#flag windows -DWINDOWS_DIRECT
'
	mut cross_prefs := pref.new_preferences()
	cross_prefs.target_os = 'cross'
	cross_prefs.user_defines = ['cross']
	cross_flags := collect_cflags_for_test_source(cross_source, mut cross_prefs)
	assert cross_flags.contains('-DCROSS_BLOCK')
	assert cross_flags.contains('-DCROSS_DIRECT')
	host_os := normalize_target_os_name(os.user_os())
	if host_os in ['linux', 'macos', 'windows'] {
		assert cross_flags.contains(match host_os {
			'macos' { '-DMACOS_BLOCK' }
			'windows' { '-DWINDOWS_BLOCK' }
			else { '-DLINUX_BLOCK' }
		})
		assert cross_flags.contains(match host_os {
			'macos' { '-DMACOS_DIRECT' }
			'windows' { '-DWINDOWS_DIRECT' }
			else { '-DLINUX_DIRECT' }
		})
	}
	if host_os != 'linux' {
		assert !cross_flags.contains('-DLINUX_BLOCK')
		assert !cross_flags.contains('-DLINUX_DIRECT')
	}
	if host_os != 'macos' {
		assert !cross_flags.contains('-DMACOS_BLOCK')
		assert !cross_flags.contains('-DMACOS_DIRECT')
	}
	if host_os != 'windows' {
		assert !cross_flags.contains('-DWINDOWS_BLOCK')
		assert !cross_flags.contains('-DWINDOWS_DIRECT')
	}

	free_source := 'module main

$if freestanding {
#flag -DFREE_BLOCK
}
$if linux {
#flag -DLINUX_BLOCK
}
#flag freestanding -DFREE_DIRECT
#flag linux -DLINUX_DIRECT
'
	mut free_prefs := pref.new_preferences()
	free_prefs.target_os = 'linux'
	free_prefs.user_defines = ['freestanding']
	free_flags := collect_cflags_for_test_source(free_source, mut free_prefs)
	assert free_flags.contains('-DFREE_BLOCK')
	assert free_flags.contains('-DFREE_DIRECT')
	assert free_flags.contains('-DLINUX_BLOCK')
	assert free_flags.contains('-DLINUX_DIRECT')
}

fn test_freestanding_diagnostics_gate_direct_runtime_helpers_by_capability() {
	for name in ['_write_buf_to_fd', '_writeln_to_fd', 'flush_stdout', 'flush_stderr'] {
		assert freestanding_test_call_name(name, ['freestanding_hooks']) == 'output_runtime'
		assert freestanding_test_call_name(name, ['freestanding_hooks', 'freestanding_output']) == 'output_runtime'
		assert freestanding_test_call_name_with_hooks(name, ['freestanding_hooks',
			'freestanding_output'], ['output']) == ''
	}

	assert freestanding_test_call_name_with_hooks('arguments', ['freestanding_hooks',
		'freestanding_output'], ['output']) == 'arguments'
	assert freestanding_test_call_name_with_hooks('arguments', ['freestanding_hooks',
		'freestanding_alloc'], ['alloc']) == 'arguments'
	assert freestanding_test_call_name('malloc_noscan',
		['freestanding_hooks', 'freestanding_alloc']) == 'alloc'
	assert freestanding_test_call_name_with_hooks('malloc_noscan', [
		'freestanding_hooks',
		'freestanding_alloc',
	], ['alloc']) == ''
	assert freestanding_test_call_name('println', ['freestanding_hooks', 'freestanding_output']) == 'println'
	assert freestanding_test_call_name_with_hooks('println', ['freestanding_hooks',
		'freestanding_output'], ['output']) == ''
	assert freestanding_restricted_call_in_expr(ast.Expr(ast.CallExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'println'
		})
		args: [
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '123'
			}),
		]
	}), FreestandingScanContext{
		user_defines:       ['freestanding_hooks', 'freestanding_output']
		target_os:          'linux'
		freestanding_hooks: ['output']
	}) == 'print_conversion'
	assert freestanding_restricted_call_in_expr(ast.Expr(ast.CallExpr{
		lhs:  ast.Expr(ast.Ident{
			name: 'println'
		})
		args: [
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '123'
			}),
		]
	}), FreestandingScanContext{
		user_defines:       ['freestanding_hooks', 'freestanding_output', 'freestanding_alloc']
		target_os:          'linux'
		freestanding_hooks: ['output', 'alloc']
	}) == 'print_conversion'
}

fn test_freestanding_diagnostics_skip_inactive_conditional_functions() {
	ctx := FreestandingScanContext{
		user_defines: ['freestanding']
		target_os:    'linux'
	}
	hosted_call := ast.Stmt(ast.ExprStmt{
		expr: ast.Expr(ast.CallExpr{
			lhs: ast.Expr(ast.Ident{
				name: 'println'
			})
		})
	})
	inactive_windows_fn := ast.Stmt(ast.FnDecl{
		attributes: [
			ast.Attribute{
				comptime_cond: ast.Expr(ast.Ident{
					name: 'windows'
				})
			},
		]
		stmts:      [hosted_call]
	})
	inactive_not_freestanding_fn := ast.Stmt(ast.FnDecl{
		attributes: [
			ast.Attribute{
				comptime_cond: ast.Expr(ast.PrefixExpr{
					op:   .not
					expr: ast.Expr(ast.Ident{
						name: 'freestanding'
					})
				})
			},
		]
		stmts:      [hosted_call]
	})
	active_fn := ast.Stmt(ast.FnDecl{
		attributes: [
			ast.Attribute{
				comptime_cond: ast.Expr(ast.Ident{
					name: 'freestanding'
				})
			},
		]
		stmts:      [hosted_call]
	})
	assert freestanding_restricted_call_in_stmt(inactive_windows_fn, ctx) == ''
	assert freestanding_restricted_call_in_stmt(inactive_not_freestanding_fn, ctx) == ''
	assert freestanding_restricted_call_in_stmt(active_fn, ctx) == 'println'
}

fn test_freestanding_diagnostics_do_not_require_alloc_for_fixed_arrays() {
	ctx := FreestandingScanContext{
		user_defines: ['freestanding']
		target_os:    'linux'
	}
	fixed_array := ast.Expr(ast.ArrayInitExpr{
		typ: ast.Expr(ast.Type(ast.ArrayFixedType{
			len:       ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '3'
			})
			elem_type: ast.Expr(ast.Ident{
				name: 'int'
			})
		}))
	})
	dynamic_array := ast.Expr(ast.ArrayInitExpr{
		typ: ast.Expr(ast.Type(ast.ArrayType{
			elem_type: ast.Expr(ast.Ident{
				name: 'int'
			})
		}))
	})
	fixed_literal_array := ast.Expr(ast.ArrayInitExpr{
		exprs: [
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '1'
			}),
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '2'
			}),
			ast.Expr(ast.BasicLiteral{
				kind:  .number
				value: '3'
			}),
		]
		len:   ast.Expr(ast.PostfixExpr{
			op:   .not
			expr: ast.empty_expr
		})
	})
	assert freestanding_restricted_call_in_expr(fixed_array, ctx) == ''
	assert freestanding_restricted_call_in_expr(fixed_literal_array, ctx) == ''
	assert freestanding_restricted_call_in_expr(dynamic_array, ctx) == 'alloc'
}

fn test_collect_cflags_from_sources_uses_or_and_not_precedence() {
	source := 'module main

$if !windows && linux {
#flag -DNEGATED_AND_LINUX
}

$if linux || windows && macos {
#flag -DOR_AND_LINUX
}
'
	mut linux_prefs := pref.new_preferences()
	linux_prefs.target_os = 'linux'
	linux_flags := collect_cflags_for_test_source(source, mut linux_prefs)
	assert linux_flags.contains('-DNEGATED_AND_LINUX')
	assert linux_flags.contains('-DOR_AND_LINUX')

	mut macos_prefs := pref.new_preferences()
	macos_prefs.target_os = 'macos'
	macos_flags := collect_cflags_for_test_source(source, mut macos_prefs)
	assert !macos_flags.contains('-DNEGATED_AND_LINUX')
	assert !macos_flags.contains('-DOR_AND_LINUX')

	mut windows_prefs := pref.new_preferences()
	windows_prefs.target_os = 'windows'
	windows_flags := collect_cflags_for_test_source(source, mut windows_prefs)
	assert !windows_flags.contains('-DNEGATED_AND_LINUX')
	assert !windows_flags.contains('-DOR_AND_LINUX')
}
