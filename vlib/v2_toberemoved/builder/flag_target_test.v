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

fn collect_cflags_for_flat_only_test_source(source string, mut prefs pref.Preferences) string {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v2_builder_flag_target_flat_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'main.v')
	os.write_file(source_path, source) or { panic(err) }
	prefs.skip_builtin = true
	mut b := new_builder(&prefs)
	b.flat = ast.flatten_files([
		ast.File{
			name: source_path
			mod:  'main'
		},
	])
	b.files = []ast.File{}
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
	assert !comptime_cond_matches_with_pref('linux ?', &cross_prefs)
	assert comptime_cond_matches_with_pref('cross && !linux', &cross_prefs)

	mut optional_linux_prefs := pref.new_preferences()
	optional_linux_prefs.target_os = 'cross'
	optional_linux_prefs.user_defines = ['cross', 'linux']
	optional_linux_prefs.explicit_user_defines = ['linux']
	assert comptime_cond_matches_with_pref('linux ?', &optional_linux_prefs)

	mut linux_prefs := pref.new_preferences()
	linux_prefs.target_os = 'linux'
	assert comptime_cond_matches_with_pref('linux', &linux_prefs)
	assert !comptime_cond_matches_with_pref('linux ?', &linux_prefs)
	assert !comptime_cond_matches_with_pref('linux ? || windows', &linux_prefs)
	linux_prefs.user_defines = ['linux']
	linux_prefs.explicit_user_defines = ['linux']
	assert comptime_cond_matches_with_pref('linux ?', &linux_prefs)
	assert comptime_cond_matches_with_pref('linux ? || windows', &linux_prefs)

	mut termux_prefs := pref.new_preferences()
	termux_prefs.target_os = 'termux'
	assert comptime_cond_matches_with_pref('termux', &termux_prefs)
	assert comptime_cond_matches_with_pref('linux || termux', &termux_prefs)
	assert !comptime_cond_matches_with_pref('android', &termux_prefs)
	assert !comptime_cond_matches_with_pref('android && !termux', &termux_prefs)

	mut free_prefs := pref.new_preferences()
	free_prefs.target_os = 'linux'
	free_prefs.user_defines = ['freestanding']
	assert comptime_cond_matches_with_pref('freestanding', &free_prefs)
	assert !comptime_cond_matches_with_pref('freestanding ?', &free_prefs)
	assert comptime_cond_matches_with_pref('linux', &free_prefs)
	assert comptime_cond_matches_with_pref('freestanding && linux', &free_prefs)
}

fn test_comptime_pkgconfig_condition_matches_available_package() {
	mut prefs := pref.new_preferences()
	prefs.target_os = os.user_os()
	assert !comptime_cond_matches_with_pref(r"$pkgconfig('__v2_definitely_missing_pkgconfig_test_package__')",
		&prefs)
	if pref.comptime_pkgconfig_value('sqlite3') {
		assert comptime_cond_matches_with_pref(r"$pkgconfig('sqlite3')", &prefs)
	}
}

fn test_pkgconfig_directive_collects_flags_for_active_native_branch() {
	mut prefs := pref.new_preferences()
	prefs.target_os = os.user_os()
	if pref.comptime_pkgconfig_value('sqlite3') {
		flag := parse_flag_directive_line_with_pref('#pkgconfig sqlite3', '/tmp/source.v', &prefs) or {
			''
		}
		assert flag.contains('sqlite3')
	}

	mut cross_prefs := pref.new_preferences()
	cross_prefs.target_os = 'cross'
	cross_prefs.output_cross_c = true
	flag := parse_flag_directive_line_with_pref('#pkgconfig sqlite3', '/tmp/source.v', &cross_prefs) or {
		''
	}
	assert flag == ''
}

fn test_comptime_optional_target_mode_flags_require_explicit_define() {
	mut cross_prefs := pref.new_preferences()
	cross_prefs.target_os = 'cross'
	cross_prefs.user_defines = ['cross']
	assert comptime_cond_matches_with_pref('cross', &cross_prefs)
	assert !comptime_cond_matches_with_pref('cross ?', &cross_prefs)
	cross_prefs.explicit_user_defines = ['cross']
	assert comptime_cond_matches_with_pref('cross ?', &cross_prefs)

	mut free_prefs := pref.new_preferences()
	free_prefs.target_os = 'linux'
	free_prefs.user_defines = ['freestanding']
	assert comptime_cond_matches_with_pref('freestanding', &free_prefs)
	assert !comptime_cond_matches_with_pref('freestanding ?', &free_prefs)
	free_prefs.explicit_user_defines = ['freestanding']
	assert comptime_cond_matches_with_pref('freestanding ?', &free_prefs)

	mut none_prefs := pref.new_preferences()
	none_prefs.target_os = 'none'
	none_prefs.user_defines = ['freestanding']
	assert comptime_cond_matches_with_pref('none', &none_prefs)
	assert !comptime_cond_matches_with_pref('none ?', &none_prefs)
	none_prefs.explicit_user_defines = ['none']
	assert comptime_cond_matches_with_pref('none ?', &none_prefs)
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
	assert ast_comptime_flag_matches('termux', [], 'termux')
	assert ast_comptime_flag_matches('none', [], 'none')
	assert !ast_comptime_flag_matches('linux', [], 'cross')
	assert !ast_comptime_flag_matches('linux', ['linux'], 'windows')
	assert !ast_comptime_flag_matches('none', ['none'], 'linux')
}

fn test_flag_directives_match_all_accepted_target_os_names() {
	for target in ['android', 'termux', 'ios', 'solaris', 'qnx', 'serenity', 'plan9', 'vinix'] {
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
$if cross ? {
#flag -DOPTIONAL_CROSS_BLOCK
}
$if linux {
#flag -DLINUX_BLOCK
}
$if linux ? {
#flag -DOPTIONAL_LINUX_BLOCK
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
	assert !cross_flags.contains('-DOPTIONAL_CROSS_BLOCK')
	assert !cross_flags.contains('-DOPTIONAL_LINUX_BLOCK')
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
	mut cross_with_optional_linux_prefs := pref.new_preferences()
	cross_with_optional_linux_prefs.target_os = 'cross'
	cross_with_optional_linux_prefs.user_defines = ['cross', 'linux']
	cross_with_optional_linux_prefs.explicit_user_defines = ['linux']
	cross_with_optional_linux_flags := collect_cflags_for_test_source(cross_source, mut
		cross_with_optional_linux_prefs)
	assert cross_with_optional_linux_flags.contains('-DOPTIONAL_LINUX_BLOCK')
	assert !cross_with_optional_linux_flags.contains('-DOPTIONAL_CROSS_BLOCK')

	mut cross_explicit_prefs := pref.new_preferences()
	cross_explicit_prefs.target_os = 'cross'
	cross_explicit_prefs.user_defines = ['cross']
	cross_explicit_prefs.explicit_user_defines = ['cross']
	cross_explicit_flags := collect_cflags_for_test_source(cross_source, mut cross_explicit_prefs)
	assert cross_explicit_flags.contains('-DOPTIONAL_CROSS_BLOCK')

	free_source := 'module main

$if freestanding {
#flag -DFREE_BLOCK
}
$if freestanding ? {
#flag -DOPTIONAL_FREE_BLOCK
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
	assert !free_flags.contains('-DOPTIONAL_FREE_BLOCK')
	assert free_flags.contains('-DFREE_DIRECT')
	assert free_flags.contains('-DLINUX_BLOCK')
	assert free_flags.contains('-DLINUX_DIRECT')

	mut free_explicit_prefs := pref.new_preferences()
	free_explicit_prefs.target_os = 'linux'
	free_explicit_prefs.user_defines = ['freestanding']
	free_explicit_prefs.explicit_user_defines = ['freestanding']
	free_explicit_flags := collect_cflags_for_test_source(free_source, mut free_explicit_prefs)
	assert free_explicit_flags.contains('-DOPTIONAL_FREE_BLOCK')
}

fn test_collect_cflags_from_sources_reads_flat_file_names() {
	source := 'module main

#flag -DFLAT_ONLY_FLAG
'
	mut prefs := pref.new_preferences()
	flags := collect_cflags_for_flat_only_test_source(source, mut prefs)
	assert flags.contains('-DFLAT_ONLY_FLAG')
}

fn test_collect_cflags_from_sources_keeps_optional_os_flags_custom_only() {
	source := 'module main

$if linux {
#flag -DLINUX_BLOCK
}
$if linux ? {
#flag -DOPTIONAL_LINUX_BLOCK
}
$if linux ? || windows {
#flag -DOPTIONAL_LINUX_OR_WINDOWS_BLOCK
}
'
	mut linux_prefs := pref.new_preferences()
	linux_prefs.target_os = 'linux'
	linux_flags := collect_cflags_for_test_source(source, mut linux_prefs)
	assert linux_flags.contains('-DLINUX_BLOCK')
	assert !linux_flags.contains('-DOPTIONAL_LINUX_BLOCK')
	assert !linux_flags.contains('-DOPTIONAL_LINUX_OR_WINDOWS_BLOCK')

	mut linux_defined_prefs := pref.new_preferences()
	linux_defined_prefs.target_os = 'linux'
	linux_defined_prefs.user_defines = ['linux']
	linux_defined_prefs.explicit_user_defines = ['linux']
	linux_defined_flags := collect_cflags_for_test_source(source, mut linux_defined_prefs)
	assert linux_defined_flags.contains('-DLINUX_BLOCK')
	assert linux_defined_flags.contains('-DOPTIONAL_LINUX_BLOCK')
	assert linux_defined_flags.contains('-DOPTIONAL_LINUX_OR_WINDOWS_BLOCK')

	mut windows_prefs := pref.new_preferences()
	windows_prefs.target_os = 'windows'
	windows_flags := collect_cflags_for_test_source(source, mut windows_prefs)
	assert !windows_flags.contains('-DLINUX_BLOCK')
	assert !windows_flags.contains('-DOPTIONAL_LINUX_BLOCK')
	assert windows_flags.contains('-DOPTIONAL_LINUX_OR_WINDOWS_BLOCK')
}

fn test_collect_cflags_from_sources_keeps_optional_none_custom_only() {
	source := 'module main

$if none {
#flag -DNONE_BLOCK
}
$if none ? {
#flag -DOPTIONAL_NONE_BLOCK
}
'
	mut none_prefs := pref.new_preferences()
	none_prefs.target_os = 'none'
	none_prefs.user_defines = ['freestanding']
	none_flags := collect_cflags_for_test_source(source, mut none_prefs)
	assert none_flags.contains('-DNONE_BLOCK')
	assert !none_flags.contains('-DOPTIONAL_NONE_BLOCK')

	mut none_explicit_prefs := pref.new_preferences()
	none_explicit_prefs.target_os = 'none'
	none_explicit_prefs.user_defines = ['freestanding']
	none_explicit_prefs.explicit_user_defines = ['none']
	none_explicit_flags := collect_cflags_for_test_source(source, mut none_explicit_prefs)
	assert none_explicit_flags.contains('-DOPTIONAL_NONE_BLOCK')
}

fn test_collect_cflags_from_sources_matches_termux_blocks_and_directives() {
	source := 'module main

$if termux {
#flag -DTERMUX_BLOCK
}
$if linux || termux {
#flag -DTERMUX_OR_LINUX_BLOCK
}
$if android && !termux {
#flag -DANDROID_OUTSIDE_TERMUX_BLOCK
}
#flag termux -DTERMUX_DIRECT
#flag android -DANDROID_DIRECT
'
	mut termux_prefs := pref.new_preferences()
	termux_prefs.target_os = 'termux'
	flags := collect_cflags_for_test_source(source, mut termux_prefs)
	assert flags.contains('-DTERMUX_BLOCK')
	assert flags.contains('-DTERMUX_OR_LINUX_BLOCK')
	assert flags.contains('-DTERMUX_DIRECT')
	assert !flags.contains('-DANDROID_OUTSIDE_TERMUX_BLOCK')
	assert !flags.contains('-DANDROID_DIRECT')
}

fn test_freestanding_diagnostics_gate_direct_builtins_by_capability() {
	for name in ['print', 'println', 'eprint', 'eprintln'] {
		assert freestanding_test_call_name(name, ['freestanding_hooks']) == name
		assert freestanding_test_call_name_with_hooks(name, ['freestanding_hooks',
			'freestanding_output'], ['output']) == ''
	}
	assert freestanding_test_call_name('panic', ['freestanding_hooks']) == 'panic'
	assert freestanding_test_call_name_with_hooks('panic', ['freestanding_hooks',
		'freestanding_panic'], ['panic']) == ''
}

fn test_freestanding_diagnostics_leave_runtime_helpers_to_cleanc() {
	for name in ['_write_buf_to_fd', '_writeln_to_fd', 'flush_stdout', 'flush_stderr'] {
		assert freestanding_test_call_name(name, ['freestanding_hooks']) == ''
	}

	assert freestanding_test_call_name_with_hooks('arguments', ['freestanding_hooks',
		'freestanding_output'], ['output']) == ''
	assert freestanding_test_call_name_with_hooks('arguments', ['freestanding_hooks',
		'freestanding_alloc'], ['alloc']) == ''
	assert freestanding_test_call_name('malloc_noscan',
		['freestanding_hooks', 'freestanding_alloc']) == ''
	assert freestanding_test_call_name_with_hooks('malloc_noscan', [
		'freestanding_hooks',
		'freestanding_alloc',
	], ['alloc']) == ''
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
	}) == ''
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
	}) == ''
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

fn test_freestanding_diagnostics_leave_array_allocation_to_cleanc() {
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
	assert freestanding_restricted_call_in_expr(dynamic_array, ctx) == ''
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
