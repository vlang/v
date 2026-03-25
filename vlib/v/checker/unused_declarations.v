module checker

import v.ast

@[inline]
fn (mut c Checker) mark_fn_decl_as_referenced(fkey string) {
	if fkey == '' {
		return
	}
	c.table.used_features.referenced_fns[fkey] = true
}

@[inline]
fn (mut c Checker) mark_const_decl_as_referenced(name string) {
	if name == '' {
		return
	}
	c.table.used_features.referenced_consts[name] = true
}

fn (mut c Checker) mark_type_str_method_as_referenced(typ ast.Type) {
	sym := c.table.sym(c.unwrap_generic(typ))
	if method := sym.find_method_with_generic_parent('str') {
		c.mark_fn_decl_as_referenced(method.fkey())
	}
}

fn (c &Checker) module_qualified_selector_const_name(node ast.SelectorExpr) string {
	if root_ident := node.root_ident() {
		if root_ident.name == c.mod {
			return '${c.mod}.${node.field_name}'
		}
		for import_sym in c.file.imports {
			alias := if import_sym.alias == '' {
				import_sym.mod.all_after_last('.')
			} else {
				import_sym.alias
			}
			if root_ident.name == alias {
				return '${import_sym.mod}.${node.field_name}'
			}
		}
	}
	return ''
}

fn (mut c Checker) check_unused_declarations(ast_files []&ast.File) {
	if c.pref.is_repl || c.pref.is_test || c.pref.check_only || c.pref.only_check_syntax
		|| c.pref.translated || c.nr_errors > 0 || c.main_fn_decl_node.name == '' {
		return
	}
	saved_file := c.file
	for file in ast_files {
		if file.mod.name != 'main' || file.is_test || file.is_generated || file.is_translated {
			continue
		}
		c.change_current_file(file)
		c.check_unused_declarations_in_stmts(file.stmts)
		if c.should_abort {
			break
		}
	}
	if saved_file != unsafe { nil } {
		c.change_current_file(saved_file)
	}
}

fn (mut c Checker) check_unused_declarations_in_stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		match stmt {
			ast.ConstDecl {
				for field in stmt.fields {
					if c.should_warn_about_unused_const(field) {
						c.warn('unused constant: `${stripped_name(field.name)}`', field.pos)
					}
				}
			}
			ast.FnDecl {
				if c.should_warn_about_unused_fn(stmt) {
					name := if stmt.short_name != '' { stmt.short_name } else { stmt.get_name() }
					c.warn('unused function: `${name}`', stmt.name_pos)
				}
			}
			ast.ExprStmt {
				match stmt.expr {
					ast.IfExpr {
						if stmt.expr.is_comptime {
							for branch in stmt.expr.branches {
								if c.is_active_comptime_branch(branch.id) {
									c.check_unused_declarations_in_stmts(branch.stmts)
								}
							}
						}
					}
					ast.MatchExpr {
						if stmt.expr.is_comptime {
							for branch in stmt.expr.branches {
								if c.is_active_comptime_branch(branch.id) {
									c.check_unused_declarations_in_stmts(branch.stmts)
								}
							}
						}
					}
					else {}
				}
			}
			else {}
		}
		if c.should_abort {
			return
		}
	}
}

fn (c &Checker) is_active_comptime_branch(branch_id int) bool {
	if branch_id == 0 {
		return true
	}
	if result := c.table.comptime_is_true['|id=${branch_id}|'] {
		return result.val
	}
	return true
}

fn (c &Checker) should_warn_about_unused_const(field ast.ConstField) bool {
	if field.is_pub || field.is_markused || field.is_exported || field.is_virtual_c {
		return false
	}
	return field.name !in c.table.used_features.referenced_consts
}

fn (c &Checker) should_warn_about_unused_fn(node ast.FnDecl) bool {
	if node.is_pub || node.is_markused || node.is_exported || node.no_body || node.is_main
		|| node.is_test || node.is_method || node.is_static_type_method || node.should_be_skipped
		|| node.short_name in ['init', 'cleanup'] {
		return false
	}
	return node.fkey() !in c.table.used_features.referenced_fns
}
