module transformer

import v2.ast
import v2.pref as vpref
import v2.types

fn import_alias_test_transformer(aliases map[string]string) &Transformer {
	env := &types.Environment{}
	return &Transformer{
		pref:               &vpref.Preferences{}
		env:                unsafe { env }
		cur_import_aliases: aliases.clone()
	}
}

fn test_selector_type_name_resolves_nested_import_aliases() {
	t := import_alias_test_transformer({
		'bar': 'foo.bar'
		'baz': 'foo.bar'
	})
	bar_leaf := ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'bar'
		})
		rhs: ast.Ident{
			name: 'Leaf'
		}
	}
	baz_leaf := ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'baz'
		})
		rhs: ast.Ident{
			name: 'Leaf'
		}
	}
	assert t.selector_type_name(bar_leaf, false) == 'Leaf'
	assert t.selector_type_name(bar_leaf, true) == 'foo__bar__Leaf'
	assert t.selector_type_name(baz_leaf, true) == 'foo__bar__Leaf'
}

fn test_generic_match_branch_variant_info_resolves_nested_import_alias() {
	t := import_alias_test_transformer({
		'bar': 'foo.bar'
	})
	lhs := ast.Expr(ast.SelectorExpr{
		lhs: ast.Expr(ast.Ident{
			name: 'bar'
		})
		rhs: ast.Ident{
			name: 'Leaf'
		}
	})
	base_name, variant_full, variant_module, ok := t.generic_match_branch_variant_info(lhs, [
		ast.Expr(ast.Ident{
			name: 'int'
		}),
	])
	assert ok
	assert base_name == 'Leaf'
	assert variant_full == 'foo__bar__Leaf_T_int'
	assert variant_module == 'foo__bar'
}
