module newjs

import strings
import v.ast
import v.token
import v.pref
import v.util
import v.util.version
import v.depgraph
import analysis


[heap]
struct JsGen {
	pref &pref.Preferences
mut: 
	table                  &ast.Table = unsafe { nil }
	definitions            strings.Builder
}

[heap]
struct ModuleContext {
mut:
	gen &JsGen = unsafe { nil }
	mod ast.Module
	files []string
	indentation int = 1
	minify bool
	depdendencies map[&ast.Ident]bool
	escaping_vars map[&ast.Var]bool
	object_names map[&ast.Ident]string
	mod_vars map[string]string
}

struct FuncContext {
mut:
	mod_ctx &ModuleContext = unsafe { nil }
	parent &FuncContext = unsafe { nil }
	all_vars map[string]int = {}
	local_vars []string
	result_names []ast.Expr
	output []byte
	delayed_output []byte 
	pos_available bool 
}


pub fn foo() {

}