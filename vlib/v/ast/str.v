// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v.util
import strings

// get_name returns the real name for the function declaration
pub fn (f &FnDecl) get_name() string {
	if f.is_static_type_method {
		return f.name.all_after_last('__static__')
	} else {
		return f.name
	}
}

// get_name returns the real name for the function calling
pub fn (f &CallExpr) get_name() string {
	if f.name != '' && f.name.all_after_last('.')[0].is_capital() && f.name.contains('__static__') {
		return f.name.replace('__static__', '.')
	} else {
		return f.name
	}
}

pub fn (node &FnDecl) modname() string {
	if node.mod != '' {
		return node.mod
	}
	mut pamod := node.name.all_before_last('.')
	if pamod == node.name.after('.') {
		pamod = if node.is_builtin { 'builtin' } else { 'main' }
	}
	return pamod
}

// fkey returns a unique name of the function/method.
// it is used in table.used_fns and v.markused.
pub fn (node &FnDecl) fkey() string {
	if node.is_method {
		return '${int(node.receiver.typ)}.${node.name}'
	}
	return node.name
}

pub fn (node &Fn) fkey() string {
	if node.is_method {
		return '${int(node.receiver_type)}.${node.name}'
	}
	return node.name
}

pub fn (node &CallExpr) fkey() string {
	if node.is_method {
		return '${int(node.receiver_type)}.${node.name}'
	}
	return node.name
}

// These methods are used only by vfmt, vdoc, and for debugging.
pub fn (t &Table) stringify_anon_decl(node &AnonFn, cur_mod string, m2a map[string]string) string {
	mut f := strings.new_builder(30)
	f.write_string('fn ')
	if node.inherited_vars.len > 0 {
		f.write_string('[')
		for i, var in node.inherited_vars {
			if i > 0 {
				f.write_string(', ')
			}
			if var.is_shared {
				f.write_string('shared ')
			} else if var.is_atomic {
				f.write_string('atomic ')
			} else if var.is_mut {
				f.write_string('mut ')
			}
			f.write_string(var.name)
		}
		f.write_string('] ')
	}
	t.stringify_fn_after_name(node.decl, mut f, cur_mod, m2a)
	return f.str()
}

pub fn (t &Table) stringify_fn_decl(node &FnDecl, cur_mod string, m2a map[string]string) string {
	mut f := strings.new_builder(30)
	if node.is_pub {
		f.write_string('pub ')
	}
	f.write_string('fn ')
	pre_comments := node.comments.filter(it.pos.pos < node.name_pos.pos)
	if pre_comments.len > 0 {
		write_comments(pre_comments, mut f)
		if !f.last_n(1)[0].is_space() {
			f.write_string(' ')
		}
	}
	if node.is_method {
		f.write_string('(')
		mut styp := util.no_cur_mod(t.type_to_code(node.receiver.typ.clear_flag(.shared_f)),
			cur_mod)
		if node.rec_mut {
			f.write_string(node.receiver.typ.share().str() + ' ')
			styp = styp[1..] // remove &
		}
		f.write_string(node.receiver.name + ' ')
		styp = util.no_cur_mod(styp, cur_mod)
		if node.params[0].is_auto_rec {
			styp = styp.trim('&')
		}
		f.write_string(styp + ') ')
	} else if node.is_static_type_method {
		mut styp := util.no_cur_mod(t.type_to_code(node.receiver.typ.clear_flag(.shared_f)),
			cur_mod)
		f.write_string(styp + '.')
	}
	mut name := if !node.is_method && node.language == .v {
		node.name.all_after_last('.')
	} else {
		node.name
	}
	if node.is_static_type_method {
		name = name.after('__static__')
	}
	f.write_string(name)
	if name in ['+', '-', '*', '/', '%', '<', '>', '==', '!=', '>=', '<='] {
		f.write_string(' ')
	}
	t.stringify_fn_after_name(node, mut f, cur_mod, m2a)
	return f.str()
}

fn (t &Table) stringify_fn_after_name(node &FnDecl, mut f strings.Builder, cur_mod string, m2a map[string]string) {
	mut add_para_types := true
	mut is_wrap_needed := false
	if node.generic_names.len > 0 {
		if node.is_method {
			sym := t.sym(node.params[0].typ)
			if sym.info is Struct {
				generic_names := sym.info.generic_types.map(t.sym(it).name)
				if generic_names == node.generic_names {
					add_para_types = false
				}
			}
		}
		if add_para_types {
			f.write_string('[')
			for i, gname in node.generic_names {
				is_last := i == node.generic_names.len - 1
				f.write_string(gname)
				if !is_last {
					f.write_string(', ')
				}
			}
			f.write_string(']')
		}
	}
	f.write_string('(')
	for i, param in node.params {
		// skip receiver
		if node.is_method && i == 0 {
			continue
		}
		if param.is_hidden {
			continue
		}
		is_last_param := i == node.params.len - 1
		is_type_only := param.name == ''
		should_add_type := true // is_last_param || is_type_only || node.params[i + 1].typ != param.typ ||
		// (node.is_variadic && i == node.params.len - 2)
		pre_comments := param.comments.filter(it.pos.pos < param.pos.pos)
		if pre_comments.len > 0 {
			if i == 0 && !pre_comments.last().is_inline {
				is_wrap_needed = true
				f.write_string('\n\t')
			}
			write_comments(pre_comments, mut f)
			if !f.last_n(1)[0].is_space() {
				f.write_string(' ')
			}
		}
		if is_wrap_needed {
			f.write_string('\t')
		}
		if param.is_mut {
			f.write_string(param.typ.share().str() + ' ')
		}
		f.write_string(param.name)
		param_sym := t.sym(param.typ)
		if param_sym.kind == .struct_ && (param_sym.info as Struct).is_anon {
			f.write_string(' struct {')
			struct_ := param_sym.info as Struct
			for field in struct_.fields {
				f.write_string(' ${field.name} ${t.type_to_str(field.typ)}')
				if field.has_default_expr {
					f.write_string(' = ${field.default_expr}')
				}
			}
			if struct_.fields.len > 0 {
				f.write_string(' ')
			}
			f.write_string('}')
		} else {
			mut s := t.type_to_str(param.typ.clear_flag(.shared_f))
			if param.is_mut {
				if s.starts_with('&') && ((!param_sym.is_number() && param_sym.kind != .bool)
					|| node.language != .v) {
					s = s[1..]
				}
			}
			s = util.no_cur_mod(s, cur_mod)
			s = shorten_full_name_based_on_aliases(s, m2a)
			if should_add_type {
				if !is_type_only {
					f.write_string(' ')
				}
				if node.is_variadic && is_last_param {
					f.write_string('...')
				}
				f.write_string(s)
			}
		}
		if !is_last_param {
			f.write_string(', ')
		}
	}
	f.write_string(')')
	if node.return_type != void_type {
		sreturn_type := util.no_cur_mod(t.type_to_str(node.return_type), cur_mod)
		short_sreturn_type := shorten_full_name_based_on_aliases(sreturn_type, m2a)
		f.write_string(' ${short_sreturn_type}')
	}
}

fn write_comments(comments []Comment, mut f strings.Builder) {
	for i, c in comments {
		if !f.last_n(1)[0].is_space() {
			f.write_string(' ')
		}
		write_comment(c, mut f)
		if c.is_inline && i < comments.len - 1 && !c.is_multi {
			f.write_string(' ')
		} else if (!c.is_inline || c.is_multi) && i < comments.len - 1 {
			f.writeln('')
		}
	}
}

fn write_comment(node Comment, mut f strings.Builder) {
	if node.is_inline {
		x := node.text.trim_left('\x01').trim_space()
		if x.contains('\n') {
			f.writeln('/*')
			f.writeln(x)
			f.write_string('*/')
		} else {
			f.write_string('/* ${x} */')
		}
	} else {
		mut s := node.text.trim_left('\x01').trim_right(' ')
		mut out_s := '//'
		if s != '' {
			if s[0].is_letter() || s[0].is_digit() {
				out_s += ' '
			}
			out_s += s
		}
		f.writeln(out_s)
	}
}

struct StringifyModReplacement {
	mod    string
	alias  string
	weight int
}

fn shorten_full_name_based_on_aliases(input string, m2a map[string]string) string {
	if m2a.len == 0 || -1 == input.index_u8(`.`) {
		// a simple typename, like `string` or `[]bool`; no module aliasings apply,
		// (or there just are not any mappings)
		return input
	}
	// Shorten the full names to their aliases, but replace the longer mods first, so that:
	//   `import user.project`
	//   `import user.project.routes`
	// will lead to replacing `user.project.routes` first to `routes`, NOT `user.project.routes` to `project.routes`.
	// Also take into account the nesting level, so `a.e.c.d` will be shortened before `a.xyz.b`, even though they are the same length.
	mut replacements := []StringifyModReplacement{cap: m2a.len}
	for mod, alias in m2a {
		if mod == alias {
			// for vlib modules like `import strings` -> mod: `strings` | alias: `strings`
			// ... which is the same, so no replacements are needed
			continue
		}
		if !input.contains(mod) {
			continue
		}
		replacements << StringifyModReplacement{
			mod: mod
			alias: alias
			weight: mod.count('.') * 100 + mod.len
		}
	}
	if replacements.len == 0 {
		return input
	}
	//
	mut res := input
	if replacements.len > 1 {
		replacements.sort(a.weight > b.weight)
	}
	for r in replacements {
		if -1 == res.index_u8(`.`) {
			// there are no remaining module parts left in the type name, it is a local one after all
			break
		}
		if !res.contains(r.mod) {
			// nothing to replace as well (just minimises modifications and string clonings)
			continue
		}
		// r.mod: `v.token` | r.alias: `xyz` | res: `v.token.Abc`                -> `xyz.Abc`
		// r.mod: `v.ast`   | r.alias: `ast` | res: `v.ast.AliasTypeDecl`        -> `ast.AliasTypeDecl`
		// r.mod: `v.ast`   | r.alias: `ast` | res: `[]v.ast.InterfaceEmbedding` -> `[]ast.InterfaceEmbedding`
		res = res.replace(r.mod, r.alias)
	}
	return res
}

// Expressions in string interpolations may have to be put in braces if they
// are non-trivial, if they would interfere with the next character or if a
// format specification is given. In the latter case
// the format specifier must be appended, separated by a colon:
// '$z $z.b $z.c.x ${x[4]} ${z:8.3f} ${a:-20} ${a>b+2}'
// This method creates the format specifier (including the colon) or an empty
// string if none is needed and also returns (as bool) if the expression
// must be enclosed in braces.
pub fn (lit &StringInterLiteral) get_fspec_braces(i int) (string, bool) {
	mut res := []string{}
	needs_fspec := lit.need_fmts[i] || lit.pluss[i]
		|| (lit.fills[i] && lit.fwidths[i] >= 0) || lit.fwidths[i] != 0
		|| lit.precisions[i] != 987698
	mut needs_braces := needs_fspec
	sx := lit.exprs[i].str()
	if sx.contains(r'"') || sx.contains(r"'") {
		needs_braces = true
	}
	if !needs_braces {
		if i + 1 < lit.vals.len && lit.vals[i + 1].len > 0 {
			next_char := lit.vals[i + 1][0]
			if util.is_func_char(next_char) || next_char == `.` || next_char == `(` {
				needs_braces = true
			}
		}
	}
	if !needs_braces {
		mut sub_expr := lit.exprs[i]
		for {
			match mut sub_expr {
				Ident {
					if sub_expr.name[0] == `@` {
						needs_braces = true
					}
					break
				}
				else {
					needs_braces = true
					break
				}
			}
		}
	}
	if needs_fspec {
		res << ':'
		if lit.pluss[i] {
			res << '+'
		}
		if lit.fills[i] && lit.fwidths[i] >= 0 {
			res << '0'
		}
		if lit.fwidths[i] != 0 {
			res << '${lit.fwidths[i]}'
		}
		if lit.precisions[i] != 987698 {
			res << '.${lit.precisions[i]}'
		}
		if lit.need_fmts[i] {
			res << '${lit.fmts[i]:c}'
		}
	}
	return res.join(''), needs_braces
}

// string representation of expr
pub fn (x Expr) str() string {
	match x {
		AnonFn {
			return 'anon_fn'
		}
		ComptimeType {
			return x.str()
		}
		DumpExpr {
			return 'dump(${x.expr.str()})'
		}
		ArrayInit {
			mut fields := []string{}
			if x.has_len {
				fields << 'len: ${x.len_expr.str()}'
			}
			if x.has_cap {
				fields << 'cap: ${x.cap_expr.str()}'
			}
			if x.has_default {
				fields << 'init: ${x.default_expr.str()}'
			}
			typ_str := global_table.type_to_str(x.elem_type)
			if fields.len > 0 {
				if x.is_fixed {
					return '${x.exprs.str()}${typ_str}{${fields.join(', ')}}'
				} else {
					return '[]${typ_str}{${fields.join(', ')}}'
				}
			} else {
				if x.is_fixed {
					return '${x.exprs.str()}${typ_str}{}'
				} else {
					return x.exprs.str()
				}
			}
		}
		AsCast {
			return '${x.expr.str()} as ${global_table.type_to_str(x.typ)}'
		}
		AtExpr {
			return '${x.val}'
		}
		CTempVar {
			return x.orig.str()
		}
		BoolLiteral {
			return x.val.str()
		}
		CastExpr {
			return '${x.typname}(${x.expr.str()})'
		}
		CallExpr {
			sargs := args2str(x.args)
			propagate_suffix := if x.or_block.kind == .propagate_option {
				'?'
			} else if x.or_block.kind == .propagate_result {
				'!'
			} else {
				''
			}
			if x.is_method {
				return '${x.left.str()}.${x.name}(${sargs})${propagate_suffix}'
			}
			if x.name.starts_with('${x.mod}.') {
				return util.strip_main_name('${x.get_name()}(${sargs})${propagate_suffix}')
			}
			if x.mod == '' && x.name == '' {
				return x.left.str() + '(${sargs})${propagate_suffix}'
			}
			if x.name.contains('.') {
				return '${x.get_name()}(${sargs})${propagate_suffix}'
			}
			if x.name.contains('__static__') {
				return '${x.mod}.${x.get_name()}(${sargs})${propagate_suffix}'
			}
			return '${x.mod}.${x.get_name()}(${sargs})${propagate_suffix}'
		}
		CharLiteral {
			return '`${x.val}`'
		}
		Comment {
			if x.is_multi {
				lines := x.text.split_into_lines()
				return '/* ${lines.len} lines comment */'
			} else {
				text := x.text.trim('\x01').trim_space()
				return '´// ${text}´'
			}
		}
		ComptimeSelector {
			return '${x.left}.$(${x.field_expr})'
		}
		ConcatExpr {
			return x.vals.map(it.str()).join(',')
		}
		EnumVal {
			return '.${x.val}'
		}
		FloatLiteral, IntegerLiteral {
			return x.val.clone()
		}
		GoExpr {
			return 'go ${x.call_expr}'
		}
		SpawnExpr {
			return 'spawn ${x.call_expr}'
		}
		Ident {
			return x.name.clone()
		}
		IfExpr {
			mut parts := []string{}
			dollar := if x.is_comptime { '$' } else { '' }
			for i, branch in x.branches {
				if i != 0 {
					parts << ' } ${dollar}else '
				}
				if i < x.branches.len - 1 || !x.has_else {
					parts << ' ${dollar}if ' + branch.cond.str() + ' { '
				} else if x.has_else && i == x.branches.len - 1 {
					parts << '{ '
				}
				for stmt in branch.stmts {
					parts << stmt.str()
				}
			}
			parts << ' }'
			return parts.join('')
		}
		IndexExpr {
			return '${x.left.str()}[${x.index.str()}]'
		}
		InfixExpr {
			return '${x.left.str()} ${x.op.str()} ${x.right.str()}'
		}
		MapInit {
			mut pairs := []string{}
			for ik, kv in x.keys {
				mv := x.vals[ik].str()
				pairs << '${kv}: ${mv}'
			}
			return 'map{ ${pairs.join(' ')} }'
		}
		Nil {
			return 'nil'
		}
		ParExpr {
			return '(${x.expr})'
		}
		PostfixExpr {
			if x.op == .question {
				return '${x.expr} ?'
			}
			return '${x.expr}${x.op}'
		}
		PrefixExpr {
			return x.op.str() + x.right.str()
		}
		RangeExpr {
			mut s := '..'
			if x.has_low {
				s = '${x.low} ' + s
			}
			if x.has_high {
				s = s + ' ${x.high}'
			}
			return s
		}
		SelectExpr {
			return 'ast.SelectExpr'
		}
		SelectorExpr {
			propagate_suffix := if x.or_block.kind == .propagate_option {
				'?'
			} else if x.or_block.kind == .propagate_result {
				'!'
			} else {
				''
			}
			return '${x.expr.str()}.${x.field_name}${propagate_suffix}'
		}
		SizeOf {
			if x.is_type {
				return 'sizeof(${global_table.type_to_str(x.typ)})'
			}
			return 'sizeof(${x.expr.str()})'
		}
		OffsetOf {
			return '__offsetof(${global_table.type_to_str(x.struct_type)}, ${x.field})'
		}
		StringInterLiteral {
			mut res := strings.new_builder(50)
			res.write_string("'")
			for i, val in x.vals {
				res.write_string(val)
				if i >= x.exprs.len {
					break
				}
				res.write_string('$')
				fspec_str, needs_braces := x.get_fspec_braces(i)
				if needs_braces {
					res.write_string('{')
					res.write_string(x.exprs[i].str())
					res.write_string(fspec_str)
					res.write_string('}')
				} else {
					res.write_string(x.exprs[i].str())
				}
			}
			res.write_string("'")
			return res.str()
		}
		StringLiteral {
			return "'${x.val}'"
		}
		TypeNode {
			return 'TypeNode(${global_table.type_str(x.typ)})'
		}
		TypeOf {
			if x.is_type {
				return 'typeof[${global_table.type_to_str(x.typ)}]()'
			}
			return 'typeof(${x.expr.str()})'
		}
		Likely {
			return '_likely_(${x.expr.str()})'
		}
		UnsafeExpr {
			return 'unsafe { ${x.expr} }'
		}
		None {
			return 'none'
		}
		IsRefType {
			if x.is_type {
				return 'isreftype(${global_table.type_to_str(x.typ)})'
			}
			return 'isreftype(${x.expr.str()})'
		}
		IfGuardExpr {
			mut s := ''
			for i, var in x.vars {
				s += var.name
				if i != x.vars.len - 1 {
					s += ', '
				}
			}
			return s + ' := ' + x.expr.str()
		}
		StructInit {
			sname := global_table.sym(x.typ).name
			return '${sname}{....}'
		}
		ArrayDecompose {
			return 'ast.ArrayDecompose'
		}
		Assoc {
			return 'ast.Assoc'
		}
		ChanInit {
			return 'ast.ChanInit'
		}
		ComptimeCall {
			return 'ast.ComptimeCall'
		}
		EmptyExpr {
			return 'ast.EmptyExpr'
		}
		LockExpr {
			return 'ast.LockExpr'
		}
		MatchExpr {
			return 'ast.MatchExpr'
		}
		NodeError {
			return 'ast.NodeError'
		}
		OrExpr {
			return 'ast.OrExpr'
		}
		SqlExpr {
			return 'ast.SqlExpr'
		}
	}
	return '[unhandled expr type ${x.type_name()}]'
}

pub fn (a CallArg) str() string {
	if a.is_mut {
		return 'mut ${a.expr.str()}'
	}
	return '${a.expr.str()}'
}

pub fn args2str(args []CallArg) string {
	mut res := []string{}
	for a in args {
		res << a.str()
	}
	return res.join(', ')
}

pub fn (node &BranchStmt) str() string {
	mut s := '${node.kind}'
	if node.label.len > 0 {
		s += ' ${node.label}'
	}
	return s
}

pub fn (node Stmt) str() string {
	match node {
		AssertStmt {
			return 'assert ${node.expr}'
		}
		AssignStmt {
			mut out := ''
			for i, left in node.left {
				if left is Ident {
					var_info := left.var_info()
					if var_info.is_mut {
						out += 'mut '
					}
				}
				out += left.str()
				if i < node.left.len - 1 {
					out += ','
				}
			}
			out += ' ${node.op.str()} '
			for i, val in node.right {
				out += val.str()
				if i < node.right.len - 1 {
					out += ','
				}
			}
			return out
		}
		BranchStmt {
			return node.str()
		}
		ConstDecl {
			fields := node.fields.map(field_to_string)
			return 'const (${fields.join(' ')})'
		}
		ExprStmt {
			return node.expr.str()
		}
		FnDecl {
			return 'fn ${node.name}( ${node.params.len} params ) { ${node.stmts.len} stmts }'
		}
		EnumDecl {
			return 'enum ${node.name} { ${node.fields.len} fields }'
		}
		ForStmt {
			if node.is_inf {
				return 'for {'
			}
			return 'for ${node.cond} {'
		}
		Module {
			return 'module ${node.name}'
		}
		Import {
			mut out := 'import ${node.mod}'
			if node.alias.len > 0 {
				out += ' as ${node.alias}'
			}
			return out
		}
		Return {
			mut out := 'return'
			for i, val in node.exprs {
				out += ' ${val}'
				if i < node.exprs.len - 1 {
					out += ','
				}
			}
			return out
		}
		StructDecl {
			return 'struct ${node.name} { ${node.fields.len} fields }'
		}
		else {
			return '[unhandled stmt str type: ${node.type_name()} ]'
		}
	}
}

fn field_to_string(f ConstField) string {
	x := f.name.trim_string_left(f.mod + '.')
	return '${x} = ${f.expr}'
}

pub fn (e ComptimeForKind) str() string {
	match e {
		.methods { return 'methods' }
		.fields { return 'fields' }
		.attributes { return 'attributes' }
		.values { return 'values' }
	}
}
