// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import v.ast
import v.util
import v.token

// return true if file being parsed imports `mod`
fn (p &Parser) known_import(mod string) bool {
	return mod in p.imports
}

fn (p &Parser) prepend_mod(name string) string {
	// println('prepend_mod() name=$name p.mod=$p.mod expr_mod=$p.expr_mod')
	if p.expr_mod != '' {
		return p.expr_mod + '.' + name
	}
	if p.builtin_mod {
		return name
	}
	return p.mod + '.' + name
}

fn (p &Parser) is_used_import(alias string) bool {
	return alias in p.used_imports
}

fn (mut p Parser) register_used_import(alias string) {
	if !p.is_used_import(alias) {
		p.used_imports << alias
	}
}

fn (mut p Parser) register_used_import_for_symbol_name(sym_name string) {
	short_import_name := sym_name.all_before_last('.').all_after_last('.')
	for alias, mod in p.imports {
		if mod == short_import_name {
			p.register_used_import(alias)
			return
		}
	}
	p.register_used_import(short_import_name)
}

fn (mut p Parser) register_auto_import(alias string) {
	if p.mod == alias {
		return
	}
	if alias !in p.imports {
		p.imports[alias] = alias
		p.table.imports << alias
		node := ast.Import{
			source_name: alias
			pos:         p.tok.pos()
			mod:         alias
			alias:       alias
		}
		p.ast_imports << node
	}
	if alias !in p.auto_imports {
		p.auto_imports << alias
	}
	p.register_used_import(alias)
}

fn (mut p Parser) check_unused_imports() {
	if p.pref.is_repl || p.pref.is_fmt {
		// The REPL should be much more liberal, and should not warn about
		// unused imports, because they probably will be in the next few lines...
		// vfmt doesn't care about unused imports either
		return
	}
	for import_m in p.ast_imports {
		alias := import_m.alias
		mod := import_m.mod
		if !(alias.len == 1 && alias[0] == `_`) && !p.is_used_import(alias) {
			mod_alias := if alias == mod { alias } else { '${alias} (${mod})' }
			p.warn_with_pos("module '${mod_alias}' is imported but never used", import_m.mod_pos)
		}
	}
}

fn (mut p Parser) module_decl() ast.Module {
	mut module_attrs := []ast.Attr{}
	mut attrs_pos := p.tok.pos()
	for p.tok.kind == .lsbr || p.tok.kind == .at {
		p.attributes()
	}
	module_attrs << p.attrs
	mut name := 'main'
	mut module_pos := token.Pos{}
	mut name_pos := token.Pos{}
	mut mod_node := ast.Module{}
	is_skipped := p.tok.kind != .key_module
	if is_skipped {
		// the attributes were for something else != module, like a struct/fn/type etc.
		module_attrs = []
	} else {
		p.attrs = []
		module_pos = p.tok.pos()
		p.next()
		name_pos = p.tok.pos()
		name = p.check_name()
		mod_node = ast.Module{
			pos: module_pos
		}
		if module_pos.line_nr != name_pos.line_nr {
			p.error_with_pos('`module` and `${name}` must be at same line', name_pos)
			return mod_node
		}
		// Note: this shouldn't be reassigned into name_pos
		// as it creates a wrong position when extended
		// to module_pos
		n_pos := p.tok.pos()
		if module_pos.line_nr == n_pos.line_nr && p.tok.kind !in [.comment, .eof, .semicolon] {
			if p.tok.kind == .name {
				p.unexpected_with_pos(n_pos,
					prepend_msg: '`module ${name}`, you can only declare one module,'
					got:         '`${p.tok.lit}`'
				)
				return mod_node
			} else {
				p.unexpected_with_pos(n_pos,
					prepend_msg: '`module ${name}`,'
					got:         '`${p.tok.kind}` after module name'
				)
				return mod_node
			}
		}
		module_pos = attrs_pos.extend(name_pos)
	}
	full_name := util.qualify_module(p.pref, name, p.file_path)
	p.mod = full_name
	p.builtin_mod = p.mod == 'builtin'
	mod_node = ast.Module{
		name:       full_name
		short_name: name
		attrs:      module_attrs
		is_skipped: is_skipped
		pos:        module_pos
		name_pos:   name_pos
	}
	if p.tok.kind == .semicolon {
		p.check(.semicolon)
	}
	if !is_skipped {
		p.table.module_attrs[p.mod] = module_attrs
		for ma in module_attrs {
			match ma.name {
				'deprecated', 'deprecated_after' {
					p.table.module_deprecated[p.mod] = true
				}
				'manualfree' {
					p.is_manualfree = true
				}
				'generated' {
					p.is_generated = true
				}
				'has_globals' {
					p.has_globals = true
				}
				'translated' {
					p.is_translated = true
				}
				'wasm_import_namespace' {
					if !p.pref.is_fmt && p.pref.backend != .wasm {
						p.note_with_pos('@[wasm_import_namespace] is only supported by the wasm backend',
							ma.pos)
					}
				}
				else {
					p.error_with_pos('unknown module attribute `[${ma.name}]`', ma.pos)
					return mod_node
				}
			}
		}
	}
	return mod_node
}

fn (mut p Parser) import_stmt() ast.Import {
	import_pos := p.tok.pos()
	p.check(.key_import)
	mut pos := p.tok.pos()
	mut import_node := ast.Import{
		pos: import_pos.extend(pos)
	}
	if p.tok.kind == .lpar {
		p.error_with_pos('`import()` has been deprecated, use `import x` instead', pos)
		return import_node
	}
	mut source_name := p.check_name()
	if source_name == '' {
		p.error_with_pos('import name can not be empty', pos)
		return import_node
	}
	mut mod_name_arr := []string{}
	mod_name_arr << source_name
	if import_pos.line_nr != pos.line_nr {
		p.error_with_pos('`import` statements must be a single line', pos)
		return import_node
	}
	mut mod_alias := mod_name_arr[0]
	import_node = ast.Import{
		source_name: source_name
		pos:         import_pos.extend(pos)
		mod_pos:     pos
		alias_pos:   pos
	}
	for p.tok.kind == .dot {
		p.next()
		submod_pos := p.tok.pos()
		if p.tok.kind != .name {
			p.error_with_pos('module syntax error, please use `x.y.z`', submod_pos)
			return import_node
		}
		if import_pos.line_nr != submod_pos.line_nr {
			p.error_with_pos('`import` and `submodule` must be at same line', submod_pos)
			return import_node
		}
		submod_name := p.check_name()
		mod_name_arr << submod_name
		mod_alias = submod_name
		pos = pos.extend(submod_pos)
		source_name = mod_name_arr.join('.')
		import_node = ast.Import{
			source_name: source_name
			pos:         import_pos.extend(pos)
			mod_pos:     pos
			alias_pos:   submod_pos
			mod:         util.qualify_import(p.pref, source_name, p.file_path)
			alias:       mod_alias
		}
	}
	if mod_name_arr.len == 1 {
		import_node = ast.Import{
			source_name: source_name
			pos:         import_node.pos
			mod_pos:     import_node.mod_pos
			alias_pos:   import_node.alias_pos
			mod:         util.qualify_import(p.pref, mod_name_arr[0], p.file_path)
			alias:       mod_alias
		}
	}
	mod_name := import_node.mod
	if p.tok.kind == .key_as {
		p.next()
		alias_pos := p.tok.pos()
		mod_alias = p.check_name()
		if mod_alias == mod_name_arr.last() {
			p.error_with_pos('import alias `${mod_name} as ${mod_alias}` is redundant',
				p.prev_tok.pos())
			return import_node
		}
		import_node = ast.Import{
			source_name: source_name
			pos:         import_node.pos.extend(alias_pos)
			mod_pos:     import_node.mod_pos
			alias_pos:   alias_pos
			mod:         import_node.mod
			alias:       mod_alias
		}
	}
	if p.tok.kind == .lcbr { // import module { fn1, Type2 } syntax
		mut initial_syms_pos := p.tok.pos()
		p.import_syms(mut import_node)
		initial_syms_pos = initial_syms_pos.extend(p.tok.pos())
		import_node = ast.Import{
			...import_node
			source_name: source_name
			syms_pos:    initial_syms_pos
			pos:         import_node.pos.extend(initial_syms_pos)
		}
	}
	pos_t := p.tok.pos()
	if import_pos.line_nr == pos_t.line_nr {
		if p.tok.kind !in [.lcbr, .eof, .comment, .semicolon, .key_import] {
			p.error_with_pos('cannot import multiple modules at a time', pos_t)
			return import_node
		}
	}
	import_node.comments = p.eat_comments(same_line: true)
	import_node.next_comments = p.eat_comments(follow_up: true)
	p.imports[mod_alias] = mod_name
	if p.tok.kind == .semicolon {
		p.check(.semicolon)
	}
	// if mod_name !in p.table.imports {
	p.table.imports << mod_name
	p.ast_imports << import_node
	// }
	return import_node
}

// import_syms parses the inner part of `import module { submod1, submod2 }`
fn (mut p Parser) import_syms(mut parent ast.Import) {
	p.next()
	pos_t := p.tok.pos()
	if p.tok.kind == .rcbr { // closed too early
		p.error_with_pos('empty `${parent.mod}` import set, remove `{}`', pos_t)
		return
	}
	if p.tok.kind != .name { // not a valid inner name
		p.error_with_pos('import syntax error, please specify a valid fn or type name',
			pos_t)
		return
	}
	for p.tok.kind == .name {
		pos := p.tok.pos()
		alias := p.check_name()
		p.imported_symbols[alias] = parent.mod + '.' + alias
		// so we can work with this in fmt+checker
		parent.syms << ast.ImportSymbol{
			pos:  pos
			name: alias
		}
		if p.tok.kind == .comma { // go again if more than one
			p.next()
			continue
		}
		if p.tok.kind == .rcbr { // finish if closing `}` is seen
			break
		}
	}
	if p.tok.kind != .rcbr {
		p.error_with_pos('import syntax error, no closing `}`', p.tok.pos())
		return
	}
	p.next()
}
