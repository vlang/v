module builder

import os
import v2.ast
import v2.gen.v
import v2.parser
import v2.types

const core_cached_module_paths = [
	'builtin',
	'strconv',
	'strings',
	'hash',
	'math.bits',
	'os',
	'time',
	'term',
	'term.termios',
	'os.cmdline',
	'encoding.binary',
	'crypto.sha256',
	'strings.textscanner',
]

const core_cached_module_names = [
	'builtin',
	'strconv',
	'strings',
	'hash',
	'bits',
	'os',
	'time',
	'term',
	'termios',
	'cmdline',
	'binary',
	'sha256',
	'textscanner',
]

const builtin_cache_name = 'builtin'

const builtin_cached_module_paths = ['builtin', 'strconv', 'strings', 'hash', 'math.bits']

const builtin_cached_module_names = ['builtin', 'strconv', 'strings', 'hash', 'bits']

const vlib_cache_name = 'vlib'

const vlib_cached_module_paths = [
	'os',
	'time',
	'term',
	'term.termios',
	'os.cmdline',
	'encoding.binary',
	'crypto.sha256',
	'strings.textscanner',
]

const vlib_cached_module_names = [
	'os',
	'time',
	'term',
	'termios',
	'cmdline',
	'binary',
	'sha256',
	'textscanner',
]

const core_cache_format = 'cc6'

const core_headers_format = 'vh44'

fn (b &Builder) core_cache_dir() string {
	return os.join_path(os.temp_dir(), 'v2_cleanc_obj_cache')
}

fn (b &Builder) core_cache_obj_path() string {
	return os.join_path(b.core_cache_dir(), '${builtin_cache_name}.o')
}

fn (b &Builder) core_cache_stamp_path() string {
	return os.join_path(b.core_cache_dir(), '${builtin_cache_name}.stamp')
}

fn (b &Builder) core_headers_stamp_path() string {
	return os.join_path(b.core_cache_dir(), 'cached_modules.vh.stamp')
}

fn (b &Builder) core_header_path(module_name string) string {
	return os.join_path(b.core_cache_dir(), '${module_name}.vh')
}

fn (b &Builder) core_header_paths() []string {
	mut paths := []string{cap: core_cached_module_names.len}
	for module_name in core_cached_module_names {
		paths << b.core_header_path(module_name)
	}
	return paths
}

fn (b &Builder) core_cached_parse_paths() []string {
	return b.core_header_paths()
}

fn (b &Builder) use_builtin_header_for_parse() bool {
	for file in b.user_files {
		if os.file_name(file) == 'v2.v' {
			return false
		}
	}
	return true
}

fn (b &Builder) module_source_files(modules []string) []string {
	mut files_set := map[string]bool{}
	for module_name in modules {
		module_path := b.pref.get_vlib_module_path(module_name)
		module_files := get_v_files_from_dir(module_path)
		for file in module_files {
			files_set[file] = true
		}
	}
	mut files := files_set.keys()
	files.sort()
	return files
}

fn (b &Builder) cache_stamp_for_modules(cache_name string, modules []string, cc string, cc_flags string) string {
	source_files := b.module_source_files(modules)
	mut lines := []string{cap: source_files.len + 10}
	lines << 'cache=${cache_name}'
	lines << 'format=${core_cache_format}'
	lines << 'cc=${cc}'
	lines << 'cc_flags=${cc_flags}'
	lines << 'context_alloc=${b.pref.use_context_allocator}'
	for file in source_files {
		lines << '${file}:${os.file_last_mod_unix(file)}'
	}
	return lines.join('\n')
}

fn (b &Builder) core_cache_context_stamp() string {
	mut files := b.user_files.clone()
	files.sort()
	mut lines := []string{cap: files.len + 1}
	for file in files {
		norm_file := os.norm_path(file)
		lines << '${norm_file}:${os.file_last_mod_unix(norm_file)}'
	}
	lines << 'entry_count=${files.len}'
	return lines.join('|')
}

fn (b &Builder) header_stamp_for_modules(modules []string) string {
	source_files := b.module_source_files(modules)
	mut lines := []string{cap: source_files.len + 4}
	lines << 'format=${core_headers_format}'
	for file in source_files {
		lines << '${file}:${os.file_last_mod_unix(file)}'
	}
	return lines.join('\n')
}

fn (b &Builder) can_use_cached_core_headers() bool {
	if b.pref.no_cache || b.pref.skip_builtin {
		return false
	}
	cc := os.getenv_opt('V2CC') or { 'cc' }
	cc_flags := os.getenv_opt('V2CFLAGS') or { '' }
	if !b.can_use_cached_module_bundle(builtin_cache_name, builtin_cached_module_paths,
		cc, cc_flags) {
		return false
	}
	if vlib_cached_module_paths.len > 0
		&& !b.can_use_cached_module_bundle(vlib_cache_name, vlib_cached_module_paths, cc, cc_flags) {
		return false
	}
	expected_header_stamp := b.header_stamp_for_modules(core_cached_module_paths)
	current_header_stamp := os.read_file(b.core_headers_stamp_path()) or { return false }
	if current_header_stamp != expected_header_stamp {
		return false
	}
	for header_path in b.core_header_paths() {
		if !os.exists(header_path) {
			return false
		}
	}
	return true
}

fn (b &Builder) can_use_cached_module_bundle(cache_name string, module_paths []string, cc string, cc_flags string) bool {
	obj_file := cache_name + '.o'
	stamp_file := cache_name + '.stamp'
	obj_path := os.join_path(b.core_cache_dir(), obj_file)
	stamp_path := os.join_path(b.core_cache_dir(), stamp_file)
	if !os.exists(obj_path) || !os.exists(stamp_path) {
		return false
	}
	expected_cache_stamp := b.cache_stamp_for_modules(cache_name, module_paths, cc, cc_flags)
	current_cache_stamp := os.read_file(stamp_path) or { return false }
	return current_cache_stamp == expected_cache_stamp
}

fn (mut b Builder) ensure_core_module_headers() {
	cache_dir := b.core_cache_dir()
	os.mkdir_all(cache_dir) or { return }
	expected_stamp := b.header_stamp_for_modules(core_cached_module_paths)
	mut has_headers := true
	for header_path in b.core_header_paths() {
		if !os.exists(header_path) {
			has_headers = false
			break
		}
	}
	mut needs_regen := !has_headers
	if has_headers {
		current_stamp := os.read_file(b.core_headers_stamp_path()) or { '' }
		if current_stamp != expected_stamp {
			needs_regen = true
		}
	}
	if !needs_regen {
		return
	}
	header_source_files := b.parse_module_source_files_for_headers(core_cached_module_paths)
	source_fn_returns := b.source_fn_return_types(core_cached_module_paths)
	for module_name in core_cached_module_names {
		header_ast := b.build_module_header_ast(header_source_files, module_name) or { return }
		mut gen := v.new_gen(b.pref)
		gen.gen(header_ast)
		mut header_source := sanitize_header_source(gen.output_string(), source_fn_returns)
		source_fn_decls := b.source_fn_decls_for_module(module_name)
		header_source = merge_missing_source_fn_decls(header_source, source_fn_decls)
		source_struct_fields := b.source_struct_field_types_for_module(module_name)
		header_source = repair_missing_struct_field_types(header_source, source_struct_fields)
		header_source = ensure_ierror_interface_methods(header_source)
		if header_source.len == 0 {
			return
		}
		if !header_source.ends_with('\n') {
			header_source += '\n'
		}
		os.write_file(b.core_header_path(module_name), header_source) or { return }
	}
	os.write_file(b.core_headers_stamp_path(), expected_stamp) or {}
}

fn (b &Builder) source_fn_return_types(modules []string) map[string]string {
	mut fn_returns := map[string]string{}
	for module_name in modules {
		module_dir := b.pref.get_vlib_module_path(module_name)
		for file in get_v_files_from_dir(module_dir) {
			lines := os.read_lines(file) or { continue }
			for raw_line in lines {
				line := raw_line.trim_space()
				info := parse_fn_signature_and_return(line) or { continue }
				if info.return_type.len > 0 {
					fn_returns[info.signature] = info.return_type
				}
			}
		}
	}
	return fn_returns
}

fn (b &Builder) source_fn_decls_for_module(module_name string) map[string]string {
	module_path := b.module_name_to_path(module_name)
	module_dir := b.pref.get_vlib_module_path(module_path)
	mut decls := map[string]string{}
	for file in get_v_files_from_dir(module_dir) {
		lines := os.read_lines(file) or { continue }
		mut in_interface := false
		mut interface_name := ''
		mut interface_is_public := false
		for raw_line in lines {
			line := raw_line.trim_space()
			if in_interface {
				if line.starts_with('}') {
					in_interface = false
					interface_name = ''
					interface_is_public = false
					continue
				}
				if line.len == 0 || line.starts_with('//') {
					continue
				}
				open_idx := line.index('(') or { continue }
				close_idx := header_find_matching_paren(line, open_idx) or { continue }
				method_name := line[..open_idx].trim_space()
				if method_name.len == 0 {
					continue
				}
				params := line[open_idx..close_idx + 1]
				mut ret := line[close_idx + 1..].trim_space()
				if comment_idx := ret.index('//') {
					ret = ret[..comment_idx].trim_space()
				}
				vis := if interface_is_public { 'pub fn' } else { 'fn' }
				mut decl_line := '${vis} (it ${interface_name}) ${method_name}${params}'
				if ret.len > 0 {
					decl_line += ' ${ret}'
				}
				info := parse_fn_signature_and_return(decl_line) or { continue }
				decls[info.signature] = decl_line
				continue
			}
			if line.starts_with('pub interface ') || line.starts_with('interface ') {
				if !line.ends_with('{') {
					continue
				}
				mut body := line
				interface_is_public = body.starts_with('pub interface ')
				if interface_is_public {
					body = body[4..].trim_space()
				}
				if !body.starts_with('interface ') {
					continue
				}
				rest := body['interface '.len..]
				interface_name = rest.all_before('{').trim_space()
				if interface_name.len == 0 {
					continue
				}
				in_interface = true
				continue
			}
			info := parse_fn_signature_and_return(line) or { continue }
			mut decl_line := info.signature
			if info.return_type.len > 0 {
				decl_line += ' ${info.return_type}'
			}
			decls[info.signature] = decl_line
		}
	}
	return decls
}

fn (b &Builder) source_struct_field_types_for_module(module_name string) map[string]string {
	module_path := b.module_name_to_path(module_name)
	module_dir := b.pref.get_vlib_module_path(module_path)
	mut field_types := map[string]string{}
	for file in get_v_files_from_dir(module_dir) {
		lines := os.read_lines(file) or { continue }
		mut in_struct := false
		mut struct_name := ''
		for raw_line in lines {
			mut line := raw_line.trim_space()
			if !in_struct {
				if sname := header_struct_block_name(line) {
					struct_name = sname
					in_struct = true
				}
				continue
			}
			if line.starts_with('}') {
				in_struct = false
				struct_name = ''
				continue
			}
			if line.len == 0 || line.starts_with('//') || line.starts_with('[') || line == 'mut:'
				|| line == 'pub:' || line == 'pub mut:' {
				continue
			}
			if comment_idx := line.index('//') {
				line = line[..comment_idx].trim_space()
			}
			if line.len == 0 {
				continue
			}
			mut lhs := line
			if eq_idx := line.index('=') {
				lhs = line[..eq_idx].trim_space()
			}
			tokens := lhs.split(' ').filter(it.len > 0)
			if tokens.len < 2 {
				continue
			}
			field_name := tokens[0]
			field_type := tokens[1..].join(' ')
			field_types['${struct_name}.${field_name}'] = field_type
		}
	}
	return field_types
}

fn (mut b Builder) parse_module_source_files_for_headers(modules []string) []ast.File {
	mut parser_reused := parser.Parser.new(b.pref)
	mut source_paths := []string{}
	for module_name in modules {
		source_paths << get_v_files_from_dir(b.pref.get_vlib_module_path(module_name))
	}
	return parser_reused.parse_files(source_paths, mut b.file_set)
}

fn (b &Builder) build_module_header_ast(source_files []ast.File, module_name string) ?ast.File {
	mut found_module := false
	mut module_stmt := ast.ModuleStmt{
		name: module_name
	}
	mut import_stmts := []ast.ImportStmt{}
	mut import_seen := map[string]bool{}
	mut enum_stmts := []ast.Stmt{}
	mut type_decl_stmts := []ast.Stmt{}
	mut type_decl_seen := map[string]bool{}
	mut decl_stmts := []ast.Stmt{}
	for file in source_files {
		if file_module_name(file) != module_name {
			continue
		}
		for stmt in file.stmts {
			match stmt {
				ast.ModuleStmt {
					if !found_module {
						module_stmt = stmt
						found_module = true
					}
				}
				ast.ImportStmt {
					key := import_stmt_cache_key(stmt)
					if key !in import_seen {
						import_seen[key] = true
						import_stmts << stmt
					}
				}
				ast.StructDecl {
					mut sfields := []ast.FieldDecl{cap: stmt.fields.len}
					for field in stmt.fields {
						mut field_typ := field.typ
						if field_typ is ast.EmptyExpr {
							if inferred_typ := infer_const_type_expr(field.value) {
								field_typ = inferred_typ
							} else {
								continue
							}
						}
						sfields << ast.FieldDecl{
							name:       field.name
							typ:        field_typ
							value:      field.value
							attributes: field.attributes
						}
					}
					decl_stmts << ast.Stmt(ast.StructDecl{
						is_public:      stmt.is_public
						is_union:       stmt.is_union
						embedded:       stmt.embedded
						language:       stmt.language
						name:           stmt.name
						generic_params: stmt.generic_params
						fields:         sfields
						pos:            stmt.pos
					})
				}
				ast.ConstDecl {
					mut fields := []ast.FieldInit{}
					for field in stmt.fields {
						if compact_value := b.header_const_type_expr(module_name, field) {
							fields << ast.FieldInit{
								name:  field.name
								value: compact_value
							}
						}
					}
					if fields.len > 0 {
						decl_stmts << ast.Stmt(ast.ConstDecl{
							is_public: stmt.is_public
							fields:    fields
						})
					}
				}
				ast.EnumDecl {
					enum_stmts << ast.Stmt(stmt)
				}
				ast.TypeDecl {
					type_decl := b.resolved_header_type_decl(module_name, stmt) or { continue }
					type_decl_stmts << ast.Stmt(type_decl)
					type_decl_seen[type_decl.name] = true
				}
				ast.InterfaceDecl {
					decl_stmts << ast.Stmt(b.resolved_header_interface_decl(module_name,
						stmt))
				}
				ast.GlobalDecl {
					mut gfields := []ast.FieldDecl{cap: stmt.fields.len}
					for field in stmt.fields {
						mut global_typ := field.typ
						if global_typ is ast.EmptyExpr {
							if inferred_typ := infer_const_type_expr(field.value) {
								global_typ = inferred_typ
							} else {
								continue
							}
						}
						if !header_type_expr_is_usable(global_typ) {
							continue
						}
						gfields << ast.FieldDecl{
							name:       field.name
							typ:        global_typ
							attributes: field.attributes
						}
					}
					if gfields.len > 0 {
						decl_stmts << ast.Stmt(ast.GlobalDecl{
							attributes: stmt.attributes
							fields:     gfields
						})
					}
				}
				ast.FnDecl {
					resolved_fn := b.resolved_header_fn_decl(module_name, stmt)
					if !header_fn_decl_is_usable(resolved_fn) {
						continue
					}
					fn_decl := ast.FnDecl{
						attributes: []ast.Attribute{}
						is_public:  resolved_fn.is_public
						is_method:  resolved_fn.is_method
						is_static:  resolved_fn.is_static
						receiver:   resolved_fn.receiver
						language:   resolved_fn.language
						name:       resolved_fn.name
						typ:        resolved_fn.typ
						stmts:      []ast.Stmt{}
						pos:        resolved_fn.pos
					}
					decl_stmts << ast.Stmt(fn_decl)
				}
				else {}
			}
		}
	}
	b.append_source_type_alias_decls(module_name, mut type_decl_stmts, mut type_decl_seen)
	if !found_module || enum_stmts.len + type_decl_stmts.len + decl_stmts.len == 0 {
		return none
	}
	mut header_stmts := []ast.Stmt{cap: 1 + import_stmts.len + enum_stmts.len +
		type_decl_stmts.len + decl_stmts.len}
	header_stmts << ast.Stmt(module_stmt)
	for import_stmt in import_stmts {
		header_stmts << ast.Stmt(import_stmt)
	}
	header_stmts << enum_stmts
	header_stmts << type_decl_stmts
	header_stmts << decl_stmts
	return ast.File{
		mod:     module_stmt.name
		name:    '${module_name}.vh'
		stmts:   header_stmts
		imports: import_stmts
	}
}

fn (b &Builder) resolved_header_interface_decl(module_name string, stmt ast.InterfaceDecl) ast.InterfaceDecl {
	if stmt.fields.len > 0 {
		return stmt
	}
	if module_name == 'builtin' && stmt.name == 'IError' {
		msg_type := ast.Expr(ast.Type(ast.FnType{
			params:      []ast.Parameter{}
			return_type: type_name_to_ast_expr('string')
		}))
		code_type := ast.Expr(ast.Type(ast.FnType{
			params:      []ast.Parameter{}
			return_type: type_name_to_ast_expr('int')
		}))
		return ast.InterfaceDecl{
			is_public:      stmt.is_public
			attributes:     stmt.attributes
			name:           stmt.name
			generic_params: stmt.generic_params
			embedded:       stmt.embedded
			fields:         [
				ast.FieldDecl{
					name: 'msg'
					typ:  msg_type
				},
				ast.FieldDecl{
					name: 'code'
					typ:  code_type
				},
			]
		}
	}
	return stmt
}

fn (b &Builder) append_source_type_alias_decls(module_name string, mut type_decl_stmts []ast.Stmt, mut type_decl_seen map[string]bool) {
	module_path := b.module_name_to_path(module_name)
	module_dir := b.pref.get_vlib_module_path(module_path)
	for file in get_v_files_from_dir(module_dir) {
		lines := os.read_lines(file) or { continue }
		for raw_line in lines {
			line := raw_line.trim_space()
			if line.len == 0 || line.starts_with('//') {
				continue
			}
			mut is_public := false
			mut body := line
			if body.starts_with('pub ') {
				is_public = true
				body = body[4..].trim_space()
			}
			if !body.starts_with('type ') || !body.contains('=') {
				continue
			}
			rest := body['type '.len..]
			eq_idx := rest.index('=') or { continue }
			lhs := rest[..eq_idx].trim_space()
			mut lhs_tokens := lhs.split_any(' \t')
			if lhs_tokens.len == 0 {
				continue
			}
			type_name := lhs_tokens[0]
			if type_name.len == 0 || type_name.contains('.') {
				continue
			}
			rhs := rest[eq_idx + 1..].trim_space()
			if rhs.len == 0 {
				continue
			}
			source_decl := ast.TypeDecl{
				is_public: is_public
				language:  .v
				name:      type_name
				base_type: type_name_to_ast_expr(rhs)
			}
			b.set_or_append_type_decl(mut type_decl_stmts, source_decl)
			type_decl_seen[type_name] = true
		}
	}
}

fn (b &Builder) set_or_append_type_decl(mut type_decl_stmts []ast.Stmt, decl ast.TypeDecl) {
	for i, stmt in type_decl_stmts {
		if stmt is ast.TypeDecl && stmt.name == decl.name {
			type_decl_stmts[i] = ast.Stmt(decl)
			return
		}
	}
	type_decl_stmts << ast.Stmt(decl)
}

fn (b &Builder) resolved_header_type_decl(module_name string, stmt ast.TypeDecl) ?ast.TypeDecl {
	if stmt.variants.len > 0 || stmt.base_type !is ast.EmptyExpr {
		return stmt
	}
	base_type_expr := b.lookup_alias_base_type_expr(module_name, stmt.name) or { return none }
	return ast.TypeDecl{
		is_public:      stmt.is_public
		language:       stmt.language
		name:           stmt.name
		generic_params: stmt.generic_params
		base_type:      base_type_expr
		variants:       stmt.variants
	}
}

fn (b &Builder) lookup_alias_base_type_expr(module_name string, type_name string) ?ast.Expr {
	if b.env == unsafe { nil } {
		return b.lookup_alias_source_type_expr(module_name, type_name)
	}
	if scope := b.env.get_scope(module_name) {
		mut mod_scope := unsafe { scope }
		if obj := mod_scope.lookup_parent(type_name, 0) {
			obj_typ := obj.typ()
			if obj_typ is types.Alias {
				return type_name_to_ast_expr(obj_typ.base_type.name())
			}
			obj_type_name := normalize_header_type_name(obj_typ.name())
			if obj_type_name != '' && obj_type_name != type_name {
				return type_name_to_ast_expr(obj_type_name)
			}
		}
	}
	return b.lookup_alias_source_type_expr(module_name, type_name)
}

fn (b &Builder) resolved_header_fn_decl(module_name string, stmt ast.FnDecl) ast.FnDecl {
	mut resolved_typ := stmt.typ
	if b.env != unsafe { nil } {
		if stmt.is_method {
			receiver_type_name := header_receiver_type_name(stmt.receiver.typ)
			if receiver_type_name != '' {
				if fn_typ := b.env.lookup_method(receiver_type_name, stmt.name) {
					resolved_typ = merge_header_fn_type(stmt.typ, types.Type(fn_typ))
				}
			}
		} else if scope := b.env.get_scope(module_name) {
			mut mod_scope := unsafe { scope }
			if obj := mod_scope.lookup_parent(stmt.name, 0) {
				obj_typ := obj.typ()
				if obj_typ is types.FnType {
					resolved_typ = merge_header_fn_type(stmt.typ, obj_typ)
				}
			}
		}
	}
	return ast.FnDecl{
		attributes: stmt.attributes
		is_public:  stmt.is_public
		is_method:  stmt.is_method
		is_static:  stmt.is_static
		receiver:   stmt.receiver
		language:   stmt.language
		name:       stmt.name
		typ:        resolved_typ
		stmts:      stmt.stmts
		pos:        stmt.pos
	}
}

fn header_receiver_type_name(receiver_type_expr ast.Expr) string {
	return match receiver_type_expr {
		ast.ModifierExpr {
			header_receiver_type_name(receiver_type_expr.expr)
		}
		ast.PrefixExpr {
			header_receiver_type_name(receiver_type_expr.expr)
		}
		else {
			receiver_type_expr.name()
		}
	}
}

fn merge_header_fn_type(source_fn ast.FnType, resolved_type types.Type) ast.FnType {
	match resolved_type {
		types.FnType {
			mut merged_return := source_fn.return_type
			if merged_return is ast.EmptyExpr {
				if return_type := resolved_type.get_return_type() {
					merged_return = type_name_to_ast_expr(return_type.name())
				}
			}
			mut merged_params := source_fn.params.clone()
			param_types := resolved_type.get_param_types()
			if param_types.len > 0 {
				limit := if merged_params.len < param_types.len {
					merged_params.len
				} else {
					param_types.len
				}
				for i in 0 .. limit {
					if merged_params[i].typ !is ast.EmptyExpr {
						continue
					}
					param_type := type_name_to_ast_expr(param_types[i].name())
					merged_params[i] = ast.Parameter{
						name:   merged_params[i].name
						typ:    param_type
						is_mut: merged_params[i].is_mut
						pos:    merged_params[i].pos
					}
				}
			}
			return ast.FnType{
				generic_params: source_fn.generic_params
				params:         merged_params
				return_type:    merged_return
			}
		}
		else {
			return source_fn
		}
	}
}

fn type_name_to_ast_expr(type_name string) ast.Expr {
	norm_name := normalize_header_type_name(type_name)
	return ast.Expr(ast.Ident{
		name: norm_name
	})
}

fn normalize_header_type_name(type_name string) string {
	mut name := type_name.trim_space()
	if name.starts_with('tuple ') {
		name = name['tuple '.len..]
	}
	return name
}

fn (b &Builder) module_name_to_path(module_name string) string {
	return match module_name {
		'bits' { 'math.bits' }
		'cmdline' { 'os.cmdline' }
		'binary' { 'encoding.binary' }
		'sha256' { 'crypto.sha256' }
		'textscanner' { 'strings.textscanner' }
		'termios' { 'term.termios' }
		else { module_name }
	}
}

fn (b &Builder) lookup_alias_source_type_expr(module_name string, type_name string) ?ast.Expr {
	module_path := b.module_name_to_path(module_name)
	module_dir := b.pref.get_vlib_module_path(module_path)
	for file in get_v_files_from_dir(module_dir) {
		lines := os.read_lines(file) or { continue }
		for raw_line in lines {
			line := raw_line.trim_space()
			if line.len == 0 || line.starts_with('//') {
				continue
			}
			is_type_decl := line.starts_with('type ${type_name} ')
				|| line.starts_with('pub type ${type_name} ')
			if !is_type_decl || !line.contains('=') {
				continue
			}
			rhs := line.all_after_first('=').trim_space()
			if rhs.len == 0 {
				continue
			}
			return type_name_to_ast_expr(rhs)
		}
	}
	return none
}

fn (b &Builder) header_const_type_expr(module_name string, field ast.FieldInit) ?ast.Expr {
	if header_const_value_is_safe(field.value) {
		return field.value
	}
	if typed_expr := b.lookup_const_type_expr(module_name, field.name) {
		return typed_expr
	}
	return infer_const_type_expr(field.value)
}

fn header_const_value_is_safe(expr ast.Expr) bool {
	return match expr {
		ast.BasicLiteral, ast.StringLiteral, ast.StringInterLiteral, ast.Ident, ast.SelectorExpr {
			true
		}
		ast.ParenExpr {
			header_const_value_is_safe(expr.expr)
		}
		ast.PrefixExpr {
			header_const_value_is_safe(expr.expr)
		}
		ast.CastExpr {
			header_const_value_is_safe(expr.expr)
		}
		ast.ModifierExpr {
			header_const_value_is_safe(expr.expr)
		}
		ast.InfixExpr {
			header_const_value_is_safe(expr.lhs) && header_const_value_is_safe(expr.rhs)
		}
		ast.CallOrCastExpr {
			if !is_type_expr(expr.lhs) {
				return false
			}
			header_const_value_is_safe(expr.expr)
		}
		else {
			false
		}
	}
}

fn (b &Builder) lookup_const_type_expr(module_name string, const_name string) ?ast.Expr {
	if b.env == unsafe { nil } {
		return none
	}
	scope := b.env.get_scope(module_name) or { return none }
	mut mod_scope := unsafe { scope }
	obj := mod_scope.lookup_parent(const_name, 0) or { return none }
	mut type_name := normalize_const_type_name(module_name, obj.typ().name())
	if type_name.len == 0 || !header_type_name_is_sane(type_name) {
		return none
	}
	if !type_name.contains('.') && b.module_defines_c_type(module_name, type_name) {
		type_name = 'C.${type_name}'
	}
	return ast.Expr(ast.Ident{
		name: type_name
	})
}

fn normalize_const_type_name(module_name string, type_name string) string {
	mut name := normalize_header_type_name(type_name)
	if name.len == 0 || !name.contains('__') {
		return name
	}
	if name.starts_with('_option_') || name.starts_with('_result_') || name.starts_with('Array_')
		|| name.starts_with('Map_') {
		return name
	}
	idx := name.index('__') or { return name }
	if idx <= 0 || idx + 2 >= name.len {
		return name
	}
	mod_prefix := name[..idx]
	rest := name[idx + 2..]
	if mod_prefix == module_name {
		return rest
	}
	return '${mod_prefix}.${rest}'
}

fn header_type_name_is_sane(type_name string) bool {
	for ch in type_name {
		if ch.is_space() {
			return false
		}
		if ch in [`+`, `-`, `*`, `/`, `%`, `=`, `,`, `;`, `:`, `(`, `)`, `{`, `}`] {
			return false
		}
	}
	return true
}

fn (b &Builder) module_defines_c_type(module_name string, type_name string) bool {
	module_path := b.module_name_to_path(module_name)
	module_dir := b.pref.get_vlib_module_path(module_path)
	patterns := [
		'struct C.${type_name}',
		'pub struct C.${type_name}',
		'type C.${type_name}',
		'pub type C.${type_name}',
	]
	for file in get_v_files_from_dir(module_dir) {
		content := os.read_file(file) or { continue }
		for pattern in patterns {
			if content.contains(pattern) {
				return true
			}
		}
	}
	return false
}

fn infer_const_type_expr(expr ast.Expr) ?ast.Expr {
	return match expr {
		ast.IfExpr, ast.ComptimeExpr {
			none
		}
		ast.Type {
			ast.Expr(expr)
		}
		ast.StringLiteral {
			ast.Expr(ast.Ident{
				name: 'string'
			})
		}
		ast.StringInterLiteral {
			ast.Expr(ast.Ident{
				name: 'string'
			})
		}
		ast.BasicLiteral {
			match expr.kind {
				.key_true, .key_false {
					ast.Expr(ast.Ident{
						name: 'bool'
					})
				}
				.char {
					ast.Expr(ast.Ident{
						name: 'rune'
					})
				}
				.number {
					mut number_type := 'int'
					if expr.value.contains('.') {
						number_type = 'f64'
					}
					ast.Expr(ast.Ident{
						name: number_type
					})
				}
				else {
					none
				}
			}
		}
		ast.ArrayInitExpr {
			if expr.typ !is ast.EmptyExpr {
				expr.typ
			} else if expr.exprs.len > 0 {
				elem_type := infer_const_type_expr(expr.exprs[0]) or { return none }
				ast.Expr(ast.Type(ast.ArrayType{
					elem_type: elem_type
				}))
			} else {
				none
			}
		}
		ast.MapInitExpr {
			if expr.typ !is ast.EmptyExpr {
				expr.typ
			} else if expr.keys.len > 0 && expr.vals.len > 0 {
				key_expr := infer_const_type_expr(expr.keys[0]) or { return none }
				val_expr := infer_const_type_expr(expr.vals[0]) or { return none }
				ast.Expr(ast.Type(ast.MapType{
					key_type:   key_expr
					value_type: val_expr
				}))
			} else {
				none
			}
		}
		ast.InitExpr {
			expr.typ
		}
		ast.CallOrCastExpr {
			if is_type_expr(expr.lhs) {
				expr.lhs
			} else {
				none
			}
		}
		ast.CastExpr {
			expr.typ
		}
		ast.AsCastExpr {
			expr.typ
		}
		ast.InfixExpr {
			if lhs_expr := infer_const_type_expr(expr.lhs) {
				lhs_expr
			} else {
				infer_const_type_expr(expr.rhs)
			}
		}
		ast.ParenExpr {
			infer_const_type_expr(expr.expr)
		}
		ast.PrefixExpr {
			infer_const_type_expr(expr.expr)
		}
		else {
			none
		}
	}
}

fn is_type_expr(expr ast.Expr) bool {
	return match expr {
		ast.Type, ast.SelectorExpr {
			true
		}
		ast.Ident {
			name := expr.name
				name in ['bool', 'byte', 'char', 'f32', 'f64', 'i8', 'i16', 'i32', 'int', 'i64', 'isize', 'rune', 'string', 'u8', 'u16', 'u32', 'u64', 'usize', 'void', 'voidptr', 'byteptr', 'charptr']
				|| name.starts_with('&') || name.starts_with('[]') || name.starts_with('?')
				|| name.starts_with('!') || name.contains('[') || name.contains('__')
		}
		else {
			false
		}
	}
}

fn import_stmt_cache_key(stmt ast.ImportStmt) string {
	mut key := '${stmt.name}|${stmt.alias}|${stmt.is_aliased}'
	if stmt.symbols.len > 0 {
		mut symbols := []string{cap: stmt.symbols.len}
		for symbol in stmt.symbols {
			symbols << symbol.name()
		}
		key += '|${symbols.join(',')}'
	}
	return key
}

fn header_fn_decl_is_usable(stmt ast.FnDecl) bool {
	if stmt.is_method && !header_type_expr_is_usable(stmt.receiver.typ) {
		return false
	}
	for param in stmt.typ.params {
		if !header_type_expr_is_usable(param.typ) {
			return false
		}
	}
	if stmt.typ.return_type !is ast.EmptyExpr && !header_type_expr_is_usable(stmt.typ.return_type) {
		return false
	}
	return true
}

fn header_type_expr_is_usable(expr ast.Expr) bool {
	return match expr {
		ast.EmptyExpr {
			false
		}
		ast.Ident {
			expr.name.len > 0
		}
		ast.SelectorExpr {
			header_type_expr_is_usable(expr.lhs) && expr.rhs.name.len > 0
		}
		ast.ModifierExpr {
			header_type_expr_is_usable(expr.expr)
		}
		ast.ParenExpr {
			header_type_expr_is_usable(expr.expr)
		}
		ast.PrefixExpr {
			header_type_expr_is_usable(expr.expr)
		}
		ast.IndexExpr {
			header_type_expr_is_usable(expr.lhs) && header_type_expr_is_usable(expr.expr)
		}
		ast.GenericArgs {
			if !header_type_expr_is_usable(expr.lhs) {
				return false
			}
			for arg in expr.args {
				if !header_type_expr_is_usable(arg) {
					return false
				}
			}
			true
		}
		ast.GenericArgOrIndexExpr {
			header_type_expr_is_usable(expr.lhs) && header_type_expr_is_usable(expr.expr)
		}
		ast.Type {
			header_type_node_is_usable(expr)
		}
		else {
			false
		}
	}
}

fn header_type_node_is_usable(node ast.Type) bool {
	return match node {
		ast.ArrayType {
			header_type_expr_is_usable(node.elem_type)
		}
		ast.ArrayFixedType {
			header_type_expr_is_usable(node.elem_type)
		}
		ast.ChannelType {
			header_type_expr_is_usable(node.elem_type)
		}
		ast.FnType {
			for param in node.params {
				if !header_type_expr_is_usable(param.typ) {
					return false
				}
			}
			if node.return_type !is ast.EmptyExpr {
				return header_type_expr_is_usable(node.return_type)
			}
			return true
		}
		ast.GenericType {
			header_type_expr_is_usable(node.name)
		}
		ast.MapType {
			header_type_expr_is_usable(node.key_type) && header_type_expr_is_usable(node.value_type)
		}
		ast.OptionType {
			node.base_type !is ast.EmptyExpr && header_type_expr_is_usable(node.base_type)
		}
		ast.ResultType {
			node.base_type !is ast.EmptyExpr && header_type_expr_is_usable(node.base_type)
		}
		ast.ThreadType {
			node.elem_type is ast.EmptyExpr || header_type_expr_is_usable(node.elem_type)
		}
		ast.TupleType {
			for t in node.types {
				if !header_type_expr_is_usable(t) {
					return false
				}
			}
			true
		}
		ast.AnonStructType {
			true
		}
		ast.NilType, ast.NoneType {
			true
		}
	}
}

struct FnReturnInfo {
	signature   string
	return_type string
}

fn parse_fn_signature_and_return(line string) ?FnReturnInfo {
	mut i := line.index('fn ') or { return none }
	i += 3
	for i < line.len && line[i].is_space() {
		i++
	}
	// Optional method receiver: fn (<receiver>) name(...)
	if i < line.len && line[i] == `(` {
		recv_end := header_find_matching_paren(line, i) or { return none }
		i = recv_end + 1
		for i < line.len && line[i].is_space() {
			i++
		}
	}
	for i < line.len && !line[i].is_space() && line[i] != `(` {
		i++
	}
	for i < line.len && line[i].is_space() {
		i++
	}
	if i >= line.len || line[i] != `(` {
		return none
	}
	params_end := header_find_matching_paren(line, i) or { return none }
	signature := line[..params_end + 1].trim_space()
	mut j := params_end + 1
	for j < line.len && line[j].is_space() {
		j++
	}
	mut return_type := line[j..].trim_space()
	if return_type.ends_with('{') {
		return_type = return_type[..return_type.len - 1].trim_space()
	}
	if return_type.ends_with(';') {
		return_type = return_type[..return_type.len - 1].trim_space()
	}
	return FnReturnInfo{
		signature:   signature
		return_type: return_type
	}
}

fn restore_fn_return_type_from_source(line string, source_fn_returns map[string]string) string {
	trimmed := line.trim_space()
	info := parse_fn_signature_and_return(trimmed) or { return line }
	if info.return_type.len > 0 {
		return line
	}
	return_type := source_fn_returns[info.signature] or { return line }
	mut indent_len := 0
	for indent_len < line.len && line[indent_len].is_space() {
		indent_len++
	}
	indent := line[..indent_len]
	return '${indent}${info.signature} ${return_type}'
}

fn merge_missing_source_fn_decls(header_source string, source_fn_decls map[string]string) string {
	if source_fn_decls.len == 0 || header_source.len == 0 {
		return header_source
	}
	mut existing := map[string]bool{}
	for line in header_source.split_into_lines() {
		info := parse_fn_signature_and_return(line.trim_space()) or { continue }
		existing[info.signature] = true
	}
	mut keys := source_fn_decls.keys()
	keys.sort()
	mut extra := []string{}
	for key in keys {
		if key in existing {
			continue
		}
		extra << source_fn_decls[key]
	}
	if extra.len == 0 {
		return header_source
	}
	mut merged := header_source
	if !merged.ends_with('\n') {
		merged += '\n'
	}
	merged += extra.join('\n')
	merged += '\n'
	return merged
}

fn ensure_ierror_interface_methods(header_source string) string {
	empty_pub := 'pub interface IError {\n}\n'
	full_pub := 'pub interface IError {\n\tmsg fn() string\n\tcode fn() int\n}\n'
	if header_source.contains(empty_pub) {
		return header_source.replace(empty_pub, full_pub)
	}
	empty_plain := 'interface IError {\n}\n'
	full_plain := 'interface IError {\n\tmsg fn() string\n\tcode fn() int\n}\n'
	if header_source.contains(empty_plain) {
		return header_source.replace(empty_plain, full_plain)
	}
	return header_source
}

fn header_struct_block_name(trimmed string) ?string {
	if !trimmed.ends_with('{') {
		return none
	}
	mut body := trimmed
	if body.starts_with('pub ') {
		body = body[4..].trim_space()
	}
	if body.starts_with('struct ') {
		name := body['struct '.len..].all_before('{').trim_space()
		return if name.len > 0 { name } else { none }
	}
	if body.starts_with('union ') {
		name := body['union '.len..].all_before('{').trim_space()
		return if name.len > 0 { name } else { none }
	}
	return none
}

fn leading_ws(line string) string {
	mut i := 0
	for i < line.len && line[i].is_space() {
		i++
	}
	return line[..i]
}

fn repair_missing_struct_field_types(header_source string, source_struct_fields map[string]string) string {
	if header_source.len == 0 || source_struct_fields.len == 0 {
		return header_source
	}
	lines := header_source.split_into_lines()
	mut out := []string{cap: lines.len}
	mut in_struct := false
	mut struct_name := ''
	for raw_line in lines {
		mut line := raw_line
		trimmed := line.trim_space()
		if !in_struct {
			if sname := header_struct_block_name(trimmed) {
				struct_name = sname
				in_struct = true
			}
			out << line
			continue
		}
		if trimmed.starts_with('}') {
			in_struct = false
			struct_name = ''
			out << line
			continue
		}
		if trimmed.len == 0 || trimmed.starts_with('//') || trimmed.starts_with('[')
			|| trimmed == 'mut:' || trimmed == 'pub:' || trimmed == 'pub mut:' {
			out << line
			continue
		}
		mut no_comment := trimmed
		if comment_idx := no_comment.index('//') {
			no_comment = no_comment[..comment_idx].trim_space()
		}
		if no_comment.len == 0 {
			out << line
			continue
		}
		if eq_idx := no_comment.index('=') {
			lhs := no_comment[..eq_idx].trim_space()
			if header_token_count(lhs) == 1 {
				field_name := lhs
				field_type := source_struct_fields['${struct_name}.${field_name}'] or { '' }
				if field_type.len > 0 {
					rhs := no_comment[eq_idx + 1..].trim_space()
					out << '${leading_ws(line)}${field_name} ${field_type} = ${rhs}'
					continue
				}
			}
		}
		out << line
	}
	return out.join('\n')
}

fn sanitize_header_source(source string, source_fn_returns map[string]string) string {
	lines := source.split_into_lines()
	mut out := []string{cap: lines.len}
	mut in_global_block := false
	mut global_start_line := ''
	mut global_body_lines := []string{}
	mut in_type_block := false
	for source_line in lines {
		mut line := source_line
		line = restore_fn_return_type_from_source(line, source_fn_returns)
		trimmed := line.trim_space()
		if !in_global_block {
			if header_starts_type_block(trimmed) {
				in_type_block = true
				out << line
				continue
			}
			if in_type_block {
				if trimmed == '}' {
					in_type_block = false
					out << line
					continue
				}
				if header_type_block_line_is_malformed(trimmed) {
					continue
				}
			}
		}
		if trimmed == '__global (' {
			in_global_block = true
			global_start_line = line
			global_body_lines = []string{}
			continue
		}
		if in_global_block {
			if trimmed == ')' {
				in_global_block = false
				if global_body_lines.len > 0 {
					out << global_start_line
					out << global_body_lines
					out << line
				}
				continue
			}
			if header_token_count(trimmed) < 2 {
				continue
			}
			global_body_lines << line
			continue
		}
		if (trimmed.starts_with('fn ') || trimmed.starts_with('pub fn '))
			&& !header_is_c_fn_decl_line(trimmed) && header_fn_decl_line_is_malformed(trimmed) {
			continue
		}
		if (trimmed.starts_with('type ') || trimmed.starts_with('pub type '))
			&& header_type_decl_line_is_malformed(trimmed) {
			continue
		}
		if (trimmed.starts_with('const ') || trimmed.starts_with('pub const '))
			&& header_const_decl_line_is_malformed(trimmed) {
			continue
		}
		out << line
	}
	return out.join('\n')
}

fn header_starts_type_block(trimmed string) bool {
	if !trimmed.ends_with('{') {
		return false
	}
	return trimmed.starts_with('struct ') || trimmed.starts_with('pub struct ')
		|| trimmed.starts_with('union ') || trimmed.starts_with('pub union ')
		|| trimmed.starts_with('interface ') || trimmed.starts_with('pub interface ')
}

fn header_is_c_fn_decl_line(trimmed string) bool {
	return trimmed.starts_with('fn C.') || trimmed.starts_with('pub fn C.')
}

fn header_type_block_line_is_malformed(trimmed string) bool {
	if trimmed.len == 0 {
		return false
	}
	if trimmed.starts_with('[') || trimmed.starts_with('//') {
		return false
	}
	if (trimmed.starts_with('fn ') || trimmed.starts_with('pub fn '))
		&& header_fn_decl_line_is_malformed(trimmed) {
		return true
	}
	if header_token_count(trimmed) >= 2 {
		return false
	}
	// Single-token lowercase lines in type blocks are almost always fields with missing types.
	token := trimmed.trim_space()
	if token.len == 0 {
		return false
	}
	first := token[0]
	return first >= `a` && first <= `z`
}

fn header_fn_decl_line_is_malformed(line string) bool {
	mut i := line.index('fn ') or { return false }
	i += 3
	for i < line.len && line[i].is_space() {
		i++
	}
	// Optional method receiver: fn (<receiver>) name(...)
	if i < line.len && line[i] == `(` {
		recv_end := header_find_matching_paren(line, i) or { return true }
		receiver := line[i + 1..recv_end].trim_space()
		if receiver.len == 0 {
			return true
		}
		if !header_receiver_decl_is_valid(receiver) {
			return true
		}
		i = recv_end + 1
		for i < line.len && line[i].is_space() {
			i++
		}
	}
	// Function name
	for i < line.len && !line[i].is_space() && line[i] != `(` {
		i++
	}
	for i < line.len && line[i].is_space() {
		i++
	}
	if i >= line.len || line[i] != `(` {
		return true
	}
	params_end := header_find_matching_paren(line, i) or { return true }
	params := line[i + 1..params_end]
	return header_params_are_malformed(params)
}

fn header_receiver_decl_is_valid(receiver string) bool {
	mut text := receiver
	if text.starts_with('mut ') {
		text = text[4..].trim_space()
	}
	if text.starts_with('shared ') {
		text = text[7..].trim_space()
	}
	return header_token_count(text) >= 2
}

fn header_params_are_malformed(params string) bool {
	mut start := 0
	mut depth := 0
	for i, ch in params {
		match ch {
			`(`, `[`, `{` {
				depth++
			}
			`)`, `]`, `}` {
				depth--
			}
			`,` {
				if depth == 0 {
					part := params[start..i].trim_space()
					if part.len > 0 && !header_param_decl_is_valid(part) {
						return true
					}
					start = i + 1
				}
			}
			else {}
		}
	}
	last := params[start..].trim_space()
	if last.len > 0 && !header_param_decl_is_valid(last) {
		return true
	}
	return false
}

fn header_param_decl_is_valid(param string) bool {
	mut text := param
	if text.starts_with('mut ') {
		text = text[4..].trim_space()
	}
	if text.starts_with('shared ') {
		text = text[7..].trim_space()
	}
	if text.starts_with('...') {
		return text.len > 3
	}
	// Header declarations should always carry both parameter name and type.
	return header_token_count(text) >= 2
}

fn header_token_count(text string) int {
	mut count := 0
	for token in text.split_any(' \t') {
		if token.len > 0 {
			count++
		}
	}
	return count
}

fn header_find_matching_paren(text string, start int) ?int {
	if start < 0 || start >= text.len || text[start] != `(` {
		return none
	}
	mut depth := 0
	for i := start; i < text.len; i++ {
		ch := text[i]
		if ch == `(` {
			depth++
		} else if ch == `)` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return none
}

fn header_type_decl_line_is_malformed(line string) bool {
	if !line.contains('=') {
		return false
	}
	rhs := line.all_after_first('=').trim_space()
	return rhs.len == 0
}

fn header_const_decl_line_is_malformed(line string) bool {
	if !line.contains('=') {
		return false
	}
	rhs := line.all_after_first('=').trim_space()
	return rhs.len == 0
}
