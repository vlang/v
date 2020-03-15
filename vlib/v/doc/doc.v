module doc

import (
	strings
	// v.builder
	v.pref
	v.table
	v.parser
	v.ast
	os
	term
)

struct Doc {
	out   strings.Builder
	table &table.Table
	mod   string
mut:
	stmts []ast.Stmt // all module statements from all files
}

type FilterFn fn(node ast.FnDecl)bool

pub fn doc(mod string, table &table.Table) string {
	mut d := Doc{
		out: strings.new_builder(1000)
		table: table
		mod: mod
	}
	vlib_path := os.dir(pref.vexe_path()) + '/vlib'
	mod_path := mod.replace('.', os.path_separator)
	path := os.join_path(vlib_path,mod_path)
	if !os.exists(path) {
		println('module "$mod" not found')
		println(path)
		return ''
	}
	// vfiles := os.walk_ext(path, '.v')
	files := os.ls(path) or {
		panic(err)
	}
	for file in files {
		if !file.ends_with('.v') {
			continue
		}
		if file.ends_with('_test.v') || file.ends_with('_windows.v') || file.ends_with('_macos.v') {
			continue
		}
		file_ast := parser.parse_file(os.join_path(path,file), table, .skip_comments)
		d.stmts << file_ast.stmts
	}
	if d.stmts.len == 0 {
		println('nothing here')
		exit(1)
	}
	// struct
	structs_string := d.get_structs()
	if structs_string.len > 0 {
		d.out.writeln(term.header('module $mod: structs', '-- -- '))
		d.out.write(structs_string)
	}
	// enum
	enums_string := d.get_enums()
	if enums_string.len > 0 {
		d.out.writeln(term.header('module $mod: enums', '-- -- '))
		d.out.write(enums_string)
	}
	// function
	fns_string := d.get_fns()
	if fns_string.len > 0 {
		d.out.writeln(term.header('module $mod: functions', '-- -- '))
		d.out.write(fns_string)
	}
	// method
	methods_string := d.get_methods()
	if methods_string.len > 0 {
		d.out.writeln(term.header('module $mod: methods', '-- -- '))
		d.out.write(methods_string)
	}
	d.out.writeln(term.header('', '-- -- '))

	return d.out.str().trim_space()
}

fn (d &Doc) get_fn_node(f ast.FnDecl) string {
	return f.str(d.table).replace_each([d.mod + '.', '', 'pub ', ''])
}

fn (d &Doc) get_fns() string {
	mut fns_strings := strings.new_builder(100)
	fn_signatures := d.get_fn_signatures(is_pub_function)
	for i, s in fn_signatures {
		fns_strings.writeln('[ ${(i+1):02d} ]  $s')
	}
	return fns_strings.str()
}

fn (d &Doc) get_methods() string {
	mut methods_strings := strings.new_builder(100)
	fn_signatures := d.get_fn_signatures(is_pub_method)
	for i, s in fn_signatures {
		methods_strings.writeln('[ ${(i+1):02d} ]  $s')
	}
	return methods_strings.str()
}

fn (d Doc) get_fn_signatures(filter_fn FilterFn) []string {
	mut fn_signatures := []string
	for stmt in d.stmts {
		match stmt {
			ast.FnDecl {
				if filter_fn(it) {
					fn_signatures << d.get_fn_node(it)
				}
			}
			else {}
		}
	}
	fn_signatures.sort()
	return fn_signatures
}

fn is_pub_method(node ast.FnDecl) bool {
	return node.is_pub && node.is_method && !node.is_deprecated
}

fn is_pub_function(node ast.FnDecl) bool {
	return node.is_pub && !node.is_method && !node.is_deprecated
}

// TODO it's probably better to keep using AST, not `table`
fn (d &Doc) get_enums() string {
	mut enums_strings := strings.new_builder(100)
	for typ in d.table.types {
		if typ.kind != .enum_ {
			continue
		}
		enums_strings.writeln('enum $typ.name {')
		info := typ.info as table.Enum
		for val in info.vals {
			enums_strings.writeln('\t$val')
		}
		enums_strings.writeln('}')
	}
	return enums_strings.str()
}

fn (d &Doc) get_structs() string {
	mut structs_strings := strings.new_builder(100)
	for typ in d.table.types {
		if typ.kind != .struct_ || !typ.name.starts_with(d.mod + '.') {
			// !typ.name[0].is_capital() || typ.name.starts_with('C.') {
			continue
		}
		name := typ.name.after('.')
		structs_strings.writeln('struct $name {')
		info := typ.info as table.Struct
		for field in info.fields {
			sym := d.table.get_type_symbol(field.typ)
			structs_strings.writeln('\t$field.name $sym.name')
		}
		structs_strings.writeln('}')
	}
	return structs_strings.str()
}
