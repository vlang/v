// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

import strings

const (
	dot_ptr = '->'
)
// returns the type of the new variable
fn (p mut Parser) gen_var_decl(name string, is_static bool) string {
	p.is_var_decl = true
	mut typ := p.bool_expression()
	// mut typ, expr := p.tmp_expr()
	p.is_var_decl = false
	if typ.starts_with('...') {
		typ = typ[3..]
	}
	// p.gen('/*after expr*/')
	// Option check ? or {
	or_else := p.tok == .key_orelse
	if or_else {
		return p.gen_handle_option_or_else(typ, name, 0)
	}
	gen_name := p.table.var_cgen_name(name)
	mut nt_gen := p.table.cgen_name_type_pair(gen_name, typ)
	// `foo := C.Foo{}` => `Foo foo;`
	if !p.is_empty_c_struct_init && !typ.starts_with('[') {
		nt_gen += '='
	}
	else if typ.starts_with('[') && typ[typ.len - 1] != `*` {
		// a fixed_array initializer, like `v := [1.1, 2.2]!!`
		// ... should translate to the following in C `f32 v[2] = {1.1, 2.2};`
		initializer := p.cgen.cur_line
		if initializer.len > 0 {
			p.cgen.resetln(' = {' + initializer.all_after('{'))
		}
		else if initializer.len == 0 {
			p.cgen.resetln(' = { 0 }')
		}
	}
	if is_static {
		nt_gen = 'static $nt_gen'
	}
	// Now that we know the type, prepend it
	// `[typ] [name] = bool_expression();`
	// p.cgen.prepend_to_statement(nt_gen)
	p.cgen.set_placeholder(0, nt_gen)
	return typ
}

fn (p mut Parser) gen_fn_decl(f Fn, typ, str_args string) {
	dll_export_linkage := if p.pref.ccompiler == 'msvc' && p.attr == 'live' && p.pref.is_so { '__declspec(dllexport) ' } else if p.attr == 'inline' { 'static inline ' } else { '' }
	fn_name_cgen := p.table.fn_gen_name(f)
	// str_args := f.str_args(p.table)

	if p.attr == 'live' && p.pref.is_so {
		// See fn.v for details about impl_live_ functions
		p.genln('$typ impl_live_${fn_name_cgen} ($str_args);')
	}
	p.genln('$dll_export_linkage$typ $fn_name_cgen ($str_args) {')
}

// blank identifer assignment `_ = 111`
fn (p mut Parser) gen_blank_identifier_assign() {
	assign_error_tok_idx := p.token_idx
	p.check_name()
	p.check_space(.assign)
	is_indexer := p.peek() == .lsbr
	is_fn_call,next_expr := p.is_expr_fn_call(p.token_idx)
	pos := p.cgen.add_placeholder()
	expr_tok := p.cur_tok_index()
	p.is_var_decl = true
	typ := p.bool_expression()
	if typ == 'void' {
		p.error_with_token_index('${next_expr}() $err_used_as_value', expr_tok)
	}
	p.is_var_decl = false
	if !is_indexer && !is_fn_call {
		p.error_with_token_index('assigning `$next_expr` to `_` is redundant', assign_error_tok_idx)
	}
	// handle or
	if p.tok == .key_orelse {
		p.gen_handle_option_or_else(typ, '', pos)
	}
	else {
		if is_fn_call {
			p.gen(';')
		}
		else {
			p.cgen.resetln('{$typ _ = $p.cgen.cur_line;}')
		}
	}
}

fn (p mut Parser) gen_handle_option_or_else(_typ, name string, fn_call_ph int) string {
	mut typ := _typ
	if !typ.starts_with('Option_') {
		p.error('`or` block cannot be applied to non-optional type')
	}
	is_assign := name.len > 0
	tmp := p.get_tmp()
	p.cgen.set_placeholder(fn_call_ph, '$typ $tmp = ')
	typ = parse_pointer(typ[7..])
	p.genln(';')
	or_tok_idx := p.token_idx
	p.fspace()
	p.check(.key_orelse)
	p.fspace()
	p.check(.lcbr)
	p.fspace()
	p.register_var(Var{
		name: 'err'
		typ: 'string'
		is_mut: false
		is_used: true
	})
	p.register_var(Var{
		name: 'errcode'
		typ: 'int'
		is_mut: false
		is_used: true
	})
	if is_assign && !name.contains('.') && !p.is_var_decl {
		// don't initialize struct fields
		p.genln('$typ $name;')
	}
	p.genln('if (!$tmp .ok) {')
	p.genln('string err = $tmp . error;')
	p.genln('int errcode = $tmp . ecode;')
	last_ph := p.cgen.add_placeholder()
	last_typ := p.statements()
	if is_assign && last_typ == typ {
		// workaround for -g with default optional value
		// when p.cgen.line_directives is true an extra
		// line is added so we need to account for that
		expr_line := if p.cgen.line_directives { p.cgen.lines[p.cgen.lines.len - 3] } else { p.cgen.lines[p.cgen.lines.len - 2] }
		last_expr := expr_line[last_ph..]
		p.cgen.lines[p.cgen.lines.len - 2] = ''
		// same here
		if p.cgen.line_directives {
			p.cgen.lines[p.cgen.lines.len - 3] = ''
		}
		p.genln('if ($tmp .ok) {')
		p.genln('$name = *($typ*) $tmp . data;')
		p.genln('} else {')
		p.genln('$name = $last_expr')
		p.genln('}')
	}
	else if is_assign {
		p.genln('$name = *($typ*)${tmp}.data;')
	}
	if !p.returns && last_typ != typ && is_assign && !(p.prev_tok2 in [.key_continue, .key_break]) {
		p.error_with_token_index('`or` block must provide a default value or return/exit/continue/break/panic', or_tok_idx)
	}
	p.returns = false
	return typ
}

// `files := os.ls('.')?`
fn (p mut Parser) gen_handle_question_suffix(f Fn, ph int) string {
	if p.cur_fn.name != 'main__main' {
		p.error('`func()?` syntax can only be used inside `fn main()` for now')
	}
	p.check(.question)
	tmp := p.get_tmp()
	p.cgen.set_placeholder(ph, '$f.typ $tmp = ')
	p.genln(';')
	p.genln('if (!${tmp}.ok) v_panic(${tmp}.error);')
	typ := f.typ[7..] // option_xxx
	p.gen('*($typ*) ${tmp}.data;')
	return typ
}

fn types_to_c(types []Type, table &Table) string {
	mut sb := strings.new_builder(10)
	for t in types {
		// if t.cat != .union_ && t.cat != .struct_ && t.cat != .objc_interface {
		if !(t.cat in [.union_, .struct_, .objc_interface, .interface_]) {
			continue
		}
		// if is_atomic {
		// sb.write('_Atomic ')
		// }
		if t.cat == .objc_interface {
			sb.writeln('@interface $t.name : $t.parent { @public')
		}
		else {
			kind := if t.cat == .union_ { 'union' } else { 'struct' }
			sb.writeln('$kind $t.name {')
			if t.cat == .interface_ {
				sb.writeln('\tvoid* _object;')
				sb.writeln('\tint _interface_idx; // int t')
			}
		}
		for field in t.fields {
			sb.write('\t')
			sb.writeln(table.cgen_name_type_pair(field.name, field.typ) + ';')
		}
		sb.writeln('};\n')
		if t.cat == .objc_interface {
			sb.writeln('@end')
		}
	}
	return sb.str()
}

fn (p mut Parser) index_get(typ string, fn_ph int, cfg IndexConfig) {
	// Erase var name we generated earlier:	"int a = m, 0"
	// "m, 0" gets killed since we need to start from scratch. It's messy.
	// "m, 0" is an index expression, save it before deleting and insert later in map_get()
	mut index_expr := ''
	if p.cgen.is_tmp {
		index_expr = p.cgen.tmp_line[fn_ph..]
		p.cgen.resetln(p.cgen.tmp_line[..fn_ph])
	}
	else {
		index_expr = p.cgen.cur_line[fn_ph..]
		p.cgen.resetln(p.cgen.cur_line[..fn_ph])
	}
	// Can't pass integer literal, because map_get() requires a void*
	tmp := p.get_tmp()
	tmp_ok := p.get_tmp()
	if cfg.is_map {
		p.gen('$tmp')
		def := type_default(typ)
		p.cgen.insert_before('$typ $tmp = $def; ' + 'bool $tmp_ok = map_get(/*$p.file_name : $p.scanner.line_nr*/$index_expr, & $tmp);')
	}
	else if cfg.is_arr {
		if p.pref.translated && !p.builtin_mod {
			p.gen('$index_expr ]')
		}
		else {
			ref := if cfg.is_ptr { '*' } else { '' }
			if cfg.is_slice {
				p.gen(' array_slice2($ref $index_expr) ')
			}
			else {
				p.gen('( *($typ*) array_get($ref $index_expr) )')
			}
		}
	}
	else if cfg.is_str && !p.builtin_mod {
		if p.pref.is_bare {
			p.gen(index_expr)
		}
		else if cfg.is_slice {
			p.gen('string_substr2($index_expr)')
		}
		else {
			p.gen('string_at($index_expr)')
		}
	}
	// Zero the string after map_get() if it's nil, numbers are automatically 0
	// This is ugly, but what can I do without generics?
	// TODO what about user types?
	if cfg.is_map && typ == 'string' {
		// p.cgen.insert_before('if (!${tmp}.str) $tmp = tos("", 0);')
		p.cgen.insert_before('if (!$tmp_ok) $tmp = tos((byte *)"", 0);')
	}
}

fn (table mut Table) fn_gen_name(f &Fn) string {
	mut name := f.name
	if f.is_method {
		name = '${f.receiver_typ}_$f.name'
		name = name.replace(' ', '')
		if f.name.len == 1 {
			match f.name[0] {
				`+` {
					name = name.replace('+', 'op_plus')
				}
				`-` {
					name = name.replace('-', 'op_minus')
				}
				`*` {
					name = name.replace('*', 'op_mul')
				}
				`/` {
					name = name.replace('/', 'op_div')
				}
				`%` {
					name = name.replace('%', 'op_mod')
				}
				else {}
	}
		}
	}
	if f.is_interface {
		// iname := f.args[0].typ // Speaker
		// var := p.expr_var.name
		return ''
	}
	// Avoid name conflicts (with things like abs(), print() etc).
	// Generate v_abs(), v_print()
	// TODO duplicate functionality
	if f.mod == 'builtin' && f.name in c_reserved {
		return 'v_$name'
	}
	// Obfuscate but skip certain names
	// TODO ugly, fix
	// NB: the order here is from faster to potentially slower checks
	if table.obfuscate && !f.is_c && !(f.name in ['main', 'WinMain', 'main__main', 'gg__vec2', 'build_token_str', 'build_keys']) && !(f.mod in ['builtin', 'darwin', 'os', 'json']) && !f.name.ends_with('_init') && !f.name.contains('window_proc') && !name.ends_with('_str') && !name.contains('contains') {
		mut idx := table.obf_ids[name]
		// No such function yet, register it
		if idx == 0 {
			table.fn_cnt++
			table.obf_ids[name] = table.fn_cnt
			idx = table.fn_cnt
		}
		old := name
		name = 'f_$idx'
		println('$old ==> $name')
	}
	return name
}

fn (p mut Parser) gen_method_call(receiver &Var, receiver_type string, cgen_name string, ftyp string, method_ph int) {
	// mut cgen_name := p.table.fn_gen_name(f)
	mut method_call := cgen_name + ' ('
	// if receiver is key_mut or a ref (&), generate & for the first arg
	if receiver.ref || (receiver.is_mut && !receiver_type.contains('*')) {
		method_call += '& /* ? */'
	}
	// generate deref (TODO copy pasta later in fn_call_args)
	if !receiver.is_mut && receiver_type.contains('*') {
		method_call += '*'
	}
	mut cast := ''
	// Method returns (void*) => cast it to int, string, user etc
	// number := *(int*)numbers.first()
	if ftyp == 'void*' {
		if receiver_type.starts_with('array_') {
			// array_int => int
			cast = parse_pointer(receiver_type.all_after('array_'))
			cast = '*($cast*) '
		}
		else {
			cast = '(voidptr) '
		}
	}
	p.cgen.set_placeholder(method_ph, '$cast $method_call')
}

fn (p mut Parser) gen_array_at(typ_ string, is_arr0 bool, fn_ph int) {
	mut typ := typ_
	// p.fgen('[')
	// array_int a; a[0]
	// type is "array_int", need "int"
	// typ = typ.replace('array_', '')
	// if is_arr0 {
	// typ = typ.right(6)
	// }
	// array a; a.first() voidptr
	// type is "array", need "void*"
	if typ == 'array' {
		typ = 'void*'
	}
	// No bounds check in translated from C code
	if p.pref.translated && !p.builtin_mod {
		// Cast void* to typ*: add (typ*) to the beginning of the assignment :
		// ((int*)a.data = ...
		p.cgen.set_placeholder(fn_ph, '(($typ*)(')
		p.gen('.data))[')
	}
	else {
		p.gen(',')
	}
}

fn (p mut Parser) gen_for_header(i, tmp, var_typ, val string) {
	p.genln('for (int $i = 0; $i < ${tmp}.len; $i++) {')
	if val == '_' {
		return
	}
	p.genln('$var_typ $val = (($var_typ *) $tmp . data)[$i];')
}

fn (p mut Parser) gen_for_fixed_header(i, tmp, var_typ, val string) {
	p.genln('for (int $i = 0; $i < sizeof(${tmp}) / sizeof($tmp [0]); $i++) {')
	if val == '_' {
		return
	}
	p.genln('$var_typ $val = $tmp[$i];')
}

fn (p mut Parser) gen_for_str_header(i, tmp, var_typ, val string) {
	// TODO var_typ is always byte
	// p.genln('array_byte bytes_$tmp = string_bytes( $tmp );')
	p.genln(';\nfor (int $i = 0; $i < $tmp .len; $i ++) {')
	if val == '_' {
		return
	}
	// p.genln('$var_typ $val = (($var_typ *) bytes_$tmp . data)[$i];')
	p.genln('$var_typ $val = ${tmp}.str[$i];')
}

fn (p mut Parser) gen_for_range_header(i, range_end, tmp, var_type, val string) {
	p.genln(';\nfor (int $i = $tmp; $i < $range_end; $i++) {')
	if val == '_' {
		return
	}
	p.genln('$var_type $val = $i;')
}

fn (p mut Parser) gen_for_map_header(i, tmp, var_typ, val, typ string) {
	def := type_default(typ)
	p.genln('array_string keys_$tmp = map_keys(& $tmp ); ')
	p.genln('for (int l = 0; l < keys_$tmp .len; l++) {')
	p.genln('string $i = ((string*)keys_$tmp .data)[l];')
	// TODO don't call map_get() for each key, fetch values while traversing
	// the tree (replace `map_keys()` above with `map_key_vals()`)
	if val == '_' {
		return
	}
	p.genln('$var_typ $val = $def; map_get($tmp, $i, & $val);')
}

fn (p mut Parser) gen_for_varg_header(i, varg, var_typ, val string) {
	p.genln('for (int $i = 0; $i < ${varg}->len; $i++) {')
	if val == '_' {
		return
	}
	p.genln('$var_typ $val = (($var_typ *) $varg->args)[$i];')
}

fn (p mut Parser) gen_array_init(typ string, no_alloc bool, new_arr_ph int, nr_elems int) {
	mut new_arr := 'new_array_from_c_array'
	if no_alloc {
		new_arr += '_no_alloc'
	}
	if nr_elems == 0 {
		p.gen(' TCCSKIP(0) })')
	}
	else {
		p.gen(' })')
	}
	// Need to do this in the second pass, otherwise it goes to the very top of the out.c file
	if !p.first_pass() {
		p.cgen.set_placeholder(new_arr_ph, '${new_arr}($nr_elems, $nr_elems, sizeof($typ), EMPTY_ARRAY_OF_ELEMS( $typ, $nr_elems ) { ')
	}
}

fn (p mut Parser) gen_array_set(typ string, is_ptr, is_map bool, fn_ph, assign_pos int, is_cao bool) {
	// `a[0] = 7`
	// curline right now: `a , 0  =  7`
	mut val := p.cgen.cur_line[assign_pos..]
	p.cgen.resetln(p.cgen.cur_line[..assign_pos])
	mut cao_tmp := p.cgen.cur_line
	mut func := ''
	if is_map {
		if is_ptr {
			func = 'map_set('
		}
		else {
			func = 'map_set(&'
		}
		// CAO on map is a bit more complicated as it loads
		// the value inside a pointer instead of returning it.
	}
	else {
		if is_ptr {
			func = 'array_set('
			if is_cao {
				cao_tmp = '*($p.expected_type *) array_get(*$cao_tmp)'
			}
		}
		else {
			func = 'array_set(&/*q*/'
			if is_cao {
				cao_tmp = '*($p.expected_type *) array_get($cao_tmp)'
			}
		}
	}
	p.cgen.set_placeholder(fn_ph, func)
	if is_cao {
		val = cao_tmp + val.all_before('=') + val.all_after('=')
	}
	p.gen(', & ($typ []) { $val })')
}

// returns true in case of an early return
fn (p mut Parser) gen_struct_init(typ string, t &Type) bool {
	// TODO hack. If it's a C type, we may need to add "struct" before declaration:
	// a := &C.A{}  ==>  struct A* a = malloc(sizeof(struct A));
	if p.is_c_struct_init {
		if t.cat != .c_typedef {
			p.cgen.insert_before('struct /*c struct init*/')
		}
	}
	// TODO tm struct struct bug
	if typ == 'tm' {
		p.cgen.lines[p.cgen.lines.len - 1] = ''
	}
	mut is_config := false
	if p.tok != .lcbr {
		p.next()
	} else {
		is_config = true
	}
	p.check(.lcbr)
	// Handle empty config ({})
	if is_config && p.tok == .rcbr {
		p.check(.rcbr)
		p.gen('($typ) {EMPTY_STRUCT_INITIALIZATION}')
		return true
	}
	ptr := typ.contains('*')
	// `user := User{foo:bar}` => `User user = (User){ .foo = bar}`
	if !ptr {
		if p.is_c_struct_init {
			// `face := C.FT_Face{}` => `FT_Face face;`
			if p.tok == .rcbr {
				p.is_empty_c_struct_init = true
				p.check(.rcbr)
				return true
			}
			p.gen('(struct $typ) {')
			p.is_c_struct_init = false
		}
		else {
			p.gen('($typ) {')
		}
	}
	else {
		if p.tok == .not {
			// old &User{!} ==> 0 hack
			p.error('use `${t.name}(0)` instead of `&$t.name{!}`')
			/*
			p.next()
			p.gen('0')
			p.check(.rcbr)
			return true
			*/

		}
		p.gen('($t.name*)memdup(&($t.name) {')
	}
	return false
}

fn (p mut Parser) gen_struct_field_init(field string) {
	p.gen('.$field = ')
}

fn (p mut Parser) gen_empty_map(typ string) {
	p.gen('new_map(1, sizeof($typ))')
}

fn (p mut Parser) cast(typ string) {
	//p.error('old cast syntax')
	p.gen('(')
	defer {
		p.gen(')')
	}
	p.next()
	pos := p.cgen.add_placeholder()
	if p.tok == .rpar {
		// skip `)` if it's `(*int)(ptr)`, not `int(a)`
		p.ptr_cast = true
		p.next()
	}
	p.check(.lpar)
	p.expected_type = typ
	expr_typ := p.bool_expression()
	// Do not allow `int(my_int)`
	if expr_typ == typ {
		p.warn('casting `$typ` to `$expr_typ` is not needed')
	}
	// `face := FT_Face(cobj)` => `FT_Face face = *((FT_Face*)cobj);`
	casting_voidptr_to_value := expr_typ == 'void*' && !(typ in ['int', 'byteptr']) && !typ.ends_with('*')
	p.expected_type = ''
	// `string(buffer)` => `tos2(buffer)`
	// `string(buffer, len)` => `tos(buffer, len)`
	// `string(bytes_array, len)` => `tos(bytes_array.data, len)`
	is_byteptr := expr_typ in ['byte*', 'byteptr']
	is_bytearr := expr_typ == 'array_byte'
	if typ == 'string' {
		if is_byteptr || is_bytearr {
			if p.tok == .comma {
				p.check(.comma)
				p.cgen.set_placeholder(pos, 'tos((byte *)')
				if is_bytearr {
					p.gen('.data')
				}
				p.gen(', ')
				p.check_types(p.expression(), 'int')
			}
			else {
				if is_bytearr {
					p.gen('.data')
				}
				p.cgen.set_placeholder(pos, 'tos2((byte *)')
			}
		}
		// `string(234)` => error
		else if expr_typ == 'int' {
			p.error('cannot cast `$expr_typ` to `$typ`, use `str()` method instead')
		}
		else {
			p.error('cannot cast `$expr_typ` to `$typ`')
		}
	}
	else if typ == 'byte' && expr_typ == 'string' {
		p.error('cannot cast `$expr_typ` to `$typ`, use backquotes `` to create a `$typ` or access the value of an index of `$expr_typ` using []')
	}
	else if casting_voidptr_to_value {
		p.cgen.set_placeholder(pos, '($typ)(')
	}
	else {
		// Nothing can be cast to bool
		if typ == 'bool' {
			if is_number_type(expr_typ) {
				p.error('cannot cast a number to `bool`')
			}
			p.error('cannot cast `$expr_typ` to `bool`')
		}
		// Strings can't be cast
		if expr_typ == 'string' {
			if is_number_type(typ) {
				p.error('cannot cast `string` to `$typ`, use `${expr_typ}.${typ}()` instead')
			}
			p.error('cannot cast `$expr_typ` to `$typ`')
		}
		// Nothing can be cast to bool
		if expr_typ == 'bool' {
			p.error('cannot cast `bool` to `$typ`')
		}
		if typ != expr_typ && typ in p.table.sum_types {
			tt := p.table.find_type(typ)
			if expr_typ in tt.ctype_names {
				// There is no need for a cast here, since it was already done
				// in p.bool_expression, SUM TYPE CAST2 . Besides, doubling the
				// cast here causes MSVC to complain with:
				// error C2440: 'type cast': cannot convert from 'ExprType' to 'ExprType'
				p.cgen.set_placeholder(pos, '(')
			}else{
				p.warn('only $tt.ctype_names can be casted to `$typ`')
				p.error('cannot cast `$expr_typ` to `$typ`')
			}
		}else{
			p.cgen.set_placeholder(pos, '($typ)(')
		}
	}
	p.check(.rpar)
	p.gen(')')
}

fn type_default(typ string) string {
	if typ.starts_with('array_') {
		return 'new_array(0, 1, sizeof( ${parse_pointer(typ[6..])} ))'
	}
	// Always set pointers to 0
	if typ.ends_with('*') {
		return '0'
	}
	// User struct defined in another module.
	if typ.contains('__') {
		return '{0}'
	}
	if typ.ends_with('Fn') { // TODO
		return '0'
	}
	// Default values for other types are not needed because of mandatory initialization
	match typ {
		'bool' {
			return '0'
		}
		'string' {
			return 'tos3("")'
		}
		'i8' {
			return '0'
		}
		'i16' {
			return '0'
		}
		'i64' {
			return '0'
		}
		'u16' {
			return '0'
		}
		'u32' {
			return '0'
		}
		'u64' {
			return '0'
		}
		'byte' {
			return '0'
		}
		'int' {
			return '0'
		}
		'rune' {
			return '0'
		}
		'f32' {
			return '0.0'
		}
		'f64' {
			return '0.0'
		}
		'byteptr' {
			return '0'
		}
		'voidptr' {
			return '0'
		}
		else {}
	}
	return '{0}'
	// TODO this results in
	// error: expected a field designator, such as '.field = 4'
	// - Empty ee= (Empty) { . =  {0}  } ;
	/*
	return match typ {
		'bool'{ '0'}
		'string'{ 'tos3("")'}
		'i8'{ '0'}
		'i16'{ '0'}
		'i64'{ '0'}
		'u16'{ '0'}
		'u32'{ '0'}
		'u64'{ '0'}
		'byte'{ '0'}
		'int'{ '0'}
		'rune'{ '0'}
		'f32'{ '0.0'}
		'f64'{ '0.0'}
		'byteptr'{ '0'}
		'voidptr'{ '0'}
		else { '{0} '}
	}
	*/

}

fn (p mut Parser) gen_array_push(ph int, typ, expr_type, tmp, elm_type string) {
	// Two arrays of the same type?
	push_array := typ == expr_type
	if push_array {
		p.cgen.set_placeholder(ph, '_PUSH_MANY(&')
		p.gen('), $tmp, $typ)')
	}
	else {
		p.check_types(expr_type, elm_type)
		// Pass tmp var info to the _PUSH macro
		// Prepend tmp initialisation and push call
		// Don't dereference if it's already a mutable array argument  (`fn foo(mut []int)`)
		push_call := if typ.contains('*') { '_PUSH(' } else { '_PUSH(&' }
		p.cgen.set_placeholder(ph, push_call)
		p.gen('), $tmp, $elm_type)')
	}
}
