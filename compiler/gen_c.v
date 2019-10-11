module main

import strings

const (
	dot_ptr = '->'
)

/*
fn (p mut Parser) gen_or_else(pos int) string {
}
*/

// returns the type of the new variable
fn (p mut Parser) gen_var_decl(name string, is_static bool) string {
	// Generate expression to tmp because we need its type first
	// `[typ] [name] = bool_expression();`
	pos := p.cgen.add_placeholder()
	mut typ := p.bool_expression()
	if typ.starts_with('...') { typ = typ.right(3) }
	//p.gen('/*after expr*/')
	// Option check ? or {
	or_else := p.tok == .key_orelse
	tmp := p.get_tmp()
	if or_else {
		// Option_User tmp = get_user(1);
		// if (!tmp.ok) { or_statement }
		// User user = *(User*)tmp.data;
		// p.assigned_var = ''
		p.cgen.set_placeholder(pos, '$typ $tmp = ')
		p.genln(';')
		typ = typ.replace('Option_', '')
		p.next()
		p.check(.lcbr)
		p.genln('if (!$tmp .ok) {')
		p.register_var(Var {
			name: 'err'
			typ: 'string'
			is_mut: false
			is_used: true
		})
		p.genln('string err = $tmp . error;')
		p.statements()
		p.genln('$typ $name = *($typ*) $tmp . data;')
		if !p.returns && p.prev_tok2 != .key_continue && p.prev_tok2 != .key_break {
			p.error('`or` block must return/exit/continue/break/panic')
		}
		p.returns = false
		return typ
	}
	gen_name := p.table.var_cgen_name(name)
	mut nt_gen := p.table.cgen_name_type_pair(gen_name, typ)
	// `foo := C.Foo{}` => `Foo foo;`
	if !p.is_empty_c_struct_init && !typ.starts_with('['){
		nt_gen += '='
	} else if typ.starts_with('[') && typ[ typ.len-1 ] != `*` {
		// a fixed_array initializer, like `v := [1.1, 2.2]!!`
		// ... should translate to the following in C `f32 v[2] = {1.1, 2.2};`
		initializer := p.cgen.cur_line.right(pos)
		if initializer.len > 0 {
			p.cgen.resetln(' = {' + initializer.all_after('{') )
		} else if initializer.len == 0 {
			p.cgen.resetln(' = { 0 }')
		}	
	}

	if is_static {
		nt_gen = 'static $nt_gen'
	}
	p.cgen.set_placeholder(pos, nt_gen)
	return typ
}

fn (p mut Parser) gen_fn_decl(f Fn, typ, str_args string) {
	dll_export_linkage := if p.os == .msvc && p.attr == 'live' && p.pref.is_so {
		'__declspec(dllexport) '
	} else if p.attr == 'inline' {
		'static inline '
	} else {
		''
	}
	fn_name_cgen := p.table.fn_gen_name(f)
	//str_args := f.str_args(p.table)
	p.genln('$dll_export_linkage$typ $fn_name_cgen($str_args) {')
}

// blank identifer assignment `_ = 111`
fn (p mut Parser) gen_blank_identifier_assign() {
	assign_error_tok_idx := p.token_idx
	p.check_name()
	p.check_space(.assign)
	expr := p.lit
	is_indexer := p.peek() == .lsbr
	is_fn_call := p.peek() == .lpar || (p.peek() == .dot && p.tokens[p.token_idx+2].tok == .lpar)
	if !is_indexer && !is_fn_call {
		p.error_with_token_index('assigning `$expr` to `_` is redundant', assign_error_tok_idx)
	}
	pos := p.cgen.add_placeholder()
	mut typ := p.bool_expression()
	tmp := p.get_tmp()
	// handle or
	if p.tok == .key_orelse {
		p.cgen.set_placeholder(pos, '$typ $tmp = ')
		p.genln(';')
		typ = typ.replace('Option_', '')
		p.next()
		p.check(.lcbr)
		p.genln('if (!$tmp .ok) {')
		p.register_var(Var {
			name: 'err'
			typ: 'string'
			is_mut: false
			is_used: true
		})
		p.genln('string err = $tmp . error;')
		p.statements()
		p.returns = false
	} else {
		if is_fn_call {
			p.gen(';')
		} else {
			p.cgen.resetln('{$typ _ = $p.cgen.cur_line;}')
		}
	}
}

fn types_to_c(types []Type, table &Table) string {
	mut sb := strings.new_builder(10)
	for t in types {
		if t.cat != .union_ && t.cat != .struct_ && t.cat != .objc_interface {
			continue
		}
		//if is_atomic {
			//sb.write('_Atomic ')
		//}
		if t.cat ==  .objc_interface {
			sb.writeln('@interface $t.name : $t.parent { @public')
		}
		else {
			kind := if t.cat == .union_ {'union'} else {'struct'}
			sb.writeln('$kind $t.name {')
		}
		for field in t.fields {
			sb.write('\t')
			sb.writeln(table.cgen_name_type_pair(field.name,
				field.typ) + ';')
		}
		sb.writeln('};\n')
		if t.cat ==  .objc_interface {
			sb.writeln('@end')
		}
	}
	return sb.str()
}

fn (p mut Parser) index_get(typ string, fn_ph int, cfg IndexCfg) {
	// Erase var name we generated earlier:	"int a = m, 0"
	// "m, 0" gets killed since we need to start from scratch. It's messy.
	// "m, 0" is an index expression, save it before deleting and insert later in map_get()
	mut index_expr := ''
	if p.cgen.is_tmp {
		index_expr = p.cgen.tmp_line.right(fn_ph)
		p.cgen.resetln(p.cgen.tmp_line.left(fn_ph))
	} else {
		index_expr = p.cgen.cur_line.right(fn_ph)
		p.cgen.resetln(p.cgen.cur_line.left(fn_ph))
	}
	// Can't pass integer literal, because map_get() requires a void*
	tmp := p.get_tmp()
	tmp_ok := p.get_tmp()
	if cfg.is_map {
		p.gen('$tmp')
		def := type_default(typ)
		p.cgen.insert_before('$typ $tmp = $def; ' +
			'bool $tmp_ok = map_get(/*$p.file_name : $p.scanner.line_nr*/$index_expr, & $tmp);')
	}
	else if cfg.is_arr {
		if p.pref.translated && !p.builtin_mod {
			p.gen('$index_expr ]')
		}
		else {
			if cfg.is_ptr {
				p.gen('( *($typ*) array_get(* $index_expr) )')
			}  else {
				p.gen('( *($typ*) array_get($index_expr) )')
			}
		}
	}
	else if cfg.is_str && !p.builtin_mod {
		p.gen('string_at($index_expr)')
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
		name = name.replace('*', '')
		name = name.replace('+', 'plus')
		name = name.replace('-', 'minus')
	}
	// Avoid name conflicts (with things like abs(), print() etc).
	// Generate v_abs(), v_print()
	// TODO duplicate functionality
	if f.mod == 'builtin' && f.name in CReserved {
		return 'v_$name'
	}
	// Obfuscate but skip certain names
	// TODO ugly, fix
	// NB: the order here is from faster to potentially slower checks
	if table.obfuscate &&
		!f.is_c &&
		f.name != 'main' && f.name != 'WinMain' && f.name != 'main__main' &&
		f.name != 'gg__vec2' &&
		f.name != 'build_token_str' &&
		f.name != 'build_keys' &&
		f.mod != 'builtin' &&
		f.mod != 'darwin' &&
		f.mod != 'os' &&
		f.mod != 'json' &&
		!f.name.ends_with('_init') &&
		!f.name.contains('window_proc') &&
		!name.ends_with('_str') &&
		!name.contains('contains') {
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

fn (p mut Parser) gen_method_call(receiver_type, ftyp string, cgen_name string, receiver Var,method_ph int) {
	//mut cgen_name := p.table.fn_gen_name(f)
	mut method_call := cgen_name + '('
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
			cast = receiver_type.all_after('_')
			cast = '*($cast*) '
		}else{
			cast = '(voidptr) '
		}
	}
	p.cgen.set_placeholder(method_ph, '$cast $method_call')
	//return method_call
}

fn (p mut Parser) gen_array_at(typ_ string, is_arr0 bool, fn_ph int) {
	mut typ := typ_
	//p.fgen('[')
	// array_int a; a[0]
	// type is "array_int", need "int"
	// typ = typ.replace('array_', '')
	if is_arr0 {
		typ = typ.right(6)
	}
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
	if val == '_' { return }
	p.genln('$var_typ $val = (($var_typ *) $tmp . data)[$i];')
}

fn (p mut Parser) gen_for_str_header(i, tmp, var_typ, val string) {
	p.genln('array_byte bytes_$tmp = string_bytes( $tmp );')
	p.genln(';\nfor (int $i = 0; $i < $tmp .len; $i ++) {')
	if val == '_' { return }
	p.genln('$var_typ $val = (($var_typ *) bytes_$tmp . data)[$i];')
}

fn (p mut Parser) gen_for_range_header(i, range_end, tmp, var_type, val string) {
	p.genln(';\nfor (int $i = $tmp; $i < $range_end; $i++) {')
	if val == '_' { return }
	p.genln('$var_type $val = $i;')
}

fn (p mut Parser) gen_for_map_header(i, tmp, var_typ, val, typ string) {
	def := type_default(typ)
	p.genln('array_string keys_$tmp = map_keys(& $tmp ); ')
	p.genln('for (int l = 0; l < keys_$tmp .len; l++) {')
	p.genln('string $i = ((string*)keys_$tmp .data)[l];')
	// TODO don't call map_get() for each key, fetch values while traversing
	// the tree (replace `map_keys()` above with `map_key_vals()`)
	if val == '_' { return }
	p.genln('$var_typ $val = $def; map_get($tmp, $i, & $val);')
}

fn (p mut Parser) gen_for_varg_header(i, varg, var_typ, val string) {
	p.genln('for (int $i = 0; $i < ${varg}->len; $i++) {')
	if val == '_' { return }
	p.genln('$var_typ $val = (($var_typ *) $varg->args)[$i];')
}

fn (p mut Parser) gen_array_init(typ string, no_alloc bool, new_arr_ph int, nr_elems int) {
	mut new_arr := 'new_array_from_c_array'
	if no_alloc {
		new_arr += '_no_alloc'
	}
	if nr_elems == 0 {
		p.gen(' TCCSKIP(0) })')
	} else {
		p.gen(' })')
	}
	// Need to do this in the second pass, otherwise it goes to the very top of the out.c file
	if !p.first_pass() {		
		p.cgen.set_placeholder(new_arr_ph,
			'$new_arr($nr_elems, $nr_elems, sizeof($typ), EMPTY_ARRAY_OF_ELEMS( $typ, $nr_elems ) { ')
	}
}	

fn (p mut Parser) gen_array_set(typ string, is_ptr, is_map bool,fn_ph, assign_pos int, is_cao bool) {
	// `a[0] = 7`
	// curline right now: `a , 0  =  7`
	mut val := p.cgen.cur_line.right(assign_pos)
	p.cgen.resetln(p.cgen.cur_line.left(assign_pos))
	mut cao_tmp := p.cgen.cur_line
	mut func := ''
	if is_map {
		func = 'map_set(&'
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
		val = cao_tmp + val.all_before('=') +	val.all_after('=')
	}
	p.gen(', & ($typ []) { $val })')
}


// returns true in case of an early return
fn (p mut Parser) gen_struct_init(typ string, t Type) bool {
	// TODO hack. If it's a C type, we may need to add "struct" before declaration:
	// a := &C.A{}  ==>  struct A* a = malloc(sizeof(struct A));
	if p.is_c_struct_init {
		if t.cat != .c_typedef {
			p.cgen.insert_before('struct /*c struct init*/')
		}
	}
	// TODO tm struct struct bug
	if typ == 'tm' {
		p.cgen.lines[p.cgen.lines.len-1] = ''
	}
	p.next()
	p.check(.lcbr)
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
		// TODO tmp hack for 0 pointers init
		// &User{!} ==> 0
		if p.tok == .not {
			p.next()
			p.gen('0')
			p.check(.rcbr)
			return true
		}
		p.gen('($t.name*)memdup(&($t.name)  {')
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
	// `face := FT_Face(cobj)` => `FT_Face face = *((FT_Face*)cobj);`
	casting_voidptr_to_value :=  expr_typ == 'void*' && typ != 'int' &&
		typ != 'byteptr' &&		!typ.ends_with('*')
	p.expected_type = ''
	// `string(buffer)` => `tos2(buffer)`
	// `string(buffer, len)` => `tos(buffer, len)`
	// `string(bytes_array, len)` => `tos(bytes_array.data, len)`
	is_byteptr := expr_typ == 'byte*' || expr_typ == 'byteptr'
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
			}  else {
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
		p.cgen.set_placeholder(pos, '*($typ*)(')
	}
	else {
		p.cgen.set_placeholder(pos, '($typ)(')
	}
	p.check(.rpar)
	p.gen(')')
}

fn type_default(typ string) string {
	if typ.starts_with('array_') {
		return 'new_array(0, 1, sizeof( ${typ.right(6)} ))'
	}
	// Always set pointers to 0
	if typ.ends_with('*') {
		return '0'
	}
	// User struct defined in another module.
	if typ.contains('__') {
		return '{0}'
	}
	// Default values for other types are not needed because of mandatory initialization
	switch typ {
	case 'bool': return '0'
	case 'string': return 'tos((byte *)"", 0)'
	case 'i8': return '0'
	case 'i16': return '0'
	case 'i64': return '0'
	case 'u16': return '0'
	case 'u32': return '0'
	case 'u64': return '0'
	case 'byte': return '0'
	case 'int': return '0'
	case 'rune': return '0'
	case 'f32': return '0.0'
	case 'f64': return '0.0'
	case 'byteptr': return '0'
	case 'voidptr': return '0'
	}
	return '{0}'
}

fn (p mut Parser) gen_array_push(ph int, typ, expr_type, tmp, elm_type string) {
	// Two arrays of the same type?
	push_array := typ == expr_type
	if push_array {
		p.cgen.set_placeholder(ph, '_PUSH_MANY(&' )
		p.gen('), $tmp, $typ)')
	} else {
		p.check_types(expr_type, elm_type)
		// Pass tmp var info to the _PUSH macro
		// Prepend tmp initialisation and push call
		// Don't dereference if it's already a mutable array argument  (`fn foo(mut []int)`)
		push_call := if typ.contains('*'){'_PUSH('} else { '_PUSH(&'}
		p.cgen.set_placeholder(ph, push_call)
		if elm_type.ends_with('*') {
			p.gen('), $tmp, ${elm_type.left(elm_type.len - 1)})')
		} else {
			p.gen('), $tmp, $elm_type)')
		}
	}
}

