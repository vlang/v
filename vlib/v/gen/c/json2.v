module c

import v.ast
import v.util
import strings

fn (mut g Gen) gen_json2_for_type(typ ast.Type) {
	utyp := g.unwrap_generic(typ).set_nr_muls(0)
	sym := g.table.sym(utyp)
	if is_js_prim(sym.name) || sym.kind == .enum_ {
		return
	}

	g.json_types << utyp
}

fn (mut g Gen) gen_jsons2() {
}

fn gen_js2_get(styp string, tmp string, name string, mut dec strings.Builder, is_required bool) {
	dec.writeln('\tOption_x__json2__Any jsonroot_$tmp = x__json2__Any_get(root,_SLIT("$name"));')
	if is_required {
		dec.writeln('\tif(jsonroot_${tmp}.state != 0) {')
		dec.writeln('\t\treturn (Option_$styp){ .state = 2, .err = _v_error(_SLIT("expected field \'$name\' is missing")), .data = {0} };')
		dec.writeln('\t}')
	}
}

fn gen_js2_get_opt(dec_name string, field_type string, styp string, tmp string, name string, mut dec strings.Builder, is_required bool) {
	gen_js_get(styp, tmp, name, mut dec, is_required)
	dec.writeln('\tOption_$field_type $tmp = $dec_name (jsonroot_$tmp);')
	dec.writeln('\tif(${tmp}.state != 0) {')
	dec.writeln('\t\treturn (Option_$styp){ .state = ${tmp}.state, .err = ${tmp}.err, .data = {0} };')
	dec.writeln('\t}')
}

fn js2_enc_name(typ string) string {
	suffix := if typ.ends_with('*') { typ.replace('*', '') } else { typ }
	name := 'x__json2__encode_$suffix'
	return util.no_dots(name)
}

fn js2_dec_name(typ string) string {
	name := 'x__json2__decode_$typ'
	return util.no_dots(name)
}
