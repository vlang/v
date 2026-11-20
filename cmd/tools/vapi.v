// vapi.v implements `v api`, snapshotting and checking a module's public API.
//
// The snapshot is deterministic JSON with no paths or timestamps, listing
// public functions and methods, structs with field mutability, enum and
// sum-type variants, generic parameters with constraints, aliases,
// interfaces, constants, and attributes. `v api check` semantically diffs
// the current API against a committed baseline, naming breaking changes.
module main

import flag
import json2
import os
import v.astjson
import v.flat
import v.parser
import v.pref
import v.types

const tool_version = '0.1.0'
const snapshot_version = 1

// ApiParam is one function parameter or struct field type reference.
pub struct ApiParam {
pub:
	name   string
	typ    string @[json: 'type']
	is_mut bool
}

// ApiGeneric is one generic parameter with its constraint, if any.
pub struct ApiGeneric {
pub:
	name       string
	constraint string
}

// ApiFn is one public function or method.
pub struct ApiFn {
pub:
	name     string
	receiver ApiParam
	params   []ApiParam
	ret      string @[json: 'return']
	generics []ApiGeneric
	attrs    []string
}

// ApiField is one public struct field.
pub struct ApiField {
pub:
	name     string
	typ      string @[json: 'type']
	is_mut   bool
	is_pub   bool
	is_embed bool
	attrs    []string
}

// ApiStruct is one public struct.
pub struct ApiStruct {
pub:
	name     string
	generics []ApiGeneric
	attrs    []string
	fields   []ApiField
}

// ApiEnum is one public enum.
pub struct ApiEnum {
pub:
	name     string
	attrs    []string
	is_flag  bool
	variants []string
}

// ApiSum is one public sum type.
pub struct ApiSum {
pub:
	name     string
	generics []ApiGeneric
	attrs    []string
	variants []string
}

// ApiAlias is one public type alias.
pub struct ApiAlias {
pub:
	name     string
	generics []ApiGeneric
	attrs    []string
	target   string
}

// ApiIfaceMethod is one interface method.
pub struct ApiIfaceMethod {
pub:
	name   string
	params []ApiParam
	ret    string @[json: 'return']
	is_mut bool
}

// ApiInterface is one public interface.
pub struct ApiInterface {
pub:
	name     string
	generics []ApiGeneric
	attrs    []string
	methods  []ApiIfaceMethod
}

// ApiConst is one public constant.
pub struct ApiConst {
pub:
	name  string
	typ   string @[json: 'type']
	attrs []string
}

// ApiSnapshot is the deterministic public API of one module.
pub struct ApiSnapshot {
pub:
	module     string
	version    int
	functions  []ApiFn
	structs    []ApiStruct
	enums      []ApiEnum
	sums       []ApiSum
	aliases    []ApiAlias
	interfaces []ApiInterface
	consts     []ApiConst
}

fn main() {
	args := os.args[1..]
	passed := if args.len > 0 && args[0] == 'api' { args[1..] } else { args }
	mut fp := flag.new_flag_parser(passed)
	fp.application('v api')
	fp.version(tool_version)
	fp.description('Snapshot and check the public API of a V module.')
	fp.arguments_description('snapshot|check [path]')
	out_path := fp.string('out', 0, '', 'output file for `snapshot` (default: stdout)')
	baseline_path := fp.string('baseline', 0, '', 'baseline snapshot for `check` (required)')
	show_help := fp.bool('help', `h`, false, 'Show this help and exit.')
	rest := fp.finalize() or {
		eprintln('v api: ${err.msg()}')
		print_usage()
		exit(1)
	}
	if show_help {
		print_usage()
		return
	}
	if rest.len == 0 {
		eprintln('v api: expected `snapshot` or `check`.')
		print_usage()
		exit(1)
	}
	match rest[0] {
		'snapshot' {
			target := if rest.len > 1 { rest[1] } else { '.' }
			snap := snapshot_module(target) or {
				eprintln('v api: ${err.msg()}')
				exit(1)
			}
			out := encode_snapshot(snap)
			if out_path != '' {
				os.write_file(out_path, out) or {
					eprintln('v api: cannot write `${out_path}`: ${err.msg()}')
					exit(1)
				}
			} else {
				println(out)
			}
		}
		'check' {
			if baseline_path == '' {
				eprintln('v api: `check` needs `--baseline <file>`.')
				print_usage()
				exit(1)
			}
			target := if rest.len > 1 { rest[1] } else { '.' }
			base_text := os.read_file(baseline_path) or {
				eprintln('v api: cannot read baseline `${baseline_path}`: ${err.msg()}')
				exit(1)
			}
			base := json2.decode[ApiSnapshot](base_text) or {
				eprintln('v api: invalid baseline `${baseline_path}`: ${err.msg()}')
				exit(1)
			}
			cur := snapshot_module(target) or {
				eprintln('v api: ${err.msg()}')
				exit(1)
			}
			breaking, non_breaking := check_snapshots(base, cur)
			for line in non_breaking {
				println('ok: ${line}')
			}
			if breaking.len > 0 {
				for line in breaking {
					eprintln('breaking: ${line}')
				}
				eprintln('v api: ${breaking.len} breaking change(s) vs `${baseline_path}`.')
				exit(1)
			}
			println('v api: no breaking changes vs `${baseline_path}`.')
		}
		'help' {
			print_usage()
		}
		else {
			eprintln('v api: unknown subcommand `${rest[0]}` (expected `snapshot` or `check`).')
			print_usage()
			exit(1)
		}
	}
}

fn print_usage() {
	println('Usage:')
	println('  v api snapshot [--out .v/api.json] [path]')
	println('  v api check --baseline .v/api.json [path]')
	println('')
	println('Options:')
	println('  --out <file>       output file for snapshot (default: stdout)')
	println('  --baseline <file>  baseline snapshot for check (required)')
	println('  -h, --help         show this help and exit')
}

// collect_v_files lists the V sources that make up the module at target.
// Only files directly in the target directory belong to the module: every
// subdirectory is its own (sub)module, so recursing would silently merge
// unrelated declarations (e.g. `hello` from `collision` and `hello` from
// `collision.child`) into one snapshot under a single module name.
fn collect_v_files(target string) []string {
	if os.is_file(target) {
		if target.ends_with('.v') {
			return [os.real_path(target)]
		}
		return []string{}
	}
	root := os.real_path(target)
	entries := os.ls(root) or { return []string{} }
	mut files := []string{}
	for entry in entries {
		path := os.join_path(root, entry)
		if os.is_file(path) && entry.ends_with('.v') && !entry.ends_with('_test.v')
			&& !entry.ends_with('.js.v') && !entry.ends_with('.native.v')
			&& !entry.ends_with('.wasm.v') {
			files << os.real_path(path)
		}
	}
	files.sort()
	return files
}

// snapshot_module builds the deterministic public API snapshot of target.
fn snapshot_module(target string) !ApiSnapshot {
	files := collect_v_files(target)
	if files.len == 0 {
		return error('no V files found at `${target}`')
	}
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	prefs.is_fmt = true
	mut parser_ := parser.Parser.new(prefs)
	parser_.no_source_digests = true
	a := parser_.parse_files(files)
	if parser_.diagnostics.len > 0 {
		return error('parse failed: ${parser_.diagnostics.str()}')
	}
	mut tc := types.TypeChecker.new(a)
	tc.enable_globals = true
	tc.collect(a)
	// Best effort only; declarations snapshot even when bodies have errors.
	tc.check_semantics_opt(false)
	mut mod_name := ''
	for node in a.nodes {
		if node.kind == .module_decl {
			if mod_name == '' {
				mod_name = node.value
			} else if node.value != mod_name {
				return error('mixed modules `${mod_name}` and `${node.value}` in `${target}`; snapshot one module at a time')
			}
		}
	}
	if mod_name == '' {
		mod_name = 'main'
	}
	mut fns := []ApiFn{}
	mut structs := []ApiStruct{}
	mut enums := []ApiEnum{}
	mut sums := []ApiSum{}
	mut aliases := []ApiAlias{}
	mut ifaces := []ApiInterface{}
	mut consts := []ApiConst{}
	for idx, node in a.nodes {
		match node.kind {
			.fn_decl {
				if node.op != .arrow {
					continue
				}
				if node.value.starts_with('C.') || node.value.starts_with('JS.') {
					continue
				}
				fns << fn_from_node(a, tc, idx, mod_name)
			}
			.struct_decl {
				if node.op != .arrow {
					continue
				}
				if node.value.starts_with('C.') {
					continue
				}
				structs << struct_from_node(a, tc, idx)
			}
			.enum_decl {
				if node.op != .arrow {
					continue
				}
				enums << enum_from_node(a, tc, idx)
			}
			.type_decl {
				if node.op != .arrow {
					continue
				}
				if node.value.starts_with('C.') {
					continue
				}
				if node.children_count > 0 {
					sums << sum_from_node(a, tc, idx)
				} else {
					aliases << alias_from_node(a, tc, idx)
				}
			}
			.interface_decl {
				if node.op != .arrow {
					continue
				}
				ifaces << iface_from_node(a, tc, idx)
			}
			.const_decl {
				if node.op != .arrow {
					continue
				}
				consts << consts_from_node(a, tc, idx, mod_name)
			}
			else {}
		}
	}
	fns.sort(a.name < b.name)
	structs.sort(a.name < b.name)
	enums.sort(a.name < b.name)
	sums.sort(a.name < b.name)
	aliases.sort(a.name < b.name)
	ifaces.sort(a.name < b.name)
	consts.sort(a.name < b.name)
	return ApiSnapshot{
		module:     mod_name
		version:    snapshot_version
		functions:  fns
		structs:    structs
		enums:      enums
		sums:       sums
		aliases:    aliases
		interfaces: ifaces
		consts:     consts
	}
}

// fn_from_node projects one public function or method from its flat node.
fn fn_from_node(a &flat.FlatAst, tc &types.TypeChecker, idx int, mod_name string) ApiFn {
	node := a.nodes[idx]
	mut name := node.value
	if receiver, method := flat.decode_static_type_method_name(name) {
		name = '${receiver}.${method}'
	}
	_ = mod_name
	mut receiver := ApiParam{}
	mut params := []ApiParam{}
	for ci in 0 .. int(node.children_count) {
		child := a.child(a.nodes[idx], ci)
		cn := a.nodes[int(child)]
		if cn.kind != .param {
			continue
		}
		p := ApiParam{
			name:   cn.value
			typ:    cn.typ_text()
			is_mut: cn.is_mut
		}
		if cn.op == .dot && params.len == 0 && name.contains('.') {
			receiver = p
		} else {
			params << p
		}
	}
	mut ret := node.typ_text()
	if ret == '' {
		ret = 'void'
	}
	return ApiFn{
		name:     name
		receiver: receiver
		params:   params
		ret:      ret
		generics: generics_of(node)
		attrs:    decl_attrs(tc, idx)
	}
}

// struct_from_node projects one public struct, keeping its public fields.
fn struct_from_node(a &flat.FlatAst, tc &types.TypeChecker, idx int) ApiStruct {
	node := a.nodes[idx]
	mut fields := []ApiField{}
	for ci in 0 .. int(node.children_count) {
		child := a.child(a.nodes[idx], ci)
		cn := a.nodes[int(child)]
		if cn.kind != .field_decl {
			continue
		}
		is_mut, is_pub, is_embed, fattrs := field_meta(cn)
		if !is_pub {
			continue
		}
		fields << ApiField{
			name:     cn.value
			typ:      cn.typ_text()
			is_mut:   is_mut
			is_pub:   is_pub
			is_embed: is_embed
			attrs:    fattrs
		}
	}
	fields.sort(a.name < b.name)
	return ApiStruct{
		name:     node.value
		generics: generics_of(node)
		attrs:    decl_attrs(tc, idx)
		fields:   fields
	}
}

// enum_from_node projects one public enum with sorted variants.
fn enum_from_node(a &flat.FlatAst, tc &types.TypeChecker, idx int) ApiEnum {
	node := a.nodes[idx]
	mut variants := []string{}
	for ci in 0 .. int(node.children_count) {
		child := a.child(a.nodes[idx], ci)
		cn := a.nodes[int(child)]
		if cn.kind == .enum_field {
			variants << cn.value
		}
	}
	variants.sort()
	attrs := decl_attrs(tc, idx)
	return ApiEnum{
		name:     node.value
		attrs:    attrs
		is_flag:  'flag' in attrs
		variants: variants
	}
}

// sum_from_node projects one public sum type with sorted variant spellings.
fn sum_from_node(a &flat.FlatAst, tc &types.TypeChecker, idx int) ApiSum {
	node := a.nodes[idx]
	mut variants := []string{}
	for ci in 0 .. int(node.children_count) {
		child := a.child(a.nodes[idx], ci)
		cn := a.nodes[int(child)]
		v := cn.value_text()
		if v != '' {
			variants << v
		}
	}
	variants.sort()
	return ApiSum{
		name:     node.value
		generics: generics_of(node)
		attrs:    decl_attrs(tc, idx)
		variants: variants
	}
}

// alias_from_node projects one public type alias.
fn alias_from_node(a &flat.FlatAst, tc &types.TypeChecker, idx int) ApiAlias {
	node := a.nodes[idx]
	return ApiAlias{
		name:     node.value
		generics: generics_of(node)
		attrs:    decl_attrs(tc, idx)
		target:   node.typ_text()
	}
}

// iface_from_node projects one public interface with sorted methods.
fn iface_from_node(a &flat.FlatAst, tc &types.TypeChecker, idx int) ApiInterface {
	node := a.nodes[idx]
	mut methods := []ApiIfaceMethod{}
	for ci in 0 .. int(node.children_count) {
		child := a.child(a.nodes[idx], ci)
		cn := a.nodes[int(child)]
		if cn.kind != .interface_field {
			continue
		}
		mut params := []ApiParam{}
		for cj in 0 .. int(cn.children_count) {
			gchild := a.child(cn, cj)
			gn := a.nodes[int(gchild)]
			if gn.kind == .param {
				params << ApiParam{
					name:   gn.value
					typ:    gn.typ_text()
					is_mut: gn.is_mut
				}
			}
		}
		mut ret := cn.typ_text()
		if ret == '' {
			ret = 'void'
		}
		methods << ApiIfaceMethod{
			name:   cn.value
			params: params
			ret:    ret
			is_mut: cn.is_mut
		}
	}
	methods.sort(a.name < b.name)
	return ApiInterface{
		name:     node.value
		generics: generics_of(node)
		attrs:    decl_attrs(tc, idx)
		methods:  methods
	}
}

// consts_from_node projects the constants of one public const block.
fn consts_from_node(a &flat.FlatAst, tc &types.TypeChecker, idx int, mod_name string) []ApiConst {
	node := a.nodes[idx]
	attrs := decl_attrs(tc, idx)
	mut out := []ApiConst{}
	for ci in 0 .. int(node.children_count) {
		child := a.child(a.nodes[idx], ci)
		cn := a.nodes[int(child)]
		if cn.kind != .const_field {
			continue
		}
		mut typ_name := cn.typ_text()
		qname := '${mod_name}.${cn.value}'
		if qname in tc.const_types {
			if t := tc.const_types[qname] {
				typ_name = t.name()
				prefix := mod_name + '.'
				if typ_name.starts_with(prefix) {
					typ_name = typ_name[prefix.len..]
				}
			}
		} else if cn.value in tc.const_types {
			if t := tc.const_types[cn.value] {
				typ_name = t.name()
			}
		}
		if typ_name == '' {
			typ_name = 'unknown'
		}
		out << ApiConst{
			name:  cn.value
			typ:   typ_name
			attrs: attrs.clone()
		}
	}
	return out
}

// generics_of pairs generic names with their constraints in declared order.
fn generics_of(node flat.Node) []ApiGeneric {
	params := node.generic_params()
	constraints := node.generic_constraints()
	mut out := []ApiGeneric{cap: params.len}
	for i, name in params {
		constraint := if i < constraints.len { constraints[i] } else { '' }
		out << ApiGeneric{
			name:       name
			constraint: constraint
		}
	}
	return out
}

// field_meta decodes the mut/pub/embed flags and attributes of a field.
fn field_meta(node flat.Node) (bool, bool, bool, []string) {
	gp := node.generic_params()
	if gp.len == 0 {
		return false, false, false, []string{}
	}
	flags := gp[0]
	mut attrs := []string{}
	for a in gp[1..] {
		clean := a.trim_space()
		if clean == '' || clean.starts_with('__v3_') {
			continue
		}
		if clean !in attrs {
			attrs << clean
		}
	}
	attrs.sort()
	return flags.contains('m'), flags.contains('p'), flags.contains('e'), attrs
}

// decl_attrs returns the sorted, de-duplicated attributes of a declaration.
fn decl_attrs(tc &types.TypeChecker, idx int) []string {
	mut out := []string{}
	if idx in tc.declaration_attributes {
		for a in tc.declaration_attributes[idx] {
			clean := a.trim_space()
			if clean == '' || clean.starts_with('__v3_') {
				continue
			}
			if clean !in out {
				out << clean
			}
		}
	}
	out.sort()
	return out
}

// encode_snapshot renders the snapshot as deterministic sorted JSON.
fn encode_snapshot(s ApiSnapshot) string {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('aliases')
	w.begin_array()
	for a in s.aliases {
		w.array_item()
		w.begin_object()
		w.key('attrs')
		encode_strings(mut w, a.attrs)
		w.key('generics')
		encode_generics(mut w, a.generics)
		w.key('name')
		w.string(a.name)
		w.key('target')
		w.string(a.target)
		w.end_object()
	}
	w.end_array()
	w.key('consts')
	w.begin_array()
	for c in s.consts {
		w.array_item()
		w.begin_object()
		w.key('attrs')
		encode_strings(mut w, c.attrs)
		w.key('name')
		w.string(c.name)
		w.key('type')
		w.string(c.typ)
		w.end_object()
	}
	w.end_array()
	w.key('enums')
	w.begin_array()
	for e in s.enums {
		w.array_item()
		w.begin_object()
		w.key('attrs')
		encode_strings(mut w, e.attrs)
		w.key('is_flag')
		w.boolean(e.is_flag)
		w.key('name')
		w.string(e.name)
		w.key('variants')
		encode_strings(mut w, e.variants)
		w.end_object()
	}
	w.end_array()
	w.key('functions')
	w.begin_array()
	for f in s.functions {
		w.array_item()
		w.begin_object()
		w.key('attrs')
		encode_strings(mut w, f.attrs)
		w.key('generics')
		encode_generics(mut w, f.generics)
		w.key('name')
		w.string(f.name)
		w.key('params')
		w.begin_array()
		for p in f.params {
			w.array_item()
			w.begin_object()
			w.key('is_mut')
			w.boolean(p.is_mut)
			w.key('name')
			w.string(p.name)
			w.key('type')
			w.string(p.typ)
			w.end_object()
		}
		w.end_array()
		w.key('receiver')
		w.begin_object()
		w.key('is_mut')
		w.boolean(f.receiver.is_mut)
		w.key('name')
		w.string(f.receiver.name)
		w.key('type')
		w.string(f.receiver.typ)
		w.end_object()
		w.key('return')
		w.string(f.ret)
		w.end_object()
	}
	w.end_array()
	w.key('interfaces')
	w.begin_array()
	for it in s.interfaces {
		w.array_item()
		w.begin_object()
		w.key('attrs')
		encode_strings(mut w, it.attrs)
		w.key('generics')
		encode_generics(mut w, it.generics)
		w.key('methods')
		w.begin_array()
		for m in it.methods {
			w.array_item()
			w.begin_object()
			w.key('is_mut')
			w.boolean(m.is_mut)
			w.key('name')
			w.string(m.name)
			w.key('params')
			w.begin_array()
			for p in m.params {
				w.array_item()
				w.begin_object()
				w.key('is_mut')
				w.boolean(p.is_mut)
				w.key('name')
				w.string(p.name)
				w.key('type')
				w.string(p.typ)
				w.end_object()
			}
			w.end_array()
			w.key('return')
			w.string(m.ret)
			w.end_object()
		}
		w.end_array()
		w.key('name')
		w.string(it.name)
		w.end_object()
	}
	w.end_array()
	w.key('module')
	w.string(s.module)
	w.key('structs')
	w.begin_array()
	for st in s.structs {
		w.array_item()
		w.begin_object()
		w.key('attrs')
		encode_strings(mut w, st.attrs)
		w.key('fields')
		w.begin_array()
		for fld in st.fields {
			w.array_item()
			w.begin_object()
			w.key('attrs')
			encode_strings(mut w, fld.attrs)
			w.key('is_embed')
			w.boolean(fld.is_embed)
			w.key('is_mut')
			w.boolean(fld.is_mut)
			w.key('is_pub')
			w.boolean(fld.is_pub)
			w.key('name')
			w.string(fld.name)
			w.key('type')
			w.string(fld.typ)
			w.end_object()
		}
		w.end_array()
		w.key('generics')
		encode_generics(mut w, st.generics)
		w.key('name')
		w.string(st.name)
		w.end_object()
	}
	w.end_array()
	w.key('sums')
	w.begin_array()
	for sm in s.sums {
		w.array_item()
		w.begin_object()
		w.key('attrs')
		encode_strings(mut w, sm.attrs)
		w.key('generics')
		encode_generics(mut w, sm.generics)
		w.key('name')
		w.string(sm.name)
		w.key('variants')
		encode_strings(mut w, sm.variants)
		w.end_object()
	}
	w.end_array()
	w.key('version')
	w.number(s.version)
	w.end_object()
	return w.str()
}

fn encode_strings(mut w astjson.Writer, items []string) {
	w.begin_array()
	for item in items {
		w.array_item()
		w.string(item)
	}
	w.end_array()
}

fn encode_generics(mut w astjson.Writer, items []ApiGeneric) {
	w.begin_array()
	for g in items {
		w.array_item()
		w.begin_object()
		w.key('constraint')
		w.string(g.constraint)
		w.key('name')
		w.string(g.name)
		w.end_object()
	}
	w.end_array()
}

// check_snapshots diffs the current API against its baseline.
fn check_snapshots(base ApiSnapshot, cur ApiSnapshot) ([]string, []string) {
	mut breaking := []string{}
	mut ok := []string{}
	if base.module != cur.module {
		breaking << 'module changed `${base.module}` -> `${cur.module}`'
		return breaking, ok
	}
	check_functions(base, cur, mut breaking, mut ok)
	check_structs(base, cur, mut breaking, mut ok)
	check_enums(base, cur, mut breaking, mut ok)
	check_sums(base, cur, mut breaking, mut ok)
	check_aliases(base, cur, mut breaking, mut ok)
	check_interfaces(base, cur, mut breaking, mut ok)
	check_consts(base, cur, mut breaking, mut ok)
	return breaking, ok
}

fn check_functions(base ApiSnapshot, cur ApiSnapshot, mut breaking []string, mut ok []string) {
	mut cur_map := map[string]ApiFn{}
	for f in cur.functions {
		cur_map[f.name] = f
	}
	mut base_map := map[string]ApiFn{}
	for f in base.functions {
		base_map[f.name] = f
	}
	for name, bf in base_map {
		if name !in cur_map {
			breaking << 'removed function `${name}`'
			continue
		}
		cf := cur_map[name]
		if (bf.receiver.name == '') != (cf.receiver.name == '') {
			breaking << 'function `${name}` receiver changed'
			continue
		}
		if bf.receiver.name != '' {
			if bf.receiver.typ != cf.receiver.typ {
				breaking << 'function `${name}` receiver type changed `${bf.receiver.typ}` -> `${cf.receiver.typ}`'
			}
			if bf.receiver.is_mut != cf.receiver.is_mut {
				breaking << 'function `${name}` receiver mutability changed'
			}
		}
		if bf.params.len != cf.params.len {
			breaking << 'function `${name}` param count changed `${bf.params.len}` -> `${cf.params.len}`'
		} else {
			for i, bp in bf.params {
				cp := cf.params[i]
				if bp.name != cp.name {
					breaking << 'function `${name}` param ${i} name changed `${bp.name}` -> `${cp.name}`'
				}
				if bp.typ != cp.typ {
					breaking << 'function `${name}` param `${bp.name}` type changed `${bp.typ}` -> `${cp.typ}`'
				}
				if bp.is_mut != cp.is_mut {
					breaking << 'function `${name}` param `${bp.name}` mutability changed'
				}
			}
		}
		check_generics('function `${name}`', bf.generics, cf.generics, mut breaking)
		if bf.ret != cf.ret {
			breaking << 'function `${name}` return changed `${bf.ret}` -> `${cf.ret}`'
		}
	}
	for name, _ in cur_map {
		if name !in base_map {
			ok << 'added function `${name}`'
		}
	}
}

// check_generics compares generic bounds, reporting tightening as breaking.
fn check_generics(owner string, base []ApiGeneric, cur []ApiGeneric, mut breaking []string) {
	if base.len != cur.len {
		breaking << '${owner} generic count changed `${base.len}` -> `${cur.len}`'
		return
	}
	for i, bg in base {
		cg := cur[i]
		if bg.constraint == cg.constraint {
			continue
		}
		if bg.constraint != '' && cg.constraint == '' {
			continue
		}
		breaking << '${owner} generic `${bg.name}` bound changed `${bg.constraint}` -> `${cg.constraint}`'
	}
}

fn check_structs(base ApiSnapshot, cur ApiSnapshot, mut breaking []string, mut ok []string) {
	mut cur_map := map[string]ApiStruct{}
	for s in cur.structs {
		cur_map[s.name] = s
	}
	mut base_map := map[string]ApiStruct{}
	for s in base.structs {
		base_map[s.name] = s
	}
	for name, bs in base_map {
		if name !in cur_map {
			breaking << 'removed struct `${name}`'
			continue
		}
		cs := cur_map[name]
		check_generics('struct `${name}`', bs.generics, cs.generics, mut breaking)
		mut cur_fields := map[string]ApiField{}
		for f in cs.fields {
			cur_fields[f.name] = f
		}
		mut base_fields := map[string]ApiField{}
		for f in bs.fields {
			base_fields[f.name] = f
		}
		for fname, bf in base_fields {
			if fname !in cur_fields {
				breaking << 'struct `${name}` field `${fname}` removed'
				continue
			}
			cf := cur_fields[fname]
			if bf.typ != cf.typ {
				breaking << 'struct `${name}` field `${fname}` type changed `${bf.typ}` -> `${cf.typ}`'
			}
			if bf.is_mut != cf.is_mut {
				breaking << 'struct `${name}` field `${fname}` mutability changed'
			}
			if bf.is_embed != cf.is_embed {
				breaking << 'struct `${name}` field `${fname}` embed changed'
			}
		}
		for fname, _ in cur_fields {
			if fname !in base_fields {
				ok << 'struct `${name}` field `${fname}` added'
			}
		}
	}
	for name, _ in cur_map {
		if name !in base_map {
			ok << 'added struct `${name}`'
		}
	}
}

fn check_enums(base ApiSnapshot, cur ApiSnapshot, mut breaking []string, mut ok []string) {
	mut cur_map := map[string]ApiEnum{}
	for e in cur.enums {
		cur_map[e.name] = e
	}
	mut base_map := map[string]ApiEnum{}
	for e in base.enums {
		base_map[e.name] = e
	}
	for name, be in base_map {
		if name !in cur_map {
			breaking << 'removed enum `${name}`'
			continue
		}
		ce := cur_map[name]
		if be.is_flag != ce.is_flag {
			breaking << 'enum `${name}` flag changed'
		}
		if has_non_exhaustive(be.attrs) && !has_non_exhaustive(ce.attrs) {
			breaking << 'enum `${name}` became exhaustive'
		}
		mut base_vars := map[string]bool{}
		for v in be.variants {
			base_vars[v] = true
		}
		mut cur_vars := map[string]bool{}
		for v in ce.variants {
			cur_vars[v] = true
		}
		for v, _ in base_vars {
			if v !in cur_vars {
				breaking << 'enum `${name}` variant `${v}` removed'
			}
		}
		open := has_non_exhaustive(be.attrs) && has_non_exhaustive(ce.attrs)
		for v, _ in cur_vars {
			if v !in base_vars {
				if open {
					ok << 'enum `${name}` variant `${v}` added (non-exhaustive)'
				} else {
					breaking << 'enum `${name}` variant `${v}` added (exhaustive enum)'
				}
			}
		}
	}
	for name, _ in cur_map {
		if name !in base_map {
			ok << 'added enum `${name}`'
		}
	}
}

fn check_sums(base ApiSnapshot, cur ApiSnapshot, mut breaking []string, mut ok []string) {
	mut cur_map := map[string]ApiSum{}
	for s in cur.sums {
		cur_map[s.name] = s
	}
	mut base_map := map[string]ApiSum{}
	for s in base.sums {
		base_map[s.name] = s
	}
	for name, bs in base_map {
		if name !in cur_map {
			breaking << 'removed sum type `${name}`'
			continue
		}
		cs := cur_map[name]
		check_generics('sum type `${name}`', bs.generics, cs.generics, mut breaking)
		if has_non_exhaustive(bs.attrs) && !has_non_exhaustive(cs.attrs) {
			breaking << 'sum type `${name}` became exhaustive'
		}
		mut base_vars := map[string]bool{}
		for v in bs.variants {
			base_vars[v] = true
		}
		mut cur_vars := map[string]bool{}
		for v in cs.variants {
			cur_vars[v] = true
		}
		for v, _ in base_vars {
			if v !in cur_vars {
				breaking << 'sum type `${name}` variant `${v}` removed'
			}
		}
		open := has_non_exhaustive(bs.attrs) && has_non_exhaustive(cs.attrs)
		for v, _ in cur_vars {
			if v !in base_vars {
				if open {
					ok << 'sum type `${name}` variant `${v}` added (non-exhaustive)'
				} else {
					breaking << 'sum type `${name}` variant `${v}` added (exhaustive sum)'
				}
			}
		}
	}
	for name, _ in cur_map {
		if name !in base_map {
			ok << 'added sum type `${name}`'
		}
	}
}

fn check_aliases(base ApiSnapshot, cur ApiSnapshot, mut breaking []string, mut ok []string) {
	mut cur_map := map[string]ApiAlias{}
	for a in cur.aliases {
		cur_map[a.name] = a
	}
	mut base_map := map[string]ApiAlias{}
	for a in base.aliases {
		base_map[a.name] = a
	}
	for name, ba in base_map {
		if name !in cur_map {
			breaking << 'removed alias `${name}`'
			continue
		}
		ca := cur_map[name]
		if ba.target != ca.target {
			breaking << 'alias `${name}` target changed `${ba.target}` -> `${ca.target}`'
		}
		check_generics('alias `${name}`', ba.generics, ca.generics, mut breaking)
	}
	for name, _ in cur_map {
		if name !in base_map {
			ok << 'added alias `${name}`'
		}
	}
}

fn check_interfaces(base ApiSnapshot, cur ApiSnapshot, mut breaking []string, mut ok []string) {
	mut cur_map := map[string]ApiInterface{}
	for it in cur.interfaces {
		cur_map[it.name] = it
	}
	mut base_map := map[string]ApiInterface{}
	for it in base.interfaces {
		base_map[it.name] = it
	}
	for name, bi in base_map {
		if name !in cur_map {
			breaking << 'removed interface `${name}`'
			continue
		}
		ci := cur_map[name]
		check_generics('interface `${name}`', bi.generics, ci.generics, mut breaking)
		mut cur_methods := map[string]ApiIfaceMethod{}
		for m in ci.methods {
			cur_methods[m.name] = m
		}
		mut base_methods := map[string]ApiIfaceMethod{}
		for m in bi.methods {
			base_methods[m.name] = m
		}
		for mname, bm in base_methods {
			if mname !in cur_methods {
				breaking << 'interface `${name}` method `${mname}` removed'
				continue
			}
			cm := cur_methods[mname]
			if bm.params.len != cm.params.len {
				breaking << 'interface `${name}` method `${mname}` param count changed'
			} else {
				for i, bp in bm.params {
					cp := cm.params[i]
					if bp.typ != cp.typ {
						breaking << 'interface `${name}` method `${mname}` param `${bp.name}` type changed `${bp.typ}` -> `${cp.typ}`'
					}
					if bp.is_mut != cp.is_mut {
						breaking << 'interface `${name}` method `${mname}` param `${bp.name}` mutability changed'
					}
				}
			}
			if bm.ret != cm.ret {
				breaking << 'interface `${name}` method `${mname}` return changed `${bm.ret}` -> `${cm.ret}`'
			}
			if bm.is_mut != cm.is_mut {
				breaking << 'interface `${name}` method `${mname}` mutability changed'
			}
		}
		for mname, _ in cur_methods {
			if mname !in base_methods {
				breaking << 'interface `${name}` method `${mname}` added'
			}
		}
	}
	for name, _ in cur_map {
		if name !in base_map {
			ok << 'added interface `${name}`'
		}
	}
}

fn check_consts(base ApiSnapshot, cur ApiSnapshot, mut breaking []string, mut ok []string) {
	mut cur_map := map[string]ApiConst{}
	for c in cur.consts {
		cur_map[c.name] = c
	}
	mut base_map := map[string]ApiConst{}
	for c in base.consts {
		base_map[c.name] = c
	}
	for name, bc in base_map {
		if name !in cur_map {
			breaking << 'removed const `${name}`'
			continue
		}
		cc := cur_map[name]
		if bc.typ != cc.typ {
			breaking << 'const `${name}` type changed `${bc.typ}` -> `${cc.typ}`'
		}
	}
	for name, _ in cur_map {
		if name !in base_map {
			ok << 'added const `${name}`'
		}
	}
}

// has_non_exhaustive reports whether attrs mark an open enum or sum type.
fn has_non_exhaustive(attrs []string) bool {
	return 'non_exhaustive' in attrs
}
