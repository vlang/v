module wit

// WIT (WebAssembly Interface Types) AST + helpers.
// This module is intentionally a small, dependency-free implementation.

pub enum WitTypeKind {
	prim
	list
	option
	result
	tuple
	named
}

pub struct WitType {
pub mut:
	kind  WitTypeKind
	prim  string
	items []WitType
	name  string
}

pub fn prim_type(s string) WitType {
	return WitType{ kind: .prim, prim: s }
}

pub fn named_type(s string) WitType {
	return WitType{ kind: .named, name: s }
}

pub struct WitParam {
pub mut:
	name string
	typ  WitType
}

pub struct WitField {
pub mut:
	name string
	typ  WitType
}

pub struct WitFunc {
pub mut:
	name      string
	params    []WitParam
	results   []WitType
	attrs     []string
	is_static bool
}

pub struct WitRecord {
pub mut:
	name   string
	fields []WitField
}

pub struct WitVariantCase {
pub mut:
	name string
	typ  ?WitType
}

pub struct WitVariant {
pub mut:
	name  string
	cases []WitVariantCase
}

pub struct WitEnum {
pub mut:
	name  string
	cases []string
}

pub struct WitFlags {
pub mut:
	name  string
	flags []string
}

pub struct WitResource {
pub mut:
	name    string
	methods []WitFunc
}

pub struct WitTypeDef {
pub mut:
	name string
	typ  WitType
}

pub struct WitUse {
pub mut:
	path  string
	items []string
}

pub enum WitItemKind {
	func
	record
	variant
	enum_
	flags
	resource
	type_alias
	use
}

pub struct WitItem {
pub mut:
	kind     WitItemKind
	func     ?WitFunc
	record   ?WitRecord
	variant  ?WitVariant
	enum_    ?WitEnum
	flags    ?WitFlags
	resource ?WitResource
	type_def ?WitTypeDef
	use_     ?WitUse
	attrs    []string
}

pub struct WitInterface {
pub mut:
	name  string
	items []WitItem
	attrs []string
}

pub struct WitWorldImport {
pub mut:
	name      string
	interface string
	func      ?WitFunc
}

pub struct WitWorldExport {
pub mut:
	name      string
	interface string
	func      ?WitFunc
}

pub struct WitInclude {
pub mut:
	path         string
	with_renames map[string]string
}

pub struct WitWorld {
pub mut:
	name     string
	imports  []WitWorldImport
	exports  []WitWorldExport
	includes []WitInclude
	attrs    []string
}

pub struct WitPackage {
pub mut:
	namespace string
	name      string
	version   string
}

pub struct WitDoc {
pub mut:
	package    WitPackage
	interfaces []WitInterface
	worlds     []WitWorld
}

// ---- helpers ----

pub fn (t WitType) str() string {
	match t.kind {
		.prim { return t.prim }
		.named { return t.name }
		.list {
			if t.items.len > 0 { return 'list<${t.items[0].str()}>' }
			return 'list<_>'
		}
		.option {
			if t.items.len > 0 { return 'option<${t.items[0].str()}>' }
			return 'option<_>'
		}
		.result {
			okstr := if t.items.len > 0 { t.items[0].str() } else { '_' }
			errstr := if t.items.len > 1 { t.items[1].str() } else { '_' }
			return 'result<${okstr}, ${errstr}>'
		}
		.tuple {
			mut parts := []string{}
			for it in t.items { parts << it.str() }
			return 'tuple<${parts.join(', ')}>'
		}
	}
}

pub fn is_prim_name(s string) bool {
	return s in ['bool', 'u8', 'u16', 'u32', 'u64', 's8', 's16', 's32', 's64', 'f32', 'f64', 'char',
		'string']
}
